# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'
require 'z80/stdlib'
require 'zxlib/sys'

module ZXUtils
  ##
  # =ZXUtils::Multitasking
  #
  # Run machine code programs (a.k.a. "tasks") in parallel with the ZX Spectrum's Basic.
  #
  # This class contains Macros and kernel labels for tasks and the kernel code.
  #
  # The "tasks" are machine code programs that are run in parallel as opposed to system programs which are run
  # in sequence.
  #
  # The Multitasking module provides a kernel that handles a task creation, execution switching and termination.
  # Switching between tasks is handled on each maskable interrupt or on demand.
  # Each task is being provided with its own stack space. System programs such as Basic or user machine code (USR)
  # use the system stack as usual. An execution of a system program is intertwined with an execution of each task
  # in turn. Tasks as well as system programs may "yield" its execution early with an API call.
  #
  # ===Task guide
  #
  # ====Machine code for tasks should respect the following restrictions:
  #
  # - Task's code shouldn't change interrupt handler (no tampering with register +I+ and no +IM+ instructions).
  # - Task's stacks may be moved up when other tasks terminate - tasks shouldn't store pointers to its own stack entries.
  # - When using +SP+ for other purposes always disable interrupts and restore +SP+ to the previous value
  #   before enabling interrupts.
  # - At the task initialization, +IY+ register is set to tasks' <tt>stack_bot + 128</tt> address
  #   and may be used as a stack pointer frame for tasks' variables as the value of +IY+ register is being moved
  #   along with the stack.
  #
  # ====Task code recommendations:
  #
  # - Avoid modifying ZX Basic variables or using ROM routines that modifies them.
  # - To yield execution instead of using +halt+ use <tt>call task_yield</tt>.
  # - To terminate itself the task should either jump to +terminate+ or just +ret+.
  # - Tasks may store local variables using indexing register +IY+ - see below.
  #
  # ====Task initialization:
  #
  # Each task receives its own, dedicated stack space. Whenever the execution context is being switched,
  # next task's +SP+ register address is being checked against its own stack's boundary. If it points
  # below the designated address space the whole multitasking is being terminated (panic) and the control 
  # is returned to the system.
  #
  # Tasks may also use bottom of the stack space for task-local variables storing them via +IY+ register
  # which addresses the 128'th byte above the stack's bottom. So to be on the safe side tasks should start 
  # allocating variables from <tt>[IY-128]</tt> up.
  #
  # =====Task registers:
  #
  # Registers may be used freely with some limitations (+SP+, +IY+) mentioned above.
  # When the task starts, the CPU registers hold:
  #
  # - +SP+:: The task's end of stack - 2. +SP+ points to the address of +terminate+ routine,
  #          so invoking +ret+ will terminate the task.
  # - +IY+:: The task's bottom of stack + 128.
  # - +IX+:: The task's stack size.
  # - +HL+:: The task's initial +PC+.
  # - +BC+:: The task's id.
  # - +DE+:: The +terminate+ routine address.
  #
  #
  # ===Memory map:
  #
  #                             TaskVars
  #   +---------+-------------+------------+-----------+-----------+-------------+--------------+--------------+
  #   | BASIC   | Display and | ZX Printer | System    | ZX Basic  | Tasks' Code | Multitasking | Multitasking |
  #   |  ROM    |  Attributes |     Buffer | Variables | Workspace |    and Data |  Stack Space |       Kernel |
  #   +---------+-------------+------------+-----------+-----------+-------------+--------------+--------------+
  #   ^         ^             ^            ^           ^           ^             ^              ^              ^
  #   $0000     $4000         $5B00        $5C00       $5CB6       RAMTOP        |              |         P_RAMT
  #                           mtvars                                             stack_bot      stack_end
  #
  # ===ZX Basic API
  #
  #   REM Setup:
  #   REM Initializes or resets multitasking.
  #   REM Returns the number of bytes available for new tasks' stacks.
  #   PRINT USR api
  #
  #   REM Spawns a task:
  #   1 DEF FN m(a,s) = USR api
  #
  #   LET tid = FN m(address,stacksize)
  #
  #   REM Terminates a task:
  #   2 DEF FN t(t) = USR api
  #
  #   REM Returns the number of bytes available for new tasks' stacks after the task is terminated.
  #   PRINT FN t(tid)
  #
  #   REM Returns the number of bytes available for new tasks' stacks.
  #   3 DEF FN f() = USR api
  #
  #   PRINT FN f()
  #
  # +address+:: Must be a task's machine code entry point address.
  # +stacksize+:: Must be an even number of bytes, at least 42.
  # +tid+:: Must be a number returned from <tt>FN m(a,s)</tt>.
  #
  # +Setup+ must be invoked at least once before any other function is used.
  # It can also be called to terminate all tasks.
  #
  # +Spawn+ will result in "4 Out of memory" error if there is not enough
  # room for a task info entry or stack space.
  #
  # "A Invalid argument" error may also be reported when arguments are incorrect.
  #
  # ===Task API
  #
  #   import        ZXUtils::Multitasking, code: false, macros: true, labels: ZXUtils::Multitasking.kernel_org
  #
  #                 # ...
  #                 # instead of invoking halt, call task_yield
  #                 call task_yield
  #                 # ...
  #                 # to terminate:
  #                 jp   terminate
  #                 # or when a stack is depleted just:
  #                 ret
  #
  class Multitasking
    include Z80
    include Z80::TAP

    ###########
    # Exports #
    ###########

    # Variables
    export mtvars
    # Api
    export api # ZX Basic api
    export task_yield
    export terminate
    # Utilities
    export find_def_fn_arg
    export ei_report_oom
    export error_4
    export get_uint_arg
    export error_a
    export ei_report_ok
    export error_0
    # Advanced
    export init_multitasking
    export stack_space_free
    export task_kill
    export task_spawn
    ##
    # Instantiate Multitasking kernel with the proper code address.
    def self.new_kernel(*args, **opts)
      new kernel_org, *args, **opts
    end
    ##
    # The Multitasking kernel code start address.
    def self.kernel_org
      0x10000 - code.bytesize
    end

    ###########
    # Structs #
    ###########

    ## A hard limit on the number of tasks. Should not be enlarged if MT_VARS are placed inside ZX Printer Buffer.
    TASK_QUEUE_MAX = 40 unless const_defined?(:TASK_QUEUE_MAX)
    ## Bottom of the multitasking stack space.
    MT_STACK_BOT = 0xE000 unless const_defined?(:MT_STACK_BOT)
    ## An address of the task variables (TaskVars). Can be moved elsewhere if ZX Printer is needed.
    MT_VARS = mem.pr_buf unless const_defined?(:MT_VARS)

    ## Task info structure. Each running task has one.
    class TaskInfo < Label
      ## Task's id.
      tid        word
      ## Task's last +sp+ before yielding execution.
      stack_save word
      ## Task's stack boundaries: task[-1].stack_bot <= +sp+ <= +task.stack_bot+
      stack_bot  word
    end

    ## Definition of +mtvars+.
    class TaskVars < Label
      ## The upper boundary of the tasks's stack, becomes 1st task's task_end. Initializes to initial_stack_end.
      stack_end    word
      ## The tasks' queue.
      tasks        TaskInfo, TASK_QUEUE_MAX
      ## A terminator value, must be 0
      terminator   word
      ## Last system +sp+ before transfering execution to tasks.
      system_sp    word
      ## The bottom boundary of the stack space. Initialized to Multtasking::MT_STACK_BOT.
      stack_bot    word
      ## A pointer to task's stack_bot of the currently executed task, 0 while executing system code.
      current_task word
      ## Last created task id.
      last_tid     word
    end

    raise CompileError if TaskVars.to_i > 256

    ###########
    # Imports #
    ###########

    macro_import  Stdlib
    macro_import  MathInt
    import        ZXLib::Sys, macros:true, code:false

    ##########
    # Macros #
    ##########

    ##
    # ==ZXUtils::Multitasking Macros for tasks.
    module Macros
      ##
      # Checks if the code is being run as a task. ZF flag will be set (Z) if not a task.
      #
      # * +tt+:: A temporary 16bit register, may be one of: +hl+, +bc+, +de+, +ix+ or +iy+.
      # * +mtvars+:: should contain an address of the task variables.
      #
      # T-States depending on +tt+ used:
      # * +ix+, +iy+: 36
      # * +hl+: 24
      # * +bc+, +de+: 28
      #
      # Modifies: +af+, +tt+.
      def task?(tt:hl, mtvars:self.mtvars)
        th, tl = tt.split
        isolate do
                        ld   tt, [mtvars.current_task]
                        ld   a, tl
                        ora  th
        end
      end
      ##
      # Retrieves current task's id.
      #
      # * +oh+, +ol+:: MSB and LSB 8-bit registers for output. Together oh|ol form a task id.
      # * +tt+:: A temporary 16bit register, may be one of: +hl+, +bc+, +de+, +ix+ or +iy+.
      # * +check_if_system+:: If truish the routine will check if the execution context is of a system or a task.
      #                       In this instance the ZF=1 (Z) will indicate that the code is being executed in a system's context
      #                       and none of +oh+,+ol+ registers will be modified.
      # * +disable_intr+:: +true+ will disable interrupts before probing; the interrupts must be disabled for this routine to work
      #                    and <tt>disable_intr=false</tt> assumes the interrupts are already disabled.
      # * +enable_intr+:: +true+ will enable interrupts after probing.
      # * +mtvars+:: should contain an address of the task variables.
      #
      # Modifies: +tt+, +oh+, +ol+, optionally +af+ if +oh+, +ol+ is one of the +tt+ sub-registers.
      def task_id(oh, ol, tt:hl, check_if_system:false, disable_intr:true, enable_intr:true, mtvars:self.mtvars)
        raise ArgumentError, "tt must be one of: hl, bc, de, ix or iy" unless [hl,bc,de,ix,iy].include?(tt)
        raise ArgumentError, "oh and ol must be different 8-bit registers" unless register?(oh) and register?(ol) and
                                                                                  oh != ol and oh.bit8? and ol.bit8?
        th, tl = tt.split
        isolate do |eoc|
                        di if disable_intr
          if check_if_system
                        task?(tt:tt, mtvars:mtvars)
                        jr   Z, eop
          else
                        ld   tt, [mtvars.current_task]
          end
          if [ix, iy].include?(tt)
            if [ixh, ixl, iyh, iyl].include?(oh)
                        ld   a, [tt - 3]
              if [th, tl].include?(oh)
                raise ArgumentError, "ol must not be accumulator if tt is ix or iy and oh is one of: tt sub-registers" if ol == a
              else
                        ld   oh, a
              end
            else
                        ld   oh, [tt - 3]
            end
            if [ixh, ixl, iyh, iyl].include?(ol)
              raise ArgumentError, "oh must not be accumulator or a sub-register of tt if tt is ix or iy and ol is one of: ix or iy sub-registers" if [a,th,tl].include?(oh)
                        ld   a, [tt - 4]
                        ld   ol, a
            else
                        ld   ol, [tt - 4]
                        ld   oh, a if [th, tl].include?(oh)
            end
          elsif tt == hl
                        3.times { dec tt }
            if [th, tl, ixh, ixl, iyh, iyl].include?(oh)
                        ld   a, [tt]
              if [th, tl].include?(oh)
                raise ArgumentError, "ol must not be accumulator if tt is hl and oh is one of: tt sub-registers" if ol == a
              else
                        ld   oh, a
              end
            else
                        ld   oh, [tt]
            end
                        dec  tt
            if [ixh, ixl, iyh, iyl].include?(ol)
              raise ArgumentError, "oh must not be accumulator or a sub-register of tt if tt is hl and ol is one of: ix or iy sub-registers" if [a,th,tl].include?(oh)
                        ld   a, [tt]
                        ld   ol, a
            else
                        ld   ol, [tt]
                        ld   oh, a if [th, tl].include?(oh)
            end
          elsif [bc, de].include?(tt)
            raise ArgumentError, "oh must not be accumulator or a sub-register of tt if tt is bc or de" if [a,th,tl].include?(oh)
                        3.times { dec tt }
                        ld   a, [tt]
                        ld   oh, a
                        dec  tt
                        ld   a, [tt]
                        ld   ol, a unless ol == a
          end
          eop           label
                        ei   if enable_intr
        end
      end
      ##
      # Calculates how many bytes are available yet on the task's stack below SP.
      #
      # * +tt+:: Temporary 16bit register, may be +ix+, +iy+ or +hl+.
      # * +positive_size+:: If +false+, will output <tt>-stack_free - 1</tt> instead.
      # * +disable_intr+:: +true+ will disable interrupts before probing; the interrupts must be disabled for this routine to work
      #                    and <tt>disable_intr=false</tt> assumes the interrupts are already disabled.
      # * +enable_intr+:: +true+ will enable interrupts after probing.
      # * +mtvars+:: should contain an address of the task variables.
      #
      # Modifies: +af+ and +hl+ if +tt+=+hl+ otherwise +tt+ and +hl+.
      #
      # Output:
      # +hl+:: how many bytes are available on the stack.
      # +CF+:: should be 1 on succees; if CF=0, SP already exceeded its designated
      #        space and the multitasking will be terminated on the next task
      #        switching iteration.
      #
      # T-states:
      #      tt  positive_size  no ei|di  ei|di  ei+di
      #   ix,iy          false        77     81     85
      #      hl          false        59     63     67
      #   ix,iy           true       101    105    109
      #      hl           true        83     87     91
      def task_stack_bytes_free(tt:hl, positive_size:true, disable_intr:true, enable_intr:true, mtvars:self.mtvars)
        isolate do
                        di if disable_intr
          if [ix, iy].include?(tt) # 58
                        ld   tt, [mtvars.current_task]
                        ld   h, [tt + 1]
                        ld   l, [tt + 0]
          elsif tt == hl # 67 40
            th, tl = tt.split
                        ld   tt, [mtvars.current_task]
                        ld   a, [tt]
                        inc  tt
                        ld   th, [tt]
                        ld   tl, a
          else
            raise ArgumentError, "tt may be only: hl, ix or iy"
          end
                        scf
                        sbc  hl, sp
          if positive_size
                        ld   a, l
                        cpl
                        ld   l, a
                        ld   a, h
                        cpl
                        ld   h, a
          end
                        ei if enable_intr
        end
      end
    end

    ##########
    # KERNEL #
    ##########

    mtvars              addr MT_VARS, TaskVars
    initial_stack_bot   addr MT_STACK_BOT
    initial_stack_end   label

    # Get and check current task info pointer.
    # CF: 0
    # ZF: 1, HL: 0 if system task
    # ZF: 0, HL: -> current_task.stack_bot
    macro :check_current_task, use: :mtvars do
                        ld   hl, [mtvars.current_task]
                        ld   a, l
                        ora  h
    end

    ##
    # :call-seq:
    #       USR api
    #       DEF FN m(a,s)=USR api
    #       DEF FN t(t)=USR api
    #       DEF FN f()=USR api
    #
    # ZX Basic API
    #
    # This endpoint should be invoked from the ZX Basic directly via USR or indirectly via FN.
    #
    #   LET stackFreeBytes=USR api: REM Initializes multitasking.
    #
    #   1 DEF FN m(a,s)=USR api: REM Spawns a task
    #   LET tid=FN m(address,stacksize)
    #
    #   2 DEF FN t(t)=USR api: REM Terminates a task
    #   LET stackFreeBytes=FN t(tid)
    #
    #   3 DEF FN f() = USR api: REM Returns the number of bytes available for new tasks' stacks.
    ns :api do
                        call find_def_fn_arg            # is this an FN call with arguments?
                        jr   Z, new_or_terminate        # yes: skip to new_or_terminate
                        jr   C, init_multitasking       # called via USR
    end                                                 # called via FN with no arguments
    ##
    # :call-seq:
    #       call stack_space_free
    #
    # Returns (in +bc+) how many bytes are available in multitasking stack space for new tasks.
    # Reports an OOM error if the task variables are corrupted or uninitialized.
    #
    # Modifies: +af+, +bc+, +de+, +hl+.
    ns :stack_space_free, use: :mtvars do
                        ld   hl, mtvars.stack_end - (+TaskInfo - 3)
                        ld   bc, +TaskInfo - 3       # begin with mtvars.stack_end as the 1st task's stack_end
      search_last       add  hl, bc                  # skip task.stack_save
                        ld   e, [hl]                 # task.stack_bot
                        inc  hl
                        ld   d, [hl]                 # task.stack_bot
                        inc  hl
                        ld   a, [hl]                 # task.tid
                        inc  hl
                        ora  [hl]                    # task.tid
                        jr   NZ, search_last         # some tid?
                        ld   hl, [mtvars.stack_bot]  # DE: tasks[last].stack_bot
                        ex   de, hl
                        sbc  hl, de                  # task.stack_bot - mtvars.stack_bot
                        jr   C, ei_report_oom
                        ld16 bc, hl
                        ret
    end

    ##
    # :call-seq:
    #       call get_uint_arg
    #
    # Attempts to read a positive 16-bit integer from a FP-value addressed by +hl+.
    #
    # _NOTE_:: This routine must never be called from a task!
    #
    # Input:
    # * +hl+:: an address of a FP-value.
    #
    # On success +de+ holds a value and +hl+ will be incremented past the last FP-value's byte.
    # On failure reports "A Invalid argument" error.
    #
    # Modifies: +af+, +de+, +hl+.
    get_uint_arg      read_positive_int_value d, e
                      inc  hl                       # point to a next argument possibly
                      ret  Z
    error_a           report_error 'A Invalid argument'

    ##
    # :call-seq:
    #       call init_multitasking
    #
    # Initializes multitasking.
    #
    # _NOTE_:: This routine must never be called from a task!
    #
    # Clears all tasks, sets global variables and enables the custom interrupt handler.
    #
    # Modifies: +af+, +bc+, +de+, +hl+, +i+, +IFF+.
    ns :init_multitasking, use: :mtvars do
                        di                              # prevent switching
                        clrmem mtvars, +mtvars          # remove all tasks
      init_stack_end    ld   hl, initial_stack_end
                        ld   [mtvars.stack_end], hl     # initialize stack_end
      init_stack_bottom ld   hl, initial_stack_bot
                        ld   [mtvars.stack_bot], hl     # initialize stack_bot
                        call stack_space_free
                        ld   a, 0x3B
                        ld   i, a                       # load the accumulator with unused (filled with 255) page in rom.
                        im2
                        ei
                        ret
    end

    # Decide based on arguments: do we kill a task (1) or spawn a new one (2)
    ns :new_or_terminate, use: :mtvars do
      # Get first DEF FN argument.
                        call get_uint_arg               # 1st argument as 16bit unsigned integer into DE
      # Determine if there is another argument and call new or kill accordingly.
                        call find_def_fn_arg.seek_next  # check if exists next argument
                        jr   NZ, task_kill              # nope: then terminate, DE: tid
      # Try to spawn new task, DE: task code address.
                        push de                         # [SP]: task code address
      # Get the stacksize argument.
                        call get_uint_arg               # 2nd argument as 16bit unsigned
      # Validate the stacksize argument (must be even and >= 42)
                        bit  0, e                       # DE: stack size
                        jr   NZ, error_a.err            # stack size must be even
                        ld   hl, 41
                        sbc  hl, de                     # 41 - stack size
                        jr   NC, error_a.err            # stack size < 42

      # Find empty task info record and create a new entry.
                        di                              # prevent switching
                        ld16 ix, de                     # IX: stack size
                        ld   [mtvars.system_sp], sp     # save system SP
      # Generate next tid.
      search_new_tid    ld   sp, mtvars.last_tid
                        pop  bc                         # last tid
      incr_tid_again    inc  bc                         # tid += 1
                        ld   a, c
                        ora  b
                        jr   Z, incr_tid_again          # repeat if tid == 0
                        push bc                         # BC: last_tid = next_tid
      # Search for a first empty task info and verify tid uniqueness.
                        ld   sp, mtvars.stack_end
      search_loop       pop  de                         # DE: task.stack_bot (which is stack_end of the next task)
                        pop  hl                         # HL: task.tid
                        ld   a, l
                        ora  h                          # CF: 0
                        jr   Z, check_is_free           # task.tid == 0
                        sbc  hl, bc                     # task.tid - next_tid == 0
                        pop  hl                         # skip task.stack_save (ignore)
                        jr   NZ, search_loop
                        jr   search_new_tid             # unfortunately it is, so we'll start again with the new next_tid
      # Check if this entry is below mtvars.terminator.
      check_is_free     ld   hl, -mtvars.terminator     # SP: -> task.stack_save
                        add  hl, sp                     # HL: -mtvars.terminator + SP
                        jr   NC, task_spawn
    end
    # Sorry, no more room for new tasks.
    no_more_room      ld   sp, [mtvars.system_sp]     # restore system SP in case an interrupt would occur
    ei_report_oom     ei                              # enable switching
    error_4           report_error '4 Out of memory'

    # Continue from main, terminate a task.
    ns :task_kill, use: :mtvars do
                        di                              # DE: tid
                        push iy
                        exx
                        push hl                         # save calculator's H'L'
                        exx
                        ld16 bc, de                     # BC: tid
                        call terminate.search_terminate
                        call stack_space_free
                        exx
                        pop  hl
                        exx
                        pop  iy
                        ei
                        ret
    end

    # Let's create a new task info entry.
    ns :task_spawn, use: :mtvars do                     # SP: -> task.stack_save, IX: stack size, DE: stack_end, BC: next_tid
                        ex   de, hl                     # HL: stack_end
                        ld   [load_task_stack + 1], hl
                        ld16 de, ix                     # DE: stack size
                        sbc  hl, de                     # HL: stack_bot = stack_end - stack size
                        jr   C, no_more_room            # stack size is waaay too big
                        pop  de                         # SP: -> task.stack_bot, DE: garbage
                        pop  de                         # SP: -> task[1], DE: garbage
                        push hl                         # SP: -> task.stack_bot = stack_bot
                        ld   de, [mtvars.stack_bot]     # validate stack_bot
                        sbc  hl, de                     # task.stack_bot < mtvars.stack_bot
                        jr   C, no_more_room            # stack bottom must not be below mtvars.stack_bot
                        ld   [mtvars.current_task], sp  # set current task, SP: -> task.stack_bot
                        push de                         # SP: -> task.stack_save = garbage
                        push bc                         # SP: -> task.tid = next_tid
      # We need to prepare system stack for switching
                        ld   sp, [mtvars.system_sp]     # restore system stack
                        pop  de                         # DE: task code address
                        push bc                         # store tid which will be restored as BC when system task is switched back
                        ld   hl, -14                    # except BC, IY and H'L' the rest of the registers is irrelevant
                        add  hl, sp                     # HL: SP - 14 (D'E', B'C', A'F', IX, AF, HL, DE)
                        ld   sp, hl                     # SP: SP - 14
                        exx
                        push hl                         # H'L' -> [SP--]
                        exx                             # DE: task code address
                        push iy                         # IY -> [SP--]
                        ld   [mtvars.system_sp], sp     # save system stack for future switching
      # Set task's IY to stack_bot + 128
                        ld   sp, [mtvars.current_task]  # SP: -> task.stack_bot
                        pop  iy                         # IY: task.stack_bot
                        ld   sp, 128
                        add  iy, sp                     # IY: task.stack_bot + 128
      # Setup SP for the task, push terminate as its return point and jump to the tasks' code.
      load_task_stack   ld   sp, 0                      # SP: stack_end
                        ld   hl, terminate
                        push hl                         # task can simply +ret+ to terminate
                        ex   de, hl                     # HL: task code address
                        ei
                        jp   (hl)                       # go to task's code
    end

    ei_report_ok        ei
    error_0             report_error '0 OK'

    ##
    # :call-seq:
    #       jp terminate
    #
    # Terminates the current task.
    #
    # Tasks may jump to this endpoint directly to terminate themselves.
    # If called or jumped to from a system program and not from a task reports "0 OK" error.
    #
    # +ret+ instruction from a task will actually jump back here.
    ns :terminate, use: :mtvars do
                        di                            # prevent switching
                        check_current_task            # HL: -> task.stack_bot, CF: 0
                        jr   Z, ei_report_ok          # called not from a task
                        ld   sp, [mtvars.system_sp]
                        ld   bc, switch_to_sys        # store switch_to_sys as a
                        push bc                       # ret address
                        3.times { dec hl }
                        ld   b, [hl]
                        dec  hl
                        ld   c, [hl]                  # BC: task's tid
      # Find task info entry by tid.
      search_terminate  ld   [restore_sp + 1], sp     # BC: tid, save SP to be restored at the end
                        ld   sp, mtvars.stack_end     # search for a task.tid == BC
      search_loop       pop  de                       # DE: stack_end
                        pop  hl                       # HL: task.tid
                        ld   a, l
                        ora  h                        # CF: 0
                        jr   Z, task_not_found        # task.tid == 0
                        sbc  hl, bc                   # task.tid == BC
                        pop  hl                       # HL: task.stack_save (ignored)
                        jr   NZ, search_loop          # SP: -> task.stack_bot, CF: 0
      # Found task info entry to terminate. Now we need to:
      # - Update the following tasks' stack_save and stack_bot pointers.
      # - Move the following tasks' info entries up.
      # - Move the following tasks' stacks down.
      terminate_found   pop  bc                       # BC: stack_bot, DE: stack_end, SP: -> task[tid + 1], CF: 0
                        ld16 ix, bc                   # IX: stack_bot
                        ld16 hl, de                   # HL: stack_end
                        sbc  hl, bc                   # HL: stack size = stack_end - stack_bot
                        ld16 bc, hl                   # BC: stack size
                        exx                           # B'C': stack size, D'E': stack_end
                        ld   hl, 0
                        ld16 de, hl
                        add  hl, sp                   # HL: task[tid + 1], DE: 0
                        ex   de, hl                   # HL: 0, DE: task[tid + 1]
                        exx                           # BC: stack size, DE: stack_end, D'E': task[tid + 1], H'L': 0
                        jr   update_pointers.start
      # Increment all the following task's pointers by the terminated task's stack size and get the last task's stack_bot.
      ns :update_pointers do
                        pop  hl                       # task.stack_save
                        ld   [restore_sp + 1], sp     # save SP
                        ld   sp, hl                   # SP: task's SP
                        pop  iy                       # IY: task's IY
                        add  iy, bc                   # add reclaimed delta to IY
                        push iy
        restore_sp      ld   sp, 0                    # SP: restore
                        add  hl, bc                   # add reclaimed delta to stack_save
                        push hl
                        pop  hl                       # task.stack_save
                        pop  hl                       # task.stack_bot
                        add  hl, bc                   # add reclaimed delta to stack_bot
                        push hl
                        pop  de                       # updated task.stack_bot
        start           pop  hl                       # task.tid
                        ld   a, l
                        ora  h                        # CF=0
                        jr   NZ, update_pointers
      end                                             # SP: -> task[ntasks].stack_save, BC: terminated task stack size, DE: task[ntasks].stack_bot + stack size
      # Move the following tasks' info entries + terminator up.
                        exx                           # DE: task[tid + 1], HL: 0
                        add  hl, sp                   # HL: SP, CF: 0
                        sbc  hl, de                   # HL: SP - task[tid + 1]
                        ld16 bc, hl                   # BC: SP - task[tid + 1]
                        ld16 hl, de                   # HL: task[tid + 1]
                        ld   sp, -(+TaskInfo)
                        add  hl, sp                   # HL: task[tid]
                        ex   de, hl                   # DE: task[tid], HL: task[tid + 1]
                        ldir
                        exx                           # BC: stack size, DE: task[ntasks-1].stack_bot + tid stack size
      # Move the following tasks' stacks down.
                        ld   sp, ix                   # SP: task[tid].stack_bot
                        ld16 hl, bc                   # tid stack size
                        add  hl, sp                   # HL: task[tid].stack_end
                        ld16 bc, hl                   # BC: task[tid].stack_end
                        sbc  hl, de                   # task[tid].stack_end - (HL: task[ntasks-1].stack_bot + tid stack size)
                        jr   Z, skip_reclaim_stk
                        ld16 de, bc                   # DE: task[tid].stack_end
                        ld16 bc, hl                   # BC: task[ntasks-1].stack_bot + tid stack size - task[tid].stack_end
                        ld   hl, -1
                        add  hl, sp                   # HL: task[tid].stack_bot - 1
                        dec  de                       # DE: task[tid].stack_end - 1
                        lddr
      # That's it, now restore SP and just return.
      skip_reclaim_stk  label
      task_not_found    label
      restore_sp        ld   sp, 0
                        ret
    end

    ##
    # :call-seq:
    #       call find_def_fn_arg
    #
    # Looks for a first DEF FN argument value address.
    #
    # _NOTE_:: This routine must never be called from a task!
    #
    # If an argument is found the +hl+ registers will address an argument's FP-value and the ZF flag will be set (Z).
    # If the code wasn't invoked via the FN function call, the CF flag will be set instead (C).
    #
    # Modifies: +af+ and +hl+.
    ns :rdoc_mark_find_def_fn_arg, merge: true do
      find_def_fn_arg     find_def_fn_args 1, cf_on_direct: true
    end

    ##
    # :call-seq:
    #       call task_yield
    #
    # Yields task execution.
    #
    # Tasks or system programs should call this endpoint instead of invoking halt.
    # This will switch the execution context to the next task in the queue immediately.
    # The execution of the calling task will resume when its turn will come.
    #
    # Modifies: nothing. Requires at least 22 bytes on a machine stack to be available.
    ns :task_yield      do
                        di
                        push bc
                        push de
                        push hl
                        push af
                        push ix
                        ex   af, af
                        push af
                        exx
                        push bc
                        push de
                        push hl
                        push iy
                        jr   handle_interrupts.task_yield
    end

    ns :handle_interrupts, use: :mtvars do
                        # push bc
                        # push de
                        # push hl
                        # push af
                        # push ix
                        # ex   af, af
                        # push af
                        # exx
                        push bc
                        push de
                        push hl
                        push iy
      # Increase system FRAMES, thus imitating ROM interrupt handler (but this is a little faster).
      increase_frames   ld   hl, vars.frames        # ++frames
                        inc  [hl]
                        jr   NZ, skip_inc_frames
                        inc  l
                        inc  [hl]
                        jr   NZ, skip_inc_frames
                        inc  l
                        inc  [hl]
      # Call the KEYBOARD routine so the keyboard will be responsive even under heavy load.
      skip_inc_frames   ld   iy, vars.err_nr
                        call rom.keyboard           # call rom keyboard routine
      # Finally switch the context.
      task_yield        check_current_task          # HL: -> task.stack_bot
                        jr   NZ, save_task_sp       # switch from a task
      # Switch from ZX Basic.
                        ld   [mtvars.system_sp], sp
                        ld   sp, mtvars.tasks        # switch to the first task in the queue
                        jr   skip_check_stack
      # PANIC. Will immediately stop all multitasking and restore ROM interrupt routine.
      task_stack_panic  restore_rom_interrupt_handler
                        jr   switch_to_sys
      # Switch from a task. Validate if SP register is within task's stack boundaries and save it.
      save_task_sp      ex   de, hl                 # DE: -> task.stack_bot
                        ld   hl, 0
                        add  hl, sp                 # HL: SP
                        ex   de, hl                 # DE: SP, HL: -> task.stack_bot
                        ld   sp, hl                 # HL: current_task -> task.stack_bot
                        push de                     # task.stack_save = SP
                        ld   sp, hl                 # SP: -> task.stack_bot
                        pop  hl                     # HL: stack_bot, SP: -> task[1]
                        scf                         # CF=1
                        sbc  hl, de                 # stack_bot - SP - 1
                        jr   NC, task_stack_panic   # panic if stack_bot >= SP + 1

                        # ld   hl, -(+TaskInfo) - 2
                        # add  hl, sp
                        # ld   a, [hl]
                        # dec  hl
                        # ld   l, [hl]
                        # ld   h, a
                        # ccf                         # CF=0
                        # sbc  hl, de                 # stack_end - SP
                        # jr   C, task_stack_panic    # panic if stack_end < SP
      # Get next task's tid and stack_save and switch to it if tid <> 0, otherwise switch to system.
      skip_check_stack  pop  hl                     # HL: next task.tid, SP: -> next task.stack_bot
                        ld   a, h
                        ora  l
                        pop  hl                     # HL: next task.stack_save
                        jr   NZ, switch_task        # stack_save <> 0 (not a terminator nor an empty slot)
    end

    # Switch to the system task, assumes every register and the return address is already on the stack
    # and the current SP is already saved.
    switch_to_sys       ld   hl, [mtvars.system_sp]
                        ld   sp, 0

    # Switch to another task, assumes every register and the return address is already on the stack
    # and the current SP is already saved.
    # HL: target value of the task's SP
    # SP: current_task to assign (0 for system, or a pointer to task.stack_bot)
    switch_task         ld   [mtvars.current_task], sp
                        ld   sp, hl
                        pop  iy
                        pop  hl
                        pop  de
                        pop  bc
                        exx
                        pop  af
                        ex   af, af
                        pop  ix
                        pop  af
                        pop  hl
                        pop  de
                        pop  bc
                        ei
                        ret

    # Indirect interrupt entry point
    ns :fff4 do
                        push bc
                        push de
                        push hl
                        push af
                        push ix
                        ex   af, af
                        push af
                        exx
                        jr   handle_interrupts
                        db   0x18                # jr 0xFFF4, direct interrupt entry point
                                                 # db 0xF3 in ROM at 0x0000
    end
  end
end

if __FILE__ == $0
  require 'test/unit/assertions'
  require 'digest/sha1'

  include Test::Unit::Assertions
  # :stopdoc:
  mtkernel = ZXUtils::Multitasking.new_kernel

  puts mtkernel.debug

  %w[
    mtvars
    initial_stack_bot
    initial_stack_end
    api
    task_yield
    terminate
    fff4
  ].each do |label|
    puts "#{label.ljust(20)}: 0x#{mtkernel[label].to_s 16} - #{mtkernel[label]}, size: #{mtkernel.code.bytesize}"
  end

  puts "Total stack space for tasks: #{mtkernel[:initial_stack_end] - mtkernel[:initial_stack_bot]}"
  puts "\n ZX Basic API:"
  puts "Setup: PRINT USR #{mtkernel[:api]}"
  puts "Spawn: DEF FN m(a,s)=USR #{mtkernel[:api]}"
  puts "       LET tid=FN m(address,stacksize)"
  puts "Kill:  DEF FN t(t)=USR #{mtkernel[:api]}"
  puts "       PRINT FN t(tid)"
  puts "Free:  DEF FN f()=USR #{mtkernel[:api]}"
  puts "       PRINT FN f()"
  puts "\n Task API:"
  puts "Yield: call #{mtkernel[:task_yield]}"
  puts "Kill:  jp   #{mtkernel['terminate']}"

  assert_equal Digest::SHA256.base64digest(mtkernel.code), '4tLG44B2QHVvmIG+dNG3g5dAVqrc6MxA49HciodTTgw='

  mtkernel.save_tap 'mtkernel'
  Z80::TAP.parse_file('mtkernel.tap') do |hb|
      puts hb.to_s
  end
end
