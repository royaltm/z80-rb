# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'
require 'z80/stdlib'
require 'zxlib/sys'
##
# =Multitasking
#
# This class contains Macros and kernel labels, for tasks and the kernel code.
#
# ===Memory map:
#
#                             TaskVars
#   +---------+-------------+------------+-----------+-----------+-------------+--------------+--------------+
#   | BASIC   | Display and | ZX Printer | System    | ZX Basic  | Tasks' Code | Multitasking | Multitasking |
#   |  ROM    |  Attributes |     Buffer | Variables | Workspace |    and Data |  Stack Space |       Kernel |
#   +---------+-------------+------------+-----------+-----------+-------------+--------------+--------------+
#   ^         ^             ^            ^           ^           ^             ^              ^              ^
#   $0000     $4000         $5B00        $5C00       $5CB6       RAMTOP                                 P_RAMT
#                           mtvars                                             stack_bot      stack_end
#
# ===ZX Basic API
#
#   LET api = PEEK 23296 + 256*PEEK 23297
#
#   REM Setup:
#   REM Initializes or resets multitasking.
#   REM Returns number of stack space bytes available.
#   PRINT USR api
#
#   REM Spawns a task:
#   1 DEF FN m(a,s) = USR api
#   LET tid = FN m(address,stacksize)
#
#   REM Terminates a task:
#   2 DEF FN t(t) = USR api
#   REM Returns number of stack space bytes available after the task is terminated.
#   PRINT FN t(tid)
#
#   REM Returns stack space bytes available:
#   3 DEF FN f() = USR api
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
# +Kill+ will result in "N Statement lost" when trying to terminate non-existing task.
#
# "A Invalid argument" error may also be triggered when arguments are incorrect.
#
# ===Task API
#
#   import Multitasking, :mt, macros: true, code: false
#
#                   # ...
#                   # instead of invoking halt, call mt.task_yield
#                   call mt.task_yield
#                   # ...
#                   # to terminate:
#                   jp   mt.terminate
#                   # or when stack is depleted just:
#                   ret
class Multitasking
  include Z80
  include Z80::TAP

  ###########
  # Exports #
  ###########

  export mtvars
  export api
  export task_yield
  export terminate

  ###########
  # Structs #
  ###########

  # Should not be enlarged if MT_VARS are placed inside ZX Printer Buffer.
  TASK_QUEUE_MAX = 40
  # Can be moved up/down to adjust stack space.
  MT_STACK_BOT = 0xE000
  # Can be moved elsewhere if ZX Printer is needed.
  MT_VARS = mem.pr_buf

  # Task info struct. Each running task has one.
  class TaskInfo < Label
    tid        word
    stack_save word  # task's current SP when yielding execution
    stack_bot  word  # task's stack boundaries: task[-1].stack_bot <= SP <= stack_bot
  end

  # Struct definition for mtvars.
  class TaskVars < Label
    stack_end    word  # the upper boundary of the tasks's stack, becomes 1st task's task_end. Initializes to initial_stack_end.
    tasks        TaskInfo, TASK_QUEUE_MAX
    terminator   word  # a terminator value, must be 0
    system_sp    word  # system SP when yielding execution
    stack_bot    word  # the bottom boundary of the stack space. Initialized to MT_STACK_BOT.
    current_task word  # pointer to task's stack_bot of the currently executed task, 0 if system code execution
    last_tid     word  # last task id created
  end

  raise CompileError if TaskVars.to_i > 256

  ###########
  # Imports #
  ###########

  macro_import  Z80Lib
  macro_import  Z80MathInt
  import        ZXSys, macros:true, code:false

  ##########
  # Macros #
  ##########

  ##
  # ==Multitasking Macros for tasks.
  module Macros
    ##
    # Retrieves current task's id.
    #
    # * +oh+, +ol+:: Hi and lo byte register for output.
    # * +tt+:: Temporary 16bit register, may be +ix+, +iy+ or +hl+.
    #
    # T-states depending on +tt+ used:
    # * +ix+, +iy+: 58
    # * +hl+: 54
    def task_id(oh, ol, tt:hl)
      isolate use: :mtvars do
        if [ix, iy].include?(tt)
                      ld   tt, [mtvars.current_task]
                      ld   oh, [tt - 3]
                      ld   ol, [tt - 4]
        elsif tt == hl
                      ld   tt, [mtvars.current_task]
                      3.times { dec tt }
                      ld   oh, [tt]
                      dec  tt
                      ld   ol, [tt]
        else
          raise ArgumentError, "tt may be only: hl, ix or iy"
        end
      end
    end
    ##
    # Calculates how many bytes are available yet on the task's stack below SP.
    #
    # * +tt+:: Temporary 16bit register, may be +ix+, +iy+ or +hl+.
    # * positive_size:: If +false+, will output <tt>-stack_free - 1</tt> instead.
    # * disable_intr:: If +true+ will disable interrupts before probing;
    #                  the interrupts must be disabled for this routine to work
    #                  and <tt>disable_intr=false</tt> assumes the interrupts are
    #                  already disabled.
    # * enable_intr:: If +false+ will enable interrupts after probing.
    #
    # Modifies: +af+ and +hl+ if +tt+=+hl+ otherwise +tt+ and +hl+.
    #
    # Output:
    # +hl+:: number of bytes available on stack.
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
    def task_stack_free_bytes(tt:hl, positive_size:true, disable_intr:true, enable_intr:true)
      isolate use: :mtvars do
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

  initial_stack_end   label
  mtvars              addr MT_VARS, TaskVars

  # Get and check current task info pointer.
  # CF: 0
  # ZF: 1 if system task
  # ZF: 0, HL: -> current_task.stack_bot
  macro :check_current_task, use: :mtvars do
                      ld   hl, [mtvars.current_task]
                      ld   a, l
                      ora  h
  end

  # ZX Basic API
  ns :api, use: mtvars do
                      call fn_argn                    # is this an FN call with arguments?
                      jr   Z, new_or_terminate        # yes: skip to new_or_terminate
                      jr   C, init_multitasking       # called via USR
                                                      # called via FN with no arguments
    # Returns free stack space reserved for new tasks in BC or raises OOM error.
    ns :stack_space_free, use: mtvars do
                        ld   hl, mtvars.stack_end - (TaskInfo.to_i - 3)
                        ld   bc, TaskInfo.to_i - 3
      search_last       add  hl, bc
                        ld   e, [hl]
                        inc  hl
                        ld   d, [hl]                 # task.stack_bot
                        inc  hl
                        ld   a, [hl]
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
    # Initialize global variables and setup interrupt handler.
    init_multitasking di                              # prevent switching
                      clrmem mtvars, +mtvars          # remove all tasks
                      ld   hl, initial_stack_end
                      ld   [mtvars.stack_end], hl     # initialize stack_end
                      ld   hl, MT_STACK_BOT
                      ld   [mtvars.stack_bot], hl     # initialize stack_bot
                      call stack_space_free
                      ld   a, 0x39
                      ld   i, a                       # load the accumulator with unused (filled with 255) page in rom.
                      im2
                      ei
                      ret

    # Try to get positive integer from FP-value addressed by HL.
    # DE holds a value on success.
    get_arg_int       read_positive_int_value d, e
                      inc  hl                       # point to a next argument possibly
                      ret  Z
    invalid_arg       report_error 'A Invalid argument'

    # Get first DEF FN argument.
    new_or_terminate  call get_arg_int                # 1st argument as 16bit unsigned integer into DE
    # Determine if there is another argument and call new or kill accordingly.
                      call fn_argn.seek_next          # check if exists next argument
                      jr   NZ, task_kill              # nope: then terminate, DE: tid
    # Try to spawn new task, DE: task code address.
                      push de                         # [SP]: task code address
    # Get the stacksize argument.
                      call get_arg_int                # 2nd argument as 16bit unsigned
    # Validate the stacksize argument (must be even and >= 42)
                      bit  0, e                       # DE: stack size
                      jr   NZ, invalid_arg            # stack size must be even
                      ld   hl, 41
                      sbc  hl, de                     # 41 - stack size
                      jr   NC, invalid_arg            # stack size < 42

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
                      jr   NC, slot_found
    # Sorry, no more room for new tasks.
    no_more_room      ld   sp, [mtvars.system_sp]     # restore system SP in case an interrupt would occur
    ei_report_oom     ei                              # enable switching
    report_oom        report_error '4 Out of memory'
    # Let's create a new task info entry.
                                                      # SP: -> task.stack_save, IX: stack size, DE: stack_end, BC: next_tid
    slot_found        ex   de, hl                     # HL: stack_end
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
    # Setup SP for the task, push terminate as its return point and jump to the tasks' code.
    load_task_stack   ld   sp, 0                      # SP: stack_end
                      ld   hl, terminate
                      push hl                         # task can simply +ret+ to terminate
                      ex   de, hl                     # HL: task code address
                      ei
                      jp   (hl)                       # go to task's code
    # Continue from main, terminate a task.
    task_kill         di                              # DE: tid
                      exx
                      push hl                         # save calculator's H'L'
                      exx
                      ld16 bc, de                     # BC: tid
                      call terminate.search_terminate
                      call stack_space_free
                      exx
                      pop  hl
                      exx
    ei_return         ei
                      ret
  end

  # Task API
  # Tasks may jump to this endpoint directly to terminate themselves.
  #
  # +ret+ from a task will actually jump here.
  ns :terminate, use: mtvars do
                      di                            # prevent switching
                      check_current_task            # HL: -> task.stack_bot, CF: 0
                      jr   Z, api.ei_return        # called not from a task
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
                      jr   Z, not_found             # task.tid == 0
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
                      add  hl, bc                   # add reclaimed delta to stack_save
                      push hl
                      pop  hl
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
                      ld   sp, -TaskInfo.to_i
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
    restore_sp        ld   sp, 0
                      ret
    # No such tid found.
    not_found         ld   sp, [vars.err_sp]
                      ei
                      report_error 'N Statement lost'
  end

  # Find DEF FN argument value.
  # ZF=1 if found and then HL points to the FP-value.
  # CF=1 if called directly via USR and not FN.
  fn_argn             find_def_fn_args 1, cf_on_direct: true

  # Task API
  # Task should call this endpoint directly instead of invoking halt.
  # This will switch the context to the next task immediately.
  task_yield          di
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

  ns :handle_interrupts, use: mtvars do
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

                      # ld   hl, -TaskInfo.to_i - 2
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

if __FILE__ == $0
  # :stopdoc:
  mtkernel = Multitasking.new 0x10000 - Multitasking.code.bytesize

  puts mtkernel.debug

  %w[
    mtvars
    initial_stack_end
    api
    task_yield
    terminate
    fff4
  ].each do |label|
    puts "#{label.ljust(20)}: 0x#{mtkernel[label].to_s 16} - #{mtkernel[label]}, size: #{mtkernel.code.bytesize}"
  end

  puts "Total stack space for tasks: #{mtkernel[:initial_stack_end] - 0xE000}"
  puts "\n ZX Basic API:"
  puts "Setup: PRINT USR #{mtkernel[:api]}"
  puts "Spawn: DEF FN m(a,s) = USR #{mtkernel[:api]}"
  puts "       LET tid = FN m(address,stacksize)"
  puts "Kill:  DEF FN t(t) = USR #{mtkernel[:api]}"
  puts "       PRINT FN t(tid)"
  puts "Free:  DEF FN f() = #{mtkernel[:api]}"
  puts "       PRINT FN f()"
  puts "\n Task API:"
  puts "Yield: call #{mtkernel[:task_yield]}"
  puts "Kill:  jp   #{mtkernel['terminate']}"

  Z80::TAP.read_chunk('examples/multitask.tap').save_tap 'mtkernel'
  Z80::TAP.read_chunk('dots.tap', index: 1).save_tap 'mtkernel', append: true
  mtkernel.save_tap('mtkernel', append: true)
  Z80::TAP.parse_file('mtkernel.tap') do |hb|
      puts hb.to_s
  end
end
