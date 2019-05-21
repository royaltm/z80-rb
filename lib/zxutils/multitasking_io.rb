# -*- coding: BINARY -*-
require 'zxutils/multitasking'
##
# =MultitaskingIO
#
# Asynchronous communication channels between tasks running in parallel with ZX Spectrum's Basic programs.
# 
# See Multitasking for more information on tasks.
#
# This class contains Macros and kernel labels for tasks and the kernel code.
#
#                                +----------- find_io_handles ------------+
#                      CHANS     v                                        |
#   +----------+       +----------+  output  +------------+  input handle |
#   | PRINT #n | ----> | User     | -------> | I/O Buffer | <------> +-------+
#   | ZX Basic |       | Channel  |          +------------+          | Tasks |
#   | INKEY$#n | <---- |   Record | <------- | I/O Buffer | <------> +-------+
#   +----------+       +----------+  input   +------------+  output handle
#
# The ZX Spectrum's I/O channels are indentified by single upper-case letters. There are four system channels:
# "K", "S", "R" and "P". Channels in order to be used must be "opened" by associating a channel with streams # 0-15.
# By default streams 0-3 are occupied by the system channels. In ZX Spectrum's Basic there are dedicated
# <tt>OPEN #</tt> and <tt>CLOSE #</tt> statements to associate channels with streams.
# Unfortunately their usage is limited only to the system channels.
#
# MultitaskingIO provides API to create its own channels and associate them with user streams.
# Each MultitaskingIO channel represents a pair of I/O Buffers, one for writing and one for reading 
# from the system (full-duplex).
# The tasks can read or write data to any of the buffers.
# The most common use-case however is that the system's output provides the input for tasks and vice-versa.
#
# To speed up things tasks are not using streams but rather acquire a direct handle to an I/O Buffer
# and operate on its data directly by the routines provided by MultitaskingIO::Macros.
# 
#
# ===Memory map:
#
#                             TaskVarsIO                    | I/O user channel records
#   +---------+-------------+-------------+-----------+----------+---+-----------+-------------+--
#   | BASIC   | Display and | ZX Printer  | System    | Channel  |$80| ZX Basic  | Tasks' Code |  
#   |  ROM    |  Attributes |      Buffer | Variables |     Info |   | Workspace |    and Data |  
#   +---------+-------------+-------------+-----------+----------+---+-----------+-------------+--
#   ^         ^             ^             ^           ^              ^           ^             ^  
#   $0000     $4000         $5B00         $5C00       $5CB6 = CHANS  PROG        RAMTOP  mtio_buffers_bot
#                           mtiovars
#
#     |            <-|
#   --+--------------+--------------+---------------+--------+--------------+
#     | Reserved for | I/O Buffers  | Multitasking  | I/O    | Multitasking |
#     |  I/O Buffers |   Data Space |   Stack Space | Kernel |       Kernel |
#   --+--------------+--------------+---------------+--------+--------------+
#     ^              ^              ^               ^                       ^
#  mtio_buffers_bot  |              |               |                  P_RAMT
#                    buffers_top    tv.stack_bot    tv.stack_end
#
# ===Multitasking Api
#
# All of the labels and macros exported by the Multitasking kernel are being re-exported by the MultitaskingIO module.
#
# Programs compiled for the Multitasking kernel without the MultitaskingIO will work fine, as the addresses of the
# Multitasking functions don't change.
#
# ===ZX Basic API
#
#   REM Setup:
#   REM Initializes multitasking and I/O task variables, makes space for channels.
#   REM Returns the number of bytes available for new tasks' stacks.
#   PRINT USR open_io
#
#   REM Reset:
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
#   REM Opens or creates a MultitaskingIO channel and assigns a stream # to it:
#   4 DEF FN o(s,c$)=USR open_io
#
#   REM Initializes channel Q and allocates buffers if called for the first time with that letter.
#   REM Opens channel Q and assigns stream #7 to that channel. Returns previously assigned channel identifier.
#   PRINT CHR$ FN o(7,"Q")
#
#   REM Closes stream #7. Returns previously assigned channel identifier.
#   PRINT CHR$ FN o(7,"")
#
#   REM Wait for I/O data availability.
#   5 DEF FN w(s,n)=USR wait_io
#
#   REM Blocks an execution of a program until at least 3 characters are available to be read from an I/O buffer at stream #7.
#   LET ReadCharsNo=FN w(7,3): REM ReadCharsNo >= 3
#
#   REM Reads 3 characters from I/O channel #7.
#   LET a$=INKEY$#7+INKEY$#7+INKEY$#7
#
#   REM Blocks an execution of a program until at least 5 characters can be written to an I/O buffer at stream #7.
#   LET WriteCharsNo=FN w(7,-5): REM WriteCharsNo >= 5
#
#   REM Sends 5 characters into I/O channel #7.
#   PRINT #7;"hello";
#
# +address+:: Must be a task's machine code entry point address.
# +stacksize+:: Must be an even number of bytes, at least 42.
# +tid+:: Must be a number returned from a call to <tt>FN m(a,s)</tt>.
#
# +Setup+ must be invoked once before any other function is used.
#
# +Reset+ can be called to terminate all tasks.
#
# +Spawn+ will report a "4 Out of memory" error if there is not enough
# room for a task info entry or stack space.
#
# +Open+ will report a "4 Out of memory" error if there is not enough
# room for I/O buffer data.
#
# "A Invalid argument", "B Integer out of range", "F Invalid file name", "O Invalid stream",
# "Q Parameter error" errors may also be reported when arguments are incorrect.
#
# Printing to the stream with the I/O buffer full will report "8 End of file" error.
#
# When reading a stream without any data available an INKEY$# function will return an empty string.
#
# ===Task API
#
# To communicate with the MultitaskingIO api tasks should obtain a direct handle to the I/O buffer.
# This is done via one of the kernel functions:
#
# * find_io_handles
# * find_input_handle
# * find_output_handle
#
# _Example_:
#
#                 # import kernel macros and labels from MultitaskingIO into the "mtio" namespace
#   import        MultitaskingIO, :mtio, code: false, macros: true, labels: MultitaskingIO.kernel_org
#
#                 # ...
#                 # instead of invoking halt, call mtio.task_yield
#                 call mtio.task_yield
#
#                 # to terminate:
#                 jp   mtio.terminate
#                 # or when a stack is depleted just:
#                 ret
#
#                 # wait for the channel Q being available
#   wait_loop     ld   a, "Q".ord
#                 call mtio.find_io_handles
#                 jr   Z, got_handles # HL: input handle, DE: output handle
#                 call mtio.task_yield
#                 jr   wait_loop
#
#                 # read a character, assume HL contains an address of the input handle
#                 mtio_getc(a, not_ready: :eoc, subroutine: false, mtyield: mtio.task_yield)
#                 jr   C, got_the_character
#
#                 # write a @ character, assume HL contains an address of the output handle
#                 ld   a, "@".ord
#                 mtio_putc(a, not_ready: :eoc, subroutine: false, mtyield: mtio.task_yield)
#                 jr   C, character_was_written
#
#                 # read a character, block a task if not available, assume HL contains an address of the input handle
#                 mtio_getc(a, not_ready: :wait, subroutine: false, mtyield: mtio.task_yield)
#                 # do something with a character in accumulator
#
#                 # wait for data, assume HL contains an address of the input handle
#                 ld   c, 5 # at least 5 characters
#                 mtio_wait(:read, c, mtyield: mtio.task_yield)
#
#                 # write a small string up to 255 characters, assume HL contains an address of the output handle
#                 ld   de, hello_world
#                 ld   a, +hello_world
#                 mtio_puts(a, subroutine:false, mtyield: mtio.task_yield)
#                 cp   +hello_world
#                 jr   Z, written_all_characters
#                 ...
#   hello_world   data "Hello world!"
class MultitaskingIO
  include Z80
  include Z80::TAP

  ###########
  # Exports #
  ###########

  # MultitaskingIO addresses
  export mtio_buffers_bot
  export mtiovars
  # Multitasking addresses
  export mtvars
  # MultitaskingIO api
  export open_io # ZX Basic api
  export wait_io # ZX Basic api
  export find_io_handles
  export find_input_handle
  export find_output_handle
  # Multitasking api
  export api # ZX Basic api
  export task_yield
  export terminate
  # Utilities
  export error_q
  export get_stream_arg
  export error_o
  export mtiobuf_inp_handle
  export mtiobuf_out_handle
  export stream_channel_data
  export channel_data_bc
  export system_chan_names
  export error_f
  export find_channel_arg
  export find_channel
  export get_int8_norm_arg
  export error_b
  # re-exported from Multitasking
  export find_def_fn_arg
  export ei_report_oom
  export error_4
  export get_uint_arg
  export error_a
  export ei_report_ok
  export error_0
  # Advanced
  export initialize_io
  export create_channel
  export open_stream_stacked
  export open_stream
  # re-exported from Multitasking
  export init_multitasking
  export stack_space_free
  export task_kill
  export task_spawn
  ##
  # Instantiate MultitaskingIO kernel with the proper code address.
  def self.new_kernel(*args, **opts)
    new(kernel_org, *args, **opts).tap do |kernel|
      mtio_buffers_bot = kernel[:initial_stack_bot] - kernel[:mtio_buffer_chans]*2*MultitaskingIO::BufferIO.to_i
      if kernel[:mtio_buffers_bot] > mtio_buffers_bot
        raise CompileError, "mtio_buffers_bot may be at most: #{mtio_buffers_bot}"
      end
    end
  end
  ##
  # The MultitaskingIO kernel code start address.
  def self.kernel_org
    0x10000 - code.bytesize
  end

  ###########
  # Structs #
  ###########

  ## I/O Buffer structure.
  class BufferIO < Label
    buffer byte 256
    outoff byte
    inpoff byte # <- handle
    jmp    byte # =0xC3 JP addr
    addr   word
  end

  ## Extended Multitasking::TaskVars structure.
  class TaskVarsIO < Label
    tv          Multitasking::TaskVars
    buffers_top word
  end

  ##
  # Defines how many I/O buffer channels may be allocated.
  # May be changed to a higher number. More buffers equals less memory but more independent channels.
  # Each channel costs 527 bytes (2 * BufferIO + 5 bytes for a channel record).
  MTIO_BUFFER_CHANNELS = 1 unless const_defined?(:MTIO_BUFFER_CHANNELS)

  ##
  # If the I/O operation blocks program while waiting for data or free buffer space, pressing BREAK
  # will stop the program execution with "8 End of file" error report.
  # Define MTIO_DETECT_BREAK_KEY_ON_BLOCKING_IO = +false+ to disable this.
  MTIO_DETECT_BREAK_KEY_ON_BLOCKING_IO = true

  ##
  # When using INPUT # with an I/O channel other than "K" and then the error report is being printed
  # in channel "K", a cursor is shown after a reported message expecting further user input, e.g.:
  #
  #   INPUT #2;a
  #
  # will report the: "J Invalid I/O device" error, but the flashing [K] will be visible at the end of the message.
  #
  # The expected behaviour is that the cursor is not visible until the next key is being pressed.
  # Pressing the key then should clear the error message first, before printing anything else.
  #
  # When MTIO_CLEAR_TV_FLAG_ON_INPUT is +true+ on each character input the TV_FLAG system variable is being
  # cleared to prevent bogus cursor behaviour.
  #
  # This is the default. Set to +false+ to disable this countermeasure.
  #
  # This happens because INPUT uses WAIT-KEY ($15D4) procedure which in turn sets the bit 3 of TV_FLAG signalling
  # to reprint the edit area. When using channel "K" after each accepted key a procedure ED-COPY ($111D) at $2162
  # is being called which reprints the edit area and clears the bit 3. However when another channel is in use
  # this part is being skipped at $2168. When the program execution ends, the editor starts reading user input
  # from channel "K" (at KEY-INPUT: $10A8). But the bit 3 of TV_FLAG is still set. This in turn makes the ED-COPY
  # being called immediately and the cursor appears. ED-COPY prevents the error message to be cleared, so the further
  # user input will be echoed after the message.
  MTIO_CLEAR_TV_FLAG_ON_INPUT = true

  ###########
  # Imports #
  ###########

  macro_import  Z80Lib
  import        ZXSys, macros:true, code:false

  ##########
  # Macros #
  ##########

  ##
  # ==MultitaskingIO Macros for tasks.
  #
  # Most of the routines created by MultitaskingIO::Macros expects an I/O buffer handle in the +hl+ register pair.
  # The buffer handle may be obtained by calling one of the kernel functions:
  # * MultitaskingIO.find_io_handles,
  # * MultitaskingIO.find_input_handle,
  # * MultitaskingIO.find_output_handle.
  module Macros
    include Multitasking::Macros
    ##
    # Checks I/O buffer's data availability.
    #
    # Arguments:
    # * +action+:: a symbol +:read+ to get the information if the data is ready to be read,
    #              a symbol +:write+ to get the information if the data can be written.
    # * +nchars+:: +nil+ to get a simple boolean answer or a number or an 8 bit register
    #              which should contain a number of characters that should be available for reading or writing.
    # Options:
    # * +disable_intr+:: a boolean flag indicating that the routine should disable interrupts. Provide +false+
    #                    only if you have already disabled the interrupts.
    # * +enable_intr+:: a boolean flag indicating that the routine should enable interrupts. Provide +false+
    #                   if you need to perform more atomic actions.
    #
    # Expects:
    # * +hl+:: an I/O buffer handle.
    #
    # Depending on the selected action and whether +nchars+ option was +nil+ or not, on return:
    #
    #    action    nchars      status     flags      accumulator
    #     *any*       nil      not ready  ZF=1 (Z)   0
    #     :read       nil      ready      ZF=0 (NZ)  no of characters ready to be read <= 255
    #    :write       nil      ready      ZF=0 (NZ)  no of characters already written + 1 <= 255
    #     :read    number      not ready  CF=1 (C)   no of characters ready to be read < nchars
    #     :read    number      ready      CF=0 (NC)  no of characters ready to be read >= nchars
    #    :write    number      not ready  CF=1 (C)   no of characters that won't fit into the buffer - 1 < nchars
    #    :write    number      ready      CF=0 (NC)  no of characters already written + nchars <= 255
    #
    # Modifies: +af+, preserves: +hl+ and +nchars+.
    def mtio_ready?(action, nchars:nil, disable_intr:true, enable_intr:true)
      raise ArgumentError, "action must be one of: :read or :write" unless action==:read or action==:write
      raise ArgumentError, "nchars must not be one of: a, h nor l" if [a, h, l].include?(nchars)
      isolate do
                        dec   hl
                        di if disable_intr
                        ld    a, [hl] # outoffs
                        inc   hl
                        ei if enable_intr
                        sub   [hl] # inpoffs
        if nchars
          case action
          when :read
                        cp   nchars
          when :write
                        add  nchars
          end
        else
                        inc   a if action == :write
        end
      end
    end
    ##
    # Waits for the I/O buffer's data availability.
    #
    # Arguments:
    # * +action+:: a symbol +:read+ to wait for the data to be read,
    #              a symbol +:write+ to wait for the data to be written.
    # * +nchars+:: a number or an 8 bit register which should contain a number of characters that should be
    #              available for reading or writing.
    # Options:
    # * +disable_intr+:: a boolean flag indicating that the routine should disable interrupts. Provide +false+
    #                    only if you have already disabled the interrupts.
    # * +enable_intr+:: a boolean flag indicating that the routine should enable interrupts. Provide +false+
    #                   if you need to perform more atomic actions.
    # * +mtyield+:: should contain an address of the kernel routine: +task_yield+.
    #
    # Expects:
    # * +hl+:: an I/O buffer handle.
    #
    # On return a CF flag always cleared and accumulator contains a number of characters ready to be read
    # or a number of characters already written + nchars.
    #
    # Modifies: +af+, preserves: +hl+ and +nchars+.
    def mtio_wait(action, nchars=1, disable_intr:true, enable_intr:true, mtyield:task_yield)
      raise ArgumentError, "nchars must not be nil" if nchars.nil?
      isolate do |eoc|
        check_ready     mtio_ready?(action, nchars:nchars, disable_intr:disable_intr, enable_intr:enable_intr)
                        jr   NC, eoc
                        call mtyield
                        di unless disable_intr
                        jr   check_ready
      end
    end
    ##
    # Drains the I/O buffer.
    #
    # Options:
    # * +disable_intr+:: a boolean flag indicating that the routine should disable interrupts. Provide +false+
    #                    only if you have already disabled the interrupts.
    # * +enable_intr+:: a boolean flag indicating that the routine should enable interrupts. Provide +false+
    #                   if you need to perform more atomic actions.
    #
    # Expects:
    # * +hl+:: an I/O buffer handle.
    #
    # Removes all the pending data from an I/O buffer.
    #
    # Modifies: +af+, preserves: +hl+.
    def mtio_drain(disable_intr:true, enable_intr:true)
      isolate do |eoc|
                        dec   hl
                        di if disable_intr
                        ld    a, [hl] # outoffs
                        inc   hl
                        ei if enable_intr
                        ld    [hl], a # inpoffs
      end
    end
    ##
    # Reads a single character from the I/O buffer.
    # Arguments:
    # * +char+:: an 8 bit register which should receive a code if the character read.
    # Options:
    # * +tt+:: an 16 bit register for temporary use.
    # * +not_ready+:: what to do when a character can't be read.
    #                 A symbol +:eoc+ to just signal the failure with the CF flag reset (NC) and the ZF flag set (Z).
    #                 A symbol +:wait+ to wait for the character to become available.
    #                 A label or an address to jump to when the character is not available.
    # * +subroutine+:: if the routine should terminate with a +ret+ instruction.
    # * +preserve_hl+:: if the routine should preserve the +hl+ register via the stack.
    # * +disable_intr+:: a boolean flag indicating that the routine should disable interrupts. Provide +false+
    #                    only if you have already disabled the interrupts.
    # * +enable_intr+:: a boolean flag indicating that the routine should enable interrupts. Provide +false+
    #                   if you need to perform more atomic actions.
    # * +mtyield+:: should contain an address of the kernel routine: +task_yield+.
    #
    # Expects:
    # * +hl+:: an I/O buffer handle.
    #
    # On success the CF flag is set and a register +char+ contains a read character.
    #
    # Modifies: +af+, +hl+, +tt+, +char+, optionally preserves: +hl+ on the machine stack.
    def mtio_getc(char=e, tt:bc, not_ready: :eoc, subroutine: true, preserve_hl:true, disable_intr:true, enable_intr:true, mtyield: task_yield)
      th, tl = tt.split
      raise ArgumentError, "char must not be a tl or th or h or l" if [h,l,th,tl].include?(char)
      isolate do
                        push  hl if preserve_hl
          repeat        label
                        di if disable_intr
                        ld    tl, [hl] # inpoffs
                        dec   hl
                        ld    a, [hl]  # outoffs
                        cp    tl
        case not_ready
        when :eoc
          if subroutine && !enable_intr && !preserve_hl
                        ret   Z
          else
                        jr    Z, eop
          end
        when :wait
          if subroutine
                        jr    Z, wait_busy
          else
                        jr    NZ, skip
                        call  mtyield
                        di unless disable_intr
                        inc   hl
                        jr    repeat
          end
        else
                        jp    Z, not_ready
        end
        skip            inc   tl
                        inc   hl
                        ld    [hl], tl # inpoffs
                        dec   hl       # outoffs
                        ld    th, -1
                        add   hl, tt   # CF=1
                        ld    char, [hl]
        eop             label
                        ei if enable_intr
                        pop   hl if preserve_hl
        if subroutine
                        ret
          if not_ready == :wait
        wait_busy       call  mtyield
                        di unless disable_intr
                        inc   hl
                        jr    repeat
          end
        end
      end
    end
    ##
    # Writes a single character to the I/O buffer.
    #
    # Arguments:
    # * +char+:: a number or an 8 bit register with the character code to be written.
    # Options:
    # * +tt+:: an 16 bit register for temporary use.
    # * +not_ready+:: what to do when a character can't be written.
    #                 A symbol +:eoc+ to just signal the failure with the CF flag reset (NC) and the ZF flag set (Z).
    #                 A symbol +:wait+ to wait until a character gets written.
    #                 A label or an address to jump to when the character can't be written.
    # * +subroutine+:: if the routine should terminate with a +ret+ instruction.
    # * +preserve_hl+:: if the routine should preserve the +hl+ register via the stack.
    # * +disable_intr+:: a boolean flag indicating that the routine should disable interrupts. Provide +false+
    #                    only if you have already disabled the interrupts.
    # * +enable_intr+:: a boolean flag indicating that the routine should enable interrupts. Provide +false+
    #                   if you need to perform more atomic actions.
    # * +mtyield+:: should contain an address of the kernel routine: +task_yield+.
    #
    # Expects:
    # * +hl+:: an I/O buffer handle.
    #
    # On success the CF flag is set and the ZF flag is clear (NZ).
    #
    # Modifies: +af+, +hl+, +tt+, preserves: +char+, optionally preserves: +hl+ on the machine stack.
    def mtio_putc(char=e, tt:bc, not_ready: :eoc, subroutine: true, preserve_hl:true, disable_intr:true, enable_intr:true, mtyield: task_yield)
      th, tl = tt.split
      raise ArgumentError, "char must not be a tl or th or h or l" if [h,l,th,tl].include?(char)
      isolate do
                        push  hl if preserve_hl
                        ld    th, a if char==a
          repeat        label
                        di if disable_intr
                        ld    a, [hl]   # inpoffs
                        dec   hl
                        ld    tl, [hl]  # outoffs
                        inc   tl
                        cp    tl
        case not_ready
        when :eoc
          if subroutine && !enable_intr && !preserve_hl
                        ret   Z
          else
                        jr    Z, eop
          end
        when :wait
          if subroutine
                        jr    Z, wait_busy
          else
                        jr    NZ, skip
                        call  mtyield
                        di unless disable_intr
                        inc   hl
                        jr    repeat
          end
        else
                        jp    Z, not_ready
        end
        skip            ld    [hl], tl  # outoffs
                        ld    a, th if char==a
                        ld    th, -1
                        add   hl, tt
                        ld    [hl], char
        eop             label
                        ei if enable_intr
                        pop   hl if preserve_hl
        if subroutine
                        ret
          if not_ready == :wait
        wait_busy       call  mtyield
                        di unless disable_intr
                        inc   hl
                        jr    repeat
          end
        end
      end
    end
    ##
    # Reads a string of characters from the I/O buffer.
    #
    # Arguments:
    # * +nchars+:: a number 1..255 or accumulator with a number of characters to read.
    # Options:
    # * +tt+:: an 16 bit register for temporary use.
    # * +check_nchars_zero+:: a boolean indicating if the accumulator should be checked against 0.
    # * +subroutine+:: if the routine should terminate with a +ret+ instruction.
    # * +disable_intr+:: a boolean flag indicating that the routine should disable interrupts. Provide +false+
    #                    only if you have already disabled the interrupts.
    # * +enable_intr+:: a boolean flag indicating that the routine should enable interrupts. Provide +false+
    #                   if you need to perform more atomic actions.
    # * +mtyield+:: should contain an address of the kernel routine: +task_yield+.
    #
    # Expects:
    # * +hl+:: an I/O buffer handle.
    # * +de+:: an address where the string will be stored.
    #
    # On success accumulator contains the number of characters successfully read.
    # If its value is 0 the ZF flag is also set indicating that no characters has been read at all.
    # Preserves +hl+ registers. +de+ will be incremented by the number of characters read.
    #
    # Modifies: +af+, +af'+, +de+, +bc+ and uses stack to preserve +hl+.
    def mtio_gets(nchars=a, check_nchars_zero:true, subroutine:true, disable_intr:true, enable_intr:true, mtyield: task_yield)
      raise ArgumentError, "nchars must be accumulator or an integer: 1..255" unless nchars == a or (1..255).include?(nchars)
      isolate do |eoc|
        if nchars == a
          if check_nchars_zero
                        anda  a         # nchars == 0
            if subroutine
                        ret   Z
            else
                        jr    Z, eoc
            end
          end
                        ex    af, af
        end
                        di if disable_intr
                        dec   hl        # -> outoffs
                        ld    a, [hl]   # outoffs
                        inc   hl        # -> inpoffs
                        ld    c, [hl]   # inpoffs
                        sub   c         # a: nchars ready
                        jr    Z, eop    # nothing to read
                        inc   c         # inpoffs + 1
                        push  hl        # -> inpoffs
                        dec   hl        # -> outoffs
                        ld    b, -1
                        add   hl, bc
                        ld    b, a      # b: nchars ready
        if nchars == a
                        ex    af, af
        else
                        ld    a, nchars
        end
                        cp    b         # nchars < nchars ready
                        jr    NC, nchars_not_less
                        ld    b, a      # counter = nchars
        nchars_not_less ld    a, c      # a: inpoffs
                        ld    c, b      # c: counter min(nchars, nchars ready)
                        dec   b         # b: counter - 1
                        add   b         # inpoffs = (inpoffs + counter - 1) % 256
                        jr    NC, one_pass_only
                        ld    b, a      # save inpoffs
                        cpl             # (-inpoffs - 1) % 256
                        add   c         # ((-inpoffs - 1) % 256 + counter) % 256, CF: 1
                        ld    c, a      # reduced counter to the end of the i/o buffer
                        ld    a, b      # restore inpoffs
        one_pass_only   ld    b, 0
                        ldir            # HL: -> buffer, DE: -> target
                        jr    NC, no_2nd_pass
                        ld    c, a
                        inc   c         # counter = inpoffs + 1
                        dec   h         # rewind buffer
                        ldir            # DE: -> source + min(nchars, nchars ready)
        no_2nd_pass     pop   hl        # -> inpoffs
                        ld    c, [hl]   # prev inpoffs
                        ld    [hl], a   # new inpoffs
                        sub   c         # written = new inpoffs - prev inpoffs
        eop             label
                        ei if enable_intr
                        ret if subroutine
      end
    end
    ##
    # Sends a string of characters to the I/O buffer.
    #
    # Arguments:
    # * +nchars+:: a number 1..255 or accumulator with a number of characters to be sent.
    # Options:
    # * +tt+:: an 16 bit register for temporary use.
    # * +check_nchars_zero+:: a boolean indicating if the accumulator should be checked against 0.
    # * +subroutine+:: if the routine should terminate with a +ret+ instruction.
    # * +disable_intr+:: a boolean flag indicating that the routine should disable interrupts. Provide +false+
    #                    only if you have already disabled the interrupts.
    # * +enable_intr+:: a boolean flag indicating that the routine should enable interrupts. Provide +false+
    #                   if you need to perform more atomic actions.
    # * +mtyield+:: should contain an address of the kernel routine: +task_yield+.
    #
    # Expects:
    # * +hl+:: an I/O buffer handle.
    # * +de+:: an address of the string to be send.
    #
    # On success accumulator contains the number of characters successfully sent.
    # If its value is 0 the ZF flag is also set indicating that no characters has been sent at all.
    # Preserves +hl+ registers. +de+ will be incremented by the number of characters sent.
    #
    # Modifies: +af+, +af'+, +de+, +bc+ and uses stack to preserve +hl+.
    def mtio_puts(nchars=a, check_nchars_zero:true, subroutine:true, disable_intr:true, enable_intr:true, mtyield: task_yield)
      raise ArgumentError, "nchars must be accumulator or an integer: 1..255" unless nchars == a or (1..255).include?(nchars)
      isolate do |eoc|
        if nchars == a
          if check_nchars_zero
                        anda  a         # nchars == 0
            if subroutine
                        ret   Z
            else
                        jr    Z, eoc
            end
          end
                        ex    af, af
        end
                        di if disable_intr
                        ld    a, [hl]   # -> inpoffs
                        dec   hl
                        ld    c, [hl]   # -> outoffs
                        inc   c         # c: outoffs + 1
                        sub   c         # a: buffer free
                        jr    Z, eop    # no room to write
                        push  hl        # -> outoffs
                        ld    b, -1
                        add   hl, bc
                        ld    b, a      # b: buffer free
        if nchars == a
                        ex    af, af
        else
                        ld    a, nchars
        end
                        cp    b         # nchars < buffer free
                        jr    NC, nchars_not_less
                        ld    b, a      # counter = nchars
        nchars_not_less ld    a, c      # a: outoffs
                        ld    c, b      # c: counter min(nchars, buffer free)
                        dec   b         # b: counter - 1
                        add   b         # outoffs = (outoffs + counter - 1) % 256
                        jr    NC, one_pass_only
                        ld    b, a      # save outoffs
                        cpl             # (-outoffs - 1) % 256
                        add   c         # ((-outoffs - 1) % 256 + counter) % 256, CF: 1
                        ld    c, a      # reduced counter to the end of the i/o buffer
                        ld    a, b      # restore outoffs
        one_pass_only   ld    b, 0
                        ex    de, hl    # DE: -> buffer, HL: -> source
                        ldir
                        jr    NC, no_2nd_pass
                        ld    c, a
                        inc   c         # counter = outoffs + 1
                        dec   d         # rewind buffer
                        ldir
        no_2nd_pass     ex    de, hl    # DE: -> source + min(nchars, buffer free)
                        pop   hl        # -> outoffs
                        ld    c, [hl]   # prev outoffs
                        ld    [hl], a   # new outoffs
                        sub   c         # written = new outoffs - prev outoffs
        eop             inc   hl        # -> inpoffs
                        ei if enable_intr
                        ret if subroutine
      end
    end
  end
  extend Macros

  ##############
  # I/O KERNEL #
  ##############

  # Number of channels
  mtio_buffer_chans   addr MTIO_BUFFER_CHANNELS

  # The initial bottom address of the tasks' stack space.
  initial_stack_bot   addr Multitasking::MT_STACK_BOT
  # The initial bottom address of the reserved space for I/O buffers.
  mtio_buffers_bot    initial_stack_bot - mtio_buffer_chans * 2 * +BufferIO

  # An override for Multitasking
  initial_stack_end   label

  ##
  # :call-seq:
  #       USR open_io
  #       DEF FN o(s,c$)=USR open_io
  #
  # ZX Basic API
  #
  # This endpoint should be invoked from the ZX Basic directly via USR or indirectly via FN.
  #
  #   LET stackFreeBytes=USR open_io: REM Initializes multitasking and I/O task variables, makes space for channels.
  #   
  #   1 DEF FN o(s,c$)=USR open_io: REM Opens or creates a MultitaskingIO channel and assigns a stream # to it.
  #
  #   REM Initializes channel Q and allocates buffers if called for the first time with that letter.
  #   REM Opens channel Q and assigns stream #7 to that channel. Returns previously assigned channel identifier.
  #   LET prevChan=CHR$ FN o(7,"Q")
  #
  #   REM Closes stream #7. Returns previously assigned channel identifier.
  #   LET prevChan=CHR$ FN o(7,"")
  ns :open_io do
                      call find_def_fn_arg
                      jr   C, initialize_io
                      jr   NZ, error_q.err
                      call get_stream_arg
                      push af              # save stream #
                      call find_def_fn_arg.seek_next
                      jr   NZ, error_q.err
                      call find_channel_arg
                      jp   NZ, create_channel
                      anda a               # was empty string?
                      jr   Z, close_stream
                      add  hl, bc
                      inc  hl
                      jp   open_stream_stacked
  end
  close_stream        pop  af              # stream #
                      call rom.str_data1   # BC: CHANS displacement, HL: STRMS entry address
                      push bc              # save previous CHANS displacement
                      call rom.close_hl    # closes the stream addressed by HL (restores 0 - 3 system streams)
                      pop  bc
                      jp   return_previous

  error_q             report_error 'Q Parameter error'

  ##
  # :call-seq:
  #       call initialize_io
  #
  # Initializes I/O and multitasking.
  #
  # _NOTE_:: This routine must never be called from a task!
  #
  # Modifies: +af+, +bc+, +de+, +hl+, +i+, +IFF+.
  ns :initialize_io do
                      ld   hl, [mtiovars.buffers_top]
                      ld   a, l
                      ora  h
                      ret  NZ              # initialize only once
                      ld   hl, [vars.prog] # make room for new channels below PROG
                      dec  hl
                      ld   bc, mtio_buffer_chans*5
                      call rom.make_room   # make space  HL->p nnnn DE->n ooooo
                      ld   bc, mtio_buffer_chans*5 - 1
                      ld16 hl, de
                      ld   [hl], 0
                      dec  de
                      lddr                 # clear new chan space
                      call init_multitasking
                      ld   hl, [mtiovars.tv.stack_bot]
                      ld   [mtiovars.buffers_top], hl
                      ret
  end

  ##
  # :call-seq:
  #       DEF FN w(s,n)=USR wait_io
  #
  # ZX Basic API
  #
  # This endpoint should be invoked from the ZX Basic indirectly via FN.
  #
  #   2 DEF FN w(s,n)=USR wait_io: REM Wait for I/O data availability.
  #
  #   REM Blocks an execution of a program until at least 3 characters are available to be read from an I/O buffer at stream #7.
  #   LET ReadCharsNo=FN w(7,3): REM ReadCharsNo >= 3
  #
  #   REM Blocks an execution of a program until at least 5 characters can be written to an I/O buffer at stream #7.
  #   LET WriteCharsNo=FN w(7,-5): REM WriteCharsNo >= 5
  ns :wait_io do
                      call find_def_fn_arg
                      jr   NZ, error_q.err
                      call get_stream_arg
                      ex   af, af             # save stream #
                      call find_def_fn_arg.seek_next
                      jr   NZ, error_q.err
                      call get_int8_norm_arg  # e: int, c: sign
                      inc  c
                      ex   af, af             # a: stream #, f': ZF: sign
                      call stream_channel_data
                      jr   Z, error_o.err
                      ex   af, af             # f: ZF: sign
                      jr   Z, wait_write

                      call mtiobuf_inp_handle
    if MTIO_DETECT_BREAK_KEY_ON_BLOCKING_IO
      check_read      mtio_ready?(:read, nchars:e)
                      jr   C, read_not_ready
      quit_bc         ld   c, a
                      ld   b, 0
                      ret
      read_not_ready  call cheeki_breeki
                      jr   check_read
    else
                      mtio_wait(:read, e)
      quit_bc         ld   c, a
                      ld   b, 0
                      ret
    end

    wait_write        call mtiobuf_out_handle
    if MTIO_DETECT_BREAK_KEY_ON_BLOCKING_IO
      check_write     mtio_ready?(:write, nchars:e)
                      jr   C, write_not_ready
                      sub  e
                      cpl
                      jr   quit_bc
      write_not_ready call cheeki_breeki
                      jr   check_write
    else
                      mtio_wait(:write, e)
                      sub  e
                      cpl
                      jr   quit_bc
    end
  end

  if MTIO_DETECT_BREAK_KEY_ON_BLOCKING_IO
    # reports error 8 on BREAK or yields
    cheeki_breeki     call rom.break_key
                      jp   C, task_yield      # return via yield
    error_8           report_error "8 End of file"
  end

  ##
  # :call-seq:
  #       call get_stream_arg
  #
  # Attempts to read a stream number from a FP-value addressed by +hl+.
  #
  # _NOTE_:: This routine must never be called from a task!
  #
  # Input:
  # * +hl+:: an address of a FP-value.
  #
  # On success accumulator holds a stream number: 0..15 and +hl+ will be incremented past
  # the last FP-value's byte.
  # On failure reports "O Invalid stream" error.
  #
  # Modifies: +af+, +de+, +hl+.
  ns :get_stream_arg do
                      call get_uint_arg
                      ld   a, d
                      ora  a
                      jr   NZ, error_o.err
                      ld   a, e
                      cp   16
                      ret  C
  end
  error_o             report_error 'O Invalid stream'

  ns :create_channel, use: :vars do        # A: channel identifier
                      ex   af, af
                      ld   hl, [mtiovars.buffers_top]
                      ld   a, l
                      ora  h
                      jp   Z, rom.error_j  # Invalid I/O device
                      push hl
                      xor  a
                      call find_channel    # Find first unused channel slot
                      jp   NZ, error_4     # Out of Memory
                      ex   [sp], hl        # (SP): channel -> name character, HL: [mtiovars.buffers_top]
                      ld   de, -(+BufferIO - 4)
                      dec  hl
                      ld   bc, system_inp
                      ld   [hl], b
                      dec  hl
                      ld   [hl], c
                      dec  hl
                      ld   [hl], 0xC3      # JP
                      push hl              # save input routine address
                      dec  hl
                      ld   [hl], a
                      dec  hl
                      ld   [hl], a
                      add  hl, de

                      ld   bc, system_out
                      ld   [hl], b
                      dec  hl
                      ld   [hl], c
                      dec  hl
                      ld   [hl], 0xC3      # JP
                      push hl              # save output routine address
                      dec  hl
                      ld   [hl], a
                      dec  hl
                      ld   [hl], a
                      add  hl, de
                      inc  hl              # HL: address of the 1st byte of BufferIO
                      ld   [mtiovars.buffers_top], hl

                      pop  de              # output routine
                      pop  bc              # input routine
                      pop  hl              # channel -> name character
                      ex   af, af
                      di                   # some tasks may try to find this channel...
                      ld   [hl], a         # channel name
                      dec  hl
                      ld   [hl], b         # input routine
                      dec  hl
                      ld   [hl], c
                      dec  hl
                      ld   [hl], d         # output routine
                      dec  hl
                      ld   [hl], e
                      ei                   # ...so prevent this until all data is written
  end                                      # from this moment channel is ready to be used by tasks

  open_stream_stacked pop  af              # A: stream, HL: address of CHAN entry
                                           # calculate the offset to the channel data
  open_stream         ld   de, [vars.chans]
                      anda a               # and store it in DE
                      inc  hl              # HL: 2nd byte of new channel data
                      sbc  hl, de
                      ex   de, hl          # DE: our channel address - chans + 1
                      call rom.str_data1   # BC: CHANS displacement, HL: STRMS entry address
                      ld   [hl], e         # lsb of 2nd byte of new channel data
                      inc  hl
                      ld   [hl], d         # msb of 2nd byte of new channel data
  return_previous     call channel_data_bc
                      ret  Z
                      ld   bc, 4
                      add  hl, bc
                      ld   c, [hl]         # return char code of the previously opened channel
                      ret

  # Expects channel data in HL, on return HL: address of BufferIO.inpoff
  # Reports 'O Invalid stream' if not a BufferIO channel.
  mtiobuf_inp_handle  inc  hl
                      inc  hl
  mtiobuf_out_handle  ld   a, [hl]         # get routine address
                      inc  hl
                      ld   h, [hl]
                      ld   l, a
                      ld   bc, [mtiovars.buffers_top]
                      scf
                      sbc  hl, bc          # verify if the routine is above mtiovars.buffers_top
                      jr   C, error_o.err
                      add  hl, bc          # hl: address of the BufferIO.inpoff
                      ret

  # A: stream #, returns ZF=0,HL: channel data, BC: channel offset, ZF=1: stream closed
  # Modifies: +af+, +hl+, +bc+
  stream_channel_data call rom.str_data1   # BC: CHANS displacement, HL: STRMS entry address
  # BC: channel offset+1, returns ZF=0,HL: channel data, BC: channel offset, ZF=1: stream closed
  # Modifies: +af+, +hl+, +bc+
  channel_data_bc     ld   a, b
                      ora  c
                      ret  Z
                      dec  bc
                      ld   hl, [vars.chans]
                      add  hl, bc          # channel data
                      ret

  system_chan_names   data "KSRP"
  error_f             report_error 'F Invalid file name'

  ##
  # :call-seq:
  #       call find_channel_arg
  #
  # Looks for a channel name from a FN string argument.
  #
  # _NOTE_:: This routine must never be called from a task!
  #
  # Input:
  # * +hl+:: An address of the DEF FN argument's FP-value (as a string).
  #
  # Reads a string and validates channel's name by checking string's length and converting the 1st letter
  # to the upper-case. Also checks if the channel name is valid and it's not the one of system's channel names:
  # "K", "S", "R" nor "P". If the validation fails reports error: "F Invalid file name".
  #
  # If the string is empty returns with the ZF flag set but with 0 in the accumulator.
  # On success the ZF flag is being set and the accumulator holds the channel code letter, +hl+ holds the
  # channel's record address + 4 and +bc+ 0xFFFB (twos complement: -5).
  #
  # See find_channel for more information.
  #
  # Modifies: +af+, +af'+, +bc+, +de+, +hl+.
  ns :find_channel_arg do
                      read_arg_string(d, e, b, c)
                      ld   a, b
                      ora  b
                      jr   NZ, error_f.err # b<>0
                      ora  c
                      ret  Z               # empty string
                      dec  c               # is 1-character string?
                      jr   NZ, error_f.err # LEN c$ > 1
                      ld   a, [de]         # get channel name
                      anda 0xDF            # make uppercase
                      cp   ?A.ord
                      jr   C, error_f.err
                      cp   ?Z.ord + 1
                      jr   NC, error_f.err
                      ld   hl, system_chan_names
                      ld   c, +system_chan_names # b: 0
                      cpir
                      jr   Z, error_f.err
  end

  ##
  # :call-seq:
  #       call find_channel
  #
  # Looks for a channel name.
  #
  # Input:
  # * +a+:: A channel name as an upper-case letter code.
  #
  # On success returns channel's record address + 4 in +hl+ (pointing to the channel name) and
  # the ZF flag is being set (Z). If ZF flag is clear (NZ) indicates that the channel could not be found.
  # +bc+ will always contain 0xFFFB (0x10000 - 5) so it's easy to move +hl+ back to the beginning of the channel record.
  #
  # e.g.:
  #     ld   a, "Q".ord
  #     call find_channel
  #     jr   NZ, not_found
  #     add  hl, bc # -5
  #     inc  hl     # hl points to the beginning of the channel's record
  #
  # Each channel record has the following format:
  # * two-byte address of the output routine,
  # * two-byte address of the input routine,
  # * one-byte channel code letter (a channel name).
  #
  # Modifies: +af+, +af'+, +bc+, +de+, +hl+.
  ns :find_channel do
                      ld   hl, [vars.prog]
                      2.times { dec  hl }
                      ld   bc, -5
                      cp   [hl]
                      ret  Z
                      ld   de, [vars.chans]
                      add  hl, bc
                      ex   af, af
    search_loop       ex   af, af
                      cp   [hl]
                      ret  Z
                      add  hl, bc
                      ex   af, af
                      cp16r d,e, h,l, jr_msb_c: search_loop, jr_msb_nz: not_found
                      jr   C, search_loop
    not_found         ex   af, af
                      ret
  end

  ##
  # :call-seq:
  #       call find_io_handles
  #
  # Looks for I/O handles.
  #
  # Provide a channel name as an upper-case letter code in accumulator.
  #
  # On success returns I/O buffer handle addresses in +hl+ (task input) and +de+ (task output) and sets the ZF flag (Z).
  # ZF flag is clear (NZ) when a channel with the requested name could not be found.
  #
  # Modifies: +af+, +af'+, +bc+, +de+, +hl+.
  ns :find_io_handles do
                      call find_channel
                      ret  NZ
                      dec  hl
                      ld   d, [hl]  # system input routine (is output for tasks)
                      dec  hl
                      ld   e, [hl]
                      dec  de       # output handle for tasks
                      dec  hl
                      ld   a, [hl]  # system output routine (is input for tasks)
                      dec  hl
                      ld   l, [hl]
                      ld   h, a
                      dec  hl       # input handle for tasks
                      ret
  end

  ##
  # :call-seq:
  #       call find_input_handle
  #
  # Looks for an input handle for tasks.
  #
  # Provide a channel name as an upper-case letter code in accumulator.
  #
  # On success returns I/O buffer handle address in +hl+ and sets the ZF flag (Z).
  # ZF flag is clear (NZ) when a channel with the requested name could not be found.
  #
  # Modifies: +af+, +af'+, +bc+, +de+, +hl+.
  ns :find_input_handle do
                      call find_channel
                      ret  NZ
                      add  hl, bc   # hl+= -5
                      inc  hl
                      ld   a, [hl]  # system output routine (is input for tasks)
                      inc  hl
                      ld   h, [hl]
                      ld   l, a
                      dec  hl       # input handle for tasks
                      ret
  end

  ##
  # :call-seq:
  #       call find_output_handle
  #
  # Looks for an output handle for tasks.
  #
  # Provide a channel name as an upper-case letter code in accumulator.
  #
  # On success returns I/O buffer handle address in +hl+ and sets the ZF flag (Z).
  # ZF flag is clear (NZ) when a channel with the requested name could not be found.
  #
  # Modifies: +af+, +af'+, +bc+, +de+, +hl+.
  ns :find_output_handle do
                      call find_channel
                      ret  NZ
                      dec  hl
                      ld   a, [hl]  # system input routine (is output for tasks)
                      dec  hl
                      ld   l, [hl]
                      ld   h, a
                      dec  hl       # output handle for tasks
                      ret
  end

  ##
  # :call-seq:
  #       call get_int8_norm_arg
  #
  # Attempts to read an integer in the range -255..255 from a FN argument.
  #
  # _NOTE_:: This routine must never be called from a task!
  #
  # Input:
  # * +hl+:: an address of a FP-value.
  #
  # On success register +e+ holds a positive number: 0..255 and register +c+ a sign (0 or -1).
  # +hl+ will be incremented past the last FP-value's byte.
  # On failure reports either "A Invalid argument" or "B Integer out of range" error.
  #
  # Modifies: +af+, +de+, +hl+, +c+.
  ns :get_int8_norm_arg do
                      xor  a
                      cp   [hl]
                      jr   NZ, error_a.err
                      call rom.int_fetch # de: sign normalized integer, c: sign
                      ld   a, d
                      ora  d
                      ret  Z
  end
  error_b             report_error 'B Integer out of range'

  # System --> channel output routine for BufferIO.
  ns :system_out do                                         # hl: -> BufferIO.jmp
                      dec   hl                              # hl: -> BufferIO.inpoffs (a handle address)
                      ld    e, a
    if MTIO_DETECT_BREAK_KEY_ON_BLOCKING_IO
      wait_loop       mtio_putc(e, tt:bc, subroutine: false, preserve_hl:false)
                      ret   NZ                              # success
                      call  cheeki_breeki                   # reports error on BREAK or yield
                      inc   hl                              # restore handle address
                      jr    wait_loop
    else
                      mtio_putc(e, tt:bc, not_ready: :wait, subroutine: true, preserve_hl:false)
    end
  end

  # System <-- channel input routine for BufferIO.
  ns :system_inp, use: [:vars, :vars_iy] do                 # hl: -> BufferIO.jmp
                      dec   hl                              # hl: -> BufferIO.inpoffs (a handle address)
    if MTIO_CLEAR_TV_FLAG_ON_INPUT
                      # res   3, [iy+vars_iy.tv_flag]         # reset bit 3 of TV_FLAG
                      ld    [iy+vars.tv_flag-vars_iy], 0    # clear TV_FLAG
    end
    if MTIO_DETECT_BREAK_KEY_ON_BLOCKING_IO
      wait_loop       mtio_getc(a, tt:bc, subroutine: false, preserve_hl:false)
                      ret   C                               # success
                      bit   5, [iy+vars.flagx-vars_iy]      # INPUT ?
                      ret   Z                               # not INPUT (probably INKEY$), don't block
                      call  cheeki_breeki                   # reports error on BREAK or yield
                      inc   hl                              # restore handle address
                      jr    wait_loop
    else
                      mtio_getc(a, tt:bc, subroutine: true, preserve_hl:false)
    end
  end

  end_of_mtio         label

  mt                  import  Multitasking, override: {initial_stack_end: initial_stack_end}

  mtiovars            union mtvars, TaskVarsIO
end


if __FILE__ == $0
  # :stopdoc:
  require 'zxlib/basic'

  class Echo
    include Z80
    include Z80::TAP

    import            MultitaskingIO, labels: MultitaskingIO.kernel_org, code: false, macros: true

    ns :echo_char do
      wait_loop       ld   a, ?Q.ord
                      call find_io_handles
                      jr   Z, got_handles   # HL: task input, DE: task output
                      call task_yield
                      jr   wait_loop
      got_handles     call io_getc
                      ex   de, hl           # DE: task input, HL: task output
                      call io_putc
                      ex   de, hl           # HL: task input, DE: task output
                      jr   got_handles
    end

    ns :echo_str do
      wait_loop       ld   a, ?Q.ord
                      call find_io_handles
                      jr   Z, got_handles   # HL: input handle, DE: output handle
                      call task_yield
                      jr   wait_loop

      got_handles     push de               # output handle
      forever         call io_getc
                      exx
                      ld   c, a
                      ld   b, 0
                      exx
                      ld   de, buffer
      read_loop       call io_gets
                      exx
                      add  a, b             # nchars read total
                      ld   b, a
                      sub  c                # nchars read < nchars to be read
                      jr   NC, read_done
                      exx
                      neg
                      call task_yield
                      jr   read_loop

      read_done       ld   a, c             # nchars to write
                      ld   b, 0
                      exx
                      ex   [sp], hl         # output handle <-> input handle
                      ld   de, buffer
      write_loop      call io_puts
                      exx
                      add  a, b             # nchars wrote total
                      ld   b, a
                      sub  c                # nchars wrote < nchars to be written
                      exx
                      jr   NC, write_done
                      neg
                      call task_yield
                      jr   write_loop
      write_done      ld   a, 13            # follow string with ENTER for INPUT
                      call io_putc
                      ex   [sp], hl         # input handle <-> output handle
                      jr   forever
    end

    io_getc           mtio_getc(a, tt:bc, not_ready: :wait, subroutine: true)
    io_putc           mtio_putc(a, tt:bc, not_ready: :wait, subroutine: true)
    io_gets           mtio_gets(a, check_nchars_zero:true, subroutine: true)
    io_puts           mtio_puts(a, check_nchars_zero:true, subroutine: true)
    buffer            bytes 255
  end

  mtiokernel = MultitaskingIO.new_kernel
  puts mtiokernel.debug
  echo = Echo.new 0xA000
  puts echo.debug
  puts "echo top:                         #{echo.org+echo.code.bytesize}"
  puts "Max top:                          #{mtiokernel[:initial_stack_bot] - mtiokernel[:mtio_buffer_chans]*2*MultitaskingIO::BufferIO.to_i}"
  puts "Multitasking::MT_STACK_BOT:       #{Multitasking::MT_STACK_BOT} (#{mtiokernel.org - Multitasking::MT_STACK_BOT})"
  puts "Multitasking::TASK_QUEUE_MAX:     #{Multitasking::TASK_QUEUE_MAX}"
  puts "Multitasking::MT_VARS:            #{Multitasking::MT_VARS}"
  puts "Size of TaskInfo:                 #{Multitasking::TaskInfo.to_i}"
  puts "Size of TaskVars:                 #{Multitasking::TaskVars.to_i}"
  puts "Size of TaskVars:                 #{Multitasking::TaskVars.to_i}"
  puts "Size of BufferIO:                 #{MultitaskingIO::BufferIO.to_i}"
  puts "BufferIO reserved size:           #{mtiokernel[:mtio_buffer_chans]*2*MultitaskingIO::BufferIO.to_i}"

  puts "IO Kernel size:   #{mtiokernel[:end_of_mtio]-mtiokernel.org}"
  puts "MT Kernel size:   #{mtiokernel.imports[mtiokernel[:mt]].code.bytesize}"
  puts "MTIO Kernel size: #{mtiokernel.code.bytesize}"
  %w[
    mtvars mtiovars
    mtiovars.buffers_top
    initial_stack_end initial_stack_bot
    mtio_buffer_chans mtio_buffers_bot
    open_io wait_io initialize_io
    find_io_handles find_input_handle find_output_handle
    system_out system_inp
    api terminate task_yield 
  ].each do |label|
    puts "#{label.ljust(20)}: 0x#{'%04x'%mtiokernel[label]} - #{mtiokernel[label]}"
  end

  puts "Setup: PRINT USR #{mtiokernel[:open_io]}"
  puts "Reset: PRINT USR #{mtiokernel[:api]}"
  puts "Spawn: DEF FN m(a,s)=USR #{mtiokernel[:api]}"
  puts "       LET tid=FN m(address,stacksize)"
  puts "Kill:  DEF FN t(t)=USR #{mtiokernel[:api]}"
  puts "       PRINT FN t(tid)"
  puts "Free:  DEF FN f()=USR #{mtiokernel[:api]}"
  puts "       PRINT FN f()"
  puts "Open:  DEF FN o(s,c$)=USR #{mtiokernel[:open_io]}"
  puts "       PRINT CHR$ FN o(7,\"Q\"): REM opens stream #7 and initializes channel Q"
  puts "       PRINT CHR$ FN o(7,\"\"): REM closes stream #7"
  puts "Wait:  DEF FN w(s,n)=USR #{mtiokernel[:wait_io]}"
  puts "       LET chars=FN w(7,3): LET a$=INKEY$#7+INKEY$#7+INKEY$#7"
  puts "       LET chars=FN w(7,-3): PRINT #7;\"abc\""

  program = Basic.parse_source <<-END
     0 DEF FN w(s,n)=USR #{mtiokernel[:wait_io]}: DEF FN f()=USR api: DEF FN t(t)=USR api: DEF FN m(a,s)=USR api: DEF FN o(s,c$)=USR io: LET api=VAL "#{mtiokernel[:api]}": LET io=VAL "#{mtiokernel[:open_io]}": LET free=VAL "65536-`USR`#{mtiokernel['rom.free_mem']}": RANDOMIZE USR io: LET total=USR api
     1 LET echo1=VAL "#{echo[:echo_char]}": LET echos=VAL "#{echo[:echo_str]}"
     5 CLS: PRINT AT VAL "0",VAL "0";"system free:",free,"multitask free:",total
   999 STOP
  1000 LET tid=FN m(echo1,48)
       PRINT "prev chan: ";CHR$ FN o(7,"q")
       PRINT "input ready: ";FN w(7,0)
       PRINT "output free: ";FN w(7,-255)
       IF INKEY$#7<>"" THEN PRINT "not empty": STOP
       PRINT #7;"Hello world!": PRINT FN w(7,0)
       IF FN w(7,13)<>13 THEN PRINT "expected 13 characters ready": STOP
       PRINT "ready"
       LET a$="": FOR i=1 TO 13: LET a$=a$+INKEY$#7: NEXT i
       IF a$<>"Hello world!"+CHR$ 13 THEN PRINT "does not match": STOP
       PRINT a$;FN t(tid)
       STOP
  2000 LET tid=FN m(echos,48)
       PRINT "prev chan: ";CHR$ FN o(7,"q")
       PRINT "input ready: ";FN w(7,0)
       PRINT "output free: ";FN w(7,-255)
       LET a$="Ala ma kota, a kot ma Ale i jest wspaniale i wcale nie jest nudno!"
       LET expected=LEN a$+1
       PRINT #7;CHR$ LEN a$;a$;#2;"expecting: ";expected
       IF FN w(7,expected)<>expected THEN PRINT "available ";FN w(7,0);" characters ready`<>`";expected: STOP
       LET a$="": PRINT "reading..."
  2010 IF FN w(7,0)<>0 THEN INPUT #7; LINE a$
       PRINT a$;FN t(tid)
  9998 STOP: GO TO 1000
  9999 CLEAR #{echo.org - 1}: LOAD "echo"CODE: LOAD "mtiokernel"CODE: RUN
  END

  puts program.to_source escape_keywords:true

  program.save_tap 'mtiotest', line:9999
  echo.save_tap 'mtiotest', append:true, name: 'echo'
  mtiokernel.save_tap 'mtiotest', append:true, name: 'mtiokernel'
  Z80::TAP.parse_file('mtiotest.tap') do |hb|
      puts hb.to_s
  end
end
