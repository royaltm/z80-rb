# -*- coding: BINARY -*-
here = File.expand_path('../lib', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'zxlib/sys'
require 'zxlib/gfx'
require 'zxlib/basic'
require 'zxutils/multitasking_io'

class Program
  include Z80
  include Z80::TAP

  CHAN_NAME = "F"

  MT_OVERRIDES = {initial_stack_bot: 0x8E3B, mtvars: mmtvars, mtiovars: mmtiovars}

  ###########
  # Exports #
  ###########

  export start
  export drain
  export fill

  ###########
  # Imports #
  ###########

  macro_import  Stdlib
  macro_import  MathInt
  macro_import  ZXGfx
  import        ZXSys, macros: true
  import        MultitaskingIO, :mtio, code: false, macros: true, labels: MultitaskingIO.kernel_org, override: MT_OVERRIDES

  mmtvars       addr mem.mmaps, Multitasking::TaskVars
  mmtiovars     union mmtvars, MultitaskingIO::TaskVarsIO
  task_yield    addr mtio.task_yield

  ##########
  # Macros #
  ##########

  def self.mtyield(condition=nil)
    if condition
            call  condition, task_yield
    else
            call  task_yield
    end
  end

  macro :checkpixel do |eoc, mask:a, counter:nil|
    if mask == a
                    ld   b, a
    elsif mask == b
                    ld   a, b
    else
        raise ArgumentError, "mask should be: a or b"
    end
    skipdupmask     ld   c, [hl]
                    anda c
                    jr   NZ, eoc
    # Plot pixel
                    ld   a, b
                    ora  c
                    ld   [hl], a
    # Store for later
                    push hl
                    push bc
                    inc  sp
                    inc  counter if counter # pixel counter
  end

  macro :nextpixel do |eoc, l, mask, bcheck|
    if mask == a
                    rrca
                    ld   b, a
    else
                    rrc  mask
    end
                    jr   NC, eoc
                    inc  l
                    ld   a, l
                    anda 31
                    jr   Z, bcheck
  end

  macro :prevpixel do |eoc, l, mask, bcheck|
    if mask == a
                    rlca
                    ld   b, a
    else
                    rlc  mask
    end
                    jr   NC, eoc
                    ld   a, l
                    anda 31
                    jr   Z, bcheck
                    dec  l
  end

  ########
  # FILL #
  ########

  ##
  # Usage:
  # 
  #     POKE coords.x, x: POKE coords.y, y: let pixels = USR start
  # where x is a horizontal start pixel 0..255 (left to right), y is a vertical start pixel 0..192 (top to bottom)
  # or
  #     1 DEF FN f(x,y)=USR start
  #     FN f(x, y)
  # where x is a horizontal start pixel 0..255 (left to right), y is a vertical start pixel 0..175 (bottom to top)
  start             task?(mtvars:mtio.mtvars)
                    jr   Z, fill_sys

  # Task (asynchronous) call
  ns :fill_io do
                    ld   a, CHAN_NAME.ord
    handler_loop    call mtio.find_input_handle
                    jr   Z, wait_loop               # HL: task output
                    mtyield
                    jr   handler_loop
    wait_loop       mtio_wait(:read, 2, enable_intr:false)
                    call getc_io                    # no need to check: interrupts are disabled, at least 2 chars ready
                    ld   e, a                       # x
                    call getc_io                    # y
                    ei
                    cp   192                        # out of screen?
                    ret  NC                         # panic
                    ld   d, a
                    jr   fill
  end

  # Drain I/O buffers
  drain             ld   a, CHAN_NAME.ord
                    call mtio.find_io_handles
                    ret  NZ
                    mtio_drain(enable_intr:false)
                    ex   de, hl
                    mtio_drain(disable_intr:false)
                    ret

  # Regular system (synchronous) call
  fill_sys          call mtio.find_def_fn_arg       # is this an FN call with arguments?
                    jr   C, drain                   # drain I/O input buffer if called not via FN
  error_q           report_error_unless Z, 'Q Parameter error'
                    call get_arg_int8               # x argument as 8bit unsigned integer into A
                    ex   af, af                     # save x
                    call mtio.find_def_fn_arg.seek_next
                    jr   NZ, error_q.err
                    call get_arg_int8               # y argument as 8bit unsigned integer into A
                    neg
                    add  175                        # convert PLOT y argument
                    cp   192
                    report_error_unless C, '5 Out of screen'
                    ld   d, a                       # H: y
                    ex   af, af                     # restore x
                    ld   e, a                       # L: x

  ns :fill do
                    xytoscr d, e, ah:h, al:l, s:b, t:c
                    xor  a
                    push af                         # place marker
                    inc  sp                         # only higher byte on stack
    # Create pixel mask
                    inc  b
                    scf
    loopmask        rra
                    djnz loopmask
                    ld   b, a
                    ld   ix, 0
                    ex   af, af                     # make task switching more often
                    xor  a                          # make task switching more often
                    ex   af, af                     # make task switching more often
                    jp   check0.skipdupmask

    loop0           pop  hl                         # plotted pixel address
                    ex   af, af                     # make task switching more often
                    sub  8                          # make task switching more often
                    mtyield C                       # make task switching more often
                    ex   af, af                     # make task switching more often
    # Go right
                    ld   e, l                       # E: saved L
                    ld   d, a                       # D: saved mask
                    nextpixel l, a, skipright       # L: new address B: new mask
    # Check right
                    checkpixel mask:b, counter:ix
    # Go left
    skipright       ld   l, e                       # restore L
                    ld   a, d                       # restore mask
                    prevpixel l, a, skipleft        # L: new address B: new mask
    # Check left
                    checkpixel mask:b, counter:ix
    # Go down
    skipleft        ld   b, d                       # B: saved restored mask
                    ld   l, e                       # restore L
                    ld   d, h                       # D: saved H
                    nextline h, l, skipdown
    # Check down
                    checkpixel mask:b, counter:ix
    # Go down
    skipdown        ld16 hl, de                     # restore HL
                    prevline h, l, skipup

    check0          checkpixel mask:b, counter:ix
    skipup          dec  sp
                    pop  af                         # A: plotted pixel mask or 0 - end marker
                    ora  a
                    jp   NZ, loop0                  # return on marker

                    ld16 bc, ix                     # pixel counter
                    task_id(d, e, check_if_system: true, mtvars:mtio.mtvars)
                    ret  Z

                    push bc                         # pixel counter
                    push de                         # task id
                    ld   a, CHAN_NAME.ord
                    call mtio.find_io_handles
                    jp   NZ, mtio.terminate         # panic if no I/O
                    push hl                         # inp_handler
                    ex   de, hl                     # HL: out_handler, DE: inp_handler
                    mtio_wait(:write, 4, enable_intr:false)
                    ex   de, hl                     # DE: out_handler, HL: inp_handler
                    ld   hl, 2
                    add  hl, sp                     # address of task id + pixel counter on a stack (safe because di)
                    ex   de, hl                     # DE: data to output, HL: out_handler
                    mtio_puts(4, subroutine:false, disable_intr:false, enable_intr:true)
                    pop  hl                         # inp_handler
                    pop  af
                    pop  af                         # erase task id and pixel count
                    jp   fill_io.wait_loop
  end

  # Make room for mtiovars and clear the buffers_top entry.
  # Must be called once before multitasking will be intialized.
  make_mtvars_space ld   hl, mtio.mtiovars-1
                    ld   bc, +mtio.mtiovars
                    call rom.make_room
                    ld   [mtio.mtiovars.buffers_top], bc
                    ret

  ###############
  # Subroutines #
  ###############

  getc_io           mtio_getc(a, tt:bc, disable_intr:false, enable_intr:false)

  get_arg_int8      call mtio.get_uint_arg
                    ld   a, d
                    ora  a
                    report_error_unless Z, '6 Number too big'
                    ld   a, e
                    ret
end

mtiokernel = MultitaskingIO.new_kernel(override: Program::MT_OVERRIDES)
puts mtiokernel.debug

fill = Program.new mtiokernel[:mtio_buffers_bot] - Program.code.bytesize
puts fill.debug

puts "MultitaskingIO size: #{mtiokernel.code.bytesize}"
%w[
  mtvars
  mtiovars
  mtiovars.buffers_top
  initial_stack_bot
  initial_stack_end
  mtio_buffers_bot
  open_io
  wait_io
  api
  task_yield
  terminate
].each do |label|
  puts "#{label.ljust(20)}: 0x#{mtiokernel[label].to_s 16} - #{mtiokernel[label]}"
end

puts "Total stack space for tasks: #{mtiokernel[:initial_stack_end] - mtiokernel[:initial_stack_bot]}"
puts "\n ZX Basic API:"
puts "Setup: PRINT USR #{mtiokernel[:open_io]}"
puts "Reset: PRINT USR #{mtiokernel[:api]}"
puts "Spawn: DEF FN m(a,s)=USR #{mtiokernel[:api]}"
puts "       LET tid=FN m(address,stacksize)"
puts "Kill:  DEF FN t(t)=USR #{mtiokernel[:api]}"
puts "       PRINT FN t(tid)"
puts "Free:  DEF FN f()=USR #{mtiokernel[:api]}"
puts "       PRINT FN f()"
puts "Open:  DEF FN o(s,c$)=USR #{mtiokernel[:open_io]}"
puts "       PRINT FN CHR$ o(7,\"Q\")"
puts "Wait:  DEF FN w(s,n)=USR #{mtiokernel[:wait_io]}"
puts "       LET chars=FN w(7,3): LET a$=INKEY$#7+INKEY$#7+INKEY$#7"
puts "       LET chars=FN w(7,-3): PRINT #7;\"abc\""
puts "\n Task API:"
puts "Yield: call #{mtiokernel[:task_yield]}"
puts "Kill:  jp   #{mtiokernel[:terminate]}"

puts "\nCLEAR #{fill.org - 1}"
%w[start drain fill mtio.mtvars mtio.mtiovars mtio.mtiovars.buffers_top mtio.find_def_fn_arg].each {|n| puts "#{n.ljust(15)} #{fill[n]}"}

tapfilename = 'examples/multifill.tap'

header = <<-END
   1 DEF FN w(s,n)=USR #{mtiokernel[:wait_io]}: DEF FN f()=USR api: DEF FN t(t)=USR api: DEF FN l(x,y)=USR fill: DEF FN m(a,s)=USR api: DEF FN o(s,c$)=USR io: DEF FN z()=VAL "65536-`USR`#{mtiokernel['rom.free_mem']}": LET fill=VAL "#{fill[:start]}": LET api=VAL "#{mtiokernel[:api]}": LET io=VAL "#{mtiokernel[:open_io]}": RANDOMIZE USR io: LET total=USR api
END
footer = <<-END
9998 STOP 
9999 CLEAR VAL "#{fill.org-1}": LOAD "fill"CODE : LOAD "mtiokernel"CODE : RANDOMIZE USR VAL "#{fill[:make_mtvars_space]}": RUN
END
program = Basic.parse_source header + IO.read('examples/multifill.bas') + ?\n + footer, start: 9999
program.save_tap tapfilename
puts "="*32
puts program
puts "="*32
fill.save_tap tapfilename, append: true, name: 'fill'
mtiokernel.save_tap tapfilename, append: true, name: 'mtiokernel'

Z80::TAP.parse_file(tapfilename) do |hb|
    puts hb.to_s
end
