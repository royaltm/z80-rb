# -*- coding: BINARY -*-
here = File.expand_path('..', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'z80/math_i'
require 'z80/stdlib'
require 'zxlib/sys'
require 'zxlib/gfx'
require 'zxlib/basic'

class Multitasking
  MT_STACK_BOT = 0x9000
end

require 'utils/multitasking'

class Program
  include Z80
  include Z80::TAP

  MTKernel = Multitasking.new_kernel
  MTYield = MTKernel['task_yield']
  def self.mtyield(condition=nil)
    if condition
            call  condition, MTYield
    else
            call  MTYield
    end
  end

  ###########
  # Exports #
  ###########

  export start
  export coords
  export fill

  ###########
  # Imports #
  ###########

  macro_import  Z80Lib
  macro_import  Z80MathInt
  macro_import  ZXGfx
  import        ZXSys, macros: true

  ##########
  # Macros #
  ##########

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
  start             jr   getparams
  coords            data ZXSys::Coords, {x: 0, y: 0}
  getparams         call fn_argn                    # is this an FN call with arguments?
                    jr   C, fill                    # called via USR

  error_q           report_error_unless Z, 'Q Parameter error'
                    call get_arg_int8               # x argument as 8bit unsigned integer into A
                    ex   af, af                     # save x
                    call fn_argn.seek_next
                    jr   NZ, error_q.err
                    call get_arg_int8               # y argument as 8bit unsigned integer into A
                    neg
                    add  175                        # convert PLOT y argument
                    ld   h, a                       # H: y
                    ex   af, af                     # restore x
                    ld   l, a                       # L: x
                    jr   skipcoords

  fill              ld   hl, [coords]               # H: y, L: x
                    mtyield
  skipcoords        ld   a, h
                    cp   192
                    report_error_unless C, '5 Out of screen'

                    xytoscr h, l, h, l, b, c
                    xor  a
                    push af                         # place marker
                    inc  sp                         # only higher byte on stack
  # Create pixel mask
                    inc  b
                    scf
  loopmask          rra
                    djnz loopmask
                    ld   b, a
                    ld   ix, 0
                    ex   af, af
                    xor  a
                    ex   af, af
                    jp   check0.skipdupmask

  loop0             pop  hl                         # plotted pixel address
                    ex   af, af
                    sub  8
                    mtyield C
                    ex   af, af
  # Go right
                    ld   e, l                       # E: saved L
                    ld   d, a                       # D: saved mask
                    nextpixel l, a, skipright       # L: new address B: new mask
  # Check right
                    checkpixel mask:b, counter:ix
  # Go left
  skipright         ld   l, e                       # restore L
                    ld   a, d                       # restore mask
                    prevpixel l, a, skipleft        # L: new address B: new mask
  # Check left
                    checkpixel mask:b, counter:ix
  # Go down
  skipleft          ld   b, d                       # B: saved restored mask
                    ld   l, e                       # restore L
                    ld   d, h                       # D: saved H
                    nextline h, l, skipdown
  # Check down
                    checkpixel mask:b, counter:ix
  # Go down
  skipdown          ld16 hl, de                     # restore HL
                    prevline h, l, skipup

  check0            checkpixel mask:b, counter:ix
  skipup            dec  sp
                    pop  af                         # A: plotted pixel mask or 0 - end marker
                    ora  a
                    jp   NZ, loop0                  # return on marker
                    ld16 bc, ix
                    ret

  ###############
  # Subroutines #
  ###############

  fn_argn           find_def_fn_args 1, cf_on_direct: true

  get_arg_int8      read_positive_int_value d, e
                    inc  hl                       # point to a next argument possibly
                    report_error_unless Z, 'A Invalid argument'
                    ld   a, d
                    ora  a
                    report_error_unless Z, '6 Number too big'
                    ld   a, e
                    ret
end

fill = Program.new 0x8E00
puts fill.debug

mtkernel = Program::MTKernel
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

puts "Total stack space for tasks: #{mtkernel[:initial_stack_end] - 0xE000}"
puts "\n ZX Basic API:"
puts "Setup: PRINT USR #{mtkernel[:api]}"
puts "Spawn: DEF FN m(a,s) = USR #{mtkernel[:api]}"
puts "       LET tid = FN m(address,stacksize)"
puts "Kill:  DEF FN t(t) = USR #{mtkernel[:api]}"
puts "       PRINT FN t(tid)"
puts "Free:  DEF FN f() = USR #{mtkernel[:api]}"
puts "       PRINT FN f()"
puts "\n Task API:"
puts "Yield: call #{mtkernel[:task_yield]}"
puts "Kill:  jp   #{mtkernel['terminate']}"

%w[start fill coords.x coords.y].each {|n| puts "#{n.ljust(15)} #{fill[n]}"}

tapfilename = 'examples/multifill.tap'
program = Basic.parse_source IO.read('examples/multifill.bas'), start: 9999
program.save_tap tapfilename
puts "="*32
puts program
puts "="*32
fill.save_tap tapfilename, append: true, name: 'fill'
mtkernel.save_tap tapfilename, append: true, name: 'mtkernel'

Z80::TAP.parse_file(tapfilename) do |hb|
    puts hb.to_s
end
