# -*- coding: BINARY -*-
here = File.expand_path('..', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'z80/math_i'
require 'z80/stdlib'
require 'zxlib/sys'
require 'zxlib/gfx'

class Program
  include Z80
  include Z80::TAP

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
                    jr   check0.skipdupmask

  loop0             pop  hl                         # plotted pixel address
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

# fill = Program.new 0xFE00
fill = Program.new 0x8000
puts fill.debug

%w[start fill coords.x coords.y].each {|n| puts "#{n.ljust(15)} #{fill[n]}"}

fill.save_tap('fill')

Z80::TAP.parse_file('fill.tap') do |hb|
    puts hb.to_s
end
