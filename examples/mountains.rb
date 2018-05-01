# -*- coding: BINARY -*-
here = File.expand_path('..', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'z80/math_i'
require 'z80/stdlib'
require 'zxlib/gfx'

class Program
  include Z80
  include Z80::TAP

  ###########
  # Exports #
  ###########

  export mountains

  ###########
  # Imports #
  ###########

  macro_import  Z80Lib
  macro_import  Z80MathInt
  macro_import  ZXGfx

  ########
  # Vars #
  ########

  # ROM procedures
  cl_all        addr 0x0DAF

  # VARS
  frames        addr 0x5C78
  rnd_seed      addr 0x5C76
  bordcr        addr 0x5C48

  # HW
  scr_p         addr 0x4000
  scrlen        6144
  attr_p        addr scr_p + scrlen
  attrlen       6912 - 6144

  ##########
  # Macros #
  ##########

  # ZF=0 if any key is being pressed
  macro :key_pressed? do
                xor  a
                inp  a, (254)
                cpl
                anda 0x1F
  end

  macro :adjust_mountain do |_, mountainy, upper, lower, variant|
                ld   de, mountainy
                ld   a, [de]
                ld   c, 0
                case variant
                when :A
                  sla  h
                when :B
                  srl  h
                else
                  raise ArgumentError
                end
                adc  c
                cp   upper
                ccf
                sbc  c
                case variant
                when :A
                  srl  l
                when :B
                  sla  l
                else
                  raise ArgumentError
                end
                sbc  c
                cp   lower unless lower == 0
                adc  c
                ld   [de], a
  end

  macro :scroll_lines do |_, lines, invert|
                  ld   c, a
                  ld   b, lines
    bglines       push hl
                  cp   b
                  ccf if invert
                  rl   [hl]
                  31.times do
                    dec  hl
                    rl   [hl]
                  end
                  pop  hl
                  nextline h, l, false
                  ld   a, c
                  djnz bglines
  end

  ########
  # DOTS #
  ########

  ns :mountains do
                  exx
                  push hl

                  halt
                  call release_key
                  halt
                  call seed_entropy

    halt_loop     halt
    mloop         call next_rnd
                  bit  1, [iy + 0x3E]
                  jp   Z, scrollfg
    scrollbg      adjust_mountain(bgmountain.y, 48, 0, :A)
                  ld   hl, xy_to_pixel_addr(255, 0)
                  scroll_lines(48, true)

    scrollfg      bit  0, [iy + 0x3E]
                  jp   Z, halt_loop
                  ld   hl, [rnd_seed]
                  adjust_mountain(fgmountain.y, 64, 16 + 3, :B)
                  ld   hl, xy_to_pixel_addr(255, 64)
                  scroll_lines(64, false)

                  ld   c, a
                  ld   hl, rnd_seed
                  ld   a, [hl]
                  inc  hl
                  xor  [hl]
                  anda 15
                  add  128 + 3
                  sub  c
                  ytoscr(a, h, l, c)
                  ld   a, 31
                  add  l
                  ld   l, a
                  set  0, [hl]

                  key_pressed?
                  jp   Z, mloop

                  pop  hl
                  exx

                  jp   cleanup_scr
  end

  ###############
  # Subroutines #
  ###############

  # clear screen using CL-ALL and reset border
  cleanup_scr   call cl_all
                ld   a, [bordcr]
                call set_border_cr
                ret

  # clears screen area with border and attributes set according to register a
  clear_screen  clrmem  scr_p, scrlen
                clrmem  attr_p, attrlen, a
  set_border_cr anda 0b00111000
                3.times { rrca }
                out  (254), a
                ret

  # waits until no key is being pressed
  release_key   halt
                key_pressed?
                jr   NZ, release_key
                ret

  # create seed from frames and register r
  seed_entropy  ld   a, [frames + 2]
                ld   h, a
                ld   a, r
                xor  h
                ld   hl, [frames]
                xor  h
                ld   h, a
                ld   de, [rnd_seed]
                sbc  hl, de
                ld   a, l
                anda 7
                out  (254), a
                jr   set_rnd_seed
  # get next pseudo-random number and update the seed
  next_rnd      ld hl, [rnd_seed]
  # get next pseudo-random number seeded in hl and update the seed
  next_rnd_hl   rnd
  # set seed from hl
  set_rnd_seed  ld [rnd_seed], hl
                ret


  ########
  # Data #
  ########

  class Position < Label
    y   byte
  end

  frame_state   dw   0
  bgmountain    data Position, {y: 32}
  fgmountain    data Position, {y: 32}
  # river         data Position, {y: 160} # 

  pixels        bytes (0..7).map{|b| 0x80 >> b }
  masks         bytes (0..7).map{|b| ~(0x80 >> b) }

end

mountains = Program.new 0x8000
puts mountains.debug

mountains.save_tap('mountains')

Z80::TAP.read_chunk('examples/loader.tzx') do |hb|
    File.open('examples/mountains.tap', 'wb') do |f|
        f.write hb.to_tap
        f.write mountains.to_tap('mountains')
    end
end
