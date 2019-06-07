# -*- coding: BINARY -*-
here = File.expand_path('../lib', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'z80/math_i'
require 'z80/stdlib'
require 'zxlib/gfx'
require 'zxlib/basic'

class Program
  include Z80
  include Z80::TAP

  ###########
  # Exports #
  ###########

  export dots

  ###########
  # Imports #
  ###########

  macro_import  Z80Lib
  macro_import  MathInt
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

  ########
  # DOTS #
  ########

  ns :dots do
                  exx
                  push hl

                  ld   a, 0xB6      # OR (HL)
                  ld   [gfx_fn], a
                  ld   ix, pixels

                  ld   a, 0b01000111
                  call clear_screen
                  call release_key
                  halt
    entropy_loop  call seed_entropy
    mloop         call next_rnd
                  ld   a, h
                  cp   192
                  jr   NC, mloop

                  anda 0b00000111
                  ld   c, a       # 00000uuu
                  xor  h          # YYyyy000
                  rrca            # 0YYyyy00
                  scf
                  rra             # 10YYyyy0
                  rrca            # 010YYyyy
                  ld   h, a

                  ld   a, c
                  ld   [fetch_mask + 2], a
    fetch_mask    ld   a, [ix + 0]
    gfx_fn        ora  [hl]
                  ld   [hl], a
                  key_pressed?
                  jr   Z, mloop
                  ld   a, [gfx_fn]
                  cp   0xB6         # OR (HL)
                  jr   NZ, skip_and
                  ld   a, 0xA6      # AND (HL)
                  ld   [gfx_fn], a
                  ld   ix, masks
                  jr   wait_and_loop
    skip_and      cp   0xA6         # AND (HL)
                  jr   NZ, skip_xor
                  ld   a, 0xAE      # XOR (HL)
                  ld   [gfx_fn], a
                  ld   ix, pixels
    wait_and_loop call release_key
                  jr   entropy_loop
        skip_xor  label

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


  pixels        bytes (0..7).map{|b| 0x80 >> b }
  masks         bytes (0..7).map{|b| ~(0x80 >> b) }

end

dots = Program.new 0x8000
puts dots.debug

File.open('examples/dots.tap', 'wb') do |f|
    prog = Basic.parse_source('1 CLEAR VAL "32767": LOAD ""CODE : RANDOMIZE USR VAL "32768"', start: 1)
    puts prog
    f.write prog.to_tap('dots')
    f.write dots.to_tap('dots')
end
