# -*- coding: BINARY -*-
here = File.expand_path('../lib', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'z80/math_i'
require 'z80/stdlib'
require 'zxlib/sys'
require 'zxlib/basic'
require 'zxutils/zx7'

class Program
  include Z80
  include Z80::TAP

  ###########
  # Exports #
  ###########

  export start

  ###########
  # Imports #
  ###########

  macro_import  Stdlib
  macro_import  MathInt
  label_import  ZXSys
  macro_import  ZX7

  ########
  # Vars #
  ########

  shadowscr     addr 0xC000

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
  # MAIN #
  ########

  ns :start do
                  exx
                  push hl

                  ld   a, 0b00000111
                  call clear_screen

                  call release_key
                  halt
                  call seed_entropy

                  ld   hl, horse
                  ld   de, shadowscr
                  call decompress

                  di
                  call depixelize
                  ld   a, 0b00111000
                  call set_border_cr

                  ei
                  call wait_any_key
                  halt
                  xor  a
                  call set_border_cr
                  call seed_entropy

                  di
                  call mess_with_it
                  ld   a, 0b00111000
                  call set_border_cr

                  ei
                  call wait_any_key
                  halt
                  xor  a
                  call set_border_cr
                  call seed_entropy

                  di
                  call mess_with_it
                  ld   a, 0b00111000
                  call set_border_cr

                  ei
                  call wait_any_key

                  pop  hl
                  exx
                  jp   cleanup_scr

    ns :depixelize do
                  ld   bc, 256*(192+24)  # number of pixels
                  exx
                  ld   ix, masks
                  ld   hl, [vars.seed]
      mloop       rnd
                  ld   a, h
                  cp   192+24
                  jr   NC, mloop
                  push hl

                  anda 0b00000111
                  ld   c, a       # 00000uuu
                  xor  h          # YYyyy000
                  rrca            # 0YYyyy00
                  scf
                  rra             # 10YYyyy0
                  rrca            # 010YYyyy
                  ld   h, a

      write_pixel ld   a, c
                  ld   [fetch_mask + 2], a
      fetch_mask  ld   c, [ix + 0]
                  ld   a, [hl]
                  set  7, h
                  xor  [hl]
                  anda c
                  xor  [hl]
                  res  7, h
                  ld   [hl], a

      next_iter   pop  hl
                  exx
                  dec  bc
                  ld   a, c
                  ora  b
                  ret  Z
                  exx
                  jp   mloop
    end

    ns :mess_with_it do
                  ld   bc, 256*(192+24)  # number of pixels
                  exx
                  ld   ix, pixels
                  ld   hl, [vars.seed]
      mloop       rnd
                  ld   a, h
                  cp   192+24
                  jr   NC, mloop
                  push hl

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
      fetch_mask  ld   a, [ix + 0]
                  xor  [hl]
                  ld   [hl], a

      next_iter   pop  hl
                  exx
                  dec  bc
                  ld   a, c
                  ora  b
                  ret  Z
                  exx
                  jp   mloop
    end
  end

  ###############
  # Subroutines #
  ###############

  # clear screen using CL-ALL and reset border
  cleanup_scr   call rom.cl_all
                ld   a, [vars.bordcr]
                call set_border_cr
                ret

  # clears screen area with border and attributes set according to register a
  clear_screen  clrmem  mem.screen, mem.scrlen
                clrmem  mem.attrs, mem.attrlen, a
  set_border_cr anda 0b00111000
                3.times { rrca }
                out  (254), a
                ret

  # waits until no key is being pressed
  release_key   halt
                key_pressed?
                jr   NZ, release_key
                ret

  # waits until any key is being pressed
  wait_any_key  halt
                key_pressed?
                jr   Z, wait_any_key
                jr   release_key

  # create seed from frames and register r
  seed_entropy  ld   a, [vars.frames + 2]
                ld   h, a
                ld   a, r
                xor  h
                ld   hl, [vars.frames]
                xor  h
                ld   h, a
                ld   de, [vars.seed]
                sbc  hl, de
  # set seed from hl
  set_rng_seed  ld [vars.seed], hl
                ret
  # ZX7 decoder by Einar Saukas, Antonio Villena & Metalbrain
  # * hl: source address (compressed data)
  # * de: destination address (decompressing)
  decompress    dzx7_standard

  ########
  # Data #
  ########


  pixels        bytes (0..7).map{|b| 0x80 >> b }
  masks         bytes (0..7).map{|b| ~(0x80 >> b) }

  horse         import_file 'examples/horse.scr', check_size: 6912, pipe: ZX7.method(:compress)

end

horse = Program.new 0x8000
puts horse.debug
puts "size: #{horse.code.bytesize}"
puts "ends: #{(horse.org + horse.code.bytesize).to_s 16}"
program = Basic.parse_source('1 CLEAR VAL "32767": LOAD ""CODE : RANDOMIZE USR VAL "32768"', start: 1)
puts "="*32
puts program
puts "="*32
program.save_tap 'examples/horse'
horse.save_tap('examples/horse', append: true)
