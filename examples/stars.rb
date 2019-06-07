# -*- coding: BINARY -*-
here = File.expand_path('../lib', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'zxlib/gfx/draw'
require 'zxlib/basic'

class Stars
  include Z80
  include Z80::TAP

  import       ZXLib::Sys, macros: true, code: false, labels: true
  macro_import Stdlib
  macro_import MathInt
  macro_import ZXGfxDraw

  BG_STARS = 256
  NSTARS = 64
  SPEED = 3
  MAX_SPEED = 32
  STAR_COORD_HI_MASK = 0b00111111

  class Star < Label
        x word
        y word
        z byte
     mask byte
    saddr word
  end

  with_saved :start, :exx, hl, ret: true do |eoc|
                      call make_stars
    release_key       key_pressed?
                      jr   NZ, release_key

                      ld   a, 0b01000111
                      call clear_screen
                      call background_stars

    mloop             call move_stars
                      key_pressed?(0xf7)       # [1]-[5]
                      rra                      # [1]
                      jr   C, inc_speed
                      rra                      # [2]
                      jr   C, dec_speed
                      jr   NZ, quit

                      key_pressed?(0x08)
                      jr   Z, mloop

                      # clear screen using CL-ALL and reset border
    quit              call rom.cl_all
                      ld   a, [vars.bordcr]
                      call set_border_cr
                      jr   eoc

    dec_speed         ld   a, [move_stars.speed_p + 1]
                      anda a
                      jr   Z, mloop
                      dec  a
                      jr   set_speed

    inc_speed         ld   a, [move_stars.speed_p + 1]
                      inc  a
                      cp   MAX_SPEED
                      jr   NC, mloop
    set_speed         ld   [move_stars.speed_p + 1], a
                      jr   mloop
  end

  macro :divmod16_8 do |eoc, th, tl, m| # divide (tt)/m
                      divmod th, m, check0:eoc, check1:eoc
                      divmod tl, m, clrrem:false
                      anda a # clear CF
  end

  macro :persp_coord do |_, th, tl| # z: c, tt: x -> hl: x/z, CF:div by 0, a': sgn(x-128)
                      rlc  tl               # we just need one random bit for a sign
                      ex   af, af           # CF: sgn(x)
                      divmod16_8 th, tl, c  # (x*4)/z
  end

  ns :move_stars do
                      ld   [restore_sp + 1], sp
                      di
                      ld   sp, stars
    nstars_p          ld   b, NSTARS

    sloop             exx

                      pop  hl                    # x
                      pop  de                    # y
                      pop  bc                    # z

                      ld   a, b                  # mask
                      exx
                      pop  hl                    # screen address
                      xor  [hl]
                      ld   [hl], a               # clear star
                      exx

                      persp_coord h, l           # hl: x/z
                      jr   C, skip_star          # z=0
                      ld   a, h
                      anda a
                      jr   NZ, skip_star         # hl >= 256
                      ex   af, af
                      jr   C, x_negative
                      ld   a, l                  # x  >= 0
                      add  128                   # x to screen coordinates
                      jr   NC, skip_neg_x        # xp <  256
                                                 # xp >= 256
    skip_star         ld   a, c                  # c: z
                      ex   af, af
                      xor  a
                      ld   d, a
                      ld   h, a
                      ld   l, a
                      jp   skip_plot

    x_negative        ld   a, 128                # x  <  0
                      sub  l                     # x to screen coordinates
                      jr   C, skip_star          # xp <  0

    skip_neg_x        ld   l, a                  # l: xp

                      ex   de, hl                # e: xp, hl: y
                      persp_coord h, l           # hl: y/z
                      jr   C, skip_star          # z=0
                      ld   a, h
                      anda a
                      jr   NZ, skip_star         # hl >= 256
                      ex   af, af
                      jr   C, y_negative
                      ld   a, l                  # y  >= 0
                      add  96                    # y to screen coordinates
                      jr   NC, skip_neg_y        # yp <  256
                      jr   skip_star             # yp >= 256
    y_negative        ld   a, 96                 # y  <  0
                      sub  l                     # y to screen coordinates
                      jr   C, skip_star          # yp <  0
    skip_neg_y        cp   192
                      jr   NC, skip_star         # yp >= 192

                      ld   h, a                  # yp
                      ld   l, e                  # xp
                      ld   a, c                  # a: z
                      ex   af, af                # a': z
    plot              plot_pixel(l, h, preshifted_pixel, fx: :skip, scraddr:0x4000)
                      ld   d, a                  # pixel
                      xor  [hl]
                      ld   [hl], a
    skip_plot         push hl                    # screen addr
                      ex   af, af                # a: z
    speed_p           sub  SPEED
                      ld   e, a                  # z
                      push de
                      jr   C, reinitialize_star
                      4.times { inc  sp }

    back_to_loop      exx
                      dec  b
                      jp   NZ, sloop

    restore_sp        ld   sp, 0
                      ei
                      ret

    reinitialize_star ld   a, 0b00010000
                      out  (254), a
                      call rand_seed
                      ld   a, h
                      anda STAR_COORD_HI_MASK
                      ld   d, a
                      ld   e, l
                      push de                    # y
                      call rand_fn_hl
                      ld   [seed], hl
                      ld   a, h
                      anda STAR_COORD_HI_MASK
                      ld   d, a
                      ld   e, l
                      push de                    # x
                      ld   hl, +stars
                      add  hl, sp
                      ld   sp, hl
                      xor  a
                      out  (254), a
                      jr   back_to_loop
  end

  # clears screen area with border and attributes set according to register a
  clear_screen        clrmem  mem.attrs, mem.attrlen, a
  set_border_cr       anda 0b00111000
                      3.times { rrca }
                      out  (254), a
                      call clearscr
                      ret

  # clear pixel screen
  clearscr            clrmem  mem.screen, mem.scrlen, 0
                      ret

  ns :make_stars do
                      ld   [restore_sp + 1], sp
                      di
                      ld   sp, stars_end
                      ld   hl, [vars.seed]
                      exx
                      ld   b, 256                 # 256 stars max
                      ld   de, 0
    mloop             push de                     # screen addr
                      exx
                      call rand_fn_hl
                      ld   e, l
                      ld   d, 0
                      push de                     # mask|z
                      call rand_fn_hl
                      ld   e, l
                      ld   a, h
                      anda STAR_COORD_HI_MASK
                      ld   d, a
                      push de                     # y
                      call rand_fn_hl
                      ld   e, l
                      ld   a, h
                      anda STAR_COORD_HI_MASK
                      ld   d, a
                      push de                     # x
                      exx
                      djnz mloop
                      exx
                      ld   [seed], hl

    restore_sp        ld   sp, 0
                      ei
                      ret
  end

  ns :background_stars do
    bstars_p          ld   b, BG_STARS
                      ld   ixl, 0b00000001
                      exx
                      call rand_seed
                      jr   skip
    bloop             exx
    next_plot         call rand_fn_hl
    skip              ld   a, h
                      cp   192
                      jr   NC, change_color
                      push hl
                      plot_pixel(l, h, preshifted_pixel, fx: :or, with_attributes: :overwrite, color_attr: ixl, scraddr:0x4000)
                      pop  hl
                      exx
                      djnz bloop
                      exx
                      ld   [seed], hl
                      ret

    change_color      ld   a, h
                      xor  l
                      anda 0b01000011
                      jr   NZ, set_color
                      ld   a, 0b00000001
    set_color         ld   ixl, a
                      jr   next_plot
  end

  rand_seed           ld   hl, 0
  rand_fn_hl          rnd
                      ret

  seed                union rand_seed+1, 2
  nstars              union move_stars.nstars_p + 1, 1
  bstars              union background_stars.bstars_p + 1, 1

  preshifted_pixel    preshifted_pixel_mask_data :pixel
  stars               label Star
  stars_end           union stars[256], Star
end

include ZXLib

stars = Stars.new 0xf000
program = Basic.parse_source <<-EOC
  10 RANDOMIZE USR #{stars[:start]}
     INPUT "How many background stars? "; bstars: POKE #{stars[:bstars]}, bstars
     INPUT "How many moving stars? "; nstars: POKE #{stars[:nstars]}, nstars
     GO TO 10
9998 STOP
9999 CLEAR #{stars.org-1}: LOAD ""CODE: RUN
EOC
puts stars.debug
puts "start:      #{stars[:start]}"
puts "nstars:     #{stars[:nstars]}"
puts "bstars:     #{stars[:bstars]}"
puts "stars data: #{stars[:stars]}"
puts "stars end:  #{stars[:stars_end]}"
puts program.to_source escape_keywords: true

program.save_tap "examples/stars", line: 9999
stars.save_tap "examples/stars", append: true
puts "TAP: stars.tap:"
Z80::TAP.parse_file('examples/stars.tap') do |hb|
    puts hb.to_s
end
