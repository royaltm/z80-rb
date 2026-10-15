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
  macro_import ZXLib::Gfx::Draw

  BG_STARS = 256
  NSTARS = 128
  SPEED = 3
  MAX_SPEED = 32
  STAR_COORD_HI_MASK = 0b01111111 # lowest bit is a sign bit

  class Star < Label
        x word # xxxxxxxs xxxxxxxx
        y word # yyyyyyys yyyyyyyy
        z byte # z: 0=near, 255=far
     mask byte # pixel mask
    saddr word # screen address
  end

  # < c: z, tt: xxxxxxxs xxxxxxxx, c0: check if z == 0
  # > hl: x/z, f': CF: sgn(x), optionally CF:div by 0 if c0 == true
  # Modifies: AF, TH, TL
  macro :persp_coord do |_, th, tl, c0:true|
                      srl  th               # shift out the sign bit
                      ex   af, af           # CF': sgn(x)
                      # (x*4)/z (optimize must be unroll or unroll_alt to preserve b)
                      divmod16_8 th, tl, c, check0:c0, check0_far:c0 && c0 != true, check1:false,
                                 k_leq_m:true, ignore_cf:c0 && c0 != true, optimize: :unroll_alt
  end

  macro :move_stars do |eoc, io:self.io|
                      # ld   [restore_sp_p], sp
                      # di
                      ld   sp, stars
    nstars_a          ld   b, NSTARS
    nstars_p          as nstars_a + 1

    sloop             exx

                      pop  hl                    # x
                      pop  de                    # y
                      pop  bc                    # mask|z

                      persp_coord h, l, c0:skip_star
                      # hl: x/z
                      ld   a, h
                      anda a
                      jr   NZ, fw_skip_star      # hl >= 256
                      ex   af, af                # CF: sgn(x)
                      jr   NC, x_positive

    x_negative        ld   a, 128                # x  <  0
                      sub  l                     # x to screen coordinates
                      jr   NC, skip_pos_x        # xp >= 0

    fw_skip_star      jp   skip_star

    x_positive        ld   a, l                  # x  >= 0
                      add  128                   # x to screen coordinates
                      jr   C, fw_skip_star       # xp <  256
                                                 # xp >= 256
    skip_pos_x        ld   l, a                  # l: xp

                      ex   de, hl                # e: xp, hl: y
                      persp_coord h, l, c0:skip_star
                      # hl: y/z
                      ld   a, h
                      anda a
                      jr   NZ, skip_star         # hl >= 256
                      ex   af, af                # CF: sgn(y)
                      jr   C, y_negative
                      ld   a, l                  # y  >= 0
                      add  96                    # y to screen coordinates
                      jr   NC, skip_neg_y        # yp <  256
                                                 # yp >= 256
    skip_star         ld   d, 0                  # no pixel
                      ld   h, d                  # invalid screen address
                      ld   l, d
                      jp   skip_plot

    ns :reinitialize_star do
                      ld   a, 0b00001000
                      out  (io.ula), a
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
                      out  (io.ula), a
                      jp   back_to_loop
    end

    y_negative        ld   a, 96                 # y  <  0
                      sub  l                     # y to screen coordinates
                      jr   C, skip_star          # yp <  0
    skip_neg_y        cp   192
                      jr   NC, skip_star         # yp >= 192

                      ld   h, a                  # yp
                      ld   l, e                  # xp
    plot              plot_pixel(l, h, preshifted_pixel, fx: :skip, scraddr:0x4000)
                      ld   d, a                  # save pixel mask
                      xor  [hl]
                      ld   [hl], a               # plot star
    skip_plot         ex   [sp], hl              # swap screen address
                      ld   a, b                  # previous pixel mask
                      xor  [hl]
                      ld   [hl], a               # clear star

                      ld   a, c                  # a: z
    speed_a           sub  SPEED
    speed_p           as speed_a + 1
                      ld   e, a                  # z
                      push de                    # mask|z
                      jr   C, reinitialize_star
                      # 4.times { inc  sp }
                      pop  de                    # sp += 4 (next star)
                      pop  hl

    back_to_loop      exx
                      dec  b
                      jp   NZ, sloop

    # restore_sp_a      ld   sp, 0
    # restore_sp_p      as restore_sp_a + 1
    #                   ei
    #                 ret
  end

                      dc! "***************************************"
                      dc! "***              START              ***"
                      dc! "***************************************"
  with_saved :start, :exx, hl, ret: true do |eoc|
                      call make_stars
    release_key       key_pressed?
                      jr   NZ, release_key

                      ld   a, 0b01000111
                      call clear_screen
                      call background_stars

                      ld   [restore_sp_p], sp
                      di
                      dc!
                      dc! "***************************************"
                      dc! "***           MOVE  STARS           ***"
                      dc! "***************************************"
    mloop             move_stars
                      dc! "***************************************"
                      dc!
                      key_pressed?(0xf7)       # [1]-[5]
                      rra                      # [1]
                      jr   C, inc_speed
                      rra                      # [2]
                      jr   C, dec_speed
                      jr   NZ, quit

                      key_pressed?(0x08)
                      jp   Z, mloop

                      # clear screen using CL-ALL and reset border
    quit              ei
    restore_sp_a      ld   sp, 0
    restore_sp_p      as restore_sp_a + 1
                      call rom.cl_all
                      ld   a, [vars.bordcr]
                      call set_border_cr
                      jr   eoc

    ns :dec_speed do
                      ld   hl, mloop.speed_p
                      ld   a, [hl]
                      anda a
                      jr   Z, fw_mloop
                      dec  [hl]
      fw_mloop        jp   mloop
    end

    ns :inc_speed do
                      ld   hl, mloop.speed_p
                      ld   a, [hl]
                      cp   MAX_SPEED
                      jr   NC, fw_mloop
                      inc  [hl]
      fw_mloop        jp   mloop
    end
  end

                      dc!
                      dc! "***************************************"
                      dc! "***          CLEAR  SCREEN          ***"
                      dc! "***************************************"
  # clears screen area with border and attributes set according to register a
  clear_screen        clrmem  mem.attrs, mem.attrlen, a
  set_border_cr       anda 0b00111000
                      3.times { rrca }
                      out  (io.ula), a
  # clear pixel screen
  clearscr            clrmem  mem.screen, mem.scrlen, 0
                      ret

                      dc!
                      dc! "***************************************"
                      dc! "***           MAKE  STARS           ***"
                      dc! "***************************************"
  ns :make_stars do
                      ld   [restore_sp_p], sp
                      di
                      ld   sp, stars_end
                      ld   hl, [vars.seed]
                      exx
                      ld   b, 256                 # 256 stars max
                      ld   de, 0                  # invalid screen address
    mloop             push de                     # screen addr
                      exx
                      call rand_fn_hl
                      ld   e, l
                      ld   d, 0                   # no pixel
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

    restore_sp_a      ld   sp, 0
    restore_sp_p      as restore_sp_a + 1
                      ei
                      ret
  end
                      dc!
                      dc! "***************************************"
                      dc! "***        BACKGROUND  STARS        ***"
                      dc! "***************************************"
  ns :background_stars do
    bstars_a          ld   b, BG_STARS
    bstars_p          as bstars_a + 1
                      ld   ixl, 0b00000001
                      exx
                      call rand_seed
                      jr   skip
    bloop             exx
    next_plot         call rand_fn_hl
    skip              ld   a, h
                      cp   192
                      jr   NC, change_color
                      ld16 bc, hl
                      plot_pixel(c, b, preshifted_pixel, fx: :or, with_attributes: :overwrite, color_attr: ixl, scraddr:0x4000)
                      ld16 hl, bc
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

                      dc!
                      dc! "***************************************"
                      dc! "***            RANDOMIZE            ***"
                      dc! "***************************************"
  rand_seed           ld   hl, 0
  rand_seed_p         as rand_seed + 1
  rand_fn_hl          rnd
                      ret

  seed                union rand_seed_p, 2
  nstars              union start.mloop.nstars_p, 1
  bstars              union background_stars.bstars_p, 1

                      dc!
                      dc! "***************************************"
                      dc! "***           PIXEL MASKS           ***"
                      dc! "***************************************"
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
