# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'
require 'z80/stdlib'
require 'z80/utils/shuffle'
require 'z80/utils/sincos'
require 'z80/utils/sort'
require 'zxlib/sys'
require 'zxlib/gfx'
require 'zxlib/gfx/bobs'
require 'zxutils/bigfont'

class Ghosts
  include Z80
  include Z80::TAP

  SinCosTable = Utils::SinCos::SinCosTable
  SinCos      = Utils::SinCos::SinCos

  export :auto

  MODE = :or # :or :xor :set :copy
  BORDER_PROFILE = false
  ASSERT_DEBUG = false
  ANIMATE_STEP = 12

  class Word < Label
    l           byte
    h           byte
  end

  class Sprite < Label
    y           Word
    x           Word
    color_attr  byte
    bitmap      Word
    direction   byte
    angle       byte
    radius      byte
  end

  MAX_SPRITES = 256/Sprite.to_i
  raise SyntaxError, "max sprites must be smaller than #{256/Sprite.to_i}" if MAX_SPRITES > 256/Sprite.to_i
  WIPE_TERMINATOR = 0
  MARK_MAX_Y = [-1].pack('c').unpack('C').first # because why not?

  ## Graphics
  ghosts        addr 0xF000
  ## Clear screen region addresses and heights.
  #  The second stack is being populated while the previous one is being pulled from.
  wipe_stack0   addr 0xF800
  wipe_stack1   addr wipe_stack0 ^ 0x0100, 1
  ## SinCostTable
  sincos        addr wipe_stack1 + 0x0100, SinCosTable
  ## Variables: number of sprites (must be in the range: 1..MAX_SPRITES)
  nsprites      addr sincos[1], 1
  ## Variables: last populated wipe stack (MSB of wipe_stack0 or wipe_stack1)
  wipe_stack    addr :next, 1
  ## Variables: ghost chase radius delta
  chase_dt      addr :next, 1
  ## Variables: collected spell step
  spell_n       addr :next, 1
  ## Variables top marker
  vars_top      addr :next, 1
  ## Sprite index, contains LSB of sprite addresses
  sprindex      addr wipe_stack0 - 0x0100, 1
  ## Unsigned sprite Y position (sprite.y + 15) being used for sorting.
  #  MARK_MAX_Y denotes OOS sprites.
  spr_ys        addr sprindex - 0x0100, 1
  ## Sprite control data
  sprites       addr spr_ys - 0x0100, Sprite
  ## Shuffled random data
  shuffled      addr sprites - 0x100, 1

  sanity_sp     addr 0xFF10 if ASSERT_DEBUG

  raise SyntaxError if vars_top.to_i > 0xFF00
  raise SyntaxError unless (sincos.to_i & 0xFF).zero?
  raise SyntaxError unless (wipe_stack0.to_i & 0xFF).zero?
  raise SyntaxError unless (wipe_stack1.to_i & 0xFF).zero?
  raise SyntaxError unless (wipe_stack0.to_i ^ wipe_stack1.to_i) == 0x0100
  raise SyntaxError if (spr_ys.to_i - sprindex.to_i).abs != 0x0100 || !(spr_ys.to_i & 0xFF).zero?
  raise SyntaxError unless (sprites.to_i & 0xFF).zero?

  macro_import MathInt
  macro_import Stdlib
  macro_import Utils::Shuffle
  macro_import Utils::SinCos
  macro_import Utils::Sort
  label_import ZXLib::Sys, macros: true
  macro_import ZXLib::Gfx
  macro_import ZXLib::Gfx::Bobs

  def self.debug_border(color)
    raise ArgumentError, "border color: 0-7" unless (0..7).include?(color)
    if BORDER_PROFILE
      isolate(use: :io) do
        if color.zero?
                xor a
        else
                ld a, color
        end
                out (io.ula), a
      end
    else
                label
    end
  end

  ##
  # Macro for calculating sprite screen address, height, bitmap adjustment and selecting drawing routine.
  #
  #        -15    -7     0         241   248   256    (x,y)
  #          +-----+-----+---------+-----+-----+ -15
  #   (+205) | 311 | 303 |   262   | 281 | 286 |
  #          +-----+-----+---------+-----+-----+ 0
  #          |     |     |         |     |     |
  #   (+150) | 256 | 248 |   207   | 226 | 231 |
  #          |     |     |         |     |     |
  #          +-----+-----+---------+-----+-----+ 177
  #   (+176) | 282 | 274 |   233   | 252 | 257 |
  #          +-----+-----+---------+-----+-----+ 192
  #          (+106) (+98)   (+57)   (+76) (+81)
  #
  # <- a': attributes, hl: bitmap, de: sx, bc: sy, cols: 2
  # -> c: height, de: screen address, [sp]:
  # * attr_cols|ZF=0,CF=1
  # * attr_addr
  # * attr_rows|attributes
  # * bshift|height
  # * screen address
  # * bitmap
  # * jump_table
  macro :calc_screen_addrs_and_push_args do |_, oos:, height:16|
    if oos.is_a?(Proc)
      oos_block = oos
      oos = define_label :oos
    end
    yy = bc
    xx = de
    yh, yl = yy.split
    xh, xl = xx.split
    attr_cols = xh
                        ld    a, xh
                        ora   a
                        jr    NZ, xneg     # x < 0 || x >= 256
                        # 0 <= x < 256
                        ld    attr_cols, a # a: 0 (undecided: 3 or 2)
                        ld    a, xl
                        cp    241
                        jr    NC, xrm2     # x > 240
                        ld    ix, draw_bob_jtable
                        # 0 <= x <= 240
                        jp    check_y
    if oos_block
                        ns(:oos, &oos_block)
    end
                        # x > 240
    xrm2                cp    248
                        jr    NC, xrm1     # x >= 248
                        # 240 < x < 248
                        ld    ix, draw_bob_rc2_jtable
                        ld    attr_cols, 2
                        jp    check_y
                        # 248 <= x < 256
    xrm1                ld    ix, draw_bob_rc1_jtable
                        inc   attr_cols    # attr_cols: 1
                        jp    check_y
                        # x < 0 || x >= 256
    xneg                inc   a
                        jr    NZ, oos      # x <  -256 || x >= 256
                        # -256 < x < 0
                        ld    a, xl
                        cp    -15
                        jr    C, oos       # x <= -16
                        cp    -7
                        jr    C, xlm1     # x <=  -8
                        # -8 < x < 0
    xlm2                ld    ix, draw_bob_lc2_jtable
                        ld    attr_cols, 2
                        add   a, 8
                        jp    check_y_setxl
                        # -16 < x <= -8
    xlm1                ld    ix, draw_bob_lc1_jtable
                        ld    attr_cols, 1
                        inc   hl
                        add   a, 16
                        anda  7
    check_y_setxl       ld    xl, a
    check_y             ld    a, yh
                        ora   a
                        jr    Z, ypos      # 0 <= y < 256
                        # y < 0 || y >= 256
    yneg                inc   a            # if yh == 255
                        jr    NZ, oos      # y < -256 || y >= 256
                        # xor a              # a: 0
                        sub   yl           # a: -y
                        jr    Z, oos       # y == -256
                        # -256 < y < 0
                        add   a, a         # a: -y*2 (y < 0)
                        adda_to h, l       # hl: bitmap + (-y*2)
                        ld    a, yl        # a: y (y < 0)
                        ld    yl, 0        # yl: 0
    calc_h              add   a, height-1  # height: yl + 15 (yl < 0)
                        jr    NC, oos      # height <= 0
                        # 176 < y < 192 || -16 < y < 0
                        inc   a            # height: yl + 16
                        jp    scraddrcalc
                        # a: height -> a: attr_rows (height + 7)/8
    lines_to_rows       add   a, 7
                        3.times { rrca }         # rows: 1 or 2
                        anda  3
                        jp    push_attr_rows

                        # 0 <= y < 256
    ypos                ld    a, 192 - height  # a: 176
                        sub   yl               # 176 - yl
                        jr    C, calc_h        # height: 176 - yl + 16 (yl > 176)
                        # 0 <= y <= 176
                        ld    a, height

    scraddrcalc         push  ix                 # jump_table
                        push  hl                 # bitmap
                        ex    af, af             # a: attributes, a': height
                        ld    l, a               # l: attributes
                        ld    h, attr_cols       # h: attr_cols (0, 1 or 2)
                        yxtoscr yl, xl, ah:d, al:e, s:b, t:c, scraddr:0x4000
                        push  de                 # screen address
                        # attribute columns
                        ld    a, h               # attr_cols
                        ora   a                  # if attr_cols.zero?
                        jr    NZ, cols_settled
                        cpl                      # a: ~0
                        add   b                  # if attr_cols.zero? and bshift.zero?
                        ld    a, 1               # attr_cols: 1
                        adc   a, a               # attr_cols: 2 or 3
    cols_settled        ex    af, af             # a: height, a': attr_cols
                        ld    c, a               # c: height (lines)
                        push  bc                 # bshift|lines (height)
                        # attribute rows
                        cp    height             # a: height (lines)
                        jr    C, lines_to_rows
                        ld    a, d               # rows determined from screen address
                        anda  0b1111_1000
                        cp    d                  # CF: 0b11111000 < 0b11111ddd (ddd != 000)
                        ld    a, 1
                        adc   a, a               # attr_rows: 2 or 3
    push_attr_rows      ld    h, a               # attr_rows
                        push  hl                 # attr_rows|attributes
                        # attribute address
                        scrtoattr d, o:h, scraddr:0x4000
                        ld    l, e
                        push  hl                 # attr_addr
                        ex    af, af             # a: attr_cols
                        ora   a                  # ZF=0 (a != 0)
                        scf                      # CF=1
                        push  af                 # attr_cols|ZF=0,CF=1
  end

=begin
  if (scr_addr & 31) <= 29
    scr_addr += 2
  else
    scr_addr = scr_addr | 31
  end
=end
  ## Prepares screen address low nibble for the clear_screen_region_fast.
  # Modifies: af, tl
  macro :wipe_scr_addr do |_, tl|
                        ld    a, tl
                        add   2       # yyyxxxxx
                        xor   tl      # 000_____ vs ddd_____
                        cp    32
                        jr    C, noedge
                        ld    a, tl
                        cpl
                        anda  31
    noedge              xor   tl      # yyyxxxxx
                        ld    tl, a
  end

  ## Converts y-coord as a 16-bit signed integer to unsigned 8-bit int or OOS.
  # Modifies: af, yl
  # <- a: height - 1
  # -> ys: y + height - 1, ZF: 1=ok (y > -height and y <= 256 - height), 0=OOS
  macro :syy_to_spr_ys do |_, yh=b, yl=c|
                        add   yl
                        ld    yl, a
                        adc   a, yh
                        sub   yl               # ZF: 1 if y > -height and y <= 256 - height
  end

=begin
  i = nsprites
  begin
    i = i - 1
    break if spr_ys[i] != MARK_MAX_Y
  end while i > 0
  if spr_ys[i] != MARK_MAX_Y
    wipe_y, wipe_lines, wipe_scr_addr = wipe_stack.shift
    sprite_y = spr_ys[i] + 16
    loop do
      if sprite_y < wipe_y
        stack.unshift wipe_lines, wipe_scr_addr
        wipe_y, wipe_lines, wipe_scr_addr = wipe_stack.shift
      else
        if calc_res = calc_screen_addrs_and_push_args(sprites[sprindex[i]])
          scr_addr, wipe_lines = calc_res
          wipe_stack_next.push wipe_scr_addr(scr_addr), wipe_lines, spr_ys[i] + 1
        end
        break if i.zero?
        i = i - 1
        sprite_y = spr_ys[i] + 16
      end
    end
  end
  until wipe_y.zero?
      stack.unshift wipe_lines, wipe_scr_addr
      wipe_y, wipe_lines, wipe_scr_addr = wipe_stack.shift
  end
=end
  ##
  # Macro for pushing arguments for drawing sprites.
  # <- hl: -> wipe_stack_last (top), iy: -> wipe_stack_next (top), a: nsprites (> 0)
  # -> iy: -> terminator of wipe_stack_next
  macro :push_sprites_args do |_, height:16|
                        dec   l
                        ld    [wipe_stack_cur_p], hl
    # find last visible sprite index
                        ld    h, spr_ys >> 8
                        ld    l, a               # nsprites (> 0)

                        xor   a                  # CF=0, ZF=1
                        push  af                 # terminator: ZF=1

    find_last           dec   l
                        ld    a, [hl]            # a: sprite_y
                        jr    Z, last_sprite
                        inc   a                  # a: sprite_y + 1
                        jr    Z, find_last
    restore_y           ld    c, l               # c: save index
                        add   a, height - 1      # a: sprite_y + 1 + 15
                        ld    b, a               # b: sprite_y + 16
                        jp    skip_wipe

    last_sprite         inc   a                  # a: sprite_y + 1
                        jp    NZ, restore_y
                        jp    no_sprites

    push_wipe           ld    a, d               # a: clr_lines
                        ora   a                  # ZF=0 CF=0
                        jp    Z, sanity1 if ASSERT_DEBUG
                        push  hl                 # clr_screen
                        push  af                 # clr_lines|ZF=0,CF=0
    skip_wipe           label
    wipe_stack_cur      ld    hl, 0
    wipe_stack_cur_p    as    wipe_stack_cur + 1
                        inc   l; ld e, [hl]      # e: wipe_y + 1
                        inc   l; ld d, [hl]      # d: clr_lines (clr_lines > 0)
                        inc   l; ld a, [hl]
                        inc   l
                        ld    [wipe_stack_cur_p], hl
                        ld    h, [hl]
                        ld    l, a               # hl: clr_screen_addr
                        ld    a, b               # a: sprite_y
                        cp    e                  # (sprite_y + 16) - (wipe_y + 1)
                        jr    C,  push_wipe      # (sprite_y + 16) < (wipe_y + 1)
                        # de+hl+bc
    push_sprite         ld    b, sprindex >> 8
                        ld    a, [bc]            # [bc]: sprite index
                        exx                      # wipes`
                        ld    h, sprites >> 8
                        ld    l, a               # hl: Sprite
                        ld    c, [hl]; inc  l
                        ld    b, [hl]; inc  l    # y
                        ld    e, [hl]; inc  l
                        ld    d, [hl]; inc  l    # x
                        ld    a, [hl]; inc  l    # color_attr
                        ex    af, af             # a': color_attr
                        ld    a, [hl]; inc  l
                        anda  ~(height*2 - 1)    # truncate to the nearest shape
                        ld    h, [hl]; ld   l, a # bitmap
    # a': color, hl: bitmap, de: xx, bc: yy, cols: 2 -> c: height, de: screen address
                        calc_screen_addrs_and_push_args(oos: proc do
                          exx
                          ld    b, spr_ys >> 8     # bc: spr_ys index
                          jp    next_sprite
                        end)
                        # store wipe
                        wipe_scr_addr e           # de: screen -> clr_screen
                        ld    a, iyl             # iy: wipe_stack_next
                        add   a, 4
                        ld    iyl, a
                        ld    [iy - 2], e        # e: screen low
                        ld    [iy - 1], d        # d: screen high
                        if ASSERT_DEBUG
                          ld  a, c
                          ora a
                          jp    Z, sanity3
                        end
                        ld    [iy - 3], c        # c: lines
                        exx
                        ld    b, spr_ys >> 8     # bc: spr_ys index
                        ld    a, [bc]            # a: sprite_y
                        inc   a                  # a: sprite_y + 1
                        jp    Z, sanity4 if ASSERT_DEBUG
                        ld    [iy - 4], a        # a: wipe_y + 1
                        # bc: spr_ys index, hl: clr_screen_addr, d: clr_lines e: wipe_y + 1
    next_sprite         ld    a, c
                        ora   a
                        jr    Z, no_more_sprites # c == 0
                        dec   c                  # bc: --spr_ys
                        ld    a, [bc]            # a: sprite_y
                        add   a, height          # a: sprite_y + 16
                        cp    e                  # (sprite_y + 16) - (wipe_y + 1)
                        jp    NC, push_sprite    # (sprite_y + 16) >= (wipe_y + 1)
                        ld    b, a               # b: sprite_y + 16
                        jp    push_wipe
if ASSERT_DEBUG
    sanity0             ld    [0xFFE0], a
                        jr    label
    sanity1             ld    [0xFFE1], a
                        jr    label
    sanity2             ld    [0xFFE2], a
                        jr    label
    sanity3             ld    [0xFFE3], a
                        jr    label
    sanity4             ld    [0xFFE4], a
                        jr    label
end
    no_more_sprites     ora   e                  # e == 0 (wipe_y)
                        jr    Z, terminate
                        ld    a, d               # a: clr_lines
                        ora   a                  # ZF=0 CF=0
                        jr    Z, sanity2 if ASSERT_DEBUG
                        push  hl                 # clr_screen
                        push  af                 # clr_lines|ZF=0,CF=0
    ns :no_sprites do   # push the rest of wipes
                        ld    hl, [wipe_stack_cur_p]
                        jp    start
      next0             inc   l; ld a, [hl]      # a: clr_lines (clr_lines > 0)
                        inc   l; ld c, [hl]
                        inc   l; ld b, [hl]      # bc: clr_screen_addr
                        ora   a                  # ZF=0 CF=0
                        jr    Z, sanity0 if ASSERT_DEBUG
                        push  bc                 # clr_screen
                        push  af                 # clr_lines|ZF=0,CF=0
      start             inc   l; ld a, [hl]      # a: wipe_y + 1 or 0
                        ora   a                  # a == 0
                        jp    NZ, next0
    end
    terminate           ld    [iy + 0], WIPE_TERMINATOR
  end

=begin
  nsprites.times do |i|
    y = sprites[sprindex[i]].y
    spr_ys[i] = if y > -height and y < 192
      y + (height - 1)
    else
      MARK_MAX_Y
    end
  end
=end
  # assert nsprites != 0
  macro :sprite_index_to_spr_y do |height:16|
                        di
                        ld    e, height - 1
                        exx
                        ld    c, 192 + height - 1
                        ld    a, [nsprites]
                        ld    b, a
                        ld    h, sprites >> 8
                        ld    de, sprindex
                        ld    [restore_sp_p], sp
    pop_loop            ld    a, [de]   # index
                        ld    l, a
                        ld    sp, hl
                        exx
                        pop   bc        # y
                        ld    a, e
                        syy_to_spr_ys b, c
                        ld    a, c      # y + height - 1
                        exx
                        jr    NZ, oos_y
                        cp    c         # c: 192 + height - 1
                        jr    C, skip_oos_y
      oos_y             ld    a, MARK_MAX_Y
      skip_oos_y        label
      select(spr_ys - sprindex) {|dt| dt < 0 }.then do
                        dec   d         # spr_ys
                        ld    [de], a   # y + 15
                        inc   d         # sprindex
      end.else do
                        inc   d         # spr_ys
                        ld    [de], a   # y + 15
                        dec   d         # sprindex
      end
                        inc   e         # next
                        djnz  pop_loop
    restore_sp          ld    sp, 0
    restore_sp_p        restore_sp + 1
                        ei
  end

=begin
  sprindex, spr_ys = sprindex.zip(spr_ys).sort { |a, b| a[1] <=> b[1] }.transpose
=end
  ## Sorts both sprindex and spr_ys using spr_ys for order.
  macro :sort_sprindex_by_spr_ys do
                        ld   b, 0
                        ld   h, sprindex >> 8
                        ld   d, h
                        exx
                        insertion_sort_bytes_max256(
                            reverse:false,
                            target:spr_ys,
                            length:[nsprites]) do |eoc|
                        # de: bottom item (inserted)
                        # hl: upper item (removed)
                          ld   a, l     # upper
                          ex   af, af   # a': upper
                          ld   a, l     # upper
                          sub  e        # a: upper - lower
                          exx
                          ld   c, a     # bc: length
                          ex   af, af   # a: upper
                          ld   e, a     # de: upper
                          dec  a
                          ld   h, d     # h: restore
                          ld   l, a     # hl: upper - 1
                          ld   a, [de]  # a: swapped
                          lddr
                          ld   [de], a
                          exx
                        end
  end

=begin
  (nsprites-1).downto(0).each do |i|
    sprite = sprites[sprindex[i]]
    sprite.radius = (sprite.radius + RADIUS_ACC) % MAX_DISTANCE
    sprite.angle = (sprite.angle + POLAR_ACC) % 2.0*PI
    sprite.y = sin(a)*r + (192/2 - 8)
    y = sprite.y
    spr_ys[i] = if y > -height and y < 192
      y + (height - 1)
    else
      MARK_MAX_Y
    end
    sprite.x = cos(a)*r + (256/2 - 8)
  end
=end
  ##
  # Animates and moves sprites around.
  # assert nsprites != 0
  macro :move_sprites do |height:16|
                        ld    a, [nsprites]
                        ld    e, a
                        ld    d, sprindex >> 8
                        ld    h, sprites >> 8
    next_sprite         dec   e
                        ld    a, [de]          # sprite index
                        add   a, Sprite.offset_of_(:bitmap)
                        ld    l, a
                        ld    a, ANIMATE_STEP
                        add   a, [hl]          # Sprite.bitmap.l
                        ld    [hl], a          # Sprite.bitmap.l
                        inc   l
                        inc   l
                        ld    a, [hl]          # Sprite.direction
                        inc   l                # hl: -> Sprite.angle
                        add   a, [hl]          # Sprite.angle
                        ld    [hl], a
                        ex    af, af           # a': angle
                        inc   l                # hl: -> Sprite.radius
                        ld    a, [chase_dt]
                        add   [hl]             # Sprite.radius
                        ld    [hl], a
                        ex    af, af           # a: angle, a': radius
                        exx                    # hl': -> Sprite.radius, de': sprindex
                        # a: angle, a': radius
                        sincos_from_angle sincos, h, l
                        ld    c, [hl]          # hl -> sin(angle)
                        inc   l
                        ld    b, [hl]          # bc: sin(angle)
                        ex    af, af           # a: radius
                        ex    de, hl           # save sincos entry
                        # hl: y = sin(a) * r
                        mul8(b, c, a, tt:bc, clrhl:true, double:false, optimize: :time)
                        ex    de, hl           # hl: sincos entry, de: y
                        ld    a, [hl]          # sin(a).msb
                        inc   l                # hl: -> cos(angle)
                        add   a, a             # sign -> CF
                        ld    a, d             # a: y.lsb
                        # hl: -> Sprite.radius, de: sprindex
                        exx
                        ld    c, a             # c: y.lsb
                        sbc   a, a             # a: 0 or -1
                        ld    b, a             # bc: y = sin(angle) * r
                        ld    a, 192/2 - 8
                        adda_to b, c           # bc: y = sin(angle) * r + 192/2
                        ld    a, [hl]          # a: radius
                        # hl: -> cos(angle) 
                        exx
                        ld    c, [hl]          # hl -> cos(angle)
                        inc   l
                        ld    b, [hl]          # bc: cos(angle)
                        ld    d, b             # d: cos(angle).msb
                        # hl: x = cos(a) * r
                        mul8(b, c, a, tt:bc, clrhl:true, double:false, optimize: :time)
                        ld    a, d             # a: cos(angle).msb
                        add   a, a             # sign -> CF
                        ld    a, h             # a: x.lsb
                        ex    af, af           # a': x.lsb, f': CF=sign
                        # bc: y, h: sprites.msb, de: sprindex
                        exx
                        ld    a, [de]          # sprite index
                        ld    l, a             # hl: -> Sprite.y
                        ld    [hl], c
                        inc   l
                        ld    [hl], b          # Sprite.y = y
                        # y = Sprite.y + height - 1
                        ld    a, height - 1
                        syy_to_spr_ys b, c
                        jr    NZ, oos_y        # b != 0
                        ld    a, c             # a: 0 <= (y + height - 1) < 256
                        cp    192 + height - 1
                        jr    C, skip_oos_y
      oos_y             ld    a, MARK_MAX_Y
      skip_oos_y        label
      # spr_ys[i] = if y > -height and y < 192 then y + (height - 1) else MARK_MAX_Y end
      select(spr_ys - sprindex) {|dt| dt == -0x100 }.then do
                        dec   d         # spr_ys
                        ld    [de], a   # y
                        inc   d         # sprindex
      end.else_select(spr_ys - sprindex) {|dt| dt == 0x100 }.then do
                        inc   d         # spr_ys
                        ld    [de], a   # y
                        dec   d         # sprindex
      end.else do
        raise ArgumentError, "sprindex and spr_ys should be apart by 256 bytes"
      end
                        ex    af, af   # a: x.lsb, f: CF=sign
                        ld    c, a     # c: x.lsb
                        sbc   a, a     # a: 0 or -1
                        ld    b, a     # bc: x = cos(angle) * r
                        ld    a, 256/2 - 8
                        adda_to b, c   # bc: x = cos(angle) * r + 256/2
                        inc   l        # hl: -> Sprite.x
                        ld    [hl], c
                        inc   l
                        ld    [hl], b  # Sprite.x = x
                        ld    a, e     # sprindex
                        ora   a
                        jp    NZ, next_sprite
  end

  ns :jump_tables, merge: true do
    draw_bob_jtable     bobs_draw_pixels_fast_jump_table( clear_and_draw.draw_bob )
    draw_bob_lc2_jtable bobs_draw_pixels_fast_jump_table( clear_and_draw.draw_bob_lc2 )
    draw_bob_rc2_jtable bobs_draw_pixels_fast_jump_table( clear_and_draw.draw_bob_rc2 )
    draw_bob_lc1_jtable bobs_draw_pixels_fast_jump_table( clear_and_draw.draw_bob_lc1 )
    draw_bob_rc1_jtable bobs_draw_pixels_fast_jump_table( clear_and_draw.draw_bob_rc1 )
  end

  ns :init do
                        ld    hl, nsprites
                        ld    [hl], 0
                        ld    hl, chase_dt
                        ld    [hl], 1
    wipe                ld    hl, wipe_stack0
                        ld    [hl], WIPE_TERMINATOR
                        ld    a, h
                        ld    [wipe_stack], a
                        ret
  end

  dc! "======================================================="
  dc! "==           B A S I C   F U N C T I O N S           =="
  dc! "======================================================="

  ##
  # Draws a single sprite on the screen.
  # FN g(x,y,c)
  ns :put_sprite do
                        call  get_sprite_args
                        ret   C
                        ld    hl, ghosts
                        xor   a      # CF=0, ZF=1
                        push  af     # terminator: ZF=1
    # a': color, hl: bitmap, de: xx, bc: yy, cols: 2 -> c: height, de: screen address
                        calc_screen_addrs_and_push_args(oos: proc do
                          report_error '5 Out of screen'
                        end)
                        ld    a, [clear_scr_lines]
                        ora   a      # ZF=? CF=0
                        jr    Z, skip_clear
                        ld    hl, [clear_scr_addr] # clr_screen
                        push  hl     # clr_screen
                        push  af     # clr_lines|ZF=0,CF=0
    skip_clear          ld    a, c   # a: lines
                        ld    [clear_scr_lines], a # lines
                        wipe_scr_addr e
                        ld    [clear_scr_addr], de
                        jp    clear_and_draw
    # vars
    clear_scr_addr      dw 0
    clear_scr_lines     db 0
  end

  ##
  # Adds a sprite to a sprite collection with given arguments.
  # FN a(x,y,c,s)
  # Removes all sprites if no arguments are given.
  ns :add_or_clear_sprites do
                        call  get_sprite_args
                        jr    NC, add_sprite
                        xor   a
                        ld    [nsprites], a
                        jr    return_nsprites
    add_sprite          push  af               # a: shape
                        ld    hl, nsprites
                        ld    a, [hl]
                        cp    MAX_SPRITES
                        jr    NC, pop_quit
                        inc   [hl]
                        ld    hl, sprites
                        push  de               # de: x
                        push  bc               # bc: y
                        mul_const a, Sprite.to_i, tt:de, clrhl:false # hl: new sprite
                        ld    b, sprindex >> 8
                        ld    c, a
                        ld    a, l
                        ld    [bc], a          # set sprindex
                        pop   bc
                        ld    [hl], c          # hl: -> Sprite.y.l
                        inc   l
                        ld    [hl], b          # hl: -> Sprite.y.h
                        inc   l
                        pop   bc
                        ld    [hl], c          # hl: -> Sprite.x.l
                        inc   l
                        ld    [hl], b          # hl: -> Sprite.x.h
                        ex    af, af           # a': color
                        inc   l                # hl: -> Sprite.color_attr
                        ld    [hl], a
                        inc   l                # hl: -> Sprite.bitmap
                        pop   af               # a: shape
                        call  set_shape
                        inc   l                # hl: -> Sprite.direction
                        call  set_random_direction
                        call  next_rng
                        inc   l; ld [hl], a    # hl: -> Sprite.angle
                        ld    a, 1
                        inc   l; ld [hl], a    # hl: -> Sprite.radius
  return_nsprites       ld    a, [nsprites]
                        ld    c, a
                        ld    b, 0
                        exx
                        ret
  pop_quit              pop   af
                        jr    return_nsprites
  end

  ## Draws sprites with their current x,y positions in a screen sync loop until keys is pressed
  ns :draw_sprites do
                        sprite_index_to_spr_y
                        sort_sprindex_by_spr_ys
                        call  setup_wipe
    check_key           key_pressed?
                        ret   NZ
                        call  draw_loop
                        jr    check_key
    draw_loop           push  af               # restore terminator: ZF=1
    restore_sp          ld    sp, 0
    restore_sp_p        as    restore_sp + 1
                        jp    sync_frame

    setup_wipe          label
    # swap wipe stacks
                        xor   a
                        ld    l, a
                        ld    iyl, a
                        ld    a, [wipe_stack]
                        ld    h, a
                        xor   1
                        ld    iyh, a
                        ld    [wipe_stack], a
                        ld    a, [nsprites]
                        ld    [sanity_sp], sp if ASSERT_DEBUG
    # hl: wipe_stack_last (top), iy: wipe_stack_next (top), a: nsprites (> 0)
    push_args           push_sprites_args height:16

                        ld    [restore_sp_p], sp

    sync_frame          ei # ensure we won't hang around here forever
                        halt
                        jp    clear_and_draw
  end

  dc! "======================================================="
  dc! "==            C L E A R   A N D   D R A W            =="
  dc! "======================================================="
=begin
  loop do
    counter, flags = stack.shift
    break if flags.Z
    if flags.C
      attrs_addr, rows, color = stack.shift
      draw_attrs attrs_addr, width, counter, color
      bshift, lines, scr_addr, bitmap, jump_table = stack.shift
      bobs_draw_pixels_fast scr_addr, bshift, lines, bitmap, jump_table
    else
      scr_addr = stack.shift
      clear_screen_region_fast scr_addr, counter, 3
    end
  end
=end
  ##
  # Pops arguments from stack and draws a sprite or clears the region.
  # Disables interrupts.
  ns :clear_and_draw do
                        di
                        debug_border(4)
    loop0               label
                        pop   af # counter|flags (CF - 0:clear 1:draw, ZF=1 over)
                        jr    Z, end_stage
                        jr    C, draw_sprite
                        pop   hl # screen address
    wipe_region         clear_screen_region_fast( hl, a, 3, 0,
                          disable_intr:false,
                          enable_intr:false,
                          save_sp:true,
                          addr_mode: :last,
                          scraddr:nil,
                          subroutine:false)
                        jp    loop0

    end_stage           debug_border(0)
    if ASSERT_DEBUG
                        ld    hl, [sanity_sp]
                        xor   a
                        sbc   hl, sp
                        jr    NZ, sanity5
    end
                        ret

    if ASSERT_DEBUG
      sanity5           ld    hl, [sanity_sp]
                        ld    [sanity_sp], hl
                        ld    [sanity_sp + 2], sp
                        jr    label
    end

                        # a: width
    draw_sprite         pop   hl # attrs_addr
                        pop   bc # rows|attr_color
    ns :attr_draw do
                        ld    d, a   # attr_cols (counter)
                        ld    e, 33
      attr_loop         ld    [hl], c
                        dec   a
                        jr    Z, skip_cols
                        inc   l
                        ld    [hl], c
                        dec   a
                        jr    Z, skip_cols
                        inc   l
                        ld    [hl], c
      skip_cols         ld    a, e
                        sub   d
                        adda_to h, l
                        ld    a, d
                        djnz  attr_loop
    end
                        pop   bc # bshift|lines
                        pop   hl # screen address
                        exx
                        pop   hl # bitmap
                        exx
                        pop   de # jump_table
    draw_bob            bobs_draw_pixels_fast( hl, c, 2,
                          target:hl,
                          bshift:b,
                          mode: MODE,
                          tx:ix,
                          disable_intr:false,
                          enable_intr:false,
                          save_sp:true,
                          scraddr:nil,
                          jump_table:de,
                          subroutine:false)
                        jp    loop0

    draw_bob_lc2        bobs_draw_pixels_fast_routines(draw_bob.next_row, 2,
                                           mode: MODE,
                                           lclip: true,
                                           rclip: false,
                                           no0shift: draw_bob.quit)
    draw_bob_rc2        bobs_draw_pixels_fast_routines(draw_bob.next_row, 2,
                                           mode: MODE,
                                           lclip: false,
                                           rclip: true,
                                           no0shift: draw_bob.quit)
    draw_bob_lc1        bobs_draw_pixels_fast_routines(draw_bob.next_row, 1,
                                           mode: MODE,
                                           skip_cols: 1,
                                           lclip: true,
                                           rclip: false,
                                           no0shift: nil)
    draw_bob_rc1        bobs_draw_pixels_fast_routines(draw_bob.next_row, 1,
                                           mode: MODE,
                                           skip_cols: 1,
                                           lclip: false,
                                           rclip: true,
                                           no0shift: draw_bob_lc1.line_rshift0.loop0)
  end

  dc! "======================================================="
  dc! "==                      D E M O                      =="
  dc! "======================================================="

  ns :demo do
                        call  setup_interrupts
                        ld    iyl, 0
    demo_loop           ld    a, iyl   # wipe stack bottom (iyl/4=no. sprites that was drawn)
                        cp    16*4     # if less than 16 sprites were drawn then enable interrupts
                        jr    NC, forget_about_sync
                        ei
    forget_about_sync   debug_border(5)

                        move_sprites

                        debug_border(3)

                        sort_sprindex_by_spr_ys

                        debug_border(6)
    # swap wipe stacks
                        xor   a
                        ld    l, a
                        ld    iyl, a
                        ld    a, [wipe_stack]
                        ld    h, a
                        xor   1
                        ld    iyh, a
                        ld    [wipe_stack], a
                        ld    a, [nsprites]
                        ld    [sanity_sp], sp if ASSERT_DEBUG
                        call  push_args
                        key_pressed?
                        ret   NZ
                        jp    demo_loop
    # hl: wipe_stack_last (top), iy: wipe_stack_next (top), a: nsprites (> 0)
    push_args           push_sprites_args height:16
                        # iy: wipe stack bottom
                        debug_border(1)
    # was there an interrupt already? (IFF2=0)
                        ld    a, i
                        jp    PO, clear_and_draw
    sync_frame          ei # ensure we won't hang around here forever
                        halt
                        jp    clear_and_draw
  end

  next_rng              exx
                        ld   hl, [vars.seed]
                        rnd
                        ld   [vars.seed], hl
                        ld   a, l
                        exx
                        ret

  # hl: -> Sprite.color_attr
  ns :set_random_color do
                        call  next_rng
                        ld    c, [hl]
                        xor   c
    attribute_mask_a    anda  0b01000111
    attribute_mask_p    as    attribute_mask_a + 1
                        xor   c
                        ld    [hl], a
    palette_ret         anda  7
                        ret   NZ
                        jr    set_random_color
  end
  # hl: -> Sprite.bitmap
  set_random_shape      call  next_rng # a: shape
  set_shape             ld    d, ghosts >> 8
                        ld    e, a
                        anda  3
                        add   a, d
                        ld    d, a
                        ld    a, e
                        anda  ~(16*2 - 1)
                        ld [hl], a    # hl: -> Sprite.bitmap
                        inc   l
                        ld [hl], d
                        ret

  # hl: -> Sprite.direction
  set_random_direction  call  next_rng
                        anda  7
                        sub   4
                        sbc   a, -1 # a: { -7..-1, 1..7 }
                        ld [hl], a
                        ret

  ns :clear_attrs_fx do
                        shuffle_bytes_source_max256 next_rng, target:shuffled, length:256
                        ld    b, 256
                        ld    hl, shuffled
    clrloop             ld    d, mem.attrs >> 8
                        ld    e, [hl]
                        inc   hl
                        xor   a
                        ld    [de], a
                        inc   d
                        ld    [de], a
                        inc   d
                        ld    [de], a
                        ld    a, e
                        anda  7
                        jr    NZ, skip_halt
                        halt
    skip_halt           djnz  clrloop
                        clrmem mem.screen, mem.scrlen
                        ret
  end

  ns :summon_sprite do
                        ld    hl, nsprites
                        ld    a, [hl]
                        cp    MAX_SPRITES
                        ret   NC
                        inc   [hl]
                        ld    hl, sprites
                        mul_const a, Sprite.to_i, tt:de, clrhl:false # hl: new sprite
                        ld    b, sprindex >> 8
                        ld    c, a
                        ld    a, l
                        ld    [bc], a          # set sprindex
                        add   a, Sprite.offset_of_(:color_attr)
                        ld    l, a             # hl: -> Sprite.color_attr
                        res   7, [hl]          # clear flash bit
                        call  set_random_color
                        inc   l                # hl: -> Sprite.bitmap
                        call  set_random_shape
                        inc   l                # hl: -> Sprite.direction
                        call  set_random_direction
                        call  next_rng
                        inc   l; ld [hl], a    # hl: -> Sprite.angle
                        inc   l; ld [hl], 1    # hl: -> Sprite.radius
                        ret
  end

  ns :banish_sprite do
                        ld    hl, nsprites
                        ld    a, [hl]          # nsprites
                        ora   a
                        ret   Z
                        dec   [hl]
                        ret   Z
                        ld    de, sprindex     # repair indexes
                        ex    de, hl           # hl: sprindex, de: nsprites
                        ld    c, +sprites      # sprite length
    search_first        ld    a, [hl]          # [sprindex]: sprites.lsb
                        sub   c
                        ld    [hl], a
                        inc   l
                        jr    NC, search_first
                        ld    a, [de]          # nsprites
                        ld16  de, hl           # sprindex
                        dec   e                # sprindex dest
                        sub   e                # how many entries to move
                        jr    Z, skip_rest
                        ld    b, a             # length
    convert_rest        ld    a, [hl]; inc   l
                        sub   c
                        ld    [de], a; inc   e
                        djnz  convert_rest
    skip_rest           ld    a, [nsprites]
                        mul_const a, Sprite.to_i, tt:de, clrhl:true
                        ld16  bc, hl           # bc: length to copy
                        memcpy sprites, sprites[1], bc
                        ret
  end

  ns :spell1 do
                        ld    a, [nsprites]
                        ld    hl, sprites.direction
                        ld    de, +Sprite + Sprite.offset_of_(:direction) - Sprite.offset_of_(:radius)
                        ld    b, a
    next_sprite         call  set_random_direction
                        call  next_rng
                        inc   l
                        inc   l
                        ld [hl], a    # hl: -> Sprite.radius
                        add   hl, de
                        djnz  next_sprite
                        ret
  end

  ns :spell2 do
                        ld    a, [nsprites]
                        ld    hl, sprites.color_attr
                        ld    de, +Sprite
                        ld    b, a
    flash               ld    a, [hl]
                        xor   0x80
                        ld    [hl], a
                        add   hl, de
                        djnz  flash
                        ret
  end

  ns :spell3 do
                        ld    a, [nsprites]
                        ld    hl, sprites.color_attr
                        ld    de, +Sprite
                        ld    b, a
    mono                ld    a, [hl]
                        ora   7
                        ld    [hl], a
                        add   hl, de
                        djnz  mono
                        ret
  end

  ns :spell4 do
                        ld    a, [nsprites]
                        ld    hl, sprites.color_attr
                        ld    de, +Sprite
                        ld    b, a
    next_sprite         call  set_random_color
                        add   hl, de
                        djnz  next_sprite
                        ret
  end

  ns :spell5 do
                        ld   hl, chase_dt
                        dec  [hl]
                        ret
  end

  ns :spell6 do
                        ld   hl, chase_dt
                        xor  a
                        sub  [hl]
                        ld   [hl], a
                        ret
  end

  ns :spell7 do
                        ld   hl, chase_dt
                        inc  [hl]
                        ret
  end

  ns :spell8 do
                        xor  a
                        ld   [chase_dt], a
                        ret
  end

  ns :spell9 do
                        ld    a, [nsprites]
                        ld    hl, sprites.radius
                        ld    de, +Sprite
                        exx
                        ld    hl, -1
                        ld16  de, hl
                        ld    c, a
                        divmod8 c, check0:quit, check1:true
                        ld    b, c
                        ex    de, hl
                        neg16 d, e
    queue               ld    a, h
                        add   hl, de
                        exx
                        ld    [hl], a # Sprite.radius
                        add   hl, de
                        exx
                        djnz  queue
    quit                ret
  end

  # a: a key character
  ns :collect_key do
                        cp   ?1.ord
                        jp   Z, summon_sprite
                        cp   ?0.ord
                        jp   Z, banish_sprite
                        ora  0b0010_0000 # tolower
                        ex   af, af      # save char
                        ld   hl, spells
                        ld   bc, +spells
                        ld   de, spell_n
                        ld   a, [de]
                        ora  a
                        jr   NZ, check_next
                        ex   af, af      # restore char
                        cpir
                        jr   Z, found_first
                        ld   a, 2
                        jp   set_border_color

    found_first         ld   a, +spells*2 - 1
                        sub  c
                        ld   [de], a
                        ret
    check_next          adda_to h, l
                        ex   af, af      # restore char
                        cp   [hl]
                        jr   Z, ok_char
    bad_char            ld   a, 3
    clear_exit          call set_border_color
                        xor  a
                        ld   [de], a
                        ret
    ok_char             ld   a, [de]
                        add  a, c        # +spells
                        cp   +spells * 3
                        ld   [de], a
                        ret  C           # next collected
                        sub  +spells * 3
                        add  a, a
                        ld   hl, spell1_jp
                        adda_to h, l
                        ld   a, [hl]
                        inc  hl
                        ld   h, [hl]
                        ld   l, a
                        push hl          # ret will jump
                        ld   a, 4
                        jr   clear_exit
  end
  spells                data "rfmcvnwsq"
                        data "nlooaeotq"
                        data "danlcgooq"
  spell1_jp             dw spell1
  spell2_jp             dw spell2
  spell3_jp             dw spell3
  spell4_jp             dw spell4
  spell5_jp             dw spell5
  spell6_jp             dw spell6
  spell7_jp             dw spell7
  spell8_jp             dw spell8
  spell9_jp             dw spell9

  dc! "======================================================="
  dc! "==               S U B R O U T I N E S               =="
  dc! "======================================================="

  make_sincos create_sincos_from_sintable sincos, sintable:sintable
  sintable    bytes   neg_sintable256_pi_half_no_zero_lo

  # <- FN f(x,y,c[,s])
  # -> CF: 0, de: x, bc: y, a': c, a: s
  # -> CF: 1, no args
  ns :get_sprite_args do
                        call  fn_argn
                        ret   C
    error_q             report_error_unless Z, 'Q Parameter error'
                        call  get_int_cde
                        push  de     # x
                        call  fn_argn.seek_next
                        jr    NZ, error_q.err
                        call  get_int_cde
                        push  de     # y
                        call  fn_argn.seek_next
                        jr    NZ, error_q.err
                        call  get_uint_de
                        ld    a, d
                        ora   a
                        jp    NZ, error_a
                        ld    a, e   # color
                        ex    af, af # a': color
                        call  fn_argn.seek_next
                        jr    NZ, random_shape
                        call  get_uint_de
                        ld    a, e   # shape
    shape_set           pop   bc     # yy
                        pop   de     # xx
                        ora   a      # CF: 0
                        ret
    random_shape        call  next_rng
                        jr    shape_set
  end

  fn_argn               find_def_fn_args 1, cf_on_direct:true

  error_a               report_error 'A Invalid argument'

  ns :get_int_cde do
                        read_integer_value d, e, c
                        jr   NZ, error_a
                        inc  hl
                        ret
  end
  ns :get_uint_de do
                        read_positive_int_value d, e
                        jr   NZ, error_a
                        inc  hl
                        ret
  end

  ns :setup_interrupts do
                        di
                        ld  a, 0xC9     # C9H is ret
                        ld  [0xFFFF], a
                        ld  a, 0x3B
                        ld  i, a
                        im2
                        ret
  end

  # Waits until any key is being pressed, wait to release it or BREAK.
  ns :wait_key_or_break do
    wait_key            halt
                        key_pressed?
                        jr   Z, wait_key
    # Waits until no key is being pressed.
    release_key         halt
                        call  rom.break_key
                        jr    C, no_break
                        call  restore_keyboard_vars
                        call  turn_off_plus
                        report_error 'D BREAK - CONT repeats'
    no_break            key_pressed?
                        ret   Z
                        jr   release_key
  end

  # Restore reppel/repdel vars
  # iy: vars_iy
  ns :restore_keyboard_vars do
                        ld    [iy + vars.repdel - vars_iy], 35
                        ld    [iy + vars.repper - vars_iy], 5
                        ret
  end

  ##
  # Detect ULAplus and set up palette.
  #
  # *  0- 7 ink
  # *  8-15 background (black)
  # * 16-23 ink
  # * 24-31 background (black)
  # * 32-39 ink (black)
  # * 40-47 background
  # * 48-55 ink (black)
  # * 56-63 background
  ns :plus_pallette_on do
                        ld    bc, io_plus.reg
                        ld    a, io_plus.mode_group
                        out   (c), a
                        ld    b, io_plus.dta >> 8
                        inp   a, (c)
                        ret   NZ        # either mode was already set or is not supported
                        inc   a         # a: palette mode=1
                        out   (c), a
                        # modify some routines
                        ld    a, 0b01111111
                        ld    [set_random_color.attribute_mask_p], a
                        ld    a, 0xC9 # ret
                        ld    [set_random_color.palette_ret], a
                        ld    hl, spell3_plus
                        ld    [spell3_jp], hl
                        ld    hl, spell4_plus
                        ld    [spell4_jp], hl
                        clrmem set_border_color, +set_border_color, 0 # 3xNOP

                        ld    hl, palcolor
                        jp    plus_set_palette
  end

  ns :set_border_color do
                        out  (io.ula), a
                        ret
  end

  # a: color
  ns :plus_set_bg_color do
                        push  de
                        ld    hl, bg_colors
                        adda_to h, l
                        ld    e, [hl]
                        ld    c, io_plus.reg
                        ld    a, 8
                        ld    d, 16
                        call  plus_set_palette.clr_loop
                        ld    a, 24
                        ld    d, 32
                        call  plus_set_palette.clr_loop
                        pop   de
                        ret
    bg_colors           db 0, 0, 0b00011100, 0b00011111, 0b11100000
  end

  ns :spell4_plus do
                        ld    hl, palcolor
                        call  plus_set_palette
                        ld    a, 4
                        call  plus_set_bg_color
                        jp    spell4
  end

  ns :spell3_plus do
                        ld    hl, palmono
                        call  plus_set_palette
                        ld    a, 4
                        jp    plus_set_bg_color
  end

  ns :plus_set_palette do
                        push  hl
                        ld    c, io_plus.reg
                        xor   a
                        ld    d, 8
                        call  pal_loop
                        ld    de, (16 << 8)|0
                        call  clr_loop
                        ld    d, 24
                        call  pal_loop
                        ld    de, (40 << 8)|0
                        call  clr_loop
                        pop   hl
                        ld    d, 48
                        call  pal_loop
                        ld    de, (56 << 8)|0
                        call  clr_loop
                        ld    d, 64

    pal_loop            ld    e, [hl]
                        inc   hl
                        ld    b, io_plus.reg >> 8
                        out   (c), a
                        ld    b, io_plus.dta >> 8
                        out   (c), e    # palette entry
                        inc   a
                        cp    d
                        jr    C, pal_loop
                        ret

    clr_loop            ld    b, io_plus.reg >> 8
                        out   (c), a
                        ld    b, io_plus.dta >> 8
                        out   (c), e    # palette entry
                        inc   a
                        cp    d
                        jr    C, clr_loop
                        ret
  end

  palcolor  db 0b000_000_11,
               0b000_011_10,
               0b011_001_10,
               0b111_100_00,
               0b100_111_00,
               0b110_001_00,
               0b010_001_11,
               0b100_101_01,

               0b111_111_00,
               0b111_100_01,
               0b100_110_01,
               0b000_011_11,
               0b011_000_11,
               0b001_110_11,
               0b101_110_00,
               0b011_010_10

  palmono   db 0b011_011_01,
               0b101_101_10,
               0b111_111_11,
               0b011_011_01,
               0b101_101_10,
               0b111_111_11,
               0b011_011_01,
               0b101_101_10,

               0b111_111_11,
               0b011_011_01,
               0b101_101_10,
               0b111_111_11,
               0b011_011_01,
               0b101_101_10,
               0b111_111_11,
               0b011_011_01

  ns :turn_off_plus do
                        ld    bc, io_plus.reg
                        ld    a, io_plus.mode_group
                        out   (c), a
                        ld    b, io_plus.dta >> 8
                        xor   a
                        out   (c), a
                        ret
  end
end

if __FILE__ == $0
  require 'zxlib/basic'
  require_relative 'sprites.rb'

  class CemeteryGate
    include Z80
    include Z80::TAP

    label_import ZXLib::Sys, macros: true

    import              Ghosts, :gh

    with_saved :initialize, :exx, hl, ret: true do
                        create_chan_and_open output: bfont.print_char, strm_no: 4, chan_name: 'B'
                        call  gh.make_sincos
                        call  gh.init
                        ld    hl, font_ch8 - 0x100
                        ld    [vars.chars], hl
    end

    with_saved :put_sprite, :exx, hl, ret: :after_ei do
                        call  gh.put_sprite
    end

    with_saved :add_or_clear_sprites, :exx, hl, ret: true do
                        call  gh.add_or_clear_sprites
    end

    with_saved :draw_sprites, :exx, hl, ret: :after_ei do
                        call  gh.wait_key_or_break.release_key
                        call  gh.draw_sprites
    end

    with_saved :start_demo, :exx, hl, ret: true do |eoc|
                        xor   a
                        ld    [gh.spell_n], a
                        ld    [gh.nsprites], a
                        call  gh.wait_key_or_break
                        ld    a, [iy + vars.last_k - vars_iy]
                        cp    ?1.ord
                        jr    NZ, eoc
                        ld    a, 1
                        ld    [vars.repdel], a
                        ld    [vars.repper], a
                        ld    a, [vars.frames]
                        ld    [vars.seed], a
                        call  gh.clear_attrs_fx
                        call  gh.plus_pallette_on
                        call  gh.summon_sprite

      loop0             ld    a, [gh.nsprites]
                        ora   a
                        jr    Z, quit

                        call  gh.demo

                        ld    iy, vars_iy
                        restore_rom_interrupt_handler enable_intr:true
                        ld    hl, vars.flags
      release_key       call  rom.break_key
                        jr    C, no_break
                        call  gh.restore_keyboard_vars
                        call  gh.turn_off_plus
                        report_error 'D BREAK - CONT repeats'
      no_break          halt
                        bit   5, [hl]         # has a new key been pressed ?
                        jr    NZ, new_char
                        key_pressed?
                        jr    Z, loop0
                        jr    release_key
      new_char          res   5, [hl]         # clear new key flag
                        ld    a, [iy + vars.last_k - vars_iy]
                        call  gh.collect_key
                        call  gh.wait_key_or_break.release_key
                        xor   a               # BORDER 0
                        call  gh.set_border_color
                        jr    loop0
      quit              call  gh.restore_keyboard_vars
                        call  gh.turn_off_plus
                        ld    bc, 0           # return FALSE
                        exx
    end

    import              ZXUtils::BigFont, :bfont
    # Undead 8x8 font from https://damieng.com/typography/zx-origins/
    font_ch8            import_file "examples/fonts/Undead.ch8", check_size:768, data_type:8
  end

  program_code = CemeteryGate.new 0xA000
  sprites_code = Sprites.new program_code['gh.ghosts']
  $sys = ZXLib::Sys.new
  puts program_code.debug
  puts sprites_code.debug
  puts "jump_tables    size: #{program_code['+gh.jump_tables']}"
  puts "clear_and_draw size: #{program_code['+gh.clear_and_draw']}"
  puts "draw_bob       size: #{program_code['+gh.clear_and_draw.draw_bob']}"
  puts "draw_bob_lc2   size: #{program_code['+gh.clear_and_draw.draw_bob_lc2']}"
  puts "draw_bob_rc2   size: #{program_code['+gh.clear_and_draw.draw_bob_rc2']}"
  puts "draw_bob_lc1   size: #{program_code['+gh.clear_and_draw.draw_bob_lc1']}"
  puts "draw_bob_rc1   size: #{program_code['+gh.clear_and_draw.draw_bob_rc1']}"
  middle_top = (20 - 3*4)/2
  program = ZXLib::Basic.parse_source <<-EOB
   0 DEF FN g(x,y,c)=USR #{program_code[:put_sprite]}: DEF FN a(x,y,c,s)=USR #{program_code[:add_or_clear_sprites]}
     BORDER 0: PAPER 0: INK 7: BRIGHT 0: INVERSE 0: OVER 0: FLASH 0: CLS
  10 PRINT AT #{middle_top},0;
     RESTORE: FOR i=1 TO 4: READ c,a$: LET t=INT ((32-LEN a$)/2)
     INK c: BRIGHT 0
     PRINT #4;AT (24-PEEK #{$sys['vars.s_posn.line']})*8+2,t;a$(1): PRINT OVER 1;TAB t+2'BRIGHT 1;TAB t+2;a$(2 TO )'''
     NEXT i
  20 IF USR #{program_code[:start_demo]} THEN GO TO 20
  98 RUN
  99 REM *** test sprites ***
 100 INPUT "color: ";c
 110 INPUT "y: ";y;" x: ";x
 120 RANDOMIZE FN g(x,y,c): PAUSE 0
     GO TO 110
 200 INPUT "color: ";c;" step: ";step;" shift: ";shift: LET s=0
 210 OUT (254),1: RANDOMIZE USR #{program_code[:add_or_clear_sprites]}
     RANDOMIZE: FOR y=7 TO 176 STEP step
     LET x=8*INT (RND*29)+shift: LET n=FN a(x,y,c,s): LET s=s+1: IF s>3 THEN LET s=0
     NEXT y
     PRINT AT 0,0;"sprites: ";n;" shift: ";shift
     RANDOMIZE USR #{program_code[:draw_sprites]}
     LET shift=shift+1: IF shift>7 THEN LET shift=0
     GO TO 210
9000 DATA 6,"Press [1] to summon the spirits"
     DATA 2,"Or banish them with [0] key"
     DATA 5,"Swirling forever is their fate"
     DATA 4,"Unless you find a hidden spell"
9998 STOP: RUN
9999 CLEAR #{program_code.org-1}: LOAD ""CODE : LOAD ""CODE : RANDOMIZE USR #{program_code[:initialize]}: RUN
EOB
  tap_name = 'examples/ghosts.tap'
  program.save_tap(tap_name, line: 9999)
  program_code.save_tap(tap_name, append: true)
  sprites_code.save_tap(tap_name, append: true, name:'spirits')
  Z80::TAP.parse_file(tap_name) { |hb| puts hb.to_s }
  puts "MAX_SPRITES: #{Ghosts::MAX_SPRITES}"
  %w[ghosts
    wipe_stack0
    wipe_stack1
    sincos
    nsprites
    wipe_stack
    chase_dt
    sprindex
    spr_ys
    spell_n
    vars_top
    sprites
    shuffled
    clear_and_draw.wipe_region.restore_sp_p
    clear_and_draw.draw_bob.restore_sp_p
    demo.push_args.wipe_stack_cur_p
  ].map{|n| [n, program_code["gh.#{n}"]] }.
    sort{|a,b| a[1] <=> b[1]}.each do |n,addr|
    puts "#{n.ljust(32)}: #{'%04X' % addr}"
  end
end
