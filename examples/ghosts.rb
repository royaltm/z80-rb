# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'
require 'z80/stdlib'
# require 'z80/utils/shuffle'
require 'z80/utils/sincos'
require 'z80/utils/sort'
# require 'z80/utils/vec_deque'
require 'zxlib/sys'
require 'zxlib/gfx'
require 'zxlib/gfx/bobs'
# require 'zxlib/gfx/draw'
require 'zxutils/bigfont'

class Ghosts
  include Z80
  include Z80::TAP

  SinCosTable = Utils::SinCos::SinCosTable
  SinCos      = Utils::SinCos::SinCos

  export :auto

  MODE = :or
  MARK_MAX_Y = [-1].pack('c').unpack('C').first # because why not?
  WIPE_TERMINATOR = 0
  MAX_SPRITES = 24 # changing it to a larger value will be UB
  BORDER_PROFILE = false
  ASSERT_DEBUG = false

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

  ## Graphics
  ghost1        addr 0xF000
  ## Clear screen region addresses and heights.
  #  The second stack is being populated while the previous one is being pulled from.
  wipe_stack0   addr 0xF800
  wipe_stack1   addr wipe_stack0 ^ 0x0100, 1
  ## SinCostTable
  sincos        addr wipe_stack1 + 0x0100, SinCosTable
  ## Variables: number of sprites (must be in the range: 1..MAX_SPRITES)
  nsprites      addr sincos[1], 1
  ## Variables: last populated wipe stack (address MSB)
  wipe_stack    addr :next, 1
  ## Variables: frame interrupt counter
  frame_iflag   addr :next, 1
  ## Variables: ghost chase radius delta
  chase_dt      addr :next, 1
  ## Variables: collected spell step
  spell_n       addr :next, 1
  ## Sprite index, contains LSB of sprite addresses
  sprindex      addr wipe_stack0 - 0x0100, 1
  ## Unsigned sprite Y position (sprite.y + 15) being used for sorting.
  #  MARK_MAX_Y denotes OOS sprites.
  spr_ys        addr sprindex - 0x0100, 1
  ## Sprite control data
  sprites       addr spr_ys  - 0x0100, Sprite

  sanity_sp     addr 0xFF10 if ASSERT_DEBUG

  raise SyntaxError if nsprites.to_i > 0xFF00
  raise SyntaxError unless (sincos.to_i & 0xFF).zero?
  raise SyntaxError unless (wipe_stack0.to_i & 0xFF).zero?
  raise SyntaxError unless (wipe_stack1.to_i & 0xFF).zero?
  raise SyntaxError unless (wipe_stack0.to_i ^ wipe_stack1.to_i) == 0x0100
  raise SyntaxError if (spr_ys.to_i - sprindex.to_i).abs != 0x0100 || !(spr_ys.to_i & 0xFF).zero?
  raise SyntaxError unless (sprites.to_i & 0xFF).zero?

  macro_import MathInt
  macro_import Stdlib
  # macro_import Utils::Shuffle
  macro_import Utils::SinCos
  macro_import Utils::Sort
  # macro_import Utils::VecDeque
  label_import ZXLib::Sys, macros: true
  macro_import ZXLib::Gfx
  macro_import ZXLib::Gfx::Bobs
  # macro_import ZXLib::Gfx::Draw

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
  # a': color, hl: bitmap, de: xx, bc: yy, cols: 2 -> c: height, de: screen address
  macro :calc_screen_addrs_and_push_args do |eoc, oos:, height:16|
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
                        # 0 <= y < 256
    ypos                ld    a, 192 - height  # a: 176
                        sub   yl               # 176 - yl
                        jr    C, calc_h        # height: 176 - yl + 16 (yl > 176)
                        # 0 <= y <= 176
                        ld    a, height

    scraddrcalc         push  ix                 # jump_table
                        push  hl                 # bitmap
                        ex    af, af             # a: color, a': height
                        ld    l, a               # l: color_attr
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
                        ld    c, a               # c: height
                        push  bc                 # bshift|lines
                        # attribute rows
                        cp    height
                        jr    NC, lines_full
    lines_trunc         add   a, 7
                        anda  0b11000
                        3.times { rrca }         # rows: 1 or 2
                        jp    skip_full
    lines_full          ld    a, d               # rows determined from screen address
                        ora   0b1111_1000
                        add   a, 7
                        ld    a, 1
                        adc   a, a               # rows: 2 or 3
    skip_full           ld    h, a               # rows
                        push  hl                 # rows|attr_color
                        # attribute address
                        scrtoattr(d, o:h, scraddr:0x4000)
                        ld    l, e
                        push  hl                 # attrs_addr
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
  ## Converts screen address low nibble for the clear_screen_region_fast.
  # Modifies: af, tl
  macro :clr_scr_addr do |_, tl|
                        ld    a, tl
                        add   2       # yyyxxxxx
                        xor   tl      # 000zzzzz vs dddzzzzz
                        cp    32
                        jr    C, noedge
                        ld    a, tl
                        cpl
                        anda  31
    noedge              xor   tl      # yyyxxxxx
                        ld    tl, a
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
          wipe_stack_next.push clr_scr_addr(scr_addr), wipe_lines, spr_ys[i] + 1
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
  # hl: wipe_stack_last (top), iy: wipe_stack_next (top), a: nsprites (> 0)
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
                        ld    h, [hl]; ld   l, a # bitmap
    # a': color, hl: bitmap, de: xx, bc: yy, cols: 2 -> c: height, de: screen address
                        calc_screen_addrs_and_push_args(oos: proc do
                          exx
                          ld    b, spr_ys >> 8     # bc: spr_ys index
                          jp    next_sprite
                        end)
                        # store wipe
                        clr_scr_addr e           # de: screen -> clr_screen
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
                        # ZF,_,c: bc + a
                        add   c
                        ld    c, a
                        adc   b
                        sub   c         # ZF: 1 if y > -height and y < 256
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
  end

=begin
  sprindex, spr_ys = sprindex.zip(spr_ys).sort { |a, b| a[1] <=> b[1] }.transpose
=end
  ## Sorts both sprindex and spr_ys using spr_ys for order.
  macro :sort_sprite_index do
                        ld   b, 0
                        ld   h, sprindex >> 8
                        ld   d, h
                        exx
                        ld   a, [nsprites]
                        insertion_sort_bytes_max256(
                            reverse:false,
                            target:spr_ys,
                            length:a) do |eoc|
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
                        ld    a, [hl]          # Sprite.bitmap
                        add   a, height*2
                        anda  0x7F
                        ld    [hl], a
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
                        # ZF,_,c: bc + a
                        add   c
                        ld    c, a
                        adc   b
                        sub   c                # ZF: 1 if y > -height and y <= 256 - height
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
                        ld    a, e
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
  # Pops arguments from stack and acts accordingly.
  ns :clear_and_draw do
                        di
                        debug_border(4)
    loop0               label
                        # ei; call wait_key; di
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

    end_stage           ei
                        debug_border(0)
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
  dc! "==                  P O P U L A T E                  =="
  dc! "======================================================="

  ## Populate sprites
  ns :populate do
                        call  fn_argn
                        jr    NC, check_args
    no_args             ld    a, [nsprites]
                        ld    e, a
                        ld    d, 0
                        jr    skip_args
    check_args          label
    error_q             report_error_unless Z, 'Q Parameter error'
                        call  get_uint_de
                        ld    a, e
    skip_args           ora   d
                        jp    Z, error_a
                        cp16n d, e, MAX_SPRITES + 1
                        jp    NC, error_a
    num_ok              ld    a, e
                        ld    [nsprites], a
                        ld    b, a
                        # initialize wipe stacks
                        ld    a, [wipe_stack]
                        ora   a
                        call  Z, init.wipe
                        # initialize sprites
                        ld    ix, sprites
                        ld    de, sprindex
                        xor   a
    loop0               ld    [de], a
                        inc   de
                        ld    hl, ghost1
                        ld    [ix + sprites.bitmap.l], l
                        ld    [ix + sprites.bitmap.h], h
                        call  next_rng
                        ld    [ix + sprites.angle], a
                        ld    l, a
                        ld    h, 0
                        ld    c, 16
                        sub_from c, h, l
                        ld    [ix + sprites.y.l], l
                        ld    [ix + sprites.y.h], h
                        call  next_rng
                        ld    [ix + sprites.radius], a
                        ld    l, a
                        ld    h, 0
                        ld    c, 8
                        sub_from c, h, l
                        ld    [ix + sprites.x.l], l
                        ld    [ix + sprites.x.h], h
                        call  next_rng
                        anda  7
                        sub   4
                        sbc   a, -1 # a: { -7..-1, 1..7 }
                        ld    [ix + sprites.direction], a
    random_color        call  next_rng
                        ld    c, [ix + sprites.color_attr]
                        xor   c
                        anda  0b01000111
                        xor   c
                        ld    [ix + sprites.color_attr], a
                        anda  7
                        jr    Z, random_color

                        ld    a, +Sprite
                        add   a, ixl
                        ld    ixl, a
                        djnz  loop0
                        ld    a, [nsprites]
                        ld    b, 0
                        ld    c, a
                        exx
                        ret
  end

  next_rng              exx
                        ld   hl, [vars.seed]
                        rnd
                        ld   [vars.seed], hl
                        ld   a, l
                        exx
                        ret

  # hl: Sprite.color_attr
  set_random_color      call  next_rng
                        ld    c, [hl]
                        xor   c
                        anda  0b01000111
                        xor   c
                        ld    [hl], a
                        anda  7
                        ret   NZ
                        jr    set_random_color

  # hl: Sprite.direction
  set_random_direction  call  next_rng
                        anda  7
                        sub   4
                        sbc   a, -1 # a: { -7..-1, 1..7 }
                        ld [hl], a
                        ret

  dc! "======================================================="
  dc! "==                      D E M O                      =="
  dc! "======================================================="

  ns :demo do
    demo_loop           debug_border(5)
                        # sprite_index_to_spr_y
                        move_sprites
                        debug_border(3)
                        sort_sprite_index
                        debug_border(6)
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

                        debug_border(1)

                        ld    hl, frame_iflag
                        xor   a
                        cp    [hl]
                        jr    NZ, skip_sync
                        ei
                        halt
    skip_sync           ld    [hl], a
                        jp    clear_and_draw
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
                        call  set_random_color
                        anda  0b01111111
                        ld    [hl], a
                        ld    de, ghost1
                        inc   l; ld [hl], e    # hl: -> Sprite.bitmap
                        inc   l; ld [hl], d
                        inc   l
                        call  set_random_direction
                        call  next_rng
                        inc   l; ld [hl], a    # hl: -> Sprite.angle
                        xor   a
                        inc   l; ld [hl], a    # hl: -> Sprite.radius
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
                ret  NZ
    found_first ld   a, +spells*2 - 1
                sub  c
                ld   [de], a
                ret
    check_next  adda_to h, l
                ex   af, af      # restore char
                cp   [hl]
                jr   Z, ok_char
    bad_char    ld   a, 2
                out  (io.ula), a
                halt
                halt
                xor  a
                out  (io.ula), a
    clear_exit  xor  a
                ld   [de], a
                ret
    ok_char     ld   a, [de]
                add  a, c        # +spells
                cp   +spells * 3
                ld   [de], a
                ret  C           # next collected
                sub  +spells * 3
                add  a, a
                ld   hl, spell_jp
                adda_to h, l
                ld   a, [hl]
                inc  hl
                ld   h, [hl]
                ld   l, a
                push hl          # ret will jump
                jr   clear_exit

    spells      data "rfmcvnwsq"
                data "nlooaeotq"
                data "danlcgooq"
    spell_jp    dw spell1
                dw spell2
                dw spell3
                dw spell4
                dw spell5
                dw spell6
                dw spell7
                dw spell8
                dw spell9
  end

  ns :put_sprite do
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
                        ex    af, af
                        pop   bc     # yy
                        pop   de     # xx
                        ld    hl, ghost1
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
                        clr_scr_addr e
                        ld    [clear_scr_addr], de
                        jp    clear_and_draw
  end

  dc! "======================================================="
  dc! "==               S U B R O U T I N E S               =="
  dc! "======================================================="

  make_sincos create_sincos_from_sintable sincos, sintable:sintable
  sintable    bytes   neg_sintable256_pi_half_no_zero_lo

  clear_scr_addr        dw 0
  clear_scr_lines       db 0
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

  ns :interrupt_handler do
                        push  hl
                        ld    hl, frame_iflag
                        ld    [hl], -1
                        pop   hl
                        ret
  end

  ns :setup_interrupts do
                        di
                        memcpy 0xFFF4, interrupt_handler, +interrupt_handler
                        ld  a, 0x18     # 18H is jr
                        ld  [0xFFFF], a
                        ld  a, 0x3B
                        ld  i, a
                        im2
                        ei
                        ret
  end

  # Waits until specified keys are being pressed.
  wait_key      halt
                key_pressed?
                jr   Z, wait_key
  # Waits until no key is being pressed.
  release_key   halt
                key_pressed?
                jr   NZ, release_key
                ret
end

class Spirits
  include Z80
  include Z80::TAP

  sprite1 bytes [
      0b00000111, 0b00000000,
      0b00000011, 0b10000000,
      0b00000011, 0b11000000,
      0b00000111, 0b11100000,
      0b10001101, 0b10100001,
      0b11111111, 0b11110011,
      0b11111111, 0b11111111,
      0b11001111, 0b11111111,
      0b11000111, 0b11100011,
      0b00000111, 0b11100001,
      0b00000111, 0b11100000,
      0b00001111, 0b11110000,
      0b00001111, 0b11110000,
      0b00011111, 0b11111000,
      0b00011111, 0b11111000,
      0b00000111, 0b11000000]

  sprite2 bytes [
      0b00000011, 0b00000000,
      0b00000101, 0b10000000,
      0b00000011, 0b11000000,
      0b00000111, 0b11100000,
      0b11000101, 0b10110000,
      0b11111111, 0b11110011,
      0b11111111, 0b11111111,
      0b11001111, 0b11111111,
      0b10000111, 0b11100111,
      0b10000111, 0b11100011,
      0b00001111, 0b11100001,
      0b00001111, 0b11110000,
      0b00001111, 0b11110000,
      0b00011111, 0b11111000,
      0b00011111, 0b11111000,
      0b00110000, 0b11111000]

  sprite3 bytes [
      0b00000111, 0b00000000,
      0b00000001, 0b10000000,
      0b00000011, 0b11000000,
      0b00000111, 0b11100000,
      0b11001101, 0b10100001,
      0b11111111, 0b11110011,
      0b11111111, 0b11111111,
      0b11001111, 0b11111111,
      0b10000111, 0b11100011,
      0b00000111, 0b11100001,
      0b00000111, 0b11110000,
      0b00000111, 0b11110000,
      0b00001111, 0b11111000,
      0b00011111, 0b11111000,
      0b00011111, 0b11110000,
      0b00001111, 0b10000000]

  sprite4 bytes [
      0b00000011, 0b10000000,
      0b00000101, 0b11000000,
      0b00000011, 0b11000000,
      0b00000111, 0b11100001,
      0b10000101, 0b10100011,
      0b11111111, 0b11110011,
      0b11111111, 0b11111111,
      0b11001111, 0b11111111,
      0b10000111, 0b11100001,
      0b00000111, 0b11100000,
      0b00000111, 0b11110000,
      0b00000111, 0b11110000,
      0b00001111, 0b11111000,
      0b00011111, 0b11111000,
      0b00011111, 0b11111000,
      0b00011100, 0b00000000]
end


if __FILE__ == $0
  require 'zxlib/basic'

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

    with_saved :test_sprite, :exx, hl, ret: true do
                        call  gh.put_sprite
    end

    with_saved :start_demo, :exx, hl, ret: true do |eoc|
                        ld    a, 1
                        ld    [vars.repdel], a
                        ld    [vars.repper], a
                        xor   a
                        ld    [gh.spell_n], a
                        ld    [gh.nsprites], a
                        call  gh.wait_key
                        jr    check_key
      loop0             ld    a, [gh.nsprites]
                        ora   a
                        jr    Z, eoc
                        call  gh.setup_interrupts
      loop1             call  gh.demo
                        key_pressed?
                        jr    Z, loop1
                        di
                        ld    iy, vars_iy
                        restore_rom_interrupt_handler enable_intr:true
      release_key       call  rom.break_key
                        jr    C, no_break
                        ld    [iy + vars.repdel - vars_iy], 35
                        ld    [iy + vars.repper - vars_iy], 5
                        report_error 'D BREAK - CONT repeats'
      no_break          key_pressed?
                        halt
                        jr    NZ, release_key
      check_key         ld    hl, vars.flags
                        bit   5, [hl]         # has a new key been pressed ?
                        jr    Z, loop0        # nope, continue
                        res   5, [hl]         # clear new key flag
                        ld    a, [iy + vars.last_k - vars_iy]
                        call  gh.collect_key
                        jr    loop0
    end

    with_saved :populate, :exx, hl, ret: true do
                        call  gh.populate
    end

    import              ZXUtils::BigFont, :bfont
    font_ch8            import_file "examples/fonts/Undead.ch8", check_size:768, data_type:8
  end

  program_code = CemeteryGate.new 0xA000
  spirits_code = Spirits.new program_code['gh.ghost1']
  $sys = ZXLib::Sys.new
  puts program_code.debug
  puts spirits_code.debug
  puts "jump_tables    size: #{program_code['+gh.jump_tables']}"
  puts "clear_and_draw size: #{program_code['+gh.clear_and_draw']}"
  puts "draw_bob       size: #{program_code['+gh.clear_and_draw.draw_bob']}"
  puts "draw_bob_lc2   size: #{program_code['+gh.clear_and_draw.draw_bob_lc2']}"
  puts "draw_bob_rc2   size: #{program_code['+gh.clear_and_draw.draw_bob_rc2']}"
  puts "draw_bob_lc1   size: #{program_code['+gh.clear_and_draw.draw_bob_lc1']}"
  puts "draw_bob_rc1   size: #{program_code['+gh.clear_and_draw.draw_bob_rc1']}"
  middle_top = (20 - 3*4)/2
  program = ZXLib::Basic.parse_source <<-EOB
   0 DEF FN n(x)=x-(65536 AND x>=32768)
     DEF FN g(x,y,c)=USR #{program_code[:test_sprite]}
     DEF FN a(n)=USR #{program_code[:populate]}
     BORDER 0: PAPER 0: INK 7: BRIGHT 0: INVERSE 0: OVER 0: FLASH 0: CLS
     PRINT AT #{middle_top},0;
     FOR i=1 TO 4: READ c,a$: LET t=INT ((32-LEN a$)/2)
     INK c: BRIGHT 1
     PRINT #4;AT (24-PEEK #{$sys['vars.s_posn.line']})*8+2,t;a$(1): PRINT OVER 1;TAB t+2'TAB t+2;a$(2 TO )'''
     NEXT i
     PAUSE 0
  10 RANDOMIZE : REM LET n=FN a(1)
     RANDOMIZE USR #{program_code[:start_demo]}
  99 RUN
 100 INPUT "y: ";y;" x: ";x
 110 RANDOMIZE FN g(x,y,BIN 10110001): PAUSE 0
     GO TO 100
9000 DATA 6,"Press [1] to summon spirits"
     DATA 2,"Or banish them with [0] key"
     DATA 5,"Flying away is what they can"
     DATA 4,"Unless you find another spell"
9998 STOP: RUN
9999 CLEAR #{program_code.org-1}: LOAD ""CODE : LOAD ""CODE : RANDOMIZE USR #{program_code[:initialize]}: RUN
EOB
  tap_name = 'examples/ghosts.tap'
  program.save_tap(tap_name, line: 9999)
  program_code.save_tap(tap_name, append: true)
  spirits_code.save_tap(tap_name, append: true, name:'spirits')
  Z80::TAP.parse_file(tap_name) { |hb| puts hb.to_s }

  %w[wipe_stack0
    wipe_stack1
    sincos
    nsprites
    wipe_stack
    frame_iflag
    chase_dt
    sprindex
    spr_ys
    sprites
    clear_and_draw.wipe_region.restore_sp_p
    clear_and_draw.draw_bob.restore_sp_p
    demo.push_args.wipe_stack_cur_p
    demo.setup_wipe
  ].map{|n| [n, program_code["gh.#{n}"]] }.
    sort{|a,b| a[1] <=> b[1]}.each do |n,addr|
    puts "#{n.ljust(32)}: #{'%04X' % addr}"
  end
end
