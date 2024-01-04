require 'z80'
require 'z80/stdlib'
require 'zxlib/gfx/draw'
require 'zxlib/basic'

class Float
  ##
  # Converts a float to a 16-bit fixed point twos complement number formatted: iiiiiiiiffffffff
  # where i represents the absolute integer part bits and f represents the fraction part.
  def to_fixed
    n = (self * 256.0).round
    raise ArgumentError if n >= 32768 || n <= -32768
    n & 0xffff
  end
end

## Quaternion helper methods
module Quaternion
  include Math
  ## Returns a rotor quaternion from radians and a unit vector.
  def rotor_quaternion(rad, av)
    sn = sin(rad/2)
    x, y, z = av
    [cos(rad/2),sn*x,sn*y,sn*z]
  end

  ## Returns a quaternion square norm.
  def quaternion_norm_q(q)
    s, x, y, z = q
    s*s + x*x + y*y + z*z
  end

  ## Returns a quaternion norm.
  def quaternion_norm(q)
    sqrt(quaternion_norm_q(q))
  end

  ## Returns a normalized quaternion.
  def normalize_quaternion(q)
    n = quaternion_norm(q)
    s, x, y, z = q
    [s/n, x/n, y/n, z/n]
  end

  ## Returns a quaternion as a cross product of quaternions a and b.
  def quaternion_cross_product(a, b)
    sa, xa, ya, za = a
    sb, xb, yb, zb = b
    [sa*sb - xa*xb - ya*yb - za*zb,
     sa*xb + sb*xa + ya*zb - yb*za,
     sa*yb + sb*ya + za*xb - zb*xa,
     sa*zb + sb*za + xa*yb - xb*ya]
  end

  ## Returns a transformation matrix from a quaternion.
  def quaternion2matrix(q)
    sq, xq, yq, zq = normalize_quaternion(q)

    xq2 = xq*xq
    yq2 = yq*yq
    zq2 = zq*zq

    xqyq = xq*yq
    sqzq = sq*zq
    xqzq = xq*zq
    sqyq = sq*yq
    yqzq = yq*zq
    sqxq = sq*xq

    qxx = 1.0 - 2.0*(yq2 + zq2)
    qyy = 1.0 - 2.0*(xq2 + zq2)
    qzz = 1.0 - 2.0*(xq2 + yq2)

    qxy = (xqyq - sqzq)*2.0
    qxz = (xqzq + sqyq)*2.0
    qyx = (xqyq + sqzq)*2.0
    qyz = (yqzq - sqxq)*2.0
    qzx = (xqzq - sqyq)*2.0
    qzy = (yqzq + sqxq)*2.0

    [qxx, qxy, qxz,
     qyx, qyy, qyz,
     qzx, qzy, qzz]
  end
end

## The code
class Quat3D
  extend Quaternion
  include Z80
  include Z80::TAP

  ## Color attributes
  BG_ATTR = 0b01100000

  ## Perspective scale factor: [0-7]
  PERSP_DSHIFT = 7

  ## A 3 element vector of 8-bit signed integers.
  class Vector < Label
    z   byte
    y   byte
    x   byte

    S = to_struct
  end

  ## A 3 element vector and 2 screen coordinates.
  class Vertex < Label
    vec Vector
    xp  byte
    yp  byte
    scr xp word

    S = to_struct

    ## Creates a Vertex data argument for given coordinates.
    def Vertex.make(x, y, z)
      x, y, z = x.round, y.round, z.round
      xp = ((x << PERSP_DSHIFT)/(z + 128)) + 128
      yp = ((y << PERSP_DSHIFT)/(z + 128)) + 96
      if xp < 0 || xp > 255 || yp < 0 || yp > 191
        xp = 255
        yp = 255
      end
      S.new(Vector::S.new(z, y, x), xp, yp)
    end

    ## Creates many Vertex data arguments from triplets: [x, y, z].
    def Vertex.make_many(*args)
      args.map{|x, y, z| Vertex.make(x, y, z)}
    end

    ## Creates a re-scaled Vertex data arguments.
    def Vertex.scale(sc, vertex)
      if vertex.is_a?(Array)
        vertex.map{|v| Vertex.scale(sc, v)}
      else
        vec = vertex.vec
        Vertex.make(vec.x*sc, vec.y*sc, vec.z*sc)
      end
    end
  end

  ## A data type representing a transformation matrix 3x3.
  class Matrix < Label
    xx  word
    xy  word
    xz  word
    yx  word
    yy  word
    yz  word
    zx  word
    zy  word
    zz  word
  end

  ###########
  # Imports #
  ###########

  macro_import    Z80Lib
  macro_import    Z80MathInt
  macro_import    ZXGfxDraw
  import          ZXSys, macros: true

  ##########
  # Macros #
  ##########

  # < th: 8-bit register
  # < tl: 8-bit register
  # < m: 8-bit register
  # > thtl: thtl / m
  macro :divmod16_8 do |eoc, th, tl, m|
                      divmod th, m, check0:eoc, check1:eoc
                      divmod tl, m, clrrem:false
                      anda a # clear CF
  end

  # < nshift: bits shift left
  # < th: 8-bit register
  # < tl: 8-bit register
  # > thtl: (tl as unsigned int 16) << nshift
  macro :lshift8_16 do |eoc, nshift, th, tl|
    raise ArgumentError unless Integer === nshift and nshift >= 0
    case nshift
    when 0 # 7
                      ld   th, 0
    when 1 # 18|23
                      ld   th, 0
        if th == h && tl == l
                      add  hl, hl
        else
                      sla  tl
                      rr   th
        end
    when 2..7 # 29|35,39,43,39,35,31
        if nshift == 2 && th == h && tl == l # 29
                      ld   h, 0
                      add  hl, hl
                      add  hl, hl
        else
                      ld   a, tl
          if nshift > 4
                      (8-nshift).times { rrca } # 0b00000000
                      ld   th, a
                      anda (0xFF << nshift) & 0xFF
                      ld   tl, a
                      xor  th
                      ld   th, a
          else
                      nshift.times { rlca }
                      ld   tl, a
                      anda (1 << nshift) - 1
                      ld   th, a
                      xor  tl
                      ld   tl, a
          end
        end
    when 8 # 11
                      ld   th, tl
                      ld   tl, 0
    when 9..15 # 19 + (nshift-9)*4
                      ld   a, tl
                      (nshift-8).times { add a }
                      ld   th, a
                      ld   tl, 0
    else # 10 | 11
        if th.match16?(tl)
                      ld   th|tl, 0
        else
                      ld   tl, 0
                      ld   th, tl
        end
    end
  end

  # Applies a matrix to vertices and calculates screen coordinates (xp, yp) for each one.
  # Vertices list must be terminated with -128 and must not be empty.
  # Uses stack pointer to read matrix data, so disable interrupts first.
  # < matrix: a matrix address
  # < vertices: an address of object vertices or hl
  # > hl: an address immediately following the end marker
  macro :apply_matrix do |_, matrix, vertices:hl|
    # x = qxx*x + qxy*y + qxz*z
    # y = qyx*x + qyy*y + qyz*z
    # z = qzx*x + qzy*y + qzz*z
                      ld   hl, vertices unless vertices==hl
                      ld   b, [hl]                # b: z
                      inc  hl                     # hl: -> y
    matrix_loop       ld   sp, matrix
    matrix_p          matrix_loop + 1             # a label pointer to a matrix argument
                      ld   d, [hl]                # d: y
                      inc  hl                     # hl: -> x
                      ld   e, [hl]                # e: x
                      apply_matrix_coord e, d, b  # x = qxx*x + qxy*y + qxz*z
                      ex   af, af                 # a': new x
                      apply_matrix_coord e, d, b  # y = qyx*x + qyy*y + qyz*z
                      ld   c, a                   # c: new y
                      apply_matrix_coord e, d, b  # z = qzx*x + qzy*y + qzz*z
                      inc  hl                     # hl: -> xp
    # calculate xp, yp
    # xp = ((x << PERSP_DSHIFT)/(z + 128)) + 128
    # yp = ((y << PERSP_DSHIFT)/(z + 128)) + 96
                      exx
    adjust_z          add  128              # a: z + 128
    adjust_z_p        adjust_z + 1          # a label pointer to adjust_z
                      ld   c, a             # c: z
                      ex   af, af           # a: x
                      anda a
                      jp   P, pos_x
                      neg
    pos_x             ld   e, a             # x
                      ex   af, af           # f': CF: sign
                      lshift8_16 PERSP_DSHIFT, d, e # de: x << PERSP_DSHIFT
                      divmod16_8 d, e, c    # (x << PERSP_DSHIFT) / z
                      jr   C, x_overflow    # z=0
                      ld   a, d
                      anda a
                      jr   NZ, x_overflow   # de >= 256
                      ex   af, af           # f: CF: sign
                      jr   C, x_negative
                      ld   a, e             # x  >= 0
                      add  128              # x to screen coordinates
                      jr   NC, skip_neg_x   # xp <  256
                                            # xp >= 256
    x_overflow        ld   a, 255           # store xp=255, yp=255 (out of screen)
                      exx
                      ld   [hl], a
                      inc  hl
                      jr   skip_overflow_x

    x_negative        ld   a, 128           # x  <  0
                      sub  e                # x to screen coordinates
                      jr   C, x_overflow    # xp <  0
    skip_neg_x        exx
                      ld   [hl], a          # store xp
                      inc  hl
                      ld   a, c             # y
                      exx
                      anda a
                      jp   P, pos_y
                      neg
    pos_y             ld   e, a             # y
                      ex   af, af           # f': CF: sign
                      lshift8_16 PERSP_DSHIFT, d, e # de: y << PERSP_DSHIFT
                      divmod16_8 d, e, c    # (y << PERSP_DSHIFT) / z
                      jr   C, y_overflow    # z=0
                      ld   a, d
                      anda a
                      jr   NZ, y_overflow   # de >= 256
                      ex   af, af           # f: CF: sign
                      jr   C, y_negative
                      ld   a, e             # y  >= 0
                      add  96               # y to screen coordinates
                      jr   NC, skip_neg_y   # yp <  256

    y_overflow        ld   a, 255           # yp >= 256
                      jr   skip_neg_y

    y_negative        ld   a, 96            # y  <  0
                      sub  e                # y to screen coordinates
                      jr   C, y_overflow    # yp <  0
    skip_neg_y        exx
    skip_overflow_x   ld   [hl], a          # store yp
                      inc  hl               # -> z
                      ld   a, [hl]          # a: z
                      inc  hl
                      cp   128              # is end marker?
                      ld   b, a             # b: z
                      jp   NZ, matrix_loop
  end

  # < x: a register with signed int8 x
  # < y: a register with signed int8 y
  # < z: a register with signed int8 z
  # < sp -> matrix elements in a special 15 bit + sign fixed point format: iiiiiiiiffffffff
  # > a: int(([sp]*x + [sp+2]*y + [sp+4]*z + 0.5) / 256)
  # > sp: sp + 6
  macro :apply_matrix_coord do |_, x, y, z|
                      apply_matrix_component x, clrhl: true  # qxx*x
                      exx
                      apply_matrix_component y, clrhl: false # +qxy*y
                      exx
                      apply_matrix_component z, clrhl: false # +qxz*z

                      xor  a
                      sla  l
                      adc  h                                 # (sum+0.5)/256

                      exx
  end

  # < x: a register with signed int8 
  # < sp -> matrix element in a special 15 bit + sign fixed point format: iiiiiiiiffffffff
  # > hl: [sp] * x
  # > sp: sp + 2
  # > x': x
  macro :apply_matrix_component do |_, x, clrhl:false|
                      ld   a, x
                      exx
                      pop  de          # de: qxx = iiiiiiiiffffffff
      multiply        mul8_signed(d, e, a, tt:de, t:c, clrhl:clrhl, double:false)
  end

  # Draws a line between vertices.
  # < v1: Vertex label
  # < v2: Vertex label
  # < draw_line: a drawing routine address
  macro :draw_wire do |_, v1, v2, draw_line: draw.line_to|
                      ld   de, [v1.scr]     # 20
                      ld   hl, [v2.scr]     # 16
                      call draw_line
  end

  # Draws a poly-line between many vertices.
  # < vertices: an array of Vertex labels
  # < closed: if the last vertex should be connected to the first
  # < draw_line: a drawing routine address
  macro :draw_poly_wire do |_, *vertices, closed: true, draw_line: draw.line_to|
    raise ArgumentError if vertices.length < 3
    vertices.each_cons(2) do |v1, v2|
                      draw_wire v1, v2, draw_line:draw_line
    end
    if closed
                      draw_wire vertices.last, vertices.first, draw_line:draw_line
    end
  end

  ########
  # Main #
  ########

  with_saved :start, :exx, hl, ret: true do |eoc|
    release_key         halt
                        key_pressed?
                        jr   NZ, release_key # wait for any key being pressed to be released
                        # prepare both shadow and regular screen memory, disabling interrupts first
                        mmu128_select_bank(bank:7, screen:0, disable_intr:true, enable_intr:false)
                        ld   a, BG_ATTR
                        call clear_screen
                        mmu128_swap_screens(swap_bank:true, disable_intr:false, enable_intr:false)
                        ld   a, BG_ATTR
                        call clear_screen

                        # reset adjustment of z
                        ld   a, 128 + 58
                        ld   [draw_loop.apply_matrix_a.adjust_z_p], a
                        # save sp
                        ld   [draw_loop.restore_sp_p], sp
    ns :draw_loop do
                        # clear shadow screen pixel area where the new frame will be rendered
                        clear_screen_region_fast(xy_to_pixel_addr(40+22*8, 10, scraddr:0xC000), 180, 22, disable_intr:false, enable_intr:false, save_sp:false)
      object_a          ld   hl, cubes
      object_p          object_a + 1 # a pointer to a current object's address
                        # apply a current matrix to a current object's vertices and calculate screen coordinates
      apply_matrix_a    apply_matrix matrix
                        # both apply_matrix and clear_screen_region_fast uses stack pointer so let's restore it
      restore_sp        ld   sp, 0
      restore_sp_p      restore_sp + 1
                        # draw current object to the shadow screen's memory
                        call rom.call_jump # effectively call (hl)
                        # set next matrix address
                        ld   hl, [apply_matrix_a.matrix_p]
                        ld   bc, +matrix
                        add  hl, bc
                        cp16n h, l, matrix_end, jr_msb_c:skip_matrix_reset, jr_msb_nz:matrix_reset
                        jr   C, skip_matrix_reset
      matrix_reset      ld   hl, matrix
      skip_matrix_reset ld   [apply_matrix_a.matrix_p], hl
                        # display shadow screen and swap bank 7 to the previous (now shadow) screen
                        mmu128_swap_screens(swap_bank:true, disable_intr:false, enable_intr:false)
                        # modify adjust_z value until object is near
                        ld   hl, apply_matrix_a.adjust_z_p
                        ld   a, [hl]
                        cp   128 - 8
                        jr   Z, skip_adjust_z
                        dec  [hl]
                        # loop unless key is pressed
      skip_adjust_z     key_pressed?
                        jp   Z, draw_loop
                        # wait until key is released or a break key was pressed
      release_key2      ei
                        halt
                        di
                        call rom.break_key
                        jr   NC, quit
                        key_pressed?
                        jr   NZ, release_key2
      ns do |eoc|
                        ld   a, [vars.last_k]
                        sub  0x30
                        jr   Z, sel_blk
                        cp   9
                        jr   Z, sel_nink
                        cp   8
                        jr   Z, sel_ink0
                        jr   NC, eoc
                        3.times { rlca }
                        ld   c, a
                        cp   0b00100000
                        sbc  a, a
                        anda 7
                        ora  c
                        jr   paint_st
        sel_blk         ld   a, [last_color]
                        3.times { rrca }
                        anda 7
                        jr   paint
        sel_nink        ld   a, [last_color]
                        ld   c, a
                        inc  a
                        xor  c
                        anda 7
                        xor  c
                        jr   paint_st
        sel_ink0        ld   a, [last_color]
                        anda ~7
        paint_st        ld   [last_color], a
        paint           push af
                        call clear_screen
                        mmu128_swap_screens(swap_bank:true, disable_intr:false, enable_intr:false)
                        pop  af
                        call clear_screen
                        jp   draw_loop
      end
      ns do
                        # find current object
                        ld   hl, [object_p]
                        ld   sp, objects
        search_loop     pop  de
                        ld   a, d
                        ora  e
                        jr   Z, not_found
                        cp16r h,l, d,e, jr_msb_nz: search_loop
                        jr   NZ, search_loop
                        # get next object
                        pop  de
                        ld   a, d
                        ora  e
                        jr   NZ, set_object
                        # next object is the first one
        not_found       ld   sp, objects
                        pop  de
        set_object      ld   [object_p], de
      end
                        jp   draw_loop
    end # draw_loop
                        # reset bank and screen and enable interrupts
    quit                mmu128_select_bank(bank:0, screen:0, disable_intr:false, enable_intr:true)
                        call rom.cl_all
                        ld   a, [vars.bordcr]
                        call set_border_cr
  end

  last_color            db   BG_ATTR

  ###############
  # Subroutines #
  ###############

  ##
  # Clears shadow screen attributes while creating a chequered pattern by alternating brightness bit,
  # sets border and clears ink/paper area.
  # < a: initial color attribute value
  ns :clear_screen do
                      ld   hl, mem128.attrs_alt
                      ld   c, 24
      rows_loop       ld   b, 32
      cols_loop       ld   [hl], a
                      inc  hl
                      xor  0b01000000
                      djnz cols_loop
                      xor  0b01000000
                      dec  c
                      jr   NZ, rows_loop
  end

  # Clears shadow ink/paper memory.
                      clrmem  mem128.screen_alt, mem.scrlen, 0
  # < a: border color in paper bits
  set_border_cr       anda 0b00111000
                      3.times { rrca }
                      out  (io.ula), a
                      ret

  ## Line drawing routines.
  draw                make_draw_line_subroutines(make_line:true, make_line_over:false, make_line_inversed:false,
                                                 scraddr:0xC000, check_oos:false)

  #############
  # 3D Models #
  #############

  ## 3D object pointers
  objects             dw cubes,
                         spaceship1,
                         spaceship2,
                         elite,
                         0

  ns :spaceship1 do
    vs                data Vertex, *Vertex.make_many(
                        [  0,   0, 50],
                        [-45,   0,-50],
                        [ 45,   0,-50],
                        [-10, -10,-50],
                        [ 10, -10,-50],
                        [ 10,  10,-50],
                        [-10,  10,-50],
                        [  0, -20,-50])
                      db   -128
                      draw_poly_wire vs[0], vs[3], vs[1]
                      draw_poly_wire vs[0], vs[6], vs[1]
                      draw_poly_wire vs[0], vs[2], vs[4]
                      draw_poly_wire vs[0], vs[2], vs[5]
                      draw_poly_wire vs[3], vs[4], vs[5], vs[6]
                      draw_wire vs[3], vs[6]
                      draw_wire vs[4], vs[5]
                      draw_wire vs[0], vs[7]
                      draw_wire vs[3], vs[7]
                      draw_wire vs[4], vs[7]
                      ret
  end

  ns :spaceship2 do
    vs                data Vertex, *Vertex.make_many(
                        [  0,  0,-50],  # 0
                        [-10, 20, 10],  # 1
                        [ 10, 20, 10],  # 2
                        [-20,  0,-10],  # 3
                        [ 20,  0,-10],  # 4
                        [-20, 20, 40],  # 5
                        [ 20, 20, 40],  # 6
                        [-30,  0, 40],  # 7
                        [ 30,  0, 40],  # 8
                        [-40,-30,-40],  # 9
                        [ 40,-30,-40],  #10
                        [-40,-29,-10],  #11
                        [ 40,-29,-10],  #12
                        [-40,-30,  0],  #13
                        [ 40,-30,  0])  #14
                      db   -128
                      draw_poly_wire vs[0], vs[3], vs[1], vs[2], vs[4]
                      draw_poly_wire vs[3], vs[7], vs[5], vs[1], closed:false
                      draw_poly_wire vs[2], vs[6], vs[8], vs[4], closed:false
                      draw_poly_wire vs[3], vs[11], vs[9], vs[13], vs[7], closed:false
                      draw_poly_wire vs[8], vs[14], vs[10], vs[12], vs[4], closed:false
                      draw_poly_wire vs[1], vs[0], vs[2], closed:false
                      draw_wire vs[5], vs[6]
                      draw_wire vs[7], vs[8]
                      draw_wire vs[3], vs[4]
                      ret
  end

  ns :elite do
    vs                data Vertex, *Vertex.scale(1.2, Vertex.make_many(
                         [  0,  8,  0],  # 0
                         [-40, -6, 30],  # 1
                         [-15,- 8, 30],  # 2
                         [ 15,- 8, 30],  # 3
                         [ 40, -6, 30],  # 4
                         [ 25,  7, 30],  # 5
                         [  0,  8, 30],  # 6
                         [-25,  7, 30],  # 7
                         [-40, -6,  5],  # 8
                         [ 40, -6,  5],  # 9
                         [-15,- 8,-30],  #10
                         [ 15,- 8,-30],  #11
                         [  0,- 8,-30],  #12
                         [  0,- 8,-40],  #13
                         [-30,  0, 30],  #14
                         [-26,  1, 30],  #15
                         [-26, -1, 30],  #16
                         [ 30,  0, 30],  #17
                         [ 26,  1, 30],  #18
                         [ 26, -1, 30],  #19
                         [-20,  4, 30],  #20
                         [ -5,  5, 30],  #21
                         [ -5, -5, 30],  #22
                         [-20, -4, 30],  #23
                         [ 20,  4, 30],  #24
                         [  5,  5, 30],  #25
                         [  5, -5, 30],  #26
                         [ 20, -4, 30])  #27
                      )
                      db   -128
                      draw_poly_wire vs[1], vs[8], vs[10], vs[2], closed:true
                      draw_poly_wire vs[9], vs[4], vs[3], vs[11], closed:true
                      draw_wire vs[2], vs[3]
                      draw_poly_wire vs[0], vs[5], vs[6], vs[7], closed:true
                      draw_wire vs[0], vs[6]
                      draw_poly_wire vs[0], vs[10], vs[11], closed:true
                      draw_poly_wire vs[8], vs[7], vs[1], closed:false
                      draw_poly_wire vs[8], vs[7], vs[1], closed:false
                      draw_poly_wire vs[4], vs[5], vs[9], closed:false
                      draw_poly_wire vs[7], vs[10], vs[0], vs[11], vs[5], closed:false
                      draw_wire vs[12], vs[13]
                      draw_poly_wire vs[14], vs[15], vs[16], closed:true
                      draw_poly_wire vs[17], vs[18], vs[19], closed:true
                      draw_poly_wire vs[20], vs[21], vs[22], vs[23], closed:true
                      draw_poly_wire vs[24], vs[25], vs[26], vs[27], closed:true
                      ret
  end

  ns :cubes do
    vs                data Vertex, *Vertex.make_many(
                        [ 35, 35, 35],
                        [ 35,-35, 35],
                        [-35,-35, 35],
                        [-35, 35, 35],
                        [ 35, 35,-35],
                        [ 35,-35,-35],
                        [-35,-35,-35],
                        [-35, 35,-35],
                        [ 25, 25, 25],
                        [ 25,-25, 25],
                        [-25,-25, 25],
                        [-25, 25, 25],
                        [ 25, 25,-25],
                        [ 25,-25,-25],
                        [-25,-25,-25],
                        [-25, 25,-25])
                      db   -128
    [0,8].each do |offs|
                      draw_poly_wire vs[0+offs], vs[1+offs], vs[2+offs], vs[3+offs]
                      draw_poly_wire vs[4+offs], vs[5+offs], vs[6+offs], vs[7+offs]
                      draw_wire vs[0+offs], vs[4+offs]
                      draw_wire vs[1+offs], vs[5+offs]
                      draw_wire vs[2+offs], vs[6+offs]
                      draw_wire vs[3+offs], vs[7+offs]
    end
    (0..7).each do |offs|
                      draw_wire vs[offs], vs[offs+8]
    end
                      ret
  end

  ## Pre-calculated matrixes.
  matrix              data Matrix, *(0...180).map {|angle|
                        rad = angle*Math::PI/90.0
                        quaternion2matrix(
                          quaternion_cross_product(
                            quaternion_cross_product(
                              rotor_quaternion(rad*2.0, [0.0, 1.0, 0.0]),
                              rotor_quaternion(rad, [1.0, 0.0, 0.0])
                            ),
                            rotor_quaternion(rad*3.0, [0.0, 0.0, 1.0]),
                          )
                        ).map(&:to_fixed)
                      }
  matrix_end  label
end

include ZXLib

quat3d = Quat3D.new 0x9000
puts quat3d.debug
program = Basic.parse_source <<-END
  10 RANDOMIZE USR #{quat3d[:start]}
9998 GO TO 10000
9999 CLEAR #{quat3d.org-1}: LOAD ""CODE: RUN
END
puts program.to_source escape_keywords:true

tap_name = 'examples/quat3d128.tap'
program.save_tap tap_name, line:9999
quat3d.save_tap tap_name, append:true
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
