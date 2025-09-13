require 'z80'
require 'z80/stdlib'
require 'zxlib/gfx/draw'
require 'zxlib/basic'
require 'z80lib3d/fixed_float'
require 'z80lib3d/primitives'
require 'z80lib3d/matrix'
require 'z80lib3d/quaternion'

class Quat3D
  extend Z80Lib3D::Quaternion
  include Z80
  include Z80::TAP
  include Z80Lib3D::Primitives

  ## Color attributes
  BG_ATTR = 0b01100000

  ## Perspective scale factor: [0-7]
  PERSP_DSHIFT = 7

  ###########
  # Imports #
  ###########

  macro_import    Stdlib
  macro_import    Z80Lib3D::Matrix3D
  macro_import    ZXLib::Gfx::Draw
  import          ZXLib::Sys, macros: true

  ##########
  # Macros #
  ##########

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

  macro :print_text do |eoc, text|
                        ld   de, text_data
                        ld   bc, +text_data
                        call rom.pr_string
                        jr   eoc
    text_data           data ZXLib::Basic::Vars.program_text_to_string text
  end

  ########
  # Main #
  ########

  with_saved :start, :exx, hl, ret: true do |eoc|
    release_key         halt
                        key_pressed?
                        jr   NZ, release_key # wait for any key being pressed to be released
                        ld   a, 2
                        call rom.chan_open
                        # prepare both shadow and regular screen memory, disabling interrupts first
                        mmu128_select_bank(bank:7, screen:0, disable_intr:true, enable_intr:false)
                        ld   a, BG_ATTR
                        call clear_screen
                        mmu128_swap_screens(swap_bank:true, disable_intr:false, enable_intr:false)
                        ld   a, BG_ATTR
                        call clear_screen
                        print_text "`AT 0,3``INK 8``PAPER 8``BRIGHT 8`Press 0-9 to change colors"
                        mmu128_swap_screens(swap_bank:true, disable_intr:false, enable_intr:false)
                        copy_shadow_screen_region(xy_to_pixel_addr(0,0,scraddr:0xC000), 8, 32, tgtaddr:0xC000, srcaddr:0x4000, check_edge:false, break_oos:false)

                        # reset adjustment of z
                        ld   a, 128 + 58
                        ld   [draw_loop.apply_matrix_a.adjust_z_p], a
                        # save sp
                        ld   [draw_loop.restore_sp_p], sp
                        dc!
                        dc!"*********************************************"
                        dc!"***               DRAW LOOP               ***"
                        dc!"*********************************************"
    ns :draw_loop do
                        # clear shadow screen pixel area where the new frame will be rendered
                        clear_screen_region_fast(xy_to_pixel_addr(40+22*8, 10, scraddr:0xC000), 180, 22, disable_intr:false, enable_intr:false, save_sp:false)
      object_a          ld   hl, cubes
      object_p          as object_a + 1 # a pointer to a current object's address
                        dc!
                        dc!"*********************************************"
                        dc!"***             APPLY  MATRIX             ***"
                        dc!"*********************************************"
                        # apply a current matrix to a current object's vertices and calculate screen coordinates
      apply_matrix_a    apply_matrix matrix, scrx0:128, scry0:96, scrz0:128, persp_dshift:PERSP_DSHIFT, optimize: :unroll_alt
                        dc!
                        dc!"*********************************************"
                        dc!"***              DRAW  WIRES              ***"
                        dc!"*********************************************"
                        # both apply_matrix and clear_screen_region_fast uses stack pointer so let's restore it
      restore_sp_a      ld   sp, 0
      restore_sp_p      as restore_sp_a + 1
                        # draw current object to the shadow screen's memory
                        call rom.call_jump # effectively call (hl)

                        dc!
                        dc!"*********************************************"
                        dc!"***              NEXT MATRIX              ***"
                        dc!"*********************************************"
                        # set next matrix address
                        ld   hl, [apply_matrix_a.matrix_p]
                        ld   bc, +matrix
                        add  hl, bc
                        cp16n h, l, matrix_end, jr_msb_c:skip_matrix_reset, jr_msb_nz:matrix_reset
                        jr   C, skip_matrix_reset
      matrix_reset      ld   hl, matrix
      skip_matrix_reset ld   [apply_matrix_a.matrix_p], hl
                        dc!
                        dc!"*********************************************"
                        dc!"***             SWAP  SCREENS             ***"
                        dc!"*********************************************"
                        # display shadow screen and swap bank 7 to the previous (now shadow) screen
                        mmu128_swap_screens(swap_bank:true, disable_intr:false, enable_intr:false)

                        dc!
                        dc!"*********************************************"
                        dc!"***                ADJUST  Z              ***"
                        dc!"*********************************************"
                        # modify adjust_z value until object is near
                        ld   hl, apply_matrix_a.adjust_z_p
                        ld   a, [hl]
                        cp   128 - 8
                        jr   Z, skip_adjust_z
                        dec  [hl]

                        dc!
                        dc!"*********************************************"
                        dc!"***                KEY CHECK              ***"
                        dc!"*********************************************"
                        # loop unless key is pressed
      skip_adjust_z     key_pressed?
                        jp   Z, draw_loop
                        # wait until key is released or a break key was pressed
      release_key2      ei
                        halt
                        di
                        call rom.break_key
                        jp   NC, quit
                        key_pressed?
                        jr   NZ, release_key2
                        ei
                        halt
                        di
                        dc!
                        dc!"*********************************************"
                        dc!"***            CHECK COLOR KEYS           ***"
                        dc!"*********************************************"
      # check color keys
      ns do |eoc|
                        ld   a, [vars.last_k]
                        sub  ?0.ord
                        jr   Z, sel_black
                        cp   9
                        jr   Z, sel_nink
                        cp   8
                        jr   Z, reset_ink
                        jr   NC, eoc # check other keys
                        3.times { rlca }
        sel_ink         ld   c, a
                        cp   0b00100000
                        sbc  a, a
                        anda 7
                        ora  c
                        jr   paint_store
        reset_ink       ld   a, [last_color]
                        anda 0b00111000
                        jr   sel_ink
        sel_black       ld   a, [last_color]
                        ld   c, a
                        3.times { rrca }
                        anda 7
                        jr   NZ, paint_store
                        ld   a, 7
                        jr   paint_store
        sel_nink        ld   a, [last_color]
                        anda 0b00111111
                        ld   c, a
                        3.times { rrca }
                        xor  c
                        anda 7
                        xor  c
                        ld   b, a
                        ld   a, c
        sel_next        inc  a
                        xor  c
                        anda 7
                        xor  c
                        cp   b
                        jr   Z, sel_next
        paint_store     ld   [last_color], a
        paint           push af
                        call clear_screen
                        mmu128_swap_screens(swap_bank:true, disable_intr:false, enable_intr:false)
                        pop  af
                        call clear_screen
                        jp   draw_loop
      end

                        dc!
                        dc!"*********************************************"
                        dc!"***               NEXT OBJECT             ***"
                        dc!"*********************************************"
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
                        dc!"*********************************************"
    last_color          db   BG_ATTR
                        dc!
                        dc!"*********************************************"
                        dc!"***                  QUIT                 ***"
                        dc!"*********************************************"
                        # reset bank and screen and enable interrupts
    quit                mmu128_select_bank(bank:0, screen:0, disable_intr:false, enable_intr:true)
                        call rom.cl_all
                        ld   a, [vars.bordcr]
                        call set_border_cr
  end

  ###############
  # Subroutines #
  ###############

                        dc!
                        dc!"*********************************************"
                        dc!"***              CLEAR SCREEN             ***"
                        dc!"*********************************************"
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
                      # Clears shadow ink/paper memory.
                      clrmem  mem128.screen_alt, mem.scrlen, 0
  end

  # < a: border color in paper bits
  ns :set_border_cr do
                      anda 0b00111000
                      3.times { rrca }
                      out  (io.ula), a
                      ret
  end

                        dc!
                        dc!"*********************************************"
                        dc!"***               DRAW  LINE              ***"
                        dc!"*********************************************"
  ## Line drawing routines.
  draw                make_draw_line_subroutines(make_line:true, make_line_over:false, make_line_inversed:false,
                                                 scraddr:0xC000, check_oos:false)

  #############
  # 3D Models #
  #############

                        dc!
                        dc!"*********************************************"
                        dc!"***                 MODELS                ***"
                        dc!"*********************************************"
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
                        [-10,  10,-50],
                        [ 10,  10,-50],
                        [ 10, -10,-50],
                        [-10, -10,-50],
                        [  0,  20,-50])
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
                        [-10,-20, 10],  # 1
                        [ 10,-20, 10],  # 2
                        [-20,  0,-10],  # 3
                        [ 20,  0,-10],  # 4
                        [-20,-20, 40],  # 5
                        [ 20,-20, 40],  # 6
                        [-30,  0, 40],  # 7
                        [ 30,  0, 40],  # 8
                        [-40, 30,-40],  # 9
                        [ 40, 30,-40],  #10
                        [-40, 29,-10],  #11
                        [ 40, 29,-10],  #12
                        [-40, 30,  0],  #13
                        [ 40, 30,  0])  #14
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
                         [  0, -8,  0],  # 0
                         [-40,  6, 30],  # 1
                         [-15,  8, 30],  # 2
                         [ 15,  8, 30],  # 3
                         [ 40,  6, 30],  # 4
                         [ 25, -7, 30],  # 5
                         [  0, -8, 30],  # 6
                         [-25, -7, 30],  # 7
                         [-40,  6,  5],  # 8
                         [ 40,  6,  5],  # 9
                         [-15,  8,-30],  #10
                         [ 15,  8,-30],  #11
                         [  0,  8,-30],  #12
                         [  0,  8,-40],  #13
                         [-30,  0, 30],  #14
                         [-26, -1, 30],  #15
                         [-26,  1, 30],  #16
                         [ 30,  0, 30],  #17
                         [ 26, -1, 30],  #18
                         [ 26,  1, 30],  #19
                         [-20, -4, 30],  #20
                         [ -5, -5, 30],  #21
                         [ -5,  5, 30],  #22
                         [-20,  4, 30],  #23
                         [ 20, -4, 30],  #24
                         [  5, -5, 30],  #25
                         [  5,  5, 30],  #26
                         [ 20,  4, 30])  #27
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
                        [ 35,-35, 35],
                        [ 35, 35, 35],
                        [-35, 35, 35],
                        [-35,-35, 35],
                        [ 35,-35,-35],
                        [ 35, 35,-35],
                        [-35, 35,-35],
                        [-35,-35,-35],
                        [ 25,-25, 25],
                        [ 25, 25, 25],
                        [-25, 25, 25],
                        [-25,-25, 25],
                        [ 25,-25,-25],
                        [ 25, 25,-25],
                        [-25, 25,-25],
                        [-25,-25,-25])
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

                        dc!
                        dc!"*********************************************"
                        dc!"***                 MATRIX                ***"
                        dc!"*********************************************"
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
                        ).map(&:to_fixed16_8)
                      }
  matrix_end  label
end

include ZXLib

quat3d = Quat3D.new 0x9000
puts quat3d.debug
puts "="*60
%w[start start.draw_loop.apply_matrix_a draw
   cubes spaceship1 spaceship2 elite].each do |label|
  puts "#{label.ljust(30)}: 0x#{quat3d[label].to_s(16).upcase} - #{quat3d[label]}, size: #{quat3d['+'+label]}"
end
puts "="*60

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
