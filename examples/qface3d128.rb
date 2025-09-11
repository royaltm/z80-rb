require 'set'
require 'z80'
require 'z80/stdlib'
require 'z80/utils/sincos'
require 'zxlib/gfx/draw'
require 'zxlib/basic'
require 'z80lib3d/fixed_float'
require 'z80lib3d/primitives'
require 'z80lib3d/matrix'
require 'z80lib3d/quaternion'

## Tools for Vector3D
module Vec3
  ## Return a vector between point a and b
  def vec3from(a, b)
    ax, ay, az = a
    bx, by, bz = b
    [bx - ax, by - ay, bz - az]
  end

  ## Returns a vector square norm.
  def vec3norm_q(v)
    x, y, z = v
    x*x + y*y + z*z
  end

  ## Returns a vector norm.
  def vec3norm(v)
    Math::sqrt(vec3norm_q(v))
  end

  ## Returns a normalized vector.
  def vec3normalize(v)
    n = vec3norm(v)
    x, y, z = v
    [x/n, y/n, z/n]
  end

  def vec3dot(a, b)
    ax, ay, az = a
    bx, by, bz = b
    ax * bx + ay * by + az * bz
  end

  def vec3cross(a, b)
    ax, ay, az = a
    bx, by, bz = b
    [ay * bz - az * by, az * bx - ax * bz, ax * by - ay * bx]
  end
end

## A helper struct (unused in this demo)
Vector3D = ::Struct.new :x, :y, :z do
  include Vec3
  extend Vec3
  def self.from_vertices(a, b)
    new *vec3from(a.to_a, b.to_a)
  end

  def normalize
    self.class.new *vec3normalize(to_a)
  end

  def dot(other)
    vec3dot(to_a, other.to_a)
  end

  def cross(other)
    self.class.new *vec3cross(to_a, other.to_a)
  end
end

##
# A helper struct used by Object3D
#
# * +indices+:: an array of 0-based vertex indices (at least 3),
# * +surface+:: an optional array tuple of 3 indices that is used to calculate face direction,
# * +decoration+:: an optional surface decoration data address or a Z80 program label.
#
# If the +surface+ is +nil+ the face direction is calculated from the first 3 indices.
Face3D = ::Struct.new :indices, :surface, :decoration do
  ##
  # Creates an Face3D object with given arguments.
  #
  # +vertices+ should be an array of [x, y, z] triplets and is only provided here for validation of the +indices+.
  def self.make(vertices, indices, surface:nil, decoration:nil)
    raise ArgumentError, "at least 3 indices expected" if !indices.is_a?(Array) or indices.length < 3
    raise ArgumentError, "expected array of vertices" if !vertices.is_a?(Array) or vertices.any?{|v| !v.respond_to?(:to_a)}
    raise ArgumentError, "duplicated index" if ::Set.new(indices).length != indices.length
    raise ArgumentError, "vertex not found" if indices.any?{|i| !i.is_a?(Integer) || i >= vertices.length || i < 0 }
    raise ArgumentError, "expected array of 3 indices in surface" unless surface.nil? or (surface.is_a?(Array) and
        surface.length == 3 and surface.all?{|i| i.is_a?(Integer) && i >= 0 && i < vertices.length })
    # a, b, c = indices[0...3].map{|i| vertices[i].to_a }
    # va = Vector3D.from_vertices(a, b)
    # vb = Vector3D.from_vertices(b, c)
    # nor = va.cross(vb).normalize
    new indices, surface, decoration
  end
end

##
# A helper struct for creating 3D objects data for Z80 programs.
#
# * +vertices+:: an array of [x, y, z] triplets (points in 3D space),
# * +edges+:: a map of {[A, B] -> edge index} used for edges deduplication, where A and B
#             are vertex indices,
# * +faces+:: an array of Face3D objects.
Object3D = ::Struct.new :vertices, :edges, :faces do
  ##
  # Creates an initial Object3D containing only vertices without any faces or edges.
  #
  # * +vertices+:: [x, y, z] triplets (points in 3D space).
  #
  # If a code block is given the block is called with an argument containing an initialized
  # object and can be used to populate it with faces.
  #
  # The center coordinate of the object is assumed to be [0, 0, 0].
  def self.make(*vertices)
    obj = new vertices, Hash.new, []
    yield obj if block_given?
    obj
  end
  ##
  # Scale all vertices in place by the given factor.
  def scale!(sc)
    vertices.map!{|x,y,z| [x*sc, y*sc, z*sc]}
  end
  ##
  # Creates and adds a new face to this object.
  # 
  # * +indices+:: at least 3 0-based vertex indices,
  # * +surface+:: an optional array tuple of 3 indices that is used to calculate face direction,
  # * +decoration+:: an optional surface decoration data address or a Z80 program label.
  #
  # If the +surface+ is +nil+ the face direction is calculated from the first 3 indices.
  def face!(*indices, surface:nil, decoration:nil)
    faces.push Face3D.make(vertices, indices, surface:surface, decoration:decoration)
    (indices + [indices.first]).each_cons(2) do |a,b|
      unless edges.has_key?([a,b]) || edges.has_key?([b,a])
        edges[[a,b]] = edges.length
      end
    end
  end
  ##
  # Creates a namespace containing Z80 program data from this object and returns its label.
  #
  # +prog+:: should be a Z80 program class to add a namespace to,
  # +name+:: should be a label name as a symbol.
  #
  # The object must contain some faces, otherwise an error is raised.
  #
  # <b>NOTE</b>: Currently this function uses +edge+ and +face+ macros to create face and edge data.
  def to_prog(prog, name)
    raise ArgumentError, "no edges!" if edges.empty?
    eind = self.edges.invert
    prog.ns name do
      vs = prog.define_label :vs, prog.data(prog::Vertex, *prog::Vertex.make_many(*vertices))
      prog.db -128 # terminator
      faces.each do |f|
        p1, p2, p3 = f.surface || f.indices[0...3]
        edge_labels = []
        (f.indices + [f.indices.first]).each_cons(2) do |a,b|
          i = edges[[a,b]] || edges[[b,a]]
          raise ArgumentError, "no such edge: #{a} - #{b}" unless i
          edge_labels.push(prog.define_label "edge#{i}")
        end
        prog.face vs[p1].scr, vs[p2].scr, vs[p3].scr, *edge_labels, decoration:f.decoration
      end
      prog.dw 0 # terminator
      eind.each do |i, (a,b)|
        prog.define_label "edge#{i}", prog.edge(vs[a], vs[b])
      end
    end
  end
end

## The code
class QFace3D
  extend Z80Lib3D::Quaternion
  include Z80
  include Z80::TAP
  include Z80Lib3D::Primitives

  OPTIMIZE = :unroll # :size :time :unroll
  MATRIX_OPTIMIZE = :unroll_alt # :size :time :time_alt :unroll :unroll_alt

  ## Initial color attributes
  BG_ATTR = 0b01100000

  ## Perspective scale factor: [0-7]
  PERSP_DSHIFT = 7

  ###########
  # Imports #
  ###########

  macro_import    Stdlib
  macro_import    Utils::SinCos
  macro_import    Z80Lib3D::Matrix3D
  macro_import    ZXLib::Gfx::Draw
  import          ZXLib::Sys, macros: true

  ##########
  # Labels #
  ##########

  sincos          addr 0xBC00, Utils::SinCos::SinCos

  ##########
  # Macros #
  ##########

  # Macro used by Object3D.to_prog
  #
  # Crates a face entry struct
  # < s1, s2, s3: addresses of scr coordinates of 3 face points
  # < edges: a list of edge address labels
  macro :face do |_, s1, s2, s3, *edges, decoration:nil|
    decoration = 0 unless decoration
    raise ArgumentError, "no edges!" if edges.empty?
    points            dw   s1, s2, s3
    count             db   edges.length
    edges.each do |edge|
                      dw   edge
    end
                      dw   decoration
  end

  # Macro used by Object3D.to_prog
  #
  # Creates the edge drawing routine with a frame counter variable header
  # < iy: drawing routine address
  # < v1: Vertex address label
  # < v2: Vertex address label
  macro :edge do |_, v1, v2, frame_counter:0|
      frame           db   frame_counter    # (1) frame
      run             ld   de, [v1.scr]     # (4) 20
                      ld   hl, [v2.scr]     # (3) 16
                      jp   (iy)             # (2) 8
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

  macro :print_text do |eoc, text|
                        ld   de, text_data
                        ld   bc, +text_data
                        call rom.pr_string
                        jr   eoc
    text_data           data ZXLib::Basic::Vars.program_text_to_string text
  end

  ##
  # Draws faces, first determining if the face visible side is towards the viewer.
  #
  # If the face front is towards the viewer (z < 0) draws that face.
  #
  # Each edge is drawn only once.
  #
  # The +frame+ value, accessible at +frame_p+ sub-label is used to check and mark
  # edges, determining if the edge was already drawn or not.
  #
  # The +frame+ value should be increased before each new frame is drawn.
  #
  # < faces: an address of a first face or hl
  # < draw_line: an address of a draw line_to routine or iy
  # > hl: an address of the last byte of the end marker
  #
  # stack depth: 8 bytes
  macro :draw_faces do |_, faces:hl, draw_line: draw.line_to|
                      ld   iy, draw_line unless draw_line == iy
                      ld   hl, faces unless faces == hl
    # First we take 3 face screen coordinates, calculate 2 vectors from them
    # and calculate the Z value of a cross-product of those vectors.
    #
    # If the Z value is < 0 the face edge vectors are directed clock-wise
    # and we should draw it.
    #
    # The screen ys axis is reversed (growing downwards), given p0, p1, p2:
    #
    # v1 = p1 - p0 = [xp1 - xp0, -(yp1 - yp0)] = [dx1, dy1]
    # v2 = p2 - p1 = [xp2 - xp1, -(yp2 - yp1)] = [dx2, dy2]
    # z = xv1 * -yv2 - (-yv1 * xv2) # reversed y
    # z = yv1 * xv2 - xv1 * yv2
    # z = (xp1 - xp0) * -(yp2 - yp1) - (-(yp1 - yp0) * (xp2 - xp1))
    # z = (yp1 - yp0) * (xp2 - xp1)  - (xp1 - xp0) * (yp2 - yp1)
    # z = dy1         * dx2          - dx1         * dy2
                      ld   e, [hl]                # hl: -> *points[0]
                      inc  hl                     # 
                      ld   d, [hl]                # de: -> points[0]
    face_loop         inc  hl                     # hl: -> *points[1]
                      ld   c, [hl]                # hl: -> *points[1]
                      inc  hl                     # 
                      ld   b, [hl]                # bc: -> points[1]
                      inc  hl                     # hl: -> *points[2]

                      ex   de, hl                 # hl: -> xp0
                      ld   a, [bc]                # a: xp1
                      sub  [hl]                   # af: xp1 - xp0
                      ex   af, af                 # af': xp1 - xp0 = dx1
                      inc  bc                     # bc: -> yp1
                      inc  hl                     # hl: -> yp0
                      ld   a, [bc]                # a: yp1
                      sub  [hl]                   # af: yp1 - yp0 = dy1
                      ex   de, hl                 # hl: -> *points[2]
                      dec  bc                     # bc: -> xp1
                      ld16 de, bc                 # de: -> xp1

                      exx
                      ld   d, a
                      sbc  a, a                   # a: sign of dy1
                      ld   c, a                   # c|d: dy1

                      exx
                      ld   c, [hl]                # hl: -> *points[2]
                      inc  hl                     # 
                      ld   b, [hl]                # bc: -> points[2]
                      inc  hl                     # hl: -> count

                      ex   de, hl                 # hl: -> xp1
                      ld   a, [bc]                # a: xp2
                      sub  [hl]                   # af: xp2 - xp1 = dx2

                      exx
                      dc!
                      dc! "*** (dy1 * dx2) ***"
                      # dy1 * dx2 (c|d: dy1 * C|a: dx2)
                      # c|hl: -> (dy1 * dx2)
                      mul_signed9 c, d, a, tt:de, m_is_zero_zf:true, k_full_range:false, m_full_range:false, optimize:OPTIMIZE
                      dc! "*******************"
                      push hl                     # c|hl: -> (dy1 * dx2)

                      exx
                      inc  bc                     # bc: -> yp2
                      inc  hl                     # hl: -> yp1
                      ld   a, [bc]                # a: yp2
                      sub  [hl]                   # af: yp2 - yp1 = dy2

                      exx
                      ld   d, a
                      sbc  a, a                   # a: sign of dy2
                      ld   b, a                   # b|d: dy2
                      ex   af, af                 # af: dx1
                      dc!
                      dc! "*** (dy2 * dx1) ***"
                      # dy2 * dx1 (b|d: dy2 * C|a: dx1)
                      # b|hl: -> (dy2 * dx1)
                      mul_signed9 b, d, a, tt:de, m_is_zero_zf:true, k_full_range:false, m_full_range:false, optimize:OPTIMIZE
                      dc! "*******************"
                      ex   de, hl                 # b|de: -> (dy2 * dx1)
                      pop  hl                     # c|hl: -> (dy1 * dx2)

                      dc!
                      dc! "*** (dy1 * dx2) - (dx1 * dy2) ***"
                      # dy1 * dx2 - dx1 * dy2
                      anda a                      # CF: 0
                      sbc  hl, de                 # z = (dy1 * dx2) - (dy2 * dx1)
                      ld   a, c
                      sbc  a, b                   # af|hl = (dy1 * dx2) - (dy2 * dx1)

                      #
                      # SF: 1 - z < 0, 0: z >= 0
                      #

                      exx
                      ex   de, hl                 # hl: -> *count

                      dc!
                      dc! "*** DRAW FACE ***"

                      ld   b, [hl]                # edge count (assert != 0)
                      inc  hl                     # hl: -> *edgeN
                      jp   M, draw_face           # z < 0 (face to camera)

                      dc!
                      dc! "*** SKIP FACE ***"

                      inc  b                      # b: count + 1 (to skip decoration)
                      ld   a, b                   # a: count + 1
                      add  a, b                   # a: (count + 1) * 2 (dw size)
                      adda_to h, l                # hl: -> next face or 0
                      jp   next_face

                      dc!
                      dc! "*** DRAW EDGES ***"

    draw_face         ld   a, 0                   # a: frame
    frame_p           as draw_face + 1

    skip_loop         ld   e, [hl]                # hl: -> *edgeN
                      inc  hl
                      ld   d, [hl]                # de: -> edgeN.frame
                      inc  hl                     # hl: -> *edgeN+1
                      ex   de, hl                 # de: -> *edgeN+1, hl -> edgeN.frame
                      cp   [hl]                   # was this edge drawn?
                      jr   Z, skip_edge

                      dc!
                      dc! "*** DRAW EDGE ***"

                      ld   [hl], a                # update edgeN.frame
                      inc  hl                     # hl: -> edgeN.run
                      push bc                     # save count
                      push de                     # save edge pointer
                      call rom.call_jump          # effectively call (hl)
                      pop  hl                     # restore edge pointer
                      pop  bc                     # restore count
                      djnz draw_face
                      jp   check_decoration

    skip_edge         ex   de, hl                 # hl: -> *edgeN+1
                      djnz skip_loop

                      dc!
                      dc! "*** DECORATION ***"

    check_decoration  ld   e, [hl]                # hl: -> *decoration object ptr
                      inc  hl
                      ld   d, [hl]
                      inc  hl                     # hl: -> next face
                      ld   a, e                   # de: -> decoration object
                      ora  d
                      jr   Z, next_face           # no decoration
                      push hl                     # save face pointer
                      ex   de, hl                 # hl: -> decoration vertices
                      call apply_matrix_to_vertices
                                                  # hl: -> decoration draw edges routine
                      call rom.call_jump          # effectively call (hl)
                      pop  hl                     # restore face pointer

    next_face         ld   e, [hl]                # hl: -> *points[N]
                      inc  hl                     # 
                      ld   d, [hl]                # d|e: -> points[N]
                      ld   a, e
                      ora  d
                      jp   NZ, face_loop          # checked faces list terminating 0
  end

  ########
  # Main #
  ########

  with_saved :start, :exx, hl, ret: true do |eoc|
                        call  make_sincos
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
                        print_text "`AT 0,0``INK 8``PAPER 8``BRIGHT 8`0-9:colors OPQAZX:rot SPACE:stop"
                        mmu128_swap_screens(swap_bank:true, disable_intr:false, enable_intr:false)
                        copy_shadow_screen_region(xy_to_pixel_addr(0,0,scraddr:0xC000), 8, 32, tgtaddr:0xC000, srcaddr:0x4000, check_edge:false, break_oos:false)

                        # reset adjustment of z
                        ld   a, 128 + 58
                        ld   [apply_matrix_to_vertices.apply_matrix_a.adjust_z_p], a
                        ld   a, 128 - 66
                        ld   [apply_matrix_to_vertices.apply_matrix_a.adjust_x_p], a
                        ld   a, 96 - 33
                        ld   [apply_matrix_to_vertices.apply_matrix_a.adjust_y_p], a
                        # save sp
                        ld   [draw_loop.restore_sp_p], sp
                        dc!
                        dc!"*********************************************"
                        dc!"***               DRAW LOOP               ***"
                        dc!"*********************************************"
    ns :draw_loop do
                        # increase frame counter, this is used to indicate whether an edge was drawn for this frame
                        ld   hl, draw_faces_a.frame_p
                        inc  [hl]
                        dc!
                        dc!"*********************************************"
                        dc!"***             CLEAR SHADOW              ***"
                        dc!"*********************************************"
                        # clear shadow screen pixel area where the new frame will be rendered
                        clear_screen_region_fast(xy_to_pixel_addr(16, 8, scraddr:0xC000), 182, 28, addr_mode: :first, disable_intr:false, enable_intr:false, save_sp:false)
                        # clear_screen_region_fast uses stack pointer so let's restore it
      restore_sp        ld   sp, 0
      restore_sp_p      as restore_sp + 1
                        dc!
                        dc!"*********************************************"
                        dc!"***             APPLY MATRIX              ***"
                        dc!"*********************************************"

      object_a          ld   hl, cube
      object_p          as object_a + 1 # a pointer to a current object's address
                        # apply a current matrix to a current object's vertices and calculate screen coordinates
                        call apply_matrix_to_vertices
                        dc!
                        dc!"*********************************************"
                        dc!"***              DRAW  FACES              ***"
                        dc!"*********************************************"
      draw_faces_a      draw_faces
                        dc!
                        dc!"*********************************************"
                        dc!"***             SWAP  SCREENS             ***"
                        dc!"*********************************************"
                        # display shadow screen and swap bank 7 to the previous (now shadow) screen
                        mmu128_swap_screens(swap_bank:true, disable_intr:false, enable_intr:false)

                        dc!
                        dc!"*********************************************"
                        dc!"***             ROTATE OBJECT             ***"
                        dc!"*********************************************"

                        ld   hl, roll
      roll_a            ld   a, 2
      roll_speed        roll_a + 1
                        add  a, [hl]
                        ld   [hl], a
                        ld   c, a     # c: roll

                        ld   hl, pitch
      pitch_a           ld   a, 3
      pitch_speed       pitch_a + 1
                        add  a, [hl]
                        ld   [hl], a
                        ld   b, a     # b: pitch

                        ld   hl, yaw
      yaw_a             ld   a, 1
      yaw_speed         yaw_a + 1
                        add  a, [hl]
                        ld   [hl], a  # a: yaw

                        call rotate

                        dc!
                        dc!"*********************************************"
                        dc!"***            ADJUST POSITION            ***"
                        dc!"*********************************************"
                        # modify adjust position until object is near
                        ld   hl, apply_matrix_to_vertices.apply_matrix_a.adjust_z_p
                        ld   a, [hl]
                        cp   128 - 8
                        jr   Z, skip_adjust_z
                        dec  [hl]
      skip_adjust_z     ld   hl, apply_matrix_to_vertices.apply_matrix_a.adjust_x_p
                        ld   a, [hl]
                        cp   128
                        jr   Z, skip_adjust_x
                        inc  [hl]
      skip_adjust_x     ld   hl, apply_matrix_to_vertices.apply_matrix_a.adjust_y_p
                        ld   a, [hl]
                        cp   96
                        jr   Z, skip_adjust_y
                        inc  [hl]

                        dc!
                        dc!"*********************************************"
                        dc!"***                KEY CHECK              ***"
                        dc!"*********************************************"
                        # loop unless key is pressed
      skip_adjust_y     key_pressed?
                        jp   Z, draw_loop
                        # restore iy
                        ld   iy, vars_iy
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
      # handle keyboard
      ns do |eoc|
                        ld   a, [vars.last_k]
                        sub  ?0.ord
                        jr   Z, sel_blk
                        cp   9
                        jr   Z, sel_nink
                        cp   8
                        jr   Z, sel_ink0
                        jr   NC, eoc # check other keys
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
                        dc!
                        dc!"*********************************************"
                        dc!"***           CHECK OTHER KEYS            ***"
                        dc!"*********************************************"
      ns do |eoc|
                        cp   'o'.ord - ?0.ord
                        jr   NZ, key_1
                        ld   hl, pitch_speed
                        dec  [hl]
                        jp   draw_loop
        key_1           cp   'p'.ord - ?0.ord
                        jr   NZ, key_2
                        ld   hl, pitch_speed
                        inc  [hl]
                        jp   draw_loop
        key_2           cp   'q'.ord - ?0.ord
                        jr   NZ, key_3
                        ld   hl, roll_speed
                        inc  [hl]
                        jp   draw_loop
        key_3           cp   'a'.ord - ?0.ord
                        jr   NZ, key_4
                        ld   hl, roll_speed
                        dec  [hl]
                        jp   draw_loop
        key_4           cp   'z'.ord - ?0.ord
                        jr   NZ, key_5
                        ld   hl, yaw_speed
                        inc  [hl]
                        jp   draw_loop
        key_5           cp   'x'.ord - ?0.ord
                        jr   NZ, key_6
                        ld   hl, yaw_speed
                        dec  [hl]
                        jp   draw_loop
        key_6           cp   ' '.ord - ?0.ord
                        jr   NZ, eoc # next object
                        xor  a
                        ld   [pitch_speed], a
                        ld   [roll_speed], a
                        ld   [yaw_speed], a
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

  last_color            db   BG_ATTR

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

                      dc!
                      dc!"*********************************************"
                      dc!"***             SINCOS TABLE              ***"
                      dc!"*********************************************"
  make_sincos         create_sincos_from_sintable sincos, sintable:sintable
  sintable            bytes   neg_sintable256_pi_half_no_zero_lo
  sintable_end        label

                      dc!
                      dc!"*********************************************"
                      dc!"***                ROTATE                 ***"
                      dc!"*********************************************"
  ns :rotate do # yaw=a, pitch=b, roll=c
                      course_to_rotation(rotation, a, b, c, sincos:sincos, save_sp:true)
                      rotation_to_matrix(matrix, rotation, inline_mul:OPTIMIZE != :size, subroutine:true, optimize:OPTIMIZE)
  end

                      dc!
                      dc!"*********************************************"
                      dc!"***             APPLY  MATRIX             ***"
                      dc!"*********************************************"
  # < hl: -> vertices
  # > hl: -> vertices terminator + 1
  ns :apply_matrix_to_vertices do
                      ld   [restore_sp_p], sp
    apply_matrix_a    apply_matrix matrix, scrx0:128, scry0:96, scrz0:128, persp_dshift:PERSP_DSHIFT, optimize:MATRIX_OPTIMIZE
    restore_sp_a      ld   sp, 0
    restore_sp_p      as restore_sp_a + 1
                      ret
  end

  #############
  # 3D Models #
  #############
                      dc!
                      dc!"*********************************************"
                      dc!"***                 MODELS                ***"
                      dc!"*********************************************"

  ## 3D object pointers
  objects             dw cube,
                         elite,
                         0

                      dc!
                      dc!"*********************************************"
                      dc!"***                CUBE 3D                ***"
                      dc!"*********************************************"

  Object3D.make([ 35,-35, 35],  #0
                [ 35, 35, 35],  #1
                [-35, 35, 35],  #2
                [-35,-35, 35],  #3
                [ 35,-35,-35],  #4
                [ 35, 35,-35],  #5
                [-35, 35,-35],  #6
                [-35,-35,-35],  #7
  ) do |obj|
    obj.scale!(1.1)
    obj.face!(0, 1, 2, 3, decoration: wire_psi)
    obj.face!(7, 6, 5, 4, decoration: wire_lambda)
    obj.face!(0, 4, 5, 1, decoration: wire_xi)
    obj.face!(3, 2, 6, 7, decoration: wire_delta)
    obj.face!(1, 5, 6, 2, decoration: wire_phi)
    obj.face!(3, 7, 4, 0, decoration: wire_pi)
    obj.to_prog(self, :cube)
  end

  # The above Object3D.to_prog creates the following program data:

  # ns :cube do
  #                     # a list of Vertex object data, terminated with -128
  #   vs                data Vertex,*Vertex.scale(1.1, Vertex.make_many(
  #                       [ 35,-35, 35],
  #                       [ 35, 35, 35],
  #                       [-35, 35, 35],
  #                       [-35,-35, 35],
  #                       [ 35,-35,-35],
  #                       [ 35, 35,-35],
  #                       [-35, 35,-35],
  #                       [-35,-35,-35]))
  #                     db   -128

  #                     # a list of faces terminated with 0
  #                     face vs[0].scr, vs[1].scr, vs[2].scr, edge0,  edge1,  edge2,  edge3,  decoration: wire_psi
  #                     face vs[7].scr, vs[6].scr, vs[5].scr, edge4,  edge5,  edge6,  edge7,  decoration: wire_lambda
  #                     face vs[0].scr, vs[4].scr, vs[5].scr, edge8,  edge6,  edge9,  edge0,  decoration: wire_xi
  #                     face vs[3].scr, vs[2].scr, vs[6].scr, edge2,  edge10, edge4,  edge11, decoration: wire_delta
  #                     face vs[1].scr, vs[5].scr, vs[6].scr, edge9,  edge5,  edge10, edge1,  decoration: wire_phi
  #                     face vs[3].scr, vs[7].scr, vs[4].scr, edge11, edge7,  edge8,  edge3,  decoration: wire_pi
  #                     dw   0

  #                     # routines for drawing edges
  #   edge0             edge vs[0], vs[1]
  #   edge1             edge vs[1], vs[2]
  #   edge2             edge vs[2], vs[3]
  #   edge3             edge vs[3], vs[0]
  #   edge4             edge vs[7], vs[6]
  #   edge5             edge vs[6], vs[5]
  #   edge6             edge vs[5], vs[4]
  #   edge7             edge vs[4], vs[7]
  #   edge8             edge vs[0], vs[4]
  #   edge9             edge vs[5], vs[1]
  #   edge10            edge vs[2], vs[6]
  #   edge11            edge vs[7], vs[3]
  # end

                      dc!
                      dc!"*********************************************"
                      dc!"***            CUBE 3D DECALS             ***"
                      dc!"*********************************************"

  ns :wire_psi do
    vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
                        [  0,-20, 35],
                        [  0, 20, 35],
                        [-20,-20, 35],
                        [-10,  0, 35],
                        [ 10,  0, 35],
                        [ 20,-20, 35]))
                      db   -128
                      draw_wire vs[0], vs[1]
                      draw_poly_wire vs[2], vs[3], vs[4], vs[5], closed: false
                      ret
  end

  ns :wire_lambda do
    vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
                        [-15, 25,-35],
                        [  0, 25,-35],
                        [ 15,-25,-35],
                        [ 25,-20,-35],
                        [  3,  5,-35],
                        [-20,-25,-35]))
                      db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], closed: false
                      draw_wire vs[4], vs[5]
                      ret
  end

  ns :wire_pi do
    vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
                        [-20,-35,-20],
                        [ 20,-35,-20],
                        [-10,-35,-15],
                        [-15,-35, 20],
                        [ 10,-35,-15],
                        [ 15,-35, 20]))
                      db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3]
                      draw_wire vs[4], vs[5]
                      ret
  end

  ns :wire_epsilon do
    vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
                        [35,-20, 20],
                        [35,-20,-20],
                        [35, 20,-20],
                        [35, 20, 20],
                        [35,  0,-20],
                        [35,  0,  5]))
                      db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], closed: false
                      draw_wire vs[4], vs[5]
                      ret
  end

  ns :wire_xi do
    vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
                        [35,-15,-20],
                        [35,-20,-20],
                        [35,-20, 20],
                        [35,-15, 20],
                        [35,  0,-10],
                        [35,  0, 10],
                        [35, 15,-20],
                        [35, 20,-20],
                        [35, 20, 20],
                        [35, 15, 20]))
                      db   -128
                      draw_wire vs[4], vs[5]
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], closed: false
                      draw_poly_wire vs[6], vs[7], vs[8], vs[9], closed: false
                      ret
  end

  ns :wire_delta do
    vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
                        [-35,-20,  0],
                        [-35, 20,-20],
                        [-35, 20, 20]))
                      db   -128
                      draw_poly_wire vs[0], vs[1], vs[2]
                      ret
  end

  ns :wire_phi do
    vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
                        [  0, 35,-20],
                        [  0, 35, 20],
                        [-15, 35,-10],
                        [ 15, 35,-10],
                        [ 15, 35, 10],
                        [-15, 35, 10]))
                      db   -128
                      draw_wire vs[0], vs[1]
                      draw_poly_wire vs[2], vs[3], vs[4], vs[5]
                      ret
  end

                      dc!
                      dc!"*********************************************"
                      dc!"***                ELITE 3D               ***"
                      dc!"*********************************************"

  Object3D.make([  0,  8,  0],  # 0
                [-40, -6, 30],  # 1
                [-15, -8, 30],  # 2
                [ 15, -8, 30],  # 3
                [ 40, -6, 30],  # 4
                [ 25,  7, 30],  # 5
                [  0,  8, 30],  # 6
                [-25,  7, 30],  # 7
                [-40, -6,  5],  # 8
                [ 40, -6,  5],  # 9
                [-15, -8,-30],  #10
                [ 15, -8,-30],  #11
                # [  0, -8,-30],  #12
                # [  0, -8,-40],  #13
  ) do |obj|
    obj.scale!(1.35)
    obj.face!(0, 11, 10)
    obj.face!(0, 10,  7)
    obj.face!(0,  5, 11)
    obj.face!(0,  7,  6)
    obj.face!(0,  6,  5)
    obj.face!(7, 10,  8)
    obj.face!(1,  7,  8)
    obj.face!(5,  9, 11)
    obj.face!(4,  9,  5)
    obj.face!(2,  1,  8, 10)
    obj.face!(9,  4,  3, 11)
    obj.face!(3,  2, 10, 11)
    obj.face!(1,  2,  3,  4, 5, 6, 7, surface: [3, 6, 2], decoration: wire_engines)
    obj.to_prog(self, :elite)
  end
                      dc!
                      dc!"*********************************************"
                      dc!"***            ELITE 3D DECALS            ***"
                      dc!"*********************************************"
  ns :wire_engines do
    vs                data Vertex, *Vertex.scale(1.35, Vertex.make_many(
                        [-30,  0, 30],  #0
                        [-26, -1, 30],  #1
                        [-26,  1, 30],  #2
                        [ 30,  0, 30],  #3
                        [ 26, -1, 30],  #4
                        [ 26,  1, 30],  #5
                        [-20, -4, 30],  #6
                        [ -5, -5, 30],  #7
                        [ -5,  5, 30],  #8
                        [-20,  4, 30],  #9
                        [ 20, -4, 30],  #10
                        [  5, -5, 30],  #11
                        [  5,  5, 30],  #12
                        [ 20,  4, 30])) #13
                      db   -128
                      draw_poly_wire vs[ 0], vs[ 1], vs[ 2]
                      draw_poly_wire vs[ 3], vs[ 4], vs[ 5]
                      draw_poly_wire vs[ 6], vs[ 7], vs[ 8], vs[ 9]
                      draw_poly_wire vs[10], vs[11], vs[12], vs[13]
                      ret
  end

                      dc!
                      dc!"*********************************************"
                      dc!"***           ROTATION / MATRIX           ***"
                      dc!"*********************************************"

  yaw                 db 0
  pitch               db 0
  roll                db 0
  rotation            data Rotation, [
                        sin(0), cos(0),
                        sin(0), cos(0),
                        sin(0), cos(0),
                      ].map(&:to_fixed16_8)
  matrix              data Matrix, *[0].map {|angle|
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

quat3d = QFace3D.new 0x9000
puts quat3d.debug
puts "="*68
%w[start apply_matrix_to_vertices.apply_matrix_a rotate draw start.draw_loop.draw_faces_a 
   cube wire_delta wire_epsilon wire_lambda wire_phi wire_pi wire_psi wire_xi
   elite wire_engines].each do |label|
  puts "#{label.ljust(40)}: 0x#{quat3d[label].to_s(16).upcase} - #{quat3d[label]}, size: #{quat3d['+'+label]}"
end
puts "="*68

program = Basic.parse_source <<-END
  10 RANDOMIZE USR #{quat3d[:start]}
9998 GO TO 10000
9999 CLEAR #{quat3d.org-1}: LOAD ""CODE: RUN
END
puts program.to_source escape_keywords:true

tap_name = 'examples/qface3d128.tap'
program.save_tap tap_name, line:9999
quat3d.save_tap tap_name, append:true
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
