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

## Tools for Object3D
module Vec3
  ## Returns a new vector by adding vector a to b
  def vec3add(a, b)
    a.zip(b).map {|a, b| a + b }
  end
  ## Returns a new vector by subtracting vector b from a
  def vec3sub(a, b)
    a.zip(b).map {|a, b| a - b }
  end
  ## Scales a vector v by scalar s and returns a new vector
  def vec3scale(v, s)
    v.map{|x| x*s }
  end
  ## Returns a dot product of 2 vectors
  def vec3dot(a, b)
    ax, ay, az = a
    bx, by, bz = b
    ax * bx + ay * by + az * bz
  end
  ## Returns a cross product of 2 vectors
  def vec3cross(a, b)
    ax, ay, az = a
    bx, by, bz = b
    [ay * bz - az * by,
     az * bx - ax * bz,
     ax * by - ay * bx]
  end
  ## Returns the length of a vector.
  def vec3len(v)
    Math::sqrt(vec3dot(v, v))
  end
  ## Returns a normalized vector.
  def vec3normalize(v)
    n = vec3len(v)
    raise ArgumentError, "Zero-length vector" if n < 1e-15
    x, y, z = v
    [x/n, y/n, z/n]
  end
  ## Calculates a centroid of a triangle: (A + B + C)/3 and returns [cx, cy, cz]
  def vec3centroid3(a, b, c)
    ax, ay, az = a
    bx, by, bz = b
    cx, cy, cz = c
    x = (ax + bx + cx) / 3.0
    y = (ay + by + cy) / 3.0
    z = (az + bz + cz) / 3.0
    [x, y, z]
  end
  ##
  # Calculates a centroid of a solid simple (planar) polygon in 3D and returns [cx, cy, cz].
  #
  # +vertices+:: [[x,y,z], ...] in order (CW or CCW).
  def vec3polygon_centroid(vertices)
    n = vertices.length
    raise ArgumentError, "need at least 3 vertices" if n < 3

    # plane normal via edge cross-sum (vector area)
    nx = ny = nz = 0.0
    n.times do |i|
      x0, y0, z0 = vertices[i]
      x1, y1, z1 = vertices[(i + 1) % n]
      nx += y0 * z1 - z0 * y1
      ny += z0 * x1 - x0 * z1
      nz += x0 * y1 - y0 * x1
    end
    norm = Math.sqrt(nx*nx + ny*ny + nz*nz)
    raise ArgumentError, "degenerate polygon (area ~ 0 or non-planar)" if norm < 1e-12
    nhx, nhy, nhz = nx / norm, ny / norm, nz / norm  # unit normal

    # triangulate as (v0, vi, v(i+1)) and accumulate area-weighted centroids
    va = vertices[0]
    area_sum = 0.0
    cx = cy = cz = 0.0

    (1...(n - 1)).each do |i|
      vb = vertices[i]
      vc = vertices[i + 1]

      # cross = (b-a) x (c-a)
      crx, cry, crz = vec3cross(vec3sub(vb, va), vec3sub(vc, va))

      # signed area of triangle relative to polygon normal
      signed_area = 0.5 * (nhx * crx + nhy * cry + nhz * crz)

      # triangle centroid
      tcx, tcy, tcz = vec3centroid3(va, vb, vc)

      cx += tcx * signed_area
      cy += tcy * signed_area
      cz += tcz * signed_area
      area_sum += signed_area
    end

    raise ArgumentError, "zero signed area" if area_sum.abs < 1e-12
    [cx / area_sum, cy / area_sum, cz / area_sum]
  end
  ##
  # Builds an orthonormal basis {U, V} lying in the triangle's (A, B, C) plane.
  # Returns a tuple of 2 vectors: [U, V].
  #
  # Options:
  # * +align+:: selects which triangle edge u is aligned with:
  #             +:ab+, +:ba+, +:ac+, +:ca+, +:bc+, +:cb+
  def vec3plane(a, b, c, align: :ba)
    # if U flips (AB vs BA) we want V to flip too
    # thus, we want normal to maintain the same direction
    # hence a deliberate choice of a 2nd edge direction
    case align
    when :ba
      u_raw = vec3sub(a, b) # b -> a
      n = vec3cross(u_raw, vec3sub(c, a)) # a -> c
      n = vec3cross(u_raw, vec3sub(c, b)) if vec3len(n) < 1e-12
    when :ab
      u_raw = vec3sub(b, a)
      n = vec3cross(u_raw, vec3sub(b, c))
      n = vec3cross(u_raw, vec3sub(a, c)) if vec3len(n) < 1e-12
    when :ac
      u_raw = vec3sub(c, a)
      n = vec3cross(u_raw, vec3sub(b, c))
      n = vec3cross(u_raw, vec3sub(b, a)) if vec3len(n) < 1e-12
    when :ca
      u_raw = vec3sub(a, c)
      n = vec3cross(u_raw, vec3sub(a, b))
      n = vec3cross(u_raw, vec3sub(c, b)) if vec3len(n) < 1e-12
    when :cb
      u_raw = vec3sub(b, c)
      n = vec3cross(u_raw, vec3sub(a, b))
      n = vec3cross(u_raw, vec3sub(a, c)) if vec3len(n) < 1e-12
    when :bc
      u_raw = vec3sub(c, b)
      n = vec3cross(u_raw, vec3sub(c, a))
      n = vec3cross(u_raw, vec3sub(b, a)) if vec3len(n) < 1e-12
    else
      raise ArgumentError, "unexpected :align option value: #{align}"
    end

    if vec3len(n) < 1e-12 || vec3len(u_raw) < 1e-12
      raise ArgumentError, "degenerate/collinear triangle"
    end
    n = vec3normalize(n)
    u = vec3normalize(u_raw)
    # v is perpendicular to u in-plane (via v = (n × u)).
    v = vec3normalize(vec3cross(n, u))
    [u, v]
  end
  ##
  # Maps flat (x, y) coordinates to the {u, v} plane with (0,0) located at the center
  # and axes lying in the plane.
  #
  # Optionally scaled. If scale is 1.0 x,y are in world-length units.
  def vec3map_vec2_plane(x, y, center, u, v, scale: 1.0)
    vec3add(center, vec3add(vec3scale(u, x * scale), vec3scale(v, y * scale)))
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
  # Return 3 indices of coordinates of this surface
  def get_surface
    surface || indices[0...3]
  end
end

##
# A helper struct for creating 3D solid data for Z80 programs.
#
# * +vertices+:: an array of [x, y, z] triplets (points in 3D space),
# * +edges+:: a map of {[A, B] -> edge index} used for edges deduplication, where A and B
#             are vertex indices,
# * +faces+:: an array of Face3D objects.
Object3D = ::Struct.new :vertices, :edges, :faces do
  include Vec3
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
  # * +indices+:: at least 3 0-based vertex indices;
  # * +surface+:: an optional array tuple of 3 indices that is used to calculate the face's direction,
  #   otherwise the first 3 vertices are chosen as the surface triangle;
  # * +decoration+:: an optional surface decoration data address or a Z80 program label.
  #
  # The winding of a +surface+ determines visibility of the face.
  def face!(*indices, surface:nil, decoration:nil)
    faces.push Face3D.make(vertices, indices, surface:surface, decoration:decoration)
    (indices + [indices.first]).each_cons(2) do |a,b|
      unless edges.has_key?([a,b]) || edges.has_key?([b,a])
        edges[[a,b]] = edges.length
      end
    end
  end
  ##
  # Returns an array of vertices in 3D space from 2D coordinates mapped onto an indicated face
  # and centered at the face's centroid.
  #
  # Options:
  # +scale+:: a scalar for optional scaling of x,y coordinates
  # +align+:: which triangle edge x is aligned with: :ab, :ba, :ac, :ca, :bc, :cb
  #
  # For clockwise triangles, when x is aligned with BA, AC or CB the y grows
  # towards the 3rd vertex, for other +align+ choices it grows away from it.
  def map_vec2_face(face_index, *vec2_args, scale: 1.0, align: :ba)
    face = faces[face_index]
    a, b, c = face.get_surface.map {|i| vertices[i]}
    u, v = vec3plane(a, b, c, align:align)
    g = if face.indices.length > 3
      vec3polygon_centroid(face.indices.map {|i| vertices[i]})
    else
      vec3centroid3(a, b, c)
    end
    vec2_args.map do |x, y|
      vec3map_vec2_plane(x, y, g, u, v, scale:scale)
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
  # <b>NOTE</b>: Currently this function uses +prog.edge+ and +prog.facet+ macros to create
  # data of facets and edges.
  def to_prog(prog, name)
    raise ArgumentError, "no edges!" if edges.empty?
    eind = edges.invert
    prog.ns name do
      vs = prog.define_label :vs, prog.data(prog::Vertex, *prog::Vertex.make_many(*vertices))
      prog.define_label :endvs, prog.db(-128) # terminator
      faces.each_with_index do |f, j|
        p1, p2, p3 = f.get_surface
        edge_labels = []
        (f.indices + [f.indices.first]).each_cons(2) do |a, b|
          i = edges[[a,b]] || edges[[b,a]]
          raise ArgumentError, "no such edge: #{a} - #{b}" unless i
          edge_labels.push(prog.define_label "edge#{i+1}")
        end
        prog.define_label "face#{j+1}",
          prog.facet(vs[p1].scr, vs[p2].scr, vs[p3].scr, *edge_labels, decoration:f.decoration)
      end
      prog.define_label :endfacets, prog.dw(0) # terminator
      eind.each do |i, (a,b)|
        prog.define_label "edge#{i+1}", prog.edge(vs[a], vs[b])
      end
      prog.define_label :endsolid, prog.label
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
  BG_ATTR = 0b01000010

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
                  # SinCos table address just below the shadow screen address
  sincos          addr 0xBC00, Utils::SinCos::SinCos

  ##########
  # Macros #
  ##########

  # Macro used by Object3D.to_prog
  #
  # Crates a facet entry struct
  # < s1, s2, s3: addresses of scr coordinates of 3 facet points
  # < edges: a list of edge address labels
  macro :facet do |_, s1, s2, s3, *edges, decoration:nil|
    decoration = 0 unless decoration
    raise ArgumentError, "no edges!" if edges.empty?
    points            dw   s1, s2, s3
    count             db   edges.length
    edges.each do |edge|
                      dw   edge
    end
    decal             dw   decoration
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

  # Plot particles.
  # < vertices: an array of Vertex labels
  # < plot_pixel: a plot_pixel routine address
  # < preshift: a pixel preshift table address
  # < tail_call: whether a routine should jump instead of calling on a last particle
  macro :particles do |eoc, *vertices, plot_pixel: plot, preshift: draw.preshifted_pixel, tail_call: false|
    vertices.each_with_index do |vs, i|
      if i == 0
                      ld   de, preshift
      else
                      ld   e, preshift & 0xFF
      end
                      ld   hl, [vs.scr]
      if tail_call && i == vertices.length - 1
                      jp   plot_pixel
      else
                      call plot_pixel
      end
    end
  end

  # Draws a line between vertices.
  # < v1: Vertex label
  # < v2: Vertex label
  # < draw_line: a drawing routine address
  # < tail_call: whether a routine should jump instead of calling
  macro :draw_wire do |_, v1, v2, draw_line: draw.line_to, tail_call: false|
                      ld   de, [v1.scr]     # (4) 20
                      ld   hl, [v2.scr]     # (3) 16
    if tail_call
                      jp   draw_line        # (3) 10
    else
                      call draw_line        # (3) 17
    end
  end

  # Draws a poly-line between many vertices.
  # < vertices: an array of Vertex labels
  # < closed: if the last vertex should be connected to the first
  # < draw_line: a drawing routine address
  # < tail_call: whether a routine should jump instead of calling on a last line
  macro :draw_poly_wire do |_, *vertices, closed: true, draw_line: draw.line_to, tail_call: false|
    raise ArgumentError if vertices.length < 3
    v1 = nil
    vertices.each_with_index do |v, i|
      unless i == 0
        v2 = v
        tc = if closed then
          false
        else
          tail_call && i == vertices.length - 1
        end
                      draw_wire v1, v2, draw_line:draw_line, tail_call:tc
      end
      v1 = v
    end
    if closed
                      draw_wire vertices.last, vertices.first, draw_line:draw_line, tail_call:tail_call
    end
  end

  # Prints +text+ string to an open channel using ROM +pr_string+ routine.
  macro :print_text do |eoc, text|
    stext = ZXLib::Basic::Vars.program_text_to_string text
                        ld   de, text_data
                        ld   bc, +text_data
                        call rom.pr_string
                    if stext.length > 0x7F
                        jp   eoc
                    else
                        jr   eoc
                    end
    text_data           data stext
  end

  ##
  # Draws visible faces of a solid object.
  #
  # The face is visible if a face of the solid is oriented towards the viewer.
  # We find out about it if the Z axis of the face's normal vector is negative.
  #
  # To simplify this process we calculate only the Z-axis of a normal vector
  # from the 2D coordinates of the face vertices already projected on the screen,
  # by calculating a cross-product of 2D vectors: (B - A) x (C - B).
  #
  # Since multiple faces can share the same edges, each edge is drawn only once.
  #
  # The +frame+ counter, accessible at +frame_p+ sub-label is used to check and
  # mark edges, determining if the edge was already drawn or not for the current
  # frame.
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

  MIN_Z = 128 - 8
  MAX_Z = 128 + 58
  X_POS = 128
  Y_POS = 96

  ROLL  = 2
  PITCH = 3
  YAW   = 1

  with_saved :start, :exx, hl, ret: true do |eoc|
                        call  make_sincos
                        call rom.cl_all
                        ld   a, 2
                        call rom.chan_open
                        print_text "`INK 0``PAPER 7``BRIGHT 0`"+
                                   "`AT  4,9`O,P - pitch speed" +
                                     "`TAB 9`Q,A - roll speed" +
                                     "`TAB 9`Z,X - yaw speed" +
                                     "`TAB 9`W,S - far/near" +
                                   "`TAB 7`SPACE - stop" +
                                     "`TAB 9`  C - reset yaw" +
                                     "`TAB 9`V-M - (re)set roll" +
                                     "`TAB 9`H-L - (re)set pitch" +
                                     "`TAB 9`0-7 - PAPER color" +
                                      "`TAB 11`8 - reset INK" +
                                      "`TAB 11`9 - next INK" +
                               "`TAB 3`any other - next solid" +
                          "`''''``TAB 2`press any key to continue"
    release_key0        halt
                        key_pressed?
                        jr   NZ, release_key0 # wait for any key being pressed to be released
    press_key           halt
                        key_pressed?
                        jr   Z, press_key # wait for any key to be pressed
    release_key1        halt
                        key_pressed?
                        jr   NZ, release_key1 # wait for any key being pressed to be released
                        # ld   a, 2
                        # call rom.chan_open
                        # prepare both shadow and regular screen memory, disabling interrupts first
                        mmu128_select_bank(bank:7, screen:0, disable_intr:true, enable_intr:false)
                        ld   a, BG_ATTR
                        ld   [last_color], a
                        call clear_screen
                        mmu128_swap_screens(swap_bank:true, disable_intr:false, enable_intr:false)
                        ld   a, BG_ATTR
                        call clear_screen
                        print_text "`AT 0,0``INK 8``PAPER 8``BRIGHT 8`OPQAZX:rot WS:zmov C,V-M,H-L:set"
                        mmu128_swap_screens(swap_bank:true, disable_intr:false, enable_intr:false)
                        copy_shadow_screen_region(xy_to_pixel_addr(0,0,scraddr:0xC000), 8, 32, tgtaddr:0xC000, srcaddr:0x4000, check_edge:false, break_oos:false)

                        dc!"*********************************************"
                        dc!"***            RESET ANIMATION            ***"
                        dc!"*********************************************"
                        # reset position
                        ld   a, MAX_Z
                        ld   [apply_matrix_to_vertices.apply_matrix_a.adjust_z_p], a
                        ld   a, X_POS # 128
                        ld   [apply_matrix_to_vertices.apply_matrix_a.adjust_x_p], a
                        ld   a, Y_POS # 96
                        ld   [apply_matrix_to_vertices.apply_matrix_a.adjust_y_p], a

                        # reset orientation
                        xor  a
                        ld   hl, yaw
                        ld   [hl], a # yaw
                        inc  hl
                        ld   [hl], a # pitch
                        inc  hl
                        ld   [hl], a # roll

                        # reset rotation speeds
                        ld   a, ROLL
                        ld   [draw_loop.roll_speed], a
                        ld   a, PITCH
                        ld   [draw_loop.pitch_speed], a
                        ld   a, YAW
                        ld   [draw_loop.yaw_speed], a

                        # reset movement
                        ld   a, -1
                        ld   [draw_loop.zpos_speed], a

                        # save sp
                        ld   [draw_loop.restore_sp_p], sp
                        ld   [draw_loop.course_to_rot.restore_sp_p], sp

                        jp   draw_loop.animate

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
                        clear_screen_region_fast(xy_to_pixel_addr(16, 8, scraddr:0xC000), 182, 28,
                            addr_mode: :first, disable_intr:false, enable_intr:false, save_sp:false)
                        # clear_screen_region_fast uses stack pointer so let's restore it
      restore_sp        ld   sp, 0
      restore_sp_p      as restore_sp + 1
                        dc!
                        dc!"*********************************************"
                        dc!"***             APPLY MATRIX              ***"
                        dc!"*********************************************"

      solid_a           ld   hl, cube
      solid_p           as solid_a + 1 # a pointer to a current object's address
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

      animate           label
                        ld   hl, roll
      roll_a            ld   a, ROLL
      roll_speed        as roll_a + 1
                        add  a, [hl]
                        ld   [hl], a
                        ld   c, a     # c: roll

                        ld   hl, pitch
      pitch_a           ld   a, PITCH
      pitch_speed       as pitch_a + 1
                        add  a, [hl]
                        ld   [hl], a
                        ld   b, a     # b: pitch

                        ld   hl, yaw
      yaw_a             ld   a, YAW
      yaw_speed         as yaw_a + 1
                        add  a, [hl]
                        ld   [hl], a  # a: yaw

                        # yaw=a, pitch=b, roll=c
      course_to_rot     course_to_rotation(rotation, a, b, c, sincos:sincos, save_sp: :restore_only)
                        dc!
                        dc!"*********************************************"
                        dc!"***          ROTATION TO MATRIX           ***"
                        dc!"*********************************************"
      apply_rotation    rotation_to_matrix(matrix, rotation, inline_mul:OPTIMIZE != :size, subroutine:false, optimize:OPTIMIZE)

                        dc!
                        dc!"*********************************************"
                        dc!"***              MOVE OBJECT              ***"
                        dc!"*********************************************"
                        ld   hl, apply_matrix_to_vertices.apply_matrix_a.adjust_z_p
                        ld   a, [hl]
      zpos_a            add  a, -1
      zpos_speed        as zpos_a + 1
                        cp   MIN_Z
                        jr   C, skip_adjust_z
                        cp   MAX_Z + 1
                        jr   NC, skip_adjust_z
                        ld   [hl], a
      skip_adjust_z     label

                        dc!
                        dc!"*********************************************"
                        dc!"***                KEY CHECK              ***"
                        dc!"*********************************************"
                        # loop unless key is pressed
      keyboard_check    key_pressed?
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
                        dc!"***           CHECK OTHER KEYS            ***"
                        dc!"*********************************************"
      ns do |eoc|
                        cp   'o'.ord - ?0.ord
                        jr   NZ, key_p
                        ld   hl, pitch_speed
        dec_hl          dec  [hl]
                        jp   draw_loop
        key_p           cp   'p'.ord - ?0.ord
                        jr   NZ, key_q
                        ld   hl, pitch_speed
        inc_hl          inc  [hl]
                        jp   draw_loop
        key_q           cp   'q'.ord - ?0.ord
                        jr   NZ, key_a
                        ld   hl, roll_speed
                        jr   inc_hl
        key_a           cp   'a'.ord - ?0.ord
                        jr   NZ, key_z
                        ld   hl, roll_speed
                        jr   dec_hl
        key_z           cp   'z'.ord - ?0.ord
                        jr   NZ, key_x
                        ld   hl, yaw_speed
                        jr   inc_hl
        key_x           cp   'x'.ord - ?0.ord
                        jr   NZ, key_w
                        ld   hl, yaw_speed
                        jr   dec_hl
        key_w           cp   'w'.ord - ?0.ord
                        jr   NZ, key_s
                        ld   hl, zpos_speed
                        ld   [hl], 1
                        jp   draw_loop
        key_s           cp   's'.ord - ?0.ord
                        jr   NZ, key_c
                        ld   hl, zpos_speed
                        ld   [hl], -1
                        jp   draw_loop
        key_c           cp   'c'.ord - ?0.ord
                        jr   NZ, key_v
                        xor  a
        set_yaw         ld   hl, yaw
                        ld   [hl], a # roll
                        jp   draw_loop
        key_v           cp   'v'.ord - ?0.ord
                        jr   NZ, key_b
                        xor  a
        set_roll        ld   hl, roll
                        ld   [hl], a # roll
                        jp   draw_loop
        key_b           cp   'b'.ord - ?0.ord
                        jr   NZ, key_n
                        ld   a, 128
                        jr   set_roll
        key_n           cp   'n'.ord - ?0.ord
                        jr   NZ, key_m
                        ld   a, -64
                        jr   set_roll
        key_m           cp   'm'.ord - ?0.ord
                        jr   NZ, key_h
                        ld   a, 64
                        jr   set_roll
        key_h           cp   'h'.ord - ?0.ord
                        jr   NZ, key_j
                        xor  a
        set_pitch       ld   hl, pitch
                        ld   [hl], a # pitch
                        jp   draw_loop
        key_j           cp   'j'.ord - ?0.ord
                        jr   NZ, key_k
                        ld   a, 128
                        jr   set_pitch
        key_k           cp   'k'.ord - ?0.ord
                        jr   NZ, key_l
                        ld   a, -64
                        jr   set_pitch
        key_l           cp   'l'.ord - ?0.ord
                        jr   NZ, key_sp
                        ld   a, 64
                        jr   set_pitch

        key_sp          cp   ' '.ord - ?0.ord
                        jr   NZ, eoc # next object
                        xor  a
                        ld   [pitch_speed], a
                        ld   [roll_speed], a
                        ld   [yaw_speed], a
                        ld   [zpos_speed], a
                        jp   draw_loop
      end

                        dc!
                        dc!"*********************************************"
                        dc!"***               NEXT OBJECT             ***"
                        dc!"*********************************************"
      ns do
                        # find current object
                        ld   hl, [solid_p]
                        ld   sp, solids
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
        not_found       ld   sp, solids
                        pop  de
        set_object      ld   [solid_p], de
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
  draw                make_draw_line_subroutines(make_line:true, make_line_over:false, make_line_inversed:false,
                                                 scraddr:0xC000, check_oos:false)

                      dc!
                      dc!"*********************************************"
                      dc!"***               PLOT PIXEL              ***"
                      dc!"*********************************************"
  plot                plot_pixel(l, h, de, fx: :or, scraddr:0xC000)
                      ret

                      dc!
                      dc!"*********************************************"
                      dc!"***             SINCOS TABLE              ***"
                      dc!"*********************************************"
  make_sincos         create_sincos_from_sintable sincos, sintable:sintable
  sintable            bytes   neg_sintable256_pi_half_no_zero_lo
  sintable_end        label

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

  ## 3D solid object pointers
  solids              dw cube,
                         octahedron,
                         boing,
                         diamond,
                         elite,
                         station,
                         0

                      dc!
                      dc!"*********************************************"
                      dc!"***                CUBE 3D                ***"
                      dc!"*********************************************"
  Object3D.make(
    [ 35, -35,  35],  #0
    [ 35,  35,  35],  #1
    [-35,  35,  35],  #2
    [-35, -35,  35],  #3
    [ 35, -35, -35],  #4
    [ 35,  35, -35],  #5
    [-35,  35, -35],  #6
    [-35, -35, -35],  #7
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
  #                       [ 35, -35,  35],
  #                       [ 35,  35,  35],
  #                       [-35,  35,  35],
  #                       [-35, -35,  35],
  #                       [ 35, -35, -35],
  #                       [ 35,  35, -35],
  #                       [-35,  35, -35],
  #                       [-35, -35, -35]))
  #   endvs             db   -128

  #                     # a list of facets terminated with 0
  #   face1             face vs[0].scr, vs[1].scr, vs[2].scr, edge1,  edge2,  edge3,  edge4,  decoration: wire_psi
  #   face2             face vs[7].scr, vs[6].scr, vs[5].scr, edge5,  edge6,  edge7,  edge8,  decoration: wire_lambda
  #   face3             face vs[0].scr, vs[4].scr, vs[5].scr, edge9,  edge7,  edge10, edge1,  decoration: wire_xi
  #   face4             face vs[3].scr, vs[2].scr, vs[6].scr, edge3,  edge11, edge5,  edge12, decoration: wire_delta
  #   face5             face vs[1].scr, vs[5].scr, vs[6].scr, edge10, edge6,  edge11, edge2,  decoration: wire_phi
  #   face6             face vs[3].scr, vs[7].scr, vs[4].scr, edge12, edge8,  edge9,  edge4,  decoration: wire_pi
  #   endfacets         dw   0

  #                     # routines for drawing edges
  #   edge1             edge vs[0], vs[1]
  #   edge2             edge vs[1], vs[2]
  #   edge3             edge vs[2], vs[3]
  #   edge4             edge vs[3], vs[0]
  #   edge5             edge vs[7], vs[6]
  #   edge6             edge vs[6], vs[5]
  #   edge7             edge vs[5], vs[4]
  #   edge8             edge vs[4], vs[7]
  #   edge9             edge vs[0], vs[4]
  #   edge10            edge vs[5], vs[1]
  #   edge11            edge vs[2], vs[6]
  #   edge12            edge vs[7], vs[3]
  #   endsolid          label
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
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_poly_wire vs[2], vs[3], vs[4], vs[5], closed: false, tail_call: true
    endwires          label
  end

  ns :wire_lambda do
    vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
                        [-15, 25,-35],
                        [  0, 25,-35],
                        [ 15,-25,-35],
                        [ 25,-20,-35],
                        [  3,  5,-35],
                        [-20,-25,-35]))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], closed: false
                      draw_wire vs[4], vs[5], tail_call: true
    endwires          label
  end

  ns :wire_pi do
    vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
                        [-20,-35,-20],
                        [ 20,-35,-20],
                        [-10,-35,-15],
                        [-15,-35, 20],
                        [ 10,-35,-15],
                        [ 15,-35, 20]))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3]
                      draw_wire vs[4], vs[5], tail_call: true
    endwires          label
  end

  # ns :wire_epsilon do
  #   vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
  #                       [35,-20, 20],
  #                       [35,-20,-20],
  #                       [35, 20,-20],
  #                       [35, 20, 20],
  #                       [35,  0,-20],
  #                       [35,  0,  5]))
  #   endvs             db   -128
  #                     draw_poly_wire vs[0], vs[1], vs[2], vs[3], closed: false
  #                     draw_wire vs[4], vs[5], tail_call: true
  #   endwires          label
  # end

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
    endvs             db   -128
                      draw_wire vs[4], vs[5]
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], closed: false
                      draw_poly_wire vs[6], vs[7], vs[8], vs[9], closed: false, tail_call: true
    endwires          label
  end

  ns :wire_delta do
    vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
                        [-35,-20,  0],
                        [-35, 20,-20],
                        [-35, 20, 20]))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], tail_call: true
    endwires          label
  end

  ns :wire_phi do
    vs                data Vertex, *Vertex.scale(1.1, Vertex.make_many(
                        [  0, 35,-20],
                        [  0, 35, 20],
                        [-15, 35,-10],
                        [ 15, 35,-10],
                        [ 15, 35, 10],
                        [-15, 35, 10]))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_poly_wire vs[2], vs[3], vs[4], vs[5], tail_call: true
    endwires          label
  end

                      dc!
                      dc!"*********************************************"
                      dc!"***             OCTAHEDRON 3D             ***"
                      dc!"*********************************************"
  o = Object3D.make(
    [ 25,  25,   0], #0
    [ 25, -25,   0], #1
    [  0,   0, -35], #2
    [-25,  25,   0], #3
    [-25, -25,   0], #4
    [  0,   0,  35], #5
  ) do |obj|
    obj.scale!(1.8)
    obj.face!(0, 1, 2, decoration: wire_cisters_1)
    obj.face!(3, 0, 2, decoration: wire_cisters_2)
    obj.face!(4, 3, 2, decoration: wire_cisters_3)
    obj.face!(1, 4, 2, decoration: wire_cisters_4)
    obj.face!(1, 0, 5, decoration: wire_cisters_5)
    obj.face!(0, 3, 5, decoration: wire_cisters_6)
    obj.face!(3, 4, 5, decoration: wire_cisters_7)
    obj.face!(4, 1, 5, decoration: wire_cisters_8)
    obj.to_prog(self, :octahedron)
  end

                      dc!
                      dc!"*********************************************"
                      dc!"***         OCTAHEDRON 3D DECALS          ***"
                      dc!"*********************************************"

  ns :wire_cisters_1 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(0, 
                        [ -5, -15],
                        [ -5,  20],
                        [  5,  20],
                        align: :cb))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], closed: false, tail_call: true
    endwires          label
  end

  ns :wire_cisters_2 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(1,
                        [ -5, -15],
                        [ -5,  20],
                        [ -5,  10],
                        [  5,  10],
                        align: :cb))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_cisters_3 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(2,
                        [ -5, -15],
                        [ -5,  20],
                        [  5,  10],
                        align: :cb))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], closed: false, tail_call: true
    endwires          label
  end

  ns :wire_cisters_4 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(3,
                        [ -5, -15],
                        [ -5,  20],
                        [ -5,  10],
                        [  5,  20],
                        align: :cb))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_cisters_5 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(4,
                        [ -5, -15],
                        [ -5,  20],
                        [  5,  20],
                        [ -5,  10],
                        align: :cb))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], closed: false, tail_call: true
    endwires          label
  end

  ns :wire_cisters_6 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(5,
                        [ -5, -15],
                        [ -5,  20],
                        [  5,  10],
                        [  5,  20],
                        align: :cb))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_cisters_7 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(6,
                        [ -5, -15],
                        [ -5,  20],
                        [  5,  20],
                        [  5,  10],
                        align: :cb))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], closed: false, tail_call: true
    endwires          label
  end

  ns :wire_cisters_8 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(7,
                        [ -5, -15],
                        [ -5,  20],
                        [ -5,  10],
                        [  5,  10],
                        [  5,  20],
                        align: :cb))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_poly_wire vs[2], vs[3], vs[4], closed: false, tail_call: true
    endwires          label
  end

                      dc!
                      dc!"*********************************************"
                      dc!"***               BOING 3D                ***"
                      dc!"*********************************************"
  o = Object3D.make(
    [ -3,  60,   0], #0
    [ 32,  49,   0], #1
    [ 19,  49, -28], #2
    [-11,  49, -34], #3
    [-35,  49, -15], #4
    [-35,  49,  15], #5
    [-11,  49,  34], #6
    [ 19,  49,  28], #7
    [ 54,  19,   0], #8
    [ 33,  19, -45], #9
    [-16,  19, -56], #10
    [-54,  19, -25], #11
    [-54,  19,  25], #12
    [-16,  19,  56], #13
    [ 33,  19,  45], #14
    [ 54, -19,   0], #15
    [ 33, -19, -45], #16
    [-16, -19, -56], #17
    [-54, -19, -25], #18
    [-54, -19,  25], #19
    [-16, -19,  56], #20
    [ 33, -19,  45], #21
    [ 32, -49,   0], #22
    [ 19, -49, -28], #23
    [-11, -49, -34], #24
    [-35, -49, -15], #25
    [-35, -49,  15], #26
    [-11, -49,  34], #27
    [ 19, -49,  28], #28
    [ -3, -60,   0], #29
  ) do |obj|
    obj.scale!(1.0)
    obj.face!(0, 1, 2)
    obj.face!(0, 2, 3)
    obj.face!(0, 3, 4)
    obj.face!(0, 4, 5)
    obj.face!(0, 5, 6)
    obj.face!(0, 6, 7)
    obj.face!(0, 7, 1)
    obj.face!(1, 8, 9, 2)
    obj.face!(2, 9, 10, 3)
    obj.face!(3, 10, 11, 4)
    obj.face!(4, 11, 12, 5)
    obj.face!(5, 12, 13, 6)
    obj.face!(6, 13, 14, 7)
    obj.face!(7, 14, 8, 1)
    obj.face!(8, 15, 16, 9, decoration: wire_ansuz)
    obj.face!(9, 16, 17, 10, decoration: wire_algiz)
    obj.face!(10, 17, 18, 11, decoration: wire_othila)
    obj.face!(11, 18, 19, 12, decoration: wire_fehu)
    obj.face!(12, 19, 20, 13, decoration: wire_mannaz)
    obj.face!(13, 20, 21, 14, decoration: wire_inguz)
    obj.face!(14, 21, 15, 8, decoration: wire_nauthiz)
    obj.face!(15, 22, 23, 16)
    obj.face!(16, 23, 24, 17)
    obj.face!(17, 24, 25, 18)
    obj.face!(18, 25, 26, 19)
    obj.face!(19, 26, 27, 20)
    obj.face!(20, 27, 28, 21)
    obj.face!(21, 28, 22, 15)
    obj.face!(29, 23, 22)
    obj.face!(29, 24, 23)
    obj.face!(29, 25, 24)
    obj.face!(29, 26, 25)
    obj.face!(29, 27, 26)
    obj.face!(29, 28, 27)
    obj.face!(29, 22, 28)
    obj.to_prog(self, :boing)
  end
                      dc!
                      dc!"*********************************************"
                      dc!"***            BOING 3D DECALS            ***"
                      dc!"*********************************************"

  ns :wire_ansuz do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(14,
                        [  0,  10], [  0, -10],
                        [  5,   5],
                        [  0,   5], [  5,  0],
                        align: :cb))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[0], vs[2]
                      draw_wire vs[3], vs[4], tail_call: true
    endwires          label
  end

  ns :wire_algiz do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(15,
                        [  0,  10], [  0, -10],
                        [ -5,   5], [  0,   0], [  5,  5],
                        align: :bc))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_poly_wire vs[2], vs[3], vs[4], closed: false, tail_call: true
    endwires          label
  end

  ns :wire_othila do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(16,
                        [  0,  10],
                        [ -5,   5], [  5,-10],
                        [  5,   5], [ -5,-10],
                        align: :cb))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], closed: false
                      draw_poly_wire vs[0], vs[3], vs[4], closed: false, tail_call: true
    endwires          label
  end

  ns :wire_fehu do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(17,
                        [  0,  10], [  0, -10],
                        [  0,   5], [  5,  10],
                        [  0,   0], [  5,  5],
                        align: :bc))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3]
                      draw_wire vs[4], vs[5], tail_call: true
    endwires          label
  end

  ns :wire_mannaz do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(18,
                        [ -5,  10], [ -5, -10],
                        [  5,  10], [  5, -10],
                        [  5,   0], [ -5,   0],
                        align: :cb))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3]
                      draw_wire vs[0], vs[4]
                      draw_wire vs[2], vs[5], tail_call: true
    endwires          label
  end

  ns :wire_inguz do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(19,
                        [ -5,  10], [  5,  0], [ -5,-10],
                        [  5,  10], [ -5,  0], [  5,-10],
                        align: :bc))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], closed: false
                      draw_poly_wire vs[3], vs[4], vs[5], closed: false, tail_call: true
    endwires          label
  end

  ns :wire_nauthiz do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(20,
                        [  0,  10], [  0,-10],
                        [ -5,  10], [  5,  0],
                        align: :bc))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3], tail_call: true
    endwires          label
  end

                      dc!
                      dc!"*********************************************"
                      dc!"***              DIAMOND 3D               ***"
                      dc!"*********************************************"
  o = Object3D.make(
    [ 30,  50,   0], #0
    [ 21,  50, -21], #1
    [  0,  50, -30], #2
    [-21,  50, -21], #3
    [-30,  50,   0], #4
    [-21,  50,  21], #5
    [  0,  50,  30], #6
    [ 21,  50,  21], #7
    [ 42,  39, -17], #8
    [ 17,  39, -42], #9
    [-17,  39, -42], #10
    [-42,  39, -17], #11
    [-42,  39,  17], #12
    [-17,  39,  42], #13
    [ 17,  39,  42], #14
    [ 42,  39,  17], #15
    [ 37,  28, -37], #16
    [  0,  28, -53], #17
    [-37,  28, -37], #18
    [-53,  28,   0], #19
    [-37,  28,  37], #20
    [  0,  28,  53], #21
    [ 37,  28,  37], #22
    [ 53,  28,   0], #23
    [  0, -50,   0], #24
  ) do |obj|
    obj.scale!(1.0)
    obj.face!(0, 1, 2, 3, 4, 5, 6, 7, surface: [0, 3, 6], decoration: wire_rune)
    obj.face!(8, 1, 0)
    obj.face!(9, 2, 1)
    obj.face!(10, 3, 2)
    obj.face!(11, 4, 3)
    obj.face!(12, 5, 4)
    obj.face!(13, 6, 5)
    obj.face!(14, 7, 6)
    obj.face!(15, 0, 7)
    obj.face!(1, 8, 16)
    obj.face!(16, 9, 1)
    obj.face!(2, 9, 17)
    obj.face!(17, 10, 2)
    obj.face!(3, 10, 18)
    obj.face!(18, 11, 3)
    obj.face!(4, 11, 19)
    obj.face!(19, 12, 4)
    obj.face!(5, 12, 20)
    obj.face!(20, 13, 5)
    obj.face!(6, 13, 21)
    obj.face!(21, 14, 6)
    obj.face!(7, 14, 22)
    obj.face!(22, 15, 7)
    obj.face!(0, 15, 23)
    obj.face!(23, 8, 0)
    obj.face!(8, 23, 16)
    obj.face!(9, 16, 17)
    obj.face!(10, 17, 18)
    obj.face!(11, 18, 19)
    obj.face!(12, 19, 20)
    obj.face!(13, 20, 21)
    obj.face!(14, 21, 22)
    obj.face!(15, 22, 23)
    obj.face!(24, 16, 23, decoration: wire_glow1)
    obj.face!(24, 17, 16, decoration: wire_glow2)
    obj.face!(24, 18, 17, decoration: wire_glow3)
    obj.face!(24, 19, 18, decoration: wire_glow4)
    obj.face!(24, 20, 19, decoration: wire_glow5)
    obj.face!(24, 21, 20, decoration: wire_glow6)
    obj.face!(24, 22, 21, decoration: wire_glow7)
    obj.face!(24, 23, 22, decoration: wire_glow8)
    obj.to_prog(self, :diamond)
  end
                      dc!
                      dc!"*********************************************"
                      dc!"***           DIAMOND 3D DECALS           ***"
                      dc!"*********************************************"
  ns :wire_rune do
    vs                data Vertex, *Vertex.scale(1.0, Vertex.make_many(
                        [-10, 50,  10],  #0
                        [ 10, 50,  10],  #1
                        [-10, 50, -10],  #2
                        [ 10, 50, -10])) #4
    endvs             db   -128
                      draw_poly_wire vs[0], vs[3], vs[1], vs[2], tail_call: true
    endwires          label
  end

  ns :wire_glow1 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(33,
                        [  0, -10],
                        [  5,   0],
                        [  0,  10],
                        [ -5,   0],
                        align: :bc))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_glow2 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(34,
                        [  0, -10],
                        [  5,   0],
                        [  0,  10],
                        [ -5,   0],
                        align: :bc))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_glow3 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(35,
                        [  0, -10],
                        [  5,   0],
                        [  0,  10],
                        [ -5,   0],
                        align: :bc))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_glow4 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(36,
                        [  0, -10],
                        [  5,   0],
                        [  0,  10],
                        [ -5,   0],
                        align: :bc))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_glow5 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(37,
                        [  0, -10],
                        [  5,   0],
                        [  0,  10],
                        [ -5,   0],
                        align: :bc))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_glow6 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(38,
                        [  0, -10],
                        [  5,   0],
                        [  0,  10],
                        [ -5,   0],
                        align: :bc))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_glow7 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(39,
                        [  0, -10],
                        [  5,   0],
                        [  0,  10],
                        [ -5,   0],
                        align: :bc))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_glow8 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(40,
                        [  0, -10],
                        [  5,   0],
                        [  0,  10],
                        [ -5,   0],
                        align: :bc))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], tail_call: true
    endwires          label
  end

                      dc!
                      dc!"*********************************************"
                      dc!"***                ELITE 3D               ***"
                      dc!"*********************************************"

  o = Object3D.make(
    [  0,   8,   0], # 0
    [-40,  -6,  30], # 1
    [-15,  -8,  30], # 2
    [ 15,  -8,  30], # 3
    [ 40,  -6,  30], # 4
    [ 25,   7,  30], # 5
    [  0,   8,  30], # 6
    [-25,   7,  30], # 7
    [-40,  -6,   5], # 8
    [ 40,  -6,   5], # 9
    [-15,  -8, -30], #10
    [ 15,  -8, -30], #11
    # [  0,  -8, -30], #12
    # [  0,  -8, -40], #13
  ) do |obj|
    obj.scale!(1.35)
    obj.face!(0, 11, 10, decoration: wire_front)
    obj.face!(0, 10,  7)
    obj.face!(0,  5, 11)
    obj.face!(0,  7,  6)
    obj.face!(0,  6,  5)
    obj.face!(7, 10,  8)
    obj.face!(1,  7,  8)
    obj.face!(5,  9, 11)
    obj.face!(4,  9,  5)
    obj.face!(2,  1,  8, 10, decoration: wire_slot_rt)
    obj.face!(4,  3, 11, 9,  decoration: wire_slot_lt)
    obj.face!(3,  2, 10, 11, decoration: wire_cannons)
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
    endvs             db   -128
                      draw_poly_wire vs[ 0], vs[ 1], vs[ 2]
                      draw_poly_wire vs[ 3], vs[ 4], vs[ 5]
                      draw_poly_wire vs[ 6], vs[ 7], vs[ 8], vs[ 9]
                      draw_poly_wire vs[10], vs[11], vs[12], vs[13], tail_call: true
    endwires          label
  end

  ns :wire_front do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(0,
                        [ -7,  10],
                        [  7,  10],
                        [  9,   3],
                        [ -9,   3],
                        [ -6,  13],
                        [  6,  13],
                        align: :bc))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3]
                      draw_wire vs[4], vs[5], tail_call: true
    endwires          label
  end

  ns :wire_slot_rt do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(9,
                        [-14, -25],
                        [-14,   0],
                        [ 9,  25],
                        [ 9, -25],
                        align: :ba))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_slot_lt do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(10,
                        [-14,  25],
                        [-14,   0],
                        [ 9, -25],
                        [ 9,  25],
                        align: :ab))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], tail_call: true
    endwires          label
  end

  ns :wire_cannons do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(11,
                        [ 10, 18],
                        [ 10, 34],
                        [-10, 18],
                        [-10, 34],
                        align: :ba))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3], tail_call: true
    endwires          label
  end

                      dc!
                      dc!"*********************************************"
                      dc!"***              STATION 3D               ***"
                      dc!"*********************************************"
  o = Object3D.make(
    [ 35, -40,  35], #0
    [  0, -40,  50], #1
    [-35, -40,  35], #2
    [-50, -40,   0], #3
    [-35, -40, -35], #4
    [  0, -40, -50], #5
    [ 35, -40, -35], #6
    [ 50, -40,   0], #7
    [ 35,  32, -35], #8
    [ 50,  32,   0], #9
    [  0,  32, -50], #10
    [-35,  32, -35], #11
    [-50,  32,   0], #12
    [-35,  32,  35], #13
    [  0,  32,  50], #14
    [ 35,  32,  35], #15
    [ 27,  40, -27], #16
    [ 38,  40,   0], #17
    [  0,  40, -38], #18
    [-27,  40, -27], #19
    [-38,  40,   0], #20
    [-27,  40,  27], #21
    [  0,  40,  38], #22
    [ 27,  40,  27], #23
  ) do |obj|
    obj.scale!(1.0)
    obj.face!(0, 1, 2, 3, 4, 5, 6, 7, surface: [2, 5, 6], decoration: wire_serial)
    obj.face!(7, 6, 8, 9,   decoration: wire_hull1)
    obj.face!(6, 5, 10, 8,  decoration: wire_windows1)
    obj.face!(5, 4, 11, 10, decoration: wire_hull2)
    obj.face!(4, 3, 12, 11, decoration: wire_windows2)
    obj.face!(3, 2, 13, 12, decoration: wire_hull3)
    obj.face!(2, 1, 14, 13, decoration: wire_windows3)
    obj.face!(1, 0, 15, 14, decoration: wire_hull4)
    obj.face!(0, 7, 9, 15,  decoration: wire_windows4)
    obj.face!(9, 8, 16, 17)
    obj.face!(8, 10, 18, 16)
    obj.face!(10, 11, 19, 18)
    obj.face!(11, 12, 20, 19)
    obj.face!(12, 13, 21, 20)
    obj.face!(13, 14, 22, 21)
    obj.face!(14, 15, 23, 22)
    obj.face!(15, 9, 17, 23)
    obj.face!(17, 16, 18, 19, 20, 21, 22, 23, surface: [19, 20, 23], decoration: wire_entrance)
    obj.to_prog(self, :station)
  end
                      dc!
                      dc!"*********************************************"
                      dc!"***           STATION 3D DECALS           ***"
                      dc!"*********************************************"
  ns :wire_entrance do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(17,
                        [-26, 10],
                        [ 26, 10],
                        [ 26,-10],
                        [-26,-10],
                        [-15, 9],
                        [-15,-9],
                        [ 15, 9],
                        [ 15,-9]))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3]
                      draw_wire vs[4], vs[5]
                      draw_wire vs[6], vs[7], tail_call: true
    endwires          label
  end

  ns :wire_serial do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(0,
                        [-30,  12], [-20, 12], [-30,-12], [-20,-12], # Z
                        [-10,  12], [  0,-12], [  0, 12], [-10,-12], # X
                        [  4,   0], [ 14,  0],                       # -
                        [ 25,  12], [ 25,-12],                       # 1
                        align: :ba))
    endvs             db   -128
                      draw_poly_wire vs[0], vs[1], vs[2], vs[3], closed: false
                      draw_wire vs[4], vs[5]
                      draw_wire vs[6], vs[7]
                      draw_wire vs[8], vs[9]
                      draw_wire vs[10], vs[11], tail_call: true
    endwires          label
  end

  ns :wire_hull1 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(1,
                        [  0, 20], [  0,-20],
                        [-10, 15], [  0, 15],
                        [  0,-10], [ 10,-10],
                      ))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3]
                      draw_wire vs[4], vs[5], tail_call: true
    endwires          label
  end

  ns :wire_windows1 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(2,
                        [-15,  5], [  5,  5], [ 10,  5], [ 15,  5],
                        [-10, -5], [  0, -5], [  5, -5], [ 10, -5],
                      align: :cb))
    endvs             db   -128
                      particles vs[0], vs[1], vs[2], vs[3],
                                vs[4], vs[5], vs[6], vs[7], tail_call: true
    endparticles      label
  end

  ns :wire_hull2 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(3,
                        [  0, 15], [  0,-15],
                        [  0,  5], [ 10,  5],
                        [ -5,-15], [  0,-15],
                      ))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3]
                      draw_wire vs[4], vs[5], tail_call: true
    endwires          label
  end

  ns :wire_windows2 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(4,
                        [-10,  5], [ -5,  5], [  5,  5], [ 10,  5],
                        [-15, -5], [  0, -5], [  5, -5], [ 10, -5],
                      align: :cb))
    endvs             db   -128
                      particles vs[0], vs[1], vs[2], vs[3],
                                vs[4], vs[5], vs[6], vs[7], tail_call: true
    endparticles      label
  end

  ns :wire_hull3 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(5,
                        [  0, 12], [  0,-12],
                        [-19,  5], [  5,  5],
                        [  0,-12], [ 25,-12],
                      align: :bc))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3]
                      draw_wire vs[4], vs[5], tail_call: true
    endwires          label
  end

  ns :wire_windows3 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(6,
                        [-10,  5], [ -5,  5], [  0,  5], [ 10,  5],
                        [-10, -5], [ -5, -5], [ 10, -5], [ 15, -5],
                      align: :cb))
    endvs             db   -128
                      particles vs[0], vs[1], vs[2], vs[3],
                                vs[4], vs[5], vs[6], vs[7], tail_call: true
    endparticles      label
  end

  ns :wire_hull4 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(7,
                        [-22, 13], [-22,  0],
                        [-22,  5], [  5,  5],
                        [  5,  9], [  5,-13],
                      align: :bc))
    endvs             db   -128
                      draw_wire vs[0], vs[1]
                      draw_wire vs[2], vs[3]
                      draw_wire vs[4], vs[5], tail_call: true
    endwires          label
  end

  ns :wire_windows4 do
    vs                data Vertex, *Vertex.make_many( *o.map_vec2_face(8,
                        [-15,  5], [ -5,  5], [  0,  5], [  5,  5],
                        [-10, -5], [ -5, -5], [  0, -5], [ 10, -5],
                      align: :cb))
    endvs             db   -128
                      particles vs[0], vs[1], vs[2], vs[3],
                                vs[4], vs[5], vs[6], vs[7], tail_call: true
    endparticles      label
  end


  endsolids label

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
  data_end  label
end

include ZXLib

quat3d = QFace3D.new 0x8100 # keep SP in uncontended RAM
puts quat3d.debug
puts "="*68
solids = %w[cube octahedron boing diamond elite station]
decals = [
  %w[wire_psi wire_lambda wire_pi wire_xi wire_delta wire_phi],
  %w[wire_cisters_1 wire_cisters_2 wire_cisters_3 wire_cisters_4 wire_cisters_5
     wire_cisters_6 wire_cisters_7 wire_cisters_8],
  %w[wire_ansuz wire_algiz wire_othila wire_fehu wire_mannaz wire_inguz wire_nauthiz],
  %w[wire_rune wire_glow1 wire_glow2 wire_glow3 wire_glow4 wire_glow5 wire_glow6 wire_glow7 ],
  %w[wire_engines wire_front wire_slot_rt wire_slot_lt wire_cannons],
  %w[wire_entrance wire_serial wire_hull1 wire_hull2 wire_hull3 wire_hull4
     wire_windows1 wire_windows2 wire_windows3 wire_windows4],
]
(%w[start 
    start.draw_loop.draw_faces_a
    start.draw_loop.course_to_rot
    start.draw_loop.apply_rotation
    draw plot
    apply_matrix_to_vertices.apply_matrix_a] +
 solids.map.with_index {|n,i| [n] + decals[i] }.flatten +
 %w[
   endsolids
   data_end
   sincos]
).each do |label|
  puts "#{label.ljust(40)}: 0x#{quat3d[label].to_s(16).upcase} - #{quat3d[label]}, size: #{quat3d['+'+label]}"
end
puts "="*68
puts "      NAME      : VERTICES FACETS EDGES DECALS D-VS D-WIRES"
solids.each_with_index do |label, sindex|
  vscount = (quat3d["#{label}.endvs"] - quat3d["#{label}.vs"]) / quat3d["+#{label}.vs"]
  fcount = (0..).find { |i| quat3d["#{label}.face#{i+1}"].nil? }
  ecount = (quat3d["#{label}.endsolid"] - quat3d["#{label}.edge1"]) / quat3d["+#{label}.edge1"]
  decors = decals[sindex]
  dvscounts = decors.map {|n| (quat3d["#{n}.endvs"] - quat3d["#{n}.vs"]) / quat3d["+#{n}.vs"]}
  dwcounts = decors.map do |n|
    if endwires = quat3d["#{n}.endwires"]
      (endwires - quat3d["#{n}.endvs"] - quat3d["+#{n}.endvs"]) / 10
    else
      0
    end
  end
  puts "Solid #{label.ljust 10}: #{vscount.to_s.rjust 8} #{fcount.to_s.rjust 6} #{ecount.to_s.rjust 5} #{decors.length.to_s.rjust 6} #{dvscounts.sum.to_s.rjust 4} #{dwcounts.sum.to_s.rjust 7}"
end
puts "="*68
puts "CODE          size: #{quat3d[:solids] - quat3d[:start]}"
puts "SOLID OBJECTS size: #{quat3d[:endsolids] - quat3d[:solids]}"
puts "DATA          size: #{quat3d[:data_end] - quat3d[:endsolids]}"
puts
raise "Code too large! #{quat3d[:data_end] - quat3d[:start]}" if quat3d[:data_end] > quat3d[:sincos]

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
