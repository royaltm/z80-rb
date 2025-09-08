require 'z80'
require 'z80/utils/sincos'

module Z80Lib3D
  ##
  # =Z80Lib3D::Primitives
  # 
  # 3D primitives for Z80 program data.
  #
  # * Vector
  # * Vertex
  # * Matrix
  # * Rotation
  module Primitives
    SinCos = Z80::Utils::SinCos::SinCos
    ##
    # =Z80Lib3D::Primitives::Vector
    # 
    # A 3 element vector of 8-bit signed integers:
    #
    # * +z+: +byte+
    # * +y+: +byte+
    # * +x+: +byte+
    class Vector < Z80::Label
      z   byte
      y   byte
      x   byte

      ## A struct to represent Vector instances
      S = to_struct
    end
    ##
    # =Z80Lib3D::Primitives::Vertex
    # 
    # A 3 element Vector and 2 screen coordinates:
    #
    # * +vec+: Vector
    # * +xp+:  +byte+
    # * +yp+:  +byte+
    # * +scr+: +word+ alias of +yp+|+xp+
    class Vertex < Z80::Label
      vec Vector
      xp  byte
      yp  byte
      scr xp word

      ## A struct to represent Vertex instances
      S = to_struct

      ## Creates a Vertex data argument for given coordinates.
      def Vertex.make(x, y, z, scrx0:128, scry0:128, scrz0:128, persp_dshift:7)
        x, y, z = x.round, y.round, z.round
        scrxmax = scrx0 * 2 - 1
        scrymax = scry0 * 2 - 1
        xp =  ((x << persp_dshift) / (z + scrz0)) + scrx0
        yp = -((y << persp_dshift) / (z + scrz0)) + scry0
        if xp < 0 || xp > scrxmax || yp < 0 || yp > scrymax
          raise ArgumentError, "Vertex.make: vertex screen coordinates out of bounds"
        end
        S.new(Vector::S.new(z, y, x), xp, yp)
      end

      ## Creates many Vertex data arguments from triplets: [x, y, z].
      def Vertex.make_many(*args, scrx0:128, scry0:128, scrz0:128, persp_dshift:7)
        args.map{|x, y, z| Vertex.make(x, y, z, scrx0:scrx0, scry0:scry0, scrz0:scrz0, persp_dshift:persp_dshift)}
      end

      ## Creates a re-scaled Vertex data arguments.
      def Vertex.scale(sc, vertex, scrx0:128, scry0:128, scrz0:128, persp_dshift:7)
        if vertex.is_a?(Array)
          vertex.map{|v| Vertex.scale(sc, v, scrx0:scrx0, scry0:scry0, scrz0:scrz0, persp_dshift:persp_dshift)}
        else
          vec = vertex.vec
          Vertex.make(vec.x*sc, vec.y*sc, vec.z*sc, scrx0:scrx0, scry0:scry0, scrz0:scrz0, persp_dshift:persp_dshift)
        end
      end
    end
    ##
    # =Z80Lib3D::Primitives::Matrix
    # 
    # A type representing a transformation matrix 3x3.
    #
    # Each matrix element should be a 16-bit fixed point twos complement signed number with an 8-bit
    # fractional part.
    #
    # * +xx+: +word+
    # * +xy+: +word+
    # * +xz+: +word+
    # * +yx+: +word+
    # * +yy+: +word+
    # * +yz+: +word+
    # * +zx+: +word+
    # * +zy+: +word+
    # * +zz+: +word+
    class Matrix < Z80::Label
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
    ##
    # =Z80Lib3D::Primitives::Rotation
    # 
    # A type representing 3D rotation.
    #
    # Each rotation element should be a 16-bit fixed point twos complement signed number with an 8-bit
    # fractional part.
    #
    # * +sin_a+: +word+ (yaw)
    # * +cos_a+: +word+ (yaw)
    # * +sin_b+: +word+ (pitch)
    # * +cos_b+: +word+ (pitch)
    # * +sin_c+: +word+ (roll)
    # * +cos_c+: +word+ (roll)
    class Rotation < Z80::Label
      sin_a  word
      cos_a  word
      yaw    sin_a SinCos
      sin_b  word
      cos_b  word
      pitch  sin_b SinCos
      sin_c  word
      cos_c  word
      roll   sin_c SinCos
    end
  end
end
