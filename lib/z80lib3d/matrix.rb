require 'z80'
require 'z80/math_i'

module Z80Lib3D
  ##
  # =Z80Lib3D::Matrix3D
  # 
  # 3D matrix related Macros.
  class Matrix3D
    ##
    # =Z80Lib3D::Matrix3D::Macros
    # 
    # Macros for applying 3D matrix to vertices.
    module Macros
      include Z80::MathInt::Macros
      ##
      # Applies a transformation matrix to vertices and calculates screen coordinates (xp, yp) for each one.
      #
      #   X = M[0]*vx + M[1]*vy + M[2]*vz
      #   Y = M[3]*vx + M[4]*vy + M[5]*vz
      #   Z = M[6]*vx + M[7]*vy + M[8]*vz
      #
      #   XP =  ((X << persp_dshift) / (Z + scrz0)) + scrx0
      #   YP = -((Y << persp_dshift) / (Z + scrz0)) + scry0
      #
      # Vertices list must be terminated with a (-128) octet and must not be empty.
      #
      # Vertices are represented by Primitives::Vertex struct.
      #
      # Matrix is represented by Primitives::Matrix struct.
      #
      # Each matrix element should be a 16-bit fixed point twos complement signed number with an 8-bit
      # fractional part.
      #
      # <b>NOTE</b>: The routine uses stack pointer to read matrix data, so disable interrupts first.
      #
      # +matrix+:: An address of matrix data as a label or a pointer.
      #
      # Options:
      # * +vertices+::     An address of object vertices or +hl+.
      # * +scrx0+::        A X coordinate adjustment used for calculating screen coordinates.
      # * +scry0+::        A Y coordinate adjustment used for calculating screen coordinates.
      # * +scrz0+::        A Z coordinate adjustment used for calculating screen coordinates.
      # * +persp_dshift+:: A screen coordinates perspective adjustment: 0-7.
      #
      # When the routine completes:
      #
      # * +hl+ will point to an address immediately following the vertices end marker.
      # * +sp+ will point to an address immediately following the matrix.
      #
      # Notable run-time labels:
      #
      # * +matrix_p+ can be used to change the address of the +matrix+.
      # * +adjust_x_p+ points to +scrx0+.
      # * +adjust_y_p+ points to +scry0+.
      # * +adjust_z_p+ points to +scrz0+.
      # * +matrix_loop+ enter to skip reading first Z coordinate, can be used to pre-check if vertices are empty.
      #   Expects Z coordinage already in +b+ and +hl+ pointing to the next coordinate.
      #
      # Modifies: +sp+, +af+, +af'+, +hl+, +bc+, +de+, +hl'+, +bc'+, +de'+.
      def apply_matrix(matrix, vertices:hl, scrx0:128, scry0:128, scrz0:128, persp_dshift:7, optimize: :time)
        raise ArgumentError, "apply_matrix: invalid arguments" unless [scrx0, scry0, scrz0].all?{|l| direct_address?(l) }
        mx_optimize = case optimize
        when :time_alt then :time
        when :unroll_alt then :unroll
        else
          optimize
        end
        check0_far = (optimize == :unroll || optimize == :unroll_alt)
        isolate do
                          ld   hl, vertices unless vertices==hl
                          ld   b, [hl]                # b: z
                          inc  hl                     # hl: -> y
          matrix_loop     ld   sp, matrix
          matrix_p        as matrix_loop + 1          # a label pointer to a matrix argument
                          ld   d, [hl]                # d: y
                          inc  hl                     # hl: -> x
                          ld   e, [hl]                # e: x
                          # af: x = qxx*x + qxy*y + qxz*z
                          apply_matrix_row e, d, b, optimize:mx_optimize
                          ex   af, af                 # a': new x
                          # af: y = qyx*x + qyy*y + qyz*z
                          apply_matrix_row e, d, b, optimize:mx_optimize
                          ld   c, a                   # c: new y
                          # af: z = qzx*x + qzy*y + qzz*z
                          apply_matrix_row e, d, b, optimize:mx_optimize
                          inc  hl                     # hl: -> xp
          # calculate xp, yp
                          exx
          adjust_z_a      add  scrz0            # a: z + 128 (make Z positive)
          adjust_z_p      as adjust_z_a + 1     # a label pointer to scrz0
                          ld   c, a             # c: z
          # xp = ((x << PERSP_DSHIFT)/(z + 128)) + scrx0
                          ex   af, af           # a: x
                          anda a                # clear CF, change SF
                          jp   P, pos_x
                          neg                   # CF: 1
          pos_x           ld   e, a             # x
                          ex   af, af           # f': CF: z sign
                           # (x << PERSP_DSHIFT) / z
                          sll8_16 persp_dshift, d, e # de: x << PERSP_DSHIFT
                          divmod16_8 d, e, c, check0:x_overflow, check0_far:check0_far, check1:false, ignore_cf:true, optimize:optimize
                          # jr   C, x_overflow    # z=0
                          ld   a, d
                          anda a
                          jr   NZ, x_overflow   # de >= 256
                                                # de < 256
                          ex   af, af           # f: CF: sign
          adjust_x_a      ld   a, scrx0         # x to screen coordinates
          adjust_x_p      as adjust_x_a + 1     # a label pointer to scrx0
                          jr   C, x_negative

          x_positive      add  e                # x  >= 0
                          jr   NC, skip_neg_x   # xp <  256
                                                # xp >= 256
          x_overflow      ld   a, 255           # store xp=255, yp=255 (out of screen)
                          exx
                          ld   [hl], a
                          inc  hl
                          jp   skip_overflow_x

          x_negative      sub  e                # x  <  0: x to screen coordinates
                          jr   C, x_overflow    # xp <  0
          skip_neg_x      exx
                          ld   [hl], a          # store xp
                          inc  hl
          # yp = -((y << PERSP_DSHIFT)/(z + 128)) + scry0
                          ld   a, c             # y
                          exx
                          anda a
                          jp   P, pos_y
                          neg
          pos_y           ld   e, a             # y
                          ex   af, af           # f': CF: sign
                          # (y << PERSP_DSHIFT) / z
                          sll8_16 persp_dshift, d, e # de: y << PERSP_DSHIFT
                          divmod16_8 d, e, c, check0:y_overflow, check0_far:check0_far, check1:false, ignore_cf:true, optimize:optimize
                          # jr   C, y_overflow    # z=0
                          ld   a, d
                          anda a
                          jr   NZ, y_overflow   # de >= 256
                                                # de < 256
                          ex   af, af           # f: CF: sign
          adjust_y_a      ld   a, scry0         # y  >= 0
          adjust_y_p      as adjust_y_a + 1        # a label pointer to scry0
                          jr   C, y_negative

          y_positive      sub  e                # y to screen coordinates
                          jr   NC, skip_neg_y   # yp <  256

          y_overflow      ld   a, 255           # yp >= 256
                          jr   skip_neg_y

          y_negative      add  e                # y  <  0: y to screen coordinates
                          jr   C, y_overflow    # yp <  0
          skip_neg_y      exx
          skip_overflow_x ld   [hl], a          # store yp
                          inc  hl               # -> z
                          ld   a, [hl]          # a: z
                          inc  hl
                          cp   -128             # is end marker?
                          ld   b, a             # b: z
                          jp   NZ, matrix_loop
        end
      end
      ##
      # Applies a row of the transformation matrix to a 3D vector.
      #
      # Each matrix element should be a 16-bit fixed point twos complement signed number with an 8-bit
      # fractional part.
      #
      # The address of a matrix row should be present in the stack pointer (+sp+) register.
      #
      # A rounded result is returned in the +accumulator+ as a twos complement signed 8-bit integer:
      #
      #   A = INT(([SP]*x + [SP+2]*y + [SP+4]*z + 128) / 256)
      #
      # On completion +sp+ points to the next matrix row elements (+6 bytes).
      #
      # +x+:: A signed 8-bit integer representing x coordinate as an 8-bit register or an address.
      # +y+:: A signed 8-bit integer representing y coordinate as an 8-bit register or an address.
      # +z+:: A signed 8-bit integer representing z coordinate as an 8-bit register or an address.
      #
      # Options:
      # * +tt+:: A 16-bit temporary register +de+ or +bc+ from the alternative register set.
      # * +t+:: An 8-bit temporary register from the alternative register set.
      #
      # Modifies: +sp+, +af+, +hl'+, +bc'+, +de'+, preserves +x+, +y+, +z+.
      def apply_matrix_row(x, y, z, optimize: :time)
        raise ArgumentError, "apply_matrix_row: invalid arguments" if y == a or z == a
        isolate do
                      apply_matrix_element x, clrhl: true,  optimize:optimize #  qxx*x
                      exx
                      apply_matrix_element y, clrhl: false, optimize:optimize # +qxy*y
                      exx
                      apply_matrix_element z, clrhl: false, optimize:optimize # +qxz*z

                      xor  a
                      sla  l
                      adc  h                                 # (sum+128)/256

                      exx
        end
      end
      ##
      # Applies a matrix element of the transformation matrix to a scalar.
      #
      # Matrix element should be a 16-bit fixed point twos complement signed number with an 8-bit
      # fractional part.
      #
      # The address of a matrix element should be present in the stack pointer (+sp+) register.
      #
      # <b>NOTE</b>: The registers +hl+|+bc+|+de+ are being swapped with alternatives before the
      # multiplication, however +x+ is read into the +accumulator+ before swapping.
      #
      # A result is returned in the +hl+ as a twos complement signed 16-bit integer:
      #
      #   HL <-> HL'
      #   HL = [SP]*x
      #   or
      #   HL = HL + [SP]*x
      #
      # On completion +sp+ points to the next matrix element (+2 bytes).
      #
      # +x+:: A signed 8-bit integer representing scalar as an 8-bit register or an address.
      #
      # Options:
      # * +clrhl+:: Whether the result should be set (+true+) or added to the previous value (+false+).
      #
      # Modifies: +sp+, +af+, +hl'+, +bc'+, +de'+, optionally preserves +x+, swaps registers.
      def apply_matrix_element(x, clrhl:false, optimize: :time)
        isolate do |eoc|
                      ld   a, x unless x == a
                      exx
                      pop  bc     # bc: qxx = iiiiiiiiffffffff
          if optimize == :size
            multiply  mul8_signed(b, c, a, tt:bc, t:e, clrhl:clrhl, double:false, optimize: :size)
          else
            unless clrhl
                      ex   de, hl # de: sum
            end
            multiply  mul16_signed(b, c, a, tt:bc, optimize:optimize)
            unless clrhl
                      add  hl, de # hl: add previous value
            end
          end
        end
      end
    end

    include Z80
  end
end
