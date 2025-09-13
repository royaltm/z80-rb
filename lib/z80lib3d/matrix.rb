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
      # * +scrx0+::        A value added to the screen X coordinate.
      # * +scry0+::        A value added to the screen Y coordinate.
      # * +scrz0+::        A value added to the Z coordinate.
      # * +persp_dshift+:: Perspective adjustment (D): 0-7. X and Y coordinates are multiplied by 2
      #                    to the power of D (bitwise left shifted) before calculating the screen X,Y.
      # * +optimize+:: Optimization options: +:size+, +:time+, +:unroll+, +:time_alt+, +:unroll_alt+.
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
        raise ArgumentError, "apply_matrix: invalid persp_dshift argument" unless (0..7).include?(persp_dshift)
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
      # * +optimize+:: Optimization options: +:size+, +:time+ or +:unroll+.
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
      # * +optimize+:: Optimization options: +:size+, +:time+ or +:unroll+.
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
      ## 
      # Converts a given course (yaw,pitch,roll) to a target Primitives::Rotation.
      #
      # +rotation+:: A target Primitives::Rotation object label.
      # +yaw+:: An 8-bit angle normalized in [0, 255] range.
      # +pitch+:: An 8-bit angle normalized in [0, 255] range.
      # +roll+:: An 8-bit angle normalized in [0, 255] range.
      #
      # Options:
      # * +sincos+:: An address of a populated Primitives::SinCos table, aligned to 256 bytes.
      # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored.
      #               Otherwise +sp+ will be clobbered.
      #               +save_sp+ can also be set to +:restore_only+ symbol, in this case the +SP+ value
      #               needs to be stored in the memory pointed by the sub-label +restore_sp_p+.
      # Modifes +sp+, +af+, +hl+, +de+.
      def course_to_rotation(rotation, yaw=a, pitch=b, roll=c, sincos:self.sincos, save_sp:true)
        raise ArgumentError, "course_to_rotation: :save_sp requires :restore_sp" if save_sp && !restore_sp
        isolate do
                      ld    [restore_sp_p], sp if save_sp && save_sp != :restore_only
          select(sincos & 0x00FF, &:zero?).then do |_|
                      ld    de, (sincos & 0xFF00)|0b11111100
          end.else do
              raise ArgumentError, "sincos address must be aligned to 256 bytes"
          end
          [yaw,   rotation.yaw,
           pitch, rotation.pitch,
           roll,  rotation.roll].each_slice(2) do |angle, target|
                      copy_sincos_from_angle(target, angle, sincos:d, mask:e)
          end
          if save_sp
            restore_a     ld    sp, 0
            restore_sp_p  as restore_a + 1
          end
        end
      end
      ##
      # Retrieves and copies sin/cos values for an +angle+ to a +target+ Primitives::SinCos object.
      #
      # +target+:: A target Primitives::SinCos object label.
      # +angle+:: An 8-bit angle normalized in [0, 255] range.
      #
      # Options:
      # * +sincos+:: An address of a populated Primitives::SinCos table, aligned to 256 bytes.
      # * +mask+:: An optional 8-bit register holding preloaded mask value: +0xFC+ (+0b11111100+).
      #
      # Modifes +sp+, +af+, +hl+.
      def copy_sincos_from_angle(target, angle=a, sincos:self.sincos, mask:nil)
        isolate do
                      ld    a, angle unless angle == a
                      sincos_from_angle sincos, h, l, mask:mask
                      ld    sp, hl
                      pop   hl
                      ld    [target.sin], hl
                      pop   hl
                      ld    [target.cos], hl
        end
      end
      ##
      # Converts a Primitives::Rotation data into a Primitives::Matrix object.
      #
      # +matrix+:: A target Primitives::Matrix object label.
      # +rotation+:: A source Primitives::Rotation object label.
      #
      # Options:
      # +inline_mul+:: A boolean indicating whether to inline multiplication routine.
      # +subroutine+:: A boolean indicating whether to create a subroutine.
      # +optimize+:: A multiplication optimization: +:size+, +:time+ or +:unroll+.
      #
      # Modifies: +af+, +af'+, +hl+, +bc+, +de+. Stack depth: 6 bytes, 4 if +:inline_mul+ is +true+.
      def rotation_to_matrix(matrix, rotation, inline_mul:false, subroutine:true, optimize: :time)
        raise ArgumentError, "rotation_to_matrix: invalid :optimize option" if optimize == :compact 
        # cos(a)*cos(b)
        # cos(a)*cos(c)
        # cos(a)*sin(b)=(cos(a)*sin(b))
        # cos(a)*sin(c)
        # cos(b)*cos(c)
        # cos(b)*sin(c)
        # sin(a)*cos(b)
        # sin(a)*cos(c)
        # sin(a)*sin(b)=(sin(a)*sin(b))
        # sin(a)*sin(c)
        # (sin(a)*sin(b))*sin(c)
        # (sin(a)*sin(b))*cos(c)
        # (cos(a)*sin(b))*sin(c)
        # (cos(a)*sin(b))*cos(c)
        #
        # [cos(a)*cos(b), cos(a)*sin(b)*sin(c)-sin(a)*cos(c), cos(a)*sin(b)*cos(c)+sin(a)*sin(c),
        #  sin(a)*cos(b), sin(a)*sin(b)*sin(c)+cos(a)*cos(c), sin(a)*sin(b)*cos(c)-cos(a)*sin(c),
        #  -sin(b)      , cos(b)*sin(c)                     , cos(b)*cos(c)]
        multiply = if inline_mul
          proc do
            isolate do |eoc|    # b|hl = de * bc
                          sra   b # 0 -> 0 1 -> 0(C) -1 -> -1(C)
                          jr    C, maybe_mneg
                          ld    b, d # limited d: 0, 1, -1
                          sra   b    # sign_extend(b, d)
                          ld    a, c
                          anda  a    # test zero
                          jp    NZ, mult16
              m_zero      ld    b, a # clear sign
                          ld    h, a
                          ld    l, a
                          jr    eoc
              m_is_256    ld    l, 0 # b|h|l = d|e|0
                          ld    h, e
                          ld    b, d
                          jr    eoc
              maybe_mneg  jr    Z, m_is_256 # m == 256 (b == 0x01)
                          neg16 d, e        # m < 0
                          sign_extend(b, a)
                          xor   a
                          sub   c          # a: -m
                          jr    NC, m_is_256 # m == 256
              mult16      mul16(d, e, a, tt:de, optimize:optimize)
            end
            # isolate do |eoc|  # b|hl = de * bc
            #             ld    a, b
            #             dec   a     # b == 1 ?
            #             jp    NZ, k_is_no_1
            #   k_is_1    ld    b, d
            #             ld    h, e
            #             ld    l, a  # a: 0
            #             jp    eoc   # b|hl = d|e|0
            #   m_is_1    ld    h, c
            #             ld    l, d  # d: 0
            #             jp    eoc   # b|hl = b|c|0
            #   km_is_1   ld    b, 1  # b|hl = 1|0|0
            #             jp    eoc
            #   k_is_no_1 dec   d     # d == 1 ?
            #             jr    Z, m_is_1
            #             inc   d     # d: 0 (+)  !0 (-)
            #             mul_signed9(b, c, e, tt:de, m_neg_cond:NZ, k_overflow:km_is_1, optimize:optimize)
            # end
          end
        else
          proc do
                      call  multiply_de_bc
          end
        end
        isolate do |eoc|
                      ld    hl, [rotation.sin_b]
                      neg16 h, l
                      ld    [matrix.zx], hl
          [rotation.cos_a, rotation.cos_b, matrix.xx,
           rotation.sin_a, rotation.cos_b, matrix.yx,
           rotation.cos_b, rotation.sin_c, matrix.zy,
           rotation.cos_b, rotation.cos_c, matrix.zz].each_slice(3) do |k_pt, m_pt, target|
                      ld    bc, [k_pt]
                      ld    de, [m_pt]
                      multiply.call         # b|hl: k_pt * m_pt
                      ld    c, h
                      ld    [target], bc    # cos_a * cos_b
          end
          [rotation.cos_a, rotation.sin_c,  rotation.sin_a, rotation.cos_c, matrix.xy,  matrix.xz,
           rotation.sin_a, rotation.cos_c,  rotation.cos_a, rotation.sin_c, matrix.yz,  matrix.yy
          ].each_slice(6) do |k_pt, m_pt, n_pt, j_pt, target_sub, target_add|
                      ld    bc, [k_pt]
                      ld    de, [rotation.sin_b]
                      multiply.call          # b|hl: cos_a * sin_b
                      ld    c, h
                      push  bc               # [sp]: cos_a * sin_b
                      ld    de, [m_pt]
                      multiply.call          # b|hl: cos_a * sin_b * sin_c
                      ld    a, b
                      ex    af, af           # a':   msb(cos_a * sin_b * sin_c)
                      push  hl               # [sp]: lsb(cos_a * sin_b * sin_c)
                      ld    bc, [n_pt]
                      ld    de, [j_pt]
                      multiply.call          # b|hl: sin_a * cos_c
                      ex    de, hl           # b|de: sin_a * cos_c
                      ex    af, af           # a:    msb(cos_a * sin_b * sin_c)
                      pop   hl               # a|hl: cos_a * sin_b * sin_c
                      anda  a
                      sbc   hl, de
                      sbc   a, b             # a|hl: cos_a * sin_b * sin_c - sin_a * cos_c
                      ld    l, h
                      ld    h, a
                      ld    [target_sub], hl # cos_a * sin_b * sin_c - sin_a * cos_c

                      pop   bc               # bc: cos_a * sin_b
                      ld    de, [j_pt]
                      multiply.call          # b|hl: cos_a * sin_b * cos_c
                      ld    a, b
                      ex    af, af           # a':   msb(cos_a * sin_b * cos_c)
                      push  hl               # [sp]: lsb(cos_a * sin_b * cos_c)
                      ld    bc, [n_pt]
                      ld    de, [m_pt]
                      multiply.call          # b|hl: sin_a * sin_c
                      ex    de, hl           # b|de: sin_a * sin_c
                      ex    af, af           # a:    msb(cos_a * sin_b * cos_c)
                      pop   hl               # a|hl: cos_a * sin_b * cos_c
                      add   hl, de
                      adc   a, b             # a|hl: cos_a * sin_b * cos_c + sin_a * sin_c
                      ld    l, h
                      ld    h, a
                      ld    [target_add], hl # cos_a * sin_b * cos_c + sin_a * sin_c
          end
                      ret if subroutine
          unless inline_mul
                      jp    eoc unless subroutine

            isolate :multiply_de_bc do |eoc|  # b|hl = de * bc, bc and de in range: (-256..256)
                          sra   b # 0 -> 0 1 -> 0(C) -1 -> -1(C)
                          jr    C, maybe_mneg
                          ld    b, d # limited d: 0, 1, -1
                          sra   b    # sign_extend(b, d)
                          ld    a, c
                          anda  a    # test zero
              if optimize == :size
                          jr    NZ, mult16
              else
                          jr    Z, m_zero
                          mul16(d, e, a, tt:de, optimize:optimize)
                          ret
              end
              m_zero      ld    b, a # clear sign
                          ld    h, a
                          ld    l, a
                          ret
              m_is_256    ld    l, 0 # b|h|l = d|e|0
                          ld    h, e
                          ld    b, d
                          ret
              maybe_mneg  jr    Z, m_is_256 # m == 256 (b == 0x01)
                          neg16 d, e        # m < 0
                          sign_extend(b, a)
                          xor   a
                          sub   c          # a: -m
                          jr    NC, m_is_256 # m == 256
              mult16      mul16(d, e, a, tt:de, optimize:optimize)
                          ret
            end
            # isolate :multiply_de_bc do # b|hl = de * bc, bc and de in range: (-256..256)
            #             ld    a, b
            #             dec   a         # b == 1 ?
            #             jr    Z, k_is_1
            #             dec   d         # d == 1 ?
            #             jr    Z, m_is_1
            #             inc   d         # d: 0 (+)  !0 (-)
            #             mul_signed9(b, c, e, tt:de, m_neg_cond:NZ, k_overflow:km_is_1, optimize: :size)
            #             ret   # b|hl
            #   k_is_1    ld    b, d
            #             ld    h, e
            #             ld    l, a      # a: 0
            #             ret   # b|hl = d|e|0
            #   m_is_1    ld    h, c
            #             ld    l, d      # d: 0
            #             ret   # b|hl = b|c|0
            #   km_is_1   ld    b, 1
            #             ret   # b|hl = 1|0|0
            # end
          end
        end
      end
    end

    include Z80
  end
end
