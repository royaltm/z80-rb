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
    # Various 3D matrix operations.
    module Macros
      include Z80::MathInt::Macros
      ##
      # Creates a routine that applies a transformation matrix to vertices and calculates
      # screen coordinates (xp, yp) for each vertex.
      #
      #   X = M[0] * v.x + M[1] * v.y + M[2] * v.z
      #   Y = M[3] * v.x + M[4] * v.y + M[5] * v.z
      #   Z = M[6] * v.x + M[7] * v.y + M[8] * v.z
      #
      #   v.xp =  ((X << persp_dshift) / (Z + scrz0)) + scrx0
      #   v.yp = -((Y << persp_dshift) / (Z + scrz0)) + scry0
      #
      # Vertices list must be terminated with an (-128) octet and must not be empty.
      #
      # The vector element values (z, y, x) of each vertex must be in range [-127, 127].
      #
      # Vertices are represented by the Primitives::Vertex object.
      #
      # Matrix is represented by the Primitives::Matrix object.
      #
      # Each matrix element should be a 16-bit fixed point twos complement signed number with
      # an 8-bit fractional part.
      #
      # <b>NOTE</b>: The routine uses stack pointer to read matrix data, so the interrupts
      # needs to be disabled before it is called.
      #
      # +matrix+:: An address of matrix data as a label or a pointer.
      #
      # Options:
      # * +vertices+:: An address of the first Primitives::Vertex object or +hl+ register
      #                pair, containing the address.
      # * +scrx0+::    A value added to the screen X coordinate.
      # * +scry0+::    A value added to the screen Y coordinate.
      # * +scrz0+::    A value added to the Z coordinate.
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
                          # af: x = M.xx*x + M.xy*y + M.xz*z
                          apply_matrix_row e, d, b, optimize:mx_optimize
                          ex   af, af                 # a': new x
                          # af: y = M.yx*x + M.yy*y + M.yz*z
                          apply_matrix_row e, d, b, optimize:mx_optimize
                          ld   c, a                   # c: new y
                          # af: z = M.zx*x + M.zy*y + M.zz*z
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
      # Creates a routine that applies a row (row.x, row.y, row.z) of the transformation matrix
      # to a 3D vector (x, y, z).
      #
      # Each matrix element should be a 16-bit, fixed point, twos complement signed number with
      # an 8-bit fractional part.
      #
      # Each vector coordinate should be a twos complement signed 8-bit integer.
      #
      # The address of a matrix row should be present in the stack pointer (+sp+) register.
      #
      # A rounded result is returned in the +accumulator+ as a twos complement signed 8-bit integer:
      #
      #   A = INT(([SP]*x + [SP+2]*y + [SP+4]*z + 128) / 256)
      #
      # On completion +sp+ points to the next matrix row elements (+6 bytes).
      #
      # +x+:: The +x+ coordinate as an 8-bit register or an address.
      # +y+:: The +y+ coordinate as an 8-bit register or an address.
      # +z+:: The +z+ coordinate as an 8-bit register or an address.
      #
      # Options:
      # * +optimize+:: Optimization options: +:size+, +:time+ or +:unroll+.
      #
      # Modifies: +sp+, +af+, +hl'+, +bc'+, +de'+, preserves +x+, +y+, +z+.
      def apply_matrix_row(x, y, z, optimize: :time)
        raise ArgumentError, "apply_matrix_row: invalid arguments" if y == a or z == a
        isolate do
                      apply_matrix_element x, clrhl: true,  optimize:optimize #  [SP+0]*x
                      exx
                      apply_matrix_element y, clrhl: false, optimize:optimize # +[SP+2]*y
                      exx
                      apply_matrix_element z, clrhl: false, optimize:optimize # +[SP+4]*z

                      xor  a
                      sla  l
                      adc  h                                 # (sum+128)/256

                      exx
        end
      end
      ##
      # Creates a routine that multiplies a value of an element of the transformation
      # matrix with a scalar.
      #
      # The matrix element should be a 16-bit, fixed point, twos complement signed number
      # with an 8-bit fractional part.
      #
      # The address of a matrix element should be present in the stack pointer (+sp+).
      #
      # _NOTE_: The registers +hl+|+bc+|+de+ are being swapped with the alternative
      # register bank before the multiplication, however +x+ is read into the +accumulator+
      # before the swapping is performed.
      #
      # A result is returned in the +hl+ register pair as a twos complement signed 16-bit
      # integer:
      #
      #   A = x
      #   HL <-> HL'
      #   if clrhl
      #     HL = [SP]*A
      #   else
      #     HL = HL + [SP]*A
      #   end
      #
      # On completion +sp+ points to the next matrix element (+2 bytes).
      #
      # +x+:: A signed 8-bit integer representing scalar as an 8-bit register or an address.
      #
      # Options:
      # * +clrhl+:: Whether the result of the multiplication should be returned (+true+) or
      #             added to the previous value in +hl'+ (+false+).
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
      # Creates a routine that converts each of the three angles (yaw, pitch, roll) to
      # a pair of sine and cosine values from these angles, and stores them in a +target+
      # Primitives::Rotation object.
      #
      # +rotation+:: An address label of a target Primitives::Rotation object.
      # +yaw+:: An 8-bit angle value normalized to the range: [0, 255].
      # +pitch+:: An 8-bit angle value normalized to the range: [0, 255].
      # +roll+:: An 8-bit angle value normalized to the range: [0, 255].
      #
      #   α = PI * angle / 128
      #   angle = α * 128 / PI
      #
      # Options:
      # * +sincos+:: An address of a populated Primitives::SinCos table, aligned to 256 bytes.
      # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and
      #               restored, otherwise +sp+ will be clobbered.
      #               +save_sp+ can be also set to +:restore_only+, in this case the value of
      #               the +sp+ register needs to be stored first in the memory pointed by the
      #               sub-label +restore_sp_p+.
      #
      # Modifes +sp+, +af+, +hl+, +de+.
      def course_to_rotation(rotation, yaw=a, pitch=b, roll=c, sincos:self.sincos, save_sp:true)
        raise ArgumentError, "course_to_rotation: invalid arguments" unless [yaw, pitch, roll].uniq.length == 3 &&
                                                        [yaw, pitch, roll].all? {|t| register?(t) && t.bit8? }
        sch, mask = [d, e, b, c].filter {|t| ![yaw, pitch, roll].include?(t) }
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
                      copy_sincos_from_angle(target, angle, sincos: sch, mask: mask)
          end
          if save_sp
            restore_a     ld    sp, 0
            restore_sp_p  as restore_a + 1
          end
        end
      end

      ##
      # Creates a routine that finds and copies the sine and cosine values of an +angle+,
      # saving them to the +target+ Primitives::SinCos object.
      #
      # +target+:: An address label of a target Primitives::SinCos object.
      # +angle+:: An 8-bit angle value normalized to the range: [0, 255].
      #
      #   α = PI * angle / 128
      #   angle = α * 128 / PI
      #
      # Options:
      # +sincos+:: An address of SinCos table, must be aligned to 256 bytes or an 8-bit register
      #            holding the MSB of the SinCos address. The LSB of +sincos+ address must be +0+.
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
      # Creates a routine that calculates a 3D rotation matrix from Rα (yaw), Rβ (pitch)
      # and Rγ (roll) matrices, and stores calculated values in a Primitives::Matrix object.
      #
      # The matrices (Rα, Rβ, Rγ) are built from an array of +word+ values:
      #
      #   (sin(α), cos(α), sin(β), cos(β), sin(γ), cos(γ))
      #
      # represented as 16-bit, fixed point, twos complement signed numbers with an 8-bit
      # fractional part, stored in a the Primitives::Rotation object.
      #
      #   M = [
      #     # xx,          xy,                                   xz
      #     cos(α)*cos(β), cos(α)*sin(β)*sin(γ) - sin(α)*cos(γ), cos(α)*sin(β)*cos(γ) + sin(α)*sin(γ),
      #     # yx,          yy,                                   yz
      #     sin(α)*cos(β), sin(α)*sin(β)*sin(γ) + cos(α)*cos(γ), sin(α)*sin(β)*cos(γ) - cos(α)*sin(γ),
      #     # zx,          zy,                                   zz
      #     -sin(β),       cos(β)*sin(γ),                        cos(β)*cos(γ)
      #   ]
      #
      # +matrix+:: An address label of a target Primitives::Matrix object.
      # +rotation+:: An address label of a source Primitives::Rotation object.
      #
      # Options:
      # +inline_mul+:: A boolean indicating whether to inline multiplication routines.
      # +subroutine+:: A boolean indicating whether to create a subroutine.
      # +optimize+:: A multiplication optimization: +:size+, +:time+ or +:unroll+.
      #
      # Modifies: +af+, +af'+, +hl+, +bc+, +de+. Stack depth: 6 bytes, 4 if +:inline_mul+ is +true+.
      def rotation_to_matrix(matrix, rotation, inline_mul:false, subroutine:true, optimize: :time)
        raise ArgumentError, "rotation_to_matrix: invalid :optimize option" if optimize == :compact
        multiply = if inline_mul
          proc do
            isolate do |eoc|    # b|hl = de * bc, bc and de in range: (-256..256)
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
            # isolate do |eoc|  # b|hl = de * bc, bc and de in range: (-256..256)
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
