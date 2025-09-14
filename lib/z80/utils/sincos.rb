# -*- coding: BINARY -*-
require 'z80'

module Z80
    module Utils
        ##
        # =Z80::Utils::SinCos - sine-cosine table routines.
        #
        # in Z80::Utils::SinCos::Macros
        #
        # ==Structs
        #
        # * Z80::Utils::SinCos::SinCos
        # * Z80::Utils::SinCos::SinCosTable
        #
        # ==Example
        #
        #    require 'z80'
        #    require 'z80/utils/sincos'
        #    class Program
        #        include Z80
        #
        #        SinCosTable = Utils::SinCos::SinCosTable
        #        SinCos      = Utils::SinCos::SinCos
        #
        #        macro_import Utils::SinCos
        #
        #        sincos      addr 0xF000, SinCos
        #
        #        start       exx
        #                    push  hl
        #                    call  make_sincos
        #                    pop   hl
        #                    exx
        #                    ld    a, 31    # angle = PI*31/128
        #                    sincos_from_angle sincos, h, l
        #                    ld    c, [hl]  # sine to bc
        #                    inc   l
        #                    ld    b, [hl]
        #                    ret
        #
        #        make_sincos create_sincos_from_sintable sincos, sintable:sintable
        #        sintable    bytes   neg_sintable256_pi_half_no_zero_lo
        #    end
        #
        class SinCos
            ##
            # A Z80::Utils::SinCos table entry struct.
            #
            # Consists of two +words+:
            # * +sin+
            # * +cos+
            # where each +word+ is a 16-bit fixed point, twos complement signed number with
            # the integral part in its high 8 bits and the fractional part in its low 8 bits,
            # in the range: [-1.0, 1.0].
            class SinCos < Z80::Label
                sin  word
                cos  word
            end
            ##
            # Z80::Utils::SinCos table struct.
            #
            # The normalized angle [0,256) being used in this table translates to radians in
            # the following way:
            #
            #   α = PI * angle / 128
            #   angle = α * 128 / PI
            #
            # The full table consist of 256 SinCos entries which occupy 1024 bytes.
            #
            #   offset fn(angle)
            #
            #   0x000: sin(0)   0x100: sin(1)   0x200: sin(2)   0x300: sin(3)
            #   0x002: cos(0)   0x102: cos(1)   0x202: cos(2)   0x302: cos(3)
            #   0x004: sin(4)   0x104: sin(5)   0x204: sin(6)   0x304: sin(7)
            #   0x008: cos(4)   0x108: cos(5)   0x208: cos(6)   0x308: cos(7)
            #   ...
            #   0x0fc: sin(252) 0x1fc: sin(253) 0x2fc: sin(254) 0x3fc: sin(255)
            #   0x0fe: cos(252) 0x1fe: cos(253) 0x2fe: cos(254) 0x3fe: cos(255)
            class SinCosTable < Z80::Label
              entries SinCos, 256
            end
            ##
            # =Z80::Utils::SinCos Macros
            module Macros
                ##
                # Returns an array of 63 bytes containing the first quarter sine table, 256-based angle,
                # negated, and fractional parts only values.
                #
                #   for a in 1..63 -> (-256 * sin(PI * a / 128)) & 0x00FF
                #
                # Suitable for #create_sincos_from_sintable macro.
                def neg_sintable256_pi_half_no_zero_lo
                    (1..63).map{|a| (-Math.sin(Math::PI*a.to_f/128.0)*256.0).truncate & 0xff }
                end
                ##
                # Returns a SinCosTable descriptors.
                #
                # Example:
                #   sincos data SinCosTable, sincos_table_descriptors
                def sincos_table_descriptors
                    (0..255).map do |a|
                        a = ((a & 0x3F) << 2) | ((a & 0xC0) >> 6)
                        sin = (Math.sin(Math::PI*a.to_f/128.0)*256.0).truncate
                        cos = (Math.cos(Math::PI*a.to_f/128.0)*256.0).truncate
                        {sin: sin, cos: cos}
                    end
                end
                ##
                # Creates a routine that calculates an address of a SinCos table entry for a given
                # +angle+ in the +a+ register and returns it in the +th+|+tl+ register pair.
                #
                # The expected +angle+ [0,256) translates to radians in the following way:
                #
                #   α = PI * angle / 128
                #   angle = α * 128 / PI
                #
                # +th+ and +tl+ should be a unique pair of any 8-bit registers, except +a+.
                #
                #    a = ABCDEFGH (binary)
                #   tl = ABCDEF00, th = 000000GH + INT (sincos / 256)
                # 
                # +sincos+:: An address of SinCos table, must be aligned to 256 bytes or an 8-bit register
                #            holding the MSB of the SinCos address. The LSB of +sincos+ address must be +0+.
                #
                # Options:
                # * +mask+:: An optional 8-bit register holding a preloaded mask value:
                #            +0xFC+ (+0b11111100+).
                #
                # T-states: 30|27|24.
                #
                # Mofifies: +af+, +th+, +tl+.
                def sincos_from_angle(sincos, th=h, tl=l, mask:nil)
                    raise ArgumentError, "sincos_from_angle: invalid th, tl arguments" unless th != tl &&
                                                    [th, tl].all? {|t| register?(t) && t.bit8? && t != a }
                    raise ArgumentError, "sincos must be a direct address" if pointer?(sincos)
                    if immediate?(sincos)
                        sincos = sincos.to_i
                        raise ArgumentError, "sincos address must be aligned to 256 bytes" unless (sincos & 0x00FF).zero?
                    end
                    raise ArgumentError, "invalid mask argument" unless mask.nil? or
                            (register?(mask) and mask.bit8? and ![a, th, tl, sincos].include?(mask))
                    mask = 0b11111100 if mask.nil?
                    isolate do
                        if register?(sincos)
                            raise ArgumentError, "invalid sincos register" unless sincos.bit8? and 
                                                                            ![a, th, tl].include?(sincos)
                                ld   th, a
                                anda mask
                                ld   tl, a
                                xor  th
                                add  sincos
                                ld   th, a
                        else
                            select(sincos & 0x00FF, &:zero?).then do |_|
                                ld   th, a
                                anda mask
                                ld   tl, a
                                xor  th
                                add  sincos >> 8
                                ld   th, a
                            end.else do
                                raise ArgumentError, "sincos address must be aligned to 256 bytes"
                            end
                        end
                    end
                end
                ##
                # Creates a subroutine that generates a full SinCosTable from a quarter sines table
                # obtainable from #neg_sintable256_pi_half_no_zero_lo.
                #
                # +sincos+:: An address of SinCos table as a label or an integer.
                # +sintable+:: Address of a #neg_sintable256_pi_half_no_zero_lo sines table.
                #              Can be a +label+, +hl+ register or a +label+ pointer.
                #
                # _NOTE_:: +sincos+ must be an address aligned to 256 bytes
                #          (the lowest 8 bits of a 16-bit +sincos+ address must be +0+).
                #
                # The SinCosTable size is 1024 bytes.
                #
                # Mofifies: +af+, +bc+, +de+, +hl+, +af'+, +bc'+, +de'+, +hl'+.
                def create_sincos_from_sintable(sincos, sintable:hl)
                    isolate do
                        sincos0     addr 0, SinCos
                                    ld   hl, sintable unless sintable == hl
                                    ld   b, 64
                                    xor  a         # -sin256(0) == -0
                                    jr   skip_aget
                        aloop       ld   a, [hl]   # -sin256(64-b)
                                    inc  hl
                        skip_aget   ex   af, af
                                    ld   a, 64
                                    sub  b         # a = angle (1-63)
                                    exx
                                    ld   b, a      # save angle (1-63)
                                    ld   a, 64
                                    add  b         # a + 64: cos256(a + 64) == -sin256(a)
                                    call to_sincos
                                    scf
                                    call put_cos
                                    ld   a, 128
                                    add  b         # a + 128: sin256(a + 128) == -sin(a)
                                    call to_sincos
                                    scf
                                    call put_sin
                                    ld   a, 192
                                    sub  b         # 192 - a: cos256(192 - a) == -sin256(a)
                                    call to_sincos
                                    scf
                                    call put_cos
                                    xor  a
                                    sub  b         # (256) - a: sin256(-a) == -sin256(a)
                                    call to_sincos
                                    scf
                                    call put_sin
                                    ex   af, af    # -sin256(a)
                                    neg            # sin256(a)
                                    ex   af, af
                                    ld   a, b      # a: sin256(a)
                                    call to_sincos # CF=0
                                    call put_sin
                                    ld   a, 64
                                    sub  b         # 64 - a: cos256(64-a) == sin256(a)
                                    call to_sincos # CF=0
                                    call put_cos
                                    ld   a, 128
                                    sub  b         # 128 - a: sin256(128 - a) == sin256(a)
                                    call to_sincos # CF=0
                                    call put_sin
                                    ld   a, 192
                                    add  b         # a + 192: cos256(a+192) == sin256(a)
                                    call to_sincos # CF=0
                                    call put_cos
                                    exx
                                    djnz aloop

                                    ld   hl, 256
                                    ld   [sincos0[0].cos + sincos], hl     # cos256(0) == 1
                                    ld   [sincos0[64/4].sin + sincos], hl  # sin256(64) == 1
                                    ld   h, -1
                                    ld   [sincos0[128/4].cos + sincos], hl # cos256(128) == -1
                                    ld   [sincos0[192/4].sin + sincos], hl # sin256(192) == -1
                                    ret

                        put_cos     inc  hl
                                    inc  hl
                        put_sin     ex   af, af    # sin(a)
                                    ld   [hl], a   # lower sin256 byte
                                    inc  hl
                                    ex   af, af    # save sin
                                    sbc  a         # 0 or -1 depending on CF
                                    ld   [hl], a   # higher sin256 byte
                                    ret
                        to_sincos   sincos_from_angle sincos
                                    ret
                    end
                end

            end
            include Z80
        end
    end
end

# DEPRECATED
Z80SinCos = Z80::Utils::SinCos # :nodoc:

if __FILE__ == $0
    require 'zxlib/gfx/draw'
    require 'zxlib/basic'
    # :stopdoc:
    class TestSinCos # :nodoc: all
        include Z80
        include Z80::TAP
        include ZXLib::Gfx::Draw::Constants

        SinCosTable = Utils::SinCos::SinCosTable
        SinCos      = Utils::SinCos::SinCos

        macro_import ZXLib::Gfx::Draw
        macro_import MathInt
        macro_import Utils::SinCos
        import       ZXLib::Sys, macros: true, code: false

        with_saved :start, :exx, hl do
                    call make_sincos
        end

        ns :test_sincos do
                    ld   hl, sincos
                    ld   de, sincos_tmpl
                    ld   bc, +sincos_tmpl
        compare     ld   a, [de]
                    inc  de
                    cpi                # [de] - [hl], hl++, bc--
                    jr   NZ, was_error # [de]<>[hl]
                    ret  PO            # bc = 0
                    jr   compare
        was_error   inc  bc
                    ret                # bc<>0 and contains an offset from the end of the table
        end

        ns :radius_arg do
                    find_def_fn_args 1, subroutine:false, cf_on_direct:true
                    ret  C
                    report_error_unless Z, 'Q Parameter error'
                    read_positive_int_value d, e
                    report_error_unless Z, 'A Invalid argument'
                    xor  a
                    ora  d
        too_big     report_error_unless Z, '6 Number too big'
                    ld   a, e
                    cp   88                   # scale_y < 88
                    jr   NC, too_big.err
                    ld   [mult_de_a + 1], a   # f(s)
                    ret
        end

        with_saved(:draw_circle, :exx, hl, ret: true) do |eoc|
                    call radius_arg

                    ld   a, [vars.p_flag]
                    ld   hl, draw.line
                    anda 0b00001010           # INVERSE 1 or OVER 1
                    jr   Z, set_fx
                    cp   0b00001010           # INVERSE 1 and OVER 1
                    jr   NZ, inverse_1
                    jp   eoc                  # do nothing
        inverse_1   anda 0b00001000           # INVERSE 1
                    jr   Z, over_1
                    ld   hl, draw.line_inversed
                    jr   set_fx
        over_1      ld   hl, draw.line_over
        set_fx      ld   [draw_jump + 1], hl

                    xor  a
        drawloop    push af
                    sincos_from_angle sincos, h, l
                    ld   e, [hl]
                    inc  l
                    ld   d, [hl]
                    inc  l
                    push hl
                    bit  7, d
                    jr   NZ, neg_sin
                    call mult_de_a      # hl = sin(a)*256*h
                    ld   l, h
                    ld   h, 0
                    jr   get_cosine
        neg_sin     neg16 d, e
                    call mult_de_a      # hl = -sin(a)*256*h
                    ld   l, h
                    ld   h, -1
        get_cosine  ex   [sp], hl
                    ld   e, [hl]
                    inc  l
                    ld   d, [hl]
                    bit  7, d
                    jr   NZ, neg_cos
                    call mult_de_a      # hl = cos(a)*256*h
                    ld   d, 0
                    jr   skip_dy
        neg_cos     neg16 d, e
                    call mult_de_a      # hl = -cos(a)*256*h
                    ld   d, -1
        skip_dy     ld   e, h
                    pop  bc
                    ld   hl, (88<<8)|128
        draw_jump   call draw.line
                    pop  af
                    inc  a
                    jr   NZ, drawloop
        end

        with_saved(:draw_sine, :exx, hl, ret: true) do |eoc|
                    call radius_arg
                                              # read system color attributes
        skip_args   ld   hl, [vars.attr_p]    # l: attr_p, h: mask_p
                    ld   a, h
                    ld   [plot_a.mask_a + 1], a
                    cpl
                    anda  l
                    ld   [plot_a.attr_a + 1], a
                    ld   a, [vars.p_flag]
                    ld   hl, draw.preshifted_pixel
                    anda 0b00001010           # INVERSE 1 or OVER 1
                    jr   Z, normal
                    cp   0b00001010           # INVERSE 1 and OVER 1
                    jr   NZ, inverse_1
                    ld   a, PLOT_FX_NONE      # OP-CODE: LD A,(HL)
                    jr   set_fx
        inverse_1   anda 0b00001000           # INVERSE 1
                    jr   Z, over_1
                    ld   a, PLOT_FX_AND       # OP-CODE: AND (HL)
                    ld   hl, draw.preshifted_inversed_pixel
                    jr   set_fx
        over_1      ld   a, PLOT_FX_XOR       # OP-CODE: XOR (HL)
                    jr   set_fx
        normal      ld   a, PLOT_FX_OR        # OP-CODE: OR (HL)
        set_fx      ld   [plot_a.plot_fx], a  # fx [hl]
                    ld   [plot_a.preshift_a + 1], hl # normal or negative preshift
                                              # FOR x=0 TO 255: PLOT x,SIN (x/128*PI)*87+88: NEXT x
                    xor  a
        drawloop    ld   e, a
                    exx
                    sincos_from_angle sincos, h, l
                    ld   e, [hl]
                    inc  l
                    ld   d, [hl]
                    bit  7, d
                    jr   NZ, negative
                    call mult_de_a      # hl = sin(x)*256*h
                    xor  a
                    sub  h              # a = 0-(sin(x)*h)
                    jr   plot_jump
        negative    neg16 d, e
                    call mult_de_a      # hl = -sin(x)*256*h
                    ld   a, h           # a = -sin(x)*h
        plot_jump   add  88
                    exx
                    ld   d, a
                    ld   a, e
                    ex   af, af

        plot_a      plot_pixel(e, d, draw.preshifted_pixel, fx: :or, with_attributes: true, color_attr: 0b00111000, color_mask: 0)

                    ex   af, af
                    inc  a
                    jr   NZ, drawloop
        end

        mult_de_a   ld   a, 87
                    mul8 d, e, a, tt:de, clrhl:true, double:true, optimize: :size
                    inc  h  # rounding up instead of truncating
                    srl  h  # (v +.5)/2
                    ret

        make_sincos create_sincos_from_sintable sincos, sintable:sintable

        sintable    bytes   neg_sintable256_pi_half_no_zero_lo
        sintable_e  label

        draw        make_draw_line_subroutines
        draw_end    label

                    org  align: 0x100
        sincos_tmpl data SinCosTable, sincos_table_descriptors

        eop         label

        center_y    draw_sine.plot_jump + 1
        scale_y     mult_de_a + 1

        sincos      addr 0xFB00, SinCos
    end

    include ZXLib

    testsincos = TestSinCos.new 0xE000
    program = Basic.parse_source <<-END
       1 DEF FN s(h)=USR #{testsincos[:draw_sine]}: DEF FN c(r)=USR #{testsincos[:draw_circle]}
      10 LET res=USR #{testsincos.org}
      20 IF res<>0 THEN PRINT "Error at: ";#{testsincos[:eop]-testsincos[:sincos_tmpl]}-res: STOP
      30 FOR h=0 TO 87: RANDOMIZE FN s(h): NEXT h: PAUSE 0
     100 CLS
     110 FOR r=1 TO 87: RANDOMIZE FN c(r): NEXT r: STOP
    9998 STOP
    9999 CLEAR #{testsincos.org - 1}: LOAD ""CODE
    END

    puts testsincos.debug
    puts program.to_source escape_keywords:true

    [:sincos,
     :sincos_tmpl,
     :start,
     :draw_circle,
     :draw_sine,
     :draw,
     :center_y,
     :scale_y,
     :eop
    ].each do |name|
        puts "#{name.to_s.ljust(14)}: 0x#{testsincos[name].to_s(16).rjust(4,?0)} : #{testsincos[name]}"
    end
    puts "make sincos size: #{testsincos[:"+make_sincos"]}"
    puts "sintable size   : #{testsincos[:sintable_e] - testsincos[:sintable]}"
    puts "draw size       : #{testsincos[:"+draw"]}"
    puts "code size       : #{testsincos[:sintable] - testsincos[:start]}"
    raise "memory clash detected" if testsincos[:eop] > testsincos[:sincos]

    program.save_tap 'test.z80.utils.sincos.tap', name:'sincos', line:9999
    testsincos.save_tap 'test.z80.utils.sincos.tap', name:'sincos', append: true
end
