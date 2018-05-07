# -*- coding: BINARY -*-
require 'z80'
##
# =Z80SinCos - integer sinus-cosinus table routines.
#
# in Z80SinCos::Macros
#
class Z80SinCos
    class SinCos < Z80::Label
        sin  word
        cos  word
    end
    ##
    # The full table has 256 entries which translates to (1024 bytes).
    #
    # 000: sin256(0)   100: sin256(1)   200: sin256(2)   300: sin256(3)
    # 002: cos256(0)   102: cos256(1)   202: cos256(2)   302: cos256(3)
    # 004: sin256(4)   104: sin256(5)   204: sin256(6)   304: sin256(7)
    # 008: cos256(4)   108: cos256(5)   208: cos256(6)   308: cos256(7)
    # ...
    # 0FC: sin256(252) 1FC: sin256(253) 2FC: sin256(254) 3FC: sin256(255)
    # 0FE: cos256(252) 1FE: cos256(253) 2FE: cos256(254) 3FE: cos256(255)
    class SinCosTable < Z80::Label
      entries SinCos, 256
    end
    ##
    # =Z80SinCos Macros
    module Macros
        ##
        # Returns array of first quarter negative sinus table, 256-based angle, scaled *256, lower bytes only.
        #
        # a=1..63: (-256 * sin(2pi * a / 256)) & 0x00FF
        #
        # Suitable for a create_sincos_from_sintable macro.
        def neg_sintable256_pi_half_no_zero_lo
            (1..63).map{|a| (-Math.sin(Math::PI*2.0*a.to_f/256.0)*256.0).truncate & 0xff }
        end
        ##
        # Returns a sincos table descriptors.
        def sincos_table_descriptors
            (0..255).map do |a|
                a = ((a & 0x3F) << 2) | ((a & 0xC0) >> 6)
                sin = (Math.sin(Math::PI*2.0*a.to_f/256.0)*256.0).truncate
                cos = (Math.cos(Math::PI*2.0*a.to_f/256.0)*256.0).truncate
                {sin: sin, cos: cos}
            end
        end
        ##
        # Code that returns an address of sincos entry for a given 256-based angle in +a+ register.
        #
        # For each angle: a <= llllllhh, th => sincos MSB + 000000hh, tl => llllll00
        #
        # 
        # +sincos+:: Immediate address of SinCos table, must be on a 256 byte boundary
        #            (LSB of +sincos+ address must be 0).
        def sincos_from_angle(sincos, th=h, tl=l)
            sincos = sincos.to_i
            raise ArgumentError unless (sincos & 0x00FF).zero?
            isolate do
                    ld   th, a
                    anda 0b11111100
                    ld   tl, a
                    xor  th
                    add  sincos >> 8
                    ld   th, a
            end
        end
        ##
        # Code that creates a full sincos table from a minimal #neg_sintable256_pi_half_no_zero_lo sinus table
        #
        # +sincos+:: Immediate address of SinCos table as a +label+, must be on a 256 byte boundary
        #            (lower byte of +sincos+ address must be 0); reserve 1024 bytes.
        # +sintable+:: Address of a #neg_sintable256_pi_half_no_zero_lo sinus table
        #              Can be a +label+, +hl+ register or a +label+ pointer.
        def create_sincos_from_sintable(sincos, sintable:hl)
            isolate do
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
                        ld   [sincos[0].cos], hl     # cos256(0) == 1
                        ld   [sincos[64/4].sin], hl  # sin256(64) == 1
                        ld   h, -1
                        ld   [sincos[128/4].cos], hl # cos256(128) == -1
                        ld   [sincos[192/4].sin], hl # sin256(192) == -1
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

if __FILE__ == $0
    # :stopdoc:
    class TestSinCos # :nodoc: all
        include Z80
        include Z80::TAP

        SinCosTable = Z80SinCos::SinCosTable
        SinCos      = Z80SinCos::SinCos

        macro_import Z80SinCos

        sincos      addr 0xF000, SinCos

        start       exx
                    push hl
                    call make_sincos
                    pop  hl
                    exx
                    ld   hl, sincos
                    ld   de, sincos_tmpl
                    ld   bc, +sincos_tmpl
        compare     ld   a, [de]
                    inc  de
                    cp   [hl]
                    inc  hl
                    ret  NZ   # bc contains how many bytes to go
                    dec  bc
                    ld   a, c
                    ora  b
                    jr   NZ, compare
                    ret

        make_sincos create_sincos_from_sintable sincos, sintable:sintable

        sintable    bytes   neg_sintable256_pi_half_no_zero_lo

                    org  (pc + 0xFF) & 0xFF00
        # normally this should be on a 256 byte boundary
        # but we only use to verify here
        sincos_tmpl data SinCosTable, sincos_table_descriptors
    end

    p = TestSinCos.new 0x8000
    puts p.debug
    p.save_tap 'testsincos.tap'
end
