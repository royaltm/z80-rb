# -*- coding: BINARY -*-
require 'z80'
require 'zxlib/sys'
class Float
    ##
    # Converts +Float+ to a ZX-Spectrum's real number encoded as a 5-byte binary string.
    #
    # Suitable to be used with ZXMath::ZXReal struct for data.
    #
    # +simplified_int+ indicates if the integers in the
    # range of -65535..65535 should be stored in a simplified integer form.
    #
    # Returns binary string.
    def to_z80bin(simplified_int=true)
        ZXMath.pack_number self, simplified_int
    end
end
##
# A module with the ZXReal struct definition and ZX-Spectrum FP helpers.
#
# Example:
#
#    require('zxlib/math')
#
#    class TestMath
#      include Z80
#      include Z80::TAP
#  
#      ZXReal = ZXMath::ZXReal
#  
#      chan_open addr 0x1601
#
#      export :auto
#      start       ld a, 2
#                  call chan_open
#                  ld hl, pi
#                  call math.print_fp_hl
#                  ret
#
#      pi          data ZXReal, Math::PI
#
#      import :math, ZXMath
#    end
class ZXMath
    ##
    # Converts +num+ to a ZX-Spectrum's real number encoded as a 5-byte binary string.
    #
    # +simplified_int+ indicates if the integers in the
    # range of -65535..65535 should be stored in a simplified integer form.
    #
    # Returns binary string.
    def ZXMath.pack_number(num, simplified_int=true)
        sgn = num < 0
        if simplified_int && num == num.truncate && -65535 <= num && num <= 65535
            [0,
             sgn ? -1 : 0,
             num,
             0].pack('CcvC')
        else
            m = sgn ? -num : num
            e = (Math.log2(m)+1.0).floor
            raise "overflow error" if e > 127
            if e < -127
                [0].pack('C')*5
            else
                # m = m*(2**-e)
                if e < 0
                    m = m*(1<< (-e))
                else
                    m = m/(1<<e)
                end
                [
                    e + 128,
                    sgn ? (m*(1<<32)).round : (m*(1<<32)).round ^ (1<<31)
                ].pack('CN')
            end
        end
    end
    ##
    # Converts a ZX-Spectrum's real number as a 5-byte binary string to +Numeric+ value.
    #
    # +simplified_int_as_fixnum+ indicates if the number encoded as a simple integer should be returned as a +Fixnum+.
    #
    # Returns +Float+ or +Fixnum+.
    def ZXMath.unpack_number(bin, simplified_int_as_fixnum=true)
        raise ArgumentError unless String === bin && bin.bytesize >= 5
        e, m = bin.unpack('CN')
        if e.zero?
            sgn, val, z = bin.unpack('xcvC')
            if z.zero?
                val = case sgn
                when 0 then val
                when -1 then val-0x10000
                else
                    raise "simplified binary integer parse error"
                end
                val = val.to_f unless simplified_int_as_fixnum
                return val
            end
        end
        e -= 128
        sgn = if (m & (1<<31)).zero?
            m |= (1<<31)
            false
        else
            true
        end
        val = m.to_f/(1<<32)
        # m = m/(2**-e)
        val = if e < 0
            val/(1<< (-e))
        else
            val*(1<<e)
        end
        sgn ? -val : val
    end

    include Z80

    ## ZX-Spectrum's float number epsilon
    EPSILON = 1.0 / (1<<32)
    ##
    # A struct representing a ZX-Spectrum's FP calculator's real number data type.
    #
    # See:
    # * http://www.worldofspectrum.org/ZXBasicManual/zxmanchap24.html
    # * http://dac.escet.urjc.es/~csanchez/pfcs/zxspectrum/CompleteSpectrumROMDisassemblyThe.pdf
    # * http://wos.meulie.net/pub/sinclair/games-info/z/Z80Toolkit2.pdf
    class ZXReal < Label
        exponent    byte
        intsign     byte
        intlsb      byte
        intmsb      byte
        intpad      byte
        mantissa    intsign byte, 4
        mantissabin intsign 4
    end

    label_import ZXSys

    export print_fp_hl

    ##
    # :call-seq:
    #   print_fp_hl
    #
    # Call +print_fp_hl+ with +hl+ pointing to the 1st byte of a +ZXReal+ number
    # to print that number to the currently opened channel.
    #
    # After return the ZF flag can be inspected to check if the number was 0.
    print_fp_hl     ld a, [hl]      # get floating point from (hl)
                    inc hl
                    ld e, [hl]
                    inc hl
                    ld d, [hl]
                    inc hl
                    ld c, [hl]
                    inc hl
                    ld b, [hl]
                    inc hl
                    push hl         # save hl
                    ld  l, a        # check if zero (return in Z flag)
                    ora e
                    ora d
                    ora c
                    ora b
                    push af
                    ld  a, l
                    call rom.stk_store  # store number on calculator stack
                    call rom.print_fp   # print number from stack
                    pop af
                    pop hl
                    ret
end

if __FILE__ == $0
    # :stopdoc:
    require 'test/unit/assertions'
    include Test::Unit::Assertions
    class TestMath # :nodoc: all
        include Z80
        include Z80::TAP

        ZXReal = ZXMath::ZXReal # :nodoc:

        label_import ZXSys

        # print real numbers, up to 0
        start         ld a, 2
                                    call rom.chan_open
                                    ld hl, numbers[0]
        ploop         call math.print_fp_hl
                                    push af
                                    push hl
                                    ld a, "\r".ord
                                    rst rom.print_a
                                    pop hl
                                    pop af
                                    jr NZ, ploop
                                    ret
        numbers       data ZXReal,
                                                        -1.0,
                                                         1.0,
                                                         0.1,
                                                (-1.0/3),
                                                 32767.0,
                                                 32767.0.to_z80bin(false),
                                                 32768.5,
                                                 65535.0,
                                                 65535.0.to_z80bin(false),
                                                -65535.0,
                                                -65535.0.to_z80bin(false),
                                                -65536.0,
                                                 65536.0,
                                                [0xff,0x7f,0xff,0xff,0xff],
                                                [0xff,0xff,0xff,0xff,0xff],
                                                [1,0,0,0,0],
                                                [1,0x80,0,0,0],
                                                Math::PI,
                                                Math::E,
                                                (Math::PI*(10**-39)),
                                                (Math::E*(10**37)),
                                                {exponent: 4+128, mantissabin: "\x80\x80\x80\x80"},
                                                {exponent: 4+128, mantissa: [0x80,0x80,0x80,0x80]},
                                                [0,-1,1,0,0],
                                                '!@#$%^',
                                                {exponent: 0, intsign: -1, intlsb: 0x80, intmsb: 0},
                                                8.5,
                                                0.5,
                                                0
        import ZXMath, :math
    end

    (-127..127).each do |e|
        100.times do
            num = if e < 0
                rand * (1<< (-e))
            else
                rand * (1<< e)
            end
            [num, -num].each do |num|
                bin = num.to_z80bin(false)
                assert_instance_of String, bin
                assert_equal bin.bytesize, 5
                assert_equal bin.encoding, Encoding::ASCII_8BIT
                res = ZXMath.unpack_number bin, false
                assert_instance_of Float, res
                assert_in_epsilon res, num, ZXMath::EPSILON
            end
        end
    end

    (-65535..65535).each do |i|
        bin = ZXMath.pack_number i
        assert_instance_of String, bin
        assert_equal bin.bytesize, 5
        assert_equal bin.encoding, Encoding::ASCII_8BIT
        res = ZXMath.unpack_number bin
        assert_kind_of Integer, res
        assert_equal res, i
    end

    p = TestMath.new 0x8000
    puts p.debug
    p.save_tap 'testmath.tap'

end
