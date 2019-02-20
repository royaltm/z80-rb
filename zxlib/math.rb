# -*- coding: BINARY -*-
require 'z80'
require 'zxlib/sys'
class Float
    ##
    # Converts +Float+ to a ZX-Spectrum's real number encoded as a 5-byte binary string.
    #
    # Suitable to be used with ZXMath::ZXReal struct for data.
    #
    # +simplified_int+ indicates if integers in the range of -65535..65535
    # should be stored in a simplified integer form.
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
            m = (sgn ? -num : num).to_f
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
                    sgn ? (m*(1<<32)).truncate : (m*(1<<32)).truncate ^ (1<<31)
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
    EPSILON = 1.0 / (1<<31)
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
    isolate :print_fp_hl, use: rom do
                    ld a, [hl]      # get floating point from (hl)
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
                    call rom.stk_store  # store number on the calculator's stack
                    call rom.print_fp   # print number from the stack
                    pop af
                    pop hl
                    ret
    end
end

if __FILE__ == $0
    # :stopdoc:
    require 'test/unit/assertions'
    require 'zxlib/basic'
    include Test::Unit::Assertions
    class TestMath # :nodoc: all
        include Z80
        include Z80::TAP

        ZXReal = ZXMath::ZXReal # :nodoc:

        label_import ZXSys

        # print real numbers, up to 0
        ns :start, use: rom do
                        ld   a, 2
                        call rom.chan_open
                        ld   hl, numbers[0]
                        ld   b, 0
        ploop           inc  b
                        push bc
                        push hl
                        ld   a, b
                        call print_index
                        pop  hl
                        call math.print_fp_hl
                        push af
                        push hl
                        ld   a, "\r".ord
                        rst  rom.print_a
                        pop  hl
                        pop  af
                        pop  bc
                        jr   NZ, ploop
                        ret
        end
        ns :print_index, use: rom do
                        cp   10
                        jr   NC, skip_spc
                        push af
                        ld   a, " ".ord
                        rst  rom.print_a
                        pop  af
        skip_spc        call rom.stack_a
                        call rom.print_fp
                        ld   a, ":".ord
                        rst  rom.print_a
                        ld   a, " ".ord
                        rst  rom.print_a
                        ret
        end
        numbers         data ZXReal,
                            -1.0,                                                #  1: -1
                             1.0,                                                #  2: 1
                             0.1,                                                #  3: 0.1
                            (-1.0/3),                                            #  4: -0.33333333
                             32767.0,                                            #  5: 32767
                             32767.0.to_z80bin(false),                           #  6: 32767.0
                             32768.5,                                            #  7: 32768.5
                             65535.0,                                            #  8: 65535
                             65535.0.to_z80bin(false),                           #  9: 65535.0
                            -65535.0,                                            # 10: -65535
                            -65535.0.to_z80bin(false),                           # 11: -65535.0
                            -65536.0,                                            # 12: -65536.0
                             65536.0,                                            # 13: 65536.0
                            [0xff,0x7f,0xff,0xff,0xff],                          # 14: 1.7014118e+38
                            [0xff,0xff,0xff,0xff,0xff],                          # 15: -1.7014118e+38
                            [1,0,0,0,0],                                         # 16: 2.9387359e-39
                            [1,0x80,0,0,0],                                      # 17: -2.9387359e-39
                            Math::PI,                                            # 18: 3.14159265
                            Math::E,                                             # 19: 2.71828183
                            (Math::PI*(10**-39)),                                # 20: 3.1415927e-39
                            (Math::E*(10**37)),                                  # 21: 2.7182818e+37
                            {exponent: 4+128, mantissabin: "\x80\x80\x80\x80"},  # 22: -8.03137255
                            {exponent: 4+128, mantissa: [0x80,0x80,0x80,0x80]},  # 23: -8.03137255
                            [0,-1,1,0,0],                                        # 24: -65535
                            '!@#$%^',                                            # 25: 1.8946198e-29
                            {exponent: 0, intsign: -1, intlsb: 0x80, intmsb: 0}, # 26: -65408
                            8.5,                                                 # 27: 8.5
                            0.5,                                                 # 28: 0.5
                            0                                                    # 29: 0
        import ZXMath, :math
    end
    puts "Testing pack/unpack..."
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

    testzxmath = TestMath.new 0x8000
    puts testzxmath.debug
    program = Basic.parse_source <<-END
      10 RANDOMIZE USR #{testzxmath[:start]}
    9998 STOP
    9999 CLEAR #{testzxmath.org - 1}: LOAD ""CODE: RUN
    END
    puts program.to_source escape_keywords:true
    puts "="*32
    (testzxmath[:numbers]...testzxmath[:math]).step(5).each.with_index(1) do |addr, i|
        n = ZXMath.unpack_number(testzxmath.code.byteslice(addr - testzxmath.org, 5))
        fmtn = if n.zero? || n.abs >= 1e-8 && n.abs < 1e+8
            n.round(8).to_s
        else
            format('%.7e', n)
        end
        puts i.to_s.rjust(2) + ": " + fmtn
    end
    program.save_tap 'testzxmath.tap', line: 9999
    testzxmath.save_tap 'testzxmath.tap', append: true
end
