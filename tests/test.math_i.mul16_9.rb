require 'z80'
require 'zxlib/sys'
require 'zxlib/basic'

class MTestFactory
    include Z80

    module Macros
        include ::ZXLib::Sys::Macros
        include ::ZXLib::Math::Macros
        include ::Z80::MathInt::Macros

        def make_tests(tt=de, optimize=:time)
            label_import  ZXLib::Sys

            th, tl = tt.split
            kk = if tt == de then bc else de end
            kh, kl = kk.split
            sgn = tl

            ns :test_unsigned do
                        call find_args
              error_q   report_error_unless Z, "Q Parameter error"
                        call read_uint # 0..65535
                        push kk     # k: kh|kl

                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        call read_uint # 0..511
                        ld   a, 1
                        cp   kh
              error_b   report_error_unless NC, "B Integer out of range"
                        ld   ix, multiply
                        jr   test_mul
            end

            ns :test_signed do
                        call find_args
              error_q   report_error_unless Z, "Q Parameter error"
                        call read_int # -32768..32767
                        sign_extend(a, kh)
                        xor  sgn
                        jr   NZ, error_b.err
                        push kk     # k: kh|kl

                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        call read_int # -256..255
                        ld   a, kh
                        xor  sgn
              error_b   report_error_unless Z, "B Integer out of range"
                        ld   ix, mult_sig
                        jr   test_mul
            end

            forward_ix  jp   (ix)

            ns :test_mul do
                         # m: -256..511
              skip_ck1  ld   a, kl  # m
                        sra  kh     # CF:m & 0x100
                        pop  tt     # tt: k
                        ld16 hl, tt

                        call forward_ix # hl = k * m

                        ld16 bc, hl
                        ret
              # skip_ock  ld   a, sgn
              #           ld   d, a
              #           ld   c, h
              #           ld   b, l
              #           ld   e, 0

              #           anda a
              # sknegs    jp   P, skipzch
              #           neg_int d, c, b, t:e, t_is_zero:true
              #           ora  0x80 # SF = 1
              # skipzch   integer32_to_fp e, d, c, b, sgn:M
            end

            # return_fp   return_with_fp restore_iy:nil, restore_hl_alt:nil

            find_args   find_def_fn_args 1, subroutine:true

            ns :read_int do
                        read_integer_value kh, kl, sgn
              error_a   report_error_unless Z, "A Invalid argument"
                        inc  hl
                        ret
            end

            ns :read_uint do
                        read_positive_int_value kh, kl
              error_a   report_error_unless Z, "A Invalid argument"
                        inc  hl
                        ret
            end

            dc! "****************************************"
            dc! "***             MULTIPLY             ***"
            dc! "****************************************"
            multiply    mul16(h, l, a, tt:tt, mbit9_carry:true, optimize:optimize)
                        ret
            dc! "****************************************"
            dc! "***          MULTIPLY signed         ***"
            dc! "****************************************"
            mult_sig    mul16_signed9(h, l, a, tt:tt, m_overflow:nil, optimize:optimize)
                        ret
        end
    end
end

class MTest1
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul16_9.compact"

    macro_import MTestFactory
    make_tests de, :compact
end

class MTest2
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul16_9.size"

    macro_import MTestFactory
    make_tests de, :size
end

class MTest3
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul16_9.time"

    macro_import MTestFactory
    make_tests de, :time
end

class MTest4
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul16_9.unroll"

    macro_import MTestFactory
    make_tests de, :unroll
end

include ZXLib

[MTest1, MTest2, MTest3, MTest4].each do |mtest_klass|
    mtest = mtest_klass.new 65536 - mtest_klass.code.bytesize
    # puts mtest.debug
    source = <<-END
       1 DEF FN n(x)=x-(65536 AND x>=32768): DEF FN u(a,b)=USR #{mtest[:test_unsigned]}: DEF FN i(a,b)=FN n(USR #{mtest[:test_signed]})
      10 RANDOMIZE
      20 FOR a=-127 TO 128: FOR b=-256 TO 255
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN i(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
      30 IF a<0 THEN GO TO 50
      40 LET c=b+256
         PRINT a;"*";c;"=";: LET r=FN u(a,c)
         IF r<>a*c THEN GO TO 1100
         PRINT r;"        "
      50 NEXT b: NEXT a
      60 RANDOMIZE
         FOR b=-256 TO 255
         LET a=INT(RND*65536)-32768
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN i(a,b)
         LET t=a*b+32768: LET t=FN n(t-65536*INT(t/65536)-32768)
         IF t<>r THEN GO TO 1200
         PRINT r;"        "
         LET a=a+32768
         LET c=b+256
         PRINT a;"*";c;"=";: LET r=FN u(a,c)
         LET t=a*c: LET t=t-65536*INT(t/65536)
         IF t<>r THEN GO TO 1200
         PRINT r;"        "
         NEXT b
         GO TO 60
     100 INPUT "a ";a, "b ";b
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN i(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         GO TO 100
    1000 PRINT "        "'"assertion failed: ";r;"`<>`";a*b: GO TO 10000
    1100 PRINT "        "'"assertion failed: ";r;"`<>`";a*c: GO TO 10000
    1200 PRINT "        "'"assertion failed: ";r;"`<>`";t: GO TO 10000
    9998 STOP: RUN
    9999 CLEAR #{mtest.org-1}: LOAD ""CODE: RUN
    END
    program = Basic.parse_source source
    puts "#{mtest.class::NAME} size: #{mtest.code.bytesize}"
    %w[
        test_mul
        find_args
        multiply
        mult_sig
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{mtest[label].to_s 16} - #{mtest[label]}, size: #{mtest['+'+label]}"
    end

    program.save_tap "#{mtest.class::NAME}.tap", line:9999
    mtest.save_tap "#{mtest.class::NAME}.tap", append:true
end
