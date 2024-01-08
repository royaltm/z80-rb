require 'z80'
require 'zxlib/sys'
require 'zxlib/basic'

class MTestFactory
    include Z80

    module Macros
        include ::ZXLib::Sys::Macros
        include ::ZXLib::Math::Macros
        include ::Z80::MathInt::Macros
        def make_tests(tt=de, optimize=:time, overflow:false)
            label_import  ZXLib::Sys

            k_overflow = if overflow
              overflow_a
            else
              nil
            end

            th, tl = tt.split
            kk = if tt == de then bc else de end
            kh, kl = kk.split
            sgn = kh

            ns :test_full do
                        ld   ix, multiply
                        jr   test_mul
            end

            ns :test_mlim do
                        ld   ix, mul_mlimit
                        jr   test_mul
            end

            ns :test_klim do
                        ld   ix, mul_klimit
                        jr   test_mul
            end

            ns :test_lim do
                        ld   ix, mul_limits
                        jr   test_mul
            end

            forward_ix  jp   (ix)

            ns :test_mul do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_integer_value th, kl, sgn
                        jr   NZ, error_a.err
                        ld   a, th
                        xor  sgn
                        jr   NZ, error_b.err
                        push kk     # k: sgn|kl

                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_integer_value th, tl, sgn
                error_a report_error_unless Z, "A Invalid argument"
                        ld   a, th
                        xor  sgn
                error_b report_error_unless Z, "B Integer out of range"
                        rrc  sgn
                        ld   a, tl  # m: CF|tl
                        pop  kk     # k: sgn|kl

                        call forward_ix # sgn|hl = k * m

                unless overflow
                        # when CF: 1 and result is 0
                        jr   NC, skip_ck
                        ld   a, sgn
                        ora  h
                        ora  l
                error_6 report_error_unless NZ, "6 Number too big" # error raised when ZF flag check failed
                end

                skip_ck ld   a, sgn
                        ld   d, a
                        ld   c, h
                        ld   b, l
                        ld   e, 0

                        anda a
                sknegs  jp   P, skipzch
                        neg_int d, c, b, t:e, t_is_zero:true
                        ora  0x80 # SF = 1
                skipzch integer32_to_fp e, d, c, b, sgn:M
            end

            return_fp   return_with_fp restore_iy:nil, restore_hl_alt:nil

            find_args   find_def_fn_args 1, subroutine:true

            if overflow
            overflow_a  report_error "6 Number too big"
            end
            # CF|sgn|hl = sgn|kl * a
            multiply    mul_signed9(sgn, kl, a, tt:tt, m_is_zero_zf:false, k_full_range: true, m_full_range: true, k_overflow:k_overflow, optimize:optimize)
                        ret

            mul_mlimit  mul_signed9(sgn, kl, a, tt:tt, m_is_zero_zf:false, k_full_range: true, m_full_range:false, optimize:optimize)
                        ret

            mul_klimit  mul_signed9(sgn, kl, a, tt:tt, m_is_zero_zf:false, k_full_range:false, m_full_range: true, k_overflow:k_overflow, optimize:optimize)
                        ret

            mul_limits  mul_signed9(sgn, kl, a, tt:tt, m_is_zero_zf:false, k_full_range:false, m_full_range:false, k_overflow:k_overflow, optimize:optimize)
                        ret
        end
    end
end

class MTest1
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul_signed9.time"
    OVERFLOW = false

    macro_import MTestFactory
    make_tests de, :time, overflow:OVERFLOW
end

class MTest2
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul_signed9.size"
    OVERFLOW = false

    macro_import MTestFactory
    make_tests de, :size, overflow:OVERFLOW
end

class MTest3
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul_signed9.time.ov"
    OVERFLOW = true

    macro_import MTestFactory
    make_tests de, :time, overflow:OVERFLOW
end

class MTest4
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul_signed9.size.ov"
    OVERFLOW = true

    macro_import MTestFactory
    make_tests de, :size, overflow:OVERFLOW
end

include ZXLib

[MTest1, MTest2, MTest3, MTest4].each do |mtest_klass|
    mtest = mtest_klass.new 65536 - mtest_klass.code.bytesize
    # puts mtest.debug
    source = <<-END
       1 DEF FN f(a,b)=USR #{mtest[:test_full]}: DEF FN m(a,b)=USR #{mtest[:test_mlim]}: DEF FN k(a,b)=USR #{mtest[:test_klim]}: DEF FN l(a,b)=USR #{mtest[:test_lim]}
      10 RANDOMIZE
      20 LET a=-256
         FOR b=-255 TO -1
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN f(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         PRINT a;"*";b;"=";: LET r=FN m(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         NEXT b
         FOR b=0 TO 255
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN f(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         PRINT a;"*";b;"=";: LET r=FN m(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         PRINT a;"*";b;"=";: LET r=FN k(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         PRINT a;"*";b;"=";: LET r=FN l(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         NEXT b
      50 FOR a=-255 TO 255
         LET b=-256
         CLS
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN f(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         PRINT a;"*";b;"=";: LET r=FN k(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
      70 FOR b=-255 TO 255
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN f(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         PRINT a;"*";b;"=";: LET r=FN m(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         PRINT a;"*";b;"=";: LET r=FN k(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         PRINT a;"*";b;"=";: LET r=FN l(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         NEXT b
         NEXT a
     100 INPUT "a ";a, "b ";b
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN f(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         GO TO 100
    1000 PRINT "        "'"assertion failed: ";r;"`<>`";a*b: GO TO 10000
    9998 STOP: RUN
    9999 CLEAR #{mtest.org-1}: LOAD ""CODE: RUN
    END
    program = Basic.parse_source source
    puts "#{mtest.class::NAME} ov: #{mtest.class::OVERFLOW} size: #{mtest.code.bytesize}"
    %w[
        test_mul
        find_args
        multiply
        mul_mlimit
        mul_klimit
        mul_limits
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{mtest[label].to_s 16} - #{mtest[label]}, size: #{mtest['+'+label]}"
    end

    program.save_tap "#{mtest.class::NAME}.tap", line:9999
    mtest.save_tap "#{mtest.class::NAME}.tap", append:true
end
