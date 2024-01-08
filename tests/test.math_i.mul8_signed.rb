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
            sgn = kh
            m = kl

            ns :test_a do
                        ld   ix, multiply_a
                        jr   test_mul
            end

            ns :test_m do
                        ld   ix, multiply_m
                        jr   test_mul
            end

            forward_ix  jp   (ix)

            ns :test_mul do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_integer_value th, tl, sgn
                        jr   NZ, error_a.err
                        ld   a, th
                        add  a, a
                        sbc  a, a
                        xor  sgn
                        jr   NZ, error_b.err
                        push tt     # k: th|tl

                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_integer_value th, m, sgn
                error_a report_error_unless Z, "A Invalid argument"
                        ld   a, th
                        xor  sgn
                error_b report_error_unless Z, "B Integer out of range"
                        ld   a, m
                        add  a, a
                        sbc  a, a
                        xor  sgn
                        jr   NZ, error_b.err
                        ld   a, m   # m: a and kl
                        pop  tt     # k: th|tl

                        call forward_ix # hl = tt * m

                        ld16 bc, hl
                        ret
            end

            find_args   find_def_fn_args 1, subroutine:true

            multiply_a  mul8_signed(th, tl, a, tt:tt, t:m, clrhl:true, double:false, optimize:optimize)
                        ret

            multiply_m  mul8_signed(th, tl, m, tt:tt, t:m, clrhl:true, double:false, optimize:optimize)
                        ret
        end
    end
end

class MTest1
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul8_signed.time"
    OVERFLOW = false

    macro_import MTestFactory
    make_tests de, :time
end

class MTest2
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul8_signed.size"
    OVERFLOW = false

    macro_import MTestFactory
    make_tests de, :size
end

include ZXLib

[MTest1, MTest2].each do |mtest_klass|
    mtest = mtest_klass.new 65536 - mtest_klass.code.bytesize
    # puts mtest.debug
    source = <<-END
       1 DEF FN n(x)=x-(65536 AND x>=32768): DEF FN t(a,b)=USR #{mtest[:test_a]}: DEF FN m(a,b)=USR #{mtest[:test_m]}
      10 RANDOMIZE
      20 FOR a=-255 TO 255
         FOR b=-128 TO 127
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN n(FN t(a,b))
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         PRINT a;"*";b;"=";: LET r=FN n(FN m(a,b))
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         NEXT b
         NEXT a
     100 INPUT "a ";a, "b ";b
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN n(FN t(a,b))
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         GO TO 100
    1000 PRINT "        "'"assertion failed: ";r;"`<>`";a*b: GO TO 10000
    9998 STOP: RUN
    9999 CLEAR #{mtest.org-1}: LOAD ""CODE: RUN
    END
    program = Basic.parse_source source
    puts "#{mtest.class::NAME} size: #{mtest.code.bytesize}"
    %w[
        test_mul
        find_args
        multiply_a
        multiply_m
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{mtest[label].to_s 16} - #{mtest[label]}, size: #{mtest['+'+label]}"
    end

    program.save_tap "#{mtest.class::NAME}.tap", line:9999
    mtest.save_tap "#{mtest.class::NAME}.tap", append:true
end
