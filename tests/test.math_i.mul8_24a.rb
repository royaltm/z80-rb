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
            t = if tt == de then c else e end
            # t = if tt == de then b else d end
            m = case t
            when c then b
            when b then c
            when e then d
            when d then e
            end

            kk = tt
            # kk = hl
            kh, kl = kk.split

            # kh = l
            # kh = h
            # kh = th
            # kh = tl
            # kh = t
            # kh = a

            # kl = l
            # kl = h
            # kl = tl
            # kl = th
            # kl = t
            # kl = a

            km = t
            # km = a
            # km = l
            # km = h
            # km = tl
            # km = th

            unless [km, kl, kh].uniq.size == 3
                raise RuntimeError, "k is not unique"
            end

            dc!
            dc!"*********************************************"
            dc!"***              16-bit uint              ***"
            dc!"*********************************************"

            ns :test_mul24_8a do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_positive_int_value d, e
                        jr   NZ, error_a.err
                        ld   [multiplicand16], de
                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_positive_int_value b, c
                error_a report_error_unless Z, "A Invalid argument"
                        cp16n b, c, 256
                error_b report_error_unless C, "B Integer out of range"

                        ld   a, c
                        ld   [multiplier], a
                        call multiply_go

                        ld   e, 0
                        ld   d, a
                        ld   c, h
                        ld   b, l
                        integer32_to_fp e, d, c, b, sgn:nil
                        jp   return_fp
            end

            dc!
            dc!"*********************************************"
            dc!"***              24-bit uint              ***"
            dc!"*********************************************"

            ns :test_mul24_8a_u24 do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_integer32_value bc, de
                        jr   C, error_b.err
                        jr   NZ, error_b.err
                        xor  a
                        ora  b
                        jr   NZ, error_b.err
                        ld   [multiplicand16], de
                        ld   a, c
                        ld   [multiplicand24], a
                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_positive_int_value b, c
                error_a report_error_unless Z, "A Invalid argument"
                        cp16n b, c, 256
                error_b report_error_unless C, "B Integer out of range"

                        ld   a, c
                        ld   [multiplier], a
                        call multip24_go

                        ld   e, 0
                        ld   d, a
                        ld   c, h
                        ld   b, l
                        integer32_to_fp e, d, c, b, sgn:nil
                        jp   return_fp
            end

            dc!
            dc!"*********************************************"
            dc!"***              24-bit sint              ***"
            dc!"*********************************************"

            ns :test_mul24_8a_s24 do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_integer_value d, e, c
                        jr   NZ, error_a.err
                        sign_extend(a, c)
                        xor  c
                        jr   NZ, error_b.err
                        ld   [multiplicand16], de
                        ld   a, c
                        ld   [multiplicand24], a
                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_positive_int_value b, c
                error_a report_error_unless Z, "A Invalid argument"
                        cp16n b, c, 256
                error_b report_error_unless C, "B Integer out of range"

                        ld   a, c
                        ld   [multiplier], a
                        call multip24_go

                        ora  a  # test SF
                        ld   e, 0
                        ld   d, a
                        ld   c, h
                        ld   b, l

                        jp   P, skipneg
                        neg_int d, c, b, t:e, t_is_zero:true
                        ora  0x80 # SF = 1
                skipneg integer32_to_fp e, d, c, b, sgn:M
            end

            return_fp   return_with_fp restore_iy:nil, restore_hl_alt:nil

            find_args   find_def_fn_args 1, subroutine:true

            dc!
            dc!"*********************************************"
            dc!"***            16-bit MULTIPLY            ***"
            dc!"*********************************************"
            # a|hl = kk * tt
            multiply_go ld   a, [multiplier]
                        ld   m, a
                        ld   ix, multiplicand16
                        ld   kl, [ix + 0]
                        ld   kh, [ix + 1]
            multiply    mul8_24a(kh, kl, m, t:t, tt:tt, k_int24:false, optimize:optimize)
                        ret

            dc!
            dc!"*********************************************"
            dc!"***            24-bit MULTIPLY            ***"
            dc!"*********************************************"

            multip24_go ld   a, [multiplier]
                        ld   m, a
                        ld   ix, multiplicand16
                        ld   kl, [ix + 0]
                        ld   kh, [ix + 1]
                        ld   km, [ix + 2]
            multiply_24 mul8_24a(kh, kl, m, t:t, tt:tt, k_int24:km, optimize:optimize)
                        ret

            multiplicand16 dw 0
            multiplicand24 db 0
            multiplier     db 0
        end
    end
end

class MTest1
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul8_24a.unroll"

    macro_import MTestFactory
    make_tests de, :unroll
end

class MTest2
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul8_24a.time"

    macro_import MTestFactory
    make_tests de, :time
end

class MTest3
    include Z80
    include Z80::TAP

    SIGNED = true
    NAME = "test.math_i.mul8_24a.size"

    macro_import MTestFactory
    make_tests de, :size
end

include ZXLib

[MTest1, MTest2, MTest3].each do |mtest_klass|
    mtest = mtest_klass.new 65536 - mtest_klass.code.bytesize
    # puts mtest.debug
    source = <<-END
       1 DEF FN m(a,b)=USR #{mtest[:test_mul24_8a]}: DEF FN u(a,b)=USR #{mtest[:test_mul24_8a_u24]}: DEF FN s(a,b)=USR #{mtest[:test_mul24_8a_s24]}
      10 RANDOMIZE
      20 FOR b=0 TO 255
         LET a=INT (RND*65536): PRINT AT 0,0;a;"*";b;"=";: LET r=FN m(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         LET a=INT (RND*258)+65536: PRINT AT 1,0;a;"*";b;"=";: LET r=FN u(a,b)
         IF r<>a*b THEN GO TO 1100
         PRINT r;"        "
         LET a=INT (RND*32897): PRINT AT 2,0;a;"*";b;"=";: LET r=FN s(a,b)
         IF r<>a*b THEN GO TO 1200
         PRINT r;"        "
         LET a=-a: PRINT AT 3,0;a;"*";b;"=";: LET r=FN s(a,b)
         IF r<>a*b THEN GO TO 1200
         PRINT r;"        "
         NEXT b
         GO TO 10
     100 INPUT "k ";a;" m ";b
         FOR b=b TO 255
         PRINT AT 0,0;"16u ";a;"*";b;"=";: LET r=FN m(a,b): PRINT r;"        "
         IF r<>a*b THEN GO TO 1000
         NEXT b
         STOP
         GO TO 100
     200 INPUT "k ";a;" m ";b
         FOR b=b TO 255
         PRINT AT 0,0;"24u ";a;"*";b;"=";: LET r=FN u(a,b): PRINT r;"        "
         LET z=a*b
         IF z>16777215 THEN LET z=z-INT (z/16777216)*16777216
         IF r<>z THEN GO TO 1100
         NEXT b
         STOP
         GO TO 200
     300 INPUT "k ";a;" m ";b
         FOR b=b TO 255
         PRINT AT 0,0;"24s ";a;"*";b;"=";: LET r=FN s(a,b): PRINT r;"        "
         LET z=a*b
         IF z>8388607 THEN LET z=z-16777216
         IF z<-8388608 THEN LET z=z+16777216
         PRINT z;"        "
         IF r<>z THEN GO TO 1200
         NEXT b
         STOP
         GO TO 300
     999 STOP
    1000 PRINT "        "'"assertion failed: ";r;"`<>`";a*b: GO TO 10000
    1100 PRINT "        "'"assertion failed(u): ";r;"`<>`";a*b: GO TO 10000
    1200 PRINT "        "'"assertion failed(s): ";r;"`<>`";a*b: GO TO 10000
    9998 STOP: RUN
    9999 CLEAR #{mtest.org-1}: LOAD ""CODE: RUN
    END
    program = Basic.parse_source source
    puts "#{mtest.class::NAME} size: #{mtest.code.bytesize}"
    %w[
        test_mul24_8a
        test_mul24_8a_u24
        test_mul24_8a_s24
        find_args
        multiply
        multiply_24
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{mtest[label].to_s 16} - #{mtest[label]}, size: #{mtest['+'+label]}"
    end

    program.save_tap "#{mtest.class::NAME}.tap", line:9999
    mtest.save_tap "#{mtest.class::NAME}.tap", append:true
end
