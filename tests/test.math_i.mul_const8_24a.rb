require 'z80'
require 'zxlib/sys'
require 'zxlib/basic'

class MTestFactory
    include Z80

    module Macros
        include ::ZXLib::Sys::Macros
        include ::ZXLib::Math::Macros
        include ::Z80::MathInt::Macros
        def make_tests(tt=de)
            label_import  ZXLib::Sys

            th, tl = tt.split
            t = if tt == de then c else e end
            # t = if tt == de then b else d end
            kk = tt
            # kk = hl
            kh, kl = kk.split

            # kh = l
            # kh = h
            # kh = th
            # kh = tl
            # kh = t
            # kh = a
            # kh = if tt == de then b else d end

            # kl = l
            # kl = h
            # kl = tl
            # kl = th
            # kl = t
            # kl = a
            # kl = if tt == de then c else e end

            km = t
            # km = a
            # km = l
            # km = h
            # km = tl
            # km = th
            # km = if tt == de then b else d end

            unless [km, kl, kh].uniq.size == 3
                raise RuntimeError, "k is not unique"
            end

            dc!
            dc!"*********************************************"
            dc!"***              16-bit uint              ***"
            dc!"*********************************************"

            ns :test_mul16u do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_positive_int_value d, e
                        jr   NZ, error_a.err
                        ld   [multiplicand16], de
                        ld   a, 129
                        ld   [multiplicand24], a
            
                        ld   ix, m16u_table

                mult    inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_positive_int_value b, c
                error_a report_error_unless Z, "A Invalid argument"
                        cp16n b, c, 257
                error_b report_error_unless C, "B Integer out of range"

                        call multiply

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

            ns :test_mul24u do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_integer32_value bc, de
                error_b report_error_unless NC, "B Integer out of range"
                        jr   NZ, error_b.err
                        xor  a
                        ora  b
                        jr   NZ, error_b.err
                        ld   [multiplicand16], de
                        ld   a, c
                        ld   [multiplicand24], a

                        ld   ix, m24u_table

                        jp   test_mul16u.mult
            end

            dc!
            dc!"*********************************************"
            dc!"***              16-bit sint              ***"
            dc!"*********************************************"

            ns :test_mul16s do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_integer_value d, e, c
                        jr   NZ, error_a.err
                        sign_extend(a, d)
                        xor  c
                        jr   NZ, error_b.err
                        ld   [multiplicand16], de
                        ld   a, c
                        ld   [multiplicand24], a

                        ld   ix, m16s_table

                mult    inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_positive_int_value b, c
                error_a report_error_unless Z, "A Invalid argument"
                        cp16n b, c, 257
                error_b report_error_unless C, "B Integer out of range"

                        call multiply

                        ld   e, 0
                        ld   d, a
                        ld   c, h
                        ld   b, l

                        jp   P, skipneg
                        neg_int d, c, b, t:e, t_is_zero:true
                        ora  0x80 # SF = 1
                skipneg integer32_to_fp e, d, c, b, sgn:M
                        jp   return_fp
            end

            dc!
            dc!"*********************************************"
            dc!"***              24-bit sint              ***"
            dc!"*********************************************"

            ns :test_mul24s do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_integer32_value bc, de
                error_b report_error_unless NC, "B Integer out of range"
                        ex   af, af # af': sgn
                        xor  a
                        ora  b # bits 24-31 must be 0
                        jr   NZ, error_b.err
                        ex   af, af # af: sgn
                        jr   Z, skipneg
                        neg_int c, d, e, t:b, t_is_zero:true
                        jp   M, skippos # must be 24-bit twos complement negative
                        jr   error_b.err
                skipneg add  a, c # 0 + c
                        jp   M, error_b.err # must be 24-bit twos complement positive
                skippos label
                        ld   [multiplicand16], de
                        ld   a, c
                        ld   [multiplicand24], a

                        ld   ix, m24s_table

                        jp   test_mul16s.mult
            end

            return_fp   return_with_fp restore_iy:nil, restore_hl_alt:nil

            find_args   find_def_fn_args 1, subroutine:true

            ns :multiply do # a|hl = km|kk * bc
                        sla  c
                        rl   b
                        add  ix, bc
                        ld   c, [ix + 0]
                        ld   b, [ix + 1]
                        ld   ixl, c
                        ld   ixh, b
                        ld   a, r # clobber flags and A
                        di
                        ld   iy, multiplicand16
                        ld   kl, [iy + 0]
                        ld   kh, [iy + 1]
                        ld   km, [iy + 2]
                        ld   iy, vars_iy
                        ei
                        jp   (ix)
            end

            m16u_table   label
            (0..256).each do |i|
                        dw  define_label :"m16u_#{i}"
            end

            m16s_table   label
            (0..256).each do |i|
                        dw  define_label :"m16s_#{i}"
            end

            m24u_table   label
            (0..256).each do |i|
                        dw  define_label :"m24u_#{i}"
            end

            m24s_table   label
            (0..256).each do |i|
                        dw  define_label :"m24s_#{i}"
            end

            (0..256).each do |i|
                define_label :"m16u_#{i}", mul_const8_24a(kh, kl, i, t:t, tt:tt, k_int24:false, signed_k:false)
                ret
            end

            (0..256).each do |i|
                define_label :"m16s_#{i}", mul_const8_24a(kh, kl, i, t:t, tt:tt, k_int24:false, signed_k:true)
                ret
            end

            (0..256).each do |i|
                define_label :"m24u_#{i}", mul_const8_24a(kh, kl, i, t:t, tt:tt, k_int24:km, signed_k:false)
                ret
            end

            (0..256).each do |i|
                define_label :"m24s_#{i}", mul_const8_24a(kh, kl, i, t:t, tt:tt, k_int24:km, signed_k:true)
                ret
            end

            multiplicand16 dw 0
            multiplicand24 db 0
        end
    end
end

class MTest1
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul_const8_24a.de"

    macro_import MTestFactory
    make_tests de
end

class MTest2
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul_const8_24a.bc"

    macro_import MTestFactory
    make_tests bc
end

include ZXLib

[MTest1, MTest2].each do |mtest_klass|
    mtest = mtest_klass.new 65536 - mtest_klass.code.bytesize
    # puts mtest.debug
    source = <<-END
       1 DEF FN m(a,b)=USR #{mtest[:test_mul16u]}: DEF FN s(a,b)=USR #{mtest[:test_mul16s]}
         DEF FN u(a,b)=USR #{mtest[:test_mul24u]}: DEF FN q(a,b)=USR #{mtest[:test_mul24s]}
      10 RANDOMIZE
      20 FOR b=0 TO 256
         LET a=INT (RND*65536): PRINT AT 0,0;a;"*";b;"=";: LET r=FN m(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         LET a=a-32768: PRINT AT 1,0;a;"*";b;"=";: LET r=FN s(a,b)
         IF r<>a*b THEN GO TO 1100
         PRINT r;"        "
         LET x=INT (RND*65536): LET y=INT (x/256): LET z=x-y*256+1: LET a=(a+32768)*z+y
         PRINT AT 2,0;a;"*";b;"=";: LET r=FN u(a,b)
         LET z=a*b
         IF z>16777215 THEN LET z=z-INT (z/16777216)*16777216
         IF r<>z THEN GO TO 1200
         PRINT r;"        "
         LET a=a-8388608: PRINT AT 3,0;a;"*";b;"=";: LET r=FN q(a,b)
         LET z=a*b
         IF z<-8388608 OR z>16777215 THEN LET z=z-INT (z/16777216)*16777216
         IF z>=8388608 THEN LET z=z-16777216
         IF r<>z THEN GO TO 1300
         PRINT r;"        "
         NEXT b
         GO TO 10
     100 INPUT "k ";a;" m ";b
         FOR b=b TO 256
         PRINT AT 0,0;"16u ";a;"*";b;"=";: LET r=FN m(a,b): PRINT r;"        "
         IF r<>a*b THEN GO TO 1000
         NEXT b
         STOP
         GO TO 100
     200 INPUT "k ";a;" m ";b
         FOR b=b TO 256
         PRINT AT 0,0;"16s ";a;"*";b;"=";: LET r=FN s(a,b): PRINT r;"        "
         IF r<>a*b THEN GO TO 1100
         NEXT b
         STOP
         GO TO 200
     300 INPUT "k ";a;" m ";b
         FOR b=b TO 256
         PRINT AT 0,0;"24u ";a;"*";b;"=";: LET r=FN u(a,b): PRINT r;"        "
         LET z=a*b
         IF z>16777215 THEN LET z=z-INT (z/16777216)*16777216
         IF r<>z THEN GO TO 1200
         NEXT b
         STOP
         GO TO 300
     400 INPUT "k ";a;" m ";b
         FOR b=b TO 256
         PRINT AT 0,0;"24s ";a;"*";b;"=";: LET r=FN q(a,b): PRINT r;"        "
         LET z=a*b
         IF z<-8388608 OR z>16777215 THEN LET z=z-INT (z/16777216)*16777216
         IF z>=8388608 THEN LET z=z-16777216
         IF r<>z THEN GO TO 1300
         NEXT b
         STOP
         GO TO 400
     999 STOP
    1000 PRINT "        "'"assertion failed(16u): ";r;"`<>`";a*b: GO TO 10000
    1100 PRINT "        "'"assertion failed(16s): ";r;"`<>`";a*b: GO TO 10000
    1200 PRINT "        "'"assertion failed(24u): ";r;"`<>`";z'"(";a*b;")": GO TO 10000
    1300 PRINT "        "'"assertion failed(24s): ";r;"`<>`";z'"(";a*b;")": GO TO 10000
    9998 STOP: RUN
    9999 CLEAR #{mtest.org-1}: LOAD ""CODE: RUN
    END
    program = Basic.parse_source source
    puts "#{mtest.class::NAME} size: #{mtest.code.bytesize}"
    %w[
        test_mul16u
        test_mul24u
        test_mul16s
        test_mul24s
        find_args
        multiply
        m16u_table
        m16s_table
        m24u_table
        m24s_table
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{mtest[label].to_s 16} - #{mtest[label]}, size: #{mtest['+'+label]}"
    end
    puts "mul16u   0..256 size: " + (0..256).inject(0) {|a, n| a+mtest["+m16u_#{n}"]}.to_s
    puts "mul16s   0..256 size: " + (0..256).inject(0) {|a, n| a+mtest["+m16s_#{n}"]}.to_s
    puts "mul24u   0..256 size: " + (0..256).inject(0) {|a, n| a+mtest["+m24u_#{n}"]}.to_s
    puts "mul24s   0..256 size: " + (0..256).inject(0) {|a, n| a+mtest["+m24s_#{n}"]}.to_s

    program.save_tap "#{mtest.class::NAME}.tap", line:9999
    mtest.save_tap "#{mtest.class::NAME}.tap", append:true
end
