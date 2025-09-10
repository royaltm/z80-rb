require 'z80'
require 'zxlib/sys'
require 'zxlib/basic'

class MTestFactory
    include Z80

    module Macros
        include ::ZXLib::Sys::Macros
        include ::ZXLib::Math::Macros
        include ::Z80::MathInt::Macros
        def make_tests(tt=de, optimize=:time, signed:false)
            label_import  ZXLib::Sys

            th, tl = tt.split
            kk = if tt == de then bc else de end
            kh, kl = kk.split
            sgn = kh

            ns :test_mul24_8 do
                        ld   ix, multiply
                        jr   test_mul
            end

            ns :test_mul24_8a do
                        ld   ix, multiply_a
                        jr   test_mul
            end

            forward_ix  jp   (ix)

            ns :test_mul do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                if signed
                        read_integer_value th, tl, sgn
                        jr   NZ, error_a.err
                        sign_extend(a, th)
                        xor  sgn
                        jr   NZ, error_b.err
                        ld   a, sgn
                else
                        read_positive_int_value th, tl
                        jr   NZ, error_a.err
                end
                        ex   af, af # a': sgn or 0
                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_positive_int_value kh, kl
                error_a report_error_unless Z, "A Invalid argument"
                        cp16n kh, kl, 256
                error_b report_error_unless C, "B Integer out of range"
                        ex   af, af # a: sgn or 0
                        ld   sgn, a
                        inc  hl
                        call find_args.seek_next
                        jr   NZ, skip_3

                        push tt
                        push kk
                        read_integer32_value de, bc
                        jr   C, error_b.err
                        ex   af, af
                        xor  a
                        ora  d
                        jr   NZ, error_b.err
                if signed
                        ld   a, 0x80
                        cp   e
                        jr   C, error_b.err
                        ex   af, af
                        jr   Z, addpos
                        neg_int e, b, c, t:d, t_is_zero:true
                end
                addpos  ld16 hl, bc
                        ld   a, e
                        pop  kk
                        pop  tt
                        call multadd
                        anda a
                        jr   skmul

                skip_3  call forward_ix
                unless signed
                        jr   C, error_6.err
                        jr   NZ, nozerch
                        anda a
                        jr   Z, skmul
                error_6 report_error "6 Number too big" # error raised when ZF flag check failed
                nozerch anda a
                        jr   Z, error_6.err
                end
                skmul   ld   d, a
                        ld   c, h
                        ld   b, l
                        ld   e, 0
                if signed
                sknegs  jp   P, skipzch
                        neg_int d, c, b, t:e, t_is_zero:true
                        ora  0x80 # SF = 1
                skipzch integer32_to_fp e, d, c, b, sgn:M
                else
                        integer32_to_fp e, d, c, b, sgn:nil
                end
            end

            return_fp   return_with_fp restore_iy:nil, restore_hl_alt:nil

            find_args   find_def_fn_args 1, subroutine:true

            # a|hl = kk * tt
            multiply    mul8_24(th, tl, kl, t:sgn, tt:tt, clrahl: true, k_int24:signed, optimize:optimize)
                        ret

            multiply_a  mul8_24a(th, tl, kl, t:sgn, tt:tt, k_int24:signed, optimize:optimize)
                        anda a # clear CF
                        ret

            # a|hl += kk * tt
            multadd     mul8_24(th, tl, kl, t:sgn, tt:tt, clrahl:false, k_int24:signed, optimize:optimize)
                        ret
        end
    end
end

class MTest1
    include Z80
    include Z80::TAP

    SIGNED = false
    NAME = "test.math_i.mul8_24.unsigned_16.time"

    macro_import MTestFactory
    make_tests de, :time, signed:SIGNED
end

class MTest2
    include Z80
    include Z80::TAP

    SIGNED = false
    NAME = "test.math_i.mul8_24.unsigned_16.size"

    macro_import MTestFactory
    make_tests de, :size, signed:SIGNED
end

class MTest3
    include Z80
    include Z80::TAP

    SIGNED = true
    NAME = "test.math_i.mul8_24.signed_24.time"

    macro_import MTestFactory
    make_tests de, :time, signed:SIGNED
end

class MTest4
    include Z80
    include Z80::TAP

    SIGNED = true
    NAME = "test.math_i.mul8_24.signed_24.size"

    macro_import MTestFactory
    make_tests de, :size, signed:SIGNED
end

include ZXLib

[MTest1, MTest2, MTest3, MTest4].each do |mtest_klass|
    mtest = mtest_klass.new 65536 - mtest_klass.code.bytesize
    # puts mtest.debug
    source = <<-END
       1 DEF FN m(a,b)=USR #{mtest[:test_mul24_8]}: DEF FN n(a,b)=USR #{mtest[:test_mul24_8a]}: DEF FN a(a,b,c)=USR #{mtest[:test_mul]}
      10 RANDOMIZE
      20 FOR b=0 TO 255
    END
    if mtest_klass::SIGNED
        source << <<-END
         LET a=INT (RND*65536)-32768: PRINT AT 0,0;a;"*";b;"=";: LET r=FN m(a,b)
         IF r<>a*b THEN GO TO 1000
         LET r=FN n(a,b): IF r<>a*b THEN GO TO 1100
         PRINT r;"        "
         LET c=INT (RND*65536)*INT (RND*256)-8388608: LET z=c+a*b
         IF z>=8388608 THEN LET z=z-16777216
         IF z<-8388608 THEN LET z=z+16777216
        END
    else
        source << <<-END
         LET a=INT (RND*65536): PRINT AT 0,0;a;"*";b;"=";: LET r=FN m(a,b)
         IF r<>a*b THEN GO TO 1000
         LET r=FN n(a,b): IF r<>a*b THEN GO TO 1100
         PRINT r;"        "
         LET c=INT (RND*65536)*INT (RND*256): LET z=c+a*b
         IF z>=16777216 THEN LET z=z-16777216
        END
    end
    source << <<-END
         PRINT AT 1,0;c;"+";a;"*";b;"=";: LET r=FN a(a,b,c)
         IF r<>z THEN GO TO 2000
         PRINT r;"        "
         NEXT b
         GO TO 10
    1000 PRINT "        "'"assertion failed: ";r;"`<>`";a*b: GO TO 10000
    1100 PRINT "        "'"assertion failed(a): ";r;"`<>`";a*b: GO TO 10000
    2000 PRINT "        "'"assertion failed: ";r;"`<>`";z: GO TO 10000
    9998 STOP: RUN
    9999 CLEAR #{mtest.org-1}: LOAD ""CODE: RUN
    END
    program = Basic.parse_source source
    puts "#{mtest.class::NAME} size: #{mtest.code.bytesize}"
    %w[
        test_mul
        find_args
        multiply
        multiply_a
        multadd
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{mtest[label].to_s 16} - #{mtest[label]}, size: #{mtest['+'+label]}"
    end

    program.save_tap "#{mtest.class::NAME}.tap", line:9999
    mtest.save_tap "#{mtest.class::NAME}.tap", append:true
end
