require 'z80'
require 'zxlib/sys'
require 'zxlib/basic'

class MTestFactory
    include Z80

    module Macros
        include ::ZXLib::Sys::Macros
        include ::ZXLib::Math::Macros
        include ::Z80::MathInt::Macros
        def make_tests(tt=de, signed:false)
            label_import  ZXLib::Sys

            th, tl = tt.split
            sgn = tl
            kk = if tt == de then bc else de end
            kh, kl = kk.split

            ns :test_mul do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                if signed
                        read_integer_value kh, kl, tl
                        jr   NZ, error_a.err
                        ld   a, kh
                        add  a, a
                        sbc  a, a
                        xor  tl
                        jr   NZ, error_b.err
                else
                        read_positive_int_value kh, kl
                        jr   NZ, error_a.err
                end
                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_positive_int_value th, tl
                error_a report_error_unless Z, "A Invalid argument"
                        cp16n th, tl, 257
                error_b report_error_unless C, "B Integer out of range"
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
                        ld   ix, madd_table
                        jr   skmul

                skip_3  ld   ix, mul_table
                skmul   call multiply
                        ld   d, a
                        ld   c, h
                        ld   b, l
                        ld   e, 0
                if signed
                sknegs  jp   P, skipzch
                        neg_int d, c, b, t:e, t_is_zero:true
                        ora  0x80 # SF = 1
                skipzch integer32_to_fp e, d, c, b, sgn:M
                else
                        jr   NZ, nozerch
                        anda a
                        jr   Z, skipzch
                error_6 report_error "6 Number too big" # error raised when ZF flag check failed
                nozerch anda a
                        jr   Z, error_6.err
                skipzch integer32_to_fp e, d, c, b, sgn:nil
                end
            end

            return_fp   return_with_fp restore_iy:nil, restore_hl_alt:nil

            find_args   find_def_fn_args 1, subroutine:true

            ns :multiply do # hl + kk * tt
                        sla  tl
                        rl   th
                        add  ix, tt
                        ld   tl, [ix + 0]
                        ld   th, [ix + 1]
                        ld   ixl, tl
                        ld   ixh, th
                        anda a
                        jp   (ix)
            end

            mul_table   label
            (0..256).each do |i|
                        dw  define_label :"mul_#{i}"
            end

            madd_table   label
            (0..256).each do |i|
                        dw  define_label :"madd_#{i}"
            end

            (0..256).each do |i|
                define_label :"mul_#{i}", mul_const8_24(kh, kl, i, t:kl, tt:tt, clrahl:true, signed_k:signed)
                ret
            end

            (0..256).each do |i|
                define_label :"madd_#{i}", mul_const8_24(kh, kl, i, t:kl, tt:tt, clrahl:false, signed_k:signed)
                ret
            end
        end
    end
end

class MTest1
    include Z80
    include Z80::TAP

    SIGNED = false
    NAME = "test.math_i.mul_const8_24.unsigned.de"

    macro_import MTestFactory
    make_tests de, signed:SIGNED
end

class MTest2
    include Z80
    include Z80::TAP

    SIGNED = false
    NAME = "test.math_i.mul_const8_24.unsigned.bc"

    macro_import MTestFactory
    make_tests bc, signed:SIGNED
end

class MTest3
    include Z80
    include Z80::TAP

    SIGNED = true
    NAME = "test.math_i.mul_const8_24.signed.de"

    macro_import MTestFactory
    make_tests de, signed:SIGNED
end

class MTest4
    include Z80
    include Z80::TAP

    SIGNED = true
    NAME = "test.math_i.mul_const8_24.signed.bc"

    macro_import MTestFactory
    make_tests bc, signed:SIGNED
end

include ZXLib

[MTest1, MTest2, MTest3, MTest4].each do |mtest_klass|
    mtest = mtest_klass.new 65536 - mtest_klass.code.bytesize
    # puts mtest.debug
    source = <<-END
       1 DEF FN m(a,b)=USR #{mtest[:test_mul]}: DEF FN a(a,b,c)=USR #{mtest[:test_mul]}
      10 RANDOMIZE
      20 FOR b=0 TO 256
    END
    if mtest_klass::SIGNED
        source << <<-END
         LET a=INT (RND*65536)-32768: PRINT AT 0,0;a;"*";b;"=";: LET r=FN m(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         LET c=INT (RND*65536)*INT (RND*256)-8388608: LET z=c+a*b
         IF z>=8388608 THEN LET z=z-16777216
         IF z<-8388608 THEN LET z=z+16777216
        END
    else
        source << <<-END
         LET a=INT (RND*65536): PRINT AT 0,0;a;"*";b;"=";: LET r=FN m(a,b)
         IF r<>a*b THEN GO TO 1000
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
    2000 PRINT "        "'"assertion failed: ";r;"`<>`";z: GO TO 10000
    9998 STOP: RUN
    9999 CLEAR #{mtest.org-1}: LOAD ""CODE: RUN
    END
    program = Basic.parse_source source
    puts mtest.class::NAME
    %w[
        test_mul
        find_args
        multiply
        mul_table
        madd_table
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{mtest[label].to_s 16} - #{mtest[label]}, size: #{mtest.code.bytesize}"
    end

    program.save_tap "#{mtest.class::NAME}.tap", line:9999
    mtest.save_tap "#{mtest.class::NAME}.tap", append:true
end
