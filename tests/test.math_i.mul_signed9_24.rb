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

            ns :test_full do
                        ld   hl, multadd
                        ld   [routine_sum], hl
                        ld   hl, multiply
                        ld   [routine], hl
                        jp   test_mul
            end

            ns :test_mlim do
                        ld   hl, madd_mlim
                        ld   [routine_sum], hl
                        ld   hl, mul_mlim
                        ld   [routine], hl
                        jp   test_mul
            end

            ns :and24 do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_integer32_value e, d, c, b
                error_a report_error_unless NC, "A Invalid argument"
                        ld   e, 0
                        jr   Z, ri_pos
                        neg_int d, c, b, t:e, t_is_zero:true
                ri_pos  ld   a, d
                        anda a
                        jp   P, ro_pos
                        neg_int d, c, b, t:e, t_is_zero:true
                        ora  0x80 # SF = 1
                ro_pos   integer32_to_fp e, d, c, b, sgn:M
                        jp   return_fp
            end

            forward_ix  jp   (ix)

            ns :test_mul do
                        call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_integer32_value de, bc
                        jr   C, error_a.err
                        push af
                        xor  a
                        ora  d
                        jr   NZ, error_b.err
                        ld   a, 0x80
                        cp   e
                        jr   C, error_b.err
                        pop  af
                        jr   Z, k_pos
                        neg_int e, b, c, t:d, t_is_zero:true
                k_pos   ld   a, e
                        push bc          # tt(k)
                        push af          # sgn(k)

                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_integer_value th, kl, sgn
                error_a report_error_unless Z, "A Invalid argument"
                        ld   a, th
                        xor  sgn
                error_b report_error_unless Z, "B Integer out of range"

                        inc  hl
                        call find_args.seek_next
                        ld   ix, [routine]
                        jr   NZ, skip_3

                        push kk          # sgn|kl
                        read_integer32_value de, bc
                        jr   C, error_b.err
                        push af
                        xor  a
                        ora  d
                        jr   NZ, error_b.err
                        ld   a, 0x80
                        cp   e
                        jr   C, error_b.err
                        pop  af
                        jr   Z, addpos
                        neg_int e, b, c, t:d, t_is_zero:true
                addpos  ld16 hl, bc      # hl: result
                        ld   a, e
                        ex   af, af      # a': result MSB
                        pop  kk          # sgn|kl
                        ld   ix, [routine_sum]

                skip_3  ld   th, sgn     # sgn(m)
                        pop  af          # sgn(k)
                        ld   sgn, a      # sgn(k)
                        ld   a, th       # sgn(m)
                        pop  tt          # tt(k)
                        anda a           # SF = sgn(m)

                        call forward_ix  # sgn|tt (k) * SF|kl (m)

                skmul   ld   d, a
                        ld   c, h
                        ld   b, l
                        ld   e, 0
                        anda a
                sknegs  jp   P, skipzch  # result positive
                        neg_int d, c, b, t:e, t_is_zero:true
                        ora  0x80 # SF = 1
                skipzch integer32_to_fp e, d, c, b, sgn:M
            end

            return_fp   return_with_fp restore_iy:nil, restore_hl_alt:nil

            find_args   find_def_fn_args 1, subroutine:true

            routine     dw  multiply
            routine_sum dw  multadd

            # a|hl = kk * tt
            multiply    mul_signed9_24(sgn, th, tl, kl, tt:tt, m_pos_cond:P, m_full_range: true, optimize:optimize)
                        ret
            # a|hl = kk * tt
            mul_mlim    mul_signed9_24(sgn, th, tl, kl, tt:tt, m_pos_cond:P, m_full_range:false, optimize:optimize)
                        ret
            # a|hl += kk * tt
            ns :multadd do
                        mul_signed9_24(sgn, th, tl, kl, tt:tt, m_pos_cond:P, m_full_range: true, optimize:optimize) do
                          ex  af, af
                        end
            end
                        ret
            # a|hl += kk * tt
            ns :madd_mlim do
                        mul_signed9_24(sgn, th, tl, kl, tt:tt, m_pos_cond:P, m_full_range:false, optimize:optimize) do
                          ex  af, af
                        end
            end
                        ret
        end
    end
end

class MTest1
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul_signed9_24.time"

    macro_import MTestFactory
    make_tests de, :time
end

class MTest2
    include Z80
    include Z80::TAP

    NAME = "test.math_i.mul_signed9_24.size"

    macro_import MTestFactory
    make_tests de, :size
end

include ZXLib

[MTest1, MTest2].each do |mtest_klass|
    mtest = mtest_klass.new 65536 - mtest_klass.code.bytesize
    # puts mtest.debug
    source = <<-END
       1 DEF FN r(x)=USR #{mtest[:and24]}: DEF FN f(a,b)=USR #{mtest[:test_full]}: DEF FN a(a,b,c)=USR #{mtest[:test_full]}: DEF FN m(a,b)=USR #{mtest[:test_mlim]}: DEF FN n(a,b,c)=USR #{mtest[:test_mlim]}
      10 RANDOMIZE
      20 FOR b=-256 TO 255
         LET a=INT (RND*65536)*INT (RND*256)-8388608: LET s=FN r(a*b)
         LET c=INT (RND*65536)*INT (RND*256)-8388608: LET z=FN r(c+a*b)
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN f(a,b)
         IF r<>s THEN GO TO 1000
         PRINT r;"        "
         IF b=-256 THEN GO TO 50
         LET r=FN m(a,b): IF r<>s THEN GO TO 1000
      50 PRINT AT 1,0;c;"+";a;"*";b;"=";: LET r=FN a(a,b,c)
         IF r<>z THEN GO TO 2000
         PRINT r;"        "
         IF b=-256 THEN GO TO 60
         LET r=FN n(a,b,c): IF r<>z THEN GO TO 2000
      60 NEXT b
         GO TO 10
     100 INPUT "a ";a, "b ";b: LET s=FN r(a*b)
         PRINT AT 0,0;a;"*";b;"=";: LET r=FN f(a,b)
         IF r<>s THEN GO TO 1000
         PRINT r;"        "
         INPUT "c ";c: LET z=FN r(c+a*b)
         PRINT AT 1,0;c;"+";a;"*";b;"=";: LET r=FN a(a,b,c)
         IF r<>z THEN GO TO 2000
         PRINT r;"        "
         GO TO 100
    1000 PRINT "        "'"assertion failed: ";r;"`<>`";s: GO TO 10000
    2000 PRINT "        "'"assertion failed: ";r;"`<>`";z: GO TO 10000
    9998 STOP: RUN
    9999 CLEAR #{mtest.org-1}: LOAD ""CODE: RUN
    END
    program = Basic.parse_source source
    puts "#{mtest.class::NAME} size: #{mtest.code.bytesize}"
    %w[
        test_mul
        and24
        find_args
        multiply
        multadd
        mul_mlim
        madd_mlim
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{mtest[label].to_s 16} - #{mtest[label]}, size: #{mtest['+'+label]}"
    end

    program.save_tap "#{mtest.class::NAME}.tap", line:9999
    mtest.save_tap "#{mtest.class::NAME}.tap", append:true
end
