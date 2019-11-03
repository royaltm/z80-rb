require 'z80'
require 'zxlib/sys'
require 'zxlib/basic'

class MTestFactory
    include Z80

    module Macros
        include ::ZXLib::Sys::Macros
        include ::Z80::MathInt::Macros
        def make_tests(tt=de)
            label_import  ZXLib::Sys

            th, tl = tt.split
            sgn = tl
            kk = if tt == de then bc else de end
            kh, kl = kk.split

            ns :test_pos do
                        call find_args
                        report_error_unless Z, "Q Parameter error"
                        read_positive_int_value kh, kl
                        report_error_unless Z, "A Invalid argument"
                        ld   a, kh
                        anda a
                        report_error_unless Z, "B Integer out of range"
                        inc  hl
                        call find_args.seek_next
                        report_error_unless Z, "Q Parameter error"
                        read_positive_int_value th, tl
                        report_error_unless Z, "A Invalid argument"
                        cp16n th, tl, 257
                        report_error_unless C, "B Integer out of range"
                        inc  hl
                        call find_args.seek_next
                        jr   NZ, skip_3
                        push tt
                        read_positive_int_value th, tl
                        report_error_unless Z, "A Invalid argument"
                        ld16 hl, tt
                        pop  tt
                        ld   ix, madd_table
                        jr   skmul
                skip_3  ld   ix, mul_table
                skmul   call multiply
                        ld16 bc, hl
                        ret
            end

            ns :test_sig do
                        call find_args
                        report_error_unless Z, "Q Parameter error"
                        read_integer_value kh, kl, sgn
                        report_error_unless Z, "A Invalid argument"
                        ld   a, kh
                        xor  sgn
                        report_error_unless Z, "B Integer out of range"
                        inc  hl
                        call find_args.seek_next
                        report_error_unless Z, "Q Parameter error"
                        read_positive_int_value th, tl
                        report_error_unless Z, "A Invalid argument"
                        cp16n th, tl, 257
                        report_error_unless C, "B Integer out of range"
                        inc  hl
                        call find_args.seek_next
                        jr   NZ, skip_3
                        push tt
                        read_positive_int_value th, tl
                        report_error_unless Z, "A Invalid argument"
                        ld16 hl, tt
                        pop  tt
                        ld   ix, maddsig_table
                        jr   skmul
                skip_3  ld   ix, msig_table
                skmul   call multiply
                        ld16 bc, hl
                        ret
            end

            find_args   find_def_fn_args 1, subroutine:true

            ns :multiply do # hl + kl * tt
                        sla  tl
                        rl   th
                        add  ix, tt
                        ld   a, [ix + 0]
                        ld   b, [ix + 1]
                        ld   ixl, a
                        ld   ixh, b
                        jp   (ix)
            end

            mul_table   label
            (0..256).each do |i|
                        dw  define_label :"mul_#{i}"
            end

            msig_table   label
            (0..256).each do |i|
                        dw  define_label :"msig_#{i}"
            end

            madd_table   label
            (0..256).each do |i|
                        dw  define_label :"madd_#{i}"
            end

            maddsig_table   label
            (0..256).each do |i|
                        dw  define_label :"maddsig_#{i}"
            end

            (0..256).each do |i|
                define_label :"mul_#{i}", mul_const(kl, i, tt:tt, clrhl:true, signed_k:false)
                ret
            end

            (0..256).each do |i|
                define_label :"msig_#{i}", mul_const(kl, i, tt:tt, clrhl:true, signed_k:true)
                ret
            end

            (0..256).each do |i|
                define_label :"madd_#{i}", mul_const(kl, i, tt:tt, clrhl:false, signed_k:false)
                ret
            end

            (0..256).each do |i|
                define_label :"maddsig_#{i}", mul_const(kl, i, tt:tt, clrhl:false, signed_k:true)
                ret
            end
        end
    end
end

class MTest1
    include Z80
    include Z80::TAP

    def name
        "test.math_i.mul_const.de"
    end

    macro_import MTestFactory
    make_tests de
end

class MTest2
    include Z80
    include Z80::TAP

    def name
        "test.math_i.mul_const.bc"
    end

    macro_import MTestFactory
    make_tests bc
end

include ZXLib

[MTest1, MTest2].each do |mtest_klass|
    mtest = mtest_klass.new 65536 - mtest_klass.code.bytesize
    # puts mtest.debug
    program = Basic.parse_source <<-END
       1 DEF FN n(x)=x-(65536 AND x>=32768): DEF FN m(a,b)=USR #{mtest[:test_pos]}: DEF FN s(a,b)=FN n(USR #{mtest[:test_sig]}): DEF FN a(a,b,c)=USR #{mtest[:test_pos]}: DEF FN q(a,b,c)=FN n(USR #{mtest[:test_sig]})
      10 RANDOMIZE
      20 FOR b=0 TO 256
         LET a=INT (RND*256): LET r=FN m(a,b): PRINT AT 0,0;a;" * ";b;" = ";
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         LET c=INT (RND*256): LET r=FN a(a,b,c): PRINT AT 1,0;c;" + ";a;" * ";b;" = ";
         IF r<>c+a*b THEN GO TO 2000
         PRINT r;"        "
         LET a=a-128: LET r=FN s(a,b): PRINT AT 2,0;a;" * ";b;" = ";
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         LET r=FN q(a,b,c): PRINT AT 3,0;c;" + ";a;" * ";b;" = ";
         IF r<>c+a*b THEN GO TO 2000
         PRINT r;"        "
         NEXT b
         GO TO 10
    1000 PRINT '"assertion failed: ";r;"`<>`";a*b: GO TO 10000
    2000 PRINT '"assertion failed: ";r;"`<>`";c+a*b: GO TO 10000
    9998 STOP: RUN
    9999 CLEAR #{mtest.org-1}: LOAD ""CODE: RUN
    END
    puts mtest.name
    %w[
        test_pos
        test_sig
        find_args
        multiply
        mul_table
        msig_table
        madd_table
        maddsig_table
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{mtest[label].to_s 16} - #{mtest[label]}, size: #{mtest.code.bytesize}"
    end

    program.save_tap "#{mtest.name}.tap", line:9999
    mtest.save_tap "#{mtest.name}.tap", append:true
end
