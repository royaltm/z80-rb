require 'z80'
require 'zxlib/sys'
require 'zxlib/basic'

OPTIMIZE = :time
# OPTIMIZE = :size

# OPTIMIZE = :k_rshift_sum
# OPTIMIZE = :neg_k_rshift_sum
# OPTIMIZE = :neg_split_hik_lshift_sum
# OPTIMIZE = :neg_sum_lshift_add_k
# OPTIMIZE = :split_hik_lshift_sum
# OPTIMIZE = :split_lok_rshift_sum
# OPTIMIZE = :sum_lshift_add_k

SKIP_ACC = [:k_rshift_sum, :neg_k_rshift_sum, :neg_split_hik_lshift_sum].include?(OPTIMIZE)

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

            test_pos_a  ld   ix, mula_table
                        jr   test_pos.skip_ix

            ns :test_pos do
                        ld   ix, mul_table
                skip_ix call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_positive_int_value kh, kl
                error_a report_error_unless Z, "A Invalid argument"
                        ld   a, kh
                        anda a
                error_b report_error_unless Z, "B Integer out of range"
                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_positive_int_value th, tl
                        jr   NZ, error_a.err
                        cp16n th, tl, 257
                        jr   NC, error_b.err
                        call multiply
                        ld16 bc, hl
                        ret
            end

            test_sig_a  ld   ix, msiga_table
                        jr   test_sig.skip_ix

            ns :test_sig do
                        ld   ix, msig_table
                skip_ix call find_args
                error_q report_error_unless Z, "Q Parameter error"
                        read_integer_value kh, kl, sgn
                error_a report_error_unless Z, "A Invalid argument"
                        ld   a, kh
                        xor  sgn
                error_b report_error_unless Z, "B Integer out of range"
                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        read_positive_int_value th, tl
                        jr   NZ, error_a.err
                        cp16n th, tl, 257
                        jr   NC, error_b.err
                        call multiply
                        ld16 bc, hl
                        ret
            end

            find_args   find_def_fn_args 1, subroutine:true

            ns :multiply do # hl = kl * tt
                        sla  tl
                        rl   th
                        add  ix, tt
                        ld   a, [ix + 0]
                        ld   b, [ix + 1]
                        ld   ixl, a
                        ld   ixh, b
                        jp   (ix)
            end

            mula_table  label

            (0..256).each do |i|
                        dw  define_label :"mula_#{i}"
            end unless SKIP_ACC

            mul_table   label
            (0..256).each do |i|
                        dw  define_label :"mul_#{i}"
            end

            msiga_table label
            (0..256).each do |i|
                        dw  define_label :"msiga_#{i}"
            end unless SKIP_ACC

            msig_table  label
            (0..256).each do |i|
                        dw  define_label :"msig_#{i}"
            end

            (0..256).each do |i|
                define_label :"mul_#{i}", mul_const_ex(kl, i, tt:tt, signed_k:false, use_a:false, optimize: OPTIMIZE)
                ret
            end

            (0..256).each do |i|
                define_label :"msig_#{i}", mul_const_ex(kl, i, tt:tt, signed_k:true, use_a:false, optimize: OPTIMIZE)
                ret
            end

            unless SKIP_ACC
                (0..256).each do |i|
                    define_label :"mula_#{i}", mul_const_ex(kl, i, tt:tt, signed_k:false, use_a:true, optimize: OPTIMIZE)
                    ret
                end

                (0..256).each do |i|
                    define_label :"msiga_#{i}", mul_const_ex(kl, i, tt:tt, signed_k:true, use_a:true, optimize: OPTIMIZE)
                    ret
                end
            end
        end
    end
end

class MTest1
    include Z80
    include Z80::TAP

    def name
        "test.math_i.mul_const_ex.de"
    end

    macro_import MTestFactory
    make_tests de
end

class MTest2
    include Z80
    include Z80::TAP

    def name
        "test.math_i.mul_const_ex.bc"
    end

    macro_import MTestFactory
    make_tests bc
end

include ZXLib

[MTest1, MTest2].each do |mtest_klass|
    mtest = mtest_klass.new 65536 - mtest_klass.code.bytesize
    # puts mtest.debug
    puts "OPTIMIZE: #{OPTIMIZE}"
    program = Basic.parse_source <<-END
       1 DEF FN n(x)=x-(65536 AND x>=32768): DEF FN m(a,b)=USR #{mtest[:test_pos]}: DEF FN s(a,b)=FN n(USR #{mtest[:test_sig]}): DEF FN a(a,b)=USR #{mtest[:test_pos_a]}: DEF FN q(a,b)=FN n(USR #{mtest[:test_sig_a]})
      10 RANDOMIZE
      20 FOR b=0 TO 256
         LET a=INT (RND*256): PRINT AT 0,0;a;" * ";b;" = ";: LET r=FN m(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         PRINT AT 1,0;a;" * ";b;" = ";: LET r=FN a(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         LET a=a-128: PRINT AT 2,0;a;" * ";b;" = ";: LET r=FN s(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         PRINT AT 3,0;a;" * ";b;" = ";: LET r=FN q(a,b)
         IF r<>a*b THEN GO TO 1000
         PRINT r;"        "
         NEXT b
         GO TO 10
    1000 PRINT "        "'"assertion failed: ";r;"`<>`";a*b: GO TO 10000
    9998 STOP: RUN
    9999 CLEAR #{mtest.org-1}: LOAD ""CODE: RUN
    END
    puts "#{mtest.name} size: #{mtest.code.bytesize}"
    %w[
        test_pos
        test_pos_a
        test_sig
        test_sig_a
        find_args
        multiply
        mula_table
        mul_table
        msiga_table
        msig_table
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{mtest[label].to_s 16} - #{mtest[label]}, size: #{mtest['+'+label]}"
    end
    puts "mul     0..256 size: " + (0..256).inject(0) {|a, n| a+mtest["+mul_#{n}"]}.to_s
    unless SKIP_ACC
        puts "mula    0..256 size: " + (0..256).inject(0) {|a, n| a+mtest["+mula_#{n}"]}.to_s
    end
    puts "msig    0..256 size: " + (0..256).inject(0) {|a, n| a+mtest["+msig_#{n}"]}.to_s
    unless SKIP_ACC
        puts "msiga   0..256 size: " + (0..256).inject(0) {|a, n| a+mtest["+msiga_#{n}"]}.to_s
    end
    program.save_tap "#{mtest.name}.tap", line:9999
    mtest.save_tap "#{mtest.name}.tap", append:true
end
