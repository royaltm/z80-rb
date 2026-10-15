require 'z80'
require 'zxlib/sys'
require 'zxlib/basic'

OPTIONS = {
    # check0: true,
    # check0_far: false,
    # check1: true,
    # check1_far: false,
    # modulo: false,
    jump_rel: false,
    ignore_cf: false,
    # subroutine: false,
    skip_clra: false
}

class DTestFactory
    include Z80

    module Macros
        include ::ZXLib::Sys::Macros
        include ::ZXLib::Math::Macros
        include ::Z80::MathInt::Macros

        def make_tests(**options)
            label_import  ZXLib::Sys

            tt = de
            th, tl = tt.split
            kk = if tt == de then bc else de end
            kh, kl = kk.split

            # > hl: dividend, kk: divisor, a: 0
            ns :get_args do
                        call find_args
              error_q   report_error_unless Z, "Q Parameter error"
                        call read_uint # 0..65535
                        push kk     # k: kh|kl

                        inc  hl
                        call find_args.seek_next
                        jr   NZ, error_q.err
                        call read_uint # 0..255
                        ld   a, kh
                        ora  a
              error_b   report_error_unless Z, "B Integer out of range"
                        pop  hl # dividend
                        ret
            end

            remainder       db 0
            carryover       db 0

            ns :test_divmod do
                        call get_args
                        call divide
                        jr   get_results
            end

            ns :test_modulo do
                        call get_args
                        call modulo
                        jr   get_results
            end

            ns :test_divmod_sub do
                        call get_args
                        call divide_sub
                        jr   get_results
            end

            ns :test_modulo_sub do
                        call get_args
                        call modulo_sub
                        # jr   get_results
                        # fallthru to get_results
            end

            ns :get_results do
                        anda a if options[:check0] && options[:ignore_cf]
                store   ld16 bc, hl
                        ld   [remainder], a
                        sbc  a, a
                        ld   [carryover], a
                        ret
            end

            check0      label
            check1      label
                        pop  tt
                        jr   get_results.store

            find_args   find_def_fn_args 1, subroutine:true

            ns :read_uint do
                        read_positive_int_value kh, kl
              error_a   report_error_unless Z, "A Invalid argument"
                        inc  hl
                        ret
            end

            options = options.map do |key, val|
                case key
                when :check0, :check1
                    case val
                    when :ret
                        [key, val]
                    when Symbol
                        [key, self.define_label(val)]
                    else
                        [key, val]
                    end
                else
                    [key, val]
                end
            end.to_h

            dc! "****************************************"
            dc! "***          DIVMOD  INLINE          ***"
            dc! "****************************************"
            divide      divmod8(kl, **options.merge({modulo:false, subroutine: false}))
                        ret

            dc! "****************************************"
            dc! "***          MODULO  INLINE          ***"
            dc! "****************************************"
            modulo      divmod8(kl, **options.merge({modulo:true, subroutine: false}))
                        ret

            dc! "****************************************"
            dc! "***        DIVMOD  SUBROUTINE        ***"
            dc! "****************************************"
            divide_sub  divmod8(kl, **options.merge({modulo:false, subroutine: true}))

            dc! "****************************************"
            dc! "***        MODULO  SUBROUTINE        ***"
            dc! "****************************************"
            modulo_sub  divmod8(kl, **options.merge({modulo:true, subroutine: true}))
        end
    end
end

class DTest1
    include Z80
    include Z80::TAP

    NAME = "test.math_i.divmod8.check0_1"
    CHECK0 = true

    macro_import DTestFactory
    make_tests **OPTIONS.merge({
            check0:true, check0_far:false,
            check1:true, check1_far:false,
    })
end

class DTest2
    include Z80
    include Z80::TAP

    NAME = "test.math_i.divmod8.check0"
    CHECK0 = true

    macro_import DTestFactory
    make_tests **OPTIONS.merge({
            check0:true, check0_far:false,
            check1:false, check1_far:false,
    })
end

class DTest3
    include Z80
    include Z80::TAP

    NAME = "test.math_i.divmod8.check1"
    CHECK0 = false

    macro_import DTestFactory
    make_tests **OPTIONS.merge({
            check0:false, check0_far:false,
            check1:true, check1_far:false,
    })
end

class DTest4
    include Z80
    include Z80::TAP

    NAME = "test.math_i.divmod8.nochecks"
    CHECK0 = false

    macro_import DTestFactory
    make_tests **OPTIONS.merge({
            check0:false, check0_far:false,
            check1:false, check1_far:false,
    })
end

class DTest5
    include Z80
    include Z80::TAP

    NAME = "test.math_i.divmod8.check0_1_far"
    CHECK0 = true

    macro_import DTestFactory
    make_tests **OPTIONS.merge({
            check0: :check0, check0_far:true,
            check1: :check1, check1_far:true,
    })
end


include ZXLib

[DTest1, DTest2, DTest3, DTest4, DTest5].each do |dtest_klass|
    minm = if dtest_klass::CHECK0 then 0 else 1 end
    dtest = dtest_klass.new 65536 - dtest_klass.code.bytesize
    File.open("#{dtest.class::NAME}.out", "w") do |out|
        out.puts dtest.debug
    end
    source = <<-END
       0 DEF FN d(k,m)=USR #{dtest[:test_divmod]}: DEF FN m(k,m)=USR #{dtest[:test_modulo]}: DEF FN s(k,m)=USR #{dtest[:test_divmod_sub]}: DEF FN o(k,m)=USR #{dtest[:test_modulo_sub]}: DEF FN c()=PEEK #{dtest[:carryover]}: DEF FN r()=PEEK #{dtest[:remainder]}
      10 RANDOMIZE
      20 FOR k=0 TO 255: FOR m=#{minm} TO 255
         GO SUB 200
         NEXT m: NEXT k
      50 RANDOMIZE
         FOR m=#{minm} TO 255
         LET k=INT(RND*65536)
         GO SUB 200
         NEXT m
         GO TO 50
     100 INPUT "k ";k, "m ";m
         GO SUB 200
         GO TO 100
     200 PRINT AT 0,0;k;"/";m;"=";: LET r=FN d(k,m)
         PRINT r;"        ": GO SUB 500
         PRINT AT 1,0;k;"/";m;"=";: LET r=FN s(k,m)
         PRINT r;"        ": GO SUB 500
         PRINT AT 2,0;k;"%";m;"=";: LET r=FN m(k,m)
         PRINT FN r();"        ": GO SUB 550
         PRINT AT 3,0;k;"%";m;"=";: LET r=FN o(k,m)
         PRINT FN r();"        ": GO SUB 550
         RETURN
     500 #{unless dtest.class::CHECK0 then 'REM ' end}IF FN c()<>0 THEN GO TO 510
         #{unless dtest.class::CHECK0 then 'REM ' end}IF m=0 THEN GO TO 1200
         IF r<>INT(k/m) THEN GO TO 1000
         GO TO 560
     510 IF m<>0 THEN GO TO 1200
         RETURN
     550 #{unless dtest.class::CHECK0 then 'REM ' end}IF FN c()<>0 THEN GO TO 510
         #{unless dtest.class::CHECK0 then 'REM ' end}IF m=0 THEN GO TO 1200
     560 IF FN r()<>(k-INT(k/m)*m) THEN GO TO 1100
         RETURN
    1000 PRINT "        "'"assertion failed:(Q)";r;"`<>`";INT(k/m): GO TO 10000
    1100 PRINT "        "'"assertion failed:(R)";FN r();"`<>`";(k-INT(k/m)*m): GO TO 10000
    1200 PRINT "        "'"assertion failed:(C)";FN c();"!";m: GO TO 10000
    9998 STOP: RUN
    9999 CLEAR #{dtest.org-1}: LOAD ""CODE: RUN
    END
    program = Basic.parse_source source
    puts "#{dtest.class::NAME} size: #{dtest.code.bytesize}"
    %w[
        divide
        modulo
        divide_sub
        modulo_sub
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{dtest[label].to_s 16} - #{dtest[label]}, size: #{dtest['+'+label]}"
    end

    program.save_tap "#{dtest.class::NAME}.tap", line:9999
    dtest.save_tap "#{dtest.class::NAME}.tap", append:true
end
