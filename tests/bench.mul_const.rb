require 'zxutils/benchmark'
require 'zxlib/basic'

SIGNED_K = false
USE_ACC = false
OPTIMIZE = nil
# OPTIMIZE = :k_rshift_sum
# OPTIMIZE = :neg_k_rshift_sum
# OPTIMIZE = :neg_split_hik_lshift_sum
# OPTIMIZE = :neg_sum_lshift_add_k
# OPTIMIZE = :split_hik_lshift_sum
# OPTIMIZE = :split_lok_rshift_sum
# OPTIMIZE = :sum_lshift_add_k

class BenchMulConst
    include Z80
    include Z80::TAP

    macro_import        MathInt
    macro_import        ZXLib::Math
    label_import        ZXLib::Sys
    import              ZXUtils::Benchmark, :bm, macros: true

    get_bench_result    calculate_benchmark_tstates(bm.counter, bm.tsframe, bm.frames, bm.idle, bm.adjustment)
    estimate_tsframes   estimate_tstates_per_interrupt(vars.udg, bm.interrup_vec, bm.forward, bm.tsframe, bm.idle)

    dc!
    dc!"*********************************************"
    dc!"***                BENCHES                ***"
    dc!"*********************************************"

    ns :set_args   do
                    call bm.fn_argn
        error_q     report_error_unless Z, 'Q Parameter error'
        if SIGNED_K
                    read_integer_value b, c, e # -128..127
        error_a     report_error_unless Z, "A Invalid argument"
                    inc  hl
                    ld   a, b
                    cp   e
        error_b     report_error_unless Z, "B Integer out of range"
                    sign_extend(a, c)
                    cp   e
                    jr   NZ, error_b.err
        else
                    call bm.get_arg_bc # 0..255
                    ld   a, b
                    ora  a
        error_b     report_error_unless Z, "B Integer out of range"
        end
                    ld   a, c
                    ld   [multiplicand], a
                    call bm.fn_argn.seek_next
                    jr   NZ, error_q.err
                    call bm.get_arg_bc
                    ld   a, 256 >> 8
                    cp   b
                    jr   C, error_b.err
                    sla  c  # m * 2
                    rl   b
                    ld   hl, mul_table
                    add  hl, bc # routine address
                    ld   c, [hl]
                    inc  hl
                    ld   b, [hl]
                    inc  hl
                    ld   [routine], bc
                    ld   e, [hl]
                    inc  hl
                    ld   d, [hl]
                    ex   de, hl
                    sbc  hl, bc
                    ld16 bc, hl
                    dec  bc      # routine size - 1 (ret)
                    ret
    end

    ns :get_result do # hl
                    ld   hl, [result]
        if SIGNED_K
                    sign_extend(a, h)
        else
                    xor  a
        end
                    ld   d, a       # sign
                    ld   c, h
                    ld   b, l
                    ld   e, 0
                    anda a
      sknegs        jp   P, skipzch
                    neg_int d, c, b, t:e, t_is_zero:true
                    ora  0x80 # SF = 1
      skipzch       integer32_to_fp e, d, c, b, sgn:M
    end

    return_fp       return_with_fp restore_iy:nil, restore_hl_alt:nil

    mul_table       label
    (0..256).each do |i|
                    dw  define_label :"mul_#{i}"
    end
                    dw  mul_end

                    dc!
                    dc!"**************************************"
                    dc!"***            ROUTINES            ***"
                    dc!"**************************************"
    (0..256).each do |i|
        if OPTIMIZE.nil?
            define_label :"mul_#{i}", mul_const(c, i, tt:de, clrhl:true, signed_k:SIGNED_K)
        else
            define_label :"mul_#{i}", mul_const_ex(c, i, tt:de, use_a:USE_ACC, signed_k:SIGNED_K, optimize:OPTIMIZE)
        end
        ret
    end
    mul_end         label

                    dc!
                    dc!"**************************************"
                    dc!"***              TEST              ***"
                    dc!"**************************************"
    ns :test_mul do # 16+13+4+17+4+10+16=80
                    ld   hl, [routine]      # 16
                    ld   a,  [multiplicand] # 13
                    ld   c, a               # 4
                    call rom.call_jump      # 17 + 4 + 10
                    ld   [result], hl       # 16
                    ret
    end

    result          dw 0
    multiplicand    db 0
    routine         dw 0
end

ZXINTERFACE1 = false

benchmark = BenchMulConst.new 0x8000 # Note: this must be the 0x8000 address at the moment.
tsframe = benchmark['bm.tsframe']
test_mul_signed9 = benchmark[:test_mul_signed9]
test_mul16_signed9 = benchmark[:test_mul16_signed9]
channel = if ZXINTERFACE1 then "T" else "P" end
rtest = benchmark[:test_mul]
rsize = (0..256).inject(0) {|a, n| a+benchmark["+mul_#{n}"]}.to_s
program = ZXLib::Basic.parse_source <<-EOC
   1 DEF FN n(x)=x-(65536 AND x>=32768): DEF FN x()=USR #{benchmark[:get_result]}
     DEF FN b(a,c)=USR #{benchmark['bm.bench']}: REM benchmark
     DEF FN t()=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM get ts/frame
     DEF FN s(t)=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM set ts/frame
     DEF FN i()=USR #{benchmark['bm.get_idle']}: REM idle
     DEF FN r()=USR #{benchmark[:get_bench_result]}: REM result
     DEF FN z(k,m)=USR #{benchmark[:set_args]}
  10 LET counter=50#{if ZXINTERFACE1 then ': FORMAT "T";19200' end}
  20 PRINT "See results on ZX Printer": OPEN #2,"#{channel}": PRINT " k * m : T-States size: #{rsize}"
  30 LET sum=0: LET max=0: LET maxi=-1: LET min=1e+38: LET mini=-1:
  50 RANDOMIZE: FOR m=0 TO 256: LET k=INT(RND*#{if SIGNED_K then "127" else "255" end})+1
     PRINT k;TAB 3;"*";m;":";
  60 LET size=FN z(k,m): LET frames=FN b(#{rtest},counter)
     LET w=FN r()-80: LET sum=sum+w
     IF w>max THEN LET max=w: LET maxi=m
     IF w<min THEN LET min=w: LET mini=m
     IF FN x()<>k*m THEN GO TO 2000
     #{if SIGNED_K then "IF k>=0 THEN LET wp=w: LET k=-k: GO TO 60" else "REM" end}
     PRINT TAB 9;#{if SIGNED_K then 'wp;",";' end}w;TAB 20;size
 100 NEXT m
     PRINT '"Sum:";sum
     PRINT "Avg:";INVERSE 1;sum/#{if SIGNED_K then 257*2 else 257 end}
     PRINT "Max:";INVERSE 1;max;INVERSE 0;" for m=";maxi
     PRINT "Min:";INVERSE 1;min;INVERSE 0;" for m=";mini
     CLOSE #2
 199 STOP
1000 REM Estimate T-States/interrupt
     LET adj=FN n(USR #{benchmark[:estimate_tsframes]}): LET idl=FN i(): 
     PRINT "Est. T-States/int.:";idl*512+adj;" (+-4)"
     INPUT "Is this ok? Y/n", a$: IF a$<>"n" AND a$<>"N" THEN RETURN
     INPUT "Enter value: ",tf: PRINT "T-States/int.:";FN s(tf)
     RETURN
2000 CLOSE #2: PRINT '"assertion failed:";k;"*";m;": ";FN x();"`<>`";k*m: GO TO 10000
9998 STOP
9999 CLEAR #{benchmark.org-1}: LOAD "benchmark"CODE: GO SUB 1000: RUN
EOC
puts benchmark.debug
puts program.to_source escape_keywords: true
puts
puts "routines size: #{rsize} bytes"

tap_name = 'bench.mul_const.tap'
program.save_tap tap_name, line: 9999
benchmark.save_tap tap_name, name:'benchmark', append: true
puts "TAP #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
