require 'zxutils/benchmark'
require 'zxlib/basic'

OPTIMIZE = :unroll # :size :time :unroll

class BenchMulSigned9
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
                    read_integer_value b, c, e
        error_a     report_error_unless Z, "A Invalid argument"
                    inc  hl
                    ld   a, e
                    xor  b
        error_b     report_error_unless Z, "B Integer out of range"
                    ld   [multiplicand], bc
                    call bm.fn_argn.seek_next
                    jr   NZ, error_q.err
                    read_integer_value b, c, e
                    jr   NZ, error_a.err
                    ld   a, e
                    xor  b
                    jr   NZ, error_b.err
                    ld   [multiplier], bc
                    ret
    end

    ns :get_result do # a|hl
                    ld   a, [resign]
                    ld   hl, [result]
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

    ns :test_mul_signed9 do # 20+20+8+16+4+13=81
                    ld   de, [multiplicand] # 20
                    ld   bc, [multiplier]   # 20
                    sra  b                  # 8
      dc!
      dc!"**************************************"
      dc!"***            ROUTINE             ***"
      dc!"**************************************"
      routine       mul_signed9(d, e, c, s:b, tt:de, m_neg_cond:C, k_full_range:false, m_full_range:false, m_is_zero_zf:false, optimize:OPTIMIZE)
      dc!"**************************************"
      dc!
                    ld   [result], hl       # 16
                    ld   a, b               # 4
                    ld   [resign], a        # 13
                    ret
    end

    ns :test_mul16_signed9 do # 20+20+8+16+4+13=81
                    ld   de, [multiplicand] # 20
                    ld   bc, [multiplier]   # 20
                    sra  b                  # 8
      dc!
      dc!"**************************************"
      dc!"***            ROUTINE             ***"
      dc!"**************************************"
      routine       mul16_signed9(d, e, c, s:b, tt:de, m_neg_cond:C, m_overflow:false, optimize:OPTIMIZE)
      dc!"**************************************"
      dc!
                    ld   [result], hl       # 16
                    ld   a, b               # 4
                    ld   [resign], a        # 13
                    ret
    end

    resign          db 0
    result          dw 0
    multiplicand    dw 0
    multiplier      dw 0
end

ZXINTERFACE1 = false

benchmark = BenchMulSigned9.new 0x8000 # Note: this must be the 0x8000 address at the moment.
tsframe = benchmark['bm.tsframe']
test_mul_signed9 = benchmark[:test_mul_signed9]
test_mul16_signed9 = benchmark[:test_mul16_signed9]
routine_size = benchmark["+test_mul_signed9.routine"]
routine16_size = benchmark["+test_mul16_signed9.routine"]
rtest, rsize = test_mul_signed9, routine_size
# rtest, rsize = test_mul16_signed9, routine16_size
channel = if ZXINTERFACE1 then "T" else "P" end
program = ZXLib::Basic.parse_source <<-EOC
   1 DEF FN n(x)=x-(65536 AND x>=32768): DEF FN x()=USR #{benchmark[:get_result]}
     DEF FN b(a,c)=USR #{benchmark['bm.bench']}: REM benchmark
     DEF FN t()=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM get ts/frame
     DEF FN s(t)=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM set ts/frame
     DEF FN i()=USR #{benchmark['bm.get_idle']}: REM idle
     DEF FN r()=USR #{benchmark[:get_bench_result]}: REM result
     DEF FN z(k,m)=USR #{benchmark[:set_args]}
  10 LET counter=50#{if ZXINTERFACE1 then ': FORMAT "T";19200' end}
  20 PRINT "See results on ZX Printer": OPEN #2,"#{channel}": PRINT "SinCos: T-States size: #{rsize}"
  30 LET sum=0: LET max=0: LET maxi=-1: LET min=1e+38: LET mini=-1: LET v=0
  50 FOR m=-255 TO 255: FOR k=-255 TO 255
 100 RANDOMIZE FN z(k,m): LET frames=FN b(#{rtest},counter)
     LET w=FN r()-81: LET sum=sum+w
     IF w>max THEN LET max=w: LET maxi=(k+256)*512+(m+256)
     IF w<min THEN LET min=w: LET mini=(k+256)*512+(m+256)
     IF v<>w THEN PRINT k;"*";m;":";TAB 16;w: LET v=w
 110 IF FN x()<>k*m THEN GO TO 2000
 120 NEXT k: NEXT m
     PRINT '"Sum:";sum
     PRINT "Avg:";INVERSE 1;sum/#{511*511}
     PRINT "Max:";INVERSE 1;max;INVERSE 0;" for ";INT(maxi/512)-256;"*";maxi-INT(maxi/512)*512-256
     PRINT "Min:";INVERSE 1;min;INVERSE 0;" for ";INT(mini/512)-256;"*";mini-INT(mini/512)*512-256
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
puts "routine mul_signed9   size: #{routine_size} bytes"
puts "routine mul16_signed9 size: #{routine16_size} bytes"

tap_name = 'bench.mul_signed9_lim.tap'
program.save_tap tap_name, line: 9999
benchmark.save_tap tap_name, name:'benchmark', append: true
puts "TAP #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
