require 'zxutils/benchmark'
require 'zxlib/basic'

OPTIMIZE = :time # :size :time :unroll
K17_SIGNED = true
M_BIT_9 = true

class BenchMul24
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
        if K17_SIGNED
                    read_integer_value b, c, e # -32768..32767
        error_a     report_error_unless Z, "A Invalid argument"
                    inc  hl
                    sign_extend(a, b)
                    cp   e
                    report_error_unless Z, "B Integer out of range"
        else
                    ld   a, 0x5A # garbage
                    call bm.get_arg_bc
        end
                    ld   [multiplicand], bc
                    ld   [multiplicandsgn], a

                    call bm.fn_argn.seek_next
                    jr   NZ, error_q.err
                    call bm.get_arg_bc
                    ld   a, b
        if M_BIT_9
                    cp   512 >> 8
        else
                    cp   256 >> 8
        end
                    report_error_unless C, "B Integer out of range"
                    ld   [multiplier], bc
                    ret
    end

    ns :get_result do # a|hl
                    ld   hl, [result]
                    ld   a,  [rehigh]
                    ld   d, a
                    ld   c, h
                    ld   b, l
                    ld   e, 0
        if K17_SIGNED
                    anda a
                    jp   P, skipzch
                    neg_int d, c, b, t:e, t_is_zero:true
                    ora  0x80 # SF = 1
            skipzch integer32_to_fp e, d, c, b, sgn:M
        else
                    integer32_to_fp e, d, c, b, sgn:nil
        end
    end

    return_fp       return_with_fp restore_iy:nil, restore_hl_alt:nil

    ns :test_mul8_24 do # 20+20+13+8+16+13=90
                    ld   de, [multiplicand]    # 20
                    ld   bc, [multiplicandsgn] # 20
                    ld   a, [multiplierhi]     # 13
                    sra  a                     # 8
unless OPTIMIZE == :unroll
      dc!
      dc!"**************************************"
      dc!"***            ROUTINE             ***"
      dc!"**************************************"
      routine       mul8_24(d, e, b, t:c, tt:de, clrahl:true, k_int24:K17_SIGNED, mbit9_carry:M_BIT_9, optimize:OPTIMIZE)
      dc!"**************************************"
      dc!
end
                    ld   [result], hl          # 16
                    ld   [rehigh], a           # 13
                    ret
end

    ns :test_mul8_24a do # 20+20+13+8+16+13=90
                    ld   de, [multiplicand]    # 20
                    ld   bc, [multiplicandsgn] # 20
                    ld   a, [multiplierhi]     # 13
                    sra  a                     # 8
      dc!
      dc!"**************************************"
      dc!"***            ROUTINE             ***"
      dc!"**************************************"
      routine       mul8_24a(d, e, b, t:c, tt:de, k_int24:K17_SIGNED, mbit9_carry:M_BIT_9, optimize:OPTIMIZE)
      dc!"**************************************"
      dc!
                    ld   [result], hl          # 16
                    ld   [rehigh], a           # 13
                    ret
    end

    result          dw 0
    rehigh          db 0
    multiplicand    dw 0
    multiplicandsgn db 0
    multiplier      db 0
    multiplierhi    db 0
end

ZXINTERFACE1 = true

benchmark = BenchMul24.new 0x8000 # Note: this must be the 0x8000 address at the moment.
tsframe = benchmark['bm.tsframe']
test_mul8_24 = benchmark[:test_mul8_24]
test_mul8_24a = benchmark[:test_mul8_24a]
routine_size = benchmark["+test_mul8_24.routine"]
routinea_size = benchmark["+test_mul8_24a.routine"]
# rtest, rsize = test_mul8_24, routine_size
rtest, rsize = test_mul8_24a, routinea_size
multiplicands = if K17_SIGNED
    if M_BIT_9
        [0,16416,-16416,16384,-16384,16383,-16383,256,-256,255,-255,-0x35aa,0x35aa,-1,1]
    else
        [0,32767,-32767,-32768,16384,-16384,16383,-16383,256,-256,255,-255,-0x55aa,0x55aa,-1,1]
    end
else
    if M_BIT_9
        [0,32832,32768,32767,16384,16383,256,255,0x5aa5,0x55aa,1]
    else
        [0,65535,32768,32767,16384,16383,256,255,0xaa55,0x55aa,1]
    end
end
max_m = if M_BIT_9 then 511 else 255 end
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
  50 FOR m=0 TO #{max_m}: RESTORE 5000: FOR i=1 TO #{multiplicands.length}: READ k
 100 RANDOMIZE FN z(k,m): LET frames=FN b(#{rtest},counter)
     LET w=FN r()-90: LET sum=sum+w
     IF w>max THEN LET max=w: LET maxi=m
     IF w<min THEN LET min=w: LET mini=m
     IF v<>w THEN PRINT k;"*";m;":";TAB 16;w: LET v=w
 110 IF FN x()<>k*m THEN GO TO 2000
 120 NEXT i: NEXT m
     PRINT '"Sum:";sum
     PRINT "Avg:";INVERSE 1;sum/#{(max_m+1)*multiplicands.length}
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
5000 DATA #{multiplicands.join(',')}
9998 STOP
9999 CLEAR #{benchmark.org-1}: LOAD "benchmark"CODE: GO SUB 1000: RUN
EOC
puts benchmark.debug
puts program.to_source escape_keywords: true
puts
puts "routine mul24_8  size: #{routine_size} bytes"
puts "routine mul24_8a size: #{routinea_size} bytes"

tap_name = 'bench.mul24_8.tap'
program.save_tap tap_name, line: 9999
benchmark.save_tap tap_name, name:'benchmark', append: true
puts "TAP #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
