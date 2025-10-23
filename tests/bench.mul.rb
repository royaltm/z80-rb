require 'zxutils/benchmark'
require 'zxlib/basic'

OPTIMIZE = :compact # :compact, :size, :time or :unroll

OPTIMIZE_MUL8 = case OPTIMIZE
when :compact, :size then :size
when :time, :unroll then :time
else
    raise ArgumentError, "OPTIMIZE must be :compact, :size, :time or :unroll"
end

class BenchMul
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
                    call bm.get_arg_bc
                    ld   [multiplicand], bc

                    call bm.fn_argn.seek_next
                    jr   NZ, error_q.err
                    call bm.get_arg_bc
                    ld   a, b
                    ora  a
                    report_error_unless Z, "B Integer out of range"
                    ld   a, c
                    ld   [multiplier], a
                    ret
    end

    ns :get_result do # bc
                    ld   bc, [result]
                    ret
    end

    ns :test_mul do # 20+13+16=49
                    ld   de, [multiplicand]    # 20
                    ld   a, [multiplier]       # 13
      dc!
      dc!"**************************************"
      dc!"***            ROUTINE             ***"
      dc!"**************************************"
      routine       mul(e, a, tt:de, clrhl:true, optimize: OPTIMIZE_MUL8)
      dc!"**************************************"
      dc!
                    ld   [result], hl          # 16
                    ret
    end

    ns :test_mul8 do # 20+13+16=49
                    ld   de, [multiplicand]    # 20
                    ld   a, [multiplier]       # 13
      dc!
      dc!"**************************************"
      dc!"***            ROUTINE             ***"
      dc!"**************************************"
      routine       mul8(d, e, a, tt:de, clrhl:true, optimize:OPTIMIZE_MUL8)
      dc!"**************************************"
      dc!
                    ld   [result], hl          # 16
                    ret
    end

    ns :test_mul8c do # 20+13+16=49
                    ld   de, [multiplicand]    # 20
                    ld   a, [multiplier]       # 13
      dc!
      dc!"**************************************"
      dc!"***            ROUTINE             ***"
      dc!"**************************************"
      routine       mul8_c(d, e, a, tt:de, clrhl:true)
      dc!"**************************************"
      dc!
                    ld   [result], hl          # 16
                    ret
    end

    ns :test_mul16 do # 20+13+16=49
                    ld   de, [multiplicand]    # 20
                    ld   a, [multiplier]       # 13
      dc!
      dc!"**************************************"
      dc!"***            ROUTINE             ***"
      dc!"**************************************"
      routine       mul16(d, e, a, tt:de, optimize:OPTIMIZE)
      dc!"**************************************"
      dc!
                    ld   [result], hl          # 16
                    ret
    end

    result          dw 0
    multiplicand    dw 0
    multiplier      db 0
end

ZXINTERFACE1 = false

benchmark = BenchMul.new 0x8000 # Note: this must be the 0x8000 address at the moment.
tsframe = benchmark['bm.tsframe']
test_mul = benchmark[:test_mul]
test_mul8 = benchmark[:test_mul8]
test_mul8c = benchmark[:test_mul8c]
test_mul16 = benchmark[:test_mul16]
routine_size = benchmark["+test_mul.routine"]
routine8_size = benchmark["+test_mul8.routine"]
routine8c_size = benchmark["+test_mul8c.routine"]
routine16_size = benchmark["+test_mul16.routine"]
rtest, rsize = test_mul, routine_size
# rtest, rsize = test_mul8, routine8_size
# rtest, rsize = test_mul8c, routine8c_size
# rtest, rsize = test_mul16, routine16_size
multiplicands = if [test_mul, test_mul8c].include?(rtest)
    [0,255,128,127,64,63,32,31,0x55,0xaa,1]
else
    [0,65535,32768,32767,16384,16383,256,255,0xaa55,0x55aa,1]
end
max_m = 255
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
  20 PRINT "See results on ZX Printer": OPEN #2,"#{channel}": PRINT "k * m   T-States size: #{rsize}"
  30 LET sum=0: LET max=0: LET maxi=-1: LET min=1e+38: LET mini=-1: LET v=0
  50 FOR m=0 TO #{max_m}: RESTORE 5000: FOR i=1 TO #{multiplicands.length}: READ k
 100 RANDOMIZE FN z(k,m): LET frames=FN b(#{rtest},counter)
     LET w=FN r()-49: LET sum=sum+w
     IF w>max THEN LET max=w: LET maxi=m
     IF w<min THEN LET min=w: LET mini=m
     IF v<>w THEN PRINT k;"*";m;":";TAB 16;w: LET v=w
 110 LET mres=k*m: IF mres>65535 THEN LET mres=mres-65536*INT (mres/65536)
     IF FN x()<>mres THEN GO TO 2000
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
puts "routine mul    size: #{routine_size} bytes"
puts "routine mul8   size: #{routine8_size} bytes"
puts "routine mul8_c size: #{routine8c_size} bytes"
puts "routine mul16  size: #{routine16_size} bytes"

tap_name = 'bench.mul.tap'
program.save_tap tap_name, line: 9999
benchmark.save_tap tap_name, name:'benchmark', append: true
puts "TAP #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
