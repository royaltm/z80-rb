require 'zxutils/benchmark'
require 'zxlib/basic'

class BenchRnd
    include Z80
    include Z80::TAP

    macro_import        MathInt
    label_import        ZXLib::Sys
    import              ZXUtils::Benchmark, :bm, macros: true

    get_bench_result    calculate_benchmark_tstates(bm.counter, bm.tsframe, bm.frames, bm.idle, bm.adjustment)
    estimate_tsframes   estimate_tstates_per_interrupt(vars.udg, bm.interrup_vec, bm.forward, bm.tsframe, bm.idle)

    ns :set_seed   do
                    find_def_fn_args(1, subroutine:false, cf_on_direct:true)
                    jr   C,  from_vars
                    report_error_unless Z, 'Q Parameter error'
                    read_positive_int_value b, c
                    report_error_unless Z, 'A Invalid argument'
                    jr   store_seed
      from_vars     ld   bc, [vars.seed]
      store_seed    ld   [seed], bc
                    ret
    end

    ns :test_rnd do
      seed_a        ld   hl, 0 # 10
      seed_p        seed_a + 1
                    rnd
                    ret
    end
    seed            union test_rnd.seed_p, 2

end

benchmark = BenchRnd.new 0x8000 # Note: this must be the 0x8000 address at the moment.
tsframe = benchmark['bm.tsframe']
test_rnd = benchmark['test_rnd']
seed = benchmark['test_rnd.seed']
program = ZXLib::Basic.parse_source <<-EOC
   1 DEF FN n(x)=x-(65536 AND x>=32768)
     DEF FN b(a,c)=USR #{benchmark['bm.bench']}: REM benchmark
     DEF FN t()=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM get ts/frame
     DEF FN s(t)=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM set ts/frame
     DEF FN i()=USR #{benchmark['bm.get_idle']}: REM idle
     DEF FN r()=USR #{benchmark[:get_bench_result]}: REM result
     DEF FN z(s)=USR #{benchmark[:set_seed]}
  10 LET counter=50
  20 PRINT "See results on ZX Printer": OPEN #2,"P": PRINT "Seed:   T-States"
  30 LET sum=0: LET max=0: LET maxi=-1: LET min=1e+38: LET mini=-1
  40 INPUT "step 1-128: ";step: IF step<1 OR step>128 OR step<>INT step THEN GO TO 40
  50 FOR i=0 TO 65535 STEP step
 100 RANDOMIZE FN z(i): LET frames=FN b(#{benchmark[:test_rnd]},counter)
     LET w=FN r()-10: LET sum=sum+w
     IF w>max THEN LET max=w: LET maxi=i
     IF w<min THEN LET min=w: LET mini=i
     PRINT i;":";TAB 8;FN r()-10
     NEXT i
     PRINT "Sum:";sum
     PRINT "Avg:";sum/INT (65536/step)
     PRINT "Max:";max;" for seed:";maxi
     PRINT "Min:";min;" for seed:";mini
     CLOSE #2
 999 STOP
1000 REM Estimate T-States/interrupt
     LET adj=FN n(USR #{benchmark[:estimate_tsframes]}): LET idl=FN i(): 
     PRINT "Est. T-States/int.:";idl*512+adj;" (+-4)"
     INPUT "Is this ok? Y/n", a$: IF a$<>"n" AND a$<>"N" THEN RETURN
     INPUT "Enter value: ",tf: PRINT "T-States/int.:";FN s(tf)
     RETURN
9998 STOP
9999 CLEAR #{benchmark.org-1}: LOAD "benchmark"CODE: GO SUB 1000: RUN
EOC
puts benchmark.debug
puts "\nseed: " + (benchmark[:seed]).to_s
puts program.to_source escape_keywords: true

tap_name = 'bench.rnd.tap'
program.save_tap tap_name, line: 9999
benchmark.save_tap tap_name, name:'benchmark', append: true
puts "TAP #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
