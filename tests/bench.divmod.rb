require 'zxutils/benchmark'
require 'zxlib/basic'

OPTIONS = {
  clrrem:true,
  check0:true,
  check0_far:false,
  check1:true,
  k_leq_m:false,
  modulo:true,
  ignore_cf:false,
  optimize: :unroll_alt
}.freeze

class BenchDivMod
    include Z80
    include Z80::TAP

    macro_import        MathInt
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
                    xor  a
                    ora  b
        error_6     report_error_unless Z, '6 Number too big'
                    ld   a, c
                    ld   [arg_k], a
                    call bm.fn_argn.seek_next
                    jr   NZ, error_q.err
                    call bm.get_arg_bc
                    xor  a
                    ora  b
                    jr   NZ, error_6.err
                    ld   a, c
                    ld   [arg_m], a
                    ret
    end

    ns :test_divmod do # 7+10+13+4+13+4+13=64
                    ld   a,  0 # 7
      divargs_a     ld   de, 0 # 10
      divargs_m     as   divargs_a + 1
      divargs_k     as   divargs_a + 2
      dc!
      dc!"*********************************************"
      dc!"***                ROUTINE                ***"
      dc!"*********************************************"
      routine       divmod(d, e, **OPTIONS)
      dc!"*********************************************"
      dc!
      routine_over  ld   [remainder], a # 13
                    ld   a, d           # 4
                    ld   [quotient], a  # 13
                    sbc  a, a           # 4
                    ld   [carryover], a # 13
                    ret
    end
    quotient        db 0
    remainder       db 0
    carryover       db 0
    arg_m           as test_divmod.divargs_m
    arg_k           as test_divmod.divargs_k
end

ZXINTERFACE1 = false

benchmark = BenchDivMod.new 0x8000 # Note: this must be the 0x8000 address at the moment.
tsframe = benchmark['bm.tsframe']
test_divmod = benchmark[:test_divmod]
arg_k = benchmark[:arg_k]
arg_m = benchmark[:arg_m]
quotient = benchmark[:quotient]
remainder = benchmark[:remainder]
carryover = benchmark[:carryover]
routine_size = benchmark["+test_divmod.routine"]
check0 = OPTIONS[:clrrem] && OPTIONS[:check0]
mstart = if check0 then 0 else 1 end
modulo = OPTIONS[:modulo]
channel = if ZXINTERFACE1 then "T" else "P" end
program = ZXLib::Basic.parse_source <<-EOC
   1 DEF FN n(x)=x-(65536 AND x>=32768): DEF FN q()=PEEK #{quotient}: DEF FN m()=PEEK #{remainder}: DEF FN c()=PEEK #{carryover}
     DEF FN b(a,c)=USR #{benchmark['bm.bench']}: REM benchmark
     DEF FN t()=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM get ts/frame
     DEF FN s(t)=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM set ts/frame
     DEF FN i()=USR #{benchmark['bm.get_idle']}: REM idle
     DEF FN r()=USR #{benchmark[:get_bench_result]}: REM result
     DEF FN z(k,m)=USR #{benchmark[:set_args]}
  10 LET counter=50#{if ZXINTERFACE1 then ': FORMAT "T";19200' end}
  20 PRINT "See results on ZX Printer": OPEN #2,"#{channel}": PRINT "Divmod: T-States size: #{routine_size}"
  30 LET sum=0: LET max=0: LET maxi=-1: LET min=1e+38: LET mini=-1: LET v=0
  50 FOR k=0 TO 255: FOR m=#{mstart} TO 255
 100 RANDOMIZE FN z(k,m): LET frames=FN b(#{test_divmod},counter)
     LET w=FN r()-64: LET sum=sum+w
     IF w>max THEN LET max=w: LET maxi=k*256+m
     IF w<min THEN LET min=w: LET mini=k*256+m
     IF v<>w THEN PRINT k;"/";m;":";TAB 8;w: LET v=w
 110 #{unless check0 then 'REM ' end}IF FN c()<>0 THEN GO TO 200
     #{unless check0 then 'REM ' end}IF m=0 THEN GO TO 2200
     #{if modulo then 'REM ' end}IF FN q()<>INT(k/m) THEN GO TO 2000
     IF FN m()<>(k-INT(k/m)*m) THEN GO TO 2100
 120 NEXT m: NEXT k
     PRINT '"Sum:";sum
     PRINT "Avg:";INVERSE 1;sum/#{256*(256-mstart)}
     PRINT "Max:";INVERSE 1;max;INVERSE 0;" for ";INT(maxi/256);"/";maxi-INT(maxi/256)*256
     PRINT "Min:";INVERSE 1;min;INVERSE 0;" for ";INT(mini/256);"/";mini-INT(mini/256)*256
     CLOSE #2
 199 STOP
 200 IF m<>0 THEN GO TO 2200
     GO TO 120
1000 REM Estimate T-States/interrupt
     LET adj=FN n(USR #{benchmark[:estimate_tsframes]}): LET idl=FN i(): 
     PRINT "Est. T-States/int.:";idl*512+adj;" (+-4)"
     INPUT "Is this ok? Y/n", a$: IF a$<>"n" AND a$<>"N" THEN RETURN
     INPUT "Enter value: ",tf: PRINT "T-States/int.:";FN s(tf)
     RETURN
2000 CLOSE #2: PRINT '"assertion failed:(q)";FN q();"`<>`";INT(k/m): GO TO 10000
2100 CLOSE #2: PRINT '"assertion failed:(r)";FN m();"`<>`";(k-INT(k/m)*m): GO TO 10000
2200 CLOSE #2: PRINT '"assertion failed:(C)";FN c();"!";m: GO TO 10000
9998 STOP
9999 CLEAR #{benchmark.org-1}: LOAD "benchmark"CODE: GO SUB 1000: RUN
EOC
puts benchmark.debug
puts program.to_source escape_keywords: true
puts "\nroutine size: #{routine_size} bytes"

tap_name = 'bench.divmod.tap'
program.save_tap tap_name, line: 9999
benchmark.save_tap tap_name, name:'benchmark', append: true
puts "TAP #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
