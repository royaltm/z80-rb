require 'zxutils/benchmark'
require 'zxlib/basic'
require 'z80/utils/sort'
require 'z80/utils/shuffle'

include ZXLib

class BenchSort
    include Z80
    include Z80::TAP

    export test_qsort
    export test_ssort

    ARRAY_SIZE = 50
    SORT_REVERSE = false

    macro_import        Stdlib
    label_import        ZXLib::Sys
    macro_import        Utils::Sort
    macro_import        Utils::Shuffle
    import              ZXUtils::Benchmark, :bm, macros: true

    get_bench_result    calculate_benchmark_tstates(bm.counter, bm.tsframe, bm.frames, bm.idle, bm.adjustment)
    estimate_tsframes   estimate_tstates_per_interrupt(vars.udg, bm.interrup_vec, bm.forward, bm.tsframe, bm.idle)

    shuffle     shuffle_bytes_source_max256(next_rng, target:sortable0, length:sortable0_end - sortable0)
    shuffle_end ret

    with_saved :randomize, :exx, hl, ret: true do
                ld   hl, sortable0
                ld   b, sortable0_end - sortable0
        loop0   exx
                call next_rng
                exx
                ld   [hl], a
                inc  hl
                djnz loop0
    end

    ns :ascending do
                ld   hl, sortable0_end - 1
                ld   b, sortable0_end - sortable0 - 1
        loop0   ld   [hl], b
                dec  hl
                djnz loop0
                ld   [hl], b
                ret
    end

    ns :descending do
                ld   hl, sortable0
                ld   b, sortable0_end - sortable0 - 1
        loop0   ld   [hl], b
                inc  hl
                djnz loop0
                ld   [hl], b
                ret
    end

    ns :clear do
                clrmem8 sortable0, sortable0_end - sortable0, 99
                ret
    end

    next_rng    ld   hl, [vars.seed]
                rnd
                ld   [vars.seed], hl
                ld   a, l
                ret

    macro :copy_sortable do
                memcpy sortable1, sortable0, ARRAY_SIZE
    end

    dc!
    dc!"*********************************************"
    dc!"***                BENCHES                ***"
    dc!"*********************************************"

    ns :test_copy do
                copy_sortable
                ret
    end

    ns :test_ssort do
            copy_sortable
        if SORT_REVERSE
            selection_sort_bytes_max256(reverse:true, target:sortable1, length:ARRAY_SIZE, subroutine:true)
        else
            selection_sort_bytes_max256(reverse:false, target:sortable1_end-1, length:ARRAY_SIZE, subroutine:true)
        end
    end

    ns :test_isort do
            copy_sortable
        if SORT_REVERSE
            insertion_sort_bytes_max256(reverse:true, target:sortable1_end-1, length:ARRAY_SIZE, subroutine:true)
        else
            insertion_sort_bytes_max256(reverse:false, target:sortable1, length:ARRAY_SIZE, subroutine:true)
        end
    end

    ns :test_qsort do
                copy_sortable
        if SORT_REVERSE
                ld   hl, sortable1
                ld   de, sortable1_end[-1]
                quicksort_bytes(:half, reverse: true, pivot_reg: c, swap_same: true, safe_args: false)
        else
                ld   de, sortable1
                ld   hl, sortable1_end[-1]
                quicksort_bytes(:half, reverse: false, pivot_reg: c, swap_same: true, safe_args: false)
        end
    end

    sortable0        bytes ARRAY_SIZE
    sortable0_end    label
    sortable1        bytes ARRAY_SIZE
    sortable1_end    label
end

benchmark = BenchSort.new 0x8000 # Note: this must be the 0x8000 address at the moment.
tsframe = benchmark['bm.tsframe']
sortable0 = benchmark['sortable0']
sortable0_end = benchmark['sortable0_end']
sortable1 = benchmark['sortable1']
sortable1_end = benchmark['sortable1_end']
program = Basic.parse_source <<-EOC
   1 DEF FN n(x)=x-(65536 AND x>=32768)
     DEF FN b(a,c)=USR #{benchmark['bm.bench']}: REM benchmark
     DEF FN t()=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM get ts/frame
     DEF FN s(t)=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM set ts/frame
     DEF FN i()=USR #{benchmark['bm.get_idle']}: REM idle
     DEF FN r()=USR #{benchmark[:get_bench_result]}: REM result
     RESTORE: DIM d$(5): LPRINT "Number of elements: #{sortable1_end-sortable1}"
     PRINT "Number of elements: #{sortable1_end-sortable1}"'"See results on ZX Printer"'"Measuring..."
     LET frames=FN b(#{benchmark[:test_copy]},30000): LET adjust=FN r()
     PRINT "Data copy adjustment: ";adjust;" ts": PRINT #1;"Press a key...": PAUSE 0
  20 READ prepdata,d$,retries: DIM q(3,3): REM [S,I,Q][min,sum,max]
     FOR i=1 TO 3: LET q(i,1)=100000: LET q(i,2)=0: LET q(i,3)=-1: NEXT i
     FOR t=1 TO retries
     RANDOMIZE: RANDOMIZE USR prepdata
     CLS: FOR i=#{sortable0} TO #{sortable0_end - 1}: PRINT PEEK i;",";: NEXT i: PRINT '"Measuring..."
     LET routine=#{benchmark[:test_ssort]}: LET n$="SSort": LET index=1: GO SUB 100
     LET routine=#{benchmark[:test_isort]}: LET n$="ISort": LET index=2: GO SUB 100
     LET routine=#{benchmark[:test_qsort]}: LET n$="QSort": LET index=3: GO SUB 100
     NEXT t
     CLS
     FOR i=#{sortable1} TO #{sortable1_end - 1}: PRINT PEEK i;",";: NEXT i:
     PRINT '"Data: ";INK 4;d$
     LET n$="SSort": LET index=1: GO SUB 200
     LET n$="ISort": LET index=2: GO SUB 200
     LET n$="QSort": LET index=3: GO SUB 200
  90 PAUSE 0: GO TO 20
  99 STOP: RUN
 100 LET counter=1000: LET frames=FN b(routine,counter): LET ts=FN r()-adjust
     IF ts < q(index,1) THEN LET q(index,1)=ts
     IF ts > q(index,3) THEN LET q(index,3)=ts
     LET q(index,2)=q(index,2)+ts
     PRINT n$;" T-States: ";ts
     RETURN
 200 LET avg=q(index,2)/retries
     PRINT "Routine: ";INK 1;n$
     REM PRINT "T-States/frame: ";FN t()
     PRINT "T-States min: ";q(index,1)
     PRINT "T-States avg: ";avg
     PRINT "T-States max: ";q(index,3)
     PRINT
     DIM f$(6)
     LET f$=STR$ q(index,1)
     LPRINT d$;" ";n$;" ";f$;
     LET f$=STR$ INT (avg+.5)
     LPRINT INVERSE 1;f$;
     LET f$=STR$ q(index,3)
     LPRINT f$
     RETURN
1000 REM Estimate T-States/interrupt
     LET adj=FN n(USR #{benchmark[:estimate_tsframes]}): LET idl=FN i()
     PRINT "Est. T-States/int.:";idl*512+adj;" (+-4)"
     INPUT "Is this ok? Y/n", a$: IF a$<>"n" AND a$<>"N" THEN RETURN
     INPUT "Enter value: ",tf: PRINT "T-States/int.:";FN s(tf)
     RETURN
9000 DATA #{benchmark[:clear]},"1MONO",10
     DATA #{benchmark[:shuffle]},"SHUFF",1000
     DATA #{benchmark[:randomize]},"RANDO",1000
     DATA #{benchmark[:ascending]},"ASCEN",10
     DATA #{benchmark[:descending]},"DESCE",10
9998 STOP
9999 CLEAR #{benchmark.org-1}: LOAD "benchmark"CODE: RUN
EOC
puts benchmark.debug
# puts program.to_source escape_keywords: true
puts "test_ssort size: #{benchmark['+test_ssort']}"
puts "test_isort size: #{benchmark['+test_isort']}"
puts "test_qsort size: #{benchmark['+test_qsort']}"

tap_name = 'bench.sort.tap'
program.save_tap tap_name, line: 9999
benchmark.save_tap tap_name, name:'benchmark', append: true
puts "TAP #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
