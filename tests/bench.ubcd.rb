require 'z80'
require 'z80/math_i'
require 'zxutils/benchmark'
require 'zxlib/basic'

class BenchPrintInt
    include Z80
    include Z80::TAP

    export test_print_int
    export test_trivial_int16
    export test_div_int16

    macro_import        Stdlib
    macro_import        MathInt
    label_import        ZXLib::Sys
    import              ZXUtils::Benchmark, :bm, macros: true

    get_bench_result    calculate_benchmark_tstates(bm.counter, bm.tsframe, bm.frames, bm.idle, bm.adjustment)
    estimate_tsframes   estimate_tstates_per_interrupt(vars.udg, bm.interrup_vec, bm.forward, bm.tsframe, bm.idle)

    dc!
    dc!"*********************************************"
    dc!"***                BENCHES                ***"
    dc!"*********************************************"

    ns :test_print_int do
        size_a      ld    b, +test_integer
        size_p      size_a + 1
        input_a     ld    de, test_integer + +test_integer
        input_p     input_a + 1
                    utobcd bcd_buffer_end, de, size: b, r: d, rr: de, byteorder: :lsb, input_end:true
                    exx
                        # push bc
                        # ld  a, 2
                        # call rom.chan_open
                        # pop  bc
                    ld  hl, bcd_buffer_end
                    sub_from c, h, l
                    bcdtoa hl, c, skip_leading0:true, preserve_in:nil do |_|
                      add  '0'.ord
                      # rst  0x10
                    end

                    ret
    end

    set_test_int128 ld    a, +test_integer128
                    ld    hl, test_integer128
                    jr    set_test_a_hl
    set_test_int64  ld    a, +test_integer64
                    ld    hl, test_integer64
                    jr    set_test_a_hl
    set_test_int48  ld    a, +test_integer48
                    ld    hl, test_integer48
                    jr    set_test_a_hl
    set_test_int32  ld    a, +test_integer32
                    ld    hl, test_integer32
                    jr    set_test_a_hl
    set_test_int24  ld    a, +test_integer24
                    ld    hl, test_integer24
                    jr    set_test_a_hl
    set_test_int16  ld    a, +test_integer16
                    ld    hl, test_integer16
    set_test_a_hl   ld    [test_print_int.size_p], a
                    adda_to h, l
                    ld    [test_print_int.input_p], hl
                    ret

    macro :calc_digit do |eoc|
                xor  a
        loopadd add  hl, bc
                inc  a
                jr   C, loopadd
                sbc  hl, bc
                dec  a
                jr   NZ, printn
                ora  e
                jr   Z, eoc
                xor  a
        printn  add  '0'.ord
                # rst  0x10
                inc  e
    end
    #     0:  365
    #     9:  365
    #    99:  608
    #   999:  851
    #  9999: 1094
    # 10000:  422
    # 19999: 1121
    # 59999: 1229
    # 65536:  878
    ns :test_trivial_int16 do
                # ld  a, 2
                # call rom.chan_open
                ld   hl, test_integer16
                ld   e, [hl]
                inc  hl
                ld   d, [hl]
                ex   de, hl
                xor  a
                ld   e, a
                ld   bc, -10000
                calc_digit
                ld   bc, -1000
                calc_digit
                ld   bc, -100
                calc_digit
                ld   bc, -10
                calc_digit
                ld   a, l
                add  '0'.ord
                # rst  0x10
                ret
    end
    #     0:  732
    #     9:  732
    #    99: 1410
    #   999: 2142
    #  9999: 2948
    # 10000: 3604
    # 19999: 3668
    # 59999: 3785
    # 65536: 3717
    ns :test_div_int16 do
                # ld  a, 2
                # call rom.chan_open
                ld   hl, test_integer16
                ld   e, [hl]
                inc  hl
                ld   d, [hl]
                ld   hl, bcd_buffer_end
                ld   c, 10
        loopdiv divmod d, c, check0:false, check1:false
                divmod e, c, clrrem:false
                dec  hl
                add  '0'.ord
                ld   [hl], a
                ld   a, e
                ora  d
                jr   NZ, loopdiv
                ld   de, bcd_buffer_end
                ex   de, hl
                sbc  hl, de
                ld   b, l
        loop_pr ld   a, [de]
                # rst  0x10
                inc  de
                djnz loop_pr
                ret
    end

    set_int16       call bm.fn_argn
                    jr   C,  set_int16_max
                    report_error_unless Z, 'Q Parameter error'
                    call bm.get_arg_bc
                    jr   set_int16_bc
    set_int16_max   ld   bc, -1
    set_int16_bc    ld   [test_integer16], bc
                    ret

    set_int_min     clrmem test_integer16, test_int_end - test_integer16, 0
                    ret

    set_int_max     clrmem test_integer16, test_int_end - test_integer16, -1
                    ret

    test_integer    as  test_integer16
                                                                # f(0)-f(max) (without bcdtoa)
    test_integer16  int 16,  0xFFFF                             #  1542- 2177 ( 1416- 1897)
    test_integer24  int 24,  0xFFFFFF                           #  2228- 3779 ( 2102- 3418)
    test_integer32  int 32,  0xFFFFFFFF                         #  2914- 5746 ( 2788- 5308)
    test_integer48  int 48,  0xFFFFFFFFFFFF                     #  4286-10957 ( 4160-10292)
    test_integer64  int 64,  0xFFFFFFFFFFFFFFFF                 #  5658-17683 ( 5532-16860)
    test_integer128 int 128, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF # 11146-60454 (11020-58865)
    test_int_end    label
    bcd_buffer      bytes 20
    bcd_buffer_end  label
end

include ZXLib

benchmark = BenchPrintInt.new 0x8000 # Note: this must be the 0x8000 address at the moment.
tsframe = benchmark['bm.tsframe']
program = Basic.parse_source <<-EOC
   1 DEF FN n(x)=x-(65536 AND x>=32768)
     DEF FN b(a,c)=USR #{benchmark['bm.bench']}: REM benchmark
     DEF FN a()=FN n(USR #{benchmark['bm.get_adjustment']}): REM adjustment
     DEF FN i()=USR #{benchmark['bm.get_idle']}: REM idle
     DEF FN r()=USR #{benchmark[:get_bench_result]}: REM result
     DEF FN f()=USR #{benchmark['bm.get_frames']}: REM frames
     DEF FN t()=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM get ts/frame
     DEF FN s(t)=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM set ts/frame
     DEF FN m(i)=USR #{benchmark[:set_int16]}: REM set int 16
  10 RESTORE
  11 CLS: LET routine=#{benchmark[:test_print_int]}: READ setr,n$: IF NOT setr THEN GO TO 20
     PRINT INK 3;n$: LPRINT "________________";OVER 1;AT 0,0;n$: RANDOMIZE USR setr
     RANDOMIZE USR #{benchmark[:set_int_min]}: PRINT INK 2;'"fn(MIN)": LPRINT " (MIN)   ";: GO SUB 100
     RANDOMIZE USR #{benchmark[:set_int_max]}: PRINT INK 2;'"fn(MAX)": LPRINT " (MAX)   ";: GO SUB 100
     LPRINT
     PAUSE 0
     GO TO 11
  20 CLS: RESTORE 9100: LET routine=#{benchmark[:test_trivial_int16]}: LET n$="simple 16"
     GO SUB 90
     LPRINT
     PAUSE 0
  30 CLS: RESTORE 9100: LET routine=#{benchmark[:test_div_int16]}: LET n$="div 10"
     GO SUB 90
     STOP: RUN
  90 PRINT INK 3;n$: LPRINT "________________";OVER 1;AT 0,0;n$
  91 READ n: RANDOMIZE FN m(n)
     PRINT INK 2;'"fn(";n;")"
     DIM s$(10): LET s$=" ("+STR$ n+")": LPRINT s$;
     GO SUB 100
     IF n=65535 THEN RETURN
     GO TO 91
 100 LET counter=1000: LET frames=FN b(routine,counter)
 110 LET tf=FN t(): LET idle=(FN i()*512+FN a())
     LET ts=frames*(tf-102)+tf-idle: LET res=ts/counter-82
 120 PRINT "`INK 9`Interrupts:`INK 1`";frames;"`INK 9` idle TS:`INK 2`";idle'"`INK 9`T-States/int.:`INK 3`";tf'"`INK 9`Total T-States:`INK 4`~";ts;"`INK 9`"
     PRINT "`BRIGHT 1``INVERSE 1`Est. T-States:`INVERSE 0`";res;"`INVERSE 1` (`FLASH 1`";FN r();"`FLASH 0`)`TAB 32``BRIGHT 0``INVERSE 0`"
     DIM s$(6): LET len=LEN STR$ FN r(): LET s$(7-len TO 6)=STR$ FN r()
     LPRINT INVERSE 1;s$
     RETURN
1000 REM Estimate T-States/interrupt
     LET adj=FN n(USR #{benchmark[:estimate_tsframes]}): LET idl=FN i()
     LET tf=FN t()
     PRINT "Est. T-States/int.:";idl*512+adj;" (+-4)"
     INPUT "Is this ok? Y/n", a$: IF a$<>"n" AND a$<>"N" THEN RETURN
1010 INPUT "Enter value [";(tf);"]: ";tf: PRINT "T-States/int.:";FN s(tf)
     RETURN
9000 DATA #{benchmark[:set_test_int16]},"`INT`16"
     DATA #{benchmark[:set_test_int24]},"`INT`24"
     DATA #{benchmark[:set_test_int32]},"`INT`32"
     DATA #{benchmark[:set_test_int48]},"`INT`48"
     DATA #{benchmark[:set_test_int64]},"`INT`64"
     DATA #{benchmark[:set_test_int128]},"`INT`128"
     DATA 0,""
9100 DATA 0,9,99,999,9999,10000,19999,59999,65535
9998 STOP
9999 CLEAR #{benchmark.org-1}: LOAD "benchmark"CODE: RUN
EOC
puts benchmark.debug
puts program.to_source escape_keywords: true
puts "test_integer: #{benchmark['+test_integer']}"
puts "test_integer16: #{benchmark['+test_integer16']}"
puts "test_integer24: #{benchmark['+test_integer24']}"
puts "test_integer32: #{benchmark['+test_integer32']}"
puts "test_integer58: #{benchmark['+test_integer48']}"
puts "test_integer64: #{benchmark['+test_integer64']}"
puts "test_integer128: #{benchmark['+test_integer128']}"
puts "test_print_int: #{benchmark['test_print_int']}"
puts "test_trivial_int16: #{benchmark['test_trivial_int16']}"
puts "test_div_int16: #{benchmark['test_div_int16']}"

tap_name = 'bench.ubcd.tap'
program.save_tap tap_name, line: 9999
benchmark.save_tap tap_name, name:'benchmark', append: true
puts "TAP #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
