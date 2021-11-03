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

    label_import        ZXSys
    macro_import        Z80MathInt
    import              ZXUtils::Benchmark, :bm, macros: true

    get_bench_result    calculate_benchmark_tstates(bm.counter, bm.tsframe, bm.frames, bm.idle, bm.adjustment)
    estimate_tsframes   estimate_tstates_per_interrupt(vars.udg, bm.interrup_vec, bm.forward, bm.tsframe, bm.idle)

    ns :test_print_int do
                    utobcd bcd_buffer_end, test_integer, size: +test_integer, r: d, rr: de, byteorder: :lsb
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
    # 10000:  422
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
    #     0: 732
    #     9: 732
    #    99: 1410
    #   999: 2142
    #  9999: 2948
    # 10000: 3604
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

    test_integer    as  test_integer16
                                                                # f(0)-f(max) (without bcdtoa)
    test_integer16  int 16,  59999                              #  1510- 2143 ( 1384- 1863)
    # test_integer16  int 16,  0xFFFF                             #  1510- 2143 ( 1384- 1863)
    # test_integer24  int 24,         0                           #  2180- 3728 ( 2054- 3367)
    # test_integer24  int 24,   9999999                           #  2180- 3728 ( 2054- 3367)
    # test_integer24  int 24,  16999999                           #  2180- 3728 ( 2054- 3367)
    test_integer24  int 24,  0xFFFFFF                           #  2180- 3728 ( 2054- 3367)
    test_integer32  int 32,  0xFFFFFFFF                         #  2850- 5678 ( 2724- 5240)
    test_integer48  int 48,  0xFFFFFFFFFFFF                     #  4190-10854 ( 4064-10189)
    test_integer64  int 64,  0xFFFFFFFFFFFFFFFF                 #  5530-17546 ( 5404-16723)
    test_integer128 int 128, 299999999999999999999999999999999999999 # 10890-60179 (10764-58590)
    # test_integer128 int 128, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF # 10890-60179 (10764-58590)
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
  10 LET counter=1000: LET frames=FN b(#{benchmark[:test_print_int]},counter)
  20 LET tf=FN t(): LET idle=(FN i()*512+FN a())
     LET ts=frames*(tf-102)+tf-idle: LET res=ts/counter-82
  30 PRINT "`INK 9`Interrupts:`INK 1`";frames;"`INK 9` idle TS:`INK 2`";idle'"`INK 9`T-States/int.:`INK 3`";tf'"`INK 9`Total T-States:`INK 4`~";ts;"`INK 9`"
     PRINT "`BRIGHT 1``INVERSE 1`Est. T-States:`INVERSE 0`";res;"`INVERSE 1` (`FLASH 1`";FN r();"`FLASH 0`)`TAB 32``BRIGHT 0``INVERSE 0`"
     STOP
 100 REM Estimate T-States/interrupt
     LET adj=FN n(USR #{benchmark[:estimate_tsframes]}): LET idl=FN i()
     LET tf=FN t()
     PRINT "Est. T-States/int.:";idl*512+adj;" (+-4)"
     INPUT "Is this ok? Y/n", a$: IF a$<>"n" AND a$<>"N" THEN RETURN
 110 INPUT "Enter value [";(tf);"]: ";tf: PRINT "T-States/int.:";FN s(tf)
     RETURN
9998 STOP
9999 CLEAR #{benchmark.org-1}: LOAD "benchmark"CODE: RUN
EOC
puts benchmark.debug
puts program.to_source escape_keywords: true
puts "test_integer: #{benchmark['+test_integer']}"
puts "test_integer16: #{benchmark['+test_integer16']}"
puts "test_integer24: #{benchmark['+test_integer24']}"
puts "test_integer32: #{benchmark['+test_integer32']}"
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
