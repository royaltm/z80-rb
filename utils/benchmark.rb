# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'
require 'zxlib/sys'
##
# =ZXBenchmark
#
# Benchmarking utilities.
#
# NOTE:: Currently the code must be located at 0x8000.
#
# Example:
#
#     require 'utils/benchmark'
#     require 'zxlib/basic'
#     
#     class BenchNeg16
#         include Z80
#         include Z80::TAP
#     
#         export test_neg16
#     
#         label_import        ZXSys
#         import              ZXBenchmark, :bm, macros: true
#     
#         get_bench_result    calculate_benchmark_tstates(bm.counter, bm.tsframe, bm.frames, bm.idle, bm.adjustment)
#         estimate_tsframes   estimate_tstates_per_interrupt(vars.udg, bm.interrup_vec, bm.forward, bm.tsframe, bm.idle)
#     
#         ns :test_neg16 do
#                     xor  a
#                     sub  l
#                     ld   l, a
#                     sbc  a, a
#                     sub  h
#                     ld   h, a
#                     ret
#         end
#     end
#     
#     benchmark = BenchNeg16.new 0x8000 # Note: this must be the 0x8000 address at the moment.
#     tsframe = benchmark['bm.tsframe']
#     program = Basic.parse_source <<-EOC
#        1 DEF FN n(x)=x-(65536 AND x>=32768)
#          DEF FN b(a,c)=USR #{benchmark['bm.bench']}: REM benchmark
#          DEF FN t()=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM get ts/frame
#          DEF FN s(t)=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM set ts/frame
#          DEF FN i()=USR #{benchmark['bm.get_idle']}: REM idle
#          DEF FN r()=USR #{benchmark[:get_bench_result]}: REM result
#       10 LET counter=65535: LET frames=FN b(#{benchmark[:test_neg16]},counter)
#          PRINT "Interrupts: ";frames
#          PRINT "T-States: ";FN r()
#          STOP
#      100 REM Estimate T-States/interrupt
#          LET adj=FN n(USR #{benchmark[:estimate_tsframes]}): LET idl=FN i(): 
#          PRINT "Est. T-States/int.:";idl*512+adj;" (+-4)"
#          INPUT "Is this ok? Y/n", a$: IF a$<>"n" AND a$<>"N" THEN RETURN
#          INPUT "Enter value: ",tf: PRINT "T-States/int.:";FN s(tf)
#          RETURN
#     9998 STOP
#     9999 CLEAR #{benchmark.org-1}: LOAD "benchmark"CODE: GO SUB 100: RUN
#     EOC
#     puts benchmark.debug
#     puts program.to_source escape_keywords: true
#     program.save_tap "testneg16", line: 9999
#     benchmark.save_tap "testneg16", name: "benchmark", append: true
#     puts "TAP: testneg16.tap:"
#     Z80::TAP.parse_file('testneg16.tap') do |hb|
#         puts hb.to_s
#     end
#
# See also: ZXBenchmark::Macros
class ZXBenchmark
    include Z80
    include Z80::TAP

    export bench
    export start
    export nop_test
    export getset_tsframe
    export get_frames
    export get_idle
    export get_adjustment
    export overflow_err
    export fn_argn
    export get_arg_bc
    export get_arg_debc
    export routine
    export counter
    export frames
    export idle
    export adjustment
    export tsframe
    export forward
    export interrup_vec

    ##
    # =ZXBenchmark macros.
    module Macros
        include Z80MathInt::Macros
        include ZXSys::Macros
        ## 
        # Returns the benchmark result.
        #
        # Calculates: (frames*(tsframe - 102) + tsframe - (idle*512 + signed adjustment)) / counter - 82
        # Limit: Tested routine should not take longer than 65453 T-states
        def calculate_benchmark_tstates(counter, tsframe, frames, idle, adjustment)
            with_saved :exx, hl, :exx, ret: true, isolate: true do
                        # a|hl = tsframe-102
                        ld   hl, [tsframe.bytes[0]]
                        ld   a, [tsframe.bytes[2]]
                        ld   c, a
                        ld   de, -102
                        add24_16(c, hl, de, signed:true) # a|hl = tsframe-102
                        # a|hl|de = frames*(tsframe-102)
                        ld   bc, [frames]
                        push af                # a: res.hi24 (tsframe-102).hi24
                        push bc
                        mul16_32(bc, tt:bc) # hl|hl'=res.lo * frames
                        pop  de                # de: frames
                        pop  af                # a: res.hi24
                        push hl                # res2.hi: (res.lo * frames).hi (4) 300898
                        exx
                        push hl                # res2.lo: (res.lo * frames).lo (38754)
                        exx
                        ld   b, a              # b: res.hi24
                        mul8_24(d, e, b, t:c, tt:de, clrahl:true) # a|hl = res.hi24*frames
                        pop  de                # res2.lo
                        pop  bc                # res2.hi
                        add  hl, bc
                        adc  0                 # res3: a|hl|de = frames*(tsframe-102)
                overflw report_error_unless Z, 'B Integer out of range'
                        exx                    # 0|hl'|de' = frames*(tsframe-102)
                        # c|hl = idle*512
                        ld   hl, [idle]        # hl: idle, a: 0
                        ld   c, h              # idle*512 (<< 9) c=h|h=l|l=0
                        ld   h, l
                        ld   l, a
                        sla  h
                        rl   c                 # c|hl: idle*512
                        # a|hl = idle*512 + signed adjustment
                        ld   de, [adjustment]
                        add24_16(c, hl, de, signed:true)  # a|hl = idle*512 + adjustment
                        # a|hl = tsframe - (idle*512 + adjustment)
                        ld   c, a
                        ex   de, hl                       # c|de: idle*512 + adjustment
                        ld   hl, [tsframe.bytes[0]]
                        ld   a, [tsframe.bytes[2]]        # a|hl: tsframe
                        ora  a                            # CF=0
                        sbc  hl, de                       # a|hl: tsframe-(idle*512 + adjustment)
                        sbc  c
                        # hl|de = frames*(tsframe-102) + tsframe - (idle*512 + adjustment)
                        push hl                           #........hl'|de' + a|hl
                        exx                               # hl|de: frames*(tsframe-102)
                        pop  bc                           #  a|bc: tsframe-(idle*512 + adjustment)
                        ex   de, hl
                        add  hl, bc
                        ex   de, hl                       # de: de + bc
                        ld   b, 0
                        ld   c, a                         # bc: 0|a
                        adc  hl, bc                       # hl|de: frames*(tsframe-92-22) + tsframe-(idle*512 + adjustment)
                        jr   C, overflw.err
                        # +hl+|+hl' = round((frames*(tsframe-102) + tsframe - (idle*512 + adjustment)) / counter)
                        push de
                        exx
                        pop  hl
                        ld   de, [counter]
                        push de
                        srl  d
                        rr   e                            # counter/2
                        add  hl, de                       # hl' = hl' + counter/2
                        exx
                        ld   de, 0
                        adc  hl, de                       # +hl+|+hl' = +hl+|+hl' + counter/2
                        jr   C, overflw.err
                        pop  de                           # counter
                        divmod32_16 check0:overflw.err    # +hl+|+hl' = +hl+|+hl'+ / +de+
                        ld   a, l
                        ora  h
                        jp   NZ, overflw.err
                        exx
                        # +bc+ = round((frames*(tsframe-102) + tsframe - (idle*512 + adjustment)) / counter) - 82
                        ld   bc, 82
                        sbc  hl, bc
                        ld16 bc, hl
            end
        end
        ##
        # Estimates the number of T-States between interrupts.
        def estimate_tstates_per_interrupt(stack_end, interrup_vec, forward, tsframe, idle)
            isolate do
                            di
                            ld   [restore_sp+1], sp
                            ld   sp, stack_end
                            ld   hl, inthandler
                            ld   [forward+1], hl
                            ld   a, interrup_vec>>8
                            ld   i, a
                            im2
                            ld   hl, finale 
                            ei
                            halt                 # 19 (accepting interrupt) + 10 (forward) + 14 (inthandler)
                            ld   [forward+1], hl # 16
                            ld   hl, forever     # 10
                            ld   bc, 0           # 10
                                                 #=79
                forever     inc  c               # 4
                            126.times { nop }    # 126*4
                            jp   (hl)            # 4
                                                 #=512
                finale      ld   [idle], bc     # idle*512 + (pc==forever ? 0 : (pc - finale)*4) + 79
                            pop  de              # de: pc
                            xor  a
                            sbc  hl, de          # hl: forever - pc  (pc >= forever)
                            jr   Z, no_adjust    # hl: 0
                            ex   de, hl          # hl: pc
                            ld   de, finale
                            xor  a
                            sbc  hl, de          # pc - finale
                            add  hl, hl          # (pc - finale)*4
                            add  hl, hl          # hl: (pc - finale)*4
                no_adjust   ld   a, 79
                            adda_to h,l          # hl: (pc - finale)*4 + 79
                            ex   de, hl          # de: (pc - finale)*4 + 79
                            ld   h, c            # idle*512 (<< 9) b=0|h=c|l=0
                            ld   l, b
                            sla  h
                            rl   b               # b|hl: idle*512
                                                 # idle*512 + (pc - finale)*4 + 79
                            add24_16(b, hl, de, signed:true)
                            ld   [tsframe.bytes[0]], hl
                            ld   [tsframe.bytes[2]], a
                                                 # return (pc - finale)*4 + 79
                            ld16 bc, de

                restore_sp  ld   sp, 0
                            restore_rom_interrupt_handler
                            ret
                                                 
                inthandler  ei                   # 4
                            ret                  # 10
            end
        end
    end

    import       ZXSys, macros: true, code: false
    macro_import Z80MathInt

    ##
    # Benchmarks the tested routine. Provide a +routine+ address and a +counter+.
    # Returns a number of seconds (multiplied by the interrupt frequency - 50Hz) that have passed.
    # This callback is to be used from the ZX Basic.
    #
    #   1 DEF FN b(a,c)=USR #{program['benchmark.bench']}
    ns :bench do
                    call fn_argn
                    jr   C, start
        error_q     report_error_unless Z, 'Q Parameter error'
                    call get_arg_bc
                    ld   [routine], bc
                    call fn_argn.seek_next
                    jr   NZ, error_q.err
                    call get_arg_bc
                    ld   [counter], bc
    end
    ##
    # A benchmark start entry for the machine-language.
    #
    # Provide a +routine+ and a +counter+ address in the memory addressed by the appropriate labels.
    with_saved :start, iy, :exx, hl, :exx, ret: true, use: vars do
                    ld   hl, [routine]
                    ld   [routine_a + 1], hl
                    ld   hl, -1
                    ld   [frames], hl
                    inc  hl
                    ld   [idle], hl
                    ld   bc, [counter]
                    di
                    ld   [restore_sp+1], sp
                    ld   sp, [vars.udg]
                    ld   hl, inthandler
                    ld   [forward+1], hl
                    ld   a, interrup_vec>>8
                    ld   i, a
                    im2
                    ei
                    halt                 # 19 (accepting interrupt) + 10 (forward) + 73 (inthandler)
                                         #=102
        iterate     push bc              # 11
        routine_a   call nop_test        # 17+10+tested
                    pop  bc              # 10
                    dec  bc              #  6
                    ld   a, c            #  4
                    ora  b               #  4
                    ld   hl, finale      # 10
                    jp   NZ, iterate     # 10
                                         #=82+tested
                    # Each full frame t-states: frames*(69888-102)
                                         # BC: 0
                    ld   [forward+1], hl # 16
                    ld   hl, forever     # 10
                                         #=26
        forever     inc  c               # 4
                    126.times { nop }    # 126*4
                    jp   (hl)            # 4
                                         #=512
                    # Last frame: tsframe-(idle*512 + (pc==forever ? 0 : (pc - finale)*4) + 102 + 26)
        finale      ld   [idle], bc     # idle*512 + (pc==forever ? 0 : (pc - finale)*4) + 79
                    pop  de              # de: pc
                    xor  a
                    ld   hl, forever
                    sbc  hl, de          # hl: forever - pc
                    jr   Z, no_adjust    # hl: 0 (pc == forever)
                    jr   C, calc_idle   # (pc > forever)
                    ld   hl, -10         # (pc < forever)
                    jr   no_adjust
        calc_idle  ex   de, hl          # hl: pc
                    ld   de, finale
                    xor  a
                    sbc  hl, de          # pc - finale
                    add  hl, hl          # (pc - finale)*4
                    add  hl, hl          # hl: adjust = (pc - finale)*4
        no_adjust   add  102 + 26
                    adda_to h,l          # hl: adjust + 102 + 26
                    ld   [adjustment], hl
        restore_sp  ld   sp, 0
                    restore_rom_interrupt_handler
                    ld   bc, [frames]    # return the number of frames that have passed
    end

    nop_test        nop
                    ret

    inthandler      push hl           # 11
                    ld   hl, [frames] # 16
                    inc  hl           # 6
                    ld   [frames], hl # 16
                    pop  hl           # 10
                    ei                # 4
                    ret               # 10
                                      #=73

    ##
    # Returns a less significant 16-bit unsigned integer. Add 65536 to get the actual value.
    ns :getset_tsframe do
                    call fn_argn
                    jr   C, only_get
                    jr   NZ, only_get
                    call get_arg_debc
                    ld   a, d
                    ora  a
                    jp   NZ, overflow_err
                    ld   [tsframe.bytes[0]], bc
                    ld   a, e
                    ld   [tsframe.bytes[2]], a
                    ret
        only_get    ld   bc, [tsframe]
                    ret
    end
    ##
    # Returns an unsigned integer
    ns :get_frames do
                    ld   bc, [frames]
                    ret
    end
    ##
    # Returns an unsigned integer
    ns :get_idle do
                    ld   bc, [idle]
                    ret
    end
    ##
    # Returns a signed integer. Convert with: LET x=x-(65536 AND x>=32768)
    ns :get_adjustment do
                    ld   bc, [adjustment]
                    ret
    end

    overflow_err    report_error 'B Integer out of range'

    fn_argn         find_def_fn_args 1, cf_on_direct:true

    ns :get_arg_bc  do
                    read_positive_int_value(b, c)
        check_z     report_error_unless Z, 'A Invalid argument'
                    inc  hl
                    ret
    end

    get_arg_debc    read_integer32_value(de, bc)
                    report_error_unless NC, '6 Number too big'
                    jr   get_arg_bc.check_z

    routine         dw nop_test
    # result: (frames*(tsframe - 102) + tsframe - (idle*512 + signed adjustment)) / counter - 82
    counter         dw 256
    frames          dw 0
    idle            dw 0
    adjustment      dw 0 # signed
    tsframe         int 24, 69888 # 70908 - 128k

    # The following assumes org will be at 0x8000.
                    org 0x0282
    forward         jp inthandler        # 10

                    org align:256
    interrup_vec    dw [forward]*129
end

if __FILE__ == $0
    require 'zxlib/basic'
    # :stopdoc:

    class Bench
        include Z80
        include Z80::TAP

        label_import        ZXSys
        import              ZXBenchmark, macros: true, labels: true, code: true

        get_result          calculate_benchmark_tstates(counter, tsframe, frames, idle, adjustment)
        estimate_tsframes   estimate_tstates_per_interrupt(vars.udg, interrup_vec, forward, tsframe, idle)
    end
    benchmark = Bench.new 0x8000
    puts benchmark.debug
    tsframe = benchmark[:tsframe]
    program = Basic.parse_source <<-EOC
       1 DEF FN n(x)=x-(65536 AND x>=32768)
         DEF FN b(a,c)=USR #{benchmark[:bench]}: REM benchmark
         DEF FN a()=FN n(USR #{benchmark[:get_adjustment]}): REM adjustment
         DEF FN i()=USR #{benchmark[:get_idle]}: REM idle
         DEF FN r()=USR #{benchmark[:get_result]}: REM result
         DEF FN f()=USR #{benchmark[:get_frames]}: REM frames
         DEF FN t()=USR #{benchmark[:getset_tsframe]}+65536*PEEK #{tsframe+2}: REM get ts/frame
         DEF FN s(t)=USR #{benchmark[:getset_tsframe]}+65536*PEEK #{tsframe+2}: REM set ts/frame
      10 LET counter=65535: LET frames=FN b(#{benchmark[:nop_test]},counter)
      20 LET tf=FN t(): LET idle=(FN i()*512+FN a())
         LET ts=frames*(tf-102)+tf-idle: LET res=ts/counter-82
      30 PRINT "`INK 9`Interrupts:`INK 1`";frames;"`INK 9` idle TS:`INK 2`";idle'"`INK 9`T-States/int.:`INK 3`";tf'"`INK 9`Total T-States:`INK 4`~";ts;"`INK 9`"
         PRINT "`BRIGHT 1``INVERSE 1`Est. T-States:`INVERSE 0`";res;"`INVERSE 1` (`FLASH 1`";FN r();"`FLASH 0`)`TAB 32``BRIGHT 0``INVERSE 0`"
         STOP
     100 REM Estimate T-States/interrupt
         LET adj=FN n(USR #{benchmark[:estimate_tsframes]}): LET idl=FN i(): 
         PRINT "Est. T-States/int.:";idl*512+adj;" (+-4)"
         INPUT "Is this ok? Y/n", a$: IF a$<>"n" AND a$<>"N" THEN RETURN
     110 INPUT "Enter value: ",tf: PRINT "T-States/int.:";FN s(tf)
         RETURN
    9998 STOP
    9999 CLEAR #{benchmark.org-1}: LOAD "benchmark"CODE: CLS: PRINT " `GO TO`  10 - benchmark"'"`GO SUB`100 - estimate TS/int."
    EOC
    puts program.to_source escape_keywords: true
    program.save_tap "testbench", line: 9999
    benchmark.save_tap "testbench", name: "benchmark", append: true
    puts "TAP: testbench.tap:"
    Z80::TAP.parse_file('testbench.tap') do |hb|
        puts hb.to_s
    end
end
