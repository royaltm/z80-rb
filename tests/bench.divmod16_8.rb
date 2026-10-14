require 'zxutils/benchmark'
require 'zxlib/basic'

OPTIONS = {
    check0: true, # :check0
    check0_far: false,
    check1: true, # :check1
    check1_far: false,
    modulo:false,
    ignore_cf: false,
    subroutine: false
}

OPTIONS16_8 = OPTIONS.merge({
    k_leq_m: nil,
    optimize: :time, # :size, :time, :time_alt, :unroll, :unroll_alt
})

OPTIONS8 = OPTIONS.merge({
    jump_rel: false,
})

IGNORE_CF_HELPER = OPTIONS[:check0] && OPTIONS[:ignore_cf]
IGNORE_CF_HELPER16_8 = IGNORE_CF_HELPER &&
    !([:time_alt, :unroll_alt].include?(OPTIONS16_8[:optimize]) && !OPTIONS16_8[:modulo])

class BenchDivMod
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
                    ld   [dividend], bc

                    call bm.fn_argn.seek_next
                    jr   NZ, error_q.err
                    call bm.get_arg_bc
                    ld   a, b
                    ora  a
                    report_error_unless Z, "B Integer out of range"
                    ld   a, c
                    ld   [divisor], a
                    ret
    end

    ns :get_quotient do # bc
                    ld   bc, [quotient]
                    ret
    end

    quotient        dw 0
    remainder       db 0
    carryover       db 0
    dividend        dw 0
    divisor         db 0

    def self.create_options(options)
        options.map do |key, val|
            case key
            when :check0, :check1
                case val
                when :ret
                    [key, val]
                when Symbol
                    [key, self.define_label(val)]
                else
                    [key, val]
                end
            else
                [key, val]
            end
        end.to_h
    end

    ns :test_divmod16_8 do # 16+13+4+16+13+4+13=79
                      ld   hl, [dividend]        # 16
                      ld   a, [divisor]          # 13
                      ld   c, a                  # 4
      if OPTIONS[:subroutine]
                      call routine               # 17
                      anda a if IGNORE_CF_HELPER16_8 # 4
      else
        dc!
        dc!"**************************************"
        dc!"***            ROUTINE             ***"
        dc!"**************************************"
        routine       divmod16_8(h, l, c, **create_options(OPTIONS16_8))
        dc!"**************************************"
        dc!
                      anda a if IGNORE_CF_HELPER16_8 # 4
        check0        label
        check1        label
      end
      routine_over    ld   [quotient], hl        # 16
                      ld   [remainder], a        # 13
                      sbc  a, a                  # 4
                      ld   [carryover], a        # 13
                      ret
      if OPTIONS[:subroutine]
        dc!
        dc!"**************************************"
        dc!"***            ROUTINE             ***"
        dc!"**************************************"
        routine       divmod16_8(h, l, c, **create_options(OPTIONS16_8))
        dc!"**************************************"
        dc!
        check0        label
        check1        label
                      pop  de
                      jp   routine_over
      end
    end

    ns :test_divmod8 do # 16+13+4+16+13+4+13=79
                      ld   hl, [dividend]        # 16
                      ld   a, [divisor]          # 13
                      ld   c, a                  # 4
      if OPTIONS[:subroutine]
                      call routine               # 17
                      anda a if IGNORE_CF_HELPER # 4
      else
        dc!
        dc!"**************************************"
        dc!"***            ROUTINE             ***"
        dc!"**************************************"
        routine       divmod8(c, **create_options(OPTIONS8))
        dc!"**************************************"
        dc!
                      anda a if IGNORE_CF_HELPER # 4
          check0      label
          check1      label
      end
      routine_over    ld   [quotient], hl        # 16
                      ld   [remainder], a        # 13
                      sbc  a, a                  # 4
                      ld   [carryover], a        # 13
                      ret
      if OPTIONS[:subroutine]
        dc!
        dc!"**************************************"
        dc!"***            ROUTINE             ***"
        dc!"**************************************"
        routine       divmod8(c, **create_options(OPTIONS8))
        dc!"**************************************"
        dc!
        check0        label
        check1        label
                      pop  de
                      jp   routine_over
      end
    end
end

ENVELOPE_TS = 79 + if OPTIONS[:subroutine] then 17 else 0 end
ZXINTERFACE1 = true

def build_program(base_name, benchmark, rtest, rsize, adjust_for_cf_clr)
    tsframe = benchmark['bm.tsframe']
    dividends = [
        0, 1, 2, 3, 4, 8, 9, 11, 12, 13, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
        0x10, 0x20, 0x40, 0x80, 0x100, 0x200, 0x400, 0x800,
        0x1000, 0x2000, 0x4000, 0x8000,
        0x1111, 0x2222, 0x4444, 0x8888, 0x3333, 0xCCCC,
        0x1234, 0x4321, 0x5678, 0x8765, 0xABCD, 0xDCBA, 0xCDEF, 0xEFCD,
        7, 0xF, 0x1F, 0x3F, 0x7F, 0xFF, 0x1FF, 0x3FF, 0x7FF, 0xFFF, 0x1FFF, 0x3FFF, 0x7FFF, 0xFFFF,
        6, 0xE, 0x1E, 0x3E, 0x7E, 0xFE, 0x1FE, 0x3FE, 0x7FE, 0xFFE, 0x1FFE, 0x3FFE, 0x7FFE, 0xFFFE,
        0xF0, 0xF00, 0xF000, 0xF0F, 0xF0F0,
        5, 0x55, 0x555, 0x5555, 0xA, 0xAA, 0xAAA, 0xAAAA,
        0x5A, 0xA5A, 0x5A5A, 0xA5, 0x5A5, 0xA5A5, 0x55AA, 0xAA55
    ]
    check0 = !!OPTIONS[:check0]
    modulo = OPTIONS[:modulo]
    mstart = if check0 then 0 else 1 end
    total_count = dividends.length * (256-mstart)
    channel = if ZXINTERFACE1 then "T" else "P" end
    envelope_ts = ENVELOPE_TS + if adjust_for_cf_clr then 4 else 0 end
    program = ZXLib::Basic.parse_source <<-EOC
       1 DEF FN n(x)=x-(65536 AND x>=32768): DEF FN q()=USR #{benchmark[:get_quotient]}: DEF FN m()=PEEK #{benchmark[:remainder]}: DEF FN c()=PEEK #{benchmark[:carryover]}
         DEF FN b(a,c)=USR #{benchmark['bm.bench']}: REM benchmark
         DEF FN t()=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM get ts/frame
         DEF FN s(t)=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM set ts/frame
         DEF FN i()=USR #{benchmark['bm.get_idle']}: REM idle
         DEF FN r()=USR #{benchmark[:get_bench_result]}: REM result
         DEF FN z(k,m)=USR #{benchmark[:set_args]}
      10 LET counter=50#{if ZXINTERFACE1 then ': FORMAT "T";19200' end}
      20 PRINT "See results on #{if ZXINTERFACE1 then 'the serial port' else 'ZX Printer' end}"
         OPEN #2,"#{channel}": PRINT "Divmod: T-States size: #{rsize}"
      30 LET sum=0: LET max=0: LET maxi=-1: LET min=1e+38: LET mini=-1: LET v=0
      50 RESTORE 5000: FOR i=1 TO #{dividends.length}: READ k
         FOR m=#{mstart} TO 255
     100 RANDOMIZE FN z(k,m): LET frames=FN b(#{rtest},counter)
         LET w=FN r()-#{envelope_ts}: LET sum=sum+w
         IF w>max THEN LET max=w: LET maxi=k*256+m
         IF w<min THEN LET min=w: LET mini=k*256+m
         IF v<>w THEN PRINT k;"/";m;":";TAB 12;w: LET v=w
     110 #{unless check0 then 'REM ' end}IF FN c()<>0 THEN GO TO 200
         #{unless check0 then 'REM ' end}IF m=0 THEN GO TO 2200
         #{if modulo then 'REM ' end}IF FN q()<>INT(k/m) THEN GO TO 2000
         IF FN m()<>(k-INT(k/m)*m) THEN GO TO 2100
     120 NEXT m: NEXT i
         PRINT '"Sum:";sum
         PRINT "Avg:";INVERSE 1;sum/#{total_count}
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
    2000 CLOSE #2: PRINT '"assertion failed:(Q)";FN q();"`<>`";INT(k/m): GO TO 10000
    2100 CLOSE #2: PRINT '"assertion failed:(R)";FN m();"`<>`";(k-INT(k/m)*m): GO TO 10000
    2200 CLOSE #2: PRINT '"assertion failed:(C)";FN c();"!";m: GO TO 10000
    5000 DATA #{dividends.sort.join(',')}
    9998 STOP
    9999 CLEAR #{benchmark.org-1}: LOAD "benchmark"CODE: RUN
    EOC
    File.open("#{base_name}.out", "w") do |out|
        out.puts benchmark.debug
        out.puts program.to_source escape_keywords: true
    end
    tap_name = base_name + '.tap'
    program.save_tap tap_name, line: 9999
    benchmark.save_tap tap_name, name:'benchmark', append: true
    puts "TAP #{tap_name}:"
    Z80::TAP.parse_file(tap_name) do |hb|
        puts hb.to_s
    end
end

benchmark = BenchDivMod.new 0x8000 # Note: this must be the 0x8000 address at the moment.
test_divmod16_8 = benchmark[:test_divmod16_8]
test_divmod8 = benchmark[:test_divmod8]
routine_size = benchmark["+test_divmod16_8.routine"]
routine8_size = benchmark["+test_divmod8.routine"]

puts "routine divmod16_8 size: #{routine_size} bytes"
puts "routine divmod8    size: #{routine8_size} bytes"

build_program('bench.divmod16_8', benchmark, test_divmod16_8, routine_size, IGNORE_CF_HELPER16_8)
build_program('bench.divmod8', benchmark, test_divmod8, routine8_size, IGNORE_CF_HELPER)
