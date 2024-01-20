require 'z80/utils/sincos'
require 'zxutils/benchmark'
require 'zxlib/basic'

MUL_OPTIMIZE = :time
MUL16_OPTIMIZE = :unroll

class BenchSinCosMul
    include Z80
    include Z80::TAP

    SinCosTable = Utils::SinCos::SinCosTable
    SinCos = Z80::Utils::SinCos::SinCos
    ##
    # Macro for calculating vector element from sin|cos and a radius.
    # <- th|tl: sin|cos(angle)
    # <- a: radius
    # -> h: sincos(angle) * a
    macro :mul_sincos_a do |eoc, th=c, tl=b, tt:bc|
                          rrc  th         # 1: 127|0: 0|-1: 255, CF: 1:1|0:0|-1:1
                          inc  th         # 1: 128, 0: 1, -1: 0, CF: unchanged
                          jp   P, mul_it  # 1: M, 0: P, -1: P
                          ld   h, a       # h: 1 * radius (ignore l)
                          ld   l, 0
                          jr   eoc
      mul_it              mul(tl, a, tt:tt, clrhl:true, signed_k:true, kbit9_carry:true, optimize: MUL_OPTIMIZE)
    end

    macro :mul_sincos_a_signed do |eoc, th=c, tl=b, t:e, tt:bc|
                          anda a
                          jp   P, mul_it_pos # m >= 0
                          ld   t, a
                          neg16 th, tl    # a: 0|1|FF depending on th
                          jr   C, k_over  # 0-0:CF=0, 0-FF:CF=1, 0-1:CF=1, FF-0:CF=0, FF-FF:CF=0
                          xor  a
                          sub  t          # a = -m

      mul_it_pos          rrc  th         # 1: 127|0: 0|-1: 255, CF: 1:1|0:0|-1:1
                          inc  th         # 1: 128, 0: 1, -1: 0, CF: unchanged
                          jp   P, mul_it  # 1: M, 0: P, -1: P
      skip_inv_m          label
                          add  a, h unless clrhl
                          ld   h, a       # h: 1 * radius
                          ld   l, 0 if clrhl
                          jr   eoc
                          # a: -k, (-1)*(-m)=m, 1*(-m)=-m
      k_over              jp   M, skip_inv_m
                          xor  a
                          sub  t          # a = -m
                          jr   skip_inv_m
      mul_it              mul(tl, a, tt:tt, clrhl:true, signed_k:true, kbit9_carry:true, optimize: MUL_OPTIMIZE)
    end

    macro_import        MathInt
    macro_import        ZXLib::Math
    label_import        ZXLib::Sys
    import              ZXUtils::Benchmark, :bm, macros: true

    get_bench_result    calculate_benchmark_tstates(bm.counter, bm.tsframe, bm.frames, bm.idle, bm.adjustment)
    estimate_tsframes   estimate_tstates_per_interrupt(vars.udg, bm.interrup_vec, bm.forward, bm.tsframe, bm.idle)

    macro_import        Utils::SinCos

    sincos      addr 0xF000, SinCos
    make_sincos create_sincos_from_sintable sincos, sintable:sintable
    sintable    bytes   neg_sintable256_pi_half_no_zero_lo

    dc!
    dc!"*********************************************"
    dc!"***                BENCHES                ***"
    dc!"*********************************************"

    ns :set_args   do
                    call bm.fn_argn
                    jr   NC, error_q
                    exx
                    push hl
                    call make_sincos
                    pop  hl
                    exx
                    ret
        error_q     report_error_unless Z, 'Q Parameter error'
                    call bm.get_arg_bc
                    xor  a
                    ora  b
        error_6     report_error_unless Z, '6 Number too big'
                    ld   a, c
                    push hl
                    sincos_from_angle sincos, h, l
                    ld   de, sincos_args
                    ld   bc, 4
                    ldir
                    pop  hl
                    call bm.fn_argn.seek_next
                    jr   NZ, error_q.err
                    read_integer_value b, c, e
        error_a     report_error_unless Z, "A Invalid argument"
                    ld   a, e
                    cp   b
        error_b     report_error_unless Z, "B Integer out of range"
                    ld   a, c
                    ld   [radius], a
                    ret
    end

    ns :ahl_ret do # a|hl
                    ld   de, 0
                    ld   c, h
                    ld   b, l
                    anda a
      sknegs        jp   P, skipzch
                    neg_int c, b, t:e, t_is_zero:true
                    ora  0x80 # SF = 1
      skipzch       integer32_to_fp e, d, c, b, sgn:M
    end

    return_fp       return_with_fp restore_iy:nil, restore_hl_alt:nil

    ns :get_x do
                    ld   hl, [x_coord]
                    ld   a, [sincos_args.cos + 1]
                    jp   ahl_ret
    end

    ns :get_y do
                    ld   hl, [y_coord]
                    ld   a, [sincos_args.sin + 1]
                    jp   ahl_ret
    end

    ns :test_sincos do # 13+20+16+13+20+16=98
                    ld   a,  [radius]   # 13
                    ld   bc, [sincos_args.sin]  # 20
      dc!
      dc!"*********************************************"
      dc!"***             ROUTINE (sin)             ***"
      dc!"*********************************************"
      routine       mul_sincos_a b, c, tt:bc
      dc!"*********************************************"
      dc!
                    ld   [y_coord], hl  # 16
                    ld   a,  [radius]   # 13
                    ld   de, [sincos_args.cos]  # 20
      dc!"*********************************************"
      dc!"***             ROUTINE (cos)             ***"
      dc!"*********************************************"
                    mul_sincos_a d, e, tt:de
      dc!"*********************************************"
      dc!
                    ld   [x_coord], hl  # 16
                    ret
    end

    ns :test_sincos8 do # 13+20+16+13+20+16=98
                    ld   a,  [radius]   # 13
                    ld   bc, [sincos_args.sin]  # 20
      dc!
      dc!"*********************************************"
      dc!"***             ROUTINE (sin)             ***"
      dc!"*********************************************"
      routine       mul8(b, c, a, tt:bc, clrhl:true, double:false, optimize: MUL_OPTIMIZE)
      dc!"*********************************************"
      dc!
                    ld   [y_coord], hl  # 16
                    ld   a,  [radius]   # 13
                    ld   de, [sincos_args.cos]  # 20
      dc!"*********************************************"
      dc!"***             ROUTINE (cos)             ***"
      dc!"*********************************************"
                    mul8(d, e, a, tt:de, clrhl:true, double:false, optimize: MUL_OPTIMIZE)
      dc!"*********************************************"
      dc!
                    ld   [x_coord], hl  # 16
                    ret
    end

    ns :test_sincos16 do # 13+20+16+13+20+16=98
                    ld   a,  [radius]   # 13
                    ld   bc, [sincos_args.sin]  # 20
      dc!
      dc!"*********************************************"
      dc!"***             ROUTINE (sin)             ***"
      dc!"*********************************************"
      routine       mul16(b, c, a, tt:bc, optimize: MUL16_OPTIMIZE)
      dc!"*********************************************"
      dc!
                    ld   [y_coord], hl  # 16
                    ld   a,  [radius]   # 13
                    ld   de, [sincos_args.cos]  # 20
      dc!"*********************************************"
      dc!"***             ROUTINE (cos)             ***"
      dc!"*********************************************"
                    mul16(d, e, a, tt:de, optimize: MUL16_OPTIMIZE)
      dc!"*********************************************"
      dc!
                    ld   [x_coord], hl  # 16
                    ret
    end

    radius          db 0
    sincos_args     data SinCos, [0, 0]
    x_coord         dw 0
    y_coord         dw 0
end

ZXINTERFACE1 = true

benchmark = BenchSinCosMul.new 0x8000 # Note: this must be the 0x8000 address at the moment.
tsframe = benchmark['bm.tsframe']
test_sincos = benchmark[:test_sincos]
test_sincos8 = benchmark[:test_sincos8]
test_sincos16 = benchmark[:test_sincos16]
radius = benchmark[:radius]
get_x = benchmark[:get_x]
get_y = benchmark[:get_y]
sin_arg = benchmark["sincos_args.sin"]
cos_arg = benchmark["sincos_args.cos"]
routine_size = benchmark["+test_sincos.routine"]
routine8_size = benchmark["+test_sincos8.routine"]
routine16_size = benchmark["+test_sincos16.routine"]
# rsize, rtest = routine_size, test_sincos
# rsize, rtest = routine8_size, test_sincos8
rsize, rtest = routine16_size, test_sincos16
channel = if ZXINTERFACE1 then "T" else "P" end
program = ZXLib::Basic.parse_source <<-EOC
   1 DEF FN n(x)=x-(65536 AND x>=32768): DEF FN x()=USR #{get_x}: DEF FN y()=USR #{get_y}
     DEF FN b(a,c)=USR #{benchmark['bm.bench']}: REM benchmark
     DEF FN t()=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM get ts/frame
     DEF FN s(t)=USR #{benchmark['bm.getset_tsframe']}+65536*PEEK #{tsframe+2}: REM set ts/frame
     DEF FN i()=USR #{benchmark['bm.get_idle']}: REM idle
     DEF FN r()=USR #{benchmark[:get_bench_result]}: REM result
     DEF FN z(a,r)=USR #{benchmark[:set_args]}
     RANDOMIZE USR #{benchmark[:set_args]}
  10 LET counter=10#{if ZXINTERFACE1 then ': FORMAT "T";19200' end}
  20 PRINT "See results on ZX Printer": OPEN #2,"#{channel}": PRINT "SinCos: T-States size: #{rsize}"
  30 LET sum=0: LET max=0: LET maxi=-1: LET min=1e+38: LET mini=-1: LET v=0
  50 FOR r=0 TO 255: RESTORE: FOR a=0 TO 255
 100 RANDOMIZE FN z(a,r): LET frames=FN b(#{rtest},counter)
     LET w=FN r()-98: LET sum=sum+w
     IF w>max THEN LET max=w: LET maxi=r*256+a
     IF w<min THEN LET min=w: LET mini=r*256+a
     IF v<>w THEN PRINT r;"*sincos(";a;"):";TAB 16;w: LET v=w
     LET y=FN y(): LET x=FN x(): READ sin,cos
 110 IF ABS(y-r*sin)>r THEN GO TO 2000
     IF ABS(x-r*cos)>r THEN GO TO 2100
     LET y=INT(y/256)+88: LET x=INT(x/256)+128: IF y<=175 AND y>=0 AND x<=255 AND x>= 0 THEN PLOT x,y
 120 NEXT a: NEXT r
     PRINT '"Sum:";sum
     PRINT "Avg:";INVERSE 1;sum/#{256*256}
     PRINT "Max:";INVERSE 1;max;INVERSE 0;" for ";INT(maxi/256);"*sincos ";maxi-INT(maxi/256)*256
     PRINT "Min:";INVERSE 1;min;INVERSE 0;" for ";INT(mini/256);"*sincos ";mini-INT(mini/256)*256
     CLOSE #2
 199 STOP
1000 REM Estimate T-States/interrupt
     LET adj=FN n(USR #{benchmark[:estimate_tsframes]}): LET idl=FN i(): 
     PRINT "Est. T-States/int.:";idl*512+adj;" (+-4)"
     INPUT "Is this ok? Y/n", a$: IF a$<>"n" AND a$<>"N" THEN RETURN
     INPUT "Enter value: ",tf: PRINT "T-States/int.:";FN s(tf)
     RETURN
2000 CLOSE #2: PRINT '"assertion failed:y:";FN y();"`<>`";r*256*SIN (a*PI/128): GO TO 3000
2100 CLOSE #2: PRINT '"assertion failed:x:";FN x();"`<>`";r*256*COS (a*PI/128): GO TO 3000
3000 LET sin=PEEK #{sin_arg}+256*PEEK #{sin_arg+1}
     LET cos=PEEK #{cos_arg}+256*PEEK #{cos_arg+1}
     LET radius=PEEK #{radius}
     PRINT "sin=";sin;" cos=";cos;" r=";radius
     PRINT "sin=";sin/256;" cos=";cos/256;" r=";radius
     GO TO 10000
5000 DATA #{(0..255).map{|a| "#{(Math.sin(a*Math::PI/128)*256).round},#{(Math.cos(a*Math::PI/128)*256).round}"}.join(",")}
9998 STOP
9999 CLEAR #{benchmark.org-1}: LOAD "benchmark"CODE: GO SUB 1000: RUN
EOC
puts benchmark.debug
puts program.to_source escape_keywords: true
puts
puts "routine   size: #{routine_size} bytes"
puts "routine8  size: #{routine8_size} bytes"
puts "routine16 size: #{routine16_size} bytes"

tap_name = 'bench.sincos_mul.tap'
program.save_tap tap_name, line: 9999
benchmark.save_tap tap_name, name:'benchmark', append: true
puts "TAP #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
