require 'z80'
require 'zxlib/gfx'
require 'zxlib/sys'
require 'zxlib/basic'

class ScrMoveLinesColsTest
    include Z80
    include Z80::TAP

    macro_import ::ZXLib::Sys
    macro_import ::ZXLib::Gfx
    macro_import ::Z80::MathInt
    label_import  ZXLib::Sys

    dc! "****************************************"
    dc! "***           TABLE  COL=0           ***"
    dc! "****************************************"
    scrmove_table_0     label
    (0..191).each do |i|
        dw  define_label :"scrmove_#{i}_0"
    end

    dc! "****************************************"
    dc! "***           TABLE  COL=1           ***"
    dc! "****************************************"
    scrmove_table_1     label
    (0..191).each do |i|
        dw  define_label :"scrmove_#{i}_1"
    end

    dc! "****************************************"
    dc! "***           TABLE COL=31           ***"
    dc! "****************************************"
    scrmove_table_31    label
    (0..191).each do |i|
        dw  define_label :"scrmove_#{i}_31"
    end

    dc! "****************************************"
    dc! "***          SCRMOVE  COL=0          ***"
    dc! "****************************************"
    scrmove_0_0         ret
    (1..191).each do |i|
        define_label :"scrmove_#{i}_0", scrmove_lines_cols(b, c, i, 0)
        ret
    end
    dc! "****************************************"
    dc! "***          SCRMOVE  COL=1          ***"
    dc! "****************************************"
    (0..191).each do |i|
        define_label :"scrmove_#{i}_1", scrmove_lines_cols(b, c, i, 1)
        ret
    end
    dc! "****************************************"
    dc! "***          SCRMOVE COL=32          ***"
    dc! "****************************************"
    (0..191).each do |i|
        define_label :"scrmove_#{i}_31", scrmove_lines_cols(b, c, i, 31)
        ret
    end

    dc! "****************************************"
    dc! "***           TEST SCRMOVE           ***"
    dc! "****************************************"
    # DEF FN m(y,x,l,c)
    # y - line number 0-191
    # x - column number 0-31
    # l - move lines 0-191
    # c - move columns 0, 1, 31
    ns :test_scrmove do
                call find_args
      error_q   report_error_unless Z, "Q Parameter error"
                call read_line # 0..191
                ld   b, e      # line

                inc  hl
                call find_args.seek_next
                jr   NZ, error_q.err
                call read_column # 0..31
                ld   c, e        # column

                inc  hl
                call find_args.seek_next
                jr   NZ, error_q.err
                call read_line   # 0..191 dlines
                ld   a, e        # dlines
                add  b           # lines + dlines
                jr   C, read_u8_limit.error_b.err
                cp   192
                jr   NC, read_u8_limit.error_b.err
                push de          # dlines

                inc  hl
                call find_args.seek_next
                jr   NZ, error_q.err
                call read_column # 0..31 dcols
                ld   a, e        # dcols
                add  c           # cols + dcols
                jr   C, read_u8_limit.error_b.err
                cp   32
                jr   NC, read_u8_limit.error_b.err

                ld   hl, scrmove_table_0
                ld   a, e        # dcols
                ora  a
                jr   Z, skip_tab
                ld   hl, scrmove_table_1
                cp   1
                jr   Z, skip_tab
                ld   hl, scrmove_table_31
                cp   31
                jr   NZ, read_u8_limit.error_b.err

      skip_tab  pop  de          # dlines
                add  hl, de      #
                add  hl, de      # hl = -> table[dlines]
                ld   e, [hl]
                inc  hl
                ld   d, [hl]     # de = -> scrmove_lines_cols
                ex   de, hl      # hl = -> scrmove_lines_cols
                call calc_screen_address # < bc, > bc
                jp   (hl)
    end

    dc! "****************************************"
    dc! "***          SCREEN ADDRESS          ***"
    dc! "****************************************"
    # DEF FN s(y,x)
    # y - line number 0-191
    # x - column number 0-31
    ns :get_screen_address do
                call find_args
      error_q   report_error_unless Z, "Q Parameter error"
                call read_line # 0..191
                ld   b, e      # line

                inc  hl
                call find_args.seek_next
                jr   NZ, error_q.err
                call read_column # 0..31
                ld   c, e        # column
                jr   calc_screen_address # < bc, > bc
    end

    dc! "****************************************"
    dc! "***             UTILITIES            ***"
    dc! "****************************************"
    # > de = 0..191
    ns :read_line do
                ld   a, 191
                jr   read_u8_limit
    end

    # > de = 0..31
    ns :read_column do
                ld   a, 31
                # jr   read_u8_limit
                # fallthru to read_u8_limit
    end

    # > de = 0..a
    ns :read_u8_limit do
                ex   af, af # a' = limit
                call read_uint
                ld   a, d
                ora  a
      error_b   report_error_unless Z, "B Integer out of range"
                ex   af, af # a = limit
                cp   e      # limit < e
                jr   C, error_b.err
                ret         # e <= limit
    end

    ns :read_uint do
                read_positive_int_value d, e
      error_a   report_error_unless Z, "A Invalid argument"
                inc  hl
                ret
    end

    # < b  = line 0..191
    # < c  = column 0..31
    # > bc = screen address
    # Modifies: af, bc, de
    calc_screen_address ytoscr(b, ah:b, al:e, col:c, t:d)
                ld    c, e
                ret

    find_args   find_def_fn_args 1, subroutine:true
end

include ZXLib
scrmovtest = ScrMoveLinesColsTest::new 65536 - ScrMoveLinesColsTest.code.bytesize
NAME = "test.zxlib.gfx.scrmove_lines_cols"
source = <<-END
   0 DEF FN m(y,x,l,c)=USR #{scrmovtest[:test_scrmove]}: DEF FN s(y,x)=USR #{scrmovtest[:get_screen_address]}
  10 CLS: DIM q(3): RESTORE 5000: FOR i=1 TO 3: READ q(i): NEXT i
  20 FOR y=0 TO 191
     FOR l=0 TO 191-y
     RESTORE 6000: FOR x=1 TO 6: READ x0
     FOR c=1 TO 3: LET c0=q(c)
     IF (x0+c0)>=32 THEN GO TO 40
     LET addr=FN m(y,x0,l,c0)
     LET chck=FN s(y+l,x0+c0)
     IF addr<>chck THEN GO TO 1000
     POKE addr,y+x+1
  40 NEXT c: NEXT x: NEXT l: NEXT y
  99 STOP
 100 CLS: INPUT "y=";y;" x=";x0
     FOR l=0 TO 191-y
     FOR c=1 TO 3: LET c0=q(c)
     IF (x0+c0)>=32 THEN GO TO 140
     LET addr=FN m(y,x0,l,c0)
     LET chck=FN s(y+l,x0+c0)
     IF addr<>chck THEN GO TO 1000
     POKE addr,y+x+1
 140 NEXT c: NEXT l: PAUSE 0: GO TO 100
 999 STOP: RUN
1000 PRINT "        "'"assertion failed:";addr;"`<>`";chck;" ";y;",";x0;",";l;",";c0: GO TO 10000
5000 DATA 0,1,31
6000 DATA 0,1,2,15,30,31
9998 STOP: RUN
9999 CLEAR #{scrmovtest.org-1}: LOAD ""CODE: RUN
END
program = Basic.parse_source source
File.open("#{NAME}.out", "w") do |out|
    out.puts scrmovtest.debug
    out.puts program.to_source escape_keywords: true
end
[0,1,31].each do |x|
    (0..191).each do |y|
        label = "scrmove_#{y}_#{x}"
        puts "#{label.ljust(20)}: 0x#{scrmovtest[label].to_s 16} - #{scrmovtest[label]}, size: #{scrmovtest['+'+label]}"
    end
end
%w[calc_screen_address].each do |label|
    puts "#{label.ljust(20)}: 0x#{scrmovtest[label].to_s 16} - #{scrmovtest[label]}, size: #{scrmovtest['+'+label]}"
end
puts "#{NAME} size: #{scrmovtest.code.bytesize}"

program.save_tap "#{NAME}.tap", line:9999
scrmovtest.save_tap "#{NAME}.tap", append:true
