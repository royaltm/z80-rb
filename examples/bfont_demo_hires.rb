# -*- coding: BINARY -*-
here = File.expand_path('../lib', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'zxlib/sys'
require 'zxlib/basic'
require 'zxutils/bigfont'

class BigFontLib
  include Z80
  macro_import      Stdlib
  label_import      ZXLib::Sys

  export :auto

  ns :clear_ink_screen do
              clrmem memT2k.screen0, mem.scrlen
              clrmem memT2k.screen1, mem.scrlen
              ret
  end

  import      ZXUtils::BigFontHires
end

class Bootstrap
  include Z80

  macro_import      Stdlib
  label_import      ZXLib::Sys, macros: true

  big_font_start    addr 0x10000 - 21*8 - BigFontLib.code.bytesize

  ns :start, use: :big_font_start do |eoc|
              # create_chan_and_open output: big_font_start + BigFontHires.new[:print_char_hires], chan_name: 'B'
              create_chan_and_open output: bfont.print_char_hires, chan_name: 'B'
              move_basic_above_scld_screen_memory
              ld    hl, [vars.prog]
              ld    bc, 0             # check if 1st program line has number 0
              call  rom.cp_lines
              jr    NZ, find_last
              call  rom.next_one
              call  rom.reclaim_2     # delete line 0 containing the bootstrap code
    find_last ld    hl, 9999          # find line 9999 in the program
              call  rom.line_addr     # Z=1 if line found, address in HL
              ret   NZ                # line not found
              call  rom.next_one
              call  rom.reclaim_2     # delete line 9999 and return
              ld    hl, 1
              jp    rom.go_to_1       # GO TO line 1
  end
                                      # called from basic line 9999
  bootinit    ld     de, mem.attrs    # address of temporary space (screen attributes in this case, can't use printer buffer on 128k)
              push   de               # will exit via :start routine
              ld     hl, [vars.prog]  # find self
              push   hl               # save PROG value
              ld     bc, start        # offset to :start from the beginning of the program data
              add    hl, bc
              memcpy de, hl, +start   # copy :start routine to temporary space
              pop    hl               # restore PROG value
              ld     bc, bfont        # find start of :bfont
              add    hl, bc           # copy :bfont code to the ramtop memory
              memcpy big_font_start, hl, +bfont
              ret                     # exit via code in the :start routine in temporary space

  import      BigFontLib, :bfont, code: big_font_start, labels: big_font_start
  eop         label
end

include ZXLib

bootstrap = Bootstrap.new Basic.parse_source('0 REM').code.bytesize - 1
bfont_phony = BigFontLib.new bootstrap['big_font_start']
VARS_PROG = Sys.new['vars.prog']
program = Basic.parse_source <<-END
   0 REM `#{bootstrap.code.bytes.join(',')}`
   1 OUT 255,54: GO SUB 1000
  10 PRINT #4;AT 0,0;"HELLO WORLD!";AT 20,0;
  20 FOR i=33 TO 164: PRINT #4;CHR$ i;: NEXT i
  30 STOP
  40 GO SUB 1000: DIM t$(165-33): FOR i=33 TO 164: LET t$(i-32)=CHR$ i: NEXT i
  50 PRINT #4;AT 0,0;t$
 999 STOP: GO TO 1
1000 RANDOMIZE USR #{bfont_phony['clear_ink_screen']}: RETURN
9999 CLEAR VAL "#{bootstrap['big_font_start'] - 1}": RANDOMIZE USR VAL "256*`PEEK`#{VARS_PROG+1}+`PEEK`#{VARS_PROG}+#{bootstrap[:bootinit]}"
END
program.start = 9999

puts bootstrap.debug
puts
puts "big_font_start address: #{bootstrap['big_font_start']}"
puts "clear_ink_screen address: #{bfont_phony['clear_ink_screen']}"
puts "print_char_hires address: #{bfont_phony['print_char_hires']}"
puts "big font size: #{bootstrap[:eop] - bootstrap[:bfont]}"
puts "bootstrap start size: #{bootstrap[:bootinit] - bootstrap[:start]}"
puts "bootstrap start offset: #{bootstrap[:start]}"
puts "bootstrap bootinit offset: #{bootstrap[:bootinit]}"
puts "bootstrap size: #{bootstrap[:bfont] - bootstrap[:start]}"
puts "program size: #{program.code.bytesize}"
puts
puts program.to_source escape_keywords:true

tap_name = 'examples/bfont_demo_hires.tap'

program.save_tap tap_name

puts "TAP: #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
