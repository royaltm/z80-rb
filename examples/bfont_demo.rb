# -*- coding: BINARY -*-
here = File.expand_path('..', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'z80/stdlib'
require 'zxlib/sys'
require 'zxlib/basic'
require 'utils/bigfont'

# NOTE: This code will work only in ZX Spectrum 48k mode.
class Bootstrap
  include Z80

  macro_import  Z80Lib
  import        ZXSys, macros: true, code: false

  big_font_start    addr 0x10000 - 21*8 - BigFont.code.bytesize

  ns :start, use: :big_font_start do |eoc|
              create_chan_and_open output: big_font_start + BigFont.new[:print_char], chan_name: 'X'
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
  bootinit    ld     de, mem.pr_buf   # address of the printer buffer
              push   de               # will exit via pr_buff
              ld     hl, [vars.prog]  # find self
              push   hl               # save PROG value
              ld     bc, start        # offset to start from the beginning of the program data
              add    hl, bc
              memcpy de, hl, +start   # copy start to printer buffer
              pop    hl               # restore PROG value
              ld     bc, bfont        # find start of bfont
              add    hl, bc           # copy bfont to the ramtop memory
              memcpy big_font_start, hl, +bfont
              ret                     # exit via code in the printer buffer

  import      BigFont, :bfont, code: big_font_start
  eop         label
end

bootstrap = Bootstrap.new Basic.parse_source('0 REM').code.bytesize - 1
VARS_PROG = ZXSys.new['vars.prog']
program = Basic.parse_source <<-END
   0 REM `#{bootstrap.code.bytes.join(',')}`
   1 INK 6: PAPER 1: BRIGHT 1: BORDER 1: CLS
  10 PRINT #4;AT 0,0;"HELLO WORLD!";AT 20,0;
  20 FOR i=33 TO 164: PRINT #4;CHR$ i;: NEXT i
  30 STOP
  40 CLS: DIM t$(165-33): FOR i=33 TO 164: LET t$(i-32)=CHR$ i: NEXT i
  50 PRINT #4;AT 0,0;t$
9999 CLEAR VAL "#{bootstrap['big_font_start'] - 1}": RANDOMIZE USR VAL "256*`PEEK`#{VARS_PROG+1}+`PEEK`#{VARS_PROG}+#{bootstrap[:bootinit]}"
END
program.start = 9999

puts bootstrap.debug
puts
puts "big_font_start address: #{bootstrap['big_font_start']}"
puts "big font size: #{bootstrap[:eop] - bootstrap[:bfont]}"
puts "bootstrap start size: #{bootstrap[:bootinit] - bootstrap[:start]}"
puts "bootstrap start offset: #{bootstrap[:start]}"
puts "bootstrap bootinit offset: #{bootstrap[:bootinit]}"
puts "bootstrap size: #{bootstrap[:bfont] - bootstrap[:start]}"
puts "program size: #{program.code.bytesize}"
puts
puts program.to_source escape_keywords:true

tap_name = 'examples/bfont_demo.tap'

program.save_tap tap_name

puts "TAP: #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
