# -*- coding: BINARY -*-
here = File.expand_path('..', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'zxlib/sys'
require 'zxlib/basic'
require 'utils/bigfont'

class Program
  include Z80
  include Z80::TAP

  import ZXSys, macros: true, code: false

  ns :start do
        create_chan_and_open output: print_char, chan_name: 'X'
        ret
  end

  import BigFont
  eop    label
end

bfont = Program.new 65536 - 21*8 - Program.code.bytesize
puts bfont.debug
puts
puts "start at: #{bfont.org}"
puts "total size: #{bfont.code.bytesize}"
puts "print_char size: #{bfont[:eop] - bfont[:print_char]}"

program = Basic.parse_source <<-END
   1 INK 6: PAPER 1: BRIGHT 1: BORDER 1: CLS
  10 PRINT #4;AT 0,0;"HELLO WORLD!";AT 20,0;
  20 FOR i=33 TO 164: PRINT #4;CHR$ i;: NEXT i
  30 STOP
  40 CLS: DIM t$(165-33): FOR i=33 TO 164: LET t$(i-32)=CHR$ i: NEXT i
  50 PRINT #4;AT 0,0;t$
9998 STOP
9999 CLEAR VAL "#{bfont.org - 1}": LOAD ""CODE : RANDOMIZE USR VAL "#{bfont.org}": RUN
END
program.start = 9999

puts
puts program.to_source escape_keywords:true

tap_name = 'examples/bfont_demo.tap'

program.save_tap tap_name
bfont.save_tap tap_name, append:true, name:'bigfont'

puts "TAP: #{tap_name}:"
Z80::TAP.parse_file(tap_name) do |hb|
    puts hb.to_s
end
