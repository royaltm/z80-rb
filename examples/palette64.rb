# -*- coding: BINARY -*-
# A simple BASIC SE program to demonstrate ULAplus 64 color palette.
require 'z80'

if __FILE__ == $0
  require 'zxlib/basic'

  def swap_ink_paper(color)
    color ^ 0b00001000
  end

  def grb64_to_grb256(grb64)
    g = (grb64 & 0b110000) << 2
    g |= 0b00100000 unless g == 0
    r = (grb64 & 0b1100) << 1
    r |= 0b00000100 unless r == 0
    b = grb64 & 0b11
    g|r|b
  end

  def grb8_to_rgb9text(grb8)
    g = (grb8 & 0b11100000) >> 5
    r = (grb8 & 0b00011100) >> 2
    b = (grb8 & 0b00000011) << 1
    b |= 1 unless b.zero?
    "RGB9(#{r},#{g},#{b})"
  end

  def color_index_to_attributes(color)
    flash = (color & 0x20) >> 5
    bright = (color & 0x10) >> 4
    color &= 0x0F
    if color < 8
      "FLASH #{flash}: BRIGHT #{bright}: PEN #{color}"
    else
      "FLASH #{flash}: BRIGHT #{bright}: PAPER #{color - 8}"
    end
  end

  palette = [0]*64
  (0..63).each {|i| palette[swap_ink_paper(i)] = grb64_to_grb256(i) }

  puts palette.map.with_index{|c,i| "#{'%2d' % i}: #{'%08b' % c} #{grb8_to_rgb9text(c)} #{color_index_to_attributes(i)}" }

  # SE BASIC
  program = ZXLib::Basic.parse_source <<-EOB
  10 PALETTE 64,1
  20 RESTORE : FOR i=0 TO 63
  30 READ c: PALETTE i,c
  90 NEXT i
 100 DATA #{palette.join(',')}
 150 FOR n=1 TO 4
 200 FOR c=0 TO 3: FORMAT c*&40: PRINT
 210 FOR i=0 TO 7: PAPER i: PRINT "`|8`";: NEXT i
 220 FOR i=0 TO 7: PEN i: PRINT "`#8`";: NEXT i
 230 NEXT c
 240 NEXT n
 990 PAUSE 0
 999 PALETTE 64,0: PAPER 0: PEN 7: FLASH 0: BRIGHT 0: CLS
EOB
  program.save_tap("examples/palette64", line: 10)
  puts program.to_source se:true
  Z80::TAP.parse_file("examples/palette64.tap") { |hb| puts hb.to_s }
end
