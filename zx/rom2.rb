# -*- coding: BINARY -*-
require 'z80'
require 'z80/stdlib.rb'
require 'z80/math_i.rb'
require 'zx/gfx+8.rb'
require 'zx/gfx+.rb'
require 'zx/sound.rb'
class MemoryMap
	include Z80

	export	:auto

	romtop	0x4000
	intvec	0x38
	scr_p	  0x4000
	scrlen	6144
	attr_p	scr_p + scrlen
	attrlen	6912 - 6144
	ram_p	  scr_p + 6912
	rammax	0xffff
  p_ramt  0x5cb4
  frames  0x5c78
  
end

class MyROM
	include Z80

	import MemoryMap, :code => false
	import Z80Lib, :labels => false, :code => false, :macros => true
  import Z80MathInt, :code => false, :labels => false, :macros => true

	macro :preserve do |eoc, *registers, &block|
		registers.each {|rr| push rr}
		block.call eoc
		registers.reverse.each {|rr| pop rr}
	end

	export	reset0	label
          di
          jp	 initrom

          org intvec
  preserve :inter, af, hl, bc, de do
          ld   hl, frames     # 10  29/49/57
          inc  [hl]           # 7
          jr   NZ, skip3      # 12/7
          inc  hl             # 6
          inc  [hl]           # 7
          jr   NZ, skip3      # 12/7
          inc  hl             # 6
          inc  [hl]           # 7
    skip3 label
          
          ex   af, af
          exx
    preserve af, hl, bc, de do
          call sound.iterate
    end
          exx
          ex   af, af
	end
          ei
          ret

	initrom	ld	 a, 7
          out	 (0xfe),a
          ld	 a, 0x3f         # load the accumulator with last page in rom.
          ld 	 i,a

	export	clrall
          clrmem(attr_p, attrlen, 0b01011110)
	clrall	label
          ld   hl, scr_p
          ld   de, scr_p + 1
          ld   a, 192/16
  scrloop ex    af, af
          ld   a, 0b10101010
          ld   [hl], a
          ld   bc, 256
          ldir
          ld   a, 0b01010101
          ld   [hl], a
          ld   bc, 255
          ldir
          inc  hl
          inc  de
          ex   af, af
          dec  a
          jr   NZ, scrloop
          ld   b, 12
  wloop0  djnz wloop0
  # memtest
          ld   bc, 0x007f
          ld   hl, rammax + 1
  noram   dec  hl
          ld   e, l
          ld   d, h
  tloop2  ld   a, 1       # 7
          ld   [hl], a    # 7
  tloop1  cp   [hl]       # 7   304
          jp   NZ, noram  # 10
          rla             # 4
          ld   [hl], a    # 7
          jp   NC, tloop1 # 10
          cp   [hl]       # 7
          jp   NZ, noram  # 10
          dec  hl         # 6
          
          ld   a, l       # 4
          xor  b          # 4
          anda 0xfe       # 7
          xor  l          # 4
          jp   M, nocolor # 10
          anda 0x07       # 7
          out (0xfe), a   # 11
          inc  b          # 4
          ld   a, 11      # 7
          cp   b          # 4
          sbc  a          # 4
          anda 12         # 7
          sub  b          # 4
          neg             # 8
          ld   b, a       # 4
  nocolor ld   a, h       # 4
          cp   c          # 4
          jp   NZ, tloop2 # 10
          
          # xor  a
          # out (0xfe), a
          ld   b, 0xff    # no color
          ld   a, 0x3f
          cp   c
          jp   NC, quittst
          ld   c, 0x3f
          jp   tloop2
  quittst ld   l, e
          ld   h, d
          inc  hl
          ld	 sp, hl
          ld   [p_ramt], de
          im1
          ei

          clrmem(scr_p, scrlen, 0)
  #  hl: input: sprite address
  #  a:  input: sprite height (1..192)
  #  a`: input: sprite width in bytes (1..32)
  #  bc: input: x - coordinate (-32768..32767) [screen area: 0-255]
  #  de: input: y - coordinate (-32768..32767) [screen area: 0-191]
  #  f:  input: flags -> f'
  #  ix: input: jump to ix
          scf
          sbc  a
          ld   a, 104
          ex   af, af
          ld   hl, 0x2848
          ld   de, fallout
          ld   bc, 0x0e00
          call gfx.draw_bitmap          
          ld   hl, 0x0509
          ld   de, fallout + 14*104
          ld   bc, 0x0d0e
          call gfx.attr_copy
          ld   b,  255
  waitsec ld   a,  7
          out  (0xfe), a
          halt
          ld   de, 442
  cwait   dec  de
          ld   a, d
          ora  e
          jr   NZ, cwait
          ld   a,  0
          out  (0xfe), a
          ld   de, 1775
  dwait   dec  de
          ld   a, d
          ora  e
          jr   NZ, dwait
          djnz waitsec
          call sound.setup

    class Sprite < Label
      x       word
      dx      word
      y       word
      dy      word
      w       byte
      h       byte
      bitmap  word
      scrlast word
      bmplast word
      wlast   byte
      hlast   byte
      skiplst byte
      scrlast2 word
      bmplast2 word
      wlast2   byte
      hlast2   byte
      skiplst2 byte
    end

    class Vars < Label
      splnum  byte
      ufo1    Sprite
      ufo2    Sprite
      ufo3    Sprite
      ufo4    Sprite
      counter byte
      flag    byte
    end
    
    class Patterns < Label
      ufo1    Sprite
      ufo2    Sprite
      ufo3    Sprite
      ufo4    Sprite
    end
    
    ufo_s addr 0, Sprite

		vars	  addr 0x8000, Vars
    splashscreens addr vars + +vars, 6912

    screens = %w[zx/me1.scr zx/me.scr zx/horse.scr zx/horse.scr]
    
    memcpy scr_p, splashscreens, +splashscreens
    xor	a
    ld	[vars.splnum], a

    ns do
        memcpy vars.ufo1, patterns.ufo1, +ufo_s
        memcpy vars.ufo2, patterns.ufo2, +ufo_s
        memcpy vars.ufo3, patterns.ufo3, +ufo_s
        memcpy vars.ufo4, patterns.ufo4, +ufo_s
        xor  a
        ld   [vars.counter], a
  ludlp label
    [vars.ufo1, vars.ufo2, vars.ufo3, vars.ufo4].each {|uu|
        ld   ix, uu
        call draw_sprite
        ld   ix, uu
        call restore_sprite
    }
    [vars.ufo1, vars.ufo2, vars.ufo3, vars.ufo4].each {|uu|
        ld   ix, uu
        call movesprite
    }
        hlt
        call swapscrn
        jp   NC, ludlp
    [vars.ufo1, vars.ufo2, vars.ufo3, vars.ufo4].each {|uu|
          ld   hl, uu
          ld   a, ufo_s.hlast
          adda_to h, l
          xor  a
          ld   [hl], a
    }
        jp   ludlp
      end
  ns :draw_sprite do # ix -> sprite
        ld   d, ixh
        ld   e, ixl
        ld   h, d
        ld   l, e
        ld   a, ufo_s.scrlast
        adda_to h, l
        ld   a, ufo_s.scrlast2
        adda_to d, e
        ld   bc, 7
        ldir                       # copy restore to restore2
        xor  a
        ld   [ix + ufo_s.hlast], a # clear restore if not drawn
        ld   c,  [ix + ufo_s.x]
        ld   b,  [ix + ufo_s.x + 1]
        ld   e,  [ix + ufo_s.y]
        ld   d,  [ix + ufo_s.y + 1]
        ld   l,  [ix + ufo_s.bitmap]
        ld   h,  [ix + ufo_s.bitmap + 1]
        ld   a, [ix + ufo_s.w]
        ex   af, af
        cp   a
        ld   a, [ix + ufo_s.h]
        push ix
        ld   ix, here1
        call gfx.calculate_coords_jump
        pop  ix
        ret
  here1 pop  af                    # return address we don't care about
        pop  ix
        ld   [ix + ufo_s.scrlast], l
        ld   [ix + ufo_s.scrlast + 1], h
        ld   [ix + ufo_s.bmplast], e
        ld   [ix + ufo_s.bmplast + 1], d
        ld   [ix + ufo_s.skiplst], c
        ld   [ix + ufo_s.wlast], b
        ex   af, af
        ld   [ix + ufo_s.hlast], a
        ex   af, af
        jp   gfx.draw_bitmap
  end
  ns :restore_sprite do
      ld   a, [ix + ufo_s.hlast2]
      ora  a
      ret  Z  # no restore
      ld   l, [ix + ufo_s.scrlast2]
      ld   h, [ix + ufo_s.scrlast2 + 1]
      ld   e, [ix + ufo_s.bmplast2]
      ld   d, [ix + ufo_s.bmplast2 + 1]
      ld   c, [ix + ufo_s.skiplst2]
      ld   b, [ix + ufo_s.wlast2]
      cp   a
      ex   af, af
      jp   gfx.draw_bitmap
  end
  ns :movesprite do # ix -> sprite
        ld   l,  [ix + ufo_s.x]
        ld   h,  [ix + ufo_s.x + 1]
        ld   e,  [ix + ufo_s.dx]
        ld   d,  [ix + ufo_s.dx + 1]
        add  hl, de
        # bit  7, d
        # jr   Z, xpos
        # dec  hl
        # dec  hl
  # xpos  inc  hl
        ld   [ix + ufo_s.x], l
        ld   [ix + ufo_s.x + 1], h
        ld   bc, 256/2
        ora  a
        sbc  hl, bc
        jp   PE, xless
        jp   M,  xless
        dec  de
        dec  de
  xless inc  de
        ld   [ix + ufo_s.dx], e
        ld   [ix + ufo_s.dx + 1], d
        ld   l,  [ix + ufo_s.y]
        ld   h,  [ix + ufo_s.y + 1]
        ld   e,  [ix + ufo_s.dy]
        ld   d,  [ix + ufo_s.dy + 1]
        add  hl, de
        # bit  7, d
        # jr   Z, ypos
        # dec  hl
        # dec  hl
  # ypos  inc  hl
        ld   [ix + ufo_s.y], l
        ld   [ix + ufo_s.y + 1], h
        ld   bc, 192/2
        ora  a
        sbc  hl, bc
        jp   PE, yless
        jp   M,  yless
        dec  de
        dec  de
  yless inc  de
        ld   [ix + ufo_s.dy], e
        ld   [ix + ufo_s.dy + 1], d
        ret
  end
  ns :swapscrn do
        ld   a, [vars.counter]
        dec  a
        ld   [vars.counter], a
        ora  a      # clear ccf
        ret  NZ
				ld	 hl, splashscreens
				ld	 de, +splashscreens
				ld	 a, [vars.splnum]
				ld	 b, a
				ora	 a
				jr	 Z, mlpover
		mlp add  hl, de
				djnz mlp
		mlpover	 inc	a
				cp	 screens.size
				jr	 C, mok
				xor	 a
		mok	ld   [vars.splnum], a
				di
				memcpy scr_p, hl, +splashscreens
				ei
  end
      scf
			ret

	forever	hlt
			jr	:forever

  import  :gfx, ZXGfxMore
  import  :sound, AYSound

  patterns data Patterns, [
    [-17, 0, -9, 0, 6, 40, ufo],
    [-13, 0, 211, 0, 6, 40, ufo],
    [293, 0, 211, 0, 6, 40, ufo],
    [13, 0, 179, 0, 6, 40, ufo]
  ]
  # ufo     import_file 'zx/ufo2.bin'
  ufo bytes [
  0x00,0x00,0x00,0x0C,0x00,0x00,
  0x00,0x00,0x00,0x0E,0x00,0x00,
  0x00,0x00,0x80,0x30,0x00,0x00,
  0x00,0x00,0x00,0x30,0x00,0x04,
  0x00,0x00,0x00,0x20,0x00,0x00,
  0x00,0x00,0x00,0x60,0x00,0x00,
  0x00,0x00,0x00,0x40,0x00,0x00,
  0x00,0x00,0x38,0x47,0x00,0x00,
  0x00,0x00,0xCF,0xF1,0x80,0x00,
  0x00,0x01,0x9C,0x1C,0x40,0x00,
  0x00,0x01,0x70,0x06,0x40,0x00,
  0x08,0x01,0x70,0x0F,0x60,0x00,
  0x00,0x00,0xC0,0x03,0x70,0x00,
  0x00,0x00,0x3A,0x27,0x9C,0x00,
  0x00,0x03,0xFF,0xFF,0xE7,0x00,
  0x00,0x1F,0x01,0xA4,0x78,0x00,
  0x00,0x3F,0x80,0x01,0xFE,0x00,
  0x00,0xFE,0x00,0x00,0x9F,0x00,
  0x4F,0xFF,0x80,0x03,0xFF,0x80,
  0xD9,0xFE,0x00,0x01,0x3F,0xC6,
  0x4E,0x3F,0xF8,0x07,0xFE,0x7B,
  0x73,0xE0,0x1F,0xFE,0x00,0x39,
  0x1B,0xBF,0xE0,0x00,0x07,0xE3,
  0x1D,0xC0,0xFF,0xFF,0xFF,0xCC,
  0x0C,0xC0,0x04,0x00,0xFF,0xC0,
  0x06,0x60,0x00,0x33,0xFF,0x80,
  0x02,0x38,0x00,0x3F,0xFF,0x00,
  0x01,0x9E,0x00,0x03,0xFE,0x00,
  0x00,0xE3,0xF0,0x1F,0xF8,0x00,
  0x00,0x79,0x1F,0xFF,0xC0,0x00,
  0x00,0x03,0x00,0x60,0x60,0x00,
  0x00,0x02,0x0F,0x40,0x20,0x00,
  0x00,0x04,0x00,0x40,0x30,0x00,
  0x00,0x9C,0x00,0x40,0x10,0x00,
  0x00,0xFE,0x00,0x40,0x18,0x00,
  0x01,0xF6,0x0F,0xFC,0x1E,0x00,
  0x03,0xFE,0x1B,0xBC,0x3F,0x00,
  0x00,0x1C,0x1F,0xF2,0x3F,0xC0,
  0x00,0x00,0x01,0xC4,0x1C,0xC0,
  0x00,0x00,0x00,0x1C,0x03,0x00]

  # ufo8    import_file 'zx/ufo28.bin'

  # 112x104+(14x13)attrs
  fallout import_file 'zx/fallout.bin'

	ludek bytes [
		0x00,0x18,
		0x18,0x3C,
		0x3C,0x7E,
		0x66,0xFF,
		0x42,0xFF,
		0x24,0x7E,
		0x18,0x3C,
		0x3C,0x7E,
		0x5A,0xFF,
		0x5A,0xFF,
		0x18,0x7E,
		0x3C,0x7E,
		0x24,0x7E,
		0x42,0xE7,
		0x42,0xE7,
		0x00,0x42]

	ludek8 bytes [
		0x00,
		0x18,
		0x3C,
		0x66,
		0x42,
		0x24,
		0x18,
		0x3C,
		0x5A,
		0x5A,
		0x18,
		0x3C,
		0x24,
		0x42,
		0x42,
		0x00]

	pointer bytes [
		0b00000000,
		0b01000000, #
		0b01000000,
		0b11100000, #
		0b01100000,
		0b11110000, #
		0b01110000,
		0b11111000, #
		0b01111000,
		0b11111100, #
		0b01111100,
		0b11111110, #
		0b00011000,
		0b00111100, #
		0b00011000,
		0b00111100, #
		0b00001100,
		0b00011110, #
		0b00000000,
		0b00000110] #

	# splashscreens label 6912
	# ns do
		# screens.each {|file|
			# import_file file, :bin
		# }
	# end

			org romtop - 0x300, 0xff
	# fontbin	import_file 'zxfont.bin', :bin, 0x300
	fontbin	import_file 'zx/andale.bin', :bin, 0x300
end
#puts MyROM.labels, nil,"D",MyROM.dummies.inspect, nil,"C",MyROM.contexts.inspect
$p = MyROM.new 0
puts "storage: #{$p[:storage]}"
puts "splashscreens: #{$p[:splashscreens]}"
#puts $p.debug[0..500]
#File.open('C:/Download/installs/zxspin/myrom.rom', 'wb') {|f| f.write $p.code }
File.open('D:/Installs/ZX Spectrum/zxspin0.666/myrom.rom', 'wb') {|f| f.write $p.code }
#$p.save_tap 'test1'
