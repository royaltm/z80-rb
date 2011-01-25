# -*- coding: BINARY -*-
require 'z80'
require 'zx/gfx_sprite.rb'
require 'zx/gfx+.rb'
require 'zx/sound.rb'
require 'z80/stdlib.rb'
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
	ramtop	0xffff
end

class Liba
	include Z80

	import :mm, MemoryMap, :code => false

	export	fntdemo
		ns :fntdemo do
			ld	hl, mm.scr_p
	start	push ix
			pop	de
			ld	c, a
	loop2	ld	b, 8
	loop1	ld	a, [de]
			inc	de
			ld	[hl], a
			inc h
			djnz :loop1

			ld	b, h			# set attribute
			ld	a, h
			3.times {rrca}
			ora	0x58
			ld	h, a
			dec	h
			ld	a, h
			add	d
			xor l
			rrca
			xor	c
			anda 0x7f
			ld	[hl], a
			4.times {rlca}
			anda 0x17
			neg
			out	(0xfe), a
			ld	h, b

			inc	l
			jr	Z, :skip1
			ld	a, h
			sub	0x08
			ld	h, a
	skip1	dec c
			jp  NZ, :loop2
			ret
		end
end

class MyROM
	include Z80
	include TAP

	import	MemoryMap, :code => false
	import	Z80Lib, :labels => false, :code => false, :macros => true

	macro :fill do |_, addr, len, byte|
			ld hl, addr
			ld d, byte
			ld bc, len
			call :fill_subr
	end

	macro :preserve do |eoc, *registers, &block|
		registers.each {|rr| push rr}
		block.call eoc
		registers.reverse.each {|rr| pop rr}
	end

	export	reset0	label
			di
			xor a
			ld de, -1
			jp		initrom

			org intvec
		preserve :inter, af, hl, bc, de do
			ld hl, [scr_p]
			inc hl
			ld [scr_p], hl
      ex af, af
      exx
      preserve af, hl, bc, de do
        call sound.iterate
      end
      exx
      ex af, af
		end
			ei
			ret

	initrom	ld	b,a         # save the flag to control later branching.
			ld	a,7             # select a white border
			out	(0xfe),a        # and set it now by writing to a port.
			ld	a,0x3f          # load the accumulator with last page in rom.
			ld 	i,a
			ld	sp, ramtop + 1

	export	clrall
	clrall	label
			fill scr_p,  scrlen,  0b01010101
			fill attr_p, attrlen, 0b00000111
			fill ram_p, ramtop + 1 - ram_p - 2, 0

      call sound.setup
			im1
			ei
      
		ns(:fntdemo8) {
			ld	ix, fontbin
			ld	b, 0x300 / 8
	loopbg	push bc
			ld	a, b
			call liba.fntdemo
			exx
			ld	b, 7
	loop1	xor	a
			out (0xfe), a
			# hlt

			push bc
			ld	bc, 470
	loopwt	nop
			dec bc
			ld	a, c
			ora	b
			jr	NZ, :loopwt
			pop	bc

			exx
			pop	af
			push af
			call liba.fntdemo.start
			exx
			djnz :loop1
			ld	bc, 8
			add	ix, bc
			pop	bc
			djnz loopbg
			xor	a
			out	(0xfe),a
		}

    class Sprite < Label
      x      word
      dx     word
      y      word
      dy     word
      w      byte
      h      byte
      buffer word
      bitmap word
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

    storage_size = 7*40 + 4
		vars	  addr 0x8000, Vars
    storage addr vars + +vars, storage_size*4
    splashscreens addr storage + +storage, 6912

    screens = %w[zx/me1.scr zx/me.scr zx/horse.scr zx/horse.scr]
    
    memcpy scr_p, splashscreens, +splashscreens
    xor	a
    ld	[vars.splnum], a

    macro :restore_sprite do |eoc, address|
        ld   hl, address
        ld   a, [hl]
        ora  a                    # check storage
        jp   Z, eoc
        call gfx.restore
    end
    ns do
        clrmem(storage, +storage)
        memcpy vars.ufo1, patterns.ufo1, +ufo_s
        memcpy vars.ufo2, patterns.ufo2, +ufo_s
        memcpy vars.ufo3, patterns.ufo3, +ufo_s
        memcpy vars.ufo4, patterns.ufo4, +ufo_s
        xor  a
        ld   [vars.counter], a
  ludlp label
        ld  ix, vars.ufo1
        call draw_sprite
        ld  ix, vars.ufo2
        call draw_sprite
        ld  ix, vars.ufo3
        call draw_sprite
        ld  ix, vars.ufo4
        call draw_sprite
        ld  ix, vars.ufo1
        call movesprite
        ld  ix, vars.ufo2
        call movesprite
        ld  ix, vars.ufo3
        call movesprite
        ld  ix, vars.ufo4
        call movesprite
        hlt
        restore_sprite [vars.ufo4.buffer]
        restore_sprite [vars.ufo3.buffer]
        restore_sprite [vars.ufo2.buffer]
        restore_sprite [vars.ufo1.buffer]
        call swapscrn
        jp   ludlp
      end
  ns :draw_sprite do # ix -> sprite
        ld   e, [ix + ufo_s.buffer]
        ld   d, [ix + ufo_s.buffer + 1]
        xor  a
        ld   [de], a              # clear storage
        exx
        ld   c,  [ix + ufo_s.x]
        ld   b,  [ix + ufo_s.x + 1]
        ld   e,  [ix + ufo_s.y]
        ld   d,  [ix + ufo_s.y + 1]
        ld   l,  [ix + ufo_s.bitmap]
        ld   h,  [ix + ufo_s.bitmap + 1]
        ld   a, [ix + ufo_s.w]
        ex   af, af
        cp   a
        scf
        ld   a, [ix + ufo_s.h]
        ld   ix, here1
        jp   h1skp
  here1 push bc
        push de
        push hl
        ex   af, af
        push af
        ex   af, af
        call   gfx.scrcopy
        pop  af
        ex   af, af
        pop  hl
        pop  de
        pop  bc
        jp gfxs.draw_sprite8
  h1skp jp gfxs.calculate_coords_jump8
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
				memcpy scr_p, nil, +splashscreens
				ei
  end
			ret

	forever	hlt
			jr	:forever
		
	ns :fill_subr do
	loop1	ld [hl], d
			inc hl
			dec bc
			ld 	a, c
			ora b
			jr  NZ, :loop1
			ret
  end

	import	:liba, Liba

	import	:gfxs, ZXGfxSprite8
  import  :gfx, ZXGfxMore
  import  :sound, AYSound

  patterns data Patterns, [
    [-17, 0, -9, 0, 6, 40, storage, ufo],
    [-13, 0, 211, 0, 6, 40, storage + storage_size*1, ufo],
    [293, 0, 211, 0, 6, 40, storage + storage_size*2, ufo],
    [13, 0, 179, 0, 6, 40, storage + storage_size*3, ufo]
  ]
  ufo   import_file 'zx/ufo2.bin'
  ufo8  import_file 'zx/ufo28.bin'

	ludek data 1, [
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

	ludek8 data 1, [
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

	pointer data 1, [
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
File.open('C:/Download/installs/zxspin/myrom.rom', 'wb') {|f| f.write $p.code }
#File.open('D:/Installs/ZX Spectrum/zxspin0.666/myrom.rom', 'wb') {|f| f.write $p.code }
#$p.save_tap 'test1'
