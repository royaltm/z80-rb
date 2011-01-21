# -*- coding: BINARY -*-
require 'z80'
require 'zx/gfx.rb'

class MemoryMap
	include Z80

	export	:auto

	romtop	0x4000
	intvec	0x38
	scr_p	  0x4000
	scrlen	6144
	attr_p	scr_p + scrlen
	attrlen	6912 - 6144
	ram_p	scr_p + 6912
	ramtop	0xffff
end

class Z80Lib
	module Macros
    def memcpy(dest, source, size) # bc de hl
      ns do
				ld	de, dest if dest
				ld	hl, source if source
				ld	bc, size if size
				ldir
      end
		end
    def clrmem8(dest, size, value = 0) # a b hl
      ns do
            ld  b, size if size
            ld  hl, dest if dest
            if value == 0
              xor a
            elsif value
              ld  a, value
            end
      loop1 ld  [hl], a
            inc hl
            djnz loop1
      end
    end
    def clrmem(dest, size, value = 0) # a bc hl
      ns do
            ld  bc, size if size
            ld  hl, dest if dest
            if value == 0
              xor a
            elsif value
              ld  a, value
            end
            inc b
      loop1 ld  [hl], a
            inc hl
            dec c
            jp  NZ, loop1
            djnz loop1
      end
    end
  end
	include Z80
end
class ZXMath
  module Macros
    def multi8c_m(th, tl)  # multiply hl by a; stops on CARRY out
      ns do |eoc|
            ld  tl, l
            ld  th, h
            ld  hl,0
      loop1 srl a
            jr  NC, noadd
            add hl, th|tl
            jr  C, eoc
      noadd jr  Z, eoc
            sla tl
            rl  th
            jp  NC, loop1
      end
    end
    def multi16(mh, ml, rr)       # multiply hl by mh|ml (b|c or d|e) and stores result in hl rr (bc | de)
      ns do
            th = mh == b ? d : b
            tl = ml == c ? e : c
            ld  a, ml
            ex af,af
            xor a
            push af
            ld  tl, a
            ld  th, a
            ld  a, mh
            ld  ml, l
            ld  mh, h
            ld  h, th
            ld  l, tl
            scf
            adc a
      loop1 jr  NC, noadd1
            ex  (sp), hl
            add hl, th|tl
            ex  (sp), hl
            adc hl, mh|ml
      nadd1 srl mh
            rr  ml
            rr  th
            add a
            jp  NZ, loop1
            ex  af, af
            add a
      loop2 jr  NC, nadd2
            ex  (sp), hl
            add hl, th|tl
            ex  (sp), hl
            adc hl, mh|ml
      nadd2 srl ml
            rr  th
            rr  tl
            add a
            jp  NZ, loop2
            pop rr
      end
    end
  end
  include Z80
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
			3.times {rra}
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
		preserve :inter, af, hl do
			ld hl, [scr_p]
			inc hl
			ld [scr_p], hl
		end
			ei
			ret

	initrom	ld	b,a             # save the flag to control later branching.
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
      x     word
      dx    word
      y     word
      dy    word
      w     byte
      h     byte
    end

    class Vars < Label
      splnum  byte
      ludek_s Sprite
      counter byte
    end

    screens = %w[zx/me1.scr]
    memcpy scr_p, splashscreens, +splashscreens
    xor	a
    ld	[vars.splnum], a

    ns do
        clrmem8(vars.ludek_s, +vars.ludek_s - 1)
        ld   hl, -80
        ld   [vars.ludek_s.x], hl
        ld   hl, -7
        ld   [vars.ludek_s.y], hl
        ld   a, 64
        ld   [vars.ludek_s.h], a
        ld   a, 8
        ld   [vars.ludek_s.w], a
        xor  a
        ld   [vars.counter], a
  ludlp ld   bc, [vars.ludek_s.x]
        ld   de, [vars.ludek_s.y]
        ld   hl, ufo
        ld   a, [vars.ludek_s.w]
        ex   af, af
        cp   a
        scf
        ld   a, [vars.ludek_s.h]
        call gfx.draw_sprite8_coords
        hlt
        ld   bc, [vars.ludek_s.x]
        ld   de, [vars.ludek_s.y]
        ld   hl, ufo8
        ld   a, [vars.ludek_s.w]
        ex   af, af
        cp   a
        ld   a, [vars.ludek_s.h]
        #call gfx.draw_sprite8_coords
        ld   hl, [vars.ludek_s.x]
        ld   de, [vars.ludek_s.dx]
        bit  7, d
        jr   Z, xpos
        dec  hl
        dec  hl
  xpos  inc  hl
        ld   de, [vars.ludek_s.dx]
        ld   [vars.ludek_s.x], hl
        ld   bc, 256/2
        ora  a
        sbc  hl, bc
        jp   PE, xless
        jp   M,  xless
        dec  de
        dec  de
  xless inc  de
        ld   [vars.ludek_s.dx], de
        ld   hl, [vars.ludek_s.y]
        ld   de, [vars.ludek_s.dy]
        bit  7, d
        jr   Z, ypos
        dec  hl
        dec  hl
  ypos  inc  hl
        ld   de, [vars.ludek_s.dy]
        ld   [vars.ludek_s.y], hl
        ld   bc, 192/2
        ora  a
        sbc  hl, bc
        jp   PE, yless
        jp   M,  yless
        dec  de
        dec  de
  yless inc  de
        ld   [vars.ludek_s.dy], de
        #call swapscrn
        jp   ludlp
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
  
		vars	addr 0x8000, Vars


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

	import	:gfx, ZXGfx8

  ufo   import_file 'zx/ufo.bin'
  ufo8  import_file 'zx/ufo8.bin'

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

	splashscreens label 6912
	ns do
		screens.each {|file|
			import_file file, :bin
		}
	end

			org romtop - 0x300, 0xff
	# fontbin	import_file 'zxfont.bin', :bin, 0x300
	fontbin	import_file 'zx/andale.bin', :bin, 0x300
end
#puts MyROM.labels, nil,"D",MyROM.dummies.inspect, nil,"C",MyROM.contexts.inspect
$p = MyROM.new 0
#puts $p.debug[0..500]
File.open('C:/Download/installs/zxspin/myrom.rom', 'wb') {|f| f.write $p.code }
#File.open('D:/Installs/ZX Spectrum/zxspin0.666/myrom.rom', 'wb') {|f| f.write $p.code }
#$p.save_tap 'test1'
