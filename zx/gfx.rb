class ZXGfx8
	module Macros
		#  advances to next screen line byte address using ah al registers
		#  (optionally) returns from subroutine if address goes out of screen area
		#  uses: a, ah, al
		#  ah: register  input/output: address high byte
		#  al: register  input/output: address low byte
		#  bcheck: boundary check flag
		#    false = disable checking
		#    true  = return if out of screen (default)
		#    label = jump to label if out of screen
		def nextline(ah, al, bcheck = true)
			ns do |eoc|
				inc 	ah
				ld		a, ah
				anda	0x07
				jr		NZ,eoc
				ld		a, al
				add 	0x20
				ld		al, a
				jr		C, bcheck ? over : eoc
				ld		a, ah
				sub		0x08
				ld  	ah, a
				if bcheck
					jp		eoc
			over	ld		a, ah
					cp		0x58
					if bcheck == true
						ret		NC
					else
						jp	NC, bcheck
					end
				end
			end
		end

		#  converts x,y coordinates to screen byte address and bits shift
		#  uses: a, y, x, s, t
		#  y: register  input: vertical-coordinate    output: address high
		#  x: register  input: horizontal-coordinate  output: address low
		#  s: register                                output: bits shift
		#  t: register  temporary
		#
		#  y< a1 a2 h3 h2 h1 l3 l2 l1  x< x5 x4 x3 x2 x1 s3 s2 s1
		#  y> 0  1  0  a1 a2 l3 l2 l1  x> h3 h2 h1 x5 x4 x3 x2 x1  s> 0  0  0  0  0  s3 s2 s1
		def xytoscr(y, x, s, t)
			ns do
				ld	a, y    # a= a a h h h l l l
				anda 0x07
				ld	s, a	# s= 0 0 0 0 0 l l l
				xor	y       # a= a a h h h 0 0 0
				rrca        # a= 0 a a h h h 0 0
				scf
				rra         # a= 1 0 a a h h h 0
				rrca
				ld	y, a    # y= 0 1 0 a a h h h
				anda 0x07
				ld  t, a    # t= 0 0 0 0 0 h h h
				xor	y       # a= 0 1 0 a a 0 0 0
				ora	s
				ld	y, a    # y= 0 1 0 a a l l l
				ld	a, x    # a= x x x x x s s s
				anda 0x07
				ld	s, a    # s= 0 0 0 0 0 s s s
				xor	x       # a= x x x x x 0 0 0
				ora t       # a= x x x x x h h h
				3.times { rrca }
				ld	x, a    # x= h h h x x x x x
			end
		end
	end
	include Z80
####################### 16xor
			# ld	a, [de]
			# inc de
			# n.times { rrca }
			# ld	c, a
			# anda 0xf0 #((0xff00 >>n) & 0xhh)
			# ld	b, a
			# xor c
			# xor	[hl]
			# ld	[hl], a
			# inc l
			# ld	a, l
			# anda 0x1f
			# jr	Z, nsline1
			# ld	a, [de]
			# inc	de
			# n.times { rrca }
			# ld	c, a
			# anda  0xf0 #((0xff <<(8-n)) & 0xhh)
			# ld	ixh, a
			# xor	c
			# ora b
			# xor	[hl]
			# ld	[hl], a
			# inc l
			# ld	a, l
			# anda 0x1f
			# jr	Z, nsline2
			# ld	a, ixh
			# xor [hl]
			# ld	[hl], a
	# nsline2	dec l
	# nsline1	dec l
	# back0	nextline h, l
###############	
	#  this macro is internally used by draw_sprite8
	#  n: bit shift offset
	#  back: jump -> back (optional)
	def self.shiftandor8(n, back = nil)
		ns do
			exx
			ld	a, [de] # bitmap
			inc de
			if n <= 4
				n.times { rrca }
			else
				(8-n).times { rlca }
			end
			ld	c, a
			if n != 0
				anda((0xff00 >>n) & 0xff)
				ld	ixh, a # right bitmap
				xor c
				ld	c, a   # left bitmap
			end
			ld	a, [de] # mask
			inc	de
			if n <= 4
				n.times { rrca }
			else
				(8-n).times { rlca }
			end
			if n != 0
				ld	b, a
				anda((0xff00 >>n) & 0xff)
				ld	ixl, a # right mask
				xor	b
			end
			cpl
			anda [hl]  # left mask
			ora	c      # left bitmap
			ld	[hl], a
			jp	back if back
		end
	end
	def self.shiftandor8neg(n, back)
		ns do
			exx
			ld	a, [de] # bitmap
			inc de
			if n <= 4
				n.times { rrca }
			else
				(8-n).times { rlca }
			end
			anda((0xff00 >>n) & 0xff)
			ld	c, a
			ld	a, [de] # mask
			inc	de
			if n <= 4
				n.times { rrca }
			else
				(8-n).times { rlca }
			end
			anda((0xff00 >>n) & 0xff)
			cpl
			anda [hl]  # left mask
			ora	c      # left bitmap
			ld	[hl], a
			jp	back if back
		end
	end

	#  this macro is internally used by draw_sprite8
	#  n: bit shift offset
	#  back: jump -> back (optional)
	def self.shiftxor8(n, back = nil)
		ns do
			exx
			ld	a, [de]
			inc	de
			inc de
			if n <= 4
				n.times { rrca }
			else
				(8-n).times { rlca }
			end
			if n != 0
				ld	c, a
				anda((0xff00 >> n) & 0xff)
				ld	b, a
				xor c
			end
			xor	[hl]
			ld	[hl], a
			jp	back if back
		end
	end
	def self.shiftxor8neg(n, back)
		ns do
			exx
			ld	a, [de]
			inc	de
			inc de
			if n <= 4
				n.times { rrca }
			else
				(8-n).times { rlca }
			end
			anda((0xff00 >> n) & 0xff)
			xor [hl]
			ld	[hl], a
			jp	back if back
		end
	end

	#  uses: a b c d e h l ix a' b' c' d' e' h' l' (1 stack)
	#  hl:  sprite address
	#  a':  input: sprite height (1..192)
	#  CF': input: 1: xor mode, 0: and+or mode
	#  bc:  x - coordinate (-32768..32767)
	#  de:  y - coordinate (-32768..32767)
	#  CF: input: 1: xor mode, 0: and+or mode
	export	draw_sprite8_coords
		ns :draw_sprite8_coords do
			ex	af, af		# store CF and sprite height
			ld	a, d
			ora	a
			jp	Z, vnext1	# 0 <= de < 256
			inc	a			# d == 0xff
			ret	NZ			# de < -256
			ex	af, af		# alter sprite size
			push af			# save CF
			neg				# -size
			sub	e			# -size - de ;e: e - 256 (-256..-1)
			jp	C, vnext2 	# size < -de
			pop	af			# size to small
			ret
	vnext2	neg				# -(-size - de)
			anda a			# CF=0
			sbc hl, de		# hl-= de increases sprite pointer
			ccf
			sbc hl, de		# hl-= de increases sprite pointer x2
			ld	d, a		# size -> d
			pop af			# restore CF
			ld	a, d		# size ->
			ex	af, af		# save size & CF
			jp	hnext1
	vnext1	ld	a, e		# 0 <= de < 192
			cp	192
			ret	NC			# de >= 192
	hnext1	ld	a, b
			ora	a			# 0<= bc < 256
			jp	Z, hnext2	# bc on screen
			inc	a
			ret	NZ			# bc < -256
			ld	a, c
			cp	0xf9
			ret C			# bc < -7
			ld	c, e
			ld	e, a
	hnext2	ex	de, hl		# sprite -> de
			ld	h, l		# h = y & 0xff
			ld	l, c        # l = x & 0xff
	end
	#  callback draws on screen using xor 8-bit wide sprite with arbitrary height
	#  uses: a b c d e h l ix a' b' c' d' e' h' l' (no stack) (undocumented: ixh ixl)
	#  de: sprite address
	#  h:  input: vertical coordinate   (0..191)
	#  l:  input: horizontal coordinate (0..255)
	#    except when h > 191 then l contains vertical coordinate and h | 0xf8 is a negative h-coordinate (from -1 to -7)
	#  a':  input: sprite height (1..192)
	#  CF': input: 1: xor mode, 0: and+or mode
	#    in and+or mode sprite bitamp bytes must be intertwined with mask: b1 m1 b2 m2 b2 m2 .....
	#    in xor mode mask bytes are skipped
	export	draw_sprite8
		ns :draw_sprite8 do
			ld	a, h
			cp	192
			jp	C, skipneg
			anda 0x07
			ret	Z		# sanity check
			ld	c, a	# negshift
			ld	a, l    # a= a a h h h l l l
			anda 0x07
			ld	h, a	# h= 0 0 0 0 0 l l l
			xor	l       # a= a a h h h 0 0 0
			rlca        # a= a h h h 0 0 0 a
			rlca        # a= h h h 0 0 0 a a
			ld	b, a    # b= h h h 0 0 0 a a
			anda 0xE0
			ld	l, a	# l= h h h 0 0 0 0 0
			xor b		# a= 0 0 0 0 0 0 a a
			3.times { rlca }
			ora	h       # a= 0 0 0 a a l l l
			ora	0x40    # a= 0 1 0 a a l l l
			ld	h, a    # h= 0 1 0 a a l l l
			ld	a, c
			exx
			ld	hl, jumpneg - 4
			jp	skiplus
	skipneg	xytoscr h, l, c, b # hl -> screen, c -> shift
			ld	a, c
			exx
			ld	hl, jumptbl	# jump table
	skiplus	add a
			add a			# a*= 4
			add	l			# hl+= a
			ld	l, a
			jp	NC, noh
			inc	h
	noh		ex	af, af
			ld	b, a		# size -> b
			jr	C, xorskip	# CF ?
			inc	hl			# hl+= 2
			inc hl
	xorskip	ld	e, [hl]		# jump vector
			inc	hl
			ld	d, [hl]
			ex	de, hl		# vector -> hl
	shftlp	jp	(hl)
	shiftx4	shiftxor8 4		# the longest one (shift*4) saves one jp less
	backx1	inc l
			ld	a, l
			anda 0x1f
			jr	Z, nslinx1
			ld	a, b
			xor [hl]
			ld	[hl], a
	nslinx1	dec l
	backx0	nextline h, l
			exx
			djnz shftlp
			ret
	shifta4	shiftandor8 4	# the longest one (shift*4) saves one jp less
	backa1	inc l
			ld	a, l
			anda 0x1f
			jr	Z, nslina1
			ld	a, ixl		# right mask
			cpl
			anda [hl]
			ora	ixh   		# right bitmap
			ld	[hl], a
	nslina1	dec l
	backa0	nextline h, l
			exx
			djnz shftlp
			ret

	shiftx0	shiftxor8 0, backx0
	nshftx7 shiftxor8neg 7, backx0
	shiftx1	shiftxor8 1, backx1
	nshftx6 shiftxor8neg 6, backx0
	shiftx2	shiftxor8 2, backx1
	nshftx5 shiftxor8neg 5, backx0
	shiftx3	shiftxor8 3, backx1
	nshftx4 shiftxor8neg 4, backx0
	shiftx5	shiftxor8 5, backx1
	nshftx3 shiftxor8neg 3, backx0
	shiftx6	shiftxor8 6, backx1
	nshftx2 shiftxor8neg 2, backx0
	shiftx7	shiftxor8 7, backx1
	nshftx1 shiftxor8neg 1, backx0
	shifta0	shiftandor8 0, backa0
	nshfta7 shiftandor8neg 7, backa0
	shifta1	shiftandor8 1, backa1
	nshfta6 shiftandor8neg 6, backa0
	shifta2	shiftandor8 2, backa1
	nshfta5 shiftandor8neg 5, backa0
	shifta3	shiftandor8 3, backa1
	nshfta4 shiftandor8neg 4, backa0
	shifta5	shiftandor8 5, backa1
	nshfta3 shiftandor8neg 3, backa0
	shifta6	shiftandor8 6, backa1
	nshfta2 shiftandor8neg 2, backa0
	shifta7	shiftandor8 7, backa1
	nshfta1 shiftandor8neg 1, backa0
	jumpneg	words [
		nshftx1, nshfta1, nshftx2, nshfta2, nshftx3, nshfta3, nshftx4, 
		nshfta4, nshftx5, nshfta5, nshftx6, nshfta6, nshftx7, nshfta7]
	jumptbl	words [
		shiftx0, shifta0, shiftx1, shifta1, shiftx2, shifta2, shiftx3, shifta3, 
		shiftx4, shifta4, shiftx5, shifta5, shiftx6, shifta6, shiftx7, shifta7]
	end
end
