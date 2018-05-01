##
#  ==A module with Z80 macros for common ZX Spectrum graphics tasks
#
#  Example:
#
#    require 'zxlib/gfx'
#
#    class Program
#      include Z80
#      macro_import  ZXGfx
#
#              # invert pixel at 100, 100
#              ld  l, 100
#              ld  a, 100
#              xytoscr(a, l, h, l, b, c)
#              inc b
#              ld  a, 1
#      shift1  rrca
#              djnz shift1
#              xor  [hl]
#              ld   [hl], a
#              ret
#    end
class ZXGfx
  module Macros
	##
	#  Calculate constant screen address from x, y pixel position
	def xy_to_pixel_addr(x, y)
		0x4000 + (
			(
				(
					((y & 0x07) << 3) | ((y & 0b111000) >> 3) | ((y & 0xffc0))
				) << 5
			) | ((x & 0xff) >> 3)
		)
	end
	##
	# Advances to the next screen line byte address (down) using ah|al registers.
	# (optionally) returns from subroutine if address goes out of screen area.
	#
	# Modifies: +af+, +ah+, +al+
	#
	# * +ah+:: input/output register: address high byte
	# * +al+:: input/output register: address low byte
	# * +bcheck+:: boundary check flag:
	#              +false+ = disable checking,
	#              +true+  = issue +ret+ if out of screen (default)
	#              +label+ = jump to label if out of screen,
	#              +hl+|+ix+|+iy+ = jump to address in a register if out of screen
	#
	# if block is given and +bcheck+ == +true+ evaluates namespaced block instead of +ret+.
	#
	# T-states: 
	#
	# * when +bcheck+ is +false+:: 27:87.5% / 49:1.56% / 59:10.94%
	# * when +bcheck+ is +true+ or +label+::  27:87.5% / (70:2 / 75:1):1.56% / 62:10.94% 
	def nextline(ah, al, bcheck = true, **nsopts, &block)
		if ah == al or [ah, al].include?(a) or
				(register?(bcheck) and ![hl_, ix_, iy_].include?(bcheck)) or
				(bcheck == hl and ([h, l].include?(ah) or [h, l].include?(al))) or
				(bcheck == ix and ([ixh, ixl].include?(ah) or [ixh, ixl].include?(al))) or
				(bcheck == iy and ([iyh, iyl].include?(ah) or [iyh, iyl].include?(al))) or
				![ah, al].all?{|r| register?(r) }
			raise ArgumentError, "nextline invalid arguments!"
		end
		ns do |eoc|
				inc  ah
				ld   a, ah
				anda 0x07
				jr   NZ,eoc
				ld   a, al
				add  0x20
				ld   al, a
			if bcheck
				jp   NC, restrh
				ld   a, ah
				cp   0x58
				jr   C, eoc
				case bcheck
				when true
					if block_given?
						ns(**nsopts, &block)
					else
						ret
					end
				else
					jp  bcheck
				end
			else
				jr   C, eoc
			end
		restrh  ld   a, ah
				sub  0x08
				ld   ah, a
		end
	end
	##
	# Converts x,y coordinates to screen byte address and bits shift
	#
	# Modifies: +af+, +y+, +x+, +s+, +t+
	#
	# * +y+:: input register: vertical-coordinate (may be a or same as: +h+, +l+, +s+, +t+)
	# * +x+:: input register: horizontal-coordinate (may be same as: +l+)
	# * +h+:: output register: address high
	# * +l+:: output register: address low
	# * +s+:: output register: bits shift
	# * +t+:: temporary register
	#
	# T-states: 101
	#
	# y < a1 a2 h3 h2 h1 l3 l2 l1,  x < x5 x4 x3 x2 x1 s3 s2 s1,
	# h > 0  1  0  a1 a2 l3 l2 l1,  l > h3 h2 h1 x5 x4 x3 x2 x1,  s > 0  0  0  0  0  s3 s2 s1
	def xytoscr(y, x, h, l, s, t)
		if y == x or [x,h,l,s,t].include?(a) or [h,l,s,t].uniq.size != 4 or
				![y, x, h, l, s, t].all?{|r| r.is_a?(Register)}
			raise ArgumentError, "xytoscr invalid arguments!"
		end
		isolate do
			if y == a
			  ld   h, a
			else
			  ld   a, y
			end            # a= a a h h h l l l
			anda 0b00000111
			ld   s, a      # s= 0 0 0 0 0 l l l
			if y == a
			  xor  h
			else
			  xor  y       # a= a a h h h 0 0 0
			end
			rrca           # a= 0 a a h h h 0 0
			scf
			rra            # a= 1 0 a a h h h 0
			rrca
			ld   h, a      # h= 0 1 0 a a h h h
			anda 0b00000111
			ld   t, a      # t= 0 0 0 0 0 h h h
			xor  h         # a= 0 1 0 a a 0 0 0
			ora  s
			ld   h, a      # h= 0 1 0 a a l l l
			ld   a, x      # a= x x x x x s s s
			anda 0b00000111
			ld   s, a      # s= 0 0 0 0 0 s s s
			xor  x         # a= x x x x x 0 0 0
			ora  t         # a= x x x x x h h h
			3.times { rrca }
			ld   l, a      # l= h h h x x x x x
		end
	end
	##
	# Converts 0,y coordinates to screen byte address
	#
	# Modifies: +af+, +y+, +h+, +l+, +t+
	#
	# * +y+:: input register: vertical-coordinate (may be same as: +h+, +l+ or +a+)
	# * +h+:: output register: address high
	# * +l+:: output register: address low
	# * +t+:: temporary register
	# * +col+:: optional column (0-31) as a register (must not be same as other arguments)
	#
	# T-states: 73
	#
	# y < a1 a2 h3 h2 h1 l3 l2 l1,
	# h > 0  1  0  a1 a2 l3 l2 l1,  l > h3 h2 h1 0  0  0  0  0
	def ytoscr(y, h, l, t, col=nil)
		if [h,l,t].include?(a) or [h,l,t].uniq.size != 3 or t == y or
				![y, h, l, t].all?{|r| r.is_a?(Register) } or
				(col.is_a?(Register) and [y, h, l, t, a].include?(col)) or
				(!col.nil? and !col.is_a?(Register))
			raise ArgumentError, "ytoscr invalid arguments!"
		end
		isolate do
			if y == a
			  ld   l, a
			else
			  ld   a, y
			end             # a= a a h h h l l l
			anda 0b00000111
			ld   t, a       # h= 0 0 0 0 0 l l l
			if y == a
				xor  l
			else
				xor  y      # a= a a h h h 0 0 0
			end
			rlca            # a= a h h h 0 0 0 a
			rlca            # a= h h h 0 0 0 a a
			ld   h, a       # b= h h h 0 0 0 a a
			anda 0b11100000
			add  col if col
			ld   l, a       # l= h h h c c c c c
			sub  col if col # l= h h h 0 0 0 0 0
			xor  h          # a= 0 0 0 0 0 0 a a
			3.times { rlca }
			ora  t          # a= 0 0 0 a a l l l
			ora  0x40       # a= 0 1 0 a a l l l
			ld   h, a       # h= 0 1 0 a a l l l
		end
	end
	##
	# Converts row,col text coordinates to screen byte address
	#
	# Modifies: +af+, +r+, +c+, +h+, +l+
	#
	# * +r+:: input register: text row (may be same as: +l+)
	# * +c+:: input register or a integer: text column  (may be same as: +l+)
	# * +h+:: output register: address high
	# * +l+:: output register: address low (may be same as: +c+, +r+)
	# * +r_already_in_a+:: when +true+ there's no need to load +r+ to +a+
	#
	# T-states:
	#
	# * 47: +r_already_in_a+ == false, +c+ is a register or integer and <> 0
	# * 43: +r_already_in_a+ == true, +c+ is a register or integer and <> 0
	# * 43: +r_already_in_a+ == false, +c+ is a integer and equals 0
	# * 39: +r_already_in_a+ == true, +c+ is a integer and equals 0
	#
	# r < 0  0  0  5r 4r 3r 2r 1r,  c < 0  0  0  5c 4c 3c 2c 1c,
	# h > 0  1  0  5r 4r 0  0  0,   l < 3r 2r 1r 5c 4c 3c 2c 1c
	def rctoscr(r, c, h, l, r_already_in_a = false)
	  if r == h or [r, c, h, l].include?(a) or [r, c, h].uniq.size != 3 or h == l or
			![r, h, l].all?{|r| r.is_a?(Register)}
		raise ArgumentError, "rctoscr invalid arguments!"
	  end
	  isolate do
			ld   a, r unless r_already_in_a
			anda 0b00011000
			ld   h, a       # h= 0  0  0  a  a  0  0  0
			xor  r          # a= 0  0  0  0  0  r  r  r
			3.times {rrca}  # a= r  r  r  0  0  0  0  0
			ora  c unless c == 0
			ld   l, a       # l= r  r  r  c  c  c  c  c
			set  6, h       # h= 0  1  0  a  a  0  0  0
	  end
	end
	##
	# Converts row, col text coordinates to attribute address
	#
	# Modifies: +af+, +r+, +c+, +h+, +l+
	#
	# * +r+:: input register: text row (may be same as: +l+)
	# * +c+:: input register or a integer: text column  (may be same as: +l+)
	# * +h+:: output register: address high
	# * +l+:: output register: address low (may be same as: +c+, +r+)
	# * +r_already_in_a+:: when +true+ there's no need to load +r+ to +a+
	#
	# T-states: 57
	#
	# r < 0  0  0  5r 4r 3r 2r 1r,  c < 0  0  0  5c 4c 3c 2c 1c,
	# h > 0  1  0  1  1  0  5r 4r,  l < 3r 2r 1r 5c 4c 3c 2c 1c
	def rctoattr(r, c, h, l, r_already_in_a = false)
	  if r == h or [r, c, h, l].include?(a) or [r, c, h].uniq.size != 3 or h == l or
			![r, h, l].all?{|r| r.is_a?(Register)}
		raise ArgumentError, "rctoattr invalid arguments!"
	  end
	  isolate do
			ld   a, r unless r_already_in_a
			3.times {rrca}
			ld   r, a       # r= r r r 0 0 0 a a
			anda 0b00000011
			ora  0b01011000
			ld   h, a       # h= 0 1 0 1 1 0 a a
			xor  0b01011000 # a= 0 0 0 0 0 0 a a
			xor  r          # a= r r r 0 0 0 0 0
			ora  c unless c == 0
			ld   l, a       # l= r r r c c c c c
	  end
	end
	##
	# Converts hi byte screen address to attribute address
	#
	# Modifies: +af+, +o+
	#
	# * +i+: input register: hi byte screen address
	# * +o+: output register: hi byte attr address, may be same as +i+
	#
	# T-states:
	#
	# * 34: when neither +i+ nor +o+ is a register +a+
	# * 30: when +i+ is a register +a+
	# * 30: when +o+ is a register +a+
	# * 26: when each +i+ and +o+ is a register +a+
	#
	# i < 0  1  0  2a 1a l  l  l,
	# o > 0  1  0  1  1  0  2a 1a
	def scrtoattr(i, o=i)
		raise ArgumentError, "scrtoattr invalid arguments!" unless [i, o].all?{|r| r.is_a?(Register)}
	  isolate do
			ld	 a, i unless i == a
			3.times {rrca}  # h= l l l 0 1 ? a a
			anda 0b00001111 # h= 0 0 0 0 1 ? a a
			ora	 0b01010000 # h= 0 1 0 1 1 ? a a
			ld	 o, a unless o == a
	  end
	end
  end
  include Z80
end
