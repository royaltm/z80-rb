# -*- coding: BINARY -*-
require 'z80'
# See:
# * http://www.worldofspectrum.org/ZXBasicManual/zxmanchap24.html
# * http://dac.escet.urjc.es/~csanchez/pfcs/zxspectrum/CompleteSpectrumROMDisassemblyThe.pdf
# * http://wos.meulie.net/pub/sinclair/games-info/z/Z80Toolkit2.pdf
class Float
	##
	# Converts a Ruby +Float+ to ZX-Spectrum's encoded real number as a 5-byte binary string.
	# Suitable to be used with ZXMath::ZXReal struct for data.
	#
	# The only argument: +simplified_int+ indicates if the integers in the
	# range of -65535..65535 should be stored in a simplified integer form.
	def to_z80bin(simplified_int = true)
		sgn = self < 0
		if simplified_int && self == self.truncate && -65535 <= self && self <= 65535
			[0,
			 sgn ? 0xff : 0,
			 self,
			 0 ].pack('CCvC')
		else
			m = sgn ? -self : self
			e = (Math.log2(m)+1.0).floor
			raise "overflow error" if e > 127
			if e < -127
				[0].pack('C')*5
			else
				# m = m*(2**-e)
				if e < 0
					m = m*(1<< (-e))
				else
					m = m/(1<<e)
				end
				[
					e + 128,
					sgn ? m*(1<<32).truncate : (m*(1<<32)).truncate ^ (1<<31)
				].pack('CN')
			end
		end
	end
end
##
# ==A module with +ZXReal+ struct definition and ZX-Spectrum FP helpers.
#
# ===Example:
#
#    require('zxlib/math')
#
#    class TestMath
#      include Z80
#      include Z80::TAP
#  
#      ZXReal = ZXMath::ZXReal
#  
#      chan_open addr 0x1601
#
#      export :auto
#      start       ld a, 2
#                  call chan_open
#                  ld hl, pi
#                  call math.print_fp_hl
#                  ret
#
#      pi          data ZXReal, Math::PI
#
#      import :math, ZXMath
#    end
class ZXMath
	include Z80

	stk_store addr 0x2ab6
	stack_bc  addr 0x2d2b
	print_fp  addr 0x2de3 
	fp_calc   addr 0x28

	##
	# A struct representing a ZX-Spectrum's FP calculator's real number data type.
	class ZXReal < Label
		exponent    byte
		intsign     byte
		intlsb      byte
		intmsb      byte
		intpad      byte
		mantissa    intsign byte, 4
		mantissabin intsign 4
	end

	export print_fp_hl

	##
	# :call-seq:
	#   print_fp_hl
	#
	# Call +print_fp_hl+ with +hl+ pointing to the 1st byte of a +ZXReal+ number
	# to print that number to the currently opened channel.
	#
	# After return the ZF flag can be inspected to check if the number was 0.
	print_fp_hl		ld a, [hl]      # get floating point from (hl)
					inc hl
					ld e, [hl]
					inc hl
					ld d, [hl]
					inc hl
					ld c, [hl]
					inc hl
					ld b, [hl]
					inc hl
					push hl         # save hl
					ld  l, a        # check if zero (return in Z flag)
					ora e
					ora d
					ora c
					ora b
					push af
					ld  a, l
					call stk_store  # store number on calculator stack
					call print_fp   # print number from stack
					pop af
					pop hl
					ret
end

if __FILE__ == $0
	# :stopdoc:
	class TestMath # :nodoc: all
		include Z80
		include Z80::TAP

		ZXReal = ZXMath::ZXReal # :nodoc:

		chan_open addr 0x1601
		print_a_1 addr 0x10

		# print real numbers, up to 0
		start         ld a, 2
									call chan_open
									ld hl, numbers[0]
		ploop         call math.print_fp_hl
									push af
									push hl
									ld a, "\r".ord
									rst print_a_1
									pop hl
									pop af
									jr NZ, ploop
									ret
		numbers       data ZXReal,
														-1.0,
														 1.0,
														 0.1,
												(-1.0/3),
												 32767.0,
												 32767.0.to_z80bin(false),
												 32768.5,
												 65535.0,
												 65535.0.to_z80bin(false),
												-65535.0,
												-65535.0.to_z80bin(false),
												-65536.0,
												 65536.0,
												[0xff,0x7f,0xff,0xff,0xff],
												[0xff,0xff,0xff,0xff,0xff],
												[1,0,0,0,0],
												[1,0x80,0,0,0],
												Math::PI,
												Math::E,
												(Math::PI*(10**-39)),
												(Math::E*(10**37)),
												{exponent: 4+128, mantissabin: "\x80\x80\x80\x80"},
												{exponent: 4+128, mantissa: [0x80,0x80,0x80,0x80]},
												[0,-1,1,0,0],
												'!@#$%^',
												{exponent: 0, intsign: -1, intlsb: 0x80, intmsb: 0},
												8.5,
												0
		import :math, ZXMath
	end

	p = TestMath.new 0x8000
	puts p.debug
	p.save_tap 'testmath.tap'

end
