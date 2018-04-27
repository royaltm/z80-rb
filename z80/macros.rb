# -*- coding: BINARY -*-
module Z80
	module Program
		#  ==Z80 Macros
		#
		#  Common used macros.
		module Macros
			##
			# Load content of the register bb into register aa.
			#
			# A sugar for two 8bit ld instructions.
			#
			# --Example:
			#   ld16  bc, hl
			def ld16(aa, bb)
				unless [bc, de, hl].include?(aa) and [bc, de, hl].include?(bb)
					raise ArgumentError, "Use one of: bc de or hl registers in ld16"
				end
				raise ArgumentError, "Registers must be different" if aa == bb
				ah, al = aa.split
				bh, bl = bb.split
				ns do
					ld  al, bl
					ld  ah, bh
				end
			end
		end
		include Macros
	end
end
