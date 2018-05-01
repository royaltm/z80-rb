# -*- coding: BINARY -*-
class Z80MathInt
	# :nodoc:
	module Integers # :nodoc: all
		# Other integers are created ad-hoc
		class Int32 < Z80::Label
			value  byte 4
			bytes  value byte, 4
		end
	end
	module Macros
		##
		# Packs an integer of arbitrary byte size, creates a label and adds it to Program.code at Program.pc.
		#
		# Provided +bitsize+ must be a multiple of 8.
		# Integer data is packed in LSB order.
		def int(bitsize, value)
			raise ArgumentError, "int bitsize must be > 0" unless bitsize.is_a?(Integer) and bitsize > 0
			raise ArgumentError, "int bitsize must be multiple of 8" unless (bitsize & 7).zero?
			bytesize = bitsize >> 3
			klass_name = "Int#{bitsize}"
			int_klass = if Z80MathInt::Integers.const_defined?(klass_name)
				Z80MathInt::Integers.const_get(klass_name)
			else
				klass = Class.new(Z80::Label) do
					value  byte bytesize
					bytes  value byte, bytesize
				end
				Z80MathInt::Integers.const_set klass_name, klass
			end
			blob = ''
			bytesize.times do
					blob << [value].pack('c')
					value >>= 8
			end
					data int_klass, blob
		end
		##
		# Adds +a+ to +h+|+l+.
		#
		# Uses: +af+, +h+, +l+.
		#
		# ====Note:
		# Although this method is often better (faster and does not use other registers) way
		# to add 8bit unsigned register +a+ value to 16bit register it does not set flags properly.
		#
		# T-states: 20
		#
		# * +h+:: register input accumulator hi.
		# * +l+:: register input accumulator lo.
		def adda_to(h, l) # 20
				if h == l or [h,l].include?(a)
						raise ArgumentError, "adda_to invalid arguments!"
				end
				ns do
						add  l
						ld   l, a
						adc  h
						sub  l
						ld   h, a
				end
		end
		##
		# Subtracts +r+ from +h+, +l+.
		#
		# Uses: +af+, +r+, +h+, +l+, preserves +r+.
		#
		# ====Note:
		# Although this method is often better (faster and does not use other registers) way
		# to subtract 8bit unsigned register value from 16bit register it does not set flags properly.
		#
		# T-states: 24
		#
		# * +r+:: register subtractor must not be +a+.
		# * +h+:: register input accumulator hi.
		# * +l+:: register input accumulator lo.
		def sub_from(r, h, l) # 24
				if h == l or [r,h,l].include?(a)
						raise ArgumentError, "sub_from invalid arguments!"
				end
				ns do
					ld   a, l
					sub  r
					ld   l, a
					sbc  a
					add  h
					ld   h, a
				end
		end
		##
		# Performs multiplication of unsigned 16bit +mh+|+ml+ * 8bit +m+ and returns result in +hl+.
		# Optionally accumulates result in +hl+.
		# Breaks on overflow with CF=1.
		# 
		# Uses: +hl+, +m+, +mh+, +ml+, +tt+
		#
		# * +mh+::    hi multiplicant immediate value or any 8bit register.
		# * +ml+::    lo multiplicant immediate value or any 8bit register.
		# * +m+::     a multiplicator register, must not be +tthi+, +ttlo+.
		# * +tt+::    16 bit temporary register (de or bc).
		# * +clrhl+:: if +hl+ should be set or accumulated, if +false+ acts like: +hl+ += +mh+|+ml+ * +m+.
		def mul8_c(mh=h, ml=l, m=a, tt=de, clrhl = true)
			th, tl = tt.split
			raise ArgumentError if tt == hl or [th,tl].include?(m) or tl == mh or th == ml or !m.is_a?(Register)
			isolate do |eoc|
							ld  tl, ml unless ml == tl
							ld  th, mh unless mh == th
							ld  hl, 0 if clrhl
				loop1 srl m
							jr  NC, noadd
							add hl, tt
							jr  C, eoc
				noadd jr  Z, eoc
				skip1 sla tl
							rl  th
							jp  NC, loop1
			end
		end
		##
		# Performs multiplication of unsigned 16bit +mh+|+ml+ * 8bit +m+ and returns result in +hl+.
		# Optionally accumulates result in +hl+.
		# Optionally multiplies result * 2.
		#
		# Uses: +hl+, +m+, +mh+, +ml+, +tt+
		#
		# * +mh+::    hi multiplicant immediate value or any 8bit register.
		# * +ml+::    lo multiplicant immediate value or any 8bit register.
		# * +m+::     a multiplicator register, must not be +tthi+, +ttlo+.
		# * +tt+::    16 bit temporary register (de or bc).
		# * +clrhl+:: if +hl+ should be set or accumulated, if +false+ acts like: +hl+ += +mh+|+ml+ * +m+.
		# * +double+:: +true+ if should double the result (mh|ml * 2).
		def mul8(mh=h, ml=l, m=a, tt=de, clrhl = true, double = false)
			th, tl = tt.split
			raise ArgumentError if tt == hl or [th,tl].include?(m) or tl == mh or th == ml or !m.is_a?(Register)
			isolate do |eoc|
							ld  tl, ml unless ml == tl
							ld  th, mh unless mh == th
							ld  hl, 0 if clrhl
							jp  muls1 unless double
				loop1 sla  tl
							rl   th
				muls1 srl  m
							jr  NC, noadd
							add hl, tt
				noadd jr  NZ, loop1
			end
		end
		##
		# Performs multiplication of unsigned 16bit +mh+|+ml+ * 8bit +m+ and returns result in 24bit +a+|+hl+.
		# Optionally accumulates result in +a+|+hl+.
		#
		# Uses: +af+, +bc+, +de+, +hl+
		#
		# * +mh+::     hi multiplicant immediate value or any 8bit register.
		# * +ml+::     lo multiplicant immediate value or any 8bit register.
		# * +m+::      a multiplicator register, must not be +a+, +tthi+, +ttlo+ or +t+.
		# * +t+::      8 bit temporary register, must not be +a+, +tthi+, +ttlo+ or +m+.
		# * +tt+::     16 bit temporary register (+de+ or +bc+).
		# * +clrahl+:: if +a+|+hl+  should be set or accumulated, if +false+ acts like: +a+|+hl+ += +mh+|+ml+ * +m+.
		def mul8_24(mh=h, ml=l, m=b, t=c, tt=de, clrahl = true)
			th, tl = tt.split
			raise ArgumentError if tt == hl or [a,th,tl,t].include?(m) or [a,th,tl,m].include?(t) or
														 tl == mh or th == ml or !m.is_a?(Register) or !t.is_a?(Register)
			isolate do |eoc|
							ld  tl, ml unless ml == tl
							ld  th, mh unless mh == th
				if clrahl
							ld  hl, 0
							xor a
							ld  t, a
				else
							ld  t, 0
				end
							srl m           # 0 -> multiplicator -> carry
							jp  NZ, loop1   # m != 0 ? start regular loop
							jr  C, skadd    # m == 1 ? add and quit
							jp  eoc         # m == 0 ? just quit
				loop1 jr  NC, noadd   # carry == 0 ? don't add
							add hl, tt      # add multiplicant to result lo16
							adc a, t        # add multiplicant to result hi8
				noadd sla tl          # multiplicant *= 2
							rl  th
							rl  t
							srl m           # 0 -> multiplicator -> carry
							jp  NZ, loop1   # m != 0 ? loop
				skadd add hl, tt      # last add b.c. carry == 1
							adc a, t
			end
		end
		##
		# Performs multiplication of unsigned 16bit +mh+|+ml+ * 8bit +m+ and returns result in 24bit +a+|+hl+.
		# Creates unrolled code so +m+ should be constant in range: 0..255.
		# Optionally accumulates result in +a+|+hl+.
		# CF=0 ZF=1 if result fits in 16bits.
		#
		# Uses: +af+, +bc+, +de+, +hl+
		#
		# * +mh+::     hi multiplicant immediate value or any 8bit register.
		# * +ml+::     lo multiplicant immediate value or any 8bit register.
		# * +m+::      an immediate 8bit multiplicator value.
		# * +t+::      8 bit temporary register, must not be +a+, +tthi+ or +ttlo+.
		# * +tt+::     16 bit temporary register (+de+ or +bc+).
		# * +clrahl+:: if +a+|+hl+  should be set or accumulated, if +false+ acts like: +a+|+hl+ += +mh+|+ml+ * +m+.
		def mul_const8_24(mh=h, ml=l, m=0, t=c, tt=de, clrahl = true)
			th, tl = tt.split
			throw ArgumentError unless m.is_a?(Integer) and (0..255).include?(m) and [bc, de].include?(tt) and
																 ![h, l, th, hl, a].include?(t) and
																 tl != mh and th != ml
			isolate do |eoc|
							ld  tl, ml unless ml == tl
							ld  th, mh unless mh == th
				if clrahl
							xor a
							ld  t, a
					if (m & 1) == 1
							ld  l, tl  unless ml == l # hl = tt * 1
							ld  h, th  unless mh == h
					else
							ld  hl, 0
					end
				else
							ld  t, 0
					if (m & 1) == 1
							add hl, tt
							adc a, t
					elsif m == 0
							cp  a     # CF=0 and ZF=1 in case when m == 0
					end
				end
				while (m >>= 1) != 0
							sla tl
							rl  th
							rl  t
					if (m & 1) == 1
							add hl, tt
							adc a, t  # CF=0 and ZF=1 in case when result 16 bit
					end
				end
			end
		end
		##
		# Performs multiplication of unsigned 16bit +hl+ by 16bit +mm+ (+bc+ or +de+)
		# and returns result in 32 bit +hl+|+hl'+.
		#
		# Uses: +af+, +af'+, +hl+, +hl'+, +mm+, +tt'+
		#
		# * +mm+:: 16bit multiplicator (+bc+ or +de+)
		# * +tt'+:: 16bit tempoarary register (+bc+ or +de+)
		def mul16_32(mm=bc, tt=bc)
			raise ArgumentError unless [bc, de].include?(mm) and [bc, de].include?(tt)
			mh, ml = mm.split
			th, tl = tt.split
			isolate do |eoc|
								ld  a, ml
								ora a            # a' ?= 0
								ex  af, af       # a' = ml, ZF' = a' == 0
								xor a            # a  = 0
								exx
								ld  h, a         # hl' = 0
								ld  l, a
								ld  th, a        # th'|tl' = 0
								ld  tl, a
								exx
								ora mh           # a |= mh
								jr  Z, multlo0   # mh <> 0

								ld  mh, h        # mh|ml = hl
								ld  ml, l
								ld  hl, 0        # hl = 0

								scf
								adc a            # carry <- mh <- 1
								jr  NC, noadd1

				shadd1  srl mh           # mh -> ml -> th'
								rr  ml
								exx
								rr  th
								add hl, th|tl    # hl' += th'|tl'
								exx
								adc hl, mh|ml    # hl  += mh|ml + carry
								add a            # carry <- ml
								jr  Z, multlo
								jp  C, shadd1

				noadd1  srl mh           # mh -> ml -> mh'
								rr  ml
								exx
								rr  th
								exx
								add a            # carry <- ml
								jr  Z, multlo
								jp  C, shadd1
								jp  noadd1

				multlo0 ld  ml, h        # mh = 0, ml = h, th' = l, tl' = 0
								ld  a, l
								exx
								ld  th, a
								exx
								ld  hl, 0        # hl = 0

				multlo  ex  af, af       # a = ml, ZF = a == 0
								jr  Z, eoc
								add a            # carry <- ml
								jr  NC, noadd2

				shadd2  srl ml           # ml -> mh' -> ml'
								exx
								rr  th
								rr  tl
								add hl, th|tl    # hl' += th'|tl'
								exx
								adc hl, mh|ml    # hl  += mh|ml + carry
								add a            # carry <- ml
								jr  Z, finlo
								jp  C, shadd2

				noadd2  srl ml           # ml -> mh' -> ml'
								exx
								rr  th
								rr  tl
								exx
								add a            # carry <- ml
								jr  Z, finlo
								jp  C, shadd2
								jp  noadd2

				finlo   jr  NC, eoc
								srl ml           # ml -> mh' -> ml'
								exx
								rr  th
								rr  tl
								add hl, th|tl    # hl' += th'|tl'
								exx
								adc hl, mh|ml    # hl  += mh|ml + carry
			end
		end
		##
		# Performs euclidean divison. Divides +hl+ by +m+.
		# Returns quotient in +hl+ and remainder in +a+. +m+ remains unaltered.
		#
		# Uses: +af+, +b+, +m+, +hl+
		#
		# * +m+:: a divisor (+c+, +d+ or +e+)
		# * opts::
		#   - :check0:: (default +true+) checks if divisor is 0, in this instance CF indicates division error
		#               and nothing except +a+ register is altered on CF=1. If +false+ CF should be ignored.
		#   - :check1:: (default +true+) checks if divisor is 1, a hot path optimization
		#   - :modulo:: (default +false+) calculates remainder only, in this instance +hl+ will be 0
		#               when division is finished.
		def divmod8(m=c, opts = {})
			raise ArgumentError unless [c, d, e].include?(m)
			flags = {
				:check0 => true,
				:check1 => true,
				:modulo => false
			}.merge opts
			isolate do |eoc|
				if flags[:check0] or flags[:check1]
								ld  a, m
								cp  1
								jr  C, eoc if flags[:check0] # division by 0
					if flags[:check1]
								jp  NZ, divstrt # division by m > 1
								xor a            # clear rest
								jp  eoc          # division by 1
					end
				end
				divstrt xor a            # a = 0
								ld  b, 16
				findhi  add hl, hl       # align highest set bit at CF
								jr  C, found
								djnz findhi
								jp  eoc          # hl == 0
				loopfit add hl, hl       # carry <- hl <- 0
				found   adc a            # carry <- a <- carry
								cp  m            # a - m
								jr  NC, fits     # a >= m
								djnz loopfit     # loop
								ccf if flags[:check0]    # clear carry only when check0
								jp  eoc
				fits    sub m            # a = a - m (rest)
				unless flags[:modulo]
									inc l          # hl <- 1 (quotient)
				end
								djnz loopfit     # loop
			end
		end
		##
		# Performs euclidean divison. Divides +hl+ by +de+.
		# Returns quotient in +hl+ and remainder in +bc+. +de+ remains unaltered.
		#
		# Uses: +af+, +bc+, +de+, +hl+, +x+
		#
		# * +x+:: a temporary register (+ixh+, +ixl+, +iyh+ or +iyl+).
		# * opts::
		#   - :check0:: (default +true+) checks if divisor is 0, in this instance CF indicates division error
		#               and nothing except +a+ register is altered on CF=1. If +false+ CF should be ignored.
		#   - :check1:: (default +true+) checks if divisor is 1, a hot path optimization
		#   - :modulo:: (default +false+) calculates remainder only, in this instance +hl+ will be 0
		#               when division is finished.
		#   - :quick8:: (default +true+) checks if divisor fits in 8 bits and in this instance
		#               uses different, optimized code.
		def divmod16(x=ixl, opts = {})
			raise ArgumentError unless [ixh, ixl, iyh, iyl].include?(x)
			flags = {
				:check0 => true,
				:check1 => true,
				:modulo => false,
				:quick8 => true
			}.merge opts
			isolate do |eoc|
				if flags[:check0] or flags[:check1] or flags[:quick8]
									xor a
									ora d
									jp  NZ, div16strt
					if flags[:quick8]
									divmod8 e, opts
									ld  b, 0
									ld  c, a
									jp  eoc
					elsif flags[:check0] or flags[:check1]
									ld  a, e
									cp  1
									jr  C, eoc if flags[:check0] # division by 0
						if flags[:check1]
									jp  NZ, div16strt # division by m > 1
									ld  bc, 0         # clear rest
									jp  eoc           # division by 1
						end
					end
				end
				div16strt xor a            # a = 0 hi remainder
									ld  c, a         # c = 0 lo remainder
									ld  b, 16
				loopfit   add hl, hl       # carry <- hl <- 0
									rl  c            # carry <- c <- carry
									adc a            # carry <- a <- carry
									cp  d            # a - d
									jr  NC, fitshi   # a >= d
									djnz loopfit     # loop
									ccf if flags[:check0]
									jp  over
				fitshi    ld  x, a
									ld  a, c
									jr  NZ, fitslo   # a > d, ignore e
									cp  e            # a == d: c - e
									jr  NC, fitslo   # a >= e
									ld  a, x 
									djnz loopfit     # loop
									ccf if flags[:check0]
									jp  over
				fitslo    sub e            # a = c - e
									ld  c, a         # c = c - e
									ld  a, x
									sbc d            # a -= d
				unless flags[:modulo]
									inc l            # hl <- 1 (quotient)
				end
									djnz loopfit     # loop
				over      ld  b, a         # bc = remainder
			end
		end
		##
		# Performs euclidean divison. Divides +hl+|+hl'+ by +m+.
		# Returns quotient in +hl+|+hl'+ and remainder in +a+. +m+ remains unaltered.
		#
		# Uses: +af+, +a'+, +b+, +b'+, +m+, +m'+, +hl+, +hl'+
		#
		# * +m+:: a divisor (+c+, +d+ or +e+)
		# * opts::
		#   - :check0:: (default +true+) checks if divisor is 0, in this instance CF indicates division error
		#               and nothing except +a+ register is altered on CF=1. If +false+ CF should be ignored.
		#   - :check1:: (default +true+) checks if divisor is 1, a hot path optimization
		#   - :modulo:: (default +false+) calculates remainder only, in this instance +hl+|+hl'+ will be 0
		#               when division is finished.
		def divmod32_8(m=c, opts={})
			raise ArgumentError unless [c, d, e].include?(m)
			flags = {
				:check0 => true,
				:check1 => true,
				:modulo => false
			}.merge opts
			isolate do |eoc|
				if flags[:check0] or flags[:check1]
									ld  a, m
									cp  1
									jr  C, eoc if flags[:check0] # division by 0
					if flags[:check1]
									jp  NZ, divstrt  # division by m > 1
									xor a            # clear rest
									jp  eoc          # division by 1
					end
				end
				divstrt   xor a            # a = 0
									ld  b, 16
				loopfit1  add hl, hl       # carry <- hl <- 0
									adc a            # carry <- a <- carry
									cp  m            # a - m
									jr  NC, fits1    # a >= m
									djnz loopfit1    # loop
									jp  divlo16
				fits1     sub m            # a = a - m (rest)
				unless flags[:modulo]
									inc l          # hl <- 1 (quotient)
				end
									djnz loopfit1    # loop

				divlo16   ex  af, af
									ld  a, m
									exx
									ld  m, a
									ex  af, af
									ld  b, 16
				loopfit2  add hl, hl       # carry <- hl <- 0
									adc a            # carry <- a <- carry
									cp  m            # a - m
									jr  NC, fits2    # a >= m
									djnz loopfit2    # loop
									ccf if flags[:check0] # clear carry only when check0
									jp  over
				fits2     sub m            # a = a - m (rest)
				unless flags[:modulo]
									inc l            # hl <- 1 (quotient)
				end
									djnz loopfit2    # loop
				over      exx
			end
		end
		##
		# Performs euclidean divison. Divides +hl+|+hl'+ by +de+.
		# Returns quotient in +hl+|+hl'+ and remainder in +bc+. +de+ remains unaltered.
		#
		# Uses: +af+, +a'+, +bc+, +bc'+, +de+, +de'+, +hl+, +hl'+, +x+
		#
		# * +x+:: a temporary register (+ixh+, +ixl+, +iyh+ or +iyl+).
		# * opts::
		#   - :check0:: (default +true+) checks if divisor is 0, in this instance CF indicates division error
		#               and nothing except +a+ register is altered on CF=1. If +false+ CF should be ignored.
		#   - :check1:: (default +true+) checks if divisor is 1, a hot path optimization
		#   - :modulo:: (default +false+) calculates remainder only, in this instance +hl+|+hl'+ will be 0
		#               when division is finished.
		#   - :quick8:: (default +true+) checks if divisor fits in 8 bits and in this instance
		#               uses different, optimized code.
		def divmod32_16(x=ixl, opts={})
			raise ArgumentError unless [ixh, ixl, iyh, iyl].include?(x)
			flags = {
				:check0 => true,
				:check1 => true,
				:modulo => false,
				:quick8 => true
			}.merge opts
			isolate do |eoc|
				if flags[:check0] or flags[:check1] or flags[:quick8]
									xor a
									ora d
									jp  NZ, div32strt
					if flags[:quick8]
									divmod32_8 e, opts
									ld  b, 0
									ld  c, a
									jp  eoc
					elsif flags[:check0] or flags[:check1]
									ld  a, e
									cp  1
									jr  C, eoc if flags[:check0] # division by 0
						if flags[:check1]
									jp  NZ, div32strt # division by m > 1
									ld  bc, 0
									jp  eoc           # division by 1
						end
					end
				end
				div32strt xor a            # a = 0 hi remainder
									ld  c, a         # c = 0 lo remainder
									ld  b, 16
				loopfit1  add hl, hl       # carry <- hl <- 0
									rl  c            # carry <- c <- carry
									adc a            # carry <- a <- carry
									cp  d            # a - d
									jr  NC, fitshi1  # a >= d
									djnz loopfit1    # loop
									jp  divlo16
				fitshi1   ld  x, a
									ld  a, c
									jr  NZ, fitslo1  # a > d, ignore e
									cp  e            # a == d: c - e
									jr  NC, fitslo1  # a >= e
									ld  a, x 
									djnz loopfit1    # loop
									jp  divlo16
				fitslo1   sub e            # a = c - e
									ld  c, a         # c = c - e
									ld  a, x
									sbc d            # a -= d
				unless flags[:modulo]
									inc l            # hl <- 1 (quotient)
				end
									djnz loopfit1    # loop

				divlo16   push de
									ld  x, c
									exx              # hl' <-> hl
									pop de           # de' = de
									ld  c, x         # c' = c

									ld  b, 16
				loopfit2  add hl, hl       # carry <- hl' <- 0
									rl  c            # carry <- c' <- carry
									adc a            # carry <- a <- carry
									cp  d            # a - d'
									jr  NC, fitshi2  # a >= d'
									djnz loopfit2    # loop
									ccf if flags[:check0]
									jp  over
				fitshi2   ld  x, a
									ld  a, c
									jr  NZ, fitslo2  # a > d, ignore e
									cp  e            # a == d: c - e
									jr  NC, fitslo2  # a >= e
									ld  a, x
									djnz loopfit2    # loop
									ccf if flags[:check0]
									jp  over
				fitslo2   sub e            # a = c' - e'
									ld  c, a         # c' = c' - e'
									ld  a, x
									sbc d            # a -= d'
				unless flags[:modulo]
									inc l            # hl' <- 1 (quotient)
				end
									djnz loopfit2    # loop
				over      ld  x, c
									exx
									ld  b, a         # bc = remainder
									ld  c, x
			end
		end
		##
		# Lehmer random number generator.
		#
		# See: https://en.wikipedia.org/wiki/Lehmer_random_number_generator
		#
		# Routine uses similar parameters as in ZX-Spectrum ROM:
		# 
		#    s1 = (s0 + 1) * 75 % 65537 - 1
		#
		# Uses: +af+, +bc+, +de+, +hl+.
		#
		# Expects seed in +hl+ and produces next seed iteration in +hl+.
		def rnd
			isolate do |eoc|
								inc hl          # seed + 1
								ld  a, l
								ora h
								jp  NZ, multi75 # (seed + 1) < 65536
																# return pre-calculated value
								ld  hl, 65536 * 75 % 65537 - 1
								jr  eoc
																# overflow 0x1_0000 - 0x1_0001 = -1 happens only for seeds:
																# 65535, 20097, 34952, 40195
				ovrflow add a           # shift left (for all possible cases CF=0 after this)
																# otherwise we should have take care of CF=1
								djnz mnext      # this was the last iteration so the remainder result is 65536
								jr  eoc         # remainder = (borrow|hl = 0x1_0000) (seed - 1) == 65535

																# a|hl = (seed + 1) * 75
				multi75 mul_const8_24(h, l, 75, c, de, true)
								jr  Z, fits     # a|hl < 0x1_0000: n mod 65537 == n
																# a|hl never == 0x1_0000 after multiplication by 75
																# so we can ignore some checks later
								ld  de, -1      # a|hl mod 65537 (0x1_0001) goes to borrow|hl
								ld  b, 8        # shift left this many times before dividend fits in a divisor
				mloop0  add hl, hl      # pre-shift left a|hl, highest bit to CF
								adc a
								dec b           # no check if b==0, there'll be at most 8 iterations since a >= 0x01
								jp  NC, mloop0  # CF == 0: loop
								ld  c, l        # swap registers: hl|a = a|hl
								ld  l, h
								ld  h, a
								ld  a, c
								dec hl          # hl = hl - 1, in effect: borrow|hl = 0x1_nnnn - 0x1_0001 = 0x0_mmmm
																# no need to check for overflow here since 65536 is never a result of n*75
				mloopck jr  Z, fits     # b==0: (seed + 1) * 75 == 0x0001_nnnn
				mloop1  add a           # shift left hl|a, highest bit to CF
								adc hl, hl
								jr  NC, mnext   # CF == 0: continue
								add hl, de      # borrow|hl = 0x1_nnnn - 0x1_0001
								jr  NC, ovrflow # 0x1_0000 - 0x1_0001 = -1 (0x1_FFFF)
				mnext   djnz mloop1
				fits    dec hl          # seed - 1
			end
		end
		##
		# Convert a 8-bit unsigned integer to bcd
		# allows for arbitrary integer size conversion.
		#
		# Used by +utobcd+.
		#
		# Uses: +a+, +b+, +r+, +t+, +hl+
		def utobcd_step(bufend, r, buflen=1, t=c, r_in_a=false)
			raise ArgumentError unless [c, d, e].include?(r) and [c, d, e].include?(t) and r != t and
							(!buflen.is_a?(Register) || buflen == t)
			isolate do
								ld  a, r unless r_in_a
								ld  t, buflen unless buflen == t
								scf
								rla              # carry <- a <- 1
								ld  r, a
				buffmul ld  hl, bufend-1 # multiply buffer[] * 2 + carry using BCD
								ld  b, t
				nextadd ld  a, [hl]
								adc a
								daa
								ld  [hl], a
								dec hl
								djnz nextadd
								jp  NC, nbufext  # no carry
								inc t            # extend buffer on carry
								inc b            # b = 1
								ld  [hl], b      # put 1 in new place
				nbufext sla r            # carry <- r <- 0
								jp  NZ, buffmul
			end
		end
		##
		# Converts arbitrary size unsigned integer (LSB) to bcd
		#
		# Uses: +a+, +b+, +rr+, +bc'+, +hl'+, +r'+
		#
		# After conversion +c'+ contains number of bytes used to store bcd number.
		# Subtract it from +bufend+ to get the first byte.
		#
		# Place integer address in +input+ of +size+ bytes and +bufend+ should point to the address
		# immediately following buffer end. Provide large enough buffer.
		#
		# * +bufend+:: must be an address (or a label)
		# * +input+:: may be an immediate address (or a label) or the same as +rr+, in this instance
		#             it is expected that +rr'+ will already contain +input+ address.
		# * +size+:: may be an integer or one of 8-bit registers.
		# * +r+:: temporary register (d or e)
		# * +rr'+:: temporary register (de or hl)
		def utobcd(bufend, input, size=4, r=d, rr=de)
			raise ArgumentError unless (!input.is_a?(Register) or input == rr) and
													(size.is_a?(Integer) or (size.is_a?(Register) and size.bit8?)) and
							[de, hl].include?(rr) and [d, e].include?(r)
			isolate do
						if !input.is_a?(Register) and !size.is_a?(Register)
							ld  b, size unless size == b
							ld  rr, input + size
						else
							ld  a, size unless size == a
							ld  b, a unless size == b
							ld  rr, input unless input == rr
							adda_to *rr.split
						end
						xor a
						ld  [bufend - 1], a
						exx
						ld  c, 1
						exx
			loopi dec  rr
						ld  a, [rr]
						exx
						utobcd_step(bufend, r, c, c, true)
						exx
						djnz loopi
			end
		end
		##
		# Reads each bcd digit as +a+ destroying content of a buffer in the process.
		#
		# Uses: +a+, +hl+, +b+.
		#
		# On first digit carry is 1 on subsequent is 0 before +block+.
		#
		# Block must not alter +hl+ or +b+.
		def bcdtoa(buffer, size, &block)
			raise ArgumentError unless (!buffer.is_a?(Register) or buffer == hl)
			isolate do
						ld  b, size unless size == b
						ld  hl, buffer unless buffer == hl
						xor a
						scf
			loopa rld
						ns(&block)
						xor a
						rld
						ns(&block)
						inc hl
						xor a
						djnz loopa
			end
		end
	end
	include Z80
end
