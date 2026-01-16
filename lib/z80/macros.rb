# -*- coding: BINARY -*-
module Z80
	module Program
		#  ==Z80 Macros
		#
		#  A few handy macros.
		module Macros
			##
			# A convenient method to create local macros.
			#
			# Give a +name+ (Symbol) to your macro, an optional list of +registers+ to push before and pop after code
			# and a block of code. See Macros.with_saved for explanation of +registers+ arguments.
			# The block will receive +eoc+ label (see Program.ns) and any argument you pass when calling a macro.
			#
			# If you want your macros being exportable, do not use this method.
			# Instead create module `Macros' inside your *program* class and define methods there.
			# Remember to wrap the code in Program.ns, or better yet: Program.isolate.
			#
			# <b>Macros and labels share the same namespace, however labels can be nested, but macros can't.</b>
			#
			# <b>Unlike labels, macros must be defined before being used.</b>
			#
			# *Note*:: Be carefull with +ret+ instruction if you used +registers+.
			#          You can use <tt>ret: +true+</tt> option instead, see: Macros.with_saved.
			#
			# Example:
			#   macro :cp16 do |eoc, aa, bb|
			#     ah, al = aa.split
			#     bh, bl = bb.split
			#              ld  a, ah
			#              cp  bh
			#              jr  NZ, eoc
			#              ld  a, al
			#              cp  bl
			#   end
			#
			#   cp16 bc, hl
			#   jr   C, bc_less_than_hl
			def macro(name, *registers, **nsopts, &mblock)
				name = name.to_sym if name.is_a?(String)
				raise Syntax, "A macro must have a name" unless name.is_a?(Symbol)
				raise Syntax, "Macros may only be defined in the program context." if @contexts.size > 1
				raise Syntax, "A label with the same name: \"#{name}\" exists." if @labels.has_key?(label.to_s)
				m = lambda do |*args, **opts, &block|
					if args.first.is_a?(Symbol)
						n = args.shift
					end
					if registers.empty?
						ns(n, **nsopts) do |eoc|
							if opts.empty?
								mblock.call(eoc, *args, &block)
							else
								mblock.call(eoc, *args, **opts, &block)
							end
						end
					else
						lbl = with_saved(*registers, **nsopts) do |eoc|
							if opts.empty?
								mblock.call(eoc, *args, &block)
							else
								mblock.call(eoc, *args, **opts, &block)
							end
						end
						lbl = self.define_label n, lbl if n
						lbl
					end
				end
				define_singleton_method(name.to_sym, &m)
			end
			##
			# Adds a code that pushes specified registers on a machine stack, code from +block+ within
			# a namespace and code that pops registers in reverse order.
			# Returns a label pointing to the beginning of the pushes. You may optionally pass a +name+
			# for the returned label to be named.
			#
			# +registers+ should be one of:
			#
			# * a 16bit register to push on the stack and pop after code from +block+, except +sp+ obviously,
			# * +:exx+ symbol to evaluate +exx+ instruction,
			# * +:ex_af+ symbol to evaluate <code>ex af, af</code> instruction,
			# * +:all+ symbol evaluates to: +af+, +bc+, +de+, +hl+, +ix+, +iy+
			# * +:all_but_ixiy+ symbol evaluates to: +af+, +bc+, +de+, +hl+
			# * +:ixiy+ symbol evaluates to: +ix+, +iy+
			#
			# +opts+ may be one of:
			#
			# * +:ret+ if the +ret+ instruction should be added after the registers have been restored.
			#   In this instance jumping to +eoc+ will effectively return from the subroutine.
			#   If <tt>:ret => :after_ei</tt> the +ei+ instruction will be added before +ret+.
			#
			# Other accepted values for +ret:+ option are:
			# * +:reti+: the +reti+ instruction will be added instead of +ret+.
			# * +:retn+: the +retn+ instruction will be added instead of +ret+.
			# * +:after_ei+: the +ei+ instruction will be added before +ret+.
			# * +:ei_reti+: the +ei+ instruction will be added before +reti+.
			#
			# Evaluates your +block+ with Program.ns. All other +opts+ are being passed along to Program.ns.
			#
			# Examples:
			#   with_saved af, de do |eoc|
			#     # ... do something with af and de
			#   end
			#
			#   with_saved af, hl, :exx, hl, :exx, ret: true do |eoc|
			#     # ... saves and restores af, hl, hl' and swaps shadow registers back before
			#     # adds `ret' instruction after restoring all the registers
			#   end
			#
			#   with_saved :bar, :all, :exx, :ex_af, :all_but_ixiy, inherit: foo, isolate: true do |eoc|
			#     # saves and restores: af, bc, de, hl, ix, iy, af', bc', de', hl'
			#     # creates an isolated namespace with inherited `foo' label
			#     # returns "bar" namespace label addressing beginning of the pushes
			#   end
			#
			# <b>Be carefull with +ret+ instruction in a provided block. Remember to pop registers first or use +:ret+ option instead.</b>
			def with_saved(*registers, **opts, &block)
				name = nil
				with_return = opts.delete :ret
				registers.map!.with_index do |rr, i|
					case rr
					when :all
						[af, bc, de, hl, ix, iy]
					when :no_ixy, :no_ixiy, :all_but_ixy, :all_but_ixiy, :afbcdehl
						[af, bc, de, hl]
					when :ixy, :ixiy
						[ix, iy]
					when :ex_af, :ex_af_af, :exx
						rr
					when Symbol, String
						if i == 0
							name = rr
							[]
						else
							rr
						end
					else
						rr
					end
				end.flatten!
				saver = proc do |rr, instr|
					case rr
					when af, bc, de, hl, ix, iy
						self.send instr, rr
					when :exx
						exx
					when :ex_af, :ex_af_af
						ex af, af
					else
						raise ArgumentError, "arguments must be one of: af, bc, de, hl, ix, iy, :exx or :ex_af, got: #{rr.inspect}"
					end
				end
				ns(name, **opts) do
					registers.each{|rr| saver[rr, :push]}
					ns(**opts, merge: true, &block)
					registers.reverse_each{|rr| saver[rr, :pop]}
					if with_return
						ei if [:after_ei, :ei, :ei_ret, :ei_reti].include?(with_return)
						case with_return
						when :reti, :ei_reti
							reti
						when :retn
							retn
						when true, :after_ei, :ei, :ei_ret, :ret
							ret
						else
							raise ArgumentError, "unrecognized value of :ret option: #{with_return.inspect}"
						end
					end
				end
			end
			##
			# Creates code that loads a content of the 16-bit register +bb+ into the 16-bit register +aa+.
			#
			# A sugar for two 8-bit +ld+ instructions.
			#
			# Example:
			#   ld16  bc, hl
			def ld16(aa, bb)
				unless [bc, de, hl, ix, iy].include?(aa) and [bc, de, hl, ix, iy].include?(bb)
					raise ArgumentError, "Use one of: bc de hl ix iy registers in ld16"
				end
				ah, al = aa.split
				bh, bl = bb.split
				isolate do
					ld  al, bl
					ld  ah, bh
				end
			end
			##
			# Creates code that compares a pair of registers +th+|+tl+ with a +value+ as unsigned
			# 16-bit integers.
			#
			# Provide +value+ as an integer or a label.
			#
			#   CF, ZF = (th|tl - value)
			#
			#   CF = 1 if th|tl <  value
			#   CF = 0 if th|tl >= value
			#   ZF = 0 if th|tl <> value
			#   ZF = 1 if th|tl == value
			#
			# Options:
			# * +jr_msb_c+: provide a label if you want to jump immediately after +th+ < (value >> 8)
			#   or +:ret+ symbol to return from a subroutine.
			# * +jr_msb_nz+: provide a label if you want to jump immediately after +th+ <> (value >> 8)
			#   or +:ret+ symbol to return from a subroutine. By default jumps to +eoc+ of the +cp16n+ code,
			#   so flags can be examined later.
			#
			# Example:
			#   cp16n  h,l, foo, jr_msb_c: less_than_foo
			#   jr C, less_than_foo
			#
			# Modifies: +af+.
			def cp16n(th, tl, value, jr_msb_c: nil, jr_msb_nz: :eoc)
				cp16r(th, tl, value >> 8, value & 0xFF, jr_msb_c:jr_msb_c, jr_msb_nz:jr_msb_nz)
			end
			##
			# Creates code that compares a pair of 16-bit registers +tt+ with +ss+ as unsigned integers.
			#
			# A sugar for:
			#   cp16r(th,tl, sh,sl, ...)
			#
			# See: Macros#cp16r.
			def cp16rr(tt, ss, jr_msb_c: nil, jr_msb_nz: :eoc)
				raise ArgumentError, "cp16rr: tt must be a 16-bit register" unless !pointer?(tt) && register?(tt) && !tt.bit8?
				raise ArgumentError, "cp16rr: ss must be a 16-bit register" unless !pointer?(ss) && register?(ss) && !ss.bit8?
				th, tl = tt.split
				sh, sl = ss.split
				cp16r(th, tl, sh, sl, jr_msb_c:jr_msb_c, jr_msb_nz:jr_msb_nz)
			end
			##
			# Creates code that compares a pair of registers +th+|+tl+ with another pair +sh+|+sl+ as
			# unsigned 16-bit integers.
			#
			#   CF, ZF = (th|tl - +sh+|+sl+)
			#
			#   CF = 1 if th|tl <  sh|sl
			#   CF = 0 if th|tl >= sh|sl
			#   ZF = 0 if th|tl <> sh|sl
			#   ZF = 1 if th|tl == sh|sl
			#
			# Options:
			# * +jr_msb_c+: provide a label if you want to jump immediately after +th+ < +sh+
			#   or +:ret+ symbol to return from a subroutine.
			# * +jr_msb_nz+: provide a label if you want to jump immediately after +th+ <> +sh+
			#   or +:ret+ symbol to return from a subroutine. By default jumps to +eoc+ of the +cp16r+ code,
			#   so flags can be examined later.
			#
			# Example:
			#   cp16r  h,l, d,e, jr_msb_nz: not_equal
			#   jr NZ, not_equal
			#
			# Modifies: +af+.
			def cp16r(th, tl, sh, sl, jr_msb_c: nil, jr_msb_nz: :eoc)
				raise ArgumentError, "cp16r: the accumulator can only be used in place of th" if [tl, sh, sl].include?(a)
				skip_nz = proc do |target|
					case target
					when :ret
							ret  NZ
					else
							jr   NZ, target
					end
				end
				isolate do |eoc|
					jr_msb_nz = eoc if jr_msb_nz == :eoc
									ld   a, th unless th == a
					if sh == 0 && sl == 0
								ora  a
								ora  tl
								skip_nz[jr_msb_nz] unless jr_msb_nz == eoc
					else
						if sh == 0
									ora  a
						else
									cp   sh
							case jr_msb_c
							when :ret
									ret  C
							else
									jr   C, jr_msb_c
							end unless jr_msb_c.nil?
						end
									skip_nz[jr_msb_nz]
									ld   a, tl
						if sl == 0
									ora  a
						else
									cp   sl
						end
					end
				end
			end
			##
			# Creates code that reverses bits in an octet.
			#
			# The result is found in the accumulator register or +t+ depending on the +t_is_target+
			# option.
			#
			# The CF flag will contain the last shifted out bit of the previous content of the reverse
			# operation target.
			#
			# The ZF flag will be set if the the reversed bits were all 0, otherwise it will be reset.
			#
			# +bits+:: bits to reverse, as an 8-bit register or an argument passed to load an 8-bit
			#          register instruction.
			#          +bits+ can also be an intermediate address label if the +t_is_target+ option
			#          is +true+. In this instace the +t+ option needs to be specified as an 8-bit
			#          register or [hl].
			#
			# Options:
			# * +t+:: a temporary register to use or the target of the reverse operation if the
			#         +t_is_target+ option is +true+.
			# * +unroll+:: controls whether to unroll the loop.
			# * +t_is_target+:: controls whether the reverse operation target is +t+ instead of +a+.
			# * +shift_target_left+:: controls whether to shift target bits left. Otherwise the
			#                         target is shifted right. This influences the content of the CF
			#                         flag at the end of the operation.
			#
			# if +unroll+ is +false+, the +b+ register is used as a loop counter and cannot be used
			# as the +t+ option.
			#
			# T-states: 202|206|209|215, unrolled: 96|100|103|109.
			#
			# Code size: 7 +(0-3) bytes, unrolled: 24 +(0-3) bytes.
			#
			# Modifies: +af+, +b+ if +unroll+ is +false+ and optionally +t+.
			def rev8(bits, t:bits, unroll: false, t_is_target:false, shift_target_left:false)
				isolate do
					t = unwrap_pointer(t)
					unless t == hl[] or (register?(t) && t.bit8? && t != a)
						raise ArgumentError, "rev8: t must be an 8-bit register or [hl] except the accumulator"
					end
					raise ArgumentError, "rev8: b can't be used for t if :unroll is false" unless unroll || t != b
					if shift_target_left
						rxc  = proc {|x| rrc  x }
						rxca = proc { rrca }
						rx   = proc {|x| rl   x }
						rxa  = proc { rla    }
					else
						rxc  = proc {|x| rlc  x }
						rxca = proc { rlca }
						rx   = proc {|x| rr   x }
						rxa  = proc { rra    }
					end
					if t_is_target
							ld   a, bits unless bits == a
						if unroll
							8.times do
								rxca.call
								rx.call t
							end
						else
								ld   b, 8
						lrev	rxca.call
								rx.call t
								djnz lrev
						end
					else
							ld   t, bits unless t == bits
						if unroll
							8.times do
								rxc.call t
								rxa.call
							end
						else
								ld   b, 8
						lrev	rxc.call t
								rxa.call
								djnz lrev
						end
					end
				end
			end
		end
		include Macros
	end
end
