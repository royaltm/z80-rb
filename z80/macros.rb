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
				m = lambda do |*args, &block|
					if args.first.is_a?(Symbol)
						n = args.shift
					end
					if registers.empty?
						ns(n, **nsopts) do |eoc|
							mblock.call eoc, *args, &block
						end
					else
						lbl = with_saved(*registers, **nsopts) do |eoc|
							mblock.call eoc, *args, &block
						end
						lbl = self.send n.to_sym, lbl if n
						lbl
					end
				end
				define_singleton_method(name.to_sym, &m)
			end
			##
			# Pushes specified registers on a machine stack, evaluates block inside a namespace
			# and pops registers in reverse order.
			# Returns a label pointing to the beginning of the pushes. You may optionally pass a +name+
			# for the returned label to be named.
			#
			# +registers+ should be one of:
			#
			# * a 16bit register to push on a stack and pop after your code, except +sp+ obviously,
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
			#
			# Evaluates your +block+ inside Program.ns. All other +opts+ are being passed along to Program.ns.
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
					ret if with_return
				end
			end
			##
			# Loads a content of the 16-bit register +bb+ into the 16-bit register +aa+.
			#
			# A sugar for two 8-bit +ld+ instructions.
			#
			# Example:
			#   ld16  bc, hl
			def ld16(aa, bb)
				unless [bc, de, hl, ix, iy].include?(aa) and [bc, de, hl, ix, iy].include?(bb)
					raise ArgumentError, "Use one of: bc de hl ix iy registers in ld16"
				end
				raise ArgumentError, "Registers must be different" if aa == bb
				ah, al = aa.split
				bh, bl = bb.split
				isolate do
					ld  al, bl
					ld  ah, bh
				end
			end
			##
			# Compares a register pair +th+|+th+ with a 16-bit +value+.
			#
			#   CF, ZF = (th|tl - value)
			#
			#   CF = 1 if th|tl <  value
			#   CF = 0 if th|tl >= value
			#   ZF = 0 if th|tl <> value
			#   ZF = 1 if th|tl == value
			#
			# * +jr_msb_c+: provide a label if you want to jump immediately after +th+ < (value >> 8)
			#   or +:ret+ symbol to return from a subroutine.
			# * +jr_msb_nz+: provide a label if you want to jump immediately after +th+ <> (value >> 8)
			#   or +:ret+ symbol to return from a subroutine. By default jumps to +eoc+ of the +cp16n+ code,
			#   so flags can be examined later.
			# Example:
			#   cp16n  h,l, foo, jr_msb_c: less_than_foo
			#   jr C, less_than_foo
			def cp16n(th, tl, value, jr_msb_c: nil, jr_msb_nz: :eoc)
				isolate do |eoc|
					jr_msb_nz = eoc if jr_msb_nz == :eoc
						ld   a, th
						cp   value>>8
						case jr_msb_c
						when :ret
							ret  C
						else
							jr   C, jr_msb_c
						end unless jr_msb_c.nil?
						case jr_msb_nz
						when :ret
							ret  NZ
						else
							jr   NZ, jr_msb_nz
						end
						ld   a, tl
						cp   value
				 end
			end
			##
			# Compares a register pair +th+|+th+ with another register pair +sh+|+sl+.
			#
			#   CF, ZF = (th|tl - +sh+|+sl+)
			#
			#   CF = 1 if th|tl <  sh|sl
			#   CF = 0 if th|tl >= sh|sl
			#   ZF = 0 if th|tl <> sh|sl
			#   ZF = 1 if th|tl == sh|sl
			#
			# * +jr_msb_c+: provide a label if you want to jump immediately after +th+ < +sh+
			#   or +:ret+ symbol to return from a subroutine.
			# * +jr_msb_nz+: provide a label if you want to jump immediately after +th+ <> +sh+
			#   or +:ret+ symbol to return from a subroutine. By default jumps to +eoc+ of the +cp16r+ code,
			#   so flags can be examined later.
			#
			# Example:
			#   cp16n  h,l, d,e, jr_msb_nz: not_equal
			#   jr NZ, not_equal
			def cp16r(th, tl, sh, sl, jr_msb_c: nil, jr_msb_nz: :eoc)
				isolate do |eoc|
					jr_msb_nz = eoc if jr_msb_nz == :eoc
						ld   a, th
						cp   sh
						case jr_msb_c
						when :ret
							ret  C
						else
							jr   C, jr_msb_c
						end unless jr_msb_c.nil?
						case jr_msb_nz
						when :ret
							ret  NZ
						else
							jr   NZ, jr_msb_nz
						end
						ld   a, tl
						cp   sl
				end
			end
		end
		include Macros
	end
end
