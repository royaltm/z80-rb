# -*- coding: BINARY -*-
module Z80
	module Program
		#  ==Z80 Macros
		#
		#  Commonly used macros.
		module Macros
			##
			# Convenient method to create local macros.
			#
			# Give a +name+ (Symbol) to your macro, (optional) list of +registers+ to push before and pop after code
			# and a block of code. See Macros.with_saved for explanation of +registers+ arguments.
			# The block will receive +eoc+ label (see Program.ns) and any argument you pass when calling a macro.
			#
			# If you want your macros being exportable, do not use this method.
			# Instead create module `Macros' inside your *program* class and define methods there.
			#
			# <b>Unlike labels, macros must be defined before being referenced.</b>
			#
			# <b>Be carefull with +ret+ instruction if you used +registers+.</b>
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
				raise Syntax, "Macro must have name" unless name.is_a?(Symbol)
				raise Syntax, "Macro may be defined only in main program context." if @contexts.size > 1
				raise Syntax, "A label: #{name} is already allocated." if @labels.has_key?(label.to_s)
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
			# Saves specified registers on a machine stack, evaluates block wrapped around
			# a namespace and restores registers in reverse order.
			# Returns a label pointing to the beginning of the pushes. You may optionally pass a +name+
			# to it as first argument.
			#
			# +registers+ should be one of:
			#
			# * a 16bit register to push on a stack and pop after your code, except +sp+ obviously,
			# * +:exx+ symbol to evaluate +exx+ instruction,
			# * +:ex_af+ symbol to evaluate <code>ex af, af</code> instruction,
			# * +:all+ evaluates to: +af+, +bc+, +de+, +hl+, +ix+, +iy+
			# * +:all_but_ixiy+ evaluates to: +af+, +bc+, +de+, +hl+
			# * +:ixiy:+ evaluates to: +ix+, +iy+
			#
			# +opts+ may be one of:
			#
			# * +:ret+ if the +ret+ instruction should be added after the registers have been restored.
			#          In this instance jumping to +eoc+ will effectively return from the subroutine.
			#
			# +opts+ are passed to Program.ns for a namespace around your code.
			#
			# Examples:
			#   with_saved af, de do
			#     # ... do something with af and de
			#   end
			#
			#   with_saved af, hl, :exx, hl, :exx do
			#     # ... saves and restores af, hl, hl' and swaps shadow registers back before here
			#   end
			#
			#   with_saved :bar, :all, :exx, :ex_af, :all_but_ixiy, inherit: foo, isolate: true do
			#     # saves and restores: af, bc, de, hl, ix, iy, af', bc', de', hl'
			#     # creates isolated namespace with inherited foo immediate label around here
			#     # creates bar label in the enclosing namespace to the beginning of the pushes
			#   end
			#
			# <b>Be carefull with a +ret+ instruction in your code block.</b>
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
			# Loads a content of the register +bb+ into register +aa+.
			#
			# A sugar for two 8bit ld instructions.
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
			# Compares a register pair +th+|+th+ with a 16bit +value+.
			#
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
