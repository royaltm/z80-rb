# -*- coding: BINARY -*-
module Z80
	module Program
		##
		#  Z80 registers are populated as singleton methods.
		#  You must not create instances of this class directly.
		#
		#  Available registers for use within your programs:
		#
		#    a, b, c, d, e, h, l, ixh, ixl, iyh, iyl, af, bc, de, hl, ix, iy, sp, i, r
		#
		#  There are also special registers internally used by the compiler which indicate indirect memory access:
		#
		#    bc_, de_, hl_, iy_, ix_, sp_ # do not use them in your programs
    #
		#  Instead for addressing values with registers in memory use one-element array wrapped
		#  around a 16bit register:
		#
		#    [bc], [de], [hl], [iy - 2], [ix + 1], [sp]
		#
		#  Sometimes it's desired to use arguments in macros as 8bit registers and a pair of them as 16bit register.
		#  It's possible thanks to Register#split and Register#| methods:
		#
		#    macro :foo do |_, rh, rl, tt|
		#      th, tl = tt.split
		#            		ld  rh|rl, tl
		#            		inc rh|rl
		#            		ld  rh|rl, th
		#    end
		#
		#    foo h, l, bc
		class Register
			@@names8 = %w[b c d e h l hl_ a]
			@@namesi8= %w[ixh ixl iyh iyl]
			@@names16 = %w[bc de hl sp af]
			@@names_index8 = %w[ix_ iy_]
			@@names_bcdep8 = %w[bc_ de_]
			@@names_index16 = %w[ix iy]
			@@names_other = %w[i r sp_]
			attr_reader :name, :prefix
			attr_accessor :index
			protected :index=
			class << self
				def [](index)
					@@registers[index]
				end
				def names
					@@names8 + @@namesi8 + @@names_index8 + @@names_bcdep8 + @@names16 + @@names_index16 + @@names_other
				end
				private :new
			end
			def initialize(name, opc)
				@name = name
				@index = 0
				@opc = opc & 0xff
				@bit8 = name.size != 2 and opc & 7 == opc
				@prefix = name.index(?x) ? "\xDD" : name.index(?y) ? "\xFD" : nil
			end
			def size
				name.size
			end
			def pointer?
				name[-1] == '_'
			end
			##
			#  Checks if +self+ can adjoin with +other+: +self+|+other+
			def match16?(other)
				other.is_a?(Register) and %w[bc de hl ixhixl iyhiyl].include?(name + other.name)
			end
			##
			#  Disjoins one of 16 bit registers: +bc+ +de+ +hl+ +ix+ or +iy+ to array of 8bit registers: [+hi+, +lo+].
			#
			#  Usefull when defining macros that may use registers passed by parameters.
			def split
				case name
				when 'bc', 'de', 'hl'
					[@@regindex[name[0]], @@regindex[name[1]]]
				when 'ix', 'iy'
					[@@regindex[name + 'h'], @@regindex[name + 'l']]
				else
					raise Syntax, "Only paired registers: bc, de, hl, ix, iy can be disjoined."
				end
			end
			##
			#  Adjoins two 8 bit registers to form one 16 bit register.
			#
			#  Usefull when defining macros that may use registers passed by parameters.
			def |(other)
				if match16? other
					@@regindex[name + other.name] || @@regindex[name[0,2]]
				else
					raise Syntax, "Only paired registers: bc, de, hl, ix, iy can be adjoined."
				end
			end
			##
			#  Method used internally by mnemonics to make pointer of a label or register.
			#  Example:
			#    ld  b, [ix + 2]
			#    ld  [hl], b
			def [](index = 0)
				if name.size == 2 or pointer?
					if index != 0 and name[0] != ?i
						raise Syntax, "Only ix,iy registers may be indexed pointers."
					elsif !index.respond_to?(:to_alloc) and !(-128..127).include?(index)
						raise Syntax, "Pointer index out of range."
					end
					raise Syntax, "Register #{name} can not be a pointer." unless r = @@regindex[name + '_'] or (pointer? and r = self)
					r = r.dup
					if index.respond_to?(:to_alloc) and !r.index.respond_to?(:to_alloc)
						r.index = index + r.index
					else
						r.index = r.index + index
					end
					r
				else
					raise Syntax, "Only 16-bits registers may be pointers."
				end
			end
			##
			#  This method makes possible to write indexed expressions with +ix/iy+ registers.
			#  Example:
			#    ld a, [ix + 7]
			def +(other)
				self[other]
			end
			#  This method makes possible to write indexed expressions with +ix/iy+ registers.
			#  Example:
			#    ld a, [ix - 7]
			def -(other)
				self[-other.to_i]
			end
			def to_debug
				if pointer?
					"(#{name[0,2]}" + if prefix
						'+%02xH'
					end.to_s + ')'
				else
					name
				end
			end
			def to_i; @opc; end
			def bit8?; @bit8; end
			def one_of?(ary); ary.include?(name); end
			alias_method :to_str, :name
			alias_method :to_s, :name
			@@registers = @@names8.each_with_index.map {|r, i| new r, i }
			@@registers+= @@namesi8.each_with_index.map {|r, i| new r, 4 + (i&1) }
			@@registers+= @@names_index8.map {|r| new r, 6 }
			@@registers+= @@names_bcdep8.each_with_index.map {|r, i| new r, 0x100 + (i << 4) }
			@@registers+= @@names16.each_with_index.map {|r, i| new r, [i, 3].min << 4 }
			@@registers+= @@names_index16.map {|r| new r, 2 << 4 }
			@@registers+= @@names_other.each_with_index.map {|r, i| new r, 0x200 + (i << 3) }
			@@regindex = {}
			@@registers.each {|r| @@regindex[r.name] = r}
		end
		##
		#  Creates +jr/jp/ret/call+ conditions as constants:
		#    NZ Z NC C PO PE P M
		#  You must not use this class directly. Use predefined constants instead.
		class Condition
			@@names = %w[NZ Z NC C PO PE P M]
			attr_reader :name
			class << self
				def [](index)
					@@conditions[index]
				end
				def names
					@@names.dup
				end
				private :new
			end
			def initialize(name, opc)
				@name = name
				@opc = opc
			end
			def to_i
				@opc
			end
			alias_method :to_str, :name
			alias_method :to_s, :name
			@@conditions = @@names.each_with_index.map {|r, i| new r, i << 3 }
		end
		Register.names.each_with_index do |r, i|
			module_eval "def #{r}; Register[#{i}]; end"
		end
		Condition.names.each_with_index do |r, i|
			const_set r.to_sym, Condition[i]
		end
	end
end
