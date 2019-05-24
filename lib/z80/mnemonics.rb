# -*- coding: BINARY -*-
module Z80
	module Program
		#  ==Z80 Mnemonics
		#  All Z80 instructions are created as singleton methods.
		#  They produce machine code which is added to the *Program* class instance variable: Program.code.
		#
		#  These mnemonics mimic standard ones (used in assemblers) but with some exceptions (because of ruby parser spec.):
		#
		#  * all instruction and register names are lower case
		#  * all conditions are upper case e.g.:
		#       jp NZ, somewhere
		#    use [ ] instead of ( ) e.g.: 
		#       ld a, [de]
		#       ex [sp], hl
		#       ld [ix+index], c
		#       ld a, [label_name]
		#  * use +anda+ instead of +and+
		#  * use +ora+  instead of +or+
		#  * use +inp+  instead of +in+
		#  * use <code>ex af,af</code> instead of <code>ex af,af'</code> (no apostrophe after +af+')
		#
		#  The ones below are due to my lazyness:
		#
		#  * use +im0+, +im01+, +im1+, +im2+  instead of +im+ +n+.
		#  * do not use [ ] around +out+ and +inp+ instructions' arguments e.g.:
		#      out c, a    # ok
		#      out (c), a  # () are ok, ruby doesn't care much about them
		#    <i>however ruby emits warning: (...) interpreted as grouped expression</i>
		#      # only Integers are allowed here; no labels!
		#      inp a, (0xfe)
		#
		#  * do not use [ ] around JP (HL/IX/IY) e.g.:
		#      jp  hl     # ok
		#      jp  (hl)   # () are ok, ruby doesn't care much about them
		#    <i>however ruby emits warning: (...) interpreted as grouped expression</i>
		#
		#
		#  ===Undocumented (by Zilog) Z80 instructions
		#  See: http://www.z80.info/z80undoc.htm
		#
		#  * +sll+ (or +sl1+)
		#    which means shift left <code>(a b c d e h l [hl] [ixn])</code> by one and set it's bit 0 to 1
		#
		#  * 2 in 1 instructions like
		#      set 4, [ix+n], b
		#    which means:
		#      set 4, [ix+n]; ld b, [ix+n]
		#    but is a single instruction only.
		#    It applies to +ix+/+iy+ indexing registers and following instructions (without +bit+):
		#      rlc rrc rl rr sla sra sll srl set res
		#
		#  * +ixh/ixl/iyh/iyl+ 8-bit registers of 16bit +ix/iy+
		#    use them replacing +l+ and +h+ registers in:
		#      ld inc dec add adc sbc sub anda xor ora cp
		#    however you can't mix (+ixh+ +ixl+)/(+iyh+ +iyl+)/+h+/+l+ registers in one instruction like:
		#      ld ixh, iyh # invalid
		#      ld h, iyh   # invalid
		#
		#  * and the less usefull but included for completeness:
		#      out (c), 0
		#      inp (c)
		#    the last one only adjust Z80 flags but the result goes to +/dev/null+ (figuratively speaking).
		#  
		#  ==Mnemonic list
	    #  <code>adc add anda bit call ccf cp cpd cpdr cpi cpir cpl daa dec di djnz ei ex exx
		#  halt hlt im0 im01 im1 im2 inc ind indr ini inir inp jp jr ld ldd lddr ldi ldir neg
        #  nop ora otdr otir out outd outi pop push res ret reti retn rl rla rlc rlca rld rr
		#  rra rrc rrca rrd rst sbc scf set sl1 sla sll sra srl sub xor</code>
		#
		module Mnemonics
			# :stopdoc:
			def ret(cc = nil)
				op = case cc
				when Condition
					tt = "  #{cc}"
					0xC0 + cc.to_i
				else
					raise Syntax, "Invalid condition for ret." unless cc.nil?
					0xC9
				end.chr
				Z80::add_code self, op, 1, "ret#{tt}"
			end
			def djnz(dd)
				dd = self[dd] if dd.is_a?(Array)
				op = "\x10" + if dd.respond_to? :to_label
					Z80::add_reloc self, dd, 1, 1
				else
					raise Syntax, "Immediate jump is out of an 8bit range" unless (-128..127).include?(dd.to_i)
					[dd.to_i].pack('c')
				end
				Z80::add_code self, op, 1, "djnz %04xH", 1, :pcrel
			end
			def jr(cc, dd = nil)
				cc = self[cc] if cc.is_a?(Array)
				dd = self[dd] if dd.is_a?(Array)
				op = case cc
				when Condition
					if cc.jr_ok?
						tt = "#{cc}, "
						0x20 + cc.to_i
					else
						raise Syntax, "Invalid condition for jr: #{cc}" if cc.to_i > 0x18
					end
				else
					dd, tt = cc, nil
					0x18
				end.chr
				op+= if dd.respond_to? :to_label
					Z80::add_reloc self, dd, 1, 1
				else
					raise Syntax, "Immediate jump is out of an 8bit range" unless (-128..127).include?(dd.to_i)
					[dd.to_i].pack('c')
				end
				Z80::add_code self, op, 1, "jr   #{tt}%04xH", 1, :pcrel
			end
			def jp(cc, nn = nil)
				cc = self[cc] if cc.is_a?(Array)
				nn = self[nn] if nn.is_a?(Array)
				op = case cc
				when Condition
					tt = "#{cc}, "
					(0xC2 + cc.to_i).chr
				when Register
					op = if cc.one_of? %w[hl ix iy]
						"\xE9"
					else
						raise Syntax, "Invalid register for jp: #{cc}"
					end
					op = cc.prefix + op if cc.prefix
					return Z80::add_code self, op, 1, "jp   (#{cc})"
				else
					nn, tt = cc, nil
					"\xC3"
				end
				op+= if nn.respond_to? :to_label
					Z80::add_reloc self, nn, 2, 1
				else
					[nn.to_i].pack('S<')
				end
				Z80::add_code self, op, 1, "jp   #{tt}%04xH", 1, :word
			end
			def call(cc, nn = nil)
				cc = self[cc] if cc.is_a?(Array)
				nn = self[nn] if nn.is_a?(Array)
				op = case cc
				when Condition
					tt = "#{cc}, "
					(0xC4 + cc.to_i).chr
				when Register
					raise Syntax, "Invalid argument for call: #{cc}"
				else
					nn, tt = cc, nil
					"\xCD"
				end
				op+= if nn.respond_to? :to_label
					Z80::add_reloc self, nn, 2, 1
				else
					[nn.to_i].pack('S<')
				end
				Z80::add_code self, op, 1, "call #{tt}%04xH", 1, :word
			end
			def rst(vec)
				v = vec.to_i
				op = 0xC7 + if v & 7 == v
					v<<= 3
				elsif v & 0x38 == v
					v
				else
					raise Syntax, "Invalid rst vector: #{vec}"
				end
				Z80::add_code self, op.chr, 1, "rst  #{'%02xH'%v}"
			end
			%w[push pop].zip([0xC5, 0xC1]).each do |n, v|
				define_method(n.to_sym) do |rr|
					rr = self[rr] if rr.is_a?(Array)
					op = if rr.is_a?(Register) and rr.one_of?(%w[bc de hl af ix iy])
						(v + rr.to_i).chr
					else
						raise Syntax, "Invalid register for push: #{rr}"
					end
					op = rr.prefix + op if rr.prefix
					Z80::add_code self, op, 1, "#{n.ljust 4} #{rr}"
				end
			end
			%w[sub anda xor ora cp].zip([0x90, 0xA0, 0xA8, 0xB0, 0xB8]).each do |n, v|
				define_method(n.to_sym) do |rr|
					rr = self[rr] if rr.is_a?(Array)
					op = case rr
					when Register
						raise Syntax, "Invalid register for #{n}: #{rr}" unless rr.bit8?
						tt = rr.to_debug
						if rr.prefix and rr.pointer?
							i = if (i = rr.index).respond_to?(:to_label)
								Z80::add_reloc self, i, 1, 2, :self
							else
								[i].pack('c')
							end
							tta = [2, :index]
							rr.prefix + (v + rr.to_i).chr + i
						else
							rr.prefix.to_s + (v + rr.to_i).chr
						end
					when Condition
						raise Syntax, "Invalid parameter for #{n}: #{rr}"
					else
						(v + 0x46).chr + if rr.respond_to?(:to_label)
							tt, *tta = "%02xH", 1, :byte
							Z80::add_reloc self, rr, 1, 1, 0
						else
							tt = "#{'%02xH' % rr.to_i}"
							[rr].pack('c')
						end
					end
					Z80::add_code self, op, 1, "#{n.gsub(/a$/,'').ljust 4} #{tt}", *tta
				end
			end
			[['add', 0x80, %w[hl ix iy], 0x09, ''],
			 ['adc', 0x88, %w[hl],       0x4A, "\xED"],
			 ['sbc', 0x98, %w[hl],       0x42, "\xED"]].each do |n, v, names, vv, pfix|
				define_method(n.to_sym, ->(aa, bb = nil) do
					aa = self[aa] if aa.is_a?(Array)
					bb = self[bb] if bb.is_a?(Array)
					if aa.is_a?(Register) and aa.one_of?(names)
						op = if bb.is_a?(Register) and (bb.one_of?(%w[bc de sp]) or aa == bb)
							tt = bb.to_debug
							aa.prefix.to_s + pfix + (vv + bb.to_i).chr
						else
							raise Syntax, "Invalid argument for add #{aa}: #{bb}"
						end
					elsif aa == a or bb.nil?
						bb, aa = aa, a unless bb
						op = case bb
						when Register
							raise Syntax, "Invalid register for #{n}: #{bb}" unless bb.bit8?
							tt = bb.to_debug
							if bb.prefix and bb.pointer?
								i = if (i = bb.index).respond_to?(:to_label)
									Z80::add_reloc self, i, 1, 2, :self
								else
									[i].pack('c')
								end
								tta = [2, :index]
								bb.prefix + (v + bb.to_i).chr + i
							else
								bb.prefix.to_s + (v + bb.to_i).chr
							end
						when Condition
							raise Syntax, "Invalid parameter for #{n}: #{bb}."
						else
							(v + 0x46).chr + if bb.respond_to?(:to_label)
								tt, *tta = "%02xH", 1, :byte
								Z80::add_reloc self, bb, 1, 1, 0
							else
								tt = "#{'%02xH' % bb.to_i}"
								[bb].pack('c')
							end
						end
					else
						raise Syntax, "Invalid register for #{n}: #{aa}."
					end
					Z80::add_code self, op, 1, "#{n}  #{aa.to_debug}, #{tt}", *tta
				end)
			end
			%w[rlc rrc rl rr sla sra sll srl].each_with_index do |n, v|
				v<<= 3
				define_method(n.to_sym, ->(rr, bb = nil) do
					rr = self[rr] if rr.is_a?(Array)
					bb = self[bb] if bb.is_a?(Array)
					op = case rr
					when Register
						raise Syntax, "Invalid register for #{n}: #{rr}." if !rr.bit8? or (!rr.pointer?&rr.prefix)
						tt = rr.to_debug
						if rr.prefix and rr.pointer?
							if bb
								raise Syntax, "Invalid second argument for #{n}: #{bb}." unless bb.bit8?
								tt+= " -> #{bb.to_debug}"
							else
								bb = rr
							end
							i = if (i = rr.index).respond_to?(:to_label)
								Z80::add_reloc self, i, 1, 2, :self
							else
								[i].pack('c')
							end
							tta = [2, :index]
							rr.prefix.to_s + "\xCB" + i + (v + bb.to_i).chr
						else
							raise Syntax, "Invalid second argument for #{n}: #{bb}." if bb
							"\xCB" + (v + rr.to_i).chr
						end
					else
						raise Syntax, "Invalid parameter for #{n}: #{rr}."
					end
					Z80::add_code self, op, 1, "#{n.ljust 3}  #{tt}", *tta
				end)
			end
			%w[bit res set].each_with_index do |n, v|
				v = v + 1 << 6
				define_method(n.to_sym, ->(bit, rr, bb = nil) do
					rr = self[rr] if rr.is_a?(Array)
					bb = self[bb] if bb.is_a?(Array)
					tt = "#{bit = bit.to_i & 7}, "
					bit<<= 3
					op = case rr
					when Register
						raise Syntax, "Invalid register for #{n}: #{rr}." if !rr.bit8? or (!rr.pointer?&rr.prefix)
						tt+= rr.to_debug
						if rr.prefix and rr.pointer?
							if bb
								raise Syntax, "Invalid third argument for #{n}: #{bb}." unless bb.bit8?
								tt+= " -> #{bb.to_debug}"
							else
								bb = rr
							end
							i = if (i = rr.index).respond_to?(:to_label)
								Z80::add_reloc self, i, 1, 2, :self
							else
								[i].pack('c')
							end
							tta = [2, :index]
							rr.prefix.to_s + "\xCB" + i + (v + bit + bb.to_i).chr
						else
							raise Syntax, "Invalid third argument for #{n}: #{bb}." if bb
							"\xCB" + (v + bit + rr.to_i).chr
						end
					else
						raise Syntax, "Invalid parameter for #{n}: #{rr}."
					end
					Z80::add_code self, op, 1, "#{n}  #{tt}", *tta
				end)
			end
			alias_method :sl1, :sll
			['inc', 0x04, 0x03, 'dec', 0x05, 0x0B].each_slice(3) do |n, v, vv|
				define_method(n.to_sym) do |rr|
					rr = self[rr] if rr.is_a?(Array)
					op = case rr
					when Register 
						tt = rr.to_debug
						if rr.bit8?
							if rr.prefix and rr.pointer?
								i = if (i = rr.index).respond_to?(:to_label)
									Z80::add_reloc self, i, 1, 2, :self
								else
									[i].pack('c')
								end
								tta = [2, :index]
								rr.prefix + (v + (rr.to_i << 3)).chr + i
							else
								rr.prefix.to_s + (v + (rr.to_i << 3)).chr
							end
						elsif rr.one_of?(%w[bc de hl sp ix iy])
							rr.prefix.to_s + (vv + rr.to_i).chr
						else
							raise Syntax, "Invalid register for #{n}: #{rr}."
						end
					else
						raise Syntax, "Invalid parameter for #{n}: #{rr}."
					end
					Z80::add_code self, op, 1, "#{n}  #{tt}", *tta
				end
			end
			def ld(aa, bb)
				aa = self[aa] if aa.is_a?(Array)
				bb = self[bb] if bb.is_a?(Array)
				op = if aa.is_a?(Register) and bb.is_a?(Register)
					tt = "#{aa.to_debug}, #{bb.to_debug}"
					if aa.one_of?(%w[bc_ de_])
						raise Syntax, "Invalid second argument for ld." unless bb == a
						(0x02 + aa.to_i).chr
					elsif bb.one_of?(%w[bc_ de_])
						raise Syntax, "Invalid first argument for ld." unless aa == a
						(0x0A + bb.to_i).chr
					elsif aa == sp
						raise Syntax, "Invalid second argument for ld." unless bb.one_of? %w[hl ix iy]
						bb.prefix.to_s + "\xF9"
					elsif aa.one_of?(%w[i r])
						raise Syntax, "Invalid second argument for ld." unless bb == a
						"\xED" + (0x47 + aa.to_i).chr
					elsif bb.one_of?(%w[i r])
						raise Syntax, "Invalid first argument for ld." unless aa == a
						"\xED" + (0x57 + bb.to_i).chr
					elsif aa.bit8? and bb.bit8?
						if (aa.pointer? and bb.pointer?) or
							(aa.pointer? and bb.prefix) or 
							(aa.prefix and bb.pointer?) or
							(aa.prefix and bb.prefix and aa.prefix != bb.prefix)
							raise Syntax, "Invalid arguments for ld." 
						end
						if aa.prefix or bb.prefix
							rr = aa.prefix ? aa : bb
							if aa.pointer? or bb.pointer?
								i = if (i = rr.index).respond_to?(:to_label)
									Z80::add_reloc self, i, 1, 2, :self
								else
									[i].pack('c')
								end
								tta = [2, :index]
								rr.prefix + (0x40 + (aa.to_i << 3) + bb.to_i).chr + i
							elsif (aa.prefix and bb.one_of? %w[h l]) or (bb.prefix and aa.one_of? %w[h l])
								raise Syntax, "Can not mix ix/iy with h/l registers."
							else
								rr.prefix + (0x40 + (aa.to_i << 3) + bb.to_i).chr
							end
						else
							(0x40 + (aa.to_i << 3) + bb.to_i).chr
						end
					else
						raise Syntax, "Invalid arguments for ld."
					end
				elsif aa.is_a?(Register) and !bb.is_a?(Condition)
					tt = "#{aa.to_debug}, "
					if aa.bit8?
						if bb.respond_to?(:to_label) and bb.pointer?
							raise Syntax, "Invalid ld from immediate adres to: #{aa}." unless aa == a
							tt+= "(%04xH)"
							tta = [1, :word]
							"\x3A" + Z80::add_reloc(self, bb, 2, 1)
						else
							raise Syntax, "Invalid first argument for ld." if aa.one_of? %w[i r]
							op = if aa.prefix and aa.pointer?
								i = if (i = aa.index).respond_to?(:to_label)
									Z80::add_reloc self, i, 1, 2, :self
								else
									[i].pack('c')
								end
								tta = [2, :index]
								aa.prefix + (0x06 + (aa.to_i << 3)).chr + i
							else
								tta = []
								aa.prefix.to_s + (0x06 + (aa.to_i << 3)).chr
							end
							op + if bb.respond_to?(:to_label)
								tt+= "%02xH"
								tta+= [op.bytesize, :byte]
								Z80::add_reloc self, bb, 1, op.bytesize, 0
							else
								bb = bb.to_i & 0xff
								tt+= "#{'%02xH' % bb}"
								[bb].pack('c')
							end
						end
					elsif aa.one_of? %w[bc de hl sp ix iy]
						if bb.respond_to?(:to_label) and bb.pointer?
							tt+= "(%04xH)"
							if aa.one_of? %w[hl ix iy]
								tta = [1 + (aa.prefix ? 1 : 0), :word]
								aa.prefix.to_s + "\x2A" + Z80::add_reloc(self, bb, 2, 1 + (aa.prefix ? 1 : 0))
							else
								tta = [2, :word]
								"\xED" + (0x4B + aa.to_i).chr + Z80::add_reloc(self, bb, 2, 2)
							end
						else
							nn = if bb.respond_to? :to_label
								tt+= "%04xH"
								tta = [1 + (aa.prefix ? 1 : 0), :word]
								Z80::add_reloc self, bb, 2, 1 + (aa.prefix ? 1 : 0)
							else
								tt+= "#{'%04xH' % (bb.to_i & 0xffff)}"
								[bb.to_i].pack('s<')
							end
							aa.prefix.to_s + (0x01 + aa.to_i).chr + nn
						end
					else
						raise Syntax, "Invalid arguments for ld."
					end
				elsif aa.respond_to?(:to_label) and aa.pointer? and bb.is_a?(Register)
					tt = "(%04xH), #{bb.to_debug}"
					if bb == a
						tta = [1, :word]
						"\x32" + Z80::add_reloc(self, aa, 2, 1)
					elsif bb.one_of? %w[hl ix iy]
						tta = [1 + (bb.prefix ? 1 : 0), :word]
						bb.prefix.to_s + "\x22" + Z80::add_reloc(self, aa, 2, 1 + (bb.prefix ? 1 : 0))
					elsif bb.one_of? %w[bc de sp]
						tta = [2, :word]
						"\xED" + (0x43 + bb.to_i).chr + Z80::add_reloc(self, aa, 2, 2)
					else
						raise Syntax, "Invalid arguments for ld."
					end
				else
					raise Syntax, "Invalid arguments for ld."
				end
				Z80::add_code self, op, 1, "ld   #{tt}", *tta
			end
			def ex(aa, bb)
				aa = self[aa] if aa.is_a?(Array)
				bb = self[bb] if bb.is_a?(Array)
				raise Syntax, "Invalid arguments for ex." unless aa.is_a?(Register) and bb.is_a?(Register)
				tt = aa.to_debug + ", " + bb.to_debug
				op = if aa == de and bb == hl
					"\xEB"
				elsif aa == af and bb == af
					tt+= "'"
					"\x08"
				elsif aa.to_s == 'sp_' and bb.one_of?(%w[hl ix iy])
					bb.prefix.to_s + "\xE3"
				else
					raise Syntax, "Invalid register for ex."
				end
				Z80::add_code self, op, 1, "ex   #{tt}"
			end
			def out(oo, rr)
				rr = self[rr] if rr.is_a?(Array)
				op = case oo
				when c
					tta = []
					if rr == 0
						tt = "(c), 0"
						"\xED\x71"
					else
						raise Syntax, "Invalid register argument for out (c)." unless rr.is_a?(Register) and rr.one_of? %w[a b c d e h l]
						tt = "(c), " + rr.to_debug
						"\xED" + (0x41 + (rr.to_i << 3)).chr
					end
				else
					raise Syntax, "Invalid register argument for: out (n)." unless rr == a
					"\xD3" + if oo.respond_to?(:to_label)
						tt = "(%02xH), a"
						tta = [1, :byte]
						Z80::add_reloc self, oo, 1, 1, 0
					else
						oo = oo.to_i & 0xff
						tt = "(#{'%02xH' % oo}), a"
						tta = []
						[oo].pack('C')
					end
				end
				Z80::add_code self, op, 1, "out  #{tt}", *tta
			end
			def inp(rr, oo = nil)
				rr = self[rr] if rr.is_a?(Array)
				oo, rr = rr, nil if oo.nil?
				op = case oo
				when c
					tta = []
					if rr.nil?
						tt = "f, (c)"
						"\xED\x70"
					else
						raise Syntax, "Invalid register argument for in (c)." unless rr.is_a?(Register) and rr.one_of? %w[a b c d e h l]
						tt = rr.to_debug + ", (c)"
						"\xED" + (0x40 + (rr.to_i << 3)).chr
					end
				else
					raise Syntax, "Invalid register argument for in (n)." unless rr == a
					"\xDB" + if oo.respond_to?(:to_label)
						tt = "a, (%02xH)"
						tta = [1, :byte]
						Z80::add_reloc self, oo, 1, 1, 0
					else 
						oo = oo.to_i & 0xff
						tt = "a, (#{'%02xH' % oo})"
						tta = []
						[oo].pack('C')
					end
				end
				Z80::add_code self, op, 1, "in   #{tt}", *tta
			end
			['nop', "\x00",
			'rlca', "\x07",
			'rrca', "\x0F",
			'rla',  "\x17",
			'rra',  "\x1F",
			'daa',  "\x27",
			'cpl',  "\x2F",
			'scf',  "\x37",
			'ccf',  "\x3F",
			'hlt',  "\x76",
			'halt', "\x76", # alias of the above some assemblers use this form, some other the above
			'exx',  "\xd9",
			'di',   "\xF3",
			'ei',   "\xFB",
			'rld',	"\xED\x6F",
			'rrd',  "\xED\x67",
			'neg',  "\xED\x44",
			'reti', "\xED\x4D",
			'retn', "\xED\x45",
			'im0',  "\xED\x46",
			'im01', "\xED\x4E",
			'im1',  "\xED\x56",
			'im2',  "\xED\x5E",
			'cpi',  "\xED\xA1",
			'cpd',  "\xED\xA9",
			'cpir', "\xED\xB1",
			'cpdr', "\xED\xB9",
			'ldi',  "\xED\xA0",
			'ldd',  "\xED\xA8",
			'ldir', "\xED\xB0",
			'lddr', "\xED\xB8",
			'ini',  "\xED\xA2",
			'ind',  "\xED\xAA",
			'inir', "\xED\xB2",
			'indr', "\xED\xBA",
			'outi', "\xED\xA3",
			'outd', "\xED\xAB",
			'otir', "\xED\xB3",
			'otdr', "\xED\xBB"].each_slice(2) do |n, v|
				define_method(n.to_sym) {
					Z80::add_code self, v, 1, n
				}
			end
		end
		include Mnemonics
	end
end
