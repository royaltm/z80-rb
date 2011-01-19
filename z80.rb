# -*- coding: BINARY -*-
require 'z80/registers'
require 'z80/labels'
require 'z80/mnemonics'
require 'z80/tap'
#===Include this module in your *program* class to turn it to a powerfull Z80 macro assembler.
module Z80
	Relocation = ::Struct.new :addr, :alloc, :size, :from
	DebugInfo  = ::Struct.new :addr, :size, :text, :args, :labels
	#  Error raised during program parsing.
	class Syntax < StandardError; end
	#  Error raised during program compilation (while creating instance).
	class CompileError < StandardError; end
	#  program's compiled code
	attr_reader :code
	#  starting address of compiled code
	attr_reader :org
	#  labels exported with Program.export instruction
	attr_reader :exports
	##
	#  Creates debugger view from instance of a program.
	#  Returns an Array of Strings.
	#
	#  Example debugger output:
	#    016F:                                  :gfx
	#    =============== ZXGfx8 ===============
	#    016F:                                  :draw_sprite8_coords
	#    016F: 08          ex   af, af'
	#    0170: 7A          ld   a, d
	#    0171: B7          or   a
	#    0172: CA9001      jp   Z, 0190H        -> vnext1
	#    0175: 3C          inc  a
	#    0176: C0          ret  NZ
	#    0177: 08          ex   af, af'
	#    0178: F5          push af
	#    0179: ED44        neg
	#    017B: 93          sub  e
	#    017C: DA8101      jp   C, 0181H        -> vnext2
	#    017F: F1          pop  af
	#    0180: C9          ret
	def debug
		return @debug if @debug
		reloc = self.class.reloc
		@debug = self.class.debug.map do |d|
			case d
			when DebugInfo
				data = code[d.addr, d.size]
				h = proc {|as, g| as.map {|c| '%02X'%c.ord}.join g}
				# puts d.labels.inspect
				label = if d.labels.last.to_name
					" :" + d.labels.map {|n| n.to_name || n.to_i(org).to_s(16) }.join('.')
				end
				unless (relocs = reloc.select {|r| (d.addr...d.addr+d.size).member? r.addr }.map {|r| r.alloc.to_s}.join(', ')).empty?
					label = label.to_s + " -> #{relocs}"
				end
				if mnemo = d.text
					if prm = d.args
						mnemo = mnemo % prm.each_slice(2).map do |o, t|
							case t
							when :word
								data[o, 2].unpack('S')[0]
							when :byte
								data[o, 1].unpack('C')[0]
							when :pcrel
								data[o, 1].unpack('c')[0] + d.addr + d.size + org
							end
						end
					end
					"#{'%04X'%(d.addr+org)}: #{h[data.split(''),''].ljust 12}#{mnemo.ljust 20}#{label}"
				elsif data.empty? and label
					"#{'%04X'%(d.addr+org)}: #{' '*32}#{label}"
				else
					a = d.addr - 8 + org
					data.split('').each_slice(8).map do |ds|
						a+= 8
						"#{'%04X'%a}: #{h[ds, ' '].ljust 24}#{ds.join.gsub(/[\x0-\x1f]/, '.').ljust 8}#{label}"
					end
				end
			else
				d
			end
		end.flatten
	end
	class << self
		def included(klass) # :nodoc:
			klass.extend Program
      if defined?(klass::Macros)
        klass.extend klass::Macros
      end
		end
		##
		#  Method used by Program instructions was placed here to not pollute *program* namespace
		def add_code(prg, data, type = 1, mnemo = nil, *mpar)
			raise TypeError unless data.is_a? String
			l = Label.new(pc = prg.pc, type, :code)
			prg.debug << DebugInfo.new(pc, data.bytesize, mnemo, mpar, prg.instance_variable_get('@context_labels').dup << l)
			prg.code << data
			l
		end
		##
		#  Method used by Program instructions was placed here to not pollute *program* namespace
		def add_reloc(prg, label, size, offset = 0, from = nil)
			if (label = label.to_label(prg).to_alloc).immediate? and (size == 2 or from)
				[label.to_i(0, size == 1 ? :self : nil)].pack(size == 1 ? 'c' : 's')
			else
				prg.reloc << (r = Relocation.new(prg.pc + offset, label, size, from))
				"\x0"*size
			end
		end
	end
	module Program
		# raw, not relocated code
		attr_reader :code
		# relocation table
		attr_reader :reloc
		# raw, debug information
		attr_reader :debug#, :labels, :exports, :contexts, :dummies, :context_labels
		def self.extended(klass) # :nodoc:
			['@code', '',
			 '@reloc', [],
			 '@debug', [],
			 '@contexts', [{}],
			 '@context_labels', [],
			 '@labels', {},
			 '@dummies', [],
			 '@exports', {},
			 '@autoexport', false].each_slice(2) do |n,v|
				klass.instance_variable_set n, v
			end
			constants.each do |c|
				klass.const_set c, const_get(c) unless c == :Macros
			end
		end
		##
		#  Compiles *program* at +start+ address passing *args to initialize().
		#  Returns compiled instance of a *program*.
		def new(start = 0x0000, *args)
			unless @dummies.empty? and !@contexts.last.any? {|_,v| v.dummy?}
				dummies = @dummies.map {|d| d[0..1]} + @contexts.last.select {|_,v| v.dummy?}.map {|d| d[0..1]}
				raise CompileError, "Variables not initialized:#{dummies.inspect}"
			end
			p = super
			c = @code.dup
			@reloc.each do |r|
				case r.size
				when 1
					addr = r.from ? r.from : r.addr + 1 + start
					unless (-128..127).include?(i = r.alloc.to_i(start, addr))
						raise CompileError, "Relative relocation out of range at 0x#{'%04x' % r.addr} -> #{i}"
					end
					c[r.addr] = [i].pack('c')
				when 2
					c[r.addr, 2] = [r.alloc.to_i(start)].pack('S')
				end
			end
			['@code', c,
			 '@org', start,
			 '@debug', nil,
			 '@exports', Hash[@exports.map {|n, l| 
					[n, l.deep_clone_with_relocation(start)]
				}]
			].each_slice(2) do |n,v|
				p.instance_variable_set n,v
			end
			p
		end
		##
		#  Current program counter relative to 0.
		def pc
			@code.bytesize
		end
		##
		#  Convenience method to create macros.
		#  Give a +name+ (Symbol) for macro, (optional) list of +registers+ to push before and pop after code
		#  and a block of code.
		#  The block will receive +eoc+ label (see Program.ns) and any argument you pass when calling a macro.
		#
		#  If you want your macros being exportable, instead of using this method
    #  create module `Macros' inside your *program* class and define methods there.
		#
		#  <b>Unlike labels, macros must be defined before being referenced.</b>
		#
		#  <b>Be carefull with +ret+ instruction inside code if you used +registers+.</b>
		def macro(name, *registers, &mblock)
			raise Syntax, "Macro may be defined only in main program context." if @contexts.size > 1
			raise Syntax, "A label: #{name} is already allocated." if @labels.has_key?(label.to_s)
			m = lambda do |*args, &block|
				if args.first.is_a?(Symbol)
					n = args.shift
				end
				ns(n) do |eoc|
					registers.each {|rr| push rr}
					mblock.call eoc, *args, &block
					registers.reverse.each {|rr| pop rr}
				end
			end
			define_singleton_method(name.to_sym, &m)
		end
		#  Creates offset from Program.pc to +address+ padding it with +pad+.
		#  Do not confuse it with assembler directive ORG which sets absolute address of program.
		#  In ruby-z80 only an instances of a program have absolute addresses.
		#
		#  Returns unnamed +label+ that points to beginning of padded space.
		def org(address, pad = 0)
			address = address.to_i & 0xffff
			raise Syntax, "The current code pointer: #{pc.to_s 16} is exceeding: #{address.to_i.to_s 16} " if pc > address
			Z80::add_code self, [pad].pack('c')*(address - pc)
		end
		#  Creates namespace for labels defined inside code.
		#  Give a block of code containing labels or other namespaces (namespaces can be nested).
		#  Give (optional) +name+ as Symbol for named (labeled) namespaces.
		#  The block receives one variable: +eoc+ which is a label pointing to an end of namespace code.
		#
		#  Example:
		#    ns :foo do |eoc|
		#      loop1 add a
		#            jr C, eoc
		#            inc b
		#            jr NZ, loop1
		#    end
		#  Returns (optionally named) +label+ that points to beginning of code.
		def ns(name = nil)
			labels = @labels
			@labels = @labels.dup #labels.merge @contexts.last
			@contexts << {}
			raise ArgumentError, "no block in ns" unless block_given?
			addr = pc
			eoc = Label.dummy
			@context_labels << (top = Label.dummy)
			@debug << DebugInfo.new(addr, 0, nil, nil, @context_labels.dup)
			yield eoc
			eoc.reinitialize(pc, 1, :code)
			members, dummies = @contexts.pop.partition do |n, l|
				if l.dummy?
					false
				else
					true
				end
			end
			contexts = @contexts.map(&:object_id)
			@labels = labels
			@dummies+= dummies.map do |n, l|
				if @labels.has_key?(n) and !@labels[n].dummy?
					l.reinitialize @labels[n]
					nil
				else
					[n, l] + contexts
				end
			end.compact
			@context_labels.pop
			top.reinitialize addr, pc - addr, :code, Hash[members]
			top = self.send name.to_sym, top if name
			top
		end
		##
		#  Import code, labels and macros from other +program+.
		#  Give (optional) +name+ for namespace.
		#  Without +name+ labels from +program+ will be defined in current namespace.
		#  Pass +program+ class (not an instance!).
		#  Give flags to choose what to import from +program+:
		#  * +:labels+ => +true/false+ (default: +true+)
		#  * +:code+   => +true/false+ (default: +true+)
		#  * +:macros+ => +true/false+ (default: +false+)
		#  
		#  To be able to import *macros* create module `Macros'
		#  inside +program+ class and put methods there. They will be imported as macros.
    #  <b>In such a method always wrap your code inside #ns.</b>
		#
		#  Returns (optionally named) +label+ that points to beginning of imported code.
		def import(name, program = nil, flags = {})
			unless name.is_a?(Symbol)
				flags, program, name = program, name, nil
			end
			addr = pc
			p = program.new addr
			options = {
				:labels => true,
				:code	=> true,
				:macros => false,
			}.merge flags
			if options[:macros]
				self.extend program::Macros if defined?(program::Macros)
			end
			if options[:labels]
				members = p.exports.dup
			end
			if name
				l = Label.new(addr, 1, :code, members)
				self.send name.to_sym, l
			else
				members.each {|n, m| self.send n.to_sym, m} if members
				l = Label.new addr, 1, :code
			end
			if options[:code]
				@debug << DebugInfo.new(addr, 0, nil, nil, @context_labels.dup << l)
				@debug << [" #{program} ".center(38, ?=)] + p.debug + [" #{program} ".center(38, ?^)]
				@code << p.code
			end
			l
		end
		##
		#  Import binary file.
		#  * +file+ is a filename.
		#  * +type+ specifies format of binary file (as Symbol),
		#    if +:any+ -> format will be determined by filename's extension.
		#
		#  <b>Currently only :tap format is supported and only,
		#  if you include Z80::TAP in your program</b>.
		#
		#  If format is not known, file is being imported as binary.
		#
		#  Returns unnamed +label+ that points to beginning of imported data.
		def import_file(file, type = :any, size = nil, args = {})
			type = type.to_s.upcase.to_sym
			if type == :ANY
				type = File.extname(file).gsub(/^\./,'').upcase.to_sym
			end
			data = if Z80.constants.include?(type) and (handler = Z80.const_get(type)) and (handler.respond_to? :read_data)
				$stderr.puts "Importing #{type} file: `#{file}'."
				handler.read_data(file, args)
			else
				$stderr.puts "Importing binary file: `#{file}'."
				File.open(file, 'rb') {|f| f.read}
			end
			Z80::add_code self, if size
				data[0, size].ljust(size, "\x0")
			else
				data
			end, size || data.bytesize
		end
	end
	#  some useless stuff; needed for z80 opcode testing; but didn't delete it (maybe will come in handy)
	module Helpers
		def neg(a); -a & 0xff; end
		def cpl(a); ~a & 0xff; end
		def hb(a); '%02x'%(a & 0xff) ; end
		def h(a); '%04x'%(a & 0xffff) ; end
	end
end
extend Z80::Helpers
