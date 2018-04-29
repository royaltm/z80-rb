# -*- coding: BINARY -*-
require 'z80/registers'
require 'z80/labels'
require 'z80/mnemonics'
require 'z80/macros'
require 'z80/tap'
# ==Include this module in your *program* class to turn it to a powerfull Z80 macro assembler.
#
# ---
# <b>To fully use ruby-z80 powers (unlike austin powers)</b>
# * use labels where applicable (see Z80::Label)
# * use namespaces where appropriate (see Program.ns)
# * use macros whenever you can
# * modularize all just like in ruby :-)
# ---
#
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
	#  evaluated label values
	attr_reader :labels
	#  Returns relocated label value
	def [](label)
		@labels[label.to_s]
	end
	##
	#  Creates debugger view from instance of a program.
	#  Returns an Array of Strings.
	#
	#  Example debugger output:
	#    =============== ZXMath ===============
	#    8023:                                  :mul
	#    8023: 5D          ld   e, l
	#    8024: 54          ld   d, h
	#    8025: 218080      ld   hl, 0000H
	#    8028: CB3F        srl  a               :mul.loop1
	#    802A: 3803        jr   NC, 802fH       -> noadd
	#    802C: 19          add  hl, de
	#    802D: 380B        jr   C, 803aH
	#    802F: 2807        jr   Z, 8038H        :mul.noadd -> ok
	#    8031: CB23        sla  e
	#    8033: CB12        rl   d
	#    8035: D22880      jp   NC, 8028H       -> loop1
	#    8038: 4D          ld   c, l            :mul.ok
	#    8039: 44          ld   b, h
	#    803A: D0          ret  NC
	#    803B: CF          rst  08H
	#    803C: 0A                      .
	#    ^^^^^^^^^^^^^^^ ZXMath ^^^^^^^^^^^^^^^
	def debug
		return @debug if @debug
		reloc = self.class.reloc
		@debug = self.class.debug.map do |d|
			case d
			when DebugInfo
				data = code[d.addr, d.size]
				h = proc {|as, g| as.map {|c| '%02X'%c.ord}.join g}
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
								data[o, 2].unpack('S<')[0]
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
				[" #{@imports[d].class} ".center(38, ?=)] + @imports[d].debug + [" #{@imports[d].class} ".center(38, ?^)]
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
			if (alloc = label.to_label(prg).to_alloc).immediate? and (size == 2 or from)
				prg.reloc << Relocation.new(prg.pc + offset, alloc, 0, from) unless alloc.to_name.nil?
				[alloc.to_i(0, size == 1 ? :self : nil)].pack(size == 1 ? 'c' : 's<')
			else
				prg.reloc << Relocation.new(prg.pc + offset, alloc, size, from)
				"\x0"*size
			end
		end
	end
	module Program
		VERSION = "0.9.2"
		# raw, not relocated code
		attr_reader :code
		# relocation table
		attr_reader :reloc
		# raw, debug information
		attr_reader :debug
		# original exported labels
		attr_reader :exports#, :labels, :contexts, :dummies, :context_labels
		def self.extended(klass) # :nodoc:
			['@code', '',
			 '@reloc', [],
			 '@debug', [],
			 '@contexts', [{}],
			 '@context_labels', [],
			 '@labels', {},
			 '@dummies', [],
			 '@imports', [],
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
				raise CompileError, "Labels referenced but not defined: #{dummies.inspect}"
			end
			p = super(*args)
			c = @code.dup
			imports = @imports.map do |addr, size, program, arguments|
				ip = program.new(addr + start, *arguments)
				raise CompileError, "Imported program #{program} has been modified." unless ip.code.bytesize == size
				c[addr, size] = ip.code
				ip
			end
			eval_labels = proc {|members, prefix|
				members.map {|n, v|
					[n = prefix ? "#{prefix}.#{n}" : n, v.to_i(start)] +
					(if (m = v.instance_variable_get '@members')
						eval_labels[m, n].flatten
					end || [])
				}.flatten
			}
			labels = Hash[*eval_labels[@labels].flatten]
			@reloc.each do |r|
				case r.size
				when 0
					# ignore, this is an absolute address but we need relocation info for debug
				when 1
					addr = r.from ? r.from : r.addr + 1 + start
					i = r.alloc.to_i(start, addr)
					unless (-128..127).include?(i)
						raise CompileError, "Relative relocation out of range at 0x#{'%04x' % r.addr} -> #{i} #{r.inspect}"
					end
					c[r.addr] = [i].pack('c')
				when 2
					c[r.addr, 2] = [r.alloc.to_i(start)].pack('S<')
				else
					c[r.addr, r.size] = [r.alloc.to_i(start)].pack('Q<')[0, r.size].ljust(r.size, "\0")
				end
			end
			['@code', c,
			 '@org', start,
			 '@debug', nil,
			 '@labels', labels,
			 '@imports', imports
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
		#  Convenience method to create local macros.
		#
		#  Give a +name+ (Symbol) for macro, (optional) list of +registers+ to push before and pop after code
		#  and a block of code.
		#  The block will receive +eoc+ label (see Program.ns) and any argument you pass when calling a macro.
		#
		#  If you want your macros being exportable, instead of using this method
		#  create module `Macros' inside your *program* class and define methods there.
		#
		#  <b>Unlike labels, macros must be defined before being referenced.</b>
		#
		#  <b>Be carefull with +ret+ instruction if you used +registers+.</b>
		def macro(name, *registers, &mblock)
			raise Syntax, "Macro must have name" unless name.is_a?(Symbol)
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
		##
		#  Creates offset from Program.pc to +address+ padding it with +pad+.
		#
		#  Do not confuse it with assembler directive ORG which sets absolute address of a program.
		#  In ruby-z80 only an instances of a program have absolute addresses.
		#
		#  Returns unnamed +label+ that points to beginning of padded space.
		def org(address, pad = 0)
			address = address.to_i & 0xffff
			raise Syntax, "The current code pointer: #{pc.to_s 16} is exceeding: #{address.to_i.to_s 16} " if pc > address
			Z80::add_code self, [pad].pack('c')*(address - pc)
		end
		##
		#  Creates a isolated namespace for relocable labels defined inside your code.
		#
		#  Isolated namespace can't reference labels defined outside of it.
		#
		#  :see: Program#ns
		def isolate(name = nil, opts = {}, &block)
			opts, name = name, nil if name.is_a?(Hash)
			ns(name, {:isolate => true}.merge(opts), &block)
		end
		##
		#  Creates namespace for relocable labels defined inside your code.
		#
		#  Give a block which generates z80 code containing labels or possibly other
		#  namespaces (namespaces can be nested).
		#
		#  Give optional +name+ as a Symbol for named (labeled) namespaces.
		#
		#  Options:
		#
		#  * +:inherit+:: if +true+ or a name (String or Symbol) or a Array of names,
		#                 namespace inherits absolute labels from parent namespaces;
		#                 +false+ by default.
		#  * +:inherit_absolute+:: alias of +:inherit+
		#  * +:inherit_labels+:: alias of +:inherit+
		#  * +:isolate+:: creates isolated namespace :see: Program#isolate
		#
		#  The block receives one variable: +eoc+ which is a label pointing immediately __after__
		#  the namespaced code.
		#
		#  If +:inherit+ option is +false+ but +:isolate+ is also +false+ it's still possible to
		#  reference absolute labels but it would be impossible to coerce such label to a Integer.
		#
		#  Example:
		#    ns :foo do |eoc|
		#      loop1 add a
		#            jr C, eoc
		#            inc b
		#            jr NZ, loop1
		#    end
		#
		#  Returns (optionally named) +label+ that points to the beginning of a namespaced code.
		def ns(name = nil, opts = {})
			raise ArgumentError, "no block given to ns" unless block_given?
			opts, name = name, nil if name.is_a?(Hash)
			inherit = opts[:inherit] || opts[:inherit_absolute] || opts[:inherit_labels]
			labels = @labels
			@labels = labels.dup
			inherit = [inherit] if inherit.is_a?(Symbol) || inherit.is_a?(String) || label?(inherit)
			@contexts << if inherit.is_a?(Array)
				inherit.map do |name|
					name = if name.respond_to?(:to_name)
						name.to_name
					else
						name.to_s
					end
					labl = labels[name]
					raise CompileError, "Absolute label: `#{name}' not found" if labl.nil?
					raise CompileError, "Label from parents: `#{name}' is not absolute" unless labl.immediate?
					[name, labl]
				end.to_h
			elsif inherit == true
				labels.select{|_,v| v.immediate?}
			elsif !inherit
				{}
			else
				raise ArgumentError, "ns :inherit option must be a boolean, name or array of names"
			end
			addr = pc
			eoc = Label.dummy 'EOC'
			@context_labels << (top = Label.dummy)
			beg_debug_index = @debug.length
			beg_reloc_index = @reloc.length
			@debug << DebugInfo.new(addr, 0, '--- begin ---', nil, @context_labels.dup)
			yield eoc
			if name.nil? and @reloc[beg_reloc_index...@reloc.length].all? {|r| r.alloc != eoc}
				@debug.delete_at beg_debug_index
			else
				@debug << DebugInfo.new(pc, 0, '---  end  ---', nil, @context_labels.dup << eoc)
			end
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
			if opts[:isolate]
				unless dummies.empty?
					dummies = dummies.map {|d| d[0..1]}
					raise CompileError, "Undefined labels referenced in isolated namespace: #{dummies.inspect}"
				end
			else
				@dummies+= dummies.map do |n, l|
					if @labels.has_key?(n) and !@labels[n].dummy?
						l.reinitialize @labels[n]
						nil
					else
						[n, l] + contexts
					end
				end.compact
			end
			@context_labels.pop
			top.reinitialize addr, pc - addr, :code, Hash[members]
			top = self.send name.to_sym, top if name
			top
		end
		##
		#  Import macros from other +program+.
		#
		#  A sugar for:
		#
		#     import program, :code => false, :macros => true, :labels => false
		#
		def macro_import(program)
			import program, :code => false, :macros => true, :labels => false
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
		#  * +:args+   => program initialize arguments
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
			options = {
				:labels => true,
				:code   => true,
				:macros => false,
				:args => []
			}.merge flags
			if options[:macros]
				self.extend program::Macros if defined?(program::Macros)
			end
			if options[:labels]
				members = Hash[program.exports.map {|n, l|
					[n, l.deep_clone_with_relocation(addr)]
				}]
			end
			type = options[:code] ? program.code.bytesize : 1
			if name
				l = Label.new(addr, type, :code, members)
				self.send name.to_sym, l
			else
				members.each {|n, m| self.send n.to_sym, m} if members
				l = Label.new addr, type, :code
			end
			if options[:code]
				@debug << DebugInfo.new(addr, 0, nil, nil, @context_labels.dup << l)
				@debug << @imports.size
				@imports << [addr, type, program, options[:args]]
				@code << program.code
			end
			program.freeze
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
