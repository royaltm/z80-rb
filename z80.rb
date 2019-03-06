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
	#  The program's compiled code.
	attr_reader :code
	#  The starting address of the compiled code.
	attr_reader :org
	#  Evaluated label values of the compiled program.
	attr_reader :labels
	#  Compiled imported code modules.
	attr_reader :imports
	#  Returns the relocated label value.
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
		imports = @imports.values
		@debug = self.class.debug.map do |d|
			case d
			when DebugInfo
				data = code[d.addr, d.size]

				hxd = proc {|as, g| as.map {|c| '%02X'%c.ord}.join g}

				label = if d.labels.last.to_name
					" :" + d.labels.map {|n| n.to_name || n.to_i(org).to_s(16) }.join('.')
				end

				relocs = reloc.select {|r| (d.addr...d.addr+d.size).member? r.addr }.map {|r| r.alloc.to_s}.join(', ')
				unless relocs.empty?
					label = label.to_s + " -> #{relocs}"
				end

				if (mnemo = d.text)
					if (prm = d.args)
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
					"#{'%04X'%(d.addr+org)}: #{hxd[data.split(''),''].ljust 12}#{mnemo.ljust 20}#{label}"
				elsif data.empty? and label
					"#{'%04X'%(d.addr+org)}: #{' '*32}#{label}"
				else
					a = d.addr - 8 + org
					data.split('').each_slice(8).map do |ds|
						a+= 8
						"#{'%04X'%a}: #{hxd[ds, ' '].ljust 24}#{ds.join.gsub(/[\x0-\x1f]/, '.').ljust 8}#{label}"
					end
				end
			else
				[" #{imports[d].class} ".center(38, ?=)] + imports[d].debug + [" #{imports[d].class} ".center(38, ?^)]
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
				[alloc.to_i(0, from)].pack(size == 1 ? 'c' : 's<')
			else
				prg.reloc << Relocation.new(prg.pc + offset, alloc, size, from)
				"\x0"*size
			end
		end
	end
	##
	#  This module defines methods that become your program's class methods when you include
	#  the Z80 module.
	#  Includes all of Program::Macros and Program::Mnemonics methods as well as Macros module
	#  defined in the including class.
	#
	#  Use these methods to build your Z80 assembler program or macros.
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
			imports = Hash[@imports.map do |addr, size, program, code_addr, arguments, name|
				code_addr = addr + start if code_addr.nil?
				ip = program.new(code_addr, *arguments)
				raise CompileError, "Imported program #{program} has been modified." unless ip.code.bytesize == size
				c[addr, size] = ip.code
				[name.nil? ? addr + start : name.to_sym, ip]
			end]
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
					unless Integer === r.from or (-128..127).include?(i)
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
		#  Creates offset from Program.pc to +address+ padding it with +pad+.
		#
		#  Do not confuse it with assembler directive ORG which sets absolute address of a program.
		#  In ruby-z80 only an instances of a program have absolute addresses.
		#
		#  Options:
		#
		#  * +:align+:: additionally align address to the nearest +:align+ bytes boundary
		#  * +:offset+:: added to the +address+ after alignment
		#
		#  Returns unnamed +label+ that points to the beginning of a padded space.
		def org(address = pc, pad = 0, align: 1, offset: 0)
			address = address.to_i
			align = align.to_i
			raise ArgumentError, "align must be >= 1" if align < 1
			address = (address + align - 1) / align * align + offset.to_i
			raise Syntax, "The current code pointer: #{pc.to_s 16} is exceeding: #{address.to_i.to_s 16} " if pc > address
			raise Syntax, "The current code pointer is exceeding 64k address range: #{address.to_i.to_s 16} " if address > 0x10000
			Z80::add_code self, [pad].pack('c')*(address - pc)
		end
		##
		#  Creates a isolated namespace for relocatable labels defined inside your code.
		#
		#  Isolated namespace can't reference labels defined outside of it.
		#
		#  See: Program#ns.
		def isolate(name = nil, **opts, &block)
			ns(name, **opts, isolate: true, &block)
		end
		##
		#  Creates a namespace for relocatable labels defined inside your code.
		#
		#  Give a block which generates z80 code containing labels or possibly other
		#  namespaces (namespaces can be nested).
		#
		#  Give optional +name+ as a String or a Symbol for named (labeled) namespaces.
		#
		#  Options:
		#
		#  * +:inherit+:: if +true+ namespace inherits absolute labels from parent namespaces
		#                 if a name, label or an Array of names only specified labels are inherited;
		#                 +false+ by default,
		#  * +:inherit_absolute+:: alias of +:inherit+,
		#  * +:inherit_labels+:: alias of +:inherit+,
		#  * +:use+:: alias of +:inherit+,
		#  * +:isolate+:: +true+ creates isolated namespace :see: Program#isolate,
		#  * +:merge+:: +true+ merges labels from within a namespace with the current context;
		#               usefull if you only want to pass a +eoc+ label to some block of code.
		#
		#  The block receives one variable: +eoc+ which is a label pointing immediately +after+
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
		def ns(name = nil, **opts)
			raise ArgumentError, "no block given to ns" unless block_given?
			inherit = opts[:inherit] || opts[:inherit_absolute] || opts[:inherit_labels] || opts[:use]
			labels = @labels
			@labels = labels.dup
			inherit = [inherit] if !inherit.is_a?(Array) &&
									(inherit.is_a?(Symbol) || inherit.is_a?(String) || label?(inherit))
			@contexts << if inherit.is_a?(Array)
				inherit.map do |name|
					name = if name.respond_to?(:to_name)
						name.to_name
					else
						name.to_s
					end
					labl = labels[name]
					if labl and labl.dummy?
						ct = @contexts.reverse_each.find do |ct|
							l = ct[name]
							l and !l.dummy?
						end
						labl = ct and ct[name]
					end
					raise CompileError, "Label: `#{name}' not found" if labl.nil?
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
			top = Label.dummy
			eoc = Label.dummy 'EOC'
			begin_reloc_index = @reloc.length
			begin_debug = DebugInfo.new(addr, 0, '--- begin ---', nil, @context_labels.dup << top)
			@debug << begin_debug
			@context_labels << top unless opts[:merge]
			yield eoc
			if @reloc[begin_reloc_index...@reloc.length].all? {|r| r.alloc != eoc}
				begin_debug.text = nil
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
			if opts[:merge]
				members.each {|n, l| self.send(n, l) }
				top.reinitialize addr, pc - addr, :code
			else
				top.reinitialize addr, pc - addr, :code, Hash[members]
			end
			top = self.send name.to_sym, top if name
			top
		end
		##
		#  Import macros from another +program+.
		#
		#  A sugar for:
		#
		#     import program, :code => false, :macros => true, :labels => false
		#
		def macro_import(program)
			import program, code: false, macros: true, labels: false
		end
		##
		#  Import labels from another +program+.
		#
		#  A sugar for:
		#
		#     import program, :code => false, :macros => false, :labels => true
		#
		def label_import(program, name = nil, labels:true)
			import program, name, code: false, macros: false, labels: labels
		end
		##
		#  Import code, labels and macros from another +program+.
		#  Give (optional) +name+ for namespace.
		#  Without +name+ labels from +program+ will be defined in a current namespace.
		#  Pass +program+ class (not an instance!).
		#  Give flags to choose what to import from +program+:
		#  * +:labels+:: +true/false+ or an absolute address (default: +true+)
		#  * +:code+::   +true/false+ or an absolute address (default: +true+)
		#  * +:macros+:: +true/false+ (default: +false+)
		#  * +:args+::   program initialize arguments
		#  
		#  To be able to import *macros* create module `Macros'
		#  inside +program+ class and put methods there. They will be imported as macros.
		#  <b>In such a method always wrap your code inside #ns.</b>
		#
		#  Returns (optionally named) +label+ that points to the beginning of imported code.
		def import(program, name=nil, labels:true, code:true, macros:false, args:[])
			if program.is_a?(Symbol)
				program, name = name, program
			end
			addr = pc

			code_addr = if code.respond_to?(:to_i)
				code.to_i
			end

			if macros
				self.extend program::Macros if defined?(program::Macros)
			end

			if labels
				label_addr, absolute = if labels.respond_to?(:to_i)
					[labels.to_i, true]
				else
					[addr, false]
				end
				members = Hash[program.exports.map {|n, l|
					[n, l.deep_clone_with_relocation(label_addr, absolute)]
				}]
			end

			type = code ? program.code.bytesize : 1

			if name
				plabel = Label.new(addr, type, :code, members)
				self.send name.to_sym, plabel
			else
				members.each {|n, m| self.send n.to_sym, m} if members
				plabel = Label.new addr, type, :code
			end

			if code and !program.code.bytesize.zero?
				@debug << DebugInfo.new(addr, 0, nil, nil, @context_labels.dup << plabel)
				@debug << @imports.size
				@imports << [addr, type, program, code_addr, args, name]
				@code << program.code
			end

			program.freeze
			program.code.freeze
			plabel
		end
		##
		#  Import binary file.
		#  * +file+ is a filename.
		#  * +type+ specifies format of binary file (as Symbol),
		#    if +:any+ -> format will be determined by filename's extension.
		#  * +size+ specifies a binary size (cropped or extended to),
		#
		#  Options:
		#  * +pipe+ should be a +proc+ to postprocess binary data with (e.g. compress it)
		#  * +check_size+ may be given to check the size of the imported data
		#    (before pipe) otherwise the CompileError is being raised.
		#  * +data_type+ argument may be given to specify label type.
		#
		#  Other options are passed to +read_data+ method of the format handler.
		#
		#  <b>Currently only :tap or :tzx format is supported and only,
		#  if you include Z80::TAP in your program</b>.
		#
		#  If format is not known, file is being imported as a binary.
		#
		#  Returns unnamed +label+ that points to beginning of imported data.
		def import_file(file, type = :any, size = nil, pipe:nil, check_size:nil, data_type:nil, **args)
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
			if Integer === check_size
				raise CompileError, "size does not match the imported file size: #{check_size} != #{data.bytesize}" if check_size != data.bytesize
			end
			unless pipe.nil?
				raise ArgumentError, "pipe should be a proc" unless pipe.respond_to? :call
				data = pipe.call data
			end
			if data_type.nil?
				data_type = size || data.bytesize
			end
			Z80::add_code self, if size
				data[0, size].ljust(size, "\x0")
			else
				data
			end, data_type
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
