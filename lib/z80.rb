# -*- coding: BINARY -*-
require 'z80/version'
require 'z80/registers'
require 'z80/labels'
require 'z80/mnemonics'
require 'z80/select'
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
	## Error raised during program parsing.
	class Syntax < StandardError; end
	## Error raised during program compilation (while creating instance).
	class CompileError < StandardError; end
	## A compiled code for the Z80 CPU as a binary string.
	attr_reader :code
	## The starting address of the compiled code.
	attr_reader :org
	## A hash containing all evaluated label values of the compiled program.
	attr_reader :labels
	## A hash containing all instanced of imported programs.
	attr_reader :imports
	## A relocation table with all selections applied.
	attr_reader :reloc_info
	## A raw, debug information with all selections applied.
	attr_reader :debug_info
	## Returns an evaluated label's value by its name.
	def [](name)
		@labels[name.to_s]
	end
	##
	#  Creates a debugger view from an instance of a Z80::Program.
	#  Returns an array of strings.
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
		reloc = @reloc_info
		imports = @imports.values
		@debug = @debug_info.map do |d|
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
						mnemo = mnemo % prm.each_slice(2).map { |o, t|
							case t
							when :word
								data[o, 2].unpack('S<')[0]
							when :index
								if (v = data[o, 1].unpack('c')[0]) < 0
									[?-, -v]
								else
									[?+, v]
								end	
							when :byte
								data[o, 1].unpack('C')[0]
							when :pcrel
								data[o, 1].unpack('c')[0] + d.addr + d.size + org
							end
						}.flatten
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
			when Integer
				[" #{imports[d].class} ".center(38, ?=)] + imports[d].debug + [" #{imports[d].class} ".center(38, ?^)]
			else
				raise "Invalid debug element: #{d.inspect}"
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
		## Method used by Program instructions was placed here to not pollute *program* namespace anymore.
		def add_code(prg, data, type = 1, mnemo = nil, *mpar)
			raise TypeError unless data.is_a? String
			l = Label.new(pc = prg.pc, type, :code)
			prg.debug << DebugInfo.new(pc, data.bytesize, mnemo, mpar, prg.instance_variable_get('@context_labels').dup << l)
			prg.code << data
			l
		end
		## Method used by Program instructions was placed here to not pollute *program* namespace anymore.
		def add_reloc(prg, label, size, offset = 0, from = nil)
			raise Syntax, "from argument accepted only for reloc size == 1" unless from.nil? or size == 1
			alloc = label.to_label(prg).to_alloc
			if alloc.immediate? and (size == 2 or from==:self)
				prg.reloc << Relocation.new(prg.pc + offset, alloc, size, from)
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
		## A raw, not relocated code.
		attr_reader :code
		## A relocation table.
		attr_reader :reloc
		## A raw, debug information.
		attr_reader :debug
		## Exportable labels.
		attr_reader :exports
		def self.extended(klass) # :nodoc:
			['@code', '',
			 '@reloc', [],
			 '@debug', [],
			 '@contexts', [{}],
			 '@context_labels', [],
			 '@labels', {},
			 '@dummies', [],
			 '@conditional_blocks', [],
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
		#  Compiles a *program* at the +start+ address passing *args to initialize().
		#  Returns a compiled instance of a *program*.
		#
		#  * +:override+:: A flat hash containing names of labels with addresses to be overwritten.
		def new(start = 0x0000, *args, override:{})
			unless @dummies.empty? and !@contexts.last.any? {|_,v| v.dummy?}
				dummies = @dummies.map {|d| d[0..1]} + @contexts.last.select {|_,v| v.dummy?}.map {|d| d[0..1]}
				raise CompileError, "Labels referenced but not defined: #{dummies.inspect}"
			end

			raise ArgumentError, "override should be a map of override names to labels" unless override.respond_to?(:to_h)
			override = Alloc.compile(override, 0, include_sizes:false)

			prog = super(*args)
			code = @code.dup

			imports = Hash[@imports.map do |addr, size, program, code_addr, arguments, name, import_override|
				merged_override = Alloc.compile(import_override, start, override, include_sizes:false)
				if name.nil?
					merged_override.merge!(override)
				else
					prefix = name.to_s + '.'.freeze
					merged_override.merge!(override.
						select{ |k,| k.start_with?(prefix) }.
						map{ |k,v| [k.slice(prefix.length..), v] }.
						to_h)
				end

				code_addr = addr + start if code_addr.nil?
				ip = program.new(code_addr, *arguments, override:merged_override)
				raise CompileError, "Imported program #{program} has been modified." unless ip.code.bytesize == size
				code[addr, size] = ip.code
				[name.nil? ? addr + start : name.to_sym, ip]
			end]

			labels = Alloc.compile(@labels, start, override)

			reloc = @reloc.dup
			debug = @debug.dup
			@conditional_blocks.each do |cndblk|
				raise CompileError, "Invalid conditional block program" unless cndblk.program.equal?(self)
				ConditionalBlock.compile(cndblk, code, reloc, debug, start, override)
			end

			reloc.each do |r|
				case r.size
				when 0
					raise CompileError, "Absolute labels need relocation sizes for the overrides"
					# OBSOLETE: ignore, this is an absolute address but we need relocation info for debug
				when 1
					addr = case r.from
					when Integer
						r.from
					when :jr, nil
						r.addr + 1 + start
					when :pc
						r.addr + start
					when :self
						:self
					else
						raise CompileError, "Unknown from relocation parameter: #{r.inspect}"
					end
					i = r.alloc.to_i(start, addr, override:override)
					if (r.from.nil? or r.from == :jr) and !(-128..127).include?(i)
						raise CompileError, "Relative relocation out of range at 0x#{'%04x' % r.addr} -> #{i} #{r.inspect}"
					end
					if r.from == :pc and !(0..255).include?(i)
						raise CompileError, "Jump table relocation out of range at 0x#{'%04x' % r.addr} -> #{i} #{r.inspect}"
					end
					code[r.addr] = [i].pack('c')
				when 2
					code[r.addr, 2] = [r.alloc.to_i(start, override:override)].pack('S<')
				else
					code[r.addr, r.size] = [r.alloc.to_i(start, override:override)].pack('Q<')[0, r.size].ljust(r.size, "\0")
				end
			end
			['@code', code,
			 '@org', start,
			 '@debug', nil,
			 '@labels', labels,
			 '@imports', imports,
			 '@reloc_info', reloc,
			 '@debug_info', debug
			].each_slice(2) do |n,v|
				prog.instance_variable_set n,v
			end
			prog
		end
		## Returns current byte offset from the beginning of the Program.code (a program counter relative to 0).
		def pc
			@code.bytesize
		end
		##
		#  Returns an unnamed, relative label that points to the beginning of padded space.
		#  The space is being padded with +pad+ byte.
		#  The +address+ should be relative to the beginning of the Program.code and must be
		#  equal to or greater than Program.pc.
		#
		#  *Note*:: Do not confuse it with assembler directive ORG which sets absolute address of a program.
		#           Only instances of Z80::Program have absolute addresses.
		#
		#  Options:
		#
		#  * +:align+:: Additionally aligns the +address+ to the nearest multiple of +:align+ bytes
		#               (relative to the beginning of code).
		#  * +:offset+:: Added to the +address+ after alignment.
		#
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
		#  Returns a relative label as an isolated namespace for relative labels defined inside the code created within +block+.
		#
		#  A code in an isolated namespace can't reference labels defined outside of it.
		#
		#  See: Program#ns.
		def isolate(name = nil, **opts, &block)
			ns(name, **opts, isolate: true, &block)
		end
		## call-seq:
		#       ns **opts {|eoc| ... }
		#       ns name, **opts {|eoc| ... }
		#
		#  Returns a relative label as a namespace for relative labels defined inside the code created within +block+.
		#
		#  This function requires a block which may generate Z80 code containing labels or possibly other
		#  namespaces (namespaces can be nested). Every label defined within this block will become a member label.
		#
		#  An optional +name+ may be provided, as a string or a symbol. In this instance the returned label
		#  will already have a name. Otherwise an unnamed label is being returned.
		#
		#  Options:
		#
		#  * +:inherit+:: If +true+ namespace inherits absolute labels from parent namespaces
		#                 if a name, label or an Array of names - only specified labels are inherited;
		#                 +false+ by default.
		#  * +:inherit_absolute+:: An alias of +:inherit+.
		#  * +:inherit_labels+:: An alias of +:inherit+.
		#  * +:use+:: An alias of +:inherit+.
		#  * +:isolate+:: If +true+ creates an isolated namespace :see: Program#isolate.
		#  * +:merge+:: If +true+ merges labels from within a namespace with the current context;
		#               usefull if you only want to pass an +eoc+ label to some block of code.
		#
		#  Given +block+ receives one variable: +eoc+ which is a "dummy" label that will relatively
		#  address the end of the namespaced code and may be referenced from within the +block+.
		#
		#  If +:inherit+ option is +false+ but +:isolate+ is also +false+ it's possible to
		#  reference absolute labels defined outside of the namespace, but it would be impossible
		#  to coerce such a label to an integer.
		#
		#  Example:
		#    ns :foo do |eoc|
		#      loop1 add a
		#            jr C, eoc
		#            inc b
		#            jr NZ, loop1
		#    end
		#
		#  Returns an (optionally named) +label+ that points to the beginning of a namespaced code.
		def ns(name = nil, **opts)
			raise ArgumentError, "no block given to ns" unless block_given?
			# Save parent labels
			labels = @labels
			@labels = labels.dup
			# Normalize inherit option
			inherit = opts[:inherit] || opts[:inherit_absolute] || opts[:inherit_labels] || opts[:use]
			inherit = [inherit] if !inherit.is_a?(Array) &&
									(inherit.is_a?(Symbol) || inherit.is_a?(String) || label?(inherit))
			# Looks for labels in every context
			find_1st_defined_label_in_contexts = ->(name) do
				ct = @contexts.reverse_each.find do |ct|
					l = ct[name]
					l and !l.dummy?
				end
				ct and ct[name]
			end
			# Handle inherit option
			@contexts << if inherit.is_a?(Array)
				inherit.map do |name|
					name = if name.respond_to?(:to_name)
						name.to_name
					else
						name.to_s
					end
					labl = labels[name]
					if labl and labl.dummy?
						labl = find_1st_defined_label_in_contexts[name]
					end
					raise CompileError, "Label: `#{name}' not found" if labl.nil?
					[name, labl]
				end.to_h
			elsif inherit == true
				labels.select do |name,label|
					if label.dummy?
						label = find_1st_defined_label_in_contexts[name]
					end
					label && label.immediate?
				end
			elsif !inherit
				{}
			else
				raise ArgumentError, "ns :inherit option must be a boolean, name or array of names"
			end
			# Prepare top and eoc labels
			addr = pc
			top = Label.dummy
			eoc = Label.dummy 'EOC'
			# Prepare debug info for a namespace
			begin_reloc_index = @reloc.length
			begin_debug = DebugInfo.new(addr, 0, '--- begin ---', nil, @context_labels.dup << top)
			@debug << begin_debug
			@context_labels << top unless opts[:merge]
			# Execute block
			yield eoc
			# Check if eoc was used and modify debug info accordingly
			if @reloc[begin_reloc_index...@reloc.length].any? {|r| Alloc.include?(r.alloc, eoc) }
				@debug << DebugInfo.new(pc, 0, '---  end  ---', nil, @context_labels.dup << eoc)
			else
				begin_debug.text = nil
			end
			# Finally define eoc label
			eoc.reinitialize(pc, 1, :code)
			# Restore parent labels
			@labels = labels
			# Get our context's id
			context_id = @contexts.last.object_id
			# Partition labels created in this context
			members, dummies = @contexts.pop.partition do |n, l|
				if l.dummy?
					false
				else
					true
				end
			end
			# Handle dummies left by inner namespaces
			@dummies.delete_if do |name, label, *cts|
				if cts.include?(context_id) and @labels.has_key?(name) and !@labels[name].dummy?
					label.reinitialize @labels[name]
					true
				else
					false
				end
			end
			# Prepare contexts array for dummies
			contexts = @contexts.map(&:object_id)
			# Handle isolate option
			if opts[:isolate]
				unless dummies.empty?
					dummies = dummies.map {|d| d[0..1]}
					raise CompileError, "Undefined labels referenced in isolated namespace: #{dummies.inspect}"
				end
			else
				@dummies+= dummies.map do |name, label|
					if @labels.has_key?(name) and !@labels[name].dummy?
						label.reinitialize @labels[name]
						nil
					else
						[name, label] + contexts
					end
				end.compact
			end
			# Remove our context from debug info
			@context_labels.pop
			# Handle merge option
			if opts[:merge]
				members.each {|n, l| self.send(n, l) }
				top.reinitialize addr, pc - addr, :code
			else
				top.reinitialize addr, pc - addr, :code, Hash[members]
			end
			# Optionally give name to top label
			top = self.send name.to_sym, top if name
			top
		end
		##
		#  Imports macros from another +program+.
		#
		#  A sugar for:
		#
		#     import program, code: false, macros: true, labels: false
		#
		def macro_import(program)
			import program, code: false, macros: true, labels: false
		end
		##
		#  Imports labels from another +program+.
		#
		#  A sugar for:
		#
		#     import program, code: false, macros: false, labels: true
		#
		def label_import(program, name = nil, labels:true)
			import program, name, code: false, macros: false, labels: labels
		end
		##
		#  Imports code, labels or macros from another +program+.
		#  Give (optional) +name+ to create a namespace for imported labels.
		#  Without +name+ labels from +program+ will be defined in the current context.
		#  Pass +program+ class (not an instance!).
		#
		#  Options to choose what to import:
		#  * +:labels+:: +true/false+ or an absolute address (default: +true+).
		#  * +:code+::   +true/false+ or an absolute address (default: +true+).
		#  * +:macros+:: +true/false+ (default: +false+).
		#  * +:override+:: A flat hash containing names with labels to be replaced.
		#  * +:args+:: Initialize arguments for an imported program.
		#  
		#  If +:labels+ is an address, all relative labels being imported will be converted to absolute
		#  and will be offset by the given value.
		#
		#  If +:code+ is an address, the imported code will be always compiled at this absolute address.
		#
		#  To be able to import *macros* create a module named +Macros+
		#  inside +program+ class and put methods there. They will be imported as macros.
		#  <b>In such methods remember to wrap your code with Program.ns.</b>
		#
		#  Returns an (optionally named) label that points to the beginning of the imported code.
		def import(program, name=nil, labels:true, code:true, macros:false, override:{}, args:[])
			if program.is_a?(Symbol)
				program, name = name, program
			end

			raise Syntax, "modules may not be imported from under namespaces" if @contexts.length != 1

			addr = pc
			raise ArgumentError, "override should be a map of override names to labels" unless override.respond_to?(:map)
			override = override.map do |n,v|
				v = case v
				when Integer
					Label.new(v, 0)
				when Label, Alloc
					v
				else
					raise ArgumentError, "override: #{n} is not a label or an address"
				end
				[n.to_s, v]
			end.to_h

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
					raise Syntax, "only named labels may be exported" if l.to_name.nil?
					[n, l.deep_clone_with_relocation(label_addr, absolute, override)]
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
				@imports << [addr, type, program, code_addr, args, name, override]
				@code << program.code
			end

			program.freeze
			program.code.freeze
			plabel
		end
		##
		#  Imports a binary file.
		#  * +file+:: A file name.
		#  * +type+:: A format of a binary file (as a symbol),
		#             if +:any+ -> format will be determined by file name's extension.
		#  * +size+:: A size in bytes to which imported data will be cropped or extended.
		#
		#  Options:
		#  * +pipe+:: A +proc+ to postprocess binary data with (e.g. compress it).
		#  * +check_size+:: A byte size to check the size of the imported data against
		#    (before pipe). If the sizes don't match a CompileError will be raised.
		#  * +data_type+:: A returned label's type.
		#
		#  Any additional options are being passed to +read_data+ method of the format handler.
		#
		#  <b>Currently only :tap or :tzx format is supported and only,
		#  if you include Z80::TAP in your program</b>.
		#
		#  If format is not known, a file is being imported as a blob.
		#
		#  Returns an unnamed label that points to the beginning of imported data.
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
	module Helpers # :nodoc:
		def neg(a); -a & 0xff; end
		def cpl(a); ~a & 0xff; end
		def hb(a); '%02x'%(a & 0xff) ; end
		def h(a); '%04x'%(a & 0xffff) ; end
	end

	autoload :MathInt, 'z80/math_i'
	autoload :Stdlib, 'z80/stdlib'
end
extend Z80::Helpers
