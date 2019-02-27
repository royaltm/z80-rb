# -*- coding: BINARY -*-
class Symbol
	##
	#  Allows to use Symbols instead of labels in some situations.
	#  Example:
	#    loop1 add [hl]
	#          inc hl
	#          jr NC, :loop1
	def to_label(program)
		program.send self
	end
end
module Z80
	module Program
		##
		#  Exports +label+. This will allow other programs to Program.import it.
		#  All members of +label+ will also be exported but within a namespace of exported label.
		#  Alternatively pass +:auto+ to make all subsequent labels to be exported
		#  or +:noauto+ to stop autoexporting.
		#
		#  Only top level labels may be exported from a program.
		def export(label)
			raise Syntax, "Only labels on the top level may be exported" if @contexts.length != 1
			case label
			when :auto
				@autoexport = true
			when :noauto
				@autoexport = false
			else
				name = label.to_name
				raise Syntax, "No label name for export: #{label.inspect}." if name.nil? || name.empty?
				@exports[name.to_s] = label
			end
		end
		##
		#  Method used internally by mnemonics to make a pointer of a label or a Register.
		#
		#  Example:
		#    ld  hl, [foo]
		#    # is equivalent to
		#    ld  hl, foo[]
		#    # is equivalent to
		#    ld  hl, self.[](foo)
		#    # or
		#    ld  hl, [foo[5]]
		#    # is equivalent to
		#    ld  hl, foo[5][]
		#    # is equivalent to
		#    ld  hl, self.[](foo[5])
		#    # or
		#    ld  hl, [foo - 8]
		#    # is equivalent to
		#    ld  hl, (foo - 8)[]
		#    # is equivalent to
		#    ld  hl, self.[](foo - 8)
		def [](label)
			label = label.first while label.is_a?(Array)
			if label.respond_to?(:to_label) || label.is_a?(Register)
				label[]
			elsif label.is_a?(Condition)
				raise Syntax, "Invalid pointer argument."
			else
				Label.new(label.to_i, 1)[]
			end
		end
		##
		#  Return normalized pointer-like label, Register or a integer.
		#  Otherwise pass-through.
		#
		#  Convenient method for checking arguments in macros.
		#
		#  The following examples will be unwrapped as pointer-like labels:
		#    [0x1234], [foo], [:bar]
		#  The following examples will be unwrapped as pointer-like registers:
		#    [hl], [de], [ix]
		#  The following examples will pass unmodified:
		#    a, bc, :foo, bar
		def unwrap_pointer(arg)
			if arg.is_a?(Array)
				self.[](arg)
			else
				arg
			end
		end
		##
		#  True if a label with a +name+ is defined in the current context.
		#
		def label_defined?(name)
				@labels.has_key? name.to_s
		end
		##
		#  Convenient method for macros to check if argument is a Register.
		#
		#  Returns +true+ for:
		#    hl, a, [hl], [iy + 6]
		def register?(arg)
			arg = arg.first while arg.is_a?(Array)
			arg.is_a?(Register)
		end
		##
		#  Convenient method for macros to check if argument is label-like.
		#
		#  Returns +true+ for:
		#    foo, :foo, [foo], [foo + 10], [:foo]
		def label?(arg)
			arg = arg.first while arg.is_a?(Array)
			arg.respond_to?(:to_label)
		end
		##
		#  Convenient method for macros to check if argument is pointer-like.
		#
		#  Returns +true+ for:
		#    [foo], [:foo], [foo + 10], [foo[10]], foo[], foo[10][], [0x1234], [hl], [ix + 6], ix[7]
		def pointer?(arg)
			if arg.is_a?(Array)
				true
			elsif arg.respond_to?(:pointer?)
				arg.pointer?
			elsif label?(arg)
				arg.to_label(self).pointer?
			else
				false
			end
		end
		##
		#  Convenient method for macros to check if argument is non-register value or a pointer.
		#
		#  Returns +true+ for:
		#    0x1234, foo, :foo, [0x1234], [foo], foo[10], [:foo], [foo + 10]
		def address?(arg)
			arg = arg.first while arg.is_a?(Array)
			arg.is_a?(Integer) or arg.respond_to?(:to_label)
		end
		##
		#  Convenient method for macros to check if argument is an immediate label.
		#
		#  Returns +true+ for:
		#    foo addr 0x1234
		#    foo, :foo, [foo], foo[10], [:foo], [foo + 10]
		def label_immediate?(arg)
			arg = arg.first while arg.is_a?(Array)
			if arg.respond_to?(:immediate?)
				arg.immediate?
			else
				label?(arg) and arg.to_label(self).immediate?
			end
		end
		##
		#  Convenient method for macros to check if argument is an immediate label or a integer
		#
		#  Returns +true+ for:
		#    foo addr 0x1234
		#    0x1234, foo, :foo, [0x1234], [foo], foo[10], [:foo], [foo + 10]
		def immediate?(arg)
			arg = arg.first while arg.is_a?(Array)
			label_immediate?(arg) or arg.is_a?(Integer)
		end
		##
		#  Creates relocatable label at Program.pc of (optional) +type+.
		#
		#  Example:
		#    foo label
		#    bar label 2
		#
		#  Returns unnamed +label+ that points to Program.pc and is of +type+.
		#  The +type+ can be a integer or a struct derived from a Label.
		def label(type = 1)
			l = Label.new pc, type, :code
			@debug << DebugInfo.new(pc, 0, nil, nil, @context_labels.dup << l)
			l
		end
		##
		#  Creates an immediate label at an absolute +address+ of (optional) +type+
		#
		#  Example:
		#    foo addr 0xffff
		#    bar addr 0x4000, 2
		#
		#  Returns unnamed +label+ that points to +address+ and is of +type+.
		#  The +type+ can be a integer or a struct derived from a Label.
		#  The +address+ may be a number or another (possibly with offset) label.
		#  It may also be a +:next+ symbol. In this instance the label address
		#  will be the previously added label address + its size.
		def addr(address, type = 1)
			if address == :next
				last_label = @labels.values.last
				raise Syntax, "There is no label added to the program yet." if last_label.nil?
				addr last_label[1], type
			else
				Label.new address, type
			end
		end
		##
		#  Creates a label at +label+ of different +type+.
		#
		#  Example:
		#    foo label
		#    bar union foo, 2
		#
		#  Returns unnamed +label+ that points to +label+ and is of different +type+.
		#  The +type+ can be a integer or a struct derived from a Label.
		def union(label, type)
			raise Syntax, "Invalid union argument." unless label.respond_to?(:to_label) and !label.dummy? and !type.nil?
			Label.new label.to_i, type, label.immediate? ? nil : :code
		end
		## call-seq:
		#       data(type = 1)
		#       data(type, size = 1)
		#       data(type, size, *data)
		#       data(type, *data)
		#
		#  Creates relocatable label and adds data to Program.code at Program.pc.
		#  The data size will be of +type.to_i+ multiplied by +size+.
		#
		#  The +type+ argument may be a +1+ to indicate integers as bytes or a +2+ to indicate them as words.
		#
		#  +type+ may also be a String. In this instance a label is created of the +type+
		#  being equal to the String bytesize. It allows you to easily access string byte size
		#  pointed by a label with a unary + method. The string is also being added as a data
		#  to Program.code at Program.pc and may be limited (or extended) with a +size+ argument.
		#  Any +data+ arguments are ignored in this form.
		#
		#  The +data+ argument must be one of the following:
		#
		#  * a String - it will be added as an 8bit binary string
		#  * a convertible Object (with method :to_z80bin which should return binary String)
		#  * an Integer starting from third argument - it will be added as a byte or word
		#    depending on the +type+
		#  * a Label already representing a value or lazy evaluated
		#  * an Array of integers, strings, convertible objects or labels
		#    (possibly containing another Arrays - it will be flattened)
		#
		#  If the +size+ is specified as a second Integer argument, +data+ will be padded with zeroes
		#  or cut according to +size+ * +type+.
		#
		#  Additionally a +type+ may be a user defined class inherited from the Label, which represents
		#  a data structure with named fields.
		#  In this case each +data+ argument must be an Array or a Hash containing data for the structure.
		#  See Label for more information and examples.
		#
		#  Example:
		#    # Creates +foo+ of type 2 and fills 10 bytes of code (5 words) with data from array.
		#    foo   data 2, [0, 2, 4, label1, label2]
		#    # Creates +bar+ and fills 10 bytes of code with 0s.
		#    bar   data 1, 10
		#    # Creates +bar+ and fills 2 words of code with data from an array and the rest (3 words) with 0s.
		#    baz   data 2, 5, [1, 2]
		#    baz   data 2, 5, 1, 2
		#    # Creates +mystr+ and fills 12 bytes of code with bytes from a string, adds a byte +10+ at the end.
		#    mystr data 1, "Hello World!", 10
		#    # Creates +mystr+ and fills 12 bytes of code with bytes from a string, adds a word +4242+ at the end
		#    # and fills additional 14 bytes of code with 0s.
		#    mystr data 2, 20, "Hello World!", 4242
		#    # Creates +hello+ which addresses the following string and +hello+ resolves to its length
		#    # which is 12 in this instance.
		#    hello data "Hello World!"
		#  See: Label for more examples.
		def data(type = 1, size = nil, *args)
			res = ''
			if type.respond_to? :to_data
				unless Integer === size
					args.unshift size
					size = args.length
				end
				type_size = type.to_i
				size.times do |i|
					res << type.to_data(self, i*type_size, args.shift)
				end
				size = nil
			elsif type.is_a?(String)
				res << type
				type = Integer === size ? size : res.bytesize
			else
				bsize = type.to_i
				raise Syntax, "Invalid data type" unless bsize == 1 || bsize == 2
				if Integer === size
					size *= bsize
				else
					args.unshift size
					size = nil
				end
				pack_string = bsize == 1 ? 'c' : 's<'
				args.flatten.each_with_index do |data, index|
					res << 
					if data.respond_to? :to_label
						case bsize
						when 1 then Z80::add_reloc(self, data, 1, index, :self)
						when 2 then Z80::add_reloc(self, data, 2, index*2)
						end
					elsif data.respond_to? :to_z80bin
						data.to_z80bin
					elsif Integer === data
						[data].pack(pack_string)
					else
						data.to_s
					end
				end
			end
			if size
				res = res.ljust(size, "\x0") if res.bytesize < size
				res.slice!(size..-1)
			end
			Z80::add_code(self, res.force_encoding(Encoding::ASCII_8BIT), type)
		end
		## call-seq:
		#       bytes(size = 1, *data)
		#       bytes(*data)
		#
		#  Creates a label and allocate bytes with Program.data.
		#
		#  Sugar for:
		#    data 1, ...
		def bytes(*args); data(1, *args); end
		## call-seq:
		#       db(*byte_integers)
		#
		#  Creates a label and allocate bytes with Program.data.
		#
		#  Sugar for:
		#    data 1, [...]
		def db(*args); data(1, args); end
		## call-seq:
		#       words(size = 1, *data)
		#       words(*data)
		#
		#  Creates a label and allocate words with Program.data.
		#
		#  Sugar for:
		#    data 2, ...
		def words(*args); data(2, *args); end
		## call-seq:
		#       dw(*word_integers)
		#
		#  Creates a label and allocate bytes with Program.data.
		#
		#  Sugar for:
		#    data 2, [...]
		def dw(*args); data(2, args); end
		##
		#  If no method +m+ is defined assume it is a label.
		#  Label with no arguments is a label being referenced.
		#  If label has argument and it is a label (or integer) allocate a name for it.
		#
		#  Example:.
		#    mylabel 0x0123
		#  is the same as:
		#    mylabel addr 0x0123
		#  This creates a label at a instruction and references it:
		#    mylabel ld  a, [hl]
		#            inc hl
		#            djnz mylabel
		#
		#  Returns named +label+ that points to +label+ or is a dummy label (not yet defined).
		def method_missing(m, label = nil)
			if ct = @contexts.last
				name = m.to_s
				@labels[name] = if label
					label = if label.respond_to? :to_label
						label.to_label self
					else
						Label.new label.to_i, 1
					end
					@dummies.delete_if do |n, lbl, *cts|
						if n == name and cts.include? ct.object_id
							lbl.reinitialize label
							true
						else
							false
						end
					end
					if ct.has_key? name
						ct[name].reinitialize label
						label.name = name
					else
						label.name = name
						ct[name] = label
					end
					if @autoexport and @contexts.length == 1
						export ct[name]
					else
						ct[name]
					end
				else
					ct[name]||= Label.dummy(name)
				end
			end
		end
	end
	##
	#  =Z80 Label
	#
	#  A Label class is the CORE of relocation mechanizm:
	#    mylabel inc [hl]
	#            inc hl
	#            djnz mylabel
	#
	#  Labels also allows to create structs:
	#      class Sprite < Label
	#        x       byte
	#        y       byte
	#        data_pl byte
	#        data_ph byte
	#        data_p  data_pl word
	#        size    byte
	#      end
	#
	#      class SpritePool < Label
	#        numspr  byte
	#        sprites Sprite, 2
	#      end
	#
	#  In the above example +data_p+ and (+data_pl+,+data_ph+) are aliases (union).
	#
	#  Allocate label with data in a *program*
	#    sprite  data SpritePool, [2,
	#             {x:0, y:0, size:12, data_p:sprite1_data},
	#             {x:0, y:0, size:16, data_p:sprite2_data}]
	#  or with an absolute address
	#    sprite  addr 0x8888, SpritePool
	#  or just a label at Program.pc
	#    someprc label
	#
	#  To peek +data_p+ from second +Sprite+:
	#    ld  hl, [sprite.sprites[1].data_p]
	#  or
	#    ld  l, [sprite.sprites[1].data_pl]
	#    ld  h, [sprite.sprites[1].data_ph]
	#  To set register pointing to data_p from second sprite:
	#    ld  hl, sprite.sprites[1].data_p
	#    ld  e, [hl]
	#    inc hl
	#    ld  d, [hl]
	#  or
	#    ld  ix, sprite
	#    ld  b, [ix + sprite.numspr]
	#    ld  l, [ix + sprite.sprites[1].data_pl]
	#    ld  h, [ix + sprite.sprites[1].data_ph]
	#  You may even want to load the offset of a struct member
	#    ld  a, sprite.sprites[1].data_ph   # -> ld a, 9
	#
	#  ===Label names
	#  Label name may be any valid ruby method name and not a singleton method name of your *program*.
	#  It excludes:
	#  * ruby statements
	#  * ruby core Class.methods:
	#    <code>! != !~ < <= <=> == === =~ > >= __id__ __send__ allocate ancestors autoload autoload? class class_eval class_exec class_variable_defined?
	#    class_variable_get class_variable_set class_variables clone const_defined? const_get const_missing const_set constants define_singleton_method
	#    display dup enum_for eql? equal? extend freeze frozen? hash include? included_modules initialize_clone initialize_dup inspect instance_eval
	#    instance_exec instance_method instance_methods instance_of? instance_variable_defined? instance_variable_get instance_variable_set instance_variables
	#    is_a? kind_of? method method_defined? methods module_eval module_exec name nil? object_id private_class_method private_instance_methods private_method_defined?
	#    private_methods protected_instance_methods protected_method_defined? protected_methods public_class_method public_instance_method public_instance_methods public_method
	#    public_method_defined? public_methods public_send remove_class_variable respond_to? respond_to_missing? send singleton_class singleton_methods superclass
	#    taint tainted? tap to_enum to_s trust untaint untrust untrusted?</code>
	#  * Z80::Program.instance_methods:
	#    <code>[] a adc add addr af anda b bc bc_ bit bytes c call ccf code cp cpd cpdr cpi cpir cpl d daa data de de_ debug dec di djnz e ei ex export
	#    exx h halt hl hl_ hlt i im0 im01 im1 im2 import import_file inc ind indr ini inir inp ix ix_ ixh ixl iy iy_ iyh iyl jp jr l label labels ld ldd
	#    lddr ldi ldir macro method_missing neg new nop ns ora org otdr otir out outd outi pc pop push r reloc res ret reti retn rl rla rlc rlca rld r rra
	#    rrc rrca rrd rst sbc scf set sl1 sla sll sp sp_ sra srl sub union words xor</code>
	#  * and macros defined in your *program*.
	#
	#  Use namespaces (Program.ns) extensively in your program.
	#  It is also wise to add numeric suffixes to label names:
	#    loop1 label
	#
	#  +loop+ is a ruby statement.
	#
	class Label
		# This method is being used when importing labels from other programs.
		def deep_clone_with_relocation(addr) # :nodoc:
			members = Hash[@members.map {|n, m| [n, m.deep_clone_with_relocation(addr)] }]
			addr = @reloc ? @address + addr.to_i : @address
			l = Label.new(addr, @type, @reloc, members)
			l.name = @name if @name
			l
		end
		# Evaluates label. Do not use it directly.
		# This method is being used during program compilation.
		def to_i(start = 0, rel_to = nil)
			raise Syntax, "a label `#{@name}' can't be coerced to a Integer before it's defined" if dummy?
			if rel_to == :self
				0
			else
				@address - rel_to.to_i + (@reloc ? start : 0)
			end
		end
		# Checks if label is a pointer. Prefer using Program.pointer? instead.
		# This method is being used during program compilation.
		def pointer?; false; end
		# Creates a dummy label. Do not use it directly.
		# This is called when referenced label has not been yet defined.
		def self.dummy(name = nil)
			n = new(0, nil)
			n.name = name if name
			n
		end
		def initialize(address, type = 1, reloc = nil, members = nil) # :notnew:
			@address = address.to_i & 0xffff
			@size = type.to_i
			# a dummy label has @type == nil
			@type = type
			@reloc = reloc
			@members = {}.update(members || {})
			@name = nil
		end
		# Checks if label is absolute (+true+) or relocatable (+false+). Prefer using Program.immediate? instead.
		# This method is being used during program compilation.
		def immediate?
			!dummy? and !@reloc
		end
		# Checks if a label is already defined or is in-the-future a.k.a. a +dummy+ label.
		# Do not use it directly.
		# This method is being used during program compilation.
		def dummy?
			@type.nil?
		end
		# Returns a member +m+ as a separate label.
		def **(m)
			@members[m]
		end
		# Shifts right label binary value when resolved.
		def >>(m)
			to_alloc >> m
		end
		# Shifts left label binary value when resolved.
		def <<(m)
			to_alloc << m
		end
		# Reinitializes dummy label. Do not use it directly.
		# This method is being used during program compilation.
		def reinitialize(address, type = 1, reloc = nil, members = nil)
			return self if address == self
			raise Syntax, "label #{self} already initialized." unless dummy?
			if address.is_a? self.class
				address, type, reloc, name, members = [
					'@address', '@type', '@reloc', '@name', '@members'
				].map {|n| address.instance_variable_get(n) }
				self.name = name if name
			end
			raise Syntax, "address is not an integer." unless Integer === address
			@address = address & 0xffff
			@size = type.to_i
			@type = type
			@reloc = reloc
			@members = members if members
			self
		end
		def to_alloc
			Alloc.new(self)
		end
		# Returns size (type size) of a label.
		def +@
			if dummy?
				+to_alloc
			else
				@size
			end
		end
		# Returns negated label.
		def -@
			-to_alloc
		end
		# Returns a label offset by +index+ multiplied by label type size.
		# If +index+ is nil, returns a pointer instead.
		#
		# e.g.:
		#    foo addr 0x1234, 2
		#    ld  hl, foo[7]   # loads 0x1234+14 into hl
		#    ld  hl, foo[-42] # loads 0x1234-84 into hl
		#                     # pointer conversion (2nd form)
		#    ld  hl, foo[]    # loads a byte from memory pointed at 0x1234 into l
		#                     # and a byte pointed at 0x1235 into h
		# =====If possible don't use directly the 2nd, pointer form in your programs.
		# For clarity use one-element array wrapped around a label, integer or a Register:
		#    ld  hl, [foo]
		def [](index = nil)
			to_alloc[index]
		end

		# Returns label indexed by +index+ but not as a pointer.
		# Returns label offset by +offset+.
		# It can be an integer or another label.
		def +(offset)
			to_alloc + offset
		end
		# Returns label offset by negative +offset+.
		# It can be an integer or another label.
		def -(offset)
			to_alloc - offset
		end
		def to_label(_); self; end
		# Gives name to no-name label. Do not use it directly.
		def name=(value)
			raise Syntax, "Invalid label name: #{value.inspect}" if (value = value.to_s).empty?
			raise Syntax, "Can't rename already named label: #{@name}!= #{value}" if @name and @name != value
			@name = value
		end
		# Returns label name or +nil+.
		def to_name; @name; end
		def to_str; "`#{@name}':#{'%04X' % @address}:#{@size} #{@reloc}#{dummy? ? '?':''}"; end
		alias_method :to_s, :to_str
		def respond_to_missing?(m, include_private=false)
			m != :to_ary && m != :to_a && m != :to_hash && m != :to_h
		end
		def method_missing(m)
			if m == :to_ary || m == :to_a || m == :to_hash || m == :to_h
				super
			else
				to_alloc.send m
			end
		end
		Member = ::Struct.new :name, :offset, :type, :count, :alias
		class << self
			def inherited(klass) # :nodoc:
				klass.instance_variable_set '@struct_size', 0
				klass.instance_variable_set '@members', []
			end
			# Struct definition type.
			def byte(size = 1)
				1*size
			end
			# Struct definition type.
			def word(size = 1)
				2*size
			end
			def method_missing(m, struct, count = 1) # :nodoc:
				n = m.to_s
				if struct.is_a? Member
					struct.name = n
					raise "#{self.name} has already member: #{n}" if @members.assoc(n)
					@members << [n, struct]
					nil
				else
					tsize = struct.is_a?(Integer) ? struct : struct.to_i
					_, mem = @members.assoc(n)
					if mem
						Member.new(nil, mem.offset, struct, count, true)
					else
						@members << [n, Member.new(n, @struct_size, struct, count, false)]
						@struct_size+= tsize*count
						nil
					end
				end
			end
			# Used by Program.data. Do not use it directly.
			# data must be a Hash, Array, String or convertible Object (with #to_z80bin)
			def to_data(prog, offset, data)
				if data.is_a?(Hash)
					res = "\x0"*@struct_size
					@members.each do |n, m|
						if data.key?(n.to_sym)
							items = Array(data[n.to_sym])
							item_offset = m.offset
							m.count.times do |index|
								s = member_item_to_data(prog, m, offset + item_offset, items[index])
								res[item_offset, size=s.bytesize] = s
								item_offset += size
							end
						end
					end
				elsif data.respond_to? :to_z80bin
					res = data.to_z80bin[0,@struct_size].ljust(@struct_size, "\x0")
				elsif data.is_a?(String)
					res = data.dup.force_encoding(Encoding::ASCII_8BIT)[0,@struct_size].ljust(@struct_size, "\x0")
				else
					data = Array(data)
					res = ''
					index = 0
					@members.reject {|_, m| m.alias}.each do |_, m|
						m.count.times do
							s = member_item_to_data(prog, m, offset, data[index])
							offset += s.bytesize
							index += 1
							res << s
						end
					end
				end
				res
			end
			private
			def member_item_to_data(prog, m, offset, data)
				len = m.type.to_i
				if m.type.respond_to?(:to_data) && (data.respond_to?(:to_ary) || data.is_a?(Hash))
					m.type.to_data(prog, offset, data)
				elsif data.respond_to? :to_label
					Z80::add_reloc(prog, data, len, offset, len == 1 ? :self : nil)
				elsif data.respond_to? :to_z80bin
					data.to_z80bin
				elsif Integer === data
					[data].pack('Q<')
				else
					data.to_s.force_encoding(Encoding::ASCII_8BIT)
				end[0,len].ljust(len, "\x0")
			end
			public
			def to_i; @struct_size; end
			def members_of_struct; @members; end
			# Creates an instance of a label. Do not use it directly.
			# Use Program.data, Program.label, Program.addr, Program.union or prepend any instruction with a name instead.
			# Some instructions like Program.ns can create named labels if given symbolic name.
			def new(addr, type = 1, reloc = nil, members = nil)
				if members.nil?
					if defined?(@struct_size)
						members = Hash[@members.map do |_, m|
							l = if m.type.is_a?(Class) && m.type.respond_to?(:to_data)
								m.type.new(addr + m.offset, 1, reloc)
							else
								super(addr + m.offset, m.type, reloc)
							end
							l.name = m.name
							[m.name, l]
						end]
						super(addr, self, reloc, members)
					elsif type.is_a?(Class) && type.ancestors.include?(self)
						type.new(addr, type.to_i, reloc)
					end
				end || (addr.is_a?(self) ? addr : super)
			end
		end
	end
	##
	#  Alloc class is used internally by relocation mechanizm.
	#  See Label instead.
	#
	class Alloc
		# This method is being used when importing labels from other programs.
		def deep_clone_with_relocation(addr)
			l = dup
			l.instance_variable_set('@label', @label.deep_clone_with_relocation(addr))
			l
		end
		def dup
			l = super
			l.instance_variable_set('@index', @index.dup)
			l
		end
		def initialize(label)
			raise Syntax, "label is not a Label" unless label.is_a? Label
			@label   = label
			@index   = []
			@offset  = 0
			@pointer = false
			@name    = nil
			@size    = false
			@shift   = 0
			@neg     = false
		end

		def [](index = nil)
			l = dup
			if index.nil?
				l.instance_variable_set('@pointer', true)
			else
				index = index.to_i
				lindex = l.instance_variable_get('@index')
				if lindex.last and lindex.last.is_a? Integer
					lindex[-1]+= index
				else
					lindex << index unless index.zero?
				end
			end
			l
		end

		def ==(other)
			if other.is_a?(Alloc)
				@label == other.instance_variable_get("@label")
			else
				@label == other
			end
		end

		def pointer?; @pointer; end

		def +(other)
			l = dup
			l.instance_variable_set('@offset', @offset + other.to_i)
			l
		end

		def -(other)
			l = dup
			l.instance_variable_set('@offset', @offset - other.to_i)
			l
		end

		def **(m)
			@label ** m
		end

		def >>(m)
			l = dup
			l.instance_variable_set('@shift', @shift - m.to_i)
			l
		end

		def <<(m)
			l = dup
			l.instance_variable_set('@shift', @shift + m.to_i)
			l
		end

		def +@
			l = dup
			l.instance_variable_set('@size', true)
			l
		end

		def -@
			l = dup
			l.instance_variable_set('@neg', !@neg)
			l
		end

		def reinitialize(*args)
			@label.reinitialize(*args)
		end

		def to_alloc; self; end

		def to_label(_); self; end

		def immediate?
			if @index.empty?
				@label.immediate?
			elsif !@label.dummy?
				label = @label
				@index.all? {|i|
					if String === i
						(l = label ** i) and l.immediate?
					else
						@label.immediate?
					end
				}
			else
				false
			end
		end
		def dummy?; @label.dummy?; end

		def to_str
			str = (@size ? '+' : '') + to_name.to_s + @index.map {|i|
				if String === i
					'.' + i
				else
					"[#{i}]"
				end
			}.join + (if @offset > 0
				"+#@offset"
			elsif @offset < 0
				"#@offset"
			end.to_s)
			str = "(#{str})" unless @shift.zero? or @offset.zero?
			if @shift > 0
			  str += " << #@shift"
			elsif @shift < 0
				str += " >> #{-@shift}"
			end
			str = "-(#{str})" if @neg
			str
		end
		alias_method :to_s, :to_str

		def to_i(start = 0, rel_to = nil)
			addr = (label = @label).to_i
			@index.each do |i|
				if String === i
					raise CompileError, "Unknown member: #{i} of label #{label}." unless l = label ** i
					if l.immediate?
						if label.immediate?
							addr+= l.to_i - label.to_i
						else
							addr = l.to_i
							rel_to = l.to_i if rel_to == :self
						end
					elsif label.immediate?
						raise CompileError, "Relative member #{l} of absolute label #{label}!"
					else
						addr+= l.to_i - label.to_i
					end
					label = l
				else
					addr+= i*(+label)
				end
			end
			val = (if @size
				+label + @offset
			else
				rel_to = @label.to_i(start) if rel_to == :self
				addr + @offset + (label.immediate? ? 0 : start) - rel_to.to_i
			end) << @shift
			val = -val if @neg
			val
		end
		def name=(value)
			raise Syntax, "Invalid label name: #{value.inspect}" if (value = value.to_s).empty?
			raise Syntax, "Can't rename already named label: #{@name}!= #{value}" if @name and @name != value and @name != @label.to_name
			@name = value
		end
		def to_name; @name || @label.to_name; end
		def respond_to_missing?(m, include_private=false)
			m != :to_ary && m != :to_a && m != :to_hash && m != :to_h
		end
		def method_missing(m)
			if m == :to_ary || m == :to_a || m == :to_hash || m == :to_h
				super
			else
				l = dup
				l.instance_variable_get('@index') << m.to_s
				l
			end
		end
	end
end
