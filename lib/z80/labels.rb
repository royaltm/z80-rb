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
		## call-seq:
		#       export label_name
		#       export :auto
		#       export :noauto
		#
		#  Marks +label_name+ as exportable. Programs may import labels from another programs with Program.import.
		#  Only exportable labels will be imported into the parent program.
		#  Imported labels retain all their members.
		#
		#  Alternatively pass +:auto+ symbol to make all subsequent top level labels exportable
		#  or +:noauto+ to stop auto-exporting.
		#
		#  Only top level labels may be exported this way.
		def export(label)
			raise Syntax, "Only labels on the top level may be exported" if @contexts.length != 1
			case label
			when :auto
				@autoexport = true
			when :noauto, :no_auto
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
		#  Returns a normalized pointer label, Register or an integer.
		#  Otherwise pass-through.
		#
		#  Convenient method for checking arguments in macros.
		#
		#  The following example arguments will be unwrapped as pointer labels:
		#    [0x1234], [foo], [:bar]
		#  The following example arguments will be unwrapped as pointer registers:
		#    [hl], [de], [ix]
		#  The following example arguments will pass unmodified:
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
		#  A convenient method for macros to check if an argument is a Register.
		#
		#  Returns +true+ for:
		#    hl, a, [hl], [iy + 6]
		def register?(arg)
			arg = arg.first while arg.is_a?(Array)
			arg.is_a?(Register)
		end
		##
		#  A convenient method for macros to check if an argument is label-like.
		#
		#  Returns +true+ for:
		#    foo, :foo, foo[10], [foo], [foo + 10], [:foo], [foo[10]]
		def label?(arg)
			arg = arg.first while arg.is_a?(Array)
			arg.respond_to?(:to_label)
		end
		##
		#  A convenient method for macros to check if an argument is a direct label (not a pointer).
		#
		#  Returns +true+ for:
		#    foo, :foo, foo[10]
		def direct_label?(arg)
			arg.respond_to?(:to_label) and !pointer?(arg)
		end
		##
		#  A convenient method for macros to check if an argument is pointer-like.
		#
		#  Returns +true+ for:
		#    [foo], [:foo], [foo + 10], [foo[10]], foo[], foo[10][], [0x1234], [hl], [ix + 6], iy[7]
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
		#  A convenient method for macros to check if an argument is a non-register address (direct or a pointer).
		#
		#  Returns +true+ for:
		#    0x1234, foo, :foo, [0x1234], [foo], foo[10], [:foo], [foo + 10]
		def address?(arg)
			arg = arg.first while arg.is_a?(Array)
			arg.is_a?(Integer) or arg.respond_to?(:to_label)
		end
		##
		#  A convenient method for macros to check if an argument is a non-register direct address (not a pointer).
		#
		#  Returns +true+ for:
		#    0x1234, foo, :foo, foo[10]
		def direct_address?(arg)
			arg.is_a?(Integer) or direct_label?(arg)
		end
		##
		#  A convenient method for macros to check if an argument is an immediate label.
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
		#  A convenient method for macros to check if an argument is an immediate label or an integer.
		#
		#  Returns +true+ for:
		#    foo addr 0x1234
		#    0x1234, foo, :foo, [0x1234], [foo], foo[10], [:foo], [foo + 10]
		def immediate?(arg)
			arg = arg.first while arg.is_a?(Array)
			label_immediate?(arg) or arg.is_a?(Integer)
		end
		##
		#  Returns an unnamed, relative label at Program.pc of the optional +type+.
		#
		#  +type+ can be an integer or a data structure (a class derived from Label).
		#
		#  Options:
		#
		#  * +:align+:: Additionally lazily aligns a label to the nearest multiple of +:align+ bytes
		#               applying a lazy evaluated expression to a label.
		#  * +:offset+:: Added to a label after alignment.
		#
		#  Example:
		#    foo     label
		#    bar     label 2
		def label(type = 1, align: nil, offset: 0)
			lbl = Label.new pc, type, :code
			if align.nil? or offset == 0
				@debug << DebugInfo.new(pc, 0, nil, nil, @context_labels.dup << lbl)
			else
				if align
					align = align.to_i
					raise ArgumentError, "align must be >= 1" if align < 1
					lbl = (lbl + align - 1) / align * align
				end
				lbl = lbl + offset unless offset == 0
			end
			lbl
		end
		##
		#  Returns an unnamed, immediate label at an absolute +address+ of the optional +type+.
		#
		#  +type+ can be an integer or a data structure (a class derived from Label).
		#  The +address+ may be a number or another label or an immediate expression.
		#  It may also be a +:next+ symbol. In this instance the label address
		#  will be the previously added label address offset by its size.
		#
		#  Options:
		#
		#  * +:align+:: Additionally aligns the +address+ to the nearest multiple of +:align+ bytes.
		#  * +:offset+:: Added to the +address+ after alignment.
		#
		#  Example:
		#    foo addr 0xffff
		#    bar addr 0x4000, 2
		#    baz addr :next, 2 # 0x4002
		def addr(address, type = 1, align: 1, offset: 0)
			if address == :next
				last_label = @labels.values.last
				raise Syntax, "There is no label added to the program yet." if last_label.nil?
				addr last_label[1], type, align:align, offset:offset
			else
				address = address.to_i
				align = align.to_i
				raise ArgumentError, "align must be >= 1" if align < 1
				address = (address + align - 1) / align * align + offset.to_i
				Label.new address, type
			end
		end
		##
		#  Returns a new, unnamed label addressed by +label+, but of different +type+.
		#  +type+ can be an integer or a data structure (a class derived from Label).
		#  If +label+ was relative the returned label will also be relative.
		#  If +label+ was absolute the returned label will also be absolute.
		#
		#  Options:
		#
		#  * +:align+:: Additionally aligns the +label+ to the nearest multiple of +:align+ bytes
		#               applying a lazy evaluated expression to a label.
		#  * +:offset+:: Added to the +label+ after alignment.
		#
		#  Example:
		#    foo label
		#    bar union foo, 2
		def union(label, type, align: nil, offset: 0)
			unless label.respond_to?(:to_label) and !label.sublabel? and !label.dummy? and !type.nil?
				raise Syntax, "Invalid union argument."
			end
			lbl = Label.new label.to_i, type, label.immediate? ? nil : :code
			if align
				align = align.to_i
				raise ArgumentError, "align must be >= 1" if align < 1
				lbl = (lbl + align - 1) / align * align
			end
			lbl = lbl + offset unless offset == 0
			lbl
		end
		## call-seq:
		#       data(type = 1)
		#       data(type, size = nil)
		#       data(type, size, *data)
		#       data(type, *data)
		#       data(str[, size = str.bytesize])
		#
		#  Returns an unnamed, relative label and adds provided data to the Program.code at Program.pc.
		#
		#  The +type+ argument may be a number +1+ to indicate bytes or +2+ to indicate words.
		#  To store larger integers please consult Z80::MathInt::Macros.int.
		#
		#  +type+ may also be a class derived from Label, which represents a data structure
		#  with named fields. In this instance each +data+ argument must be an Array or a Hash
		#  containing data for each field in the structure.
		#  Please consult Label for more information and examples.
		#
		#  +type+ may also be one of the following symbols - in this instance the +type+ will always be 1 (a byte):
		#  * +:pc+:: A +data+ label will be evaluated relatively to the program counter, e.g. an offset of a jump table.
		#  * +:jr+:: A +data+ label will be evaluated relatively to the program counter + 1, like an offset of a +jr+ instruction.
		#  * +:self+:: A +data+ label will be evaluated relatively to self, like an offset of an address using +ix+/+iy+ registers.
		#              In this instance all labels but fields will evaluate to 0.
		#
		#  If the first argument is a string, a label is being created of the +type+
		#  being equal to the string's byte size. It allows you to easily access string's size
		#  with unary + method on a returned label. The string is being added as a binary string
		#  to Program.code at Program.pc and may be limited (or extended) with a +size+ argument.
		#  Any other arguments are ignored in this form.
		#
		#  The +data+ argument must be one of the following:
		#
		#  * A string: it will be added as a binary string.
		#  * A convertible Object (with method +:to_z80bin+ which should return a binary string).
		#  * An integer (starting from 3rd argument), a label or a lazy evaluated expression
		#    will be added as a byte or a word (2-bytes, LSB) depending on the +type+.
		#  * An array of integers, strings, convertible objects or labels (the array will be flattened),
		#    will be added consecutively to the Program.code.
		#
		#  If the +size+ is specified as a second argument, +data+ will be padded with zeroes
		#  or cut according to +size+ * +type.to_i+.
		#
		#  Examples:
		#    # Creates "foo" label of type 2 and fills 10 bytes of code (5 words) with data from array.
		#    foo   data 2, [0, 2, 4, label1, label2]
		#    # Creates "bar" label and fills 10 bytes of code with 0s.
		#    bar   data 1, 10
		#    # Creates "bar" label and fills 2 words of code with data from an array and the rest (3 words) with 0s.
		#    baz   data 2, 5, [1, 2]
		#    baz   data 2, 5, 1, 2
		#    # Creates "mystr" label and fills 12 bytes of code with bytes from a string, adds a byte +10+ at the end.
		#    mystr data 1, "Hello World!", 10
		#    # Creates "mystr" label and fills 12 bytes of code with bytes from a string, adds a word +4242+ at the end
		#    # and fills additional 14 bytes of code with 0s.
		#    mystr data 2, 20, "Hello World!", 4242
		#    # Creates "hello" label which addresses the following string and +hello resolves to its length
		#    # which is 12 in this instance.
		#    hello data "Hello World!"
		#  See also: Label for additional examples on how to use labels in a more advanced way.
		def data(type = 1, size = nil, *args)
			res = ''
			from = 0
			case type
				when :jr, :pc, :self
					from = type
					type = 1
			end
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
						when 1 then Z80::add_reloc(self, data, 1, index, from)
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
		#       bytes count
		#       bytes count, byte_integer, ...
		#       bytes count, [byte_integer, ...]
		#       bytes [byte_integer, ...]
		#
		#  Returns an unnamed label and allocates +count+ bytes with Program.data.
		#  Optionally you can provide values for the allocated bytes.
		#
		#  See: Program.data.
		def bytes(*args); data(1, *args); end
		## call-seq:
		#       db *byte_integers
		#
		#  Returns an unnamed label and adds the provided integers to Program.code as bytes.
		#
		#  See: Program.data.
		def db(*args); data(1, args); end
		## call-seq:
		#       words count
		#       words count, word_integer, ...
		#       words count, [word_integer, ...]
		#       words [word_integer, ...]
		#
		#  Returns an unnamed label and allocates +count+ words with Program.data.
		#  Optionally you can provide values for the allocated words.
		#
		#  See: Program.data.
		def words(*args); data(2, *args); end
		## call-seq:
		#       dw *word_integers
		#
		#  Returns an unnamed label and adds the provided integers to Program.code as words.
		#
		#  See: Program.data.
		def dw(*args); data(2, args); end
		##
		#  Defines a label with the given name in the current namespace's context.
		#  Returns a named label.
		#
		#  A +label+, if provided, may be an integer, an instance of an unnamed label, or a lazy evaluated expression.
		#  If +label+ has already some different name an error will be raised.
		#  
		#  If +label+ argument is missing the "dummy" label (a label that is being referenced before being given
		#  value and type) is being created instead. See Label and Label.dummy?.
		#
		#  This method exists for ability to create an arbitrary named label without any constraint on its name.
		#  However the way one should normally define labels is via: method_missing
		#
		#  Example:.
		#    define_label :loop, label
		#            # ... some code
		#            djnz define_label :loop
		#
		#  See method_missing for more examples.
		def define_label(name, label=nil)
			raise "there is no context for a label" unless (ct = @contexts.last)
			name = name.to_s
			if label
				label = if label.respond_to?(:to_label)
					label.to_label self
				else
					Label.new label.to_i, 1
				end
				@dummies.delete_if do |n, lbl, *cts|
					if n == name and cts.include? ct.object_id
						lbl.reinitialize label
						label.name = name
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
				ct[name]||= Label.dummy(name).to_alloc
			end.tap do |label|
				label.name = name
				@labels.delete name # move label to last position
				@labels[name] = label
			end
		end
		def respond_to_missing?(m, include_private=false) # :nodoc:
			m != :to_ary && m != :to_a && m != :to_hash && m != :to_h
		end
		##
		#  If no singleton method +m+ is defined, assume +m+ is a label name to define.
		#  Returns a named label.
		#
		#  A +label+, if provided, may be an integer, an instance of an unnamed label, or a lazy evaluated expression.
		#  If +label+ has already some other name an error will be raised.
		#  
		#  If +label+ argument is missing the "dummy" label is being created instead. See Label and Label.dummy?.
		#
		#  To create a label with the name of the existing singleton method or a ruby keyword, see: define_label
		#
		#  Example:.
		#    mylabel 0x0123
		#    mylabel addr 0x0123, 2
		#  This example gives a name "mylabel" to a label produced by an +ld+ instruction and later references it:
		#    mylabel ld  a, [hl]
		#            inc hl
		#            djnz mylabel
		#  This example creates a dummy label "skipret" and assings a value to it later:
		#            jp  Z, skipret
		#            ret
		#    skipret label
		def method_missing(m, label = nil)
			if m == :to_ary || m == :to_a || m == :to_hash || m == :to_h
				super
			else
				define_label(m, label)
			end
		end
	end
	##
	#  =Z80 Label
	#
	#    myloop  inc [hl]
	#            inc hl
	#            djnz myloop 
	#
	#  A label in a Z80::Program represents an address, a number, an expression or an index to another label.
	#  Instead of using numbers, provide a name and define its value above or below.
	#  Labels have three properties assotiated with them: an +address+, a +type+ (which influences its size) and
	#  the property indicating if the label is absolute or relative to the code or a field of a structure.
	#
	#  Labels are being lazy evaluated when a program is being compiled.
	#  Labels as well as integers may be used in expressions.
	#  Currently there are lazy evaluated expression functions available:
	#
	#    -x      - a negative x
	#    x + y   - a result of y added to x
	#    x - y   - a result of y subtracted from x
	#    x * y   - a result of x times y
	#    x / y   - a quotient of an euclidean division of x by y
	#    x % y   - a remainder of an euclidean division of x by y
	#    x << y  - a bitwise left shifted x by y bits
	#    x >> y  - a bitwise right shifted x by y bits
	#    ~x      - a bitwise negated x
	#    x ^ y   - a bitwise "exclusive or" of x and y
	#    x | y   - a bitwise "or" of x and y
	#    x & y   - a bitwise "and" of x and y
	#    +z      - a byte size of a type of z (in this instance z must not be an expression)
	#
	#  Where +x+ and +y+ represents labels or indexes to labels or expressions; +y+ may also be an integer;
	#  +z+ may only be a label or an index.
	#
	#  Labels may be nested, that is each label may contain named members.
	#  There are two types of such members:
	#  * Independent labels, living in a namespace, formed as a result of using: Program.import or Program.ns.
	#  * Fields of a data structure; fields represents offsets relative to a parent label;
	#    fields may also contain members, but only as fields.
	#
	#  Members are being accessed by indexes. An index is being formed by either:
	#
	#  * using [index] after a label name, see: Label#[], e.g.: <tt>foo[:bar][2]['baz']</tt>
	#  * using an undefined method on a label, e.g.: <tt>foo.bar[2].baz</tt>
	#
	#  Indices as integers or expressions offset labels' addresses by their type's size.
	#  Indices as names access members or sub-labels.
	#
	#  A data structure is a ruby class inherited from Label.
	#
	#      # a data structure, also a new type: Sprite
	#      class Sprite < Label
	#      # name    type[, count]
	#        x       byte, 1
	#        y       byte # , 1 is default
	#        data_pl byte
	#        data_ph byte
	#        size    word
  #      # alias   orig.   type
	#        data_p  data_pl word
	#      end
	#
	#      class SpritePool < Label
	#        numspr  byte
	#        sprites Sprite, 2
	#      end
	#
	#  Fields are being formed by labeling data types and providing its count as a second, optional argument.
	#  The only basic data types are: Label.byte and Label.word. A data structure may also be used as a data type.
	#  In the above example +data_p+ and +data_pl+ are aliases (unions) which means that +data_p+ represents
	#  the same offset as +data_pl+ but has different size.
	#
	#  Labels that are not part of a structure may be absolute or relative to the program code. 
	#  Absolute labels resolve always to the same value so they are also called "immediate" as we don't
	#  need to know the program origin to evaluate them. See Program.immediate?.
	#
	#  Note:: Labels are being evaluated by calling Label#to_i method on them. Normally you don't need to know this,
	#         but in some corner cases you may want to take advantage of that. However be warned that labels are lazy
	#         by its nature and may be defined in the future. Labels that are part of an expression but are not defined
	#         yet are called "dummy". See: Label.dummy?. Calling +to_i+ on a "dummy" label results in an error.
	#         An expression containing at least one "dummy" label is also a "dummy" one.
	#
	#  Allocating, that is assigning values to labels may be done in several ways:
	#  * With Program.addr::
  #    Creates an absolute (immediate) label "spritep", with +value+ = +0x8888+ and of type +SpritePool+:
  #      spritep   addr 0x8888, SpritePool
  #
	#  * With Program.data::
	#    Creates a relative label "spritep", with +value+ = Program.pc and of type +SpritePool+,
	#    fills SpritePool fields with provided data:
	#      spritep   data SpritePool, [2,
	#                    {x:0, y:0, size:12, data_p: sprite1_data},
	#                    {x:0, y:0, size:16, data_p: sprite2_data}]
  #
	#  * With Program.label::
	#    Creates a relative label "someprc", with +value+ = Program.pc and of type 1:
	#      someprc   label
	#
	#  * With a mnemonic::
	#    Creates a relative label "someprc", with +value+ = Program.pc and of type 1:
	#      someprc   ld  de, [foo.bar]
	#
	#  * With Program.union::
	#    Creates a relative label "foo", with +value+ = +bar+ and of type 2:
	#      foo       union bar, 2
	#
	#  Examples::
	#  To access +data_p+ field from a second +Sprite+ in the +spritep+:
	#    ld  hl, [spritep.sprites[1].data_p]
	#  or
	#    ld  l, [spritep.sprites[1].data_pl]
	#    ld  h, [spritep.sprites[1].data_ph]
	#
	#  To set register pointing to data_p from second sprite:
	#    ld  hl, spritep.sprites[1].data_p
	#    ld  e, [hl]
	#    inc hl
	#    ld  d, [hl]
	#  or
	#    ld  ix, sprite
	#    ld  b, [ix + spritep.numspr]
	#    ld  l, [ix + spritep.sprites[1].data_pl]
	#    ld  h, [ix + spritep.sprites[1].data_ph]
	#  Access a size of a label
	#    ld  bc, +spritep # lazy evaluates to +SpritePool to 13
	#  Access a size of a field
	#    ld  bc, +spritep.sprites       # lazy evaluates to +Sprite to 6
	#    ld  b, +spritep.sprites.size   # lazy evaluates to 2
	#
	#  ===Label names
	#  Label name may be a valid ruby method name except singleton method names of your *program* and any existing ruby class method name:
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
	#  Pro tip:: Use namespaces: Program.ns extensively in your program.
	#
	#  Be carefull with loops, as +loop+ is a ruby statement.
	#
	class Label
		# This method is being used internally when importing labels from other programs.
		def deep_clone_with_relocation(addr, absolute, override, prefix=''.freeze) # :nodoc:
			fullname = prefix + @name.to_s
			if @name
				if (override_label = override && override[fullname])
					return override_label
				end
			end
			members = Hash[@members.map {|n, m| [n, m.deep_clone_with_relocation(addr, absolute, override, fullname + '.'.freeze)] }]
			addr = @reloc == :code ? @address + addr.to_i : @address
			reloc = (absolute && @reloc == :code) ? nil : @reloc
			Label.new(addr, @type, reloc, members).tap do |l|
				l.name = @name if @name
			end
		end
		##
		# Evaluates a label. This method is being used during program compilation.
		# * +start+:: An absolute address to offset a label if it's relative to the code base.
		# * +rel_to+:: An absolute address to subtract from a label value or +:self+ (used by ix/iy offset addressing).
		# * +override+:: A Hash containing a possibly nested label names and override values for label overriding.
		# * +prefix+:: A prefix of a nested label used for label overriding.
		# * +size_of+:: If +true+ returns a size of a label's type instead.
		def to_i(start = 0, rel_to = nil, override:nil, prefix:''.freeze, size_of:false)
			raise Syntax, "a label `#{@name}' can't be coerced to a Integer before it's defined" if dummy?

			return @size if size_of

			if rel_to == :self
				0
			else
				if @name
					fullname = prefix + @name
					if (override_value = override && override[fullname])
						return override_value - rel_to.to_i
					end
				end
				@address - rel_to.to_i + (@reloc == :code ? start : 0)
			end
		end
		## Checks if label is a pointer. Prefer using Program.pointer? instead.
		def pointer?; false; end
		## Checks if a label is an expression.
		def expression?; false; end
		## Checks if a label is a named sub-label access expression.
		def sublabel_access_expression?; false; end
		## Checks if a label is defined and absolute: +true+ or not (relative or dummy): (+false+). Prefer using Program.immediate? instead.
		def immediate?
			!dummy? and !@reloc
		end
		## Checks if a label is a member of a struct or a stand-alone label.
		def sublabel?
			@reloc == :parent
		end
		## Checks if a label is not yet given value and type (in-the-future a.k.a. a +dummy+ label).
		def dummy?
			@type.nil?
		end
		##
		# Creates a dummy label. Should not be used directly in programs.
		# This is called by Program.method_missing when referenced label has not been defined yet.
		def self.dummy(name = nil)
			n = new(0, nil)
			n.name = name if name
			n
		end

		def initialize(address, type = 1, reloc = nil, members = nil) # :notnew:
			# an absolute or relative address
			@address = address.to_i
			# size in bytes
			@size = type.to_i
			# a dummy label has @type == nil, usually 1 or 2 or a class inherited from the Label
			if type.nil? and (!(members.nil? or members.empty?) or !reloc.nil? or address != 0)
				raise Syntax, "not a really dummy label: #{self.inspect} reloc: #{reloc.inspect} address: #{address.inspect} members: #{members.inspect}"
			end
			@type = type
			# nil, :code or :parent
			raise Syntax, "reloc should be nil, :code or :parent, got: #{reloc.inspect}" unless reloc.nil? or reloc == :code or reloc == :parent
			@reloc = reloc
			# a hash with members (struct base members have reloc == :parent)
			@members = {}.update(members || {})
			# optional name, assigned later
			@name = nil
		end
		## Returns a member by its +name+ as a separate label. This is used internally. Use Label#[] and Label#method_missing instead.
		def **(name)
			@members[name]
		end
		## Reinitializes a dummy label. Internal use only.
		def reinitialize(address, type = 1, reloc = nil, members = nil) # :nodoc:
			return self if address == self
			raise Syntax, "label #{self} already initialized." unless dummy?
			if address.is_a? self.class
				raise Syntax, "can't assign a dummy to another dummy" if address.dummy?
				address, type, reloc, name, members = %w[@address @type @reloc @name @members].
				                                        map {|n| address.instance_variable_get(n) }
				self.name = name if name
			end
			raise Syntax, "address is not an integer." unless Integer === address
			@address = address
			@size = type.to_i
			@type = type
			@reloc = reloc
			@members = members if members
			self
		end
		## Returns a lazy evaluated label as an instance of Alloc class. Use one of the lazy operators directly on a label instead.
		def to_alloc
			Alloc.new(self)
		end
		## Returns a lazy evaluated size of a type of a label.
		def +@
			+to_alloc
		end
		## Returns a lazy evaluated negative label.
		def -@
			-to_alloc
		end
		# Returns a lazy evaluated label offset by +index+.
		# * If +index+ is +nil+, returns a pointer label instead.
		# * If +index+ is a number or an expression the offset is multiplied by a size of a label's type.
		# * If +index+ is a symbol or a string an accessor to the member of this label will be created. See: Label#method_missing.
		#
		# e.g.:
		#    foo addr 0x1234, 2
		#    ld  hl, foo[7]   # loads 0x1234+14 into hl
		#    ld  hl, foo[-42] # loads 0x1234-84 into hl
		#                     # pointer conversion (2nd form)
		#    ld  hl, foo[]    # loads a byte from memory pointed at 0x1234 into l
		#                     # and a byte pointed at 0x1235 into h
		# =====For clarity don't use the pointer form directly in your programs.
		# Instead prefer to use one-element array wrapped around a label, integer or a Register, like this:
		#    ld  hl, [foo]
		def [](index = nil)
			to_alloc[index]
		end

		## Returns a lazy evaluated label offset by an +other+ label or an integer.
		def +(other)
			to_alloc + other
		end
		## Returns a lazy evaluated label negatively offset by an +other+ label or an integer.
		def -(other)
			to_alloc - other
		end
		## Returns a lazy evaluated label multiplied by an +other+ label or an integer.
		def *(other)
			to_alloc * other
		end
		## Returns a lazy evaluated quotient of a label divided by an +other+ label or an integer.
		def /(other)
			to_alloc / other
		end
		## Returns a lazy evaluated remainder of a label divided by an +other+ label or an integer.
		def %(other)
			to_alloc % other
		end
		## Returns a lazy evaluated label right shifted by a number of bits as an +other+ label or an integer.
		def >>(m)
			to_alloc >> m
		end
		## Returns a lazy evaluated label left shifted by a number of bits as an +other+ label or an integer.
		def <<(m)
			to_alloc << m
		end
		## Returns a lazy evaluated bitwise "exclusive or" of a label and an +other+ label or an integer.
		def ^(m)
			to_alloc ^ m
		end
		## Returns a lazy evaluated bitwise "or" of a label and an +other+ label or an integer.
		def |(m)
			to_alloc | m
		end
		## Returns a lazy evaluated bitwise "and" of a label and an +other+ label or an integer.
		def &(m)
			to_alloc & m
		end
		## Returns a lazy evaluated bitwise negated label.
		def ~
			~to_alloc
		end
		## call-seq:
		#       to_label(program)
		#
		# Should return a Label or an Alloc. This method's existence indicates that something quacks like a label.
		# The only argument is a program class on which the label will be used.
		def to_label(_); self; end
		## Gives a name to a no-named label. Should not be used directly in programs.
		def name=(value)
			value = value.to_s
			raise Syntax, "Invalid label name: #{value.inspect}" if value.empty?
			raise Syntax, "Can't rename already named label: #{@name} != #{value}" if @name and @name != value
			@name = value
		end
		## Returns this label's name as string or +nil+.
		def to_name; @name; end
		## Returns an abbreviated string information about a label, mostly used in error messages.
		def to_str; "`#{@name}':#{'%04X' % @address}:#{@size} #{@reloc}#{dummy? ? '?':''}"; end
		alias_method :to_s, :to_str
		def respond_to_missing?(m, include_private=false) # :nodoc:
			m != :to_ary && m != :to_a && m != :to_hash && m != :to_h
		end
		## Any other method will lazy evaluate as an accessor to the member label of this label.
		def method_missing(m)
			if m == :to_ary || m == :to_a || m == :to_hash || m == :to_h
				super
			else
				to_alloc.send m
			end
		end

		## A class representing members of a data structure.
		Member = ::Struct.new :name, :offset, :type, :count, :alias

		class << self
			def inherited(klass) # :nodoc:
				klass.instance_variable_set '@struct_size', 0
				klass.instance_variable_set '@members', []
			end
			##
			# Returns a new Ruby +Struct+ from members defined in a data structure.
			#
			# Instances of such a +Struct+ are suitable for passing as arguments to Program.data
			# instead of e.g. +Hash+ instances.
			#
			# Member aliases are being ignored when creating a +Struct+.
			def to_struct
				raise Syntax, "Label is not a data strucutre" unless defined?(@members)
				::Struct.new *@members.reject {|_, m| m.alias}.map{|n, _| n.to_sym}
			end
			## A data structure's field type.
			def byte(size = 1)
				1*size
			end
			## A data structure's field type.
			def word(size = 1)
				2*size
			end
			def respond_to_missing?(m, include_private=false) # :nodoc:
				m != :to_ary && m != :to_a && m != :to_hash && m != :to_h && defined?(@struct_size) && defined?(@members)
			end
			## Any other method is being used as a label to a member of a data structure.
			def method_missing(m, struct=nil, count=1)
				if struct.nil? || !defined?(@struct_size) || !defined?(@members)
					super
				else
					n = m.to_s
					if struct.is_a? Member
						struct.name = n
						raise "#{self.name} has already a member: #{n}" if @members.assoc(n)
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
			end
			## Used by Program.data. Do not use it directly in programs.
			#  +data+ must be a +Hash+, +Struct+, +Array+, +String+ or a convertible +Object+ (with a +to_z80bin+ method).
			def to_data(prog, offset, data)
				unless defined?(@struct_size) && defined?(@members)
					raise Syntax, "Label is not a data strucutre"
				end
				data = data.to_h if data.is_a?(::Struct)
				if data.is_a?(Hash)
					res = "\x0"*@struct_size
					@members.each do |n, m|
						n = n.to_sym
						if data.key?(n)
							item = data[n]
							items = if item.is_a?(Hash) || item.is_a?(::Struct) then [item] else Array(item) end
							item_offset = m.offset
							m.count.times do |index|
								s = member_item_to_data(prog, m, offset + item_offset, items[index])
								size = s.bytesize
								res[item_offset, size] = s
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
				if m.type.respond_to?(:to_data) && (data.respond_to?(:to_ary) || data.is_a?(Hash) || data.is_a?(::Struct))
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
			## Returns a size of a data structure immediately.
			def to_i; @struct_size; end
			## Returns a lazy evaluated size of a data structure. Better for debugging than Label.to_i.
			def +@
				if defined?(@struct_size)
					+(self.new(0).tap{|l|l.name = self.name}.to_alloc)
				else
					raise Syntax, "Label has no size"
				end
			end
			## Returns a hash containing structure members as instances of a Member class.
			def members_of_struct; @members && Hash[@members]; end
			##
			# Creates an instance of a label. Do not use it directly in programs.
			# Instead use Program.data, Program.label, Program.addr, Program.union or prepend any instruction with a name instead.
			# Some instructions like Program.ns can create named labels if given a symbol.
			def new(addr, type = 1, reloc = nil, members = nil)
				if members.nil?
					if defined?(@struct_size)
						members = Hash[@members.map do |_, m|
							l = if m.type.is_a?(Class) && m.type.respond_to?(:to_data)
								m.type.new(m.offset, 1, :parent)
							else
								Label.new(m.offset, m.type, :parent)
							end
							l.name = m.name
							[m.name, l]
						end]
						Label.new(addr, self, reloc, members)
					elsif type.is_a?(Class) && type.ancestors.include?(self)
						type.new(addr, type.to_i, reloc)
					end
				end || (addr.is_a?(self) ? addr : super)
			end
		end
	end
	##
	# Alloc class is used internally by relocation mechanizm and lazy evaluation of labels' values.
	# See Label instead.
	class Alloc
		# Compile labels recursively to re-allocated addresses.
		def Alloc.compile(labels, start, override={}, include_sizes:true) # :nodoc:
			res = {}
			eval_labels = ->(name, alloc, label) do
				res[name] = alloc.to_i(start, override:override)
				begin
					res['+' + name] =  alloc.to_i(size_of:true)
				rescue
				end if include_sizes
				label = label.instance_variable_get('@lhs') if label.is_a?(Alloc)
				if label.is_a?(Label)
					label.instance_variable_get('@members').each do |n, l|
						eval_labels["#{name}.#{n}", alloc[n], l]
					end
				end
			end
			labels.each do |name, label|
				if label.respond_to?(:to_alloc)
					eval_labels[name.to_s, label.to_alloc, label]
				else
					res[name.to_s] = label.to_i
				end
			end
			res
		end
		## Return true if +label+ takes part in an +alloc+ expression.
		def Alloc.include?(alloc, label)
			return true if alloc == label
			return false unless alloc.is_a?(Alloc)
			alloc.instance_exec do
				Alloc.include?(@lhs, label) || Alloc.include?(@rhs, label)
			end
		end
		# This method is being used when importing labels from other programs.
		def deep_clone_with_relocation(addr, absolute, override, prefix=''.freeze) # :nodoc:
			if @name
				if (override_label = override && override[prefix + @name])
					return override_label.to_alloc
				end
			end
			lhs = @lhs.deep_clone_with_relocation(addr, absolute, override, prefix)
			rhs = case @rhs
			when Integer
				@rhs
			when Alloc
				@rhs.deep_clone_with_relocation(addr, absolute, override, prefix)
			end
			index = @index.map do |idx|
				case idx
				when Alloc
					idx.deep_clone_with_relocation(addr, absolute, override, prefix)
				else
					idx
				end
			end
			Alloc.new(lhs, @oper, rhs, index).tap do |l| 
				l.instance_variable_set('@pointer', @pointer)
				if (name = to_name)
					l.name = name
				end
			end
		end

		def dup
			super.tap do |l| 
				l.instance_variable_set('@index', @index.dup)
				l.instance_variable_set('@name', nil)
			end
		end

		def initialize(lhs, oper=nil, rhs=nil, index=[])
			raise Syntax, "lhs is not a Label or an Alloc" unless lhs.is_a?(Label) or lhs.is_a?(Alloc)
			rhs = rhs.to_alloc if rhs.is_a?(Label)
			raise Syntax, "rhs is not an Alloc or an integer" unless rhs.nil? or Integer === rhs or rhs.is_a?(Alloc)
			raise Syntax, "lhs nor rhs must not be a pointer" if lhs.pointer? or (rhs.respond_to?(:pointer?) and rhs.pointer?)
			raise Syntax, "invalid operator" unless oper.nil? or [:+,:-,:+@,:-@,:>>,:<<,:/,:%,:*,:^,:&,:|,:~].include?(oper)
			raise Syntax, "invalid operator's rhs" unless (rhs.nil? and (oper.nil? or [:+@,:-@,:~].include?(oper))) or
			                                              (!oper.nil? and !rhs.nil?)
			raise Syntax, "invalid index" unless Array === index and index.all?{|m| Integer === m || String === m || m.is_a?(Alloc) }
			raise Syntax, "index-op is only allowed on labels" unless index.empty? or oper.nil?
			unless oper.nil?
				raise Syntax, "invalid operator's lhs" unless lhs.is_a?(Alloc)
			end
			@lhs     = lhs
			@oper    = oper
			@rhs     = rhs
			@index   = index
			@pointer = false
			@name    = nil
		end

		def ==(other)
			case other
			when Label
				!expression? && @lhs == other
			when Alloc
				%w[@lhs @oper @rhs @index @pointer].all? do |n|
					self.instance_variable_get(n) == other.instance_variable_get(n)
				end
			else
				false
			end
		end

		def pointer?; @pointer; end

		def expression?
			@pointer || !@oper.nil? || !@index.empty?
		end

		def sublabel_access_expression?
			!@pointer && @oper.nil? && !@index.empty? && @index.all? {|s| String === s }
		end

		def **(m)
			label = self
			begin
				if label.sublabel_access_expression?
					label.instance_eval do
						label = @lhs
						@index.each do |idx|
							label = label ** idx
						end
					end
				else
					raise Syntax, "** #{m} not allowed on an expression: #{label.inspect}" if label.expression?
					label = label.instance_variable_get('@lhs')
				end
			end until label.is_a?(Label)
			label.**(m)
		end

		def +(other)
			Alloc.new(self, :+, other)
		end

		def -(other)
			Alloc.new(self, :-, other)
		end

		def *(other)
			Alloc.new(self, :*, other)
		end

		def /(other)
			Alloc.new(self, :/, other)
		end

		def %(other)
			Alloc.new(self, :%, other)
		end

		def >>(other)
			Alloc.new(self, :>>, other)
		end

		def <<(other)
			Alloc.new(self, :<<, other)
		end

		def +@
			Alloc.new(self, :+@)
		end

		def -@
			Alloc.new(self, :-@)
		end

		def ^(other)
			Alloc.new(self, :^, other)
		end

		def |(other)
			Alloc.new(self, :|, other)
		end

		def &(other)
			Alloc.new(self, :&, other)
		end

		def ~
			Alloc.new(self, :~)
		end

		def dummy?
			@lhs.dummy? || (@rhs.respond_to?(:dummy?) && @rhs.dummy?)
		end

		def immediate?
			if @index.empty?
				@lhs.immediate? && (@rhs.respond_to?(:immediate?) ? @rhs.immediate? : true)
			else
				label = @lhs
				return false if label.dummy?
				@index.all? do |idx|
					case idx
					when String
						label = label ** idx
						label ? label.immediate? : false
					when Alloc
						idx.immediate?
					else
						label.immediate?
					end
				end # all?
			end
		end

		def sublabel?
			false
		end

		def reinitialize(address, type = 1, reloc = nil, members = nil)
			return self if address == self
			raise Syntax, "Can't re-initialize" unless @oper.nil? && @index.empty? && @lhs.is_a?(Label) && @lhs.dummy? && !@pointer
			if address.is_a?(Label) || Integer === address
				@lhs.reinitialize(address, type, reloc, members)
			elsif address.is_a?(self.class)
				lhs, oper, rhs, index, pointer = %w[@lhs @oper @rhs @index @pointer].
																					 map {|n| address.instance_variable_get(n) }
				if lhs.is_a?(Label)
					@lhs.reinitialize(lhs, type, reloc, members)
				else
					@lhs   = lhs
				end
				@oper    = oper
				@rhs     = rhs
				@index   = index
				@pointer = pointer
			else
				raise Syntax, "invalid re-initialize address"
			end
			name = if address.respond_to?(:to_name)
				address.to_name
			elsif
				@lhs.to_name
			end
			self.name = name unless name.nil?
			self
		end

		def to_alloc; self; end

		def to_label(_); self; end

		def to_str
			return @name if @name
			return @lhs.to_str if @pointer
			case @oper
			when nil
				@lhs.to_name.to_s + @index.map {|idx|
					case idx
					when String
						'.' + idx
					else
						"[#{idx}]"
					end
				}.join
			when :+@
				"(+#{@lhs})"
			when :~, :-@
				"#{@oper.to_s[0]}(#{@lhs})"
			else
				"(#{@lhs}#{@oper}#{@rhs})"
			end
		end
		alias_method :to_s, :to_str

		# rel_to: an absolute address or :self used by ix/iy offset addressing
		def to_i(start = 0, rel_to = nil, override:nil, prefix:''.freeze, size_of:false)
			rel_to_label = rel_to == :self ? :self : nil

			arg_to_i = ->(arg, rel_to) do
				case arg
				when Integer
					arg.to_i
				when Alloc, Label
					arg.to_alloc.to_i(start, rel_to, override:override)
				else
					raise CompileError, "Invalid argument: #{arg.inspect}"
				end
			end

			if !size_of and @name and rel_to_label.nil?
				if (override_value = override && override[prefix + @name])
					return override_value - rel_to.to_i
				end
			end

			val = if @oper.nil?
				label = @lhs
				raise CompileError, "can't calculate an address of a directly addressed sublabel: #{label}" if label.sublabel?
				addr = label.to_i(start, rel_to_label, override:override, prefix:prefix)
				# allow overrides of simple nested sublabels that only exist when overridden
				if !size_of and rel_to_label.nil? and !@index.empty? and label.to_name and
					 @index.all?{|idx| idx.is_a?(String)} and
					 (override_value = override && override[prefix + label.to_name + '.'.freeze + @index.join('.'.freeze)])
					 override_value - rel_to.to_i
				else
					@index.each do |idx|
						case idx
						when String
							raise CompileError, "Unknown member: #{idx} of label #{label}." unless sublabel = label ** idx
							subprefix = prefix + label.to_name + '.'.freeze
							# a member of struct
							if sublabel.sublabel?
								addr += sublabel.to_i
							elsif label.sublabel?
								raise CompileError, "Non struct member as a member of a struct label: #{subprefix}#{sublabel.to_name}"
							# a label
							else
								if label.immediate? and !sublabel.immediate?
									raise CompileError, "Relative member #{subprefix}#{sublabel.to_name} of an absolute label #{label}!"
								end
								addr = sublabel.to_i(start, rel_to_label, override:override, prefix:subprefix)
							end
							prefix = subprefix
							label = sublabel
						else
							addr+= arg_to_i.call(idx, nil) * label.to_i(size_of:true)
						end
					end
					if size_of
						label.to_i(size_of:true)
					else
						addr
					end
				end
			else
				raise CompileError, "Can't get a size from an expression: #{self.inspect}" if size_of
				case @oper
				when :+@ then return @lhs.to_i(start, rel_to, override:override, size_of:true)
				when :-@ then -arg_to_i.call(@lhs, rel_to_label)
				when :~ then ~arg_to_i.call(@lhs, rel_to_label)
				else
					arg_to_i[@lhs, rel_to_label].send(@oper, arg_to_i[@rhs, rel_to_label])
				end
			end

			if Integer === rel_to
				val - rel_to
			else
				val
			end
		end

		def name=(value)
			value = value.to_s
			raise Syntax, "Invalid label name: #{value.inspect}" if value.empty?
			raise Syntax, "Can't rename already named label: #{@name} != #{value}" if @name and @name != value
			@name = value
		end

		def to_name
		 	return @name if @name
		 	return @lhs.to_name if !expression?
		end

		def [](index = nil)
			if index.nil?
				if @pointer
					dup
				else
					raise Syntax, "pointer not allowed from a sizeof" if @oper == :+@
					Alloc.new(self).tap { |l| l.instance_variable_set('@pointer', true) }
				end 
			else
				raise Syntax, "indexing is only allowed on a label" unless !@pointer and @oper.nil? and 
				                                                      (@lhs.is_a?(Label) or !@index.empty?)
        if @index.empty?
        	Alloc.new(self)
        else
        	dup
        end.tap do |l|
					lix = l.instance_variable_get('@index')
					case index
					when String, Symbol
						lix << index.to_s
					when Label, Alloc
						lix << index.to_alloc
					when Integer
						if Integer === lix.last
							lix[-1]+= index
						else
							lix << index
						end
					else
						raise Syntax, "invalid index"
					end
				end
			end
		end

		def respond_to_missing?(m, include_private=false)
			m != :to_ary && m != :to_a && m != :to_hash && m != :to_h
		end

		def method_missing(m)
			if m == :to_ary || m == :to_a || m == :to_hash || m == :to_h
				super
			else
				self.[](m)
			end
		end
	end
end
