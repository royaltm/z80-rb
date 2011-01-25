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
    def export(label)
      case label
      when :auto
        @autoexport = true
      when :noauto
        @autoexport = false
      else
        name = label.to_name
        raise Syntax, "No label name for export: #{label.inspect}." if name.nil? or name.empty?
        @exports[name.to_s] = label
      end
    end
    ##
    #  Method used internally by mnemonics to make pointer of a label.
    #
    #  Example:
    #    ld  hl, [label]
    def [](label)
      label = label.first while label.is_a?(Array)
      if label.respond_to?(:to_label) or label.is_a?(Register)
        label[]
      elsif label.is_a?(Condition)
        raise Syntax, "Invalid pointer argument."
      else
        Label.new(label.to_i, 1)[]
      end
    end
    ##
    #  Creates relocable label at Program.pc of (optional) +type+.
    #
    #  Example:
    #    foo label
    #    bar label 2
    #
    #  Returns unnamed +label+ that points to Program.pc and is of +type+.
    def label(type = 1)
      l = Label.new pc, type, :code
      @debug << DebugInfo.new(pc, 0, nil, nil, @context_labels.dup << l)
      l
    end
    ##
    #  Creates absolute label at +address+ of (optional) +type+
    #
    #  Example:
    #    foo addr 0xffff
    #    bar addr 0x4000, 2
    #
    #  Returns unnamed +label+ that points to +address+ and is of +type+.
    def addr(address, type = 1)
      Label.new address, type
    end
    ##
    #  Creates a label at +label+ of different +type+.
    #
    #  Example:
    #    foo label
    #    bar union foo, 2
    #
    #  Returns unnamed +label+ that points to +label+ and is of different +type+.
    def union(label, type)
      raise Syntax, "Invalid union argument." unless label.respond_to?(:to_label) and !label.dummy?
      Label.new label.to_i, type, label.immediate? ? nil : :code
    end
    ## call-seq:
    #       data(type = 1)
    #       data(type, size = 1)
    #       data(type, size, data)
    #       data(type, data)
    #
    #  Creates relocable label and adds data to Program.code at Program.pc.
    #  The data size will be of +type.to_i+ multiplied by +size+.
    #  * +data+ may be a String or an Array (possible containing another Arrays if +type+ is a struct).
    #  * +data+ is padded with zeroes.
    #  Example:
    #    # creates label foo of type 2 and fills 10 bytes of code (5 words) with data from array.
    #    foo   data 2, [0, 2, 4, label1, label2]
    #    # creates label bar and fills 10 bytes of code with 0s.
    #    bar   data 1, 10
    #    # creates label bar and fills 2 words of code with data from array and the rest (3 words) with 0s.
    #    baz   data 2, 10, [1, 2]
    #    # creates label mystr and fills 12 bytes of code with bytes from string.
    #    mystr data 1, "Hello World!"
    #  See: Label for more examples.
    #
    #  Returns unnamed +label+ that points to Program.pc and is of +type+ and size +type.to_i+ * +size+.
    def data(type = 1, size = nil, data = nil)
      data, size = size, nil if size.respond_to? :to_a
      data = if type.respond_to? :to_data
        data||= []
        type.to_data(self, 0, *data)
      elsif data.respond_to? :to_a
        size||= data.to_a.size
        data.to_a[0,size].each_with_index.map do |d,i|
          case type.to_i
          when 1
            if d.respond_to? :to_label
              Z80::add_reloc(self, d, 1, i, :self)
            else
              [d].pack('c')
            end
          when 2
            if d.respond_to? :to_label
              Z80::add_reloc(self, d, 2, i*2)
            else
              [d].pack('s')
            end
          else
            raise Syntax, "Invalid data type"
          end
        end.join
      elsif data.respond_to? :to_s
        size||= data.to_s.bytesize
        type = 1
        data.to_s
      end
      size||= 1
      size*= type.to_i
      data||= 0.chr * size
      data = data.ljust(size, "\x0") if data.bytesize < size
      Z80::add_code(self, data[0,size], type)
    end
    ## call-seq:
    #       bytes(size = 1, data = nil)
    #
    #  Creates a label and allocate bytes with Program.data.
    #
    #  Shortcut for:
    #    data 1, ...
    def bytes(*args); data(1, *args); end
    ## call-seq:
    #       words(size = 1, data = nil)
    #
    #  Creates a label and allocate words with Program.data.
    #
    #  Shortcut for:
    #    data 2, ...
    def words(*args); data(2, *args); end
    ##
    #  If no method +m+ is defined assume it is a label.
    #  Label with no arguments is a label being referenced.
    #  If label has argument and it is a label (or integer) allocate a name for it.
    #
    #  Example:.
    #    mylabel 0x0123
    #  is the same as:
    #    mylabel addr 0x0123
    #  This creates a label at instruction and references it:
    #    mylabel ld  a, [hl]
    #            inc hl
    #            djnz mylabel
    #
    #  Returns named +label+ that points to +label+ or is a dummy label (not yet defined).
    def method_missing(m, label = nil)
      # puts [m.inspect, label.inspect]*', '
      name = m.to_s
      if ct = @contexts.last
        @labels[name] = if label
          label = if label.respond_to? :to_label
            label.to_label self
          else
            Label.new label.to_i
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
          if @autoexport
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
  #  Label class is the CORE of relocation mechanizm:
  #    mylabel ld  a, [hl]
  #            inc hl
  #            djnz mylabel
  #
  #  Labels also allow to create structs:
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
  #  In the above example +data_p+ and +data_pl+ are unions.
  #
  #  To peek +data_p+ from second +Sprite+:
  #    ld  hl, sprite.sprites[2].data_p
  #  To set register pointing to data_p from second sprite:
  #    ld  hl, (sprite.sprites*2).data_p
  #    ld  e, [hl]
  #    inc hl
  #    ld  d, [hl]
  #  or
  #    ld  ix, sprite
  #    ld  b, [ix + sprite.numspr]
  #    ld  l, [ix + sprite.sprites[2].data_pl]
  #    ld  h, [ix + sprite.sprites[2].data_ph]
  #  Allocate label with data in *program*
  #    sprites data SpritePool, [2,
  #            [0, 0, 12, sprite1],
  #            [0, 0, 16, sprite2]]
  #  or with absolute address
  #    sprites addr 0x8888, SpritePool
  #  or just a label at Program.pc
  #    someprc label
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
    def deep_clone_with_relocation(addr)
      members = Hash[@members.map {|n, m| [n, m.deep_clone_with_relocation(addr)] }]
      addr = reloc ? @address + addr.to_i : @address
      l = Label.new(addr, @type, @reloc, members)
      l.name = @name if @name
      l
    end
    # Evaluates label. Do not use it directly.
    # This method is being used during program compilation.
    def to_i(start = 0, rel_to = nil)
      if rel_to == :self
        0
      else
        @address - rel_to.to_i + (@reloc ? start : 0)
      end unless dummy?
    end
    # Checks if label is a pointer. Do not use it directly.
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
      @type = type
      @reloc = reloc
      @members = {}.update(members || {})
      @name = nil
    end
    # Checks if label is absolute (+true+) or relocable (+false+). Do not use it directly.
    # This method is being used during program compilation.
    def immediate?
      !dummy? and !@reloc
    end
    # Checks if label is dummy. Do not use it directly.
    # This method is being used during program compilation.
    def dummy?
      @type.nil?
    end
    # Returns a member +m+ as a separate label.
    def >>(m)
      @members[m]
    end
    # Reinitializes dummy label. Do not use it directly.
    # This method is being used during program compilation.
    def reinitialize(address, type = 1, reloc = nil, members = nil)
      raise Syntax, "label #{self} already initialized." unless dummy?
      if address.is_a? self.class
        address, type, reloc, name, members = [
          '@address', '@type', '@reloc', '@name', '@members'
        ].map {|n| address.instance_variable_get(n) }
        self.name = name if name
      end
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
        +Alloc.new(self)
      else
        @size
      end
    end
    # Returns label indexed by +index+ as a pointer.
    def [](index = 0)
      Alloc.new(self)[index]
    end
    # Returns label indexed by +index+ but not as a pointer.
    def *(index)
      Alloc.new(self) * index
    end
    # Returns label offset by +offset+.
    # It can be an integer or another label.
    def +(offset)
      Alloc.new(self) + offset
    end
    # Returns label offset by negative +offset+.
    # It can be an integer or another label.
    def -(offset)
      Alloc.new(self) - offset
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
    def to_str; "`#{@name}':#{'%04X'%@address}:#{@size} #{@reloc}#{dummy? ? '?':''}"; end
    alias_method :to_s, :to_str
    def method_missing(m)
      Alloc.new(self).send m
    end
    Member = ::Struct.new :name, :offset, :type, :count, :alias
    class << self
      def inherited(klass) # :nodoc:
        klass.instance_variable_set '@size', 0
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
            Member.new(n, mem.offset, struct, count, true)
          else
            @members << [n, Member.new(n, @size, struct, count, false)]
            @size+= tsize*count
            nil
          end
        end
      end
      # Used by Program.data. Do not use it directly.
      def to_data(prog, offset, *data)
        @members.reject {|_, m| m.alias}.each_with_index.map do |(n, m), i|
          s = ''
          m.count.times do
            d = data.shift
            len = m.type.to_i
            s << if m.type.respond_to?(:to_data) and d.is_a?(Array)
              m.type.to_data(prog, offset, *d)
            elsif d.is_a?(String)
              d[0, len]
            elsif d.respond_to? :to_label
              Z80::add_reloc(prog, d, len, offset + s.bytesize, len == 1 ? :self : nil)
            else
              [d.to_i].pack('Q')[0, len]
            end.ljust(len, "\0")
          end
          offset+= s.bytesize
          s
        end.join
      end
      attr_reader :size
      alias :to_i :size
      def members_of_struct; @members; end
      # Creates an instance of a label. Do not use it directly.
      # Use Program.data, Program.label, Program.addr, Program.union or prepend any instruction with a name instead.
      # Some instructions like Program.ns can create named labels if given symbolic name.
      def new(addr, type = 1, reloc = nil, members = nil)
        if members.nil?
          if defined?(@size)
            members = Hash[@members.map do |n, m|
              l = if m.type.is_a?(Class) and m.type.respond_to?(:to_data)
                m.type.new(addr + m.offset, 1, reloc)
              else
                super(addr + m.offset, m.type, reloc)
              end
              l.name = m.name
              [m.name, l]
            end]
            super(addr, self, reloc, members)
          elsif type.is_a?(Class) and type.ancestors.include?(self)
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
    end

    def [](index = 0)
      l = self.*(index)
      l.instance_variable_set('@pointer', true)
      l
    end
    
    def pointer?; @pointer; end

    def *(index)
      l = dup
      lindex = l.instance_variable_get('@index')
      if lindex.last and lindex.last.is_a? Integer
        lindex[-1]+= index.to_i
      else
        lindex << index.to_i unless index.to_i == 0
      end
      l
    end

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

    def >>(m)
      @label >> m
    end

    def +@
      l = dup
      l.instance_variable_set('@size', true)
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
            (l = label >> i) and l.immediate?
          else
            true
          end
        }
      else
        false
      end
    end
    def dummy?; @label.dummy?; end

    def to_str
      (@size ? '+' : '') + to_name.to_s + @index.map {|i|
        if String === i
          '.' + i
        else
          "*#{i}"
        end
      }.join + (if @offset > 0
        "+#@offset"
      elsif @offset < 0
        "#@offset"
      end.to_s)
    end
    alias_method :to_s, :to_str

    def to_i(start = 0, rel_to = nil)
      addr = (label = @label).to_i
      @index.each {|i|
        if String === i
          raise CompileError, "Unknown member: #{i} of label #{label}." unless l = label >> i
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
      }
      if @size
        +label + @offset
      else
        rel_to = @label.to_i(start) if rel_to == :self
        addr + @offset + (label.immediate? ? 0 : start) - rel_to.to_i
      end
    end
    def name=(value)
      raise Syntax, "Invalid label name: #{value.inspect}" if (value = value.to_s).empty?
      raise Syntax, "Can't rename already named label: #{@name}!= #{value}" if @name and @name != value and @name != @label.to_name
      @name = value
    end
    def to_name; @name || @label.to_name; end

    def method_missing(m)
      l = dup
      l.instance_variable_get('@index') << m.to_s
      l
    end
  end
end
