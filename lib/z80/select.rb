# -*- coding: BINARY -*-
module Z80
  ##
  # See Program.select.
  class ConditionalBlock
    Selection = ::Struct.new :args, :test, :then, :else # :nodoc:
    Variant = ::Struct.new :code, :reloc, :debug # :nodoc:

    attr_reader :program # :nodoc:
    attr_reader :address # :nodoc:
    attr_reader :selection # :nodoc:

    def initialize(program, *args, address:nil, codesize:nil, &test) # :nodoc:
      raise ArgumentError, "At least one condition argument is needed in selection" if args.empty?
      args = args.map do |arg|
        arg = Label.new arg, 1 if Integer === arg
        raise ArgumentError, "Selection arguments must be labels or label expressions" unless arg.respond_to?(:to_alloc)
        arg
      end
      raise ArgumentError, "Selection needs a condition test block provided" unless block_given?
      @program = program
      @address = address || program.code.bytesize
      @codesize = codesize
      @selection = Selection.new(args, test)
    end

    ##
    #  Evaluates a block in an anonymous namespace if the condition evaluates to +true+.
    #  Returns an instance of ConditionalBlock.
    #
    #  _NOTE_:: The block must produce some code or raise an error without producing any code.
    #           Labels defined inside a block are not accessible from outside of the conditional block.
    def then(&block)
      raise Syntax, "`then' variant may be defined only once" unless @selection.then.nil?
      @selection.then = create_variant(&block)
      self
    end
 
    ##
    #  Evaluates a block in an anonymous namespace if the condition evaluates to +false+.
    #  Returns an instance of ConditionalBlock.
    #
    #  _NOTE_:: The block must produce some code or raise an error without producing any code.
    #           Labels defined inside a block are not accessible from outside of the conditional block.
    def else(&block)
      raise Syntax, "Only one of `else' or `else_select` variant may be defined" unless @selection.else.nil?
      raise Syntax, "`else' variant defined without `then'" if @selection.then.nil?
      @selection.else = create_variant(&block)
      self
    end
 
    ##
    #  Evaluates additional condition if the previous condition evaluates to +false+.
    #  Returns an instance of ConditionalBlock.
    #
    #  See Program.select.
    def else_select(*args, &test)
      raise Syntax, "Only one of `else' or `else_select` variant may be defined" unless @selection.else.nil?
      raise Syntax, "`else' variant defined without `then'" if @selection.then.nil?
      ConditionalBlock.new(@program, *args, address: @address, codesize: @codesize, &test).tap do |cond|
        @selection.else = cond.selection
      end
    end

    def ConditionalBlock.compile(cndblk, code, reloc, debug, start, override) # :nodoc:
      address = cndblk.address
      choice = cndblk.selection
      while choice.is_a?(Selection)
        args = choice.args.map {|a| a.to_i(start, override:override) }
        choice = if choice.test.call(*args)
          choice.then
        else
          choice.else
        end
      end 

      case choice
      when Variant
      when Exception
        raise choice
      when NilClass
        raise CompileError, "Undefined variant of a conditional block"
      else
        raise CompileError, "Invalid variant of a conditional block"
      end

      code[address, choice.code.bytesize] = choice.code

      unless choice.reloc.empty?
        addr = choice.reloc.first.addr
        offset = reloc.index {|r| addr < r.addr } || reloc.length
        reloc[offset, 0] = choice.reloc
      end

      unless choice.debug.empty?
        addr = choice.debug.first.addr
        offset = debug.index {|d| d.respond_to?(:addr) && addr < d.addr } || debug.length
        debug[offset, 0] = choice.debug
      end
    end

    private
    def create_variant(&block)
      code_offset = @program.code.bytesize
      unless code_offset == @address + @codesize.to_i
        raise Syntax, "conditional block variant must be defined immediately"
      end
      unless @codesize.nil?
        code_offset = @address
        @program.code.slice!(code_offset, @codesize)
      end
      reloc_offset = @program.reloc.length
      debug_offset = @program.debug.length
      error = nil
      @program.ns do |eoc|
        debug_ns_offset = @program.debug.length
        begin
          block.call eoc
        rescue StandardError => e
          if code_offset == @program.code.bytesize &&
             reloc_offset == @program.reloc.length &&
             debug_ns_offset == @program.debug.length
            @program.debug.slice!(debug_offset..-1)
            @program.code << "\x0"*@codesize.to_i
            error = e
          else
            raise e
          end
        end
      end
      return error if error
      variant = Variant.new @program.code[code_offset..-1]
      variant.reloc = @program.reloc.slice!(reloc_offset..-1)
      variant.debug = @program.debug.slice!(debug_offset..-1)
      if @codesize.nil?
        @codesize = variant.code.bytesize
      else
        unless @codesize == variant.code.bytesize
          raise Syntax, "conditional block variant code sizes differ"
        end
      end
      raise Syntax, "conditional block variant is empty" if @codesize.zero?
      variant
    end
  end

  module Program
    ##
    #  Creates a conditional block that creates alternative code
    #  based on the lazy evaluated boolean condition.
    #
    #  Returns an instance of ConditionalBlock.
    #
    #  Each argument should be a label or a label expression.
    #  Provide a block of code that computes a boolean value based on the evaluated
    #  label expressions.
    #
    #  _NOTE_:: Currently code produced by each variant must have the same number of bytes.
    #           Labels defined inside a variant are not accessible from outside of the conditional block.
    #
    #  Example:
    #
    #      select((io.ay_out ^ io.ay_sel) & 0xFF00, &:zero?).then do |eoc|
    #        ld   b, io.ay_out >> 8
    #      end.else_select((io.ay_out ^ io.ay_sel) & 0x00FF, &:zero?).then do |eoc|
    #        ld   c, io.ay_out
    #      end.else do
    #        raise ArgumentError, "ay_out and ay_sel should differ only on either 8-bit lsb or msb"
    #      end
    def select(*args, &test)
      ConditionalBlock.new(self, *args, &test).tap {|cndblk| @conditional_blocks << cndblk }
    end
  end
end
