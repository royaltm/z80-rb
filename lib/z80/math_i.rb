# -*- coding: BINARY -*-
module Z80
    ##
    # =Z80::MathInt - integer math common routines.
    #
    # in Z80::MathInt::Macros
    #
    #   require 'z80'
    #   
    #   class Program
    #     include Z80
    #     include Z80::TAP
    #                   macro_import  MathInt
    #   
    #                   ld   hl, [dividend.words[0]]
    #                   exx
    #                   push hl
    #                   ld   hl, [dividend.words[1]]
    #                   ld   de, [divisor]
    #                   divmod32_16
    #                   ld   [result.words[1]], hl
    #                   exx
    #                   ld   [result.words[0]], hl
    #                   ld   [remainder], de
    #                   # convert integer to bcd
    #                   utobcd bcdbufend, result, size: 4
    #                   exx
    #                   # print integer
    #                   bcdtoa hl, c, skip_leading0: true do
    #                     add ?0.ord
    #                     rst 0x10     # print a character
    #                   end
    #                   pop  hl
    #                   exx
    #                   ret
    #   
    #     dividend      int 32, 0xdeadbaca
    #     divisor       int 16, 0xbaba
    #     result        int 32, 0
    #     remainder     int 16, 0
    #                   bytes 5
    #     bcdbufend     label
    #   end
    class MathInt
        ##
        # =Z80::MathInt Integers
        #
        # This module holds integer data types created on the fly by the Macros.int macro.
        # The types differ only in byte size. The fields of each of the type are:
        #
        # * +bytes+:: This field provides access to individual bytes of the binary integer.
        # * +words+:: This field provides access to individual words of the binary integer, it's
        #             being defined only when the bit size of the type is a multiple of 16.
        #
        # E.g.:
        #                 ld   hl, [somebigint.words[1]]
        #                 ld   de, [somebigint.words[0]]
        #                 # hl|de now holds a 32-bit integer
        #                 # ...
        #    somebigint   int  32, -1
        module Integers
            class Int32 < Z80::Label # :nodoc:
                value  byte 4
                bytes  value byte, 4
                words  value word, 2
            end
        end
        ##
        # =Z80::MathInt Macros
        module Macros
            ## Returns the number of leading zeros in the binary representation of +m+.
            def leading_zeros(m, bitsize:8)
                raise ArgumentError, "leading_zeros: m is not an integer" unless Integer === m
                raise ArgumentError, "leading_zeros: m is out of range" if m < 0 || m >= (1<<bitsize)
                return bitsize if m.zero?
                zshift = 0
                mask = 1 << (bitsize - 1)
                while (m & (mask >> zshift)).zero?
                    zshift += 1
                end
                zshift
            end
            ## Returns the number of trailing zeros in the binary representation of +m+.
            def trailing_zeros(m, bitsize:8)
                raise ArgumentError, "trailing_zeros: m is not an integer" unless Integer === m
                raise ArgumentError, "trailing_zeros: m is out of range" if m < 0 || m >= (1<<bitsize)
                return bitsize if m.zero?
                zshift = 0
                while (m & (1 << zshift)).zero?
                    zshift += 1
                end
                zshift
            end
            ##
            # Packs an integer of an arbitrary size and adds it to the Program.code at Program.pc.
            # Returns an unnamed relative label. The label's type is one of the structures created
            # in the Integers module. The type depends on the +bitsize+ provided.
            #
            # Provided +bitsize+ must be a multiple of 8.
            # Integer data is being packed in the least significant byte first order, unless
            # +byteorder:+ +:msb+ option is given. In this instance it is being packed in
            # the most significant byte first order.
            def int(bitsize, value, byteorder: :lsb)
                raise ArgumentError, "int bitsize must be > 0" unless bitsize.is_a?(Integer) and bitsize > 0
                raise ArgumentError, "int bitsize must be multiple of 8" unless (bitsize & 7).zero?
                bytesize = bitsize >> 3
                klass_name = "Int#{bitsize}"
                int_klass = if Z80::MathInt::Integers.const_defined?(klass_name)
                    Z80::MathInt::Integers.const_get(klass_name)
                else
                    klass = Class.new(Z80::Label) do
                        value  byte bytesize
                        bytes  value byte, bytesize
                        words  value word, bytesize >> 1 if (bytesize & 1).zero?
                    end
                    Z80::MathInt::Integers.const_set klass_name, klass
                end
                bytes = []
                bytesize.times do
                    case byteorder
                    when :lsb then bytes.push(value & 0xFF)
                    when :msb then bytes.unshift(value & 0xFF)
                    else
                        raise ArgumentError, "byteorder must be :lsb or :msb"
                    end
                    value >>= 8
                end
                data int_klass, bytes.pack('c*'.freeze)
            end
            ##
            # Compares +va+ with +vb+ as twos complement signed 8-bit integers.
            #
            # Provide +va+ and +vb+ as an 8-bit registers, integers or labels.
            #
            # Options:
            # * +lt+: provide a label to jump to or +:ret+ when +va+ < +vb+.
            # * +gt+: provide a label to jump to or +:ret+ when +va+ > +vb+.
            # * +eq+: provide a label to jump to or +:ret+ when +va+ = +vb+.
            # * +jump_rel+: set to +true+ to use relative jumps wherever applicable.
            #
            # _NOTE_:: At least +lt+ or +gt+ must be provided.
            #          Only two of: +lt+, +gt+ and +eq+ may be specified.
            #
            # Modifies: +af+.
            def cmp_i8(va, vb, lt:nil, gt:nil, eq:nil, jump_rel:false)
                raise ArgumentError, "only va can be the accumulator" if vb == a
                raise ArgumentError, "lt, gt or eq should be labels if specified" unless (lt.nil? or lt == :ret or (address?(lt) and !pointer?(lt))) and
                                                                                         (gt.nil? or gt == :ret or (address?(gt) and !pointer?(gt))) and
                                                                                         (eq.nil? or eq == :ret or (address?(eq) and !pointer?(eq)))
                raise ArgumentError, "specify at least one of: lt or gt" if lt == gt or (!lt and !gt)
                raise ArgumentError, "specify at most two of: lt, gt or eq" if lt and gt and eq
                isolate do |eoc|
                                ld   a, va unless va == a
                                sub  vb
                    if eq == :ret
                                ret  Z
                    elsif jump_rel
                                jr   Z, eq || eoc
                    else
                                jp   Z, eq || eoc
                    end
                                jp   NV, skip_xor       # VF: 0, no XORing
                                xor  0x80               # SF: SF ^ VF
                    skip_xor    label
                    if lt == :ret
                                ret  M
                    elsif lt
                                jp   M, lt
                    end
                    if gt == :ret
                                ret  P
                    elsif gt
                                jp   P, gt
                    end
                end
            end
            ##
            # Compares a bitwise concatenated pair of 8-bit values +th+|+tl+ with a +value+
            # as twos complement signed 16-bit integers.
            #
            # Provide +value+ as an integer or a label.
            #
            # Options:
            # * +lt+: provide a label to jump to or +:ret+ when +th+|+tl+ < +value+.
            # * +gt+: provide a label to jump to or +:ret+ when +th+|+tl+ > +value+.
            # * +eq+: provide a label to jump to or +:ret+ when +th+|+tl+ = +value+.
            # * +jump_rel+: set to +true+ to use relative jumps wherever applicable.
            #
            # _NOTE_:: At least +lt+ or +gt+ must be provided.
            #          Only two of: +lt+, +gt+ and +eq+ may be specified.
            #
            # Modifies: +af+.
            def cmp_i16n(th, tl, value, lt:nil, gt:nil, eq:nil, jump_rel:false)
                cmp_i16r(th, tl, value>>8, value, lt:lt, gt:gt, eq:eq, jump_rel:jump_rel)
            end
            ##
            # Compares a bitwise concatenated pair of 8-bit values +th+|+tl+ with another
            # pair +sh+|+sl+ as twos complement signed 16-bit integers.
            #
            # Options:
            # * +lt+: provide a label to jump to or +:ret+ when +th+|+tl+ < +sh+|+sl+.
            # * +gt+: provide a label to jump to or +:ret+ when +th+|+tl+ > +sh+|+sl+.
            # * +eq+: provide a label to jump to or +:ret+ when +th+|+tl+ = +sh+|+sl+.
            # * +jump_rel+: set to +true+ to use relative jumps wherever applicable.
            #
            # _NOTE_:: At least +lt+ or +gt+ must be provided.
            #          Only two of: +lt+, +gt+ and +eq+ may be specified.
            #
            # Modifies: +af+.
            def cmp_i16r(th, tl, sh, sl, lt:nil, gt:nil, eq:nil, jump_rel:false)
                raise ArgumentError, "only th can be the accumulator" if [tl, sh, sl].include?(a)
                raise ArgumentError, "lt, gt or eq should be labels if specified" unless (lt.nil? or lt == :ret or (address?(lt) and !pointer?(lt))) and
                                                                                         (gt.nil? or gt == :ret or (address?(gt) and !pointer?(gt))) and
                                                                                         (eq.nil? or eq == :ret or (address?(eq) and !pointer?(eq)))
                raise ArgumentError, "specify at least one of: lt or gt" if lt == gt or (!lt and !gt)
                raise ArgumentError, "specify at most two of: lt, gt or eq" if lt and gt and eq
                gte = gt if gt == eq
                jump = proc do |cond, target|
                    if target == :ret
                                ret  cond
                    elsif jump_rel && (cond.nil? || cond.jr_ok?)
                                jr   cond, target
                    else
                                jp   cond, target
                    end
                end
                isolate do |eoc|
                                ld   a, th unless th == a
                    if sh == 0                          # 8bit optimization
                                ora  a
                    else
                                sub  sh
                    end
                                jump.call Z, equal_msb
                    unless sh == 0                      # 8bit optimization
                                jp   NV, no_xor         # VF: 0, no XORing
                                xor  0x80               # SF: SF ^ VF
                        no_xor  label
                    end
                                jump.call M, lt if lt
                                jump.call P, gt if gt
                                jump.call nil, eoc if !lt or !gt
                    equal_msb   ld   a, tl
                                cp   sl
                    if gte
                                jump.call NC, gte
                    elsif gt
                        if lt
                                jump.call C, lt
                                jump.call NZ, gt
                        else
                                jump.call Z, eq || eoc
                                jump.call NC, gt
                        end
                    else
                                jump.call Z, eq if eq
                                jump.call C, lt
                    end
                end
            end
            ##
            # Creates a routine that changes the sign of a twos complement 16-bit integer
            # depending on the content of the +sgn+.
            #
            # +sh+:: An 8-bit register holding MSB of the input 16-bit integer.
            # +sl+:: An 8-bit register holding LSB of the input 16-bit integer.
            # +sgn+:: An 8-bit sign register which must hold 0 or -1 (0xFF).
            #
            # Options:
            # * +th+:: An 8-bit MSB output register, may be the same as +sh+ or +sl+.
            # * +tl+:: An 8-bit LSB output register, may be the same as +sl+.
            # * +t+:: An 8-bit temporary register, which is used when +sgn+ is the accumulator.
            #
            # When +sgn+ equals to 0 an integer is left unmodified: +th+|+tl+ = +sh+|+sl+.
            # When +sgn+ equals to -1 an integer's sign is being changed: +th+|+tl+ = 0 - +sh+|+sl+.
            #
            # _NOTE_:: Other values of +sgn+ will render unexpected results.
            #
            # Uses: +af+, +th+, +tl+, preserves: +sgn+ and optionally: +sh+, +sl+.
            #
            # T-states: 32
            def twos_complement16_by_sgn(sh, sl, sgn, th:sh, tl:sl, t:sgn)
                if [sh, sl, tl, t].include?(a) or sh == sl or th == tl or sh == tl or
                   [sh, sl, th, tl].include?(sgn) or [sh, sl, th, tl].include?(t)
                    raise ArgumentError, "twos_complement16_by_sgn: invalid arguments!"
                end
                ns do
                    if sgn == a
                        sgn = t
                        ld    sgn, a
                    else
                        ld    a, sgn
                    end
                        xor   sl
                        sub   sgn
                        ld    tl, a
                        ld    a, sh
                        adc   a, sgn
                        xor   sgn
                        ld    th, a unless th == a
                end
            end
            ##
            # Creates a routine that changes the sign of a twos complement 16-bit integer in
            # +sh+|+sl+ or an 8-bit integer in +sl+ and extends it to a 16-bit integer.
            #
            # +sh+:: An 8-bit MSB of the 16-bit integer input, as a register, except +a+,
            #        a constant value or +nil+ to extend 8-bit integer in +sl+.
            # +sl+:: An 8-bit register holding LSB of the 16-bit integer input or an 8-bit
            #        integer input.
            #
            # Options:
            # * +th+:: An 8-bit MSB output register.
            # * +tl+:: An 8-bit LSB output register, except +a+ or +sh+.
            #
            # To extend an 8-bit integer in a register +e+ to a negative 16-bit in +hl+:
            #
            #    neg16(  0, e, th:h, tl:l) # e = positive or 0 (0..255)
            #    neg16( -1, e, th:h, tl:l) # e = negative, twos complement (-1..-256)
            #    neg16(nil, e, th:h, tl:l) # e = an 8-bit signed twos complement (-128..127)
            #
            # Uses: +af+, +th+, +tl+, preserves optionally: +sh+, +sl+.
            #
            # T-states (sh):
            #     reg,1,-1: 20|24, 0: 16|20, const: 23|27, nil: 27|28|31|32
            def neg16(sh, sl, th:sh, tl:sl)
                if [sh, tl].include?(a) or sh == sl or th == tl or sh == tl
                    raise ArgumentError, "neg16: invalid arguments!"
                end
                ns do
                    if sl == a
                        neg
                    else
                        xor  a
                        sub  sl
                    end
                        ld   tl, a
                    if sh.nil?
                        add  a, a
                        jr   Z, skip_ext # tl=0|128
                    end
                        sbc  a, a
                    case sh
                    when 0,nil
                    when 1
                        dec  a
                    when -1
                        inc  a
                    else
                        sub  sh
                    end
                    skip_ext label if sh.nil?
                        ld   th, a unless th == a
                end
            end
            ##
            # Creates a routine that changes the sign of a twos complement integer held in any number of +regs+.
            #
            # Pass any number of 8-bit registers, except the +accumulator+, as function arguments.
            # A register passed as the first argument should hold the most significant byte of the negated integer.
            #
            # Options:
            # * +t+:: A temporary 8-bit register used when the number of argument registers is 3 or more to
            #         slightly optimize the routine.
            # * +t_is_zero+:: Assume the +t+ register already holds 0. If +false+ the content of the +t+ register
            #                 will be set to 0.
            # * +optimize_last+:: Saves 3 T-states on the last iteration when +t+ is not being used.
            #                     This however renders the state of the CF flag useless.
            #
            # Integers are being processed starting from the least significant byte (the last argument).
            #
            # On completion the +accumulator+ holds a copy of the MSB register after the operation.
            #
            # The CF flag will hold the carry over state of the last operation and the SF flag the sign
            # of the resulting integer, unless +optimize_last+ is +true+.
            #
            # Modifies: +af+ and argument registers.
            #
            # T-states (register count):
            #     1: 12, 2: 24|27, 3: 36|39|40|42, 4: 48|52|54|57, 5: 60|64|69|72, 6: 84|87.
            def neg_int(*regs, t:nil, t_is_zero:false, optimize_last:false)
                raise ArgumentError, "neg_int invalid arguments!" if regs.empty? or regs.include?(a) or
                                                                   regs.any?{|r| !register?(r) || !r.bit8? } or
                                                                   regs.uniq.size != regs.size or
                                                                   !(t.nil? or [b,c,d,e,h,l].include?(t))
                t = 0 if t.nil? or regs.size < 3
                isolate do
                    regs.reverse_each.with_index do |reg, i|
                        if i.zero?
                            xor  a
                            ld   t, a if !t_is_zero && register?(t)
                            sub  reg
                        elsif optimize_last && (i == regs.size - 1) && !register?(t)
                            sbc  a, a
                            sub  reg
                        else
                            ld   a, t
                            sbc  reg
                        end
                            ld   reg, a
                    end
                end
            end
            ##
            # Creates a routine that extends a sign bit from an octet indicated by +s+ into a +t+.
            #
            # +t+:: A target 8-bit register or a pointer.
            # +s+:: An octet being sign-extended as an 8-bit register or a pointer.
            #
            # Uses: +af+, +t+, optionally preserves: +s+.
            #
            # T-states: 8|12|16.
            def sign_extend(t=a, s=a)
                isolate do
                            ld   a, s unless s == a
                            add  a, a
                            sbc  a, a
                            ld   t, a unless t == a
                end
            end
            ##
            # Creates a routine that adds an 8-bit accumulator value to a 16-bit value
            # in a +th+|+tl+ register pair.
            #
            # +th+:: An 8-bit register containing MSB bits of the 16-bit value.
            # +tl+:: An 8-bit register containing LSB bits of the 16-bit value.
            #
            # Options:
            # * +oh+:: An output of an 8-bit MSB part of the result, +th+ by default.
            # * +ol+:: An output of an 8-bit LSB part of the result, +tl+ by default.
            #
            # As a side effect: +a+ equals +oh+ when the routine ends.
            #
            # ====Note:
            # Although this method is often a more convenient way to add an 8-bit unsigned
            # integer to a 16-bit pair of registers, it sets flags in the following way:
            #
            #   ZF: (tt + a) & 0xFF00 == 0
            #   CF: (((tt + a) & 0xFF) + (((tt + a) & 0xFF00) >> 8)) > 0xFF
            #
            # Modifies: +af+, +oh+, +ol+.
            #
            # T-states: 16|20
            def adda_to(th, tl, oh:th, ol:tl)
                if th == tl or oh == ol or ol == th or  [th,tl,ol].include?(a)
                    raise ArgumentError, "adda_to: invalid arguments!"
                end
                ns do
                        add  tl
                        ld   ol, a
                        adc  th
                        sub  ol
                        ld   oh, a unless oh == a
                end
            end
            ##
            # Creates a routine that adds an 8-bit accumulator value to a 16-bit
            # immediate value and places the result in an +oh+|+ol+ register pair.
            #
            # +n+:: A 16-bit value as an address, a label, or an integer, and it must
            #       not be an intermediate pointer.
            #
            # Options:
            # * +oh+:: An output of an 8-bit MSB part of the result, +h+ by default.
            # * +ol+:: An output of an 8-bit LSB part of the result, +l+ by default.
            #
            # As a side effect: +a+ equals +oh+ when the routine ends.
            #
            # ====Note:
            # See Macros#adda_to.
            #
            # Modifies: +af+, +oh+, +ol+.
            #
            # T-states: 22|26
            def adda_to_n16(n, oh:h, ol:l)
                raise ArgumentError, "adda_to_n16: invalid n" unless address?(n) and !pointer?(n)
                th = n >> 8
                tl = n & 0xFF
                adda_to(th, tl, oh:oh, ol:ol)
            end
            ##
            # Creates a routine that subtracts an 8-bit +s+ register value from a 16-bit value
            # in a +th+|+tl+ register pair.
            #
            # +s+:: An 8-bit subtractor, except the accumulator.
            # +th+:: An 8-bit register containing MSB bits of the 16-bit value.
            # +tl+:: An 8-bit register containing LSB bits of the 16-bit value.
            #
            # Options:
            # * +oh+:: An output of an 8-bit MSB part of the result, +th+ by default.
            # * +ol+:: An output of an 8-bit LSB part of the result, +tl+ by default.
            #
            # As a side effect: +a+ equals to +oh+ when the routine ends.
            #
            # ====Note:
            # Although this method is often a more convenient way to subtract an 8-bit unsigned
            # register value from a 16-bit pair of registers, it does not set flags properly.
            #
            # Modifies: +af+, +oh+, +ol+.
            #
            # T-states: 20|24
            def sub_from(s, th, tl, oh:th, ol:tl)
                if th == tl or ol == th or [s,th,ol].include?(a)
                    raise ArgumentError, "sub_from: invalid arguments!"
                end
                ns do
                    ld   a, tl unless tl == a
                    sub  s
                    ld   ol, a
                    sbc  a
                    add  th
                    ld   oh, a unless oh == a
                end
            end
            ##
            # Creates a routine that subtracts an 8-bit +s+ register value from a 16-bit
            # immediate value and places the result in an +oh+|+ol+ register pair.
            #
            # +s+:: An 8-bit subtractor, except the accumulator.
            # +n+:: A 16-bit value as an address, a label, or an integer, and it must
            #       not be an intermediate pointer.
            #
            # Options:
            # * +oh+:: An output of an 8-bit MSB part of the result, +h+ by default.
            # * +ol+:: An output of an 8-bit LSB part of the result, +l+ by default.
            #
            # As a side effect: +a+ equals to +oh+ when the routine ends.
            #
            # ====Note:
            # See Macros#sub_from.
            #
            # Modifies: +af+, +oh+, +ol+.
            #
            # T-states: 26|30
            def sub_from_n16(s, n, oh:h, ol:l)
                raise ArgumentError, "sub_from_n16: invalid n" unless address?(n) and !pointer?(n)
                th = n >> 8
                tl = n & 0xFF
                sub_from(s, th, tl, oh:oh, ol:ol)
            end
            ##
            # Creates a routine that adds a 16-bit integer in +tt+ to a 24-bit integer in +th8+|+tl16+.
            # Returns the result in +a+|+tl16+.
            #
            # +th8+::  An 8-bit register holding the highest 8 bits of the value to be added to, must not be
            #          the +accumulator+.
            # +tl16+:: A 16-bit register holding the lowest 16 bits of the value to be added to (+hl+, +ix+ or +iy+).
            # +tt+::   A 16-bit register holding the value to be added (+bc+, +de+ or +sp+).
            #
            # Options:
            # * +signed+:: +true+ if the 16-bit value being added is a twos complement signed integer.
            #
            # Modifies: +af+, +tl16+, preserves: +th8+.
            #
            # T-states: 27 signed / 19 unsigned
            def add24_16(th8=c, tl16=hl, tt=de, signed:true)
                th, tl = tt.split
                tlh, tll = tl16.split
                if ![bc, de, sp].include?(tt) or ![hl, ix, iy].include?(tl16) or [tlh, tll, th, tl, a].include?(th8)
                    raise ArgumentError, "add24_16: invalid arguments!"
                end
                ns do
                    if signed
                        ld   a, th
                        add  a
                        sbc  a
                    else
                        sub  a
                    end
                        add  tl16, tt
                        adc  th8
                end
            end
            ##
            # Shift logical left 8-bit register with a result extended to 16-bits.
            #
            # +bshift+:: How many bits to shift the content of the +tl+ register.
            # +th+:: An 8-bit register, except the +accumulator+ for receiving the highest 8 bits
            #        of the result.
            # +tl+:: An 8-bit register, except the +accumulator+ for receiving the lowest 8 bits
            #        of the result.
            #
            # Modifies: +af+, +th+, +tl+, optionally preserves +a+, +sl+.
            #
            # Options:
            # * +sl+:: An 8-bit register holding the initial value to shift.
            # * +zero+:: An 8-bit register holding zero value or a literal 0. Specifying a register
            #            may save additional 3 or 7 T-states.
            #
            # T-states (bshift):
            #     0-7:  7|11, 18|22|23|27, 29|31|33|35, 35|39, 39|43, 35|39, 31|35, 23|27,
            #     8-15: 7|11, 15|19,       19|23,       23|27, 27|31, 22|26, 26|30, 30|34,
            #     >=16: 10|11
            def sll8_16(bshift, th=h, tl=l, sl:tl, zero:0)
                raise ArgumentError, "lshift8_16: invalid arguments!" unless Integer === bshift and bshift >= 0 and
                                                                         register?(th) and th.bit8? and th != a and
                                                                         register?(tl) and tl.bit8? and tl != a and
                                                                         register?(sl) and sl.bit8? and
                                                                         (zero == 0 ||
                                                                            (register?(zero) and zero.bit8? and
                                                                                (zero != sl)))
                isolate do
                    case bshift
                    when 0 # 0|4|7|11
                        if zero == tl && sl != th
                                    ld   th, zero
                                    ld   tl, sl unless sl == tl
                        else
                                    ld   tl, sl unless sl == tl
                                    zero = 0 if zero == tl
                                    ld   th, zero unless zero == th
                        end
                    when 1 # 18|22|23|27
                        tc1 = if th == h && tl == l then 22 else 1000 end
                        if sl == l then tc1 -= 4 end
                        if zero == h
                            tc1 -= 7
                        elsif (zero == l && sl != h) || (zero != 0 && zero != l)
                            tc1 -= 3
                        end
                        tc2 = if sl == a then 23 else 1000 end
                        if zero == th
                            tc2 -= 7
                        elsif zero != 0
                            tc2 -= 3
                        end
                        tc3 = 27
                        if sl == tl then tc3 -= 4 end
                        if zero == th
                            tc3 -= 7
                        elsif (zero == tl && sl != th) || (zero != 0 && zero != tl)
                            tc3 -= 3
                        end
                        tcmin = [tc1, tc2, tc3].min
                        if tcmin == tc1
                            # 11(+4|7)(+4)|15|18|19|22
                            if zero == l && sl != h
                                    ld   h, zero
                                    ld   l, sl unless sl == l
                            else
                                    ld   l, sl unless sl == l
                                    zero = 0 if zero == l
                                    ld   h, zero unless zero == h
                            end
                                    add  hl, hl
                        elsif tcmin == tc2 # 16|20|23
                                    ld   th, zero unless zero == th
                                    add  a, a
                                    rr   th
                                    ld   tl, a
                        else # 16(+4|7)(+4)|20|23|24|27
                            if zero == tl && sl != th
                                    ld   th, zero
                                    ld   tl, sl unless sl == tl
                            else
                                    ld   tl, sl unless sl == tl
                                    zero = 0 if zero == tl
                                    ld   th, zero unless zero == th
                            end
                                    sla  tl
                                    rr   th
                        end
                    when 2..6 # 29|31|33|35,35|39,39|43,35|39,31|35,(27|31)
                        if bshift == 2 && th == h && tl == l && 
                            (sl != a || ![0, l].include?(zero) || (zero == l && sl != h))
                            # 22(+4|7)(+4)|26|29|30|33
                            if zero == l && sl != h
                                    ld   h, zero
                                    ld   l, sl unless sl == l
                            else
                                    ld   l, sl unless sl == l
                                    zero = 0 if zero == l
                                    ld   h, zero unless zero == h
                            end
                                    add  hl, hl
                                    add  hl, hl
                        else
                                    ld   a, sl unless sl == a
                          if bshift > 4 # 35|39,31|35,(27|31)
                                    (8-bshift).times { rrca }
                                    ld   th, a
                                    anda (0xFF << bshift) & 0xFF
                                    ld   tl, a
                                    xor  th
                                    ld   th, a
                          else # 31|35,35|39,39|43
                                    bshift.times { rlca }
                                    ld   tl, a
                                    anda (1 << bshift) - 1
                                    ld   th, a
                                    xor  tl
                                    ld   tl, a
                          end
                        end
                    when 7..9 # 23|27,7|11,15|19
                        if zero == th && sl != tl
                                    ld   tl, zero
                                    ld   th, sl unless sl == th
                        else
                                    ld   th, sl unless sl == th
                                    zero = 0 if zero == th
                                    ld   tl, zero unless zero == tl
                        end
                        if bshift == 7
                                    srl  th
                                    rr   tl
                        elsif bshift == 9
                                    sla  th
                        end
                    when 10..12 # (15|19),19|23,23|27,27|31
                        if zero == a && sl != tl
                                    ld   tl, zero
                                    ld   a, sl unless sl == a
                        else
                                    ld   a, sl unless sl == a
                                    zero = 0 if zero == a
                                    ld   tl, zero unless zero == tl
                        end
                                    (bshift-8).times { add a }
                                    ld   th, a
                    when 13..15 # 22|26,26|30,30|34
                        if zero == a && sl != tl
                                    ld   tl, zero
                                    ld   a, sl unless sl == a
                        else
                                    ld   a, sl unless sl == a
                                    zero = 0 if zero == a
                                    ld   tl, zero unless zero == tl
                        end
                                    (16-bshift).times { rrca }
                                      anda (0xFF << (bshift-8)) & 0xFF
                                    ld   th, a
                    else # 4|8|10|11
                        if th.match16?(tl) && zero == 0
                                      ld   th|tl, 0
                        else
                                      ld   tl, zero unless zero == tl
                                      ld   th, tl unless zero == th
                        end
                    end
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 16-bit integer +kh+|+kl+ * 9-bit
            # signed integer +f+|+m+. Returns the 16-bit result in +hl+ or 17-bit result in +s+|+hl+.
            #
            # +kh+:: The MSB part of the multiplicand as an immediate value or an 8-bit register.
            # +kl+:: The LSB part of the multiplicand as an immediate value or an 8-bit register.
            # +m+::  The lowest 8-bits of the twos complement multiplier integer.
            #
            # The branching condition specified in +m_neg_cond+ determines the sign (bit 9th) of
            # a 9-bit twos complement multiplier.
            #
            # Options:
            # * +s+::          An optional sign output register which extends the result to 17 bits:
            #                  (-65536..65535). When +s+ is specified +m+ can't be +a+.
            #                  The twos complement value returned in +s+ can be either 0 for a positive
            #                  or -1 for a negative result.
            # * +tt+::         A 16-bit temporary register (+de+ or +bc+ unless +optimize+ is +:compact+).
            # * +m_neg_cond+:: A flags register branching condition indicating that +m+ is negative.
            #                  When +s+ is specified then +m_neg_cond+ can be +nil+ to indicate that
            #                  no check should be performed. In this instance enter the routine at
            #                  +posmul+ sub-label when +m+ >= 0 or at +negmul+ sub-label when +m+ < 0.
            # * +m_overflow+:: A program address to branch to when +m+ = (-256). When branched
            #                  +kh+|+kl+ will be already negated +k+ = (- +k+) and +a+, +CF+ = 0.
            #                  Alternatively +m_overflow+ can be set to +false+, implicating that
            #                  +m+ is never equal to (-256).
            # * +optimize+::   Optimization options: +:compact+, +:size+, +:time+ or +:unroll+.
            #                  _NOTE_: +:compact+ can't be selected if +s+ is +b+.
            #
            # Modifies: +af+, +hl+, +kh+, +kl+, +tt+, optionally +b+ if +optimize+ is +:compact+.
            def mul16_signed9(kh=h, kl=l, m=c, s:nil, tt:de, m_overflow:nil, m_neg_cond:C, optimize: :time)
                th, tl = tt.split
                raise ArgumentError, "mul16_signed9: invalid arguments" if [kh, kl].include?(m) or
                                                                    (!s.nil? and (m == a or
                                                                        [kh, kl, th, tl, h, l, m, a].include?(s) or
                                                                        !register?(m) or !m.bit8?))
                unless m_neg_cond.nil? and !s.nil?
                    raise ArgumentError, "mul16_signed9: m_neg_cond must be a Condition" unless m_neg_cond.is_a?(Condition)
                end
                if optimize == :compact and s == b
                    raise ArgumentError, "mul16_signed9: optimize can't be :compact with :s as b"
                end
                t = if register?(m) && m.bit8? && m != a
                    m
                else
                    [l, h, tl, th].find {|r| r != kh && r != kl}
                end
                jump = proc do |cond, target|
                    if optimize == :size || optimize == :compact
                            jr   cond, target
                    else
                            jp   cond, target
                    end
                end
                isolate do |eoc|
                    if s
                        if m_neg_cond.jr_ok?
                                jr   m_neg_cond, negmul
                        else
                                jp   m_neg_cond, negmul
                        end unless m_neg_cond.nil?
                        posmul  sign_extend(s, kh)
                                ld   a, m
                                anda a    # clear CF, test zero
                                jump.call NZ, mult
                                ld   s, a # clear sign
                                ld   h, a
                                ld   l, a
                                jump.call nil, eoc
                        negmul  neg16 kh, kl
                                sign_extend(s, a)
                    else
                                ld   a, m unless m == a
                        if m_neg_cond.jr_ok?        # m >= 0
                                jr   m_neg_cond.not, mult
                        else
                                jp   m_neg_cond.not, mult
                        end
                                ld   t, a unless t == m
                                neg16 kh, kl
                    end
                                xor  a
                                sub  t              # a: -m
                        if m_overflow
                                jr   NC, m_overflow # m == 256
                        elsif m_overflow.nil?
                                ccf                 # CF: 1 when m == 256, otherwise CF: 0
                        end
                        mult    mul16(kh, kl, a, tt:tt, mbit9_carry:m_overflow.nil?, optimize:optimize)
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 16-bit integer +kh+|+kl+ * 8-bit
            # signed +m+. Returns the result in +hl+.
            #
            # +kh+:: The MSB part of the multiplicand as an immediate value or an 8-bit register.
            # +kl+:: The LSB part of the multiplicand as an immediate value or an 8-bit register.
            # +m+::  An 8-bit multiplier.
            #
            # Options:
            # * +tt+::       A 16-bit temporary register (+de+ or +bc+ unless +optimize+ is +:compact+).
            # * +optimize+:: Optimization options: +:compact+, +:size+, +:time+ or +:unroll+.
            #
            # Modifies: +af+, +hl+, +tt+, optionally +b+ if +optimize+ is +:compact+.
            def mul16_signed(kh=h, kl=l, m=b, tt:de, optimize: :time)
                th, tl = tt.split
                raise ArgumentError, "mul16_signed: invalid arguments" if [kh, kl].include?(m)
                t = if register?(m) && m.bit8? && m != a
                    m
                else
                    [l, h, tl, th].find {|r| r != kh && r != kl}
                end
                isolate do
                            ld   a, m unless m == a
                            anda a
                            jp   P, mult        # m >= 0
                            ld   t, a unless t == m
                            neg16 kh, kl
                            xor  a
                            sub  t              # a: -m
                    mult    mul16(kh, kl, a, tt:tt, optimize:optimize)
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 16-bit integer +kh+|+kl+ * 8(9)-bit
            # unsigned +m+. Returns the result in +hl+.
            #
            # This routine is so far the most optimized of the similar routines: Macros.mul8 or Macros.mul.
            # However, there is no way to accumulate results using this code.
            #
            # +kh+::    The MSB part of the multiplicand as an immediate value or an 8-bit register.
            # +kl+::    The LSB part of the multiplicand as an immediate value or an 8-bit register.
            # +m+::     An 8-bit multiplier or the lowest 8-bits of a multiplier.
            #
            # If +mbit9_carry+ option is +true+ the CF flag should contain the most significant bit (bit 9th)
            # of a 9-bit unsigned multiplier.
            #
            # Options:
            # * +tt+::           A 16-bit temporary register (+de+ or +bc+ unless +optimize+ is +:compact+).
            # * +mbit9_carry+::  If the multiplier (+m+) is 9-bit, where MSB (9th bit) is read from CARRY flag.
            # * +optimize+::     Optimization options: +:compact+, +:size+, +:time+ or +:unroll+.
            #
            # _NOTE_:: Optimization +:compact+ is both a smaller and slightly faster alternative to +:size+,
            #          however it requires the loop register +b+, thus preventing +bc+ to be used as +tt.
            # Modifies: +af+, +hl+, +tt+, optionally +b+ if +optimize+ is +:compact+.
            def mul16(kh=h, kl=l, m=a, tt:de, mbit9_carry:false, optimize: :time)
                th, tl = tt.split
                raise ArgumentError, "mul16: invalid arguments" if tt == hl or [kh, kl].include?(a)
                raise ArgumentError, "mul16: tt must be +de+ if optimize is :compact" if tt == bc &&
                                                                                    optimize == :compact
                isolate do |eoc|
                                    ld   a, m  unless  m == a
                    if kh == tl
                        raise ArgumentError, "mul16: invalid arguments" if kl == th
                                    ld   th, kh
                                    ld   tl, kl unless kl == tl
                    else
                                    ld   tl, kl unless kl == tl
                                    ld   th, kh unless kh == th
                    end
                    case optimize
                    when :time
                                    ld   l, tl unless kl == l
                                    ld   h, th unless kh == h
                        if mbit9_carry
                                    jr   C, start9    # bit9 of m
                        end
                                    scf               # terminator
                                    adc  a, a         # CF <- m <- 1
                                    jr   C, cont1
                            loop0   add  a, a         # CF <- m <- 0
                                    jr   NC, loop0
                                    jp   NZ, cont1
                                    ld   h, a         # m == 0
                                    ld   l, a
                                    jp   eoc
                        if mbit9_carry
                            start9  adc  a, a         # CF <- m <- 1
                                    jr   C, doadd1
                        end
                            loop1   add  hl, hl       # hl * 2
                            cont1   add  a, a         # CF <- m <- 0
                                    jr   NC, loop1
                                    jr   Z, eoc
                            doadd1  add  hl, hl       # hl * 2
                                    add  hl, tt       # hl + tt
                                    add  a, a         # CF <- m <- 0
                                    jr   NC, loop1
                                    jp   NZ, doadd1
                    when :compact
                                    ld   hl, 0
                        if mbit9_carry
                                    ld   b, 9
                                    jr   cont1
                        else
                                    ld   b, 8
                        end
                            loop1   add  hl, hl       # hl * 2
                                    add  a, a         # CF <- m <- 0
                            cont1   jr   NC, skip1
                                    add  hl, tt       # hl + tt
                            skip1   djnz loop1
                    when :size
                                    ld   hl, 0
                        if mbit9_carry
                                    jr   NC, start8   # bit9 of m
                                    add  hl, tt
                        end
                        start8      scf               # terminator
                                    adc  a, a         # CF <- m <- 1
                                    jr   cont2
                            loop1   add  hl, hl       # hl * 2
                            cont1   add  a, a         # CF <- m <- 0
                            cont2   jr   NC, loop1
                                    jr   Z, eoc
                            doadd1  add  hl, hl       # hl * 2
                                    add  hl, tt       # hl + tt
                                    jr   cont1
                    when :unroll
                                    ld   l, tl unless kl == l
                                    ld   h, th unless kh == h
                        if mbit9_carry
                                    jr   C, iter0     # bit9 of m
                        end
                        7.times do |i|
                                    add  a, a
                                    jr   C, :"iter#{i+1}"
                        end
                                    add  a, a
                                    jr   C, eoc
                                    ld   h, a         # m == 0
                                    ld   l, a
                                    jp   eoc
                        (if mbit9_carry then 0 else 1 end..7).each do |i|
                            ns :"iter#{i}" do |eoc|
                                    add  hl, hl
                                    add  a, a
                                    jr   NC, eoc
                            doadd   add  hl, tt
                            end
                        end
                    else
                        raise ArgumentError, "mul16: optimize should be :compact, :size, :time or :unroll"
                    end
                end
            end
            ##
            # Creates a routine that performs a multiplication of a signed 8-bit +k+ * 8-bit signed +m+.
            #
            # See Macros.mul for details.
            def mul_signed(k=d, m=a, tt:de, clrhl:true, optimize: :time)
                th, tl = tt.split
                raise ArgumentError, "mul_signed: invalid arguments" if tt == hl or k == a
                isolate do
                            ld   a, m unless m == a
                            ld   th, k unless k == th
                            anda a
                            jp   P, mul_it      # m >= 0
                            ld   tl, a if m == a
                            xor  a
                            sub  th
                            ld   th, a
                            xor  a
                        if m == a
                            sub  tl
                        else
                            sub  m
                        end
                    mul_it  mul(th, a, tt:tt, clrhl:clrhl, signed_k:true, optimize:optimize)
                end
            end
            ##
            # Creates a routine that performs a multiplication of a signed 9-bit integer +kh+|+kl+ *
            # 9-bit signed integer +m+. Returns the result as a 17-bit integer in +s+|+hl+.
            #
            # The sign of a twos complement multiplicand +kl+ is expected in +kh+ extended to all bits.
            #
            # The sign of a twos complement multiplier +m+ is provided via the flags register.
            #
            # Optionally the ZERO flag (ZF) must signal +m+ equals to 0 if +m_is_zero_zf+ option is set.
            #
            # The sign of a twos complement result is returned in a register +s+.
            #
            # The result has only 17 bits capacity, thus multiplying (-256)x(-256) leads to overflow.
            #
            # To detect overflow check that all of the following conditions are met on result:
            #
            #   if (CF = 1) and (s == 0) and (hl == 0) then overflow is detected
            #
            # Alternatively provide an address or a label of a routine to enter on overflow as
            # +k_overflow+ option. In this instance the values of flags and registers will be
            # unspecified on overflow.
            #
            # +kh+::  An extended multiplicand sign as an immediate value or an 8-bit register.
            #         +kh+ is expected to be either equal to 0 or to 255 (twos complement -1).
            #         Other sign values will render <b>UNDEFINED BEHAVIOUR</b>.
            # +kl+::  Least significant 8-bits of a twos complement multiplicand as an immediate value
            #         or an 8-bit register.
            # +m+::   Least significant 8-bits of a twos complement multiplier as an immediate value
            #         or an 8-bit register.
            #
            # Options:
            # * +s+::            An 8-bit sign output register, preferably the same as +kh+.
            # * +tt+::           A 16-bit temporary register (+de+ or +bc+).
            # * +m_neg_cond+::   A flags register branching condition indicating that +m+ is negative.
            # * +k_full_range+:: Determines whether the multiplicand is allowed to be equal to (-256).
            #                    Saves 7 T-states and 18-21 bytes if +false+.
            # * +m_full_range+:: Determines whether the multiplier is allowed to be equal to (-256).
            #                    Saves 7 T-states and 6-9 bytes if +false+.
            # * +k_overflow+::   An address of a rountine to enter on overflow.
            #                    When +k_overflow+ is +true+ but +k_full_range+ is +false+ the routine
            #                    will be entered in case if +k+ = (-256) and +m+ is negative.
            #                    When +k_full_range+ and +k_overflow+ are enabled then the +m_full_range+
            #                    option must be also +true+.
            # * +m_is_zero_zf+:: Determines whether +ZF+ flag holds the condition whether the multiplier
            #                    +m+ is equal to 0 when the routine is entered.
            #                    Saves 4 T-states and 1 byte if +true+.
            # * +optimize+::     Optimization options: +:size+, +:time+ or +:unroll+.
            #
            # ====Note:
            # When +m_is_zero_zf+ option is +true+ and the +Z+ flag is set to indicate that +m+ is equal to 0,
            # then flags must also indicate positive +m+ when +m_neg_cond+ is checked (the condition must fail).
            # Otherwise the routine will result in <b>UNDEFINED BEHAVIOUR</b>.
            #
            # When +m_full_range+ option is +false+, then passing (-256) to +m+ will resolve in
            # <b>UNDEFINED BEHAVIOUR</b>.
            #
            # When +k_full_range+ option is +false+, then passing (-256) to +k+ and a negative value to +m+
            # will resolve in <b>UNDEFINED BEHAVIOUR</b> unless +k_overflow+ option is enabled.
            # In this instance the +k_overflow+ routine will be called when attempting to multiply (-256) by
            # a negative multiplier.
            #
            # For example if your multiplicand or a multiplier is calculated from a subtraction of 2 positive
            # 8-bit values, the resulting value never equals to (-256). Thus it is safe to disable the
            # corresponding +x_full_range+ option.
            #
            # Uses: +af+, +hl+, +tt+, +s+, optionally preserves: +kh+, +kl+ or +m+.
            def mul_signed9(kh, kl, m=a, s:kh, tt:de, m_neg_cond:C, k_full_range:true, m_full_range:true, k_overflow:nil, m_is_zero_zf:false, optimize: :time)
                if optimize == :size
                    th, tl = tt.split
                else
                    tl, th = tt.split
                end
                raise ArgumentError, "mul_signed9: invalid arguments" if tt == hl or kh == kl or
                                                                         [kh, kl].include?(a) or
                                                                         [th, tl, h, l, a].include?(s)
                raise ArgumentError, "mul_signed9: invalid options" if k_overflow and k_full_range and !m_full_range
                raise ArgumentError, "mul_signed9: m_neg_cond must be a Condition" unless m_neg_cond.is_a?(Condition)
                unless [:size, :time, :unroll].include?(optimize)
                    raise ArgumentError, "mul_signed9: optimize should be :size, :time or :unroll"
                end
                jump = proc do |cond, target|
                    if optimize == :size && (cond.nil? || cond.jr_ok?)
                            jr   cond, target
                    else
                            jp   cond, target
                    end
                end
                isolate do |eoc|
                            ld   a, m unless m == a
                    if th == kh
                        raise ArgumentError, "mul_signed9: invalid arguments" if s == kl
                            ld   s, kh unless s == kh
                            ld   th, kl unless kl == th
                    else
                            ld   th, kl unless kl == th
                            ld   s, kh unless s == kh
                    end
                            ld   hl, 0 if optimize == :size
                            # m < 0
                    if m_neg_cond.jr_ok?
                            jr   m_neg_cond, m_neg
                    else
                            jp   m_neg_cond, m_neg
                    end
                            anda a unless m_is_zero_zf
                            jump.call NZ, mul_it # m != 0
                    if k_full_range && m_full_range && !k_overflow && m_is_zero_zf && m_neg_cond != C
                            xor  a               # clear CF
                    end
                            ld   s, a            # clear sign
                    unless optimize == :size
                            ld   h, a
                            ld   l, a
                    end
                            jump.call nil, eoc   # CF=0

                    if k_full_range              # k = -(-256)
                    k_over  xor  a
                            ld   s, a            # clear sign (-256 * m) where m < 0
                            ld   l, a unless optimize == :size
                            sub  tl              # a = -m, CF = 1 (unless m == -256)
                        if m_full_range
                            # m == (-256)
                            jp   NC, k_overflow if k_overflow
                            ccf unless k_overflow # CF:1 when m == -256
                        end
                            ld   h, a            # hl: 256 * -m where m < 0
                            jump.call nil, eoc   # CF=0|1 depending on overflow unless k_overflow is set
                    end

                    if m_full_range
                    m_n256  ld   h, th           # hl: k * (-256)
                            ld   l, a unless optimize == :size
                            jump.call nil, eoc   # CF=0|1 depending on overflow unless k_overflow is set
                    end

                    m_neg   label
                            ld   tl, a unless m == tl
                            neg16 s, th, th:a # a: 0|FF depending on th == 0
                    if k_full_range
                            jr   C, k_over    # 0-0:CF=0, 0-FF:CF=1, FF-0:CF=0, FF-FF:CF=0
                    elsif k_overflow
                            jp   C, k_overflow
                    end
                            ld   s, a         # s|k = -(s|k)
                            xor  a
                            sub  tl           # a = -m
                    if m_full_range
                            jr   Z, m_n256    # m == (-256)
                    end
                    if optimize == :size
                    mul_it  sra  s            # k sign -> CF
                            ld   tl, l
                    mult    mul(th, a, tt:tt, clrhl:false, signed_k:true, kbit9_carry:true, tl_is_zero:true, optimize: :size)
                    else
                    mul_it  mul16(s, th, a, tt:tt, mbit9_carry:false, optimize:optimize)
                            anda a if k_full_range && m_full_range && !k_overflow # clear CF
                    end
                end
            end
            ##
            # Creates a routine that performs a multiplication of an 8(9)-bit integer +k+ * 8-bit
            # unsigned +m+. Returns the result as a 16-bit integer in +hl+.
            #
            # Optionally the result can be added to an existing value in +hl+.
            #
            # As a side-effect accumulator is always cleared to 0 and +m+ and +k+ are left unmodified
            # if they are not an: +accumulator+ nor part of +tt+.
            #
            # +k+::        A multiplicand as an immediate value or an 8-bit register.
            # +m+::        A multiplier as an immediate value or an 8-bit register.
            #
            # Options:
            # * +tt+::       A 16-bit temporary register (+de+ or +bc+).
            # * +clrhl+::    +true+ if the result should replace the +hl+ content, +false+ if the
            #                result should be added.
            # * +signed_k+:: If the multiplicand (+k+) represents a twos complement signed integer.
            # * +kbit9_carry+:: If the multiplicand (+k+) is 9-bit, where MSB (9th) bit is read from CARRY flag.
            # * +tl_is_zero+::  Whether +tl+ (LSB of +tt+) register has been already cleared.
            # * +optimize+:: What is more important: +:time+ or +:size+?
            #
            # Uses: +af+, +hl+, +tt+, optionally preserves: +k+, +m+.
            def mul(k=d, m=a, tt:de, clrhl:true, signed_k:false, kbit9_carry:false, tl_is_zero:false, optimize: :time)
                raise ArgumentError, "mul: invalid arguments" if tt == hl or k == a
                raise ArgumentError, "mul: optimize should be :time or :size" unless [:size, :time].include?(optimize)
                th, tl = tt.split
                srx = if signed_k
                    proc {|t| sra t}
                else
                    proc {|t| srl t}
                end
                isolate do |eoc|
                            ld   a, m unless m == a
                            ld   th, k unless k == th
                    if clrhl
                            ld   hl, 0
                            ld   tl, l unless tl_is_zero
                    else
                            ld   tl, 0 unless tl_is_zero
                    end
                    if kbit9_carry
                            rr   th
                        if optimize == :time
                            rr   tl
                            add  a, a
                            jr   NC, noadd9
                            add  hl, tt
                    noadd9  jr   Z, eoc
                        else
                            jr   cont9
                        end
                    elsif optimize == :time
                            srx[th]
                            rr   tl
                            add  a, a
                            jr   NC, noadd8
                            add  hl, tt
                    noadd8  jr   Z, eoc
                    end
                    loop1   srx[th]
                    cont9   rr   tl
                            add  a, a
                    if optimize == :time
                            jr   NC, loop1
                    else
                            jr   NC, noadd
                    end
                            add  hl, tt
                    if optimize == :time
                            jp   NZ, loop1
                    else
                    noadd   jr   NZ, loop1
                    end
                end
            end
            ##
            # Creates a routine that performs a multiplication of an 8-bit integer +k+ * 8-bit unsigned +m+.
            # Returns the result as a 16-bit integer in +hl+.
            #
            # Creates an optimized, unrolled code so +m+ should be a constant value in the range: 0..256.
            #
            # Optionally the result can be added to an existing value in +hl+.
            #
            # As a side-effect +k+ is left unmodified if +k+ is not part of +tt+ or +hl+.
            #
            # For those +m+ that has only one bit set or none, +tt+ is not being used.
            # For some of +m+ when +tt+ is +de+ the code can be better optimized as we may leverage
            # the usage of <tt>ex  de, hl</tt> instruction.
            #
            # +k+::        A multiplicand as an immediate value or an 8-bit register.
            # +m+::        A multiplier value as a constant integer.
            #
            # Options:
            # * +tt+::       A 16-bit temporary register (+de+ or +bc+).
            # * +clrhl+::    +true+ if the result should replace the +hl+ content, +false+ if the
            #                result should be added.
            # * +signed_k+:: If the multiplicand (+k+) represents a twos complement signed integer (-128..127).
            #
            # Uses: +f+, +hl+, +tt+, optionally preserves: +k+.
            def mul_const(k=d, m=0, tt:de, clrhl:true, signed_k:false)
                raise ArgumentError, "mul_const: invalid arguments" unless tt != hl and m.is_a?(Integer) and
                                                                           (0..256).include?(m)
                tt = hl if clrhl and (m & (m - 1)) == 0
                th, tl = tt.split
                if m == 0
                    return isolate do
                            ld   hl, 0 if clrhl
                    end
                end

                ml, tsl, clrhlt = m, 4+7, clrhl
                tsl += 19.5 if signed_k
                while (ml & 1) == 0
                    tsl += 11
                    ml >>= 1
                end if clrhlt
                just_cleared = false
                while tt != hl
                    if (ml & 1) != 0
                        tsl += if clrhlt
                            clrhlt = false
                            just_cleared = true
                            8
                        else
                            just_cleared = false
                            11
                        end
                    end
                    break if (ml >>= 1) == 0
                    if tt == de and ((ml & 1) == 0 or ml == 1 or just_cleared)
                        tsl += 4 unless just_cleared
                        tsl += 11
                        while (ml & 1) == 0
                            tsl += 11
                            ml >>= 1
                        end
                        tsl += 4 if ml != 1
                    else
                        tsl += 16
                    end
                end

                mr, tsr, clrhlt = m, 4+7, clrhl
                while mr != 0
                    if tt != hl and (mr & 0x100) != 0
                        tsr += if clrhlt
                            clrhlt = false
                            8
                        else
                            11
                        end
                    end
                    break if (mr & 0xFF) == 0
                    tsr += 16
                    mr <<= 1
                end

                isolate do
                    if tsl <= tsr
                        th, tl = hl.split if clrhl
                                ld   tl, k unless k == tl
                                ld   th, 0
                        if signed_k
                                bit  7, tl
                                jr   Z, sk_neg
                                dec  th
                        sk_neg  label
                        end
                        th, tl = tt.split if clrhl
                        while (m & 1) == 0
                            add  hl, hl
                            m >>= 1
                        end if clrhl
                        just_cleared = false
                        while tt != hl
                            if (m & 1) != 0
                                if clrhl
                                    clrhl = false
                                    just_cleared = true
                                    ld16 tt, hl
                                else
                                    just_cleared = false
                                    add  hl, tt
                                end
                            end
                            break if (m >>= 1) == 0
                            if tt == de and ((m & 1) == 0 or m == 1 or just_cleared)
                                ex   de, hl unless just_cleared
                                add  hl, hl
                                while (m & 1) == 0
                                    add  hl, hl
                                    m >>= 1
                                end
                                ex   de, hl if m != 1
                            else
                                sla  tl
                                rl   th
                            end
                        end
                    else
                                ld   th, k unless k == th
                                ld   tl, 0
                        while m != 0
                            if tt != hl and (m & 0x100) != 0
                                if clrhl
                                    clrhl = false
                                    ld16 hl, tt
                                else
                                    add  hl, tt
                                end
                            end
                            break if (m & 0xFF) == 0
                            if signed_k
                                sra  th
                            else
                                srl  th
                            end
                                rr   tl
                            m <<= 1
                        end
                    end
                end
            end
            ##
            # Creates a routine that performs a multiplication of an 8-bit integer +k+ * 8-bit
            # unsigned +m+. Returns the result as a 16-bit integer in +hl+.
            #
            # Creates an optimized, unrolled code so +m+ should be a constant value in the range: 0..256.
            #
            # As a side-effect +k+ is left unmodified if +k+ is not part of +tt+ or +hl+.
            #
            # For those +m+ that has only one bit set or none, +tt+ is not being used.
            #
            # This macro can produce faster routines compared to Macros.mul_const, but does not allow
            # to accumulate results.
            #
            # +k+::        A multiplicand as an immediate value or an 8-bit register.
            # +m+::        A multiplier value as a constant integer.
            #
            # Options:
            # * +tt+::       A 16-bit temporary register (+de+ or +bc+).
            # * +signed_k+:: If the multiplicand (+k+) represents a twos complement signed integer (-128..127).
            # * +use_a+::    Whether to allow modifying the +a+ register. For some values of +m+ the routine
            #                can be optimized further by utilizing the +accumulator+.
            # * +optimize+:: Optimization options: +:size+, +:time+ or a specific algorithm.
            #
            # Algorithm list for +optimize+:
            # * :k_rshift_sum
            # * :neg_k_rshift_sum
            # * :split_hik_lshift_sum
            # * :neg_split_hik_lshift_sum
            # * :split_lok_rshift_sum
            # * :sum_lshift_add_k
            # * :neg_sum_lshift_add_k
            #
            # Uses: +f+, +hl+, +tt+, optionally +a+, optionally preserves: +k+.
            def mul_const_ex(k=d, m=0, tt:de, signed_k:false, use_a:false, optimize: :time)
                raise ArgumentError, "mul_const_ex: invalid arguments" unless tt != hl and m.is_a?(Integer) and
                                                                              (0..256).include?(m)
                if m == 0
                    return isolate do
                        ld   hl, 0
                    end
                elsif m == 256
                    return isolate do
                        ld   h, k unless k == h
                        ld   l, 0
                    end
                end
                is_single_bit = (m & (m - 1)) == 0 # single bit only
                th, tl = tt.split
                srx = if signed_k
                    ->(x) { sra x }
                else
                    ->(x) { srl x }
                end
                # tricky optim:
                # if [rl, a].include?(k) this method should be called before k is modified
                sign_extend_k = proc do |rh, rl, s:k, use_a:false, zero:0|
                    ld   rl, s unless k == rl # not a BUG!
                    if signed_k
                        if use_a
                            sign_extend(rh, if k == a then a else rl end) # not a BUG!
                        else
                                ld   rh, zero
                                bit  7, rl
                                jr   Z, skip_neg
                                dec  rh
                    skip_neg    label
                        end
                    else
                        ld   rh, zero
                    end
                end
                # only 8-bit k
                k_rshift_sum = proc do |m|
                    ztshift = trailing_zeros(m)
                    zlshift = leading_zeros(m)
                    m >>= ztshift
                    mask = 0x80 >> (ztshift + zlshift + 1)
                    isolate do
                        ld   h, k unless k == h
                        ld   l, 0
                        (zlshift+1).times do
                            srx[h]
                            rr   l
                        end
                        ld16 tt, hl unless is_single_bit
                        while mask != 0
                            srx[th]
                            rr   tl
                            add  hl, tt unless (mask & m).zero?
                            mask >>= 1
                        end
                    end
                end
                # only 8-bit k
                neg_k_rshift_sum = proc do |m|
                    m = (-m) & 0xFF
                    ztshift = trailing_zeros(m)
                    m >>= ztshift
                    mask = 0x80 >> ztshift
                    isolate do
                        ld   h, k unless k == h # hl = k * 256
                        ld   l, 0
                        ld16 tt, hl
                        while mask != 0
                            srx[th]
                            rr   tl
                            sbc  hl, tt unless (mask & m).zero?
                            mask >>= 1
                        end
                    end
                end
                # only 8-bit k
                split_hik_lshift_sum = proc do |m, use_a|
                    zlshift = leading_zeros(m)
                    mask = 0x80 >> zlshift
                    isolate do
                        if use_a && !signed_k
                            if is_single_bit
                                sll8_16(7 - zlshift, h, l, sl:k)
                            else
                                ld   tl, k unless k == tl
                                ld   th, 0
                                sll8_16(7 - zlshift, h, l, sl: if k == th then tl else k end, zero:th)
                            end
                        else
                            ld   h, k unless k == h
                            ld   l, 0
                            sign_extend_k.call(th, tl, s:h, use_a:use_a, zero:l) unless is_single_bit
                            (zlshift+1).times do
                                srx[h]
                                rr   l
                            end
                        end
                        m ^= mask
                        mask = 1
                        until m.zero?
                            unless (mask & m).zero?
                                add  hl, tt
                                m ^= mask
                                break if m.zero?
                            end
                            sla  tl
                            rl   th
                            mask <<= 1
                        end
                    end
                end
                # 16-bit k supported
                neg_split_hik_lshift_sum = proc do |m|
                    m = (-m) & 0xFF
                    isolate do
                        ld   h, k unless k == h
                        ld   l, 0
                        sign_extend_k.call th, tl, s:h, zero:l
                        mask = 1
                        until m.zero?
                            unless (mask & m).zero?
                                anda a if signed_k || mask == 1
                                sbc  hl, tt
                                m ^= mask
                                break if m.zero?
                            end
                            sla  tl
                            rl   th
                            mask <<= 1
                        end
                    end
                end
                # only 8-bit k
                split_lok_rshift_sum = proc do |m, use_a|
                    ztshift = trailing_zeros(m)
                    m >>= ztshift
                    mask = 0x80 >> ztshift
                    isolate do
                        if use_a && !signed_k
                            if is_single_bit
                                sll8_16(ztshift, h, l, sl:k)
                            else
                                ld   th, k unless k == th
                                ld   tl, 0
                                sll8_16(ztshift, h, l, sl: if k == tl then th else k end, zero:tl)
                            end
                        else
                            sign_extend_k.call h, l, use_a:use_a
                            unless is_single_bit
                                ld   th, l unless k == th # not a BUG!
                                ld   tl, if signed_k then 0 else h end
                            end
                            ztshift.times do
                                add  hl, hl
                            end
                        end
                        m ^= 1
                        until m.zero?
                            srx[th]
                            rr  tl
                            unless (mask & m).zero?
                                add  hl, tt
                                m ^= mask
                            end
                            mask >>= 1
                        end
                    end
                end
                # 16-bit k supported
                sum_lshift_add_k = proc do |m, use_a|
                    ztshift = trailing_zeros(m)
                    zlshift = leading_zeros(m)
                    m >>= ztshift
                    mask = 0x80 >> (ztshift + zlshift + 1)
                    isolate do
                        if ztshift.zero?
                            if k == tl && !is_single_bit
                                sign_extend_k.call th, tl, use_a:use_a
                                ld16 hl, tt
                            else
                                sign_extend_k.call h, l, use_a:use_a
                                ld16 tt, hl unless is_single_bit
                            end
                        else
                            if use_a && !signed_k
                                sll8_16(ztshift, h, l, sl:k)
                            else
                                sign_extend_k.call h, l, use_a:use_a
                                ztshift.times do
                                    add  hl, hl
                                end
                            end
                            ld16 tt, hl unless is_single_bit
                        end
                        while mask != 0
                                add  hl, hl
                            unless (mask & m).zero?
                                add  hl, tt
                            end
                            mask >>= 1
                        end
                    end
                end
                # 16-bit k supported
                neg_sum_lshift_add_k = proc do |m, use_a|
                    m = (-m) & 0xFF
                    ztshift = trailing_zeros(m)
                    zlshift = leading_zeros(m)
                    m >>= ztshift
                    mask = 0x80 >> ztshift
                    is_single_bit = (m & (m - 1)) == 0 # single bit only
                    isolate do
                        if use_a
                            mask >>= zlshift + 1
                            kk = if [a, h, l].include?(k)
                                ld   tl, k
                                tl
                            else
                                k
                            end
                            if ztshift < 5 || signed_k
                                neg16(unless signed_k then 0 end, k, th:h, tl:l)
                                ztshift.times do
                                    add  hl, hl
                                end
                            else # 224, 192, 160, 128, 96, 64, 32
                                sll8_16(ztshift, h, l, sl:k)
                                neg16(h, l)
                            end
                            ld   a, kk
                            ld16 tt, hl unless is_single_bit
                        else
                            if ztshift.zero? && k == tl
                                sign_extend_k.call th, tl
                                ld16 hl, tt
                            else
                                sign_extend_k.call h, l
                                ztshift.times do
                                    add  hl, hl
                                end
                                ld16 tt, hl
                            end
                        end
                        while mask != 0
                                add  hl, hl
                          unless (mask & m).zero?
                            if use_a
                                add  hl, tt
                            else
                                anda a if signed_k
                                sbc  hl, tt
                            end
                          end
                          mask >>= 1
                        end
                        if use_a
                            add  a, h
                            ld   h, a
                        end
                    end
                end
                algorithms = {
                    k_rshift_sum: k_rshift_sum,
                    neg_k_rshift_sum: neg_k_rshift_sum,
                    split_hik_lshift_sum: split_hik_lshift_sum,
                    neg_split_hik_lshift_sum: neg_split_hik_lshift_sum,
                    split_lok_rshift_sum: split_lok_rshift_sum,
                    sum_lshift_add_k: sum_lshift_add_k,
                    neg_sum_lshift_add_k: neg_sum_lshift_add_k,
                }
                # the choice made here may be slightly off, depending on what k is selected
                # some k choices can benefit some algos, while punishing others
                algo, with_a = case optimize
                when :time
                    if signed_k
                        if use_a
                            case m
                            when 224
                                [:neg_k_rshift_sum, false]
                            when 32,64,80,96,112,128,144,152,160,168,176,192,208
                                [:k_rshift_sum, false]
                            when 33,65,67,129,131,133..135,137..143
                                [:split_hik_lshift_sum, true]
                            when 66,68,97..98,130,132,136,145,161..162,164,177,193..194,196,200,209,225..226
                                [:split_lok_rshift_sum, true]
                            when 126..127,158..159,174..175,182..184,186..191,199,203..207,211..223,227..255
                                [:neg_sum_lshift_add_k, true]
                            else
                                [:sum_lshift_add_k, true]
                            end
                        else
                            case m
                            when 127,191
                                [:neg_sum_lshift_add_k, false]
                            when 120,124,184,188,190,216,220,224,232,240
                                [:neg_k_rshift_sum, false]
                            when 33,65,67,69..71,129,131,133..135,137..143
                                [:split_hik_lshift_sum, false]
                            when 66,68,97..98,130,132,136,145,161..162,164,177,193..194,196,209,225..226,228,241
                                [:split_lok_rshift_sum, false]
                            when 222..223,231,235..239,242..255
                                [:neg_split_hik_lshift_sum, false]
                            when 32,48,64,72,80,88,96,104,112,128,144,148,152,156,160,168,172,176,180,192,200,204,208,212
                                [:k_rshift_sum, false]
                            else
                                [:sum_lshift_add_k, false]
                            end
                        end
                    else
                        if use_a
                            case m
                            when 176,192,208
                                [:k_rshift_sum, false]
                            when 9,17,33..35,65,67,69..71,129,131,133..135,137..143
                                [:split_hik_lshift_sum, true]
                            when 66,68,81,97..98,113,130,132,136,144..146,160..162,164,177..178,193..194,196,200,209..210,225..226
                                [:split_lok_rshift_sum, true]
                            when 95,111,119,123..127,151,155..159,167,171..175,179..191,198..199,202..207,211..224,227..255
                                [:neg_sum_lshift_add_k, true]
                            else
                                [:sum_lshift_add_k, true]
                            end
                        else
                            case m
                            when 224,240
                                [:neg_k_rshift_sum, false]
                            when 126..127,188,190..191
                                [:neg_sum_lshift_add_k, false]
                            when 32,64,80,96,112,128,160,176,192,208
                                [:k_rshift_sum, false]
                            when 33,65,67,69..71,129,131,133..135,137..143
                                [:split_hik_lshift_sum, false]
                            when 66,68,72,81,97..98,113,130,132,136,144..146,161..162,164,177..178,193..194,196,200,209..210,225..226,228,241
                                [:split_lok_rshift_sum, false]
                            when 207,215,219..223,230..232,234..239,242..255
                                [:neg_split_hik_lshift_sum, false]
                            else
                                [:sum_lshift_add_k, false]
                            end
                        end
                    end
                when :size
                    if signed_k
                        if use_a
                            case m
                            when 129
                                [:split_hik_lshift_sum, true]
                            when 130,132
                                [:split_lok_rshift_sum, true]
                            when 64,128,192
                                [:k_rshift_sum, false]
                            when 191,222..224,231,235..240,242..255
                                [:neg_sum_lshift_add_k, true]
                            else
                                [:sum_lshift_add_k, true]
                            end
                        else
                            case m
                            when 224
                                [:neg_k_rshift_sum, false]
                            when 248
                                [:neg_sum_lshift_add_k, false]
                            when 129
                                [:split_hik_lshift_sum, false]
                            when 130,132
                                [:split_lok_rshift_sum, false]
                            when 252..255
                                [:neg_split_hik_lshift_sum, false]
                            when 64,128,160,192
                                [:k_rshift_sum, false]
                            else
                                [:sum_lshift_add_k, false]
                            end
                        end
                    else
                        if use_a
                            case m
                            when 129
                                [:split_hik_lshift_sum, true]
                            when 136
                                [:split_lok_rshift_sum, false]
                            when 127
                                [:neg_sum_lshift_add_k, false]
                            when 130,132
                                [:split_lok_rshift_sum, true]
                            when 8,16,24,32,40,48,56,72,80,88,96,104,112,120,144,152,160,168,176,184,200,208,216,224
                                [:sum_lshift_add_k, false]
                            when 190..191,207,215,219..223,230..232,234..255
                                [:neg_sum_lshift_add_k, true]
                            else
                                [:sum_lshift_add_k, true]
                            end
                        else
                            case m
                            when 129
                                [:split_hik_lshift_sum, false]
                            when 128
                                [:k_rshift_sum, false]
                            when 130,132,136
                                [:split_lok_rshift_sum, false]
                            when 250,252..255
                                [:neg_split_hik_lshift_sum, false]
                            when 127,190..191,222..223,238..240,246..248,251
                                [:neg_sum_lshift_add_k, false]
                            else
                                [:sum_lshift_add_k, false]
                            end
                        end
                    end
                else
                    [optimize, use_a]
                end
                algo_impl = algorithms[algo]
                raise ArgumentError, "mul_const_ex: unsupported algorithm in :optimize" if algo_impl.nil?
                algo_impl.call(m, with_a)
                # case optimize
                # when :time
                #     algo = algorithms.each_value.map do |algo|
                #         tsc = Z80::TStatesCounter.new
                #         tsc.instance_exec(m, use_a, &algo)
                #         [tsc.to_i, algo]
                #     end.min_by {|i| i.first}[1]
                # when :size
                #     algo = algorithms.each_value.map do |algo|
                #         csc = Z80::CodeSizeCounter.new
                #         csc.instance_exec(m, use_a, &algo)
                #         [csc.to_i, algo]
                #     end.min_by {|i| i.first}[1]
                # else
                #     algo = algorithms[optimize]
                #     raise ArgumentError, "mul_const_ex: unsupported algorithm in :optimize" if algo.nil?
                #     algo.call(m, use_a)
                # end
            end
            ##
            # Creates a routine that performs a multiplication of an unsigned 16-bit integer +kh+|+kl+ *
            # 8-bit unsigned +m+. Returns the result as a 16-bit unsigned integer in +hl+.
            #
            # Optionally the result can be added to an existing value in +hl+.
            #
            # Breaks on overflow with +CF+=1.
            # 
            # As a side-effect +m+ is cleared to 0 when CF=0 and +kl+ and +kh+ are left unmodified
            # if they are not part of +tt+.
            #
            # +kh+::    The MSB part of the multiplicand as an immediate value or an 8-bit register.
            # +kl+::    The LSB part of the multiplicand as an immediate value or an 8-bit register.
            # +m+::     An 8-bit multiplier register, must not be a part of the +tt+.
            #
            # Options:
            # * +tt+::    A 16-bit temporary register (+de+ or +bc+).
            # * +clrhl+:: +true+ if the result should replace the +hl+ content, +false+ if the result
            #             should be added.
            #
            # Uses: +f+, +hl+, +m+, +tt+, optionally preserves: +kh+, +kl+.
            def mul8_c(kh=h, kl=l, m=a, tt:de, clrhl:true)
                th, tl = tt.split
                raise ArgumentError, "mul8_c: invalid arguments" if tt == hl or [th, tl].include?(m) or
                                                                    !register?(m) or !m.bit8?
                isolate do |eoc|
                    if kh == tl
                        raise ArgumentError, "mul8_c: invalid arguments" if kl == th
                            ld   th, kh
                            ld   tl, kl unless kl == tl
                    else
                            ld   tl, kl unless kl == tl
                            ld   th, kh unless kh == th
                    end
                            ld   hl, 0 if clrhl
                    loop1   srl  m
                            jr   NC, noadd
                            add  hl, tt
                            jr   C, eoc
                    noadd   jr   Z, eoc
                    skip1   sla  tl
                            rl   th
                            jp   NC, loop1
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 16-bit signed integer +kh+|+kl+ *
            # 8bit signed integer +m+. Returns the result as a 16-bit integer in +hl+.
            #
            # Optionally the result can be added to an existing value in +hl+.
            #
            # Optionally multiplies the result by 2 if +double+ option is +true+.
            #
            # As a side-effect +t+ is always cleared to 0 and +kl+ and +kh+ are left unmodified
            # if they are not part of +tt+ and +m+ is left unmodified if it's not +a+ or the same as +t+.
            #
            # +kh+::    The MSB part of the multiplicand as an immediate value or an 8-bit register.
            # +kl+::    The LSB part of the multiplicand as an immediate value or an 8-bit register.
            # +m+::     An 8-bit multiplier register.
            #
            # Options:
            # * +tt+::  A 16-bit temporary register (+de+ or +bc+).
            # * +t+::   An 8-bit temporary register, it must not be the +accumulator+ nor be a part of the +tt+.
            # * +clrhl+:: +true+ if the result should replace the +hl+ content, +false+ if the result
            #             should be added.
            # * +double+:: +true+ if the result should be multiplied by 2 (+kh+|+kl+ * 2).
            # * +optimize+:: What is more important: +:time+ or +:size+?
            #
            # Uses: +af+, +hl+, +m+, +kh+, +kl+, +t+, +tt+, optionally preserves: +kh+, +kl+, +m+.
            def mul8_signed(kh=h, kl=l, m=c, tt:de, t:m, clrhl:true, double:false, optimize: :time)
                th, tl = tt.split
                raise ArgumentError, "mul8_signed: invalid arguments" if !register?(t) or !t.bit8? or
                                                                         [a, tl, th].include?(t) or
                                                                         [kh, kl].include?(a)

                isolate do
                            ld   a, m unless m == a
                    if kh == tl
                        raise ArgumentError, "mul8_signed: invalid arguments" if kl == th
                            ld   th, kh
                            ld   tl, kl unless kl == tl
                    else
                            ld   tl, kl unless kl == tl
                            ld   th, kh unless kh == th
                    end
                            anda a
                            jp   P, mul_it      # m >= 0
                            ld   t, a unless t == m
                            neg16 th, tl
                            xor  a
                            sub  t              # a: -m
                    mul_it  mul8(th, tl, a, tt:tt, clrhl:clrhl, double:double, optimize:optimize)
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 16-bit integer +kh+|+kl+ *
            # 8bit unsigned +m+. Returns the result as a 16-bit integer in +hl+.
            #
            # Optionally the result can be added to an existing value in +hl+.
            #
            # Optionally multiplies the result by 2 if +double+ option is +true+.
            #
            # As a side-effect +m+ is always cleared to 0 and +kl+ and +kh+ are left unmodified
            # if they are not part of +tt+.
            #
            # +kh+::    The MSB part of the multiplicand as an immediate value or an 8-bit register.
            # +kl+::    The LSB part of the multiplicand as an immediate value or an 8-bit register.
            # +m+::     An 8-bit multiplier register, must not be a part of the +tt+.
            #
            # Options:
            # * +tt+::    A 16-bit temporary register (+de+ or +bc+).
            # * +clrhl+:: +true+ if the result should replace the +hl+ content, +false+ if the result
            #             should be added.
            # * +double+:: +true+ if the result should be multiplied by 2 (+kh+|+kl+ * 2).
            # * +optimize+:: What is more important: +:time+ or +:size+?
            #
            # Uses: +f+, +hl+, +m+, +kh+, +kl+, +tt+, optionally preserves: +kh+, +kl+.
            def mul8(kh=h, kl=l, m=a, tt:de, clrhl:true, double:false, optimize: :time)
                raise ArgumentError, "mul8: optimize should be :time or :size" unless [:size, :time].include?(optimize)
                th, tl = tt.split
                raise ArgumentError, "mul8: invalid arguments" if tt == hl or [th, tl].include?(m) or
                                                                  !register?(m) or !m.bit8?
                isolate do |eoc|
                    if kh == tl
                        raise ArgumentError, "mul8: invalid arguments" if kl == th
                            ld   th, kh
                            ld   tl, kl unless kl == tl
                    else
                            ld   tl, kl unless kl == tl
                            ld   th, kh unless kh == th
                    end
                            ld   hl, 0 if clrhl
                    if optimize == :time
                        if double
                            sla  tl
                            rl   th
                        end
                            srl  m
                            jr   C, doadd
                            jr   Z, eoc
                    elsif !double
                            jr   muls1
                    end
                    loop1   sla  tl
                            rl   th
                    muls1   srl  m
                    if optimize == :time
                            jr   NC, loop1
                    else
                            jr   NC, noadd
                    end
                    doadd   add  hl, tt
                    if optimize == :time
                            jp   NZ, loop1
                    else
                    noadd   jr   NZ, loop1
                    end
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 24-bit signed integer
            # +ks+|+kh+|+kl+ * 9-bit signed integer +m+.
            # Returns the result as a 24-bit integer in +a+|+hl+.
            #
            # The sign of a twos complement multiplier +m+ is provided via the flags register.
            #
            # Optionally the result can be added to an existing value in +a+|+hl+.
            #
            # +ks+::  The MS 8-bits of the multiplicand as an immediate value or an 8-bit register.
            # +kh+::  Bits 8-15 of the multiplicand as an immediate value or an 8-bit register.
            # +kl+::  The LS 8-bits of the multiplicand as an immediate value or an 8-bit register.
            # +m+::   The LS 8-bits of the multiplier as an 8-bit register, it must not be the +accumulator+.
            #
            # Options:
            # * +tt+::     A 16 bit temporary register (+de+ or +bc+).
            # * +m_pos_cond+::   A flags register branching condition indicating that +m+ is positive (including 0).
            # * +m_full_range+:: Determines whether the multiplier is allowed to be equal to (-256).
            #                    Saves 10-12 T-states and 7-9+ bytes if disabled.
            # * +optimize+:: What is more important: +:time+ or +:size+?
            #
            # If a block of code (+restore_a+) is present, +a+|+hl+ registers are not cleared. Provide
            # instructions to restore +a+ and optionally +hl+ registers, thus the result will be added:
            #
            #   a|hl += k * m.
            # 
            # * The block of code must not modify the content of +tt+, +t+ or +m+ registers.
            # * There is no need to restore +hl+ registers as they are not modified before the +restore_a+
            #   instructions are executed.
            # * The +restore_a+ block is inserted twice if the +optimize+ option is +:time+ and +m_full_range+
            #   is +true+.
            #
            # When +m_full_range+ option is disabled, then passing (-256) to +m+ will resolve in
            # <b>UNDEFINED BEHAVIOUR</b>.
            #
            # Uses: +af+, +bc+, +de+, +hl+.
            def mul_signed9_24(ks=c, kh=h, kl=l, m=b, tt:de, m_pos_cond:NC, m_full_range:true, optimize: :time, &restore_a)
                th, tl = tt.split
                raise ArgumentError, "mul_signed9_24: invalid arguments" if tt == hl or [th, tl, ks, h, l, a].include?(m) or
                                                                            kh == kl or [th, tl, m, h, l, a].include?(ks)
                raise ArgumentError, "mul_signed9_24: m_pos_cond must be a Condition" unless m_pos_cond.is_a?(Condition)
                clrahl = !block_given?
                jump = proc do |cond, target|
                    if optimize == :size && (cond.nil? || cond.jr_ok?)
                            jr   cond, target
                    else
                            jp   cond, target
                    end
                end
                isolate do |eoc|
                    if kh == tl
                        raise ArgumentError, "mul_signed9_24: invalid arguments" if kl == th
                            ld   th, kh
                            ld   tl, kl unless kl == tl
                    else
                            ld   tl, kl unless kl == tl
                            ld   th, kh unless kh == th
                    end
                    if m_pos_cond.jr_ok?
                            jr   m_pos_cond, mul_it # m >= 0
                    else
                            jp   m_pos_cond, mul_it # m >= 0
                    end
                            neg_int ks, th, tl, optimize_last:true

                            xor  a
                            sub  m
                            ld   m, a
                    if m_full_range
                            jump.call NZ, mul_it # m != (-256)
                        if clrahl
                            ld   l, a             # a: 0
                            ld   h, tl
                            add  a, th            # set flags
                            jump.call nil, eoc
                        else
                            ld   ks, th
                            ld   th, tl
                            ld   tl, a            # a: 0
                            if optimize == :size
                                ld   m, 1         # m: 1
                            elsif optimize == :time
                                ns(&restore_a)
                                jump.call nil, mult.skadd
                            end
                        end
                    end
                    mul_it  label
                            ns(&restore_a) unless clrahl
                    mult    mul8_24(th, tl, m, t:ks, tt:tt, clrahl:clrahl, k_int24:true, optimize:optimize)
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 16-bit unsigned integer +kh+|+kl+ or
            # a 24-bit integer +t+|+kh+|+kl+ * 8(9)-bit unsigned +m+.
            # Returns the result as a 24-bit integer in +a+|+hl+.
            #
            # Optionally the result can be added to an existing value in +a+|+hl+.
            #
            # +kh+::     The MSB part of the multiplicand as an immediate value or an 8-bit register.
            # +kl+::     The LSB part of the multiplicand as an immediate value or an 8-bit register.
            # +m+::      A multiplier register, except +a+, +t+, +h+, +l+ or a part of +tt+.
            #
            # Options:
            # * +t+::      An 8-bit temporary register, except +a+, +m+, +h+, +l+ or a part of +tt+.
            # * +tt+::     A 16 bit temporary register (+de+ or +bc+).
            # * +clrahl+:: +true+ if the result should replace the +a+|+hl+ content, +false+ if the result 
            #              should be added.
            # * +k_int24+:: Whether a multiplicand is a 24-bit (possibly signed) integer with its MSB bits
            #               in +t+.
            # * +mbit9_carry+:: If the multiplier (+m+) is 9-bit, where MSB (9th bit) is read from the
            #                   CARRY flag.
            # * +optimize+:: What is more important: +:time+ or +:size+?
            #
            # On exit, the SF flag contains the sign of the 24-bit result.
            #
            # If +k_int24+ is +false+ and +clrahl+ is +true+ the resulting flags: ZF=1 signals the result 
            # fits in 16 bits, and CF=1 indicates overflow (only possible with 9-bit +m+).
            #
            #   bytes|~avg T-states|max T-states
            #
            #        k_int24    false           true          false          true
            #    mbit9_carry    false          false           true          true
            #  optimize
            #   :size        24|~466.5|569  23|~462.5|565  29|~548.2|656  28|~544.2|652
            #   :time        33|~385.4|504  32|~381.4|500  34|~441.2|570  33|~437.2|566
            #
            # Uses: +af+, +bc+, +de+, +hl+.
            def mul8_24(kh=h, kl=l, m=b, t:c, tt:de, clrahl:true, k_int24:false, mbit9_carry:false, optimize: :time)
                th, tl = tt.split
                raise ArgumentError, "mul8_24: invalid arguments" if tt == hl or [a, h, l, th, tl, t].include?(m) or
                                                                    [a, h, l, th, tl, m].include?(t) or
                                                                    !register?(m) or !register?(t) or
                                                                    !m.bit8? or !t.bit8?
                isolate do |eoc|
                    if kh == tl
                        raise ArgumentError, "mul8_24: invalid arguments" if kl == th
                                ld  th, kh
                                ld  tl, kl unless kl == tl
                    else
                                ld  tl, kl unless kl == tl
                                ld  th, kh unless kh == th
                    end
                    if clrahl
                        if mbit9_carry
                                ld  hl, 0
                                ld  a, l
                        else
                                xor a
                                ld  h, a
                                ld  l, a
                        end
                                ld  t, a unless k_int24
                    elsif !k_int24
                                ld  t, 0
                    end
                    if optimize == :size        # 20 bytes (+~10 cycles per m bit)
                        if mbit9_carry
                                rr  m           # CF -> multiplier -> CF
                                jr  cont1
                        end
                        loop1   srl m           # 0 -> multiplier -> CF
                        cont1   jr  Z, last
                                jr  NC, noadd   # CF == 0 ? don't add
                                add hl, tt      # add multiplicand to result lo16
                                adc a, t        # add multiplicand to result hi8
                        noadd   sla tl          # multiplicand *= 2
                                rl  th
                                rl  t
                                jr  loop1       # m != 0 ? loop
                        last    jr  NC, eoc
                                add hl, tt      # last add
                                adc a, t
                    elsif optimize == :time     # 29 bytes
                        if mbit9_carry
                                rr  m           # CF -> multiplier -> CF
                        else
                                srl m           # 0 -> multiplier -> CF
                        end
                                jp  NZ, skip0   # m != 0 ? start regular loop
                                jr  C, skadd    # m == 1 ? add and quit
                                jp  eoc         # m == 0 ? just quit
                        skip0   jr  NC, noadd   # CF == 0 ? don't add
                        doadd   add hl, tt      # add multiplicand to result lo16
                                adc a, t        # add multiplicand to result hi8
                        noadd   sla tl          # multiplicand *= 2
                                rl  th
                                rl  t
                                srl m           # 0 -> multiplier -> carry
                                jr  NC, noadd   # carry == 0 ? don't add
                                jp  NZ, doadd   # carry == 1 and m != 0 ? loop
                        skadd   add hl, tt      # last add b.c. carry == 1
                                adc a, t
                    else
                        raise ArgumentError, "optimize should be :time or :size"
                    end
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 16-bit unsigned integer +kh+|+kl+ or
            # a 24-bit integer +km+|+kh+|+kl+ * 8(9)-bit unsigned +m+.
            # Returns the result as a 24-bit integer in +a+|+hl+.
            #
            # +kh+::   The MSB part of the multiplicand as an immediate value or an 8-bit register.
            # +kl+::   The LSB part of the multiplicand as an immediate value or an 8-bit register.
            # +m+::    A multiplier register, except +a+, +t+, +h+, +l+ or a part of +tt+.
            #
            # Options:
            # * +t+::  An 8-bit temporary register, except +a+, +m+, +h+, +l+ or a part of +tt+.
            # * +tt+:: A 16 bit temporary register (+de+ or +bc+).
            # * +k_int24+:: Whether a multiplicand is a 24-bit (possibly signed) integer with its MSB bits
            #               in +t+. Alternatively provide an 8-bit register holding the MSB bits.
            # * +mbit9_carry+:: If the multiplier (+m+) is 9-bit, where MSB (9th bit) is read from
            #                   the CARRY flag.
            # * +optimize+::    Optimization options: +:size+, +:time+ or +:unroll+.
            #
            #   bytes|~avg T-states|max T-states
            #
            #        k_int24    false           true          false          true
            #    mbit9_carry    false          false           true          true
            #  optimize
            #    :size       22|~451.0|567  21|~447.0|563  27|~470.0|591  26|~466.0|587
            #    :time       39|~347.0|443  39|~343.0|439  47|~378.9|488  45|~374.9|484
            #  :unroll       97|~291.2|351  97|~287.3|347 109|~323.1|388 107|~319.1|384
            #
            # Uses: +af+, +bc+, +de+, +hl+.
            def mul8_24a(kh=h, kl=l, m=b, t:c, tt:de, k_int24:false, mbit9_carry:false, optimize: :time)
                th, tl = tt.split
                raise ArgumentError, "mul8_24a: invalid arguments" if tt == hl or [a, h, l, th, tl, t].include?(m) or
                                                                    [a, h, l, th, tl, m].include?(t) or
                                                                    !register?(m) or !register?(t) or
                                                                    !m.bit8? or !t.bit8?
                case k_int24
                when false
                when true
                    k_int24 = t
                else 
                    raise ArgumentError, "mul8_24a: invalid k_int24 option" unless register?(k_int24) and k_int24.bit8?
                end
                isolate do |eoc|
                    if k_int24 && ![kh, kl].include?(t)
                                    ld  t, k_int24 unless k_int24 == t
                    end
                    if kh == tl
                        raise ArgumentError, "mul8_24a: invalid kh, kl arguments" if kl == th
                                    ld  th, kh
                                    ld  tl, kl unless kl == tl
                    else
                                    ld  tl, kl unless kl == tl
                                    ld  th, kh unless kh == th
                    end
                    if k_int24 && [kh, kl].include?(t)
                        if (k_int24 == tl && kl != tl) || (k_int24 == th && kh != th)
                            raise ArgumentError, "mul8_24a: invalid k_int24 register"
                        end
                                    ld  t, k_int24 unless k_int24 == t
                    end
                    case optimize
                    when :unroll
                        if k_int24
                                    ld   a, t unless k_int24 == a
                        elsif !mbit9_carry
                                    xor  a
                                    ld   t, a
                        end
                                    ld   l, tl unless kl == l
                                    ld   h, th unless kh == h
                        if mbit9_carry
                                    jr   C, mbit9set  # bit9 of m
                            unless k_int24
                                    xor  a
                                    ld   t, a
                            end
                        end
                        7.times do |i|
                                    sla  m            # CF <- m <- 0
                                    jr   C, :"iter#{i+1}"
                        end
                                    sla  m            # CF <- m <- 0
                                    jr   C, eoc
                                    xor  a if k_int24
                                    ld   h, a         # m == 0
                                    ld   l, a
                                    jp   eoc
                        mbit9set    label
                        if mbit9_carry && !k_int24
                                    xor  a
                                    ld   t, a
                        end
                        (if mbit9_carry then 0 else 1 end..7).each do |i|
                            ns :"iter#{i}" do |eoc|
                                    add  hl, hl
                                    adc  a, a
                                    sla  m            # CF <- m <- 0
                                    jr   NC, eoc
                            doadd   add  hl, tt
                                    adc  a, t
                            end
                        end
                    when :time
                        if k_int24
                                    ld   a, t unless k_int24 == a
                        elsif !mbit9_carry
                                    xor  a
                                    ld   t, a
                        end
                                    ld   l, tl unless kl == l
                                    ld   h, th unless kh == h
                        if mbit9_carry
                                    jr   C, start9    # bit9 of m
                            unless k_int24
                                    xor  a
                                    ld   t, a
                            end
                        end
                                    sll  m            # CF <- m <- 1 (terminator)
                                    jr   C, cont1
                            loop0   sla  m            # CF <- m <- 0
                                    jr   NC, loop0
                                    jp   NZ, cont1
                                    xor  a if k_int24
                                    ld   h, a         # m == 0
                                    ld   l, a
                                    jp   eoc
                        if mbit9_carry
                            start9  label
                            unless k_int24
                                    xor  a
                                    ld   t, a
                            end
                                    sll  m            # CF <- m <- 1 (terminator)
                                    jr   C, doadd1
                        end
                            loop1   add  hl, hl       # hl * 2
                                    adc  a, a         # a|hl * 2
                            cont1   sla  m            # CF <- m <- 0
                            cont2   jr   NC, loop1
                                    jr   Z, eoc
                            doadd1  add  hl, hl       # hl * 2
                                    adc  a, a         # a|hl * 2
                                    add  hl, tt       # hl + tt
                                    adc  a, t         # a|hl + t|tt
                                    sla  m            # CF <- m <- 0
                                    jr   NC, loop1
                                    jp   NZ, doadd1
                    when :size
                        if mbit9_carry
                                    ld   hl, 0
                                    ld   a, l
                        else
                                    xor  a
                                    ld   l, a
                                    ld   h, a
                        end
                                    ld   t, a unless k_int24
                        if mbit9_carry
                                    jr   NC, start8
                                    add  hl, tt
                                    adc  a, t
                        end
                        start8      sll  m            # CF <- m <- 1 (terminator)
                                    jr   cont2
                        loop1       add  hl, hl       # hl * 2
                                    adc  a, a         # a|hl * 2
                        cont1       sla  m            # CF <- m <- 0
                        cont2       jr   NC, loop1
                                    jr   Z, eoc
                                    add  hl, hl       # hl * 2
                                    adc  a, a         # a|hl * 2
                                    add  hl, tt       # hl + tt
                                    adc  a, t         # a|hl + t|tt
                                    jr   cont1
                    else
                        raise ArgumentError, "optimize should be :size, :time or :unroll"
                    end
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 16-bit integer +kh+|+kl+ *
            # 8-bit unsigned +m+. Returns the result as a 24-bit integer in +a+|+hl+.
            #
            # Creates an optimized, unrolled code so +m+ should be a constant value in the range: 0..256.
            #
            # Optionally the result can be added to an existing value in +a+|+hl+.
            #
            # On exit, the SF flag contains the sign of the 24-bit result.
            #
            # If +signed_k+ is +false+ and +clrahl+ is +true+ the resulting flag: ZF=1 signals the result 
            # fits in 16 bits.
            #
            # +kh+::     The MSB part of the multiplicand as an immediate value or an 8-bit register.
            # +kl+::     The LSB part of the multiplicand as an immediate value or an 8-bit register.
            # +m+::      A multiplier value as a constant integer.
            #
            # Options:
            # * +t+::      An 8-bit temporary register, except +a+, +h+, +l+ or a part of +tt+.
            # * +tt+::     A 16 bit temporary register (+de+ or +bc+).
            # * +clrahl+:: +true+ if the result should replace the register content, +false+ if added.
            # * +signed_k+:: If the multiplicand (+kh+|+kl+) represents a twos complement signed integer
            #                (-32768..32767).
            #
            # +t+ and +tt+ registers are ignored if +clrahl+ is +true+ and +m+ is a power of two or 0.
            #
            # Uses: +af+, +t+, optionally: +tt+, +hl+, optionally preserves: +kh+ or +kl+.
            def mul_const8_24(kh=h, kl=l, m=0, t:c, tt:de, clrahl:true, signed_k:false)
                th, tl = tt.split
                throw ArgumentError, "mul_const8_24: invalid arguments" unless m.is_a?(Integer) and (0..256).include?(m) and
                                                                        [bc, de].include?(tt) and
                                                                        ![h, l, th, tl, a].include?(t)
                if clrahl and (m & (m - 1)) == 0
                    t, tt = a, hl
                    th, tl = tt.split
                end
                if m == 0
                    return isolate do
                        if clrahl
                            xor  a
                            ld   h, a
                            ld   l, a
                        end
                    end
                end

                ml, tsl, clrahlt = m, 8, clrahl
                if clrahlt
                    tsl += if signed_k
                        12
                    else
                        4
                    end
                else
                    tsl += 7
                    tsl += 19.5 if signed_k
                end
                while (ml & 1) == 0
                    tsl += 15
                    ml >>= 1
                end if clrahlt
                just_cleared = false
                while tt != hl
                    if (ml & 1) != 0
                        if clrahlt
                            clrahlt = false
                            just_cleared = true
                            tsl += 12
                        else
                            just_cleared = false
                            tsl += 15
                        end
                    end
                    break if (ml >>= 1) == 0
                    if tt == de and ((ml & 1) == 0 or ml == 1 or just_cleared)
                        tsl += 4 unless just_cleared
                        tsl += 19
                        while (ml & 1) == 0
                            tsl += 19
                            ml >>= 1
                        end
                        tsl += 4 if ml != 1
                    else
                        tsl += 24
                    end
                end

                mr, tsr, clrahlt = m, 8+7, clrahl
                while mr != 0
                    if tt != hl and (mr & 0x100) != 0
                        if clrahlt
                            clrahlt = false
                            tsr += 12
                        else
                            tsr += 15
                        end
                    end
                    break if (mr & 0xFF) == 0
                        tsr += 24
                    mr <<= 1
                end
                tsr += 4 if t == a

                isolate do
                    if tsl <= tsr
                        th, tl = hl.split if clrahl
                        if th == kl and tl != kh
                                    ld   tl, kl
                                    ld   th, kh unless th == kh
                        elsif th != kl
                                    ld   th, kh unless th == kh
                                    ld   tl, kl unless tl == kl
                        else
                            throw ArgumentError, "mul_const8_24 condition violated: kh<>tl or kl<>th"
                        end
                        if clrahl
                            if signed_k
                                    sign_extend(a, th)
                            else
                                    xor  a
                            end
                        else
                                    ld   t, 0
                            if signed_k
                                    bit  7, th
                                    jr   Z, sk_neg
                                    dec  t
                            sk_neg  label
                            end
                        end
                        th, tl = tt.split if clrahl
                        while (m & 1) == 0
                                    add  hl, hl
                                    adc  a, a
                            m >>= 1
                        end if clrahl
                        just_cleared = false
                        while tt != hl
                            if (m & 1) != 0
                                if clrahl
                                    clrahl = false
                                    just_cleared = true
                                    ld16 tt, hl
                                    ld   t, a
                                else
                                    just_cleared = false
                                    add  hl, tt
                                    adc  a, t  # CF=0 and ZF=1 in case when result 16 bit
                                end
                            end
                            break if (m >>= 1) == 0
                            if tt == de and ((m & 1) == 0 or m == 1 or just_cleared)
                                    ex   de, hl unless just_cleared
                                    add  hl, hl
                                while (m & 1) == 0
                                    rl   t
                                    add  hl, hl
                                    m >>= 1
                                end
                                    ex   de, hl if m != 1
                            else
                                    sla  tl
                                    rl   th
                            end
                                    rl   t
                        end
                    else
                        if t == kl and th != kh
                                    ld   th, kl
                                    ld   t,  kh unless t == kh
                        elsif t != kl
                                    ld   t,  kh unless t == kh
                                    ld   th, kl unless th == kl
                        else
                            throw ArgumentError, "mul_const8_24 condition violated: kh<>th or kl<>t"
                        end
                                    ld   tl, 0
                        while m != 0
                            if tt != hl and (m & 0x100) != 0
                                if clrahl
                                    clrahl = false
                                    ld16 hl, tt
                                    ld   a, t
                                else
                                    add  hl, tt
                                    adc  a, t
                                end
                            end
                            break if (m & 0xFF) == 0
                            if signed_k
                                    sra  t
                            else
                                    srl  t
                            end
                                    rr   th
                                    rr   tl
                            m <<= 1
                        end
                                    anda a if t == a # CF=0 and ZF=1, SF sign
                    end
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 16-bit integer +kh+|+kl+ or a 24-bit
            # integer +km+|+kh+|+kl+ * 8-bit unsigned +m+.
            # Returns the result as a 24-bit integer in +a+|+hl+.
            #
            # Creates an optimized, unrolled code so +m+ should be a constant value in the range: 0..256.
            #
            # On exit, if +k_int24+ and +signed_k+ is +false+ the resulting flag: ZF=1 signals the result
            # fits in 16 bits.
            # If +k_int24+ is +false+ or +signed_k+ is +true+ the resulting flag SF contains the sign
            # of the 24-bit result.
            #
            # +kh+::     The MSB part of the multiplicand as an immediate value or an 8-bit register.
            # +kl+::     The LSB part of the multiplicand as an immediate value or an 8-bit register.
            # +m+::      A multiplier value as a constant integer.
            #
            # Options:
            # * +t+::      An 8-bit temporary register, except +a+, +h+, +l+ or a part of +tt+.
            # * +tt+::     A 16 bit temporary register (+de+ or +bc+).
            # * +k_int24+:: Whether a multiplicand is a 24-bit (possibly signed) integer with its MSB bits
            #               in +t+. Alternatively provide an 8-bit register holding the MSB bits.
            # * +signed_k+:: If +k_int24+ is +false+ this controls whether the multiplicand (+kh+|+kl+)
            #                represents a twos complement signed integer (-32768..32767).
            #                Otherwise if +k_int24+ is truthy this ensures the SF flag is set properly.
            #
            # +t+ and +tt+ registers are ignored +m+ is a power of two or 0.
            #
            # Uses: +af+, +hl+, optionally: +t+, +tt+, optionally preserves: +kh+ or +kl+.
            def mul_const8_24a(kh=h, kl=l, m=0, t:c, tt:de, k_int24:false, signed_k:false)
                th, tl = tt.split
                throw ArgumentError, "mul_const8_24a: invalid arguments" unless m.is_a?(Integer) and (0..256).include?(m) and
                                                                            [bc, de].include?(tt) and
                                                                            ![h, l, th, tl, a].include?(t)
                case k_int24
                when false
                when true
                    k_int24 = t
                else 
                    raise ArgumentError, "mul_const8_24a: invalid k_int24 option" unless register?(k_int24) and k_int24.bit8?
                end
                if m == 0
                    return isolate do
                            xor  a
                            ld   h, a
                            ld   l, a
                    end
                elsif m == 256
                    return isolate do
                        if k_int24 && !signed_k
                            if kl == a && kh != h
                                ld   h, kl
                                ld   a, kh unless kh == a
                                ld   l, 0
                            elsif kl != a
                                ld   a, kh unless kh == a
                                ld   h, kl unless kl == h
                                ld   l, 0
                            else
                                throw ArgumentError, "mul_const8_24a: invalid kh, kl arguments"
                            end
                        else
                            if kl == a && kh != h
                                ld   h, kl
                                xor  a
                                ld   l, a unless kh == l
                                add  a, kh
                                ld   l, 0 if kh == l
                            else
                                if kh == a
                                    ld   h, kl unless kl == h
                                    ora  a # a = kh
                                    ld   l, 0
                                elsif kl != a
                                    xor  a
                                    if kl == l && kh != h
                                        ld   h, kl
                                        ld   l, a
                                        if kh == kl
                                            add  a, h
                                        else
                                            add  a, kh
                                        end
                                    else
                                        ld   l, a unless [kh, kl].include?(l)
                                        add  a, kh
                                        ld   h, kl unless kl == h
                                        ld   l, 0 if [kh, kl].include?(l)
                                    end
                                else
                                    throw ArgumentError, "mul_const8_24a: invalid kh, kl arguments"
                                end
                            end
                        end
                    end
                end
                is_single_bit = (m & (m - 1)) == 0 # single bit only
                ztshift = trailing_zeros(m)
                zlshift = leading_zeros(m)
                m >>= ztshift
                mask = 0x80 >> (ztshift + zlshift + 1)
                isolate do
                    if k_int24
                        if ![kh, kl].include?(a)
                                ld  a, k_int24 unless k_int24 == a
                        end
                        if kh == l
                            raise ArgumentError, "mul_const8_24a: invalid kh, kl arguments" if kl == h
                                ld  h, kh
                                ld  l, kl unless kl == l
                        else
                                ld  l, kl unless kl == l
                                ld  h, kh unless kh == h
                        end
                        if [kh, kl].include?(a)
                            if (k_int24 == l && kl != l) || (k_int24 == h && kh != h)
                                raise ArgumentError, "mul_const8_24a: invalid k_int24 register"
                            end
                                ld  a, k_int24 unless k_int24 == a
                        end
                        if signed_k && is_single_bit && ztshift.zero?
                                ora a
                        end
                    elsif signed_k
                        if kh == l
                            raise ArgumentError, "mul_const8_24a: invalid kh, kl arguments" if [h, a].include?(kl)
                                sign_extend(a, kh)
                                ld  h, kh
                                ld  l, kl unless kl == l
                        else
                                ld  l, kl unless kl == l
                                ld  h, kh unless kh == h
                                sign_extend(a, kh)
                        end
                    else
                        if kh == l
                            raise ArgumentError, "mul_const8_24a: invalid kh, kl arguments" if kl == h
                                ld  h, kh
                                ld  l, kl unless kl == l
                        else
                                ld  l, kl unless kl == l
                                ld  h, kh unless kh == h
                        end
                                xor  a
                    end
                    ztshift.times do
                                add  hl, hl
                                adc  a, a
                    end
                    unless is_single_bit
                                ld   tl, l unless ztshift.zero? && kl == tl
                                ld   th, h unless ztshift.zero? && kh == th
                                ld   t, a  unless ztshift.zero? && k_int24 == t
                    end
                    while mask != 0
                                add  hl, hl
                                adc  a, a
                        unless (mask & m).zero?
                                add  hl, tt
                                adc  a, t
                        end
                        mask >>= 1
                    end
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 16-bit integer (+hl+) by an unsigned
            # 16-bit integer +mm+ (+bc+ or +de+).
            # Returns the 32-bit integer result in +hl+|+hl'+.
            #
            # +mm+::        A 16bit multiplier register (+bc+ or +de+).
            #
            # Options:
            # * +tt+::      A 16bit tempoarary register (+bc'+ or +de'+) of the alternative set.
            # * +clrhlhl+:: If +hl+|+hl'+ should be cleared, if +false+ acts like: +hl+|+hl'+ += +hl+ * +mm+;
            #               also it may be a 32-bit constant, in this instance acts like:
            #               +hl+|+hl'+ = +clrhlhl+ + +hl+ * +mm+.
            # * +signed_hl+:: If the multiplicand (+hl+) represents a twos complement signed integer
            #                 (-32768..32767).
            # * +optimize+::  What is more important: +:time+ (117 bytes) or +:size+ (51 bytes)?
            #
            # Uses: +af+, +af'+, +hl+, +hl'+, +mm+, +tt'+.
            #
            # T-states: (+2 if +clrhlhl+ is an integer)
            #        optimize:  time   size
            #   0xFFFF*0xFFFF:  1346   1674
            #        0*0     :   102   1136
            #   0xFFFF*0     :   102   1136
            #        0*0xFFFF:  1346   1674
            #   0xFFFF*1     :   604   1169
            #        1*0xFFFF:  1346   1674
            #   0xFFFF*0xAAAA:  1085   1410
            def mul16_32(mm=bc, tt:bc, clrhlhl:true, signed_hl:false, optimize: :time)
                raise ArgumentError unless [bc, de].include?(mm) and [bc, de].include?(tt)
                mh, ml = mm.split
                th, tl = tt.split
                srx = if signed_hl
                    ->(x) { sra x }
                else
                    ->(x) { srl x }
                end
                isolate do |eoc|
                    if optimize == :size
                                    ld  a, ml
                                    scf
                                    adc a, a         # carry <- ml <- 1
                                    ex  af, af       # a' = ml, CF' = carry, ZF = 0
                                    ld  a, mh
                                    ld  mh, h
                                    ld  ml, l
                        if address?(clrhlhl)
                                    ld  hl, clrhlhl >> 16
                        elsif clrhlhl
                                    ld  hl, 0        # hl = 0
                        end
                                    srx[mh]
                                    rr  ml
                                    exx
                        if address?(clrhlhl)
                                    ld  hl, clrhlhl & 0xFFFF
                                    ld  tt, 0        # th'|tl' = 0
                        elsif clrhlhl
                                    ld  hl, 0        # hl' = 0
                                    ld  th, h
                                    ld  tl, l
                        end
                                    rr  th
                                    scf
                                    adc a, a         # carry <- mh <- 1
                                    jr  C, doadd1
                        shadd0      exx
                        shadd1      srx[mh]          # mh -> ml -> th'
                                    rr  ml
                                    exx
                                    rr  th
                                    rr  tl
                                    add a, a         # carry <- mh|ml
                                    jr  Z, multlo
                        backloop    jr  NC, shadd0
                        doadd1      add hl, th|tl    # hl' += th'|tl'
                                    exx
                                    adc hl, mh|ml    # hl  += mh|ml + carry
                                    jr  shadd1

                        multlo      ex  af, af       # a = ml, ZF = a == 0
                                    jr  NZ, backloop
                                    exx

                    elsif optimize == :time

                                    ld  a, ml
                                    ora a            # a' ?= 0
                                    ex  af, af       # a' = ml, ZF' = a' == 0
                                    xor a            # a  = 0
                                    exx
                        if address?(clrhlhl)
                                    ld  hl, clrhlhl & 0xFFFF
                        elsif clrhlhl
                                    ld  h, a         # hl' = 0
                                    ld  l, a
                        end
                                    ld  th, a        # th'|tl' = 0
                                    ld  tl, a
                                    exx
                                    ora mh           # a |= mh
                                    jr  Z, multlo0   # mh <> 0

                                    ld  mh, h        # mh|ml = hl
                                    ld  ml, l
                        if address?(clrhlhl)
                                    ld  hl, clrhlhl >> 16
                        elsif clrhlhl
                                    ld  hl, 0        # hl = 0
                        end
                                    scf
                                    adc a            # carry <- mh <- 1
                                    jr  NC, noadd1

                        shadd1      srx[mh]          # mh -> ml -> th'
                                    rr  ml
                                    exx
                                    rr  th
                                    add hl, th|tl    # hl' += th'|tl'
                                    exx
                                    adc hl, mh|ml    # hl  += mh|ml + carry
                                    add a            # carry <- mh
                                    jr  Z, multlo
                                    jp  C, shadd1

                        noadd1      srx[mh]          # mh -> ml -> mh'
                                    rr  ml
                                    exx
                                    rr  th
                                    exx
                                    add a            # carry <- ml
                                    jr  Z, multlo
                                    jp  C, shadd1
                                    jp  noadd1

                        multlo0     ld  ml, h        # mh = 0, ml = h, th' = l, tl' = 0
                                    ld  a, l
                                    exx
                                    ld  th, a
                                    exx
                                    ld  hl, 0        # hl = 0

                        multlo      ex  af, af       # a = ml, ZF = a == 0
                                    jr  Z, eoc
                                    add a            # carry <- ml
                                    jr  NC, noadd2

                        shadd2      srx[ml]          # ml -> mh' -> ml'
                                    exx
                                    rr  th
                                    rr  tl
                                    add hl, th|tl    # hl' += th'|tl'
                                    exx
                                    adc hl, mh|ml    # hl  += mh|ml + carry
                                    add a            # carry <- ml
                                    jr  Z, finlo
                                    jp  C, shadd2

                        noadd2      srx[ml]          # ml -> mh' -> ml'
                                    exx
                                    rr  th
                                    rr  tl
                                    exx
                                    add a            # carry <- ml
                                    jr  Z, finlo
                                    jp  C, shadd2
                                    jp  noadd2

                        finlo       jr  NC, eoc
                                    srx[ml]          # ml -> mh' -> ml'
                                    exx
                                    rr  th
                                    rr  tl
                                    add hl, th|tl    # hl' += th'|tl'
                                    exx
                                    adc hl, mh|ml    # hl  += mh|ml + carry
                    else
                        raise ArgumentError, "optimize should be :time or :size"
                    end
                end
            end
            ##
            # Creates a routine that performs an euclidean division of unsigned 8-bit: +k+ / +m+.
            # Returns a quotient in +k+ and a remainder in +accumulator+.
            #
            # This routine can be chained one after another to divide arbitrary size dividends.
            # Just preserve the +a+ and +m+ registers and pass +false+ to +clrrem:+ on subsequent routines.
            # Start with the most significant byte of the dividend:
            #
            #    ns :divide24_8 do |eoc| # divide (l|de)/c
            #      divmod l, c, check0:eoc, check1:eoc, ignore_cf:true
            #      divmod d, c, clrrem:false
            #      divmod e, c, clrrem:false
            #      anda a # clear CF
            #    end
            #
            # +k+:: A dividend as an 8-bit register except: +a+, +b+.
            # +m+:: A divisor as an 8-bit register except: +a+, +b+, +k+.
            #
            # Options:
            # * +clrrem+:: Clears a reminder (+accumulator+). If this is +false+, +check0+, +check1+ and
            #              +k_leq_m+ options are being ignored.
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the +accumulator+ is being altered.
            #              +CF+ should be ignored if +check0+ is +false+. It may also be a label
            #              (within the relative jump range) to indicate where to jump if +m+ is 0.
            # * +check0_far+:: Allow to specify far (outside the relative jump range) +check0+ label.
            # * +check1+:: Checks if a divisor equals to 1, a hot path optimization. It may also be a label
            #              to indicate where to jump if +m+ equals 1.
            # * +k_leq_m+:: Short circuits if a dividend is equal to or less than a divisor, a hot path
            #               optimization.
            # * +modulo+:: Calculates a remainder only.
            # * +ignore_cf+:: Removes instruction to clear CF if +check0+ is also set. This can be used if
            #                 +check0+ is provided as a label instead of checking the state of the CF flag.
            # * +optimize+:: Optimization options: +:size+, +:time+, +:unroll+, +:time_alt+, +:unroll_alt+.
            #                The +:time_alt+ and +:unroll_alt+ use undocumented +sll+ instruction.
            #
            # ====Optimization options:
            # 
            #   bytes|~avg T-states|max T-states
            #
            #    options       check       check       check        check          check      no clrrem
            #   optimize        all       0 & km       0 & 1        0 only         none       remainder=0
            #   :size        36|~221|400  34|~216|396  24|~367|389  19|~357|380  13|~336|358  14|~388|410
            #   :time        49|~215|447  47|~210|453  39|~336|436  33|~331|442  27|~310|420  29|~358|460
            #   :time_alt    51|~214|423  49|~209|425  41|~338|412  35|~332|414  30|~316|396  32|~364|436
            #   :unroll     119|~172|301 117|~166|297 110|~256|290 104|~250|286 104|~250|286  80|~282|304
            #   :unroll_alt 164|~161|296 162|~155|298 155|~236|285 149|~230|287 144|~213|269 140|~261|309
            #
            # Uses: +af+, +b+, +k+, preserves: +m+.
            def divmod(k, m, clrrem:true, check0:true, check0_far:false, check1:true, k_leq_m:nil, modulo:false, ignore_cf:false, optimize: :time)
                unless clrrem
                    check0 = false
                    check1 = false
                    k_leq_m = false
                end
                raise ArgumentError, "divmod: invalid arguments" unless [d, e, h, l, c].include?(k) and
                                                                        [d, e, h, l, c].include?(m) and k != m
                opt_alt = case optimize
                when :time_alt
                    optimize = :time
                    true
                when :unroll_alt
                    optimize = :unroll
                    true
                when :size, :time, :unroll then false
                else
                    raise ArgumentError, "divmod: optimize should be :size, :time, :time_alt, :unroll or :unroll_alt"
                end
                if k_leq_m.nil?
                    k_leq_m = (optimize != :size)
                end
                isolate do |eoc|
                    if check0 == true
                        check0 = eoc
                        check0_far = (optimize == :unroll && opt_alt)
                    end
                    check1 = eoc if check1 == true
                    if check0 or check1 or k_leq_m
                                    ld  a, m
                                    cp  1 if check0 or check1 
                        if check0
                            if check0_far && optimize == :size
                                    jp  C, check0 # division by 0
                            elsif check0_far
                                    jr  C, fw_check0 # division by 0
                            else
                                    jr  C, check0 # division by 0
                            end
                        end
                        if check1
                            if optimize == :size && !k_leq_m
                                    jr  NZ, divstrt  # division by m > 1
                                    xor a            # clear rest
                                if check1 == eoc
                                    jr  eoc          # division by 1
                                else
                                    jp  check1
                                end
                            else
                                    jr  Z, divone    # division by m == 1
                            end
                        end
                        if k_leq_m
                                    cp  k            # m < k ?
                            if optimize == :size
                                    jr  C, divstrt   # m < k
                            kislqm  jr  Z, kiseqm    # k == m
                                    ld  a, k         # k <  m
                                    ld  k, 0 unless modulo
                                    jr  eoc
                            kiseqm  label            # k == m
                                    ld  k, 1 unless modulo
                                if check1 && check1 == eoc
                            divone  xor a
                                    jr  eoc
                                else
                                    xor a 
                                    jr  eoc
                                    if check1
                                divone  xor a
                                        jp  check1
                                    end
                                end
                            else
                                    jr  NC, kislqm   # m >= k
                            end
                        end
                    end
                    if optimize == :unroll
                        if clrrem
                                    xor a
                            7.times do |i|
                                    sla k      # align highest set bit at CF
                                    jr  C, define_label("iter#{i}").found1
                            end
                            if k_leq_m
                                    jp  iter7  # k == 0x80
                            else
                                    sla k
                                    jr  C, iter7.found1
                        fw_check0   label if check0 && check0_far && check0 == eoc
                                    jp  eoc    # k == 0
                            end
                            if k_leq_m
                            kislqm  jr  Z, kiseqm   # k == m
                                    ld  a, k        # k <  m
                                    ld  k, 0 unless modulo
                        fw_check0   label if check0 && check0_far && check0 == eoc
                                    jp  eoc
                            kiseqm  label           # k == m
                                    ld  k, 1 unless modulo
                                unless check1 && check1 == eoc
                                    xor a 
                                    jp  eoc
                                end
                            end
                            if check1
                            divone  xor a      # clear rest
                        fw_check0   label if check0 && check0_far && !(check0 == eoc) && check0 == check1
                                    jp  check1 # division by 1
                            end
                        fw_check0   jp  check0 if check0 && check0_far && !(check0 == eoc) && !(check0 == check1)
                        end
                        if opt_alt
                            8.times do |i|
                                ns :"iter#{i}" do
                                        sla k unless clrrem && i.zero?
                                found1  adc a
                                        jr  C, :"fits#{i+1}" unless clrrem
                                        cp  m
                                        jr  NC, :"fits#{i+1}"
                                end
                            end
                                        sla k unless modulo # clear carry, shift quotient into position
                            if modulo && check0 && !ignore_cf
                                        jp  quitccf # clear carry
                            else
                                        jp  eoc
                            end
                            (1..7).each do |i|
                                ns :"fits#{i}" do
                                        sub m
                                        sll k
                                        adc a
                                        jr  C, :"fits#{i+1}" unless clrrem
                                        cp  m
                                    if i != 7
                                        jr  C, :"iter#{i+1}"
                                    else
                                        jr  NC, fits8
                                        sla k unless modulo # clear carry, shift quotient into position
                                        if modulo && check0 && !ignore_cf
                                            jp  quitccf # clear carry
                                        else
                                            jp  eoc
                                        end
                                    end
                                end
                            end
                                fits8   sub m
                                quitccf anda a if modulo && check0 && !ignore_cf # clear carry
                                        sll k unless modulo # clear carry, shift quotient into position
                        else
                            8.times do |i|
                                isolate :"iter#{i}" do |skipadd|
                                        sla k unless clrrem && i.zero?
                                found1  adc a
                                        jr  C, fits unless clrrem
                                        cp  m
                                        jr  C, skipadd
                                fits    sub m
                                        inc k unless modulo
                                end
                            end
                                        anda a if check0 && !ignore_cf # clear carry
                        end
                    else # optimize != :unroll
                        divstrt     ld  b, 8
                        if clrrem
                                    xor a            # a = 0
                            if optimize == :time
                            findhi  sla k            # align highest set bit at CF
                                    jr  C, found1
                                    djnz findhi
                                    jp  eoc unless k_leq_m # k == 0
                                if k_leq_m
                            kislqm  jr  Z, kiseqm   # k == m
                                    ld  a, k        # k <  m
                                    ld  k, 0 unless modulo
                                    jp  eoc
                            kiseqm  label           # k == m
                                    ld  k, 1 unless modulo
                                    unless check1 && check1 == eoc
                                        xor a       # clear rest
                                        jp  eoc
                                    end
                                end
                                if check1
                            divone  xor a            # clear rest
                        fw_check0   label if check0 && check0_far && check0 == check1
                                    jp  check1       # division by 1
                                end
                        fw_check0   jp  check0 if check0 && check0_far && !(check0 == check1)
                            end
                        end
                        if optimize == :time
                            unless clrrem
                                    sla k            # carry <- k <- 0 (quotient)
                                    adc a            # carry <- a <- carry
                                    jr  C, fits unless clrrem # a >= 256
                                    cp  m
                                    jr  NC, fits     # a >= m
                                    djnz loopfit     # b = 7
                            end
                            fits    sub m            # a = a - m (rest)
                            if opt_alt && !modulo
                                    sll k            # carry <- k <- 1 (quotient)
                                    djnz found1      # loop
                            else
                                    inc k unless modulo # k <- 1 (quotient)
                                    djnz loopfit     # loop
                            end
                            if !(opt_alt && !modulo) && check0 && !ignore_cf
                                    jp  quitccf      # quit with clear carry
                            else
                                    jp  eoc
                            end
                            loopfit sla k            # carry <- k <- 0
                            found1  adc a            # carry <- a <- carry
                                    jr  C, fits unless clrrem # a >= 256
                                    cp  m            # a - m
                                    jr  NC, fits     # a >= m
                            nfits   djnz loopfit     # loop
                            if opt_alt && !modulo
                                    sla k            # clear carry, shift quotient into position
                            elsif check0 && !ignore_cf
                            quitccf anda a           # clear carry only when check0
                            end
                        elsif optimize == :size
                            loopfit sla k            # carry <- k <- 0
                                    adc a            # carry <- a <- carry
                            if modulo && clrrem
                                    sub m
                                    jr  NC, fits
                                    add m
                            fits    djnz loopfit
                            else
                                    jr  C, fits unless clrrem # a >= 256
                                    cp  m
                                    jr  C, nfits
                            fits    sub m
                                    inc k unless modulo # k <- 1 (quotient)
                            nfits   djnz loopfit
                            end
                                    anda a if check0 && !ignore_cf # clear carry
                        end
                    end
                end
            end
            ##
            # Creates a routine that performs an euclidean division of an unsigned 16-bit
            # +hl+ / 8-bit +m+.
            # Returns a quotient in +hl+ and a remainder in +a+.
            #
            # +m+:: A divisor, one of: +c+, +d+ or +e+.
            #
            # Options:
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the register +a+ is being altered.
            #              +CF+ should be ignored if +check0+ is +false+.
            # * +check1+:: Checks if a divisor is 1, a hot path optimization.
            # * +modulo+:: Calculates a remainder only.
            #
            # _NOTE_:: A stacked Macros.divmod presents a faster (up to 12%) but a significantly
            #          larger (x1.5) alternative.
            #
            # Uses: +af+, +b+, +hl+, preserves: +m+.
            def divmod8(m=c, check0:true, check1:true, modulo:false)
                raise ArgumentError unless [c, d, e].include?(m)
                isolate do |eoc|
                    check1 = eoc if check1 == true
                    check0 = eoc if check0 == true
                    if check0 or check1
                                    ld  a, m
                                    cp  1
                                    jr  C, check0 if check0 # division by 0
                        if check1
                                    jr  Z, divone    # division by m == 1
                        end
                    end
                    divstrt         xor a            # a = 0
                                    ld  b, 16
                    findhi          add hl, hl       # align highest set bit at CF
                                    jr  C, found
                                    djnz findhi
                                    jp  eoc          # hl == 0
                    if check1
                        divone      xor a            # clear rest
                                    jp  check1       # division by 1
                    end
                    loopfit         add hl, hl       # carry <- hl <- 0
                    found           adc a            # carry <- a <- carry
                                    jr  C, fits      # a >= 256
                                    cp  m            # a - m
                                    jr  NC, fits     # a >= m
                                    djnz loopfit     # loop
                    if check0
                                    jp  quitccf      # clear carry only when check0
                    else
                                    jp  eoc
                    end
                    fits            sub m            # a = a - m (rest)
                    unless modulo
                                    inc l            # hl <- 1 (quotient)
                    end
                                    djnz loopfit     # loop
                    quitccf         anda a if check0 # clear carry only when check0
                end
            end
            ##
            # Creates a routine that performs an euclidean division of unsigned 16-bit: +kh+|+kl+ / 8-bit +m+.
            # Returns a quotient in +kh+|+kl+ and a remainder in +accumulator+.
            #
            # This routine utilizes a stacked up Macros.divmod routines.
            #
            # +kh+, +kl+:: A dividend as two unique 8-bit registers except: +a+, +b+.
            # +m+:: A divisor as an 8-bit register except: +a+, +b+ and none of +kh+, +kl+.
            #
            # Options:
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the +accumulator+ is being altered.
            #              +CF+ should be ignored if +check0+ is +false+. It may also be a label
            #              (within the relative jump range) to indicate where to jump if +m+ equals 0.
            # * +check0_far+:: Allow to specify far (outside the relative jump range) +check0+ label.
            # * +check1+:: Checks if a divisor equals 1, a hot path optimization. It may also be a label
            #              to indicate where to jump if +m+ equals 1.
            # * +k_leq_m+:: Short circuits if +kh+ is equal or less than a divisor, a hot path optimization.
            # * +modulo+:: Calculates a remainder only.
            # * +ignore_cf+:: Removes instruction to clear CF if +check0+ is also set. This can be used if
            #                 +check0+ is provided as a label instead of checking the state of the CF flag.
            # * +optimize+:: Optimization options: +:size+, +:time+, +:unroll+, +:time_alt+, +:unroll_alt+.
            #                See Macros.divmod for details.
            #
            # _NOTE_:: A Macros.divmod8 presents a slower (up to 12%) but a significantly
            #          smaller (x1.5) alternative.
            #
            # Uses: +af+, +b+, +kh+, +kl+, preserves: +m+.
            def divmod16_8(kh, kl, m, check0:true, check0_far:false, check1:true, k_leq_m:nil, modulo:false, ignore_cf:false, optimize: :time)
                raise ArgumentError unless [kh, kl, m].uniq.size == 3
                isolate do |eoc|
                    if check0 == true
                        check0 = eoc
                        check0_far = (optimize == :unroll || optimize == :unroll_alt)
                    end
                    check1 = eoc if check1 == true
                    divmod kh, m, check0:check0, check0_far:check0_far, check1:check1, k_leq_m:k_leq_m, modulo:modulo, optimize:optimize, ignore_cf:true
                    divmod kl, m, clrrem:false, modulo:modulo, optimize:optimize
                    anda a if check0 && !ignore_cf # clear CF unless ignored
                end
            end
            ##
            # Creates a routine that performs an euclidean division of unsigned 16-bit: +hl+ / +de+.
            # Returns a quotient in +hl+ and a remainder in +bc+.
            #
            # +x+:: A temporary 8-bit register, one of: +ixh+, +ixl+, +iyh+ or +iyl+.
            #
            # Options:
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the +accumulator+ is being altered. 
            #              +CF+ should be ignored if +check0+ is +false+.
            # * +check1+:: Checks if a divisor is 1, a hot path optimization.
            # * +modulo+:: Calculates a remainder only.
            # * +quick8+:: Checks if a divisor fits in 8 bits and in this instance uses a different,
            #              optimized for 8-bit division code. +quick8+ can also be set to +:divmod8+ to use
            #              Macros.divmod8 (smaller code) instead of stacked Macros.divmod for an 8-bit division.
            #
            # Uses: +af+, +bc+, +hl+, +x+, preserves: +de+.
            def divmod16(x=ixl, check0:true, check1:true, modulo:false, quick8:true)
                raise ArgumentError unless [ixh, ixl, iyh, iyl].include?(x)
                isolate do |eoc|
                    check0 = eoc if check0 == true
                    check1 = eoc if check1 == true
                    if check0 or check1 or quick8
                                        xor a
                                        ora d
                                        jp  NZ, div16strt
                        if quick8
                            if quick8 == :divmod8
                                        divmod8 e, check0:(check0 && qcheck0), check1:(check1 && qcheck1), modulo:modulo
                            else
                                        divmod h, e, check0:(check0 && qcheck0), check1:(check1 && qcheck1), modulo:modulo, optimize: :time, ignore_cf:true
                                        divmod l, e, clrrem:false, optimize: :time
                                        anda a if check0
                            end
                                        ld  c, a
                            if check0 == eoc
                            qcheck0     label
                            end
                                        jp  eoc
                            if check1
                            qcheck1     ld  b, a
                                        ld  c, a
                                        jp  check1
                            end
                            if check0 && !(check0 == eoc)
                            qcheck0     jp  check0
                            end
                        elsif check0 or check1
                                        ld  a, e
                                        cp  1
                                        jr  C, check0 if check0 # division by 0
                            if check1
                                        jr  Z, divone     # division by m == 1
                            end
                        end
                    end
                    div16strt           xor a            # a = 0 hi remainder
                                        ld  c, a         # c = 0 lo remainder
                                        ld  b, 16
                    findhi              add hl, hl       # align highest set bit at CF
                                        jr  C, found
                                        djnz findhi
                                        jp  eoc          # hl == 0, bc == 0
                    loopfit             add hl, hl       # carry <- hl <- 0
                    found               rl  c            # carry <- c <- carry
                                        adc a            # carry <- a <- carry
                                        cp  d            # a - d
                                        jr  NC, fitshi   # a >= d
                                        djnz loopfit     # loop
                                        ccf if check0
                                        jp  over
                    if check1 && !quick8
                        divone          ld  bc, 0         # clear rest
                                        jp  check1        # division by 1
                    end
                    fitshi              ld  x, a
                                        ld  a, c
                                        jr  NZ, fitslo   # a > d, ignore e
                                        cp  e            # a == d: c - e
                                        jr  NC, fitslo   # a >= e
                                        ld  a, x
                                        djnz loopfit     # loop
                                        ccf if check0
                                        jp  over
                    fitslo              sub e            # a = c - e
                                        ld  c, a         # c = c - e
                                        ld  a, x
                                        sbc d            # a -= d
                    unless modulo
                                        inc l            # hl <- 1 (quotient)
                    end
                                        djnz loopfit     # loop
                    over                ld  b, a         # bc = remainder
                end
            end
            ##
            # Creates a routine that performs an euclidean division of unsigned 24-bit: +kh+|+km+|+kl+ / 8-bit +m+.
            # Returns a quotient in +kh+|+km+|+kl+ and a remainder in +accumulator+.
            #
            # This routine utilizes a stacked up Macros.divmod routines.
            #
            # +kh+, +km+, +kl+:: A dividend as three unique 8-bit registers except: +a+, +b+.
            # +m+:: A divisor as an 8-bit register except: +a+, +b+ and none of +kh+, +km+, +kl+.
            #
            # Options:
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the +accumulator+ is being altered.
            #              +CF+ should be ignored if +check0+ is +false+. It may also be a label
            #              (within the relative jump range) to indicate where to jump if +m+ equals 0.
            # * +check0_far+:: Allow to specify far (outside the relative jump range) +check0+ label.
            # * +check1+:: Checks if a divisor equals 1, a hot path optimization. It may also be a label
            #              to indicate where to jump if +m+ equals 1.
            # * +k_leq_m+:: Short circuits if +kh+ is equal or less than a divisor, a hot path optimization.
            # * +modulo+:: Calculates a remainder only.
            # * +ignore_cf+:: Removes instruction to clear CF if +check0+ is also set. This can be used if
            #                 +check0+ is provided as a label instead of checking the state of the CF flag.
            # * +optimize+:: Optimization options: +:size+, +:time+, +:unroll+, +:time_alt+, +:unroll_alt+.
            #                See Macros.divmod for details.
            #
            # Uses: +af+, +b+, +kh+, +km+, +kl+, preserves: +m+.
            def divmod24_8(kh, km, kl, m, check0:true, check0_far:false, check1:true, k_leq_m:nil, modulo:false, ignore_cf:false, optimize: :time)
                raise ArgumentError unless [kh, km, kl, m].uniq.size == 4
                isolate do |eoc|
                    if check0 == true
                        check0 = eoc
                        check0_far = (optimize == :unroll || optimize == :unroll_alt)
                    end
                    check1 = eoc if check1 == true
                    divmod kh, m, check0:check0, check0_far:check0_far, check1:check1, k_leq_m:k_leq_m, modulo:modulo, optimize:optimize, ignore_cf:true
                    divmod km, m, clrrem:false, modulo:modulo, optimize:optimize
                    divmod kl, m, clrrem:false, modulo:modulo, optimize:optimize
                    anda a if check0 && !ignore_cf # clear CF unless ignored
                end
            end
            ##
            # Creates a routine that performs an euclidean division of unsigned 32-bit: +hl+|+hl'+ / +m+.
            # Returns a quotient in +hl+|+hl'+ and a remainder in +a+.
            #
            # +m+:: A divisor, one of: +c+, +d+ or +e+.
            # +mt'+:: A temporary register from the alternative set: +c'+, +d'+ or +e'+.
            #
            # Options:
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the register +a+ is being altered.
            #              +CF+ should be ignored if +check0+ is +false+.
            # * +check1+:: Checks if a divisor is 1, a hot path optimization.
            # * +modulo+:: Calculates a remainder only.
            #
            # _NOTE_:: A stacked Macros.divmod presents a faster (9-16%) but a significantly
            #          larger (x2) alternative.
            #
            # Uses: +af+, +af'+, +b+, +b'+, +hl+, +hl'+, +mt'+, preserves: +m+.
            def divmod32_8(m=c, mt:c, check0:true, check1:true, modulo:false)
                raise ArgumentError unless [c, d, e].include?(m)
                isolate do |eoc|
                    check0 = eoc if check0 == true
                    check1 = eoc if check1 == true
                    if check0 or check1
                                        ld  a, m
                                        cp  1
                                        jr  C, check0 if check0 # division by 0
                        if check1
                                        jp  NZ, divstrt  # division by m > 1
                                        xor a            # clear rest
                                        jp  check1       # division by 1
                        end
                    end
                    divstrt             xor a            # a = 0
                                        ld  b, 16
                    loopfit1            add hl, hl       # carry <- hl <- 0
                                        adc a            # carry <- a <- carry
                                        jr  C, fits1     # a > m
                                        cp  m            # a - m
                                        jr  NC, fits1    # a >= m
                                        djnz loopfit1    # loop
                                        jp  divlo16
                    fits1               sub m            # a = a - m (rest)
                    unless modulo
                                        inc l            # hl <- 1 (quotient)
                    end
                                        djnz loopfit1    # loop

                    divlo16             ex  af, af
                                        ld  a, m
                                        exx
                                        ld  mt, a
                                        ex  af, af
                                        ld  b, 16
                    loopfit2            add hl, hl       # carry <- hl <- 0
                                        adc a            # carry <- a <- carry
                                        jr  C, fits2     # a > m
                                        cp  mt           # a - m
                                        jr  NC, fits2    # a >= m
                                        djnz loopfit2    # loop
                                        ccf if check0    # clear carry only when check0
                                        jp  over
                    fits2               sub mt           # a = a - m (rest)
                    unless modulo
                                        inc l            # hl <- 1 (quotient)
                    end
                                        djnz loopfit2    # loop
                                        ora  a if check0 # clear carry only when check0
                    over                exx
                end
            end
            ##
            # Creates a routine that performs an euclidean division of unsigned 32-bit: +hl+|+hl'+ / +de+.
            # Returns a quotient in +hl+|+hl'+ and a remainder in +bc+.
            #
            # +x+:: A temporary 8-bit register, one of: +ixh+, +ixl+, +iyh+ or +iyl+.
            #
            # Options:
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the register +a+ is being altered.
            #              +CF+ should be ignored if +check0+ is +false+.
            # * +check1+:: Checks if a divisor is 1, a hot path optimization.
            # * +modulo+:: Calculates a remainder only.
            # * +quick8+:: Checks if a divisor fits in 8 bits and in this instance
            #              uses a different, optimized code.
            #
            # Uses: +af+, +af'+, +bc+, +bc'+, +hl+, +hl'+, +de'+, +x+, preserves: +de+.
            def divmod32_16(x:ixl, check0:true, check1:true, modulo:false, quick8:true)
                raise ArgumentError unless [ixh, ixl, iyh, iyl].include?(x)
                isolate do |eoc|
                    check0 = eoc if check0 == true
                    check1 = eoc if check1 == true
                    if check0 or check1 or quick8
                                        xor a
                                        ora d
                                        jp  NZ, div32strt
                        if quick8
                                        divmod32_8 e, check0:(check0 && qcheck0), check1:(check1 && qcheck1), modulo:modulo
                                        ld  c, a
                            if check0 == eoc
                            qcheck0     label
                            end
                                        jp  eoc
                            if check1
                            qcheck1     ld  b, a
                                        ld  c, a
                                        jp  check1
                            end
                            if check0 && !(check0 == eoc)
                            qcheck0     jp  check0
                            end
                        elsif check0 or check1
                                        ld  a, e
                                        cp  1
                                        jr  C, check0 if check0 # division by 0
                            if check1
                                        jp  NZ, div32strt # division by m > 1
                                        ld  bc, 0
                                        jp  check1        # division by 1
                            end
                        end
                    end
                    div32strt           xor a            # a = 0 hi remainder
                                        ld  c, a         # c = 0 lo remainder
                                        ld  b, 16
                    loopfit1            add hl, hl       # carry <- hl <- 0
                                        rl  c            # carry <- c <- carry
                                        adc a            # carry <- a <- carry
                                        cp  d            # a - d
                                        jr  NC, fitshi1  # a >= d
                                        djnz loopfit1    # loop
                                        jp  divlo16
                    fitshi1             ld  x, a
                                        ld  a, c
                                        jr  NZ, fitslo1  # a > d, ignore e
                                        cp  e            # a == d: c - e
                                        jr  NC, fitslo1  # a >= e
                                        ld  a, x 
                                        djnz loopfit1    # loop
                                        jp  divlo16
                    fitslo1             sub e            # a = c - e
                                        ld  c, a         # c = c - e
                                        ld  a, x
                                        sbc d            # a -= d
                    unless modulo
                                        inc l            # hl <- 1 (quotient)
                    end
                                        djnz loopfit1    # loop

                    divlo16             push de
                                        ld  x, c
                                        exx              # hl' <-> hl
                                        pop de           # de' = de
                                        ld  c, x         # c' = c

                                        ld  b, 16
                    loopfit2            add hl, hl       # carry <- hl' <- 0
                                        rl  c            # carry <- c' <- carry
                                        adc a            # carry <- a <- carry
                                        jr  C, fitshi2c  # a > d
                                        cp  d            # a - d'
                                        jr  NC, fitshi2  # a >= d'
                                        djnz loopfit2    # loop
                                        ccf if check0
                                        jp  over
                    fitshi2             ld  x, a
                                        ld  a, c
                                        jr  NZ, fitslo2  # a > d, ignore e
                                        cp  e            # a == d: c - e
                                        jr  NC, fitslo2  # a >= e
                                        ld  a, x
                                        djnz loopfit2    # loop
                                        ccf if check0
                                        jp  over
                    fitshi2c            ld  x, a
                                        ld  a, c
                    fitslo2             sub e            # a = c' - e'
                                        ld  c, a         # c' = c' - e'
                                        ld  a, x
                                        sbc d            # a -= d'
                    unless modulo
                                        inc l            # hl' <- 1 (quotient)
                    end
                                        djnz loopfit2    # loop
                                        ora  a if check0 # clear carry only when check0
                    over                ld  x, c
                                        exx
                                        ld  b, a         # bc = remainder
                                        ld  c, x
                end
            end
            ##
            # Creates a Lehmer random number generator routine.
            #
            # See: https://en.wikipedia.org/wiki/Lehmer_random_number_generator
            #
            # This routine uses the following parameters (similar to ZX-Spectrum ROM's):
            # 
            #    s1 = (s0 + 1) * 75 % 65537 - 1
            #
            # Expects a seed (s0) in +hl+ and produces the result (s1) in +hl+.
            #
            # Modifies: +af+, +bc+, +de+, +hl+.
            #
            # T-states: ~ 547.7 (46 for seed=65535, 193 for seed<=872, max: 601)
            def rnd
                isolate do |eoc|
                                    inc hl          # seed + 1
                                    ld  a, l
                                    ora h
                                    jp  NZ, multi75 # (seed + 1) < 65536
                                                    # return pre-calculated value
                                    ld  hl, 65536 * 75 % 65537 - 1
                                    jr  eoc
                                                    # overflow 0x1_0000 - 0x1_0001 = -1 happens only for seeds:
                                                    # 65535, 20097, 34952, 40195
                    ovrflow         add a           # shift left (for all possible cases CF=0 after this)
                                                    # otherwise we should have take care of CF=1
                                    djnz mnext      # this was the last iteration so the remainder result is 65536
                                    jr  eoc         # remainder = (borrow|hl = 0x1_0000) (seed - 1) == 65535

                                                    # a|hl = (seed + 1) * 75
                    multi75         mul_const8_24a(h, l, 75, t:c, tt:de, k_int24:false, signed_k:false)
                                    jr  Z, fits     # a|hl < 0x1_0000: n mod 65537 == n
                                                    # a|hl never == 0x1_0000 after multiplication by 75
                                                    # so we can ignore some checks later
                                    ld  de, -1      # a|hl mod 65537 (0x1_0001) goes to borrow|hl
                                    ld  b, 8        # shift left this many times before dividend fits in a divisor
                    mloop0          add hl, hl      # pre-shift left a|hl, highest bit to CF
                                    adc a
                                    dec b           # no check if b==0, there'll be at most 8 iterations since a >= 0x01
                                    jp  NC, mloop0  # CF == 0: loop
                                    ld  c, l        # swap registers: hl|a = a|hl
                                    ld  l, h
                                    ld  h, a
                                    ld  a, c
                                    dec hl          # hl = hl - 1, in effect: borrow|hl = 0x1_nnnn - 0x1_0001 = 0x0_mmmm
                                                    # no need to check for overflow here since 65536 is never a result of n*75
                                    jr  Z, fits     # b==0: (seed + 1) * 75 == 0x0001_nnnn
                    mloop1          add a           # shift left hl|a, highest bit to CF
                                    adc hl, hl
                                    jr  NC, mnext   # CF == 0: continue
                                    add hl, de      # borrow|hl = 0x1_nnnn - 0x1_0001
                                    jr  NC, ovrflow # 0x1_0000 - 0x1_0001 = -1 (0x1_FFFF)
                    mnext           djnz mloop1
                    fits            dec hl          # seed - 1
                end
            end
            ##
            # Creates a routine that converts an 8-bit unsigned integer to a BCD string.
            #
            # Used by Macros.utobcd.
            #
            # +bufend+:: A direct address of the byte immediately following an end of the BCD memory area
            #            as an integer or a label or a pointer to the memory address holding the +bufend+ value.
            # +r+:: An 8-bit register holding the integer during conversion: +c+, +d+ or +e+.
            # +buflen+:: The current byte size of the BCD buffer as an integer, a label or +t+.
            # +t+:: The register holding the current byte size of the BCD buffer: +c+, +d+ or +e+.
            # +r_in_a+:: +true+ if the integer to convert is provided in +accumulator+ instead of in +r+.
            #
            # Modifies: +af+, +b+, +r+, +t+, +hl+.
            def utobcd_step(bufend, r, buflen=1, t=c, r_in_a=false)
                raise ArgumentError unless address?(bufend) and
                                           [c,d,e].include?(r) and [c,d,e].include?(t) and r != t and
                                           ((address?(buflen) and !pointer?(buflen)) or buflen == t)
                isolate do
                                    ld  a, r unless r_in_a
                                    ld  t, buflen unless buflen == t
                                    scf
                                    rla              # carry <- a <- 1
                                    ld  r, a
                    buffmul         ld  hl, bufend   # multiply buffer[] * 2 + carry using BCD
                                    ld  b, t
                    nextadd         dec hl
                                    ld  a, [hl]
                                    adc a
                                    daa
                                    ld  [hl], a
                                    djnz nextadd
                                    jr  NC, nbufext  # no carry
                                    inc t            # extend buffer on carry
                                    dec hl
                                    ld  [hl], 1      # put 1 in new place
                    nbufext         sla r            # carry <- r <- 0
                                    jp  NZ, buffmul
                end
            end
            ##
            # Creates a routine that converts an unsigned binary integer of an arbitrary size to a BCD string.
            #
            # +bufend+:: A direct address of the byte immediately following an end of the BCD memory area
            #            as an integer or a label or a pointer to the memory address holding the +bufend+ value.
            # +input+:: An address of the binary integer being converted as an integer, a label, a pointer or +rr+.
            #
            # After the conversion is done the +c'+ register will hold the number of bytes written to the buffer
            # and +hl'+ will hold the address of the first byte of the result.
            #
            # Provide a large enough BCD buffer. E.g. for a 32-bit integer, 5 bytes (10 digits) should be enough.
            #
            # Options:
            # * +size+:: A byte size of the integer being converted as an integer, a label, a pointer or
            #            an 8-bit register.
            # * +r+:: A temporary register used in an alternative register set: +d+ or +e+.
            # * +rr+:: A 16-bit register: +de+ or +hl+.
            # * +byteorder+:: The order of bytes in the integer being converted: +:lsb+ or +:msb+.
            # * +input_end+:: Indicates that +input+ points to the byte after the end of the integer payload
            #                 instead of pointing to its beginning.
            #
            # Modifies: +af+, +b+, +rr+, +bc'+, +hl'+, +r'+.
            def utobcd(bufend, input=de, size: 4, r: d, rr: de, byteorder: :lsb, input_end:false)
                raise ArgumentError unless address?(bufend) and
                                           (address?(input) or input == rr) and
                                           (address?(size) or (register?(size) and size.bit8?)) and
                                           [de, hl].include?(rr) and [d, e].include?(r)
                raise ArgumentError, "input_end should be a boolean" unless [true, false].include?(input_end)
                adjust_input = case byteorder
                when :lsb then !input_end
                when :msb then input_end
                else
                    raise ArgumentError, "byteorder should be :lsb or :msb"
                end
                isolate do
                    if address?(size) and pointer?(size)
                            ld   a, size
                            ld   b, a
                    elsif adjust_input and byteorder == :lsb and
                                (pointer?(input) or pointer?(size) or !address?(input) or !address?(size))
                            ld   a, size unless size == a
                            ld   b, a unless size == b
                    else
                            ld   b, size unless size == b
                    end
                    if adjust_input and address?(input) and address?(size) and
                                        !pointer?(input) and !pointer?(size)
                            ld   rr, input + size if byteorder == :lsb
                            ld   rr, input - size if byteorder == :msb
                    else
                            ld   rr, input unless input == rr
                        if adjust_input
                            adda_to *rr.split if byteorder == :lsb
                            sub_from b, *rr.split if byteorder == :msb
                        end
                    end
                    unless pointer?(bufend)
                            xor  a
                            ld   [bufend - 1], a
                    end
                            exx
                    if pointer?(bufend)
                            ld   hl, bufend
                            dec  hl
                            ld   [hl], 0
                    end
                            ld   c, 1
                            exx
                    loopi   label
                            dec  rr if byteorder == :lsb
                            ld   a, [rr]
                            inc  rr if byteorder == :msb
                            exx
                            utobcd_step(bufend, r, c, c, true)
                            exx
                            djnz loopi
                end
            end
            ##
            # Creates a routine that reads BCD digits from the memory buffer, one at a time, into the accumulator.
            #
            # As a side effect the buffer will be zeroed after execution of this routine unless
            # +preserve_in+ is set.
            #
            # Provide a +block+ of code which will receive digits. The block will be inserted twice.
            # On the first digit CF=1, on subsequent CF=0. Alternatively set +skip_leading0:+ to +true+.
            # In this instance CF will be always 0 and the block will not be evaluated if the first digit
            # is 0. The +block+ should produce a relatively small code (< 60 bytes).
            #
            # +buffer+:: A memory address of the beginning of the BCD data as an integer, a label,
            #            a pointer or +hl+.
            # +size+:: The size in bytes of the BCD data as an integer, a label, a pointer or
            #          an 8-bit register.
            #
            # Options:
            # * +skip_leading0+:: If true the +block+ will not be executed on the first digit if it's 0.
            # * +preserve_in+:: An optional register: +c+, +d+ or +e+ which will preserve the current
            #                   value of the processed byte of the buffer.
            #
            # _NOTE_:: The code in +block+ must preserve +hl+, +b+ and +preserve_in+ registers if set.
            #
            # Modifies: +af+, +hl+, +b+ and optionally +preserve_in+.
            #
            # Example:
            #
            #       bcdtoa hl, b do |eoc|
            #         jr  NC, pr1  # skip zero check if not first
            #         jr  Z, eoc   # skip leading 0
            #   pr1   add ?0.ord
            #         rst 0x10     # print a character
            #       end
            #       # or
            #       bcdtoa(hl, b, skip_leading0: true) do |eoc|
            #         add ?0.ord
            #         rst 0x10     # print a character
            #       end
            def bcdtoa(buffer=hl, size=b, skip_leading0:false, preserve_in:nil, &block)
                raise ArgumentError unless (address?(buffer) or buffer == hl) and
                                           (address?(size) or (register?(size) and size.bit8?)) and
                                           (!preserve_in or [c,d,e].include?(preserve_in))
                isolate do
                    if pointer?(size)
                            ld   a, size
                            ld   b, a
                    elsif size != b
                            ld   b, size
                    end
                            ld   hl, buffer unless buffer == hl
                            xor  a
                    if skip_leading0
                            ld   preserve_in, [hl] if preserve_in
                            rld
                            jr   NZ, noskip0
                            jr   skip0
                    else
                            scf
                    end
                loop_a      label
                            ld   preserve_in, [hl] if preserve_in
                            rld
                noskip0     ns(&block)
                            xor  a
                skip0       rld
                            ns(&block)
                            ld   [hl], preserve_in if preserve_in
                            inc  hl
                            xor  a
                            djnz loop_a
                end
            end
        end

        include Z80
    end
end

# DEPRECATED
Z80MathInt = ::Z80::MathInt # :nodoc:
