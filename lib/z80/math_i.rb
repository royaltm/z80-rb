# -*- coding: BINARY -*-
module Z80
    ##
    # =Z80::MathInt - integer math common routines.
    #
    # in Z80::MathInt::Macros
    #
    # Example:
    #   require 'z80'
    #
    #   class Program
    #     include Z80
    #
    #     macro_import  MathInt
    #
    #                   ld  hl, [dividend.words[0]]
    #                   exx
    #                   ld  hl, [dividend.words[1]]
    #                   ld  de, [divisor]
    #                   divmod32_16
    #                   ld  [result.words[1]], hl
    #                   exx
    #                   ld  [result.words[0]], hl
    #                   ld  [remainder], de
    #                   # convert integer to bcd
    #                   utobcd bcdbufend, result, size: 4
    #                   exx
    #                   ld  hl, bcdbufend
    #                   sub_from c, h, l
    #                   # print integer
    #                   bcdtoa hl, c do |eoc|
    #                     jr  NC, pr1  # skip check if not first
    #                     jr  Z, eoc   # skip if 0
    #               pr1   add '0'.ord
    #                     rst 0x10
    #                   end
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
            # Creates a routine that changes the sign of a twos complement 16-bit integer depending on
            # the content of the +sgn+.
            #
            # When +sgn+ equals 0 an integer is left unmodified: +th+|+tl+ = +sh+|+sl+.
            # When +sgn+ equals -1 an integer's sign is being changed: +th+|+tl+ = 0 - +sh+|+sl+.
            #
            # _NOTE_:: Other values of +sgn+ will render unexpected results.
            #
            # Uses: +af+, +th+, +tl+, preserves: +sgn+ and optionally: +sh+, +sl+.
            #
            # T-states: 32
            #
            # * +sh+:: An 8-bit register holding MSB of the input 16-bit integer.
            # * +sl+:: An 8-bit register holding LSB of the input 16-bit integer.
            # * +sgn+:: An 8-bit sign register, must be 0 or -1 (0xFF).
            # Options:
            # * +th+:: An 8-bit MSB output register, may be the same as +sh+ or +sl+.
            # * +tl+:: An 8-bit LSB output register, may be the same as +sl+.
            def twos_complement16_by_sgn(sh, sl, sgn, th:sh, tl:sl)
                if [sh, sl, tl, sgn].include?(a) or sh == sl or th == tl or sh == tl or [sh, sl, th, tl].include?(sgn)
                    raise ArgumentError, "twos_complement16_by_sgn: invalid arguments!"
                end
                ns do
                        ld    a, sgn
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
            # Creates a routine that changes the sign of a twos complement 16-bit integer in +sh+|+sl+.
            #
            # Uses: +af+, +th+, +tl+, preserves optionally: +sh+, +sl+.
            #
            # T-states: 24
            #
            # * +sh+:: An 8-bit register holding MSB of the input 16-bit integer.
            # * +sl+:: An 8-bit register holding LSB of the input 16-bit integer.
            # Options:
            # * +th+:: An 8-bit MSB output register, may be the same as +sh+ or +sl+.
            # * +tl+:: An 8-bit LSB output register, may be the same as +sl+.
            def neg16(sh, sl, th:sh, tl:sl)
                if [sh, sl, tl].include?(a) or sh == sl or th == tl or sh == tl
                    raise ArgumentError, "neg16: invalid arguments!"
                end
                ns do
                        xor  a
                        sub  sl
                        ld   tl, a
                        sbc  a, a
                        sub  sh
                        ld   th, a unless th == a
                end
            end
            ##
            # Creates a routine that changes the sign of a twos complement 32-bit integer in +t3+|+t2+|+t1+|+t0+.
            #
            # Uses: +af+, +t3+, +t2+, +t1+, +t0+.
            #
            # T-states: 48
            def neg32(t3, t2, t1, t0)
                raise ArgumentError, "neg32 invalid arguments!" if [t3, t2, t1, t0].include?(a) or
                                                                   [t3, t2, t1, t0].any?{|r| !register?(r) || !r.bit8? } or
                                                                   [t3, t2, t1, t0].uniq.size != 4
                ns do
                        xor  a
                        sub  t0
                        ld   t0, a
                        sbc  a, a
                        sub  t1
                        ld   t1, a
                        sbc  a, a
                        sub  t2
                        ld   t2, a
                        sbc  a, a
                        sub  t3
                        ld   t3, a
                end
            end
            ##
            # Creates a routine that adds an 8-bit accumulator value to a 16-bit +th+|+tl+ register pair.
            #
            # Uses: +af+, +th+, +tl+.
            #
            # ====Note:
            # Although this method is often a more convenient way to add an 8-bit unsigned integer
            # to a 16-bit pair of registers, it sets flags in the following way:
            #
            #   ZF: (tt + a) & 0xFF00 == 0
            #   CF: (((tt + a) & 0xFF) + (((tt + a) & 0xFF00) >> 8)) > 0xFF
            #
            # T-states: 20
            #
            # * +th+:: A target MSB 8-bit register.
            # * +tl+:: A target LSB 8-bit register.
            def adda_to(th, tl)
                if th == tl or [th,tl].include?(a)
                    raise ArgumentError, "adda_to: invalid arguments!"
                end
                ns do
                        add  tl
                        ld   tl, a
                        adc  th
                        sub  tl
                        ld   th, a
                end
            end
            ##
            # Creates a routine that subtracts an 8-bit +s+ register value from a 16-bit +th+|+tl+ register pair.
            #
            # Uses: +af+, +th+, +tl+, preserves: +s+.
            #
            # ====Note:
            # Although this method is often a more convenient way to subtract an 8-bit unsigned register value
            # from a 16-bit pair of registers, it does not set flags properly.
            #
            # T-states: 24
            #
            # * +s+:: A subtractor as an 8-bit register except the accumulator.
            # * +th+:: A target MSB 8-bit register.
            # * +tl+:: A target LSB 8-bit register.
            def sub_from(s, th, tl)
                if th == tl or [s,th,tl].include?(a)
                    raise ArgumentError, "sub_from: invalid arguments!"
                end
                ns do
                    ld   a, tl
                    sub  s
                    ld   tl, a
                    sbc  a
                    add  th
                    ld   th, a
                end
            end
            ##
            # Creates a routine that adds a 16-bit integer in +tt+ to a 24-bit integer in +th8+|+tl16+.
            # Returns the result in +a+|+tl16+.
            #
            # Modifies: +af+, +tl16+, preserves: +th8+.
            #
            # T-states: 27 signed / 19 unsigned
            #
            # * +th8+:: An 8-bit register holding the highest 8 bits of the value to be added to, must not be
            #           the +accumulator+.
            # * +tl16+:: A 16-bit register holding the lowest 16 bits of the value to be added to (+hl+, +ix+ or +iy+).
            # * +tt+:: A 16-bit register holding the value to be added (+bc+, +de+ or +sp+).
            # Options:
            # * +signed+:: +true+ if the 16-bit value being added is a twos complement signed integer.
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
            # Creates a routine that performs a multiplication of a signed 8-bit +k+ * 8-bit signed +m+.
            #
            # See Macros.mul for details.
            def mul_signed(k=d, m=a, tt:de, clrhl:true)
                th, tl = tt.split
                raise ArgumentError if tt == hl or m == th
                isolate do
                            ld   th, k unless k == th
                            ld   a, m unless m == a
                            anda a
                            jp   P, mul_it      # m >= 0
                            ld   tl, a
                            xor  a
                            sub  th
                            ld   th, a
                            xor  a
                            sub  tl
                    mul_it  mul(th, a, tt:tt, clrhl:clrhl, signed_k:true)
                end
            end
            ##
            # Creates a routine that performs a multiplication of an 8-bit integer +k+ * 8-bit unsigned +m+.
            # Returns the result as a 16-bit integer in +hl+.
            #
            # Optionally the result in the +hl+ is being accumulated.
            #
            # As a side-effect accumulator is always cleared to 0 and +m+ and +k+ are left unmodified
            # if they are not an: +accumulator+ nor part of +tt+.
            #
            # Uses: +af+, +hl+, +tt+, optionally preserves: +k+, +m+.
            #
            # * +k+::        A multiplicant as an immediate value or an 8-bit register.
            # * +m+::        A multiplicator as an immediate value or an 8-bit register.
            # Options:
            # * +tt+::       A 16-bit temporary register (+de+ or +bc+).
            # * +clrhl+::    If the result should be set or accumulated, if +false+ acts like: +hl+ += +k+ * +m+.
            # * +signed_k+:: If the multiplicant (+k+) represents a twos complement signed integer (-128..127).
            def mul(k=d, m=a, tt:de, clrhl:true, signed_k:false)
                th, tl = tt.split
                raise ArgumentError if tt == hl or m == th
                isolate do
                            ld   th, k unless k == th
                            ld   a, m unless m == a
                            ld   tl, 0
                    if clrhl
                            ld   h, tl
                            ld   l, tl
                    end
                    if signed_k
                        loop1   sra  th
                    else
                        loop1   srl  th
                    end
                            rr   tl
                            add  a, a
                            jr   NC, noadd
                            add  hl, tt
                    noadd   jr   NZ, loop1
                end
            end
            ##
            # Creates a routine that performs a multiplication of an 8-bit integer +k+ * 8-bit unsigned +m+.
            # Returns the result as a 16-bit integer in +hl+.
            #
            # Creates a fast, unrolled code so +m+ should be a constant value in the range: 0..255.
            #
            # Optionally the result in the +hl+ is being accumulated.
            #
            # As a side-effect +k+ is left unmodified if +k+ is not part of +tt+.
            #
            # Uses: +f+, +hl+, +tt+, optionally preserves: +k+.
            #
            # * +k+::        A multiplicant as an immediate value or an 8-bit register.
            # * +m+::        A multiplicator value as a constant 8-bit integer.
            # Options:
            # * +tt+::       A 16-bit temporary register (+de+ or +bc+).
            # * +clrhl+::    If the result should be set or accumulated, if +false+ acts like: +hl+ += +k+ * +m+.
            # * +signed_k+:: If the multiplicant (+k+) represents a twos complement signed integer (-128..127).
            def mul_const(k=d, m=0, tt:de, clrhl:true, signed_k:false)
                th, tl = tt.split
                raise ArgumentError unless tt != hl and m.is_a?(Integer) and (0..255).include?(m)
                isolate do
                            ld   th, k unless k == th
                            ld   tl, 0
                    if clrhl
                            ld   h, tl
                            ld   l, tl
                    end
                    begin
                        if signed_k
                            sra  th
                        else
                            srl  th
                        end
                            rr   tl
                            add  hl, tt if (m & 0x80) == 0x80
                    end while (m = (m << 1) & 0xFF) != 0
                end
            end
            ##
            # Creates a routine that performs a multiplication of an unsigned 16-bit integer +mh+|+ml+ * 8-bit unsigned +m+.
            # Returns the result as a 16-bit unsigned integer in +hl+.
            #
            # Optionally accumulates the result in +hl+.
            #
            # Breaks on overflow with +CF+=1.
            # 
            # As a side-effect +m+ is cleared to 0 when CF=0 and +ml+ and +mh+ are left unmodified
            # if they are not part of +tt+.
            #
            # Uses: +f+, +hl+, +m+, +tt+, optionally preserves: +mh+, +ml+.
            #
            # * +mh+::    The MSB part of the multiplicant as an immediate value or an 8-bit register.
            # * +ml+::    The LSB part of the multiplicant as an immediate value or an 8-bit register.
            # * +m+::     An 8-bit multiplicator register, must not be a part of the +tt+.
            # Options:
            # * +tt+::    A 16-bit temporary register (+de+ or +bc+).
            # * +clrhl+:: If the result should be set or accumulated, if +false+ acts like: +hl+ += +mh+|+ml+ * +m+.
            def mul8_c(mh=h, ml=l, m=a, tt:de, clrhl:true)
                th, tl = tt.split
                raise ArgumentError if tt == hl or [th,tl].include?(m) or tl == mh or th == ml or !register?(m)
                isolate do |eoc|
                            ld   tl, ml unless ml == tl
                            ld   th, mh unless mh == th
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
            # Creates a routine that performs a multiplication of a 16-bit integer +mh+|+ml+ * 8bit unsigned +m+.
            # Returns the result as a 16-bit integer in +hl+.
            #
            # Optionally the result in the +hl+ is being accumulated.
            #
            # Optionally multiplies the result by 2 if +double+ option is +true+.
            #
            # As a side-effect +m+ is always cleared to 0 and +ml+ and +mh+ are left unmodified
            # if they are not part of +tt+.
            #
            # Uses: +f+, +hl+, +m+, +mh+, +ml+, +tt+.
            #
            # * +mh+::    The MSB part of the multiplicant as an immediate value or an 8-bit register.
            # * +ml+::    The LSB part of the multiplicant as an immediate value or an 8-bit register.
            # * +m+::     An 8-bit multiplicator register, must not be a part of the +tt+.
            # Options:
            # * +tt+::    A 16-bit temporary register (+de+ or +bc+).
            # * +clrhl+:: If the result should be set or accumulated, if +false+ acts like: +hl+ += +mh+|+ml+ * +m+.
            # * +double+:: +true+ if the result should be multiplied by 2 (+mh+|+ml+ * 2).
            # * +optimize+:: What is more important: +:time+ or +:size+? Applies only if +double+ is +false+.
            def mul8(mh=h, ml=l, m=a, tt:de, clrhl:true, double:false, optimize: :time)
                th, tl = tt.split
                raise ArgumentError if tt == hl or [th,tl].include?(m) or tl == mh or th == ml or !register?(m)
                isolate do |eoc|
                            ld   tl, ml unless ml == tl
                            ld   th, mh unless mh == th
                            ld   hl, 0 if clrhl
                    unless double
                        if optimize == :size
                            jr   muls1
                        elsif optimize == :time
                            srl  m
                            jr   C, doadd
                            jr   Z, eoc
                        else
                            raise ArgumentError, "optimize should be :time or :size"
                        end
                    end
                    loop1   sla  tl
                            rl   th
                    muls1   srl  m
                            jr   NC, noadd
                    doadd   add  hl, tt
                    noadd   jr   NZ, loop1
                end
            end
            ##
            # Creates a routine that performs a multiplication of an unsigned 16-bit integer +mh+|+ml+ * 8-bit unsigned +m+.
            # Returns the result as a 24-bit unsigned integer in +a+|+hl+.
            #
            # Optionally accumulates the result in +a+|+hl+.
            #
            # Uses: +af+, +bc+, +de+, +hl+.
            #
            # * +mh+::     The MSB part of the multiplicant as an immediate value or an 8-bit register.
            # * +ml+::     The LSB part of the multiplicant as an immediate value or an 8-bit register.
            # * +m+::      A multiplicator register, it must not be the +accumulator+ or +t+ or be a part of +tt+.
            # Options:
            # * +t+::      An 8-bit temporary register, it must not be the +accumulator+ or +m+ or be a part of +tt+.
            # * +tt+::     A 16 bit temporary register (+de+ or +bc+).
            # * +clrahl+:: If +a+|+hl+ should be set or accumulated, if +false+ acts like: +a+|+hl+ += +mh+|+ml+ * +m+.
            def mul8_24(mh=h, ml=l, m=b, t:c, tt:de, clrahl:true)
                th, tl = tt.split
                raise ArgumentError if tt == hl or [a,th,tl,t].include?(m) or [a,th,tl,m].include?(t) or
                                                             tl == mh or th == ml or !register?(m) or !register?(t)
                isolate do |eoc|
                                ld  tl, ml unless ml == tl
                                ld  th, mh unless mh == th
                    if clrahl
                                ld  hl, 0
                                xor a
                                ld  t, a
                    else
                                ld  t, 0
                    end
                                srl m           # 0 -> multiplicator -> carry
                                jp  NZ, loop1   # m != 0 ? start regular loop
                                jr  C, skadd    # m == 1 ? add and quit
                                jp  eoc         # m == 0 ? just quit
                    loop1       jr  NC, noadd   # carry == 0 ? don't add
                                add hl, tt      # add multiplicant to result lo16
                                adc a, t        # add multiplicant to result hi8
                    noadd       sla tl          # multiplicant *= 2
                                rl  th
                                rl  t
                                srl m           # 0 -> multiplicator -> carry
                                jp  NZ, loop1   # m != 0 ? loop
                    skadd       add hl, tt      # last add b.c. carry == 1
                                adc a, t
                end
            end
            ##
            # Creates a routine that performs a multiplication of an unsigned 16-bit +mh+|+ml+ * 8-bit unsigned +m+.
            # Returns the result as a 24-bit unsigned integer in +a+|+hl+.
            #
            # Creates a fast, unrolled code so +m+ should be a constant value in the range: 0..255.
            #
            # Optionally accumulates the result in +a+|+hl+.
            #
            # Sets flags: CF: 0 and ZF: 1 if the result fits in 16bits.
            #
            # Uses: +af+, +t+, +tt+, +hl+.
            #
            # * +mh+::     The MSB part of the multiplicant as an immediate value or an 8-bit register.
            # * +ml+::     The LSB part of the multiplicant as an immediate value or an 8-bit register.
            # * +m+::      A multiplicator value as a constant 8-bit integer.
            # Options:
            # * +t+::      An 8-bit temporary register, it must not be the +accumulator+ or be a part of +tt+.
            # * +tt+::     A 16 bit temporary register (+de+ or +bc+).
            # * +clrahl+:: If +a+|+hl+  should be set or accumulated, if +false+ acts like: +a+|+hl+ += +mh+|+ml+ * +m+.
            def mul_const8_24(mh=h, ml=l, m=0, t:c, tt:de, clrahl:true)
                th, tl = tt.split
                throw ArgumentError unless m.is_a?(Integer) and (0..255).include?(m) and [bc, de].include?(tt) and
                                                                     ![h, l, th, hl, a].include?(t) and
                                                                     tl != mh and th != ml
                isolate do |eoc|
                                ld  tl, ml unless ml == tl
                                ld  th, mh unless mh == th
                    if clrahl
                                xor a
                                ld  t, a
                        if (m & 1) == 1
                                ld  l, tl  unless ml == l # hl = tt * 1
                                ld  h, th  unless mh == h
                        else
                                ld  hl, 0
                        end
                    else
                                ld  t, 0
                        if (m & 1) == 1
                                add hl, tt
                                adc a, t
                        elsif m == 0
                                cp  a     # CF=0 and ZF=1 in case when m == 0
                        end
                    end
                    while (m >>= 1) != 0
                                sla tl
                                rl  th
                                rl  t
                        if (m & 1) == 1
                                add hl, tt
                                adc a, t  # CF=0 and ZF=1 in case when result 16 bit
                        end
                    end
                end
            end
            ##
            # Creates a routine that performs a multiplication of a 16-bit integer (+hl+) by an unsigned 16-bit integer +mm+ (+bc+ or +de+).
            # Returns the 32-bit integer result in +hl+|+hl'+.
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
            #
            # * +mm+::      A 16bit multiplicator register (+bc+ or +de+).
            # * +tt'+::     A 16bit tempoarary register (+bc+ or +de+) of the alternative set.
            # Options:
            # * +clrhlhl+:: If +hl+|+hl'+ should be cleared, if +false+ acts like: +hl+|+hl'+ += +hl+ * +mm+;
            #               also it may be a 32-bit constant, in this instance acts like: +hl+|+hl'+ = +clrhlhl+ + +hl+ * +mm+.
            # * +signed_hl+:: If the multiplicant (+hl+) represents a twos complement signed integer (-32768..32767).
            # * +optimize+:: What is more important: +:time+ (117 bytes) or +:size+ (51 bytes)?
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
            # Creates a routine that performs an euclidean division of unsigned: +k+ / +m+.
            # Returns a quotient in +k+ and a remainder in +accumulator+.
            #
            # This routine can be stacked up one after another to divide arbitrary size dividends.
            # Just preserve the +a+ and +m+ registers and pass +false+ to +clrrem:+ on subsequent routines.
            # Start with the most significant byte of the dividend:
            #
            #    ns :divide24_8 do |eoc| # divide (l|de)/c
            #      divmod l, c, check0:eoc, check1:eoc
            #      divmod d, c, clrrem:false
            #      divmod e, c, clrrem:false
            #      anda a # clear CF
            #    end
            #
            # Uses: +af+, +b+, +k+, preserves: +m+.
            #
            # * +k+:: A dividend as an 8-bit register except: +a+, +b+.
            # * +m+:: A divisor as an 8-bit register except: +a+, +b+, +k+.
            #
            # Options:
            # * +clrrem+:: Clears a reminder (+accumulator+). If this is +false+, +check0+ and +check1+
            #              options are being ignored.
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the +accumulator+ is being altered.
            #              +CF+ should be ignored if +check0+ is +false+. It may also be a label
            #              (within the relative jump range) to indicate where to jump if +m+ equals 0.
            # * +check1+:: Checks if a divisor equals 1, a hot path optimization. It may also be a label
            #              to indicate where to jump if +m+ equals 1.
            # * +modulo+:: Calculates a remainder only.
            # * +optimize+:: What is more important: +:time+ or +:size+?
            def divmod(k, m, clrrem:true, check0:true, check1:true, modulo:false, optimize: :time)
                unless clrrem
                    check0 = false
                    check1 = false
                end
                raise ArgumentError unless [d, e, h, l, c].include?(k) and [d, e, h, l, c].include?(m) and k != m
                raise ArgumentError, "optimize should be :time or :size" unless [:size, :time].include?(optimize)
                isolate do |eoc|
                    check1 = eoc if check1 == true
                    check0 = eoc if check0 == true
                    if check0 or check1
                                    ld  a, m
                                    cp  1
                                    jr  C, check0 if check0 # division by 0
                        if check1
                            if optimize == :size
                                    jr  NZ, divstrt  # division by m > 1
                            else
                                    jp  NZ, divstrt  # division by m > 1
                            end
                                    xor a            # clear rest
                                    jp  check1       # division by 1
                        end
                    end
                    divstrt         ld  b, 8
                    if clrrem
                                    xor a            # a = 0
                        if optimize == :time
                            findhi  sla k            # align highest set bit at CF
                                    jr  C, found1
                                    djnz findhi
                                    jp  eoc          # k == 0
                        end
                    end
                    loopfit         sla k            # carry <- k <- 0
                    found1          adc a            # carry <- a <- carry
                    unless clrrem
                                    jr  C, fits      # a >= 256
                    end
                                    cp  m            # a - m
                                    jr  NC, fits     # a >= m
                                    djnz loopfit     # loop
                                    ccf if check0    # clear carry only when check0
                    if optimize == :size
                                    jr  eoc
                    else
                                    jp  eoc
                    end
                    fits            sub m            # a = a - m (rest)
                    unless modulo
                                    inc k            # k <- 1 (quotient)
                    end
                                    djnz loopfit     # loop
                                    ora  a if check0 # clear carry only when check0
                end
            end
            ##
            # Creates a routine that performs an euclidean division of unsigned: +hl+ / +m+.
            # Returns a quotient in +hl+ and a remainder in +a+.
            #
            # _NOTE_:: A stacked Macros.divmod presents a faster (up to 12%) but a significantly
            #          larger (x1.5) alternative.
            #
            # Uses: +af+, +b+, +hl+, preserves: +m+.
            #
            # * +m+:: A divisor, one of: +c+, +d+ or +e+.
            # Options:
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the register +a+ is being altered.
            #              +CF+ should be ignored if +check0+ is +false+.
            # * +check1+:: Checks if a divisor is 1, a hot path optimization.
            # * +modulo+:: Calculates a remainder only.
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
                                    jp  NZ, divstrt  # division by m > 1
                                    xor a            # clear rest
                                    jp  check1       # division by 1
                        end
                    end
                    divstrt         xor a            # a = 0
                                    ld  b, 16
                    findhi          add hl, hl       # align highest set bit at CF
                                    jr  C, found
                                    djnz findhi
                                    jp  eoc          # hl == 0
                    loopfit         add hl, hl       # carry <- hl <- 0
                    found           adc a            # carry <- a <- carry
                                    jr  C, fits      # a >= 256
                                    cp  m            # a - m
                                    jr  NC, fits     # a >= m
                                    djnz loopfit     # loop
                                    ccf if check0    # clear carry only when check0
                                    jp  eoc
                    fits            sub m            # a = a - m (rest)
                    unless modulo
                                    inc l            # hl <- 1 (quotient)
                    end
                                    djnz loopfit     # loop
                                    ora  a if check0 # clear carry only when check0
                end
            end
            ##
            # Creates a routine that performs an euclidean division of unsigned 16-bit: +hl+ / +de+.
            # Returns a quotient in +hl+ and a remainder in +bc+.
            #
            # Uses: +af+, +bc+, +hl+, +x+, preserves: +de+.
            #
            # * +x+:: A temporary 8-bit register, one of: +ixh+, +ixl+, +iyh+ or +iyl+.
            # Options:
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the +accumulator+ is being altered. 
            #              +CF+ should be ignored if +check0+ is +false+.
            # * +check1+:: Checks if a divisor is 1, a hot path optimization.
            # * +modulo+:: Calculates a remainder only.
            # * +quick8+:: Checks if a divisor fits in 8 bits and in this instance uses a different,
            #              optimized for 8-bit division code. It can also be set to +:divmod8+ to use
            #              Macros.divmod8 (smaller code) instead of stacked Macros.divmod for an 8-bit division.
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
                                        divmod h, e, check0:(check0 && qcheck0), check1:(check1 && qcheck1), modulo:modulo, optimize: :time
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
                                        jp  NZ, div16strt # division by m > 1
                                        ld  bc, 0         # clear rest
                                        jp  check1        # division by 1
                            end
                        end
                    end
                    div16strt           xor a            # a = 0 hi remainder
                                        ld  c, a         # c = 0 lo remainder
                                        ld  b, 16
                    loopfit             add hl, hl       # carry <- hl <- 0
                                        rl  c            # carry <- c <- carry
                                        adc a            # carry <- a <- carry
                                        cp  d            # a - d
                                        jr  NC, fitshi   # a >= d
                                        djnz loopfit     # loop
                                        ccf if check0
                                        jp  over
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
            # Creates a routine that performs an euclidean division of unsigned 32-bit: +hl+|+hl'+ / +m+.
            # Returns a quotient in +hl+|+hl'+ and a remainder in +a+.
            #
            # _NOTE_:: A stacked Macros.divmod presents a faster (9-16%) but a significantly
            #          larger (x2) alternative.
            #
            # Uses: +af+, +af'+, +b+, +b'+, +hl+, +hl'+, +mt'+, preserves: +m+.
            #
            # * +m+:: A divisor, one of: +c+, +d+ or +e+.
            # * +mt'+:: A temporary register from the alternative set: +c+, +d+ or +e+.
            # Options:
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the register +a+ is being altered.
            #              +CF+ should be ignored if +check0+ is +false+.
            # * +check1+:: Checks if a divisor is 1, a hot path optimization.
            # * +modulo+:: Calculates a remainder only.
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
            # Uses: +af+, +af'+, +bc+, +bc'+, +hl+, +hl'+, +de'+, +x+, preserves: +de+.
            #
            # * +x+:: A temporary 8-bit register, one of: +ixh+, +ixl+, +iyh+ or +iyl+.
            # Options:
            # * +check0+:: Checks if a divisor is 0, in this instance CF=1 indicates an error 
            #              and nothing except the register +a+ is being altered.
            #              +CF+ should be ignored if +check0+ is +false+.
            # * +check1+:: Checks if a divisor is 1, a hot path optimization.
            # * +modulo+:: Calculates a remainder only.
            # * +quick8+:: Checks if a divisor fits in 8 bits and in this instance
            #              uses a different, optimized code.
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
            # Uses: +af+, +bc+, +de+, +hl+.
            #
            # T-states: ~ 601.7 (46 for seed=65535, 247 for seed<=872, max: 655)
            #
            # Expects a seed (s0) in +hl+ and produces the result (s1) in +hl+.
            #
            # Modifies: +af+, +bc+, +de+, +hl+.
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
                    multi75         mul_const8_24(h, l, 75, t:c, tt:de, clrahl:true)
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
                    mloopck         jr  Z, fits     # b==0: (seed + 1) * 75 == 0x0001_nnnn
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
            #            as an integer or a label.
            # +r+:: An 8-bit register holding the integer during conversion: +c+, +d+ or +e+.
            # +buflen+:: The current byte size of the BCD buffer as an integer, a label or +t+.
            # +t+:: The register holding the current byte size of the BCD buffer: +c+, +d+ or +e+.
            # +r_in_a+:: +true+ if the integer to convert is provided in +accumulator+ instead of in +r+.
            #
            # Modifies: +af+, +b+, +r+, +t+, +hl+.
            def utobcd_step(bufend, r, buflen=1, t=c, r_in_a=false)
                raise ArgumentError unless (address?(bufend) and !pointer?(bufend)) and
                                           [c,d,e].include?(r) and [c,d,e].include?(t) and r != t and
                                           ((address?(buflen) and !pointer?(buflen)) or buflen == t)
                isolate do
                                    ld  a, r unless r_in_a
                                    ld  t, buflen unless buflen == t
                                    scf
                                    rla              # carry <- a <- 1
                                    ld  r, a
                    buffmul         ld  hl, bufend - 1 # multiply buffer[] * 2 + carry using BCD
                                    ld  b, t
                    nextadd         ld  a, [hl]
                                    adc a
                                    daa
                                    ld  [hl], a
                                    dec hl
                                    djnz nextadd
                                    jp  NC, nbufext  # no carry
                                    inc t            # extend buffer on carry
                                    ld  [hl], 1      # put 1 in new place
                    nbufext         sla r            # carry <- r <- 0
                                    jp  NZ, buffmul
                end
            end
            ##
            # Creates a routine that converts an unsigned binary integer of an arbitrary size to a BCD string.
            #
            # +bufend+:: A direct address of the byte immediately following an end of the BCD memory area
            #            as an integer or a label.
            # +input+:: An address of the binary integer being converted as an integer, a label, a pointer or +rr+.
            #
            # Provide a large enough BCD buffer. E.g. for a 32-bit integer, 5 bytes (10 digits) should be enough.
            #
            # Options:
            # * +size+:: A byte size of the integer being converted as an integer, a label, a pointer or
            #            an 8-bit register.
            # * +r+:: A temporary register used in an alternative register set: +d+ or +e+.
            # * +rr+:: A 16-bit register: +de+ or +hl+.
            # * +byteorder+:: The order of bytes in the integer being converted: +:lsb+ or +:msb+.
            #
            # Modifies: +af+, +af'+, +b+, +rr+, +bc'+, +hl'+, +r'+.
            def utobcd(bufend, input=de, size: 4, r: d, rr: de, byteorder: :lsb)
                raise ArgumentError unless (address?(bufend) and !pointer?(bufend)) and
                                           (address?(input) or input == rr) and
                                           (address?(size) or (register?(size) and size.bit8?)) and
                                           [de, hl].include?(rr) and [d, e].include?(r)
                raise ArgumentError, "byteorder should be :lsb or :msb" unless [:lsb, :msb].include?(byteorder)

                isolate do
                    if address?(size) and pointer?(size)
                            ld   a, size
                            ld   b, a
                    elsif byteorder == :lsb and (pointer?(input) or pointer?(size) or !address?(input) or !address?(size))
                            ld   a, size unless size == a
                            ld   b, a unless size == b
                    else
                            ld   b, size unless size == b
                    end
                    if byteorder == :lsb and address?(input) and address?(size) and
                                              !pointer?(input) and !pointer?(size)
                            ld   rr, input + size
                    else
                            ld   rr, input unless input == rr
                            adda_to *rr.split if byteorder == :lsb
                    end
                            xor  a
                            ld   [bufend - 1], a
                            exx
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
