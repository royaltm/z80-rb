class Z80MathInt
  module Macros
    ##
    # Adds +a+ to +h+|+l+.
    #
    # Uses: +af+, +h+, +l+
    #
    # * +h+:: register input accumulator hi
    # * +l+:: register input accumulator lo
    def adda_to(h, l) # 20
        if h == l or [h,l].include?(a)
            raise ArgumentError, "adda_to invalid arguments!"
        end
        ns do
            add  l
            ld   l, a
            adc  h
            sub  l
            ld   h, a
        end
    end
    ##
    # Subtracts +r+ from +h+, +l+.
    #
    # Uses: +af+, +r+, +h+, +l+, preserves +r+
    #
    # * +r+:: register subtractor must not be +a+
    # * +h+:: register input accumulator hi
    # * +l+:: register input accumulator lo
    def sub_from(r, h, l) # 24
        if h == l or [r,h,l].include?(a)
            raise ArgumentError, "sub_from invalid arguments!"
        end
        ns do
          ld   a, l
          sub  r
          ld   l, a
          sbc  a
          add  h
          ld   h, a
        end
    end
    ##
    # Performs multiplication of unsigned 16bit +mh+|+ml+ * 8bit +m+ and returns result in +hl+.
    # Optionally accumulates result in +hl+
    # Breaks on overflow with CF=1.
    # 
    # Uses: +hl+, +m+, +mh+, +ml+, +th+, +tl+
    #
    # * +mh+::    register/value input multiplicant hi
    # * +ml+::    register/value input multiplicant lo
    # * +m+::     register input multiplicator
    # * +th+::    temporary register (d or b)
    # * +tl+::    temporary register (e or c)
    # * +clrhl+::  +true+ if should +hl+ be set (true) or accumulated (false),
    #   if +false+ acts like: +hl+ += +mh+|+ml+ * +m+
    def mul8_c(mh=h, ml=l, m=a, th=d, tl=e, clrhl = true)
      raise ArgumentError if th|tl == hl or [th,tl].include?(m) or tl == mh or th == ml
      ns do |eoc|
              ld  tl, ml unless ml == tl
              ld  th, mh unless mh == th
              ld  hl, 0 if clrhl
        loop1 srl m
              jr  NC, noadd
              add hl, th|tl
              jr  C, eoc
        noadd jr  Z, eoc
        skip1 sla tl
              rl  th
              jp  NC, loop1
      end
    end
    ##
    # Performs multiplication of unsigned 16bit +mh+|+ml+ * 8bit +m+ and returns result in +hl+.
    # Optionally accumulates result in +hl+
    # Optionally multiplies result * 2.
    #
    # Uses: +hl+, +m+, +mh+, +ml+, +th+, +tl+
    #
    # * +mh+::     register/value input multiplicant hi
    # * +ml+::     register/value input multiplicant lo
    # * +m+::      register input multiplicator
    # * +th+::     register temporary (d or b)
    # * +tl+::     register temporary (e or c)
    # * +clrhl+::  +true+ if should +hl+ be set (true) or accumulated (false),
    #   if +false+ acts like: +hl+ += +mh+|+ml+ * +m+
    # * +double+:: +true+ if should double the result (mh|ml * 2)
    def mul8(mh=h, ml=l, m=a, th=d, tl=e, clrhl = true, double = false)
      raise ArgumentError if th|tl == hl or [th,tl].include?(m) or tl == mh or th == ml
      ns do |eoc|
              ld  tl, ml unless ml == tl
              ld  th, mh unless mh == th
              ld  hl, 0 if clrhl
              jp  muls1 unless double
        loop1 sla  tl
              rl   th
        muls1 srl  m
              jr  NC, noadd
              add hl, th|tl
        noadd jr  NZ, loop1
      end
    end
    ##
    # Performs multiplication of unsigned 16bit +hl+ by 16bit +mm+ (+bc+ or +de+)
    # and returns result in 32 bit +hl+|+hl'+.
    #
    # Uses: +af+, +af'+, +hl+, +hl'+, +mm+, +tt'+
    #
    # * +mm+:: 16bit multiplicator (+bc+ or +de+)
    # * +tt'+:: 16bit tempoarary register (+bc+ or +de+)
    def mul16_32(mm=bc, tt=bc)
      raise ArgumentError unless [bc, de].include?(mm) and [bc, de].include?(tt)
      mh, ml = mm.split
      th, tl = tt.split
      ns do |eoc|
              ld  a, ml
              ora a            # a' ?= 0
              ex  af, af       # a' = ml, ZF' = a' == 0
              xor a            # a  = 0
              exx
              ld  h, a         # hl' = 0
              ld  l, a
              ld  th, a        # th'|tl' = 0
              ld  tl, a
              exx
              ora mh           # a |= mh
              jr  Z, multlo0   # mh <> 0

              ld  mh, h        # mh|ml = hl
              ld  ml, l
              ld  hl, 0        # hl = 0

              scf
              adc a            # carry <- mh <- 1
              jr  NC, noadd1

      shadd1  srl mh           # mh -> ml -> th'
              rr  ml
              exx
              rr  th
              add hl, th|tl    # hl' += th'|tl'
              exx
              adc hl, mh|ml    # hl  += mh|ml + carry
              add a            # carry <- ml
              jr  Z, multlo
              jp  C, shadd1

      noadd1  srl mh           # mh -> ml -> mh'
              rr  ml
              exx
              rr  th
              exx
              add a            # carry <- ml
              jr  Z, multlo
              jp  C, shadd1
              jp  noadd1

      multlo0 ld  ml, h        # mh = 0, ml = h, th' = l, tl' = 0
              ld  a, l
              exx
              ld  th, a
              exx
              ld  hl, 0        # hl = 0

      multlo  ex  af, af       # a = ml, ZF = a == 0
              jr  Z, eoc
              add a            # carry <- ml
              jr  NC, noadd2

      shadd2  srl ml           # ml -> mh' -> ml'
              exx
              rr  th
              rr  tl
              add hl, th|tl    # hl' += th'|tl'
              exx
              adc hl, mh|ml    # hl  += mh|ml + carry
              add a            # carry <- ml
              jr  Z, finlo
              jp  C, shadd2

      noadd2  srl ml           # ml -> mh' -> ml'
              exx
              rr  th
              rr  tl
              exx
              add a            # carry <- ml
              jr  Z, finlo
              jp  C, shadd2
              jp  noadd2

      finlo   jr  NC, eoc
              srl ml           # ml -> mh' -> ml'
              exx
              rr  th
              rr  tl
              add hl, th|tl    # hl' += th'|tl'
              exx
              adc hl, mh|ml    # hl  += mh|ml + carry
      end
    end
    ##
    # Performs euclidean divison. Divides +hl+ by +m+.
    # Returns quotient in +hl+ and remainder in +a+. +m+ remains unaltered.
    #
    # Uses: +af+, +b+, +m+, +hl+
    #
    # * +m+:: a divisor (+c+, +d+ or +e+)
    # * opts::
    #   - :check0:: (default +true+) checks if divisor is 0, in this instance CF indicates division error
    #               and nothing except +a+ register is altered on CF=1. If +false+ CF should be ignored.
    #   - :check1:: (default +true+) checks if divisor is 1, a hot path optimization
    #   - :modulo:: (default +false+) calculates remainder only, in this instance +hl+ will be 0
    #               when division is finished.
    def divmod8(m=c, opts = {})
      raise ArgumentError unless [c, d, e].include?(m)
      flags = {
        :check0 => true,
        :check1 => true,
        :modulo => false
      }.merge opts
      ns do |eoc|
        if flags[:check0] or flags[:check1]
                ld  a, m
                cp  1
                jr  C, eoc if flags[:check0] # division by 0
          if flags[:check1]
                  jp  NZ, divstrt # division by m > 1
                  xor a            # clear rest
                  jp  eoc          # division by 1
          end
        end
        divstrt xor a            # a = 0
                ld  b, 16
        loopfit add hl, hl       # carry <- hl <- 0
                adc a            # carry <- a <- carry
                cp  m            # a - m
                jr  NC, fits     # a >= m
                djnz loopfit     # loop
                ccf if flags[:check0]    # clear carry only when check0
                jp  eoc
        fits    sub m            # a = a - m (rest)
        unless flags[:modulo]
                  inc l          # hl <- 1 (quotient)
        end
                djnz loopfit     # loop
      end
    end
    ##
    # Performs euclidean divison. Divides +hl+ by +de+.
    # Returns quotient in +hl+ and remainder in +bc+. +de+ remains unaltered.
    #
    # Uses: +af+, +bc+, +de+, +hl+, +x+
    #
    # * +x+:: a temporary register (+ixh+, +ixl+, +iyh+ or +iyl+).
    # * opts::
    #   - :check0:: (default +true+) checks if divisor is 0, in this instance CF indicates division error
    #               and nothing except +a+ register is altered on CF=1. If +false+ CF should be ignored.
    #   - :check1:: (default +true+) checks if divisor is 1, a hot path optimization
    #   - :modulo:: (default +false+) calculates remainder only, in this instance +hl+ will be 0
    #               when division is finished.
    #   - :quick8:: (default +true+) checks if divisor fits in 8 bits and in this instance
    #               uses different, optimized code.
    def divmod16(x=ixl, opts = {})
      raise ArgumentError unless [ixh, ixl, iyh, iyl].include?(x)
      flags = {
        :check0 => true,
        :check1 => true,
        :modulo => false,
        :quick8 => true
      }.merge opts
      ns do |eoc|
        if flags[:check0] or flags[:check1] or flags[:quick8]
                xor a
                ora d
                jp  NZ, div16strt
          if flags[:quick8]
                divmod8 e, opts
                ld  b, 0
                ld  c, a
                jp  eoc
          elsif flags[:check0] or flags[:check1]
                ld  a, e
                cp  1
                jr  C, eoc if flags[:check0] # division by 0
            if flags[:check1]
                  jp  NZ, div16strt # division by m > 1
                  ld  bc, 0         # clear rest
                  jp  eoc           # division by 1
            end
          end
        end
        div16strt xor a            # a = 0 hi remainder
                  ld  c, a         # c = 0 lo remainder
                  ld  b, 16
        loopfit   add hl, hl       # carry <- hl <- 0
                  rl  c            # carry <- c <- carry
                  adc a            # carry <- a <- carry
                  cp  d            # a - d
                  jr  NC, fitshi   # a >= d
                  djnz loopfit     # loop
                  ccf if flags[:check0]
                  jp  over
        fitshi    ld  x, a
                  ld  a, c
                  jr  NZ, fitslo   # a > d, ignore e
                  cp  e            # a == d: c - e
                  jr  NC, fitslo   # a >= e
                  ld  a, x 
                  djnz loopfit     # loop
                  ccf if flags[:check0]
                  jp  over
        fitslo    sub e            # a = c - e
                  ld  c, a         # c = c - e
                  ld  a, x
                  sbc d            # a -= d
        unless flags[:modulo]
                  inc l            # hl <- 1 (quotient)
        end
                  djnz loopfit     # loop
        over      ld  b, a         # bc = remainder
      end
    end
    ##
    # Performs euclidean divison. Divides +hl+|+hl'+ by +m+.
    # Returns quotient in +hl+|+hl'+ and remainder in +a+. +m+ remains unaltered.
    #
    # Uses: +af+, +a'+, +b+, +b'+, +m+, +m'+, +hl+, +hl'+
    #
    # * +m+:: a divisor (+c+, +d+ or +e+)
    # * opts::
    #   - :check0:: (default +true+) checks if divisor is 0, in this instance CF indicates division error
    #               and nothing except +a+ register is altered on CF=1. If +false+ CF should be ignored.
    #   - :check1:: (default +true+) checks if divisor is 1, a hot path optimization
    #   - :modulo:: (default +false+) calculates remainder only, in this instance +hl+|+hl'+ will be 0
    #               when division is finished.
    def divmod32_8(m=c, opts={})
      raise ArgumentError unless [c, d, e].include?(m)
      flags = {
        :check0 => true,
        :check1 => true,
        :modulo => false
      }.merge opts
      ns do |eoc|
        if flags[:check0] or flags[:check1]
                ld  a, m
                cp  1
                jr  C, eoc if flags[:check0] # division by 0
          if flags[:check1]
                  jp  NZ, divstrt  # division by m > 1
                  xor a            # clear rest
                  jp  eoc          # division by 1
          end
        end
        divstrt   xor a            # a = 0
                  ld  b, 16
        loopfit1  add hl, hl       # carry <- hl <- 0
                  adc a            # carry <- a <- carry
                  cp  m            # a - m
                  jr  NC, fits1    # a >= m
                  djnz loopfit1    # loop
                  jp  divlo16
        fits1     sub m            # a = a - m (rest)
        unless flags[:modulo]
                  inc l          # hl <- 1 (quotient)
        end
                  djnz loopfit1    # loop

        divlo16   ex  af, af
                  ld  a, m
                  exx
                  ld  m, a
                  ex  af, af
                  ld  b, 16
        loopfit2  add hl, hl       # carry <- hl <- 0
                  adc a            # carry <- a <- carry
                  cp  m            # a - m
                  jr  NC, fits2    # a >= m
                  djnz loopfit2    # loop
                  ccf if flags[:check0] # clear carry only when check0
                  jp  over
        fits2     sub m            # a = a - m (rest)
        unless flags[:modulo]
                  inc l            # hl <- 1 (quotient)
        end
                  djnz loopfit2    # loop
        over      exx
      end
    end
    ##
    # Performs euclidean divison. Divides +hl+|+hl'+ by +de+.
    # Returns quotient in +hl+|+hl'+ and remainder in +bc+. +de+ remains unaltered.
    #
    # Uses: +af+, +a'+, +bc+, +bc'+, +de+, +de'+, +hl+, +hl'+, +x+
    #
    # * +x+:: a temporary register (+ixh+, +ixl+, +iyh+ or +iyl+).
    # * opts::
    #   - :check0:: (default +true+) checks if divisor is 0, in this instance CF indicates division error
    #               and nothing except +a+ register is altered on CF=1. If +false+ CF should be ignored.
    #   - :check1:: (default +true+) checks if divisor is 1, a hot path optimization
    #   - :modulo:: (default +false+) calculates remainder only, in this instance +hl+|+hl'+ will be 0
    #               when division is finished.
    #   - :quick8:: (default +true+) checks if divisor fits in 8 bits and in this instance
    #               uses different, optimized code.
    def divmod32_16(x=ixl, opts={})
      raise ArgumentError unless [ixh, ixl, iyh, iyl].include?(x)
      flags = {
        :check0 => true,
        :check1 => true,
        :modulo => false,
        :quick8 => true
      }.merge opts
      ns do |eoc|
        if flags[:check0] or flags[:check1] or flags[:quick8]
                xor a
                ora d
                jp  NZ, div32strt
          if flags[:quick8]
                divmod32_8 e, opts
                ld  b, 0
                ld  c, a
                jp  eoc
          elsif flags[:check0] or flags[:check1]
                ld  a, e
                cp  1
                jr  C, eoc if flags[:check0] # division by 0
            if flags[:check1]
                  jp  NZ, div32strt # division by m > 1
                  ld  bc, 0
                  jp  eoc           # division by 1
            end
          end
        end
        div32strt xor a            # a = 0 hi remainder
                  ld  c, a         # c = 0 lo remainder
                  ld  b, 16
        loopfit1  add hl, hl       # carry <- hl <- 0
                  rl  c            # carry <- c <- carry
                  adc a            # carry <- a <- carry
                  cp  d            # a - d
                  jr  NC, fitshi1  # a >= d
                  djnz loopfit1    # loop
                  jp  divlo16
        fitshi1   ld  x, a
                  ld  a, c
                  jr  NZ, fitslo1  # a > d, ignore e
                  cp  e            # a == d: c - e
                  jr  NC, fitslo1  # a >= e
                  ld  a, x 
                  djnz loopfit1    # loop
                  jp  divlo16
        fitslo1   sub e            # a = c - e
                  ld  c, a         # c = c - e
                  ld  a, x
                  sbc d            # a -= d
        unless flags[:modulo]
                  inc l            # hl <- 1 (quotient)
        end
                  djnz loopfit1    # loop

        divlo16   push de
                  ld  x, c
                  exx              # hl' <-> hl
                  pop de           # de' = de
                  ld  c, x         # c' = c

                  ld  b, 16
        loopfit2  add hl, hl       # carry <- hl' <- 0
                  rl  c            # carry <- c' <- carry
                  adc a            # carry <- a <- carry
                  cp  d            # a - d'
                  jr  NC, fitshi2  # a >= d'
                  djnz loopfit2    # loop
                  ccf if flags[:check0]
                  jp  over
        fitshi2   ld  x, a
                  ld  a, c
                  jr  NZ, fitslo2  # a > d, ignore e
                  cp  e            # a == d: c - e
                  jr  NC, fitslo2  # a >= e
                  ld  a, x
                  djnz loopfit2    # loop
                  ccf if flags[:check0]
                  jp  over
        fitslo2   sub e            # a = c' - e'
                  ld  c, a         # c' = c' - e'
                  ld  a, x
                  sbc d            # a -= d'
        unless flags[:modulo]
                  inc l            # hl' <- 1 (quotient)
        end
                  djnz loopfit2    # loop
        over      ld  x, c
                  exx
                  ld  b, a         # bc = remainder
                  ld  c, x
      end
    end
    ##
    # Convert a 8-bit unsigned integer to bcd
    # allows for arbitrary integer size conversion.
    #
    # Used by +utobcd+.
    #
    # Uses: +a+, +b+, +r+, +t+, +hl+
    def utobcd_step(bufend, r, buflen=1, t=c, r_in_a=false)
      raise ArgumentError unless r != a and r != b and t != a and t != b and t != r and
              (!buflen.is_a?(Register) || buflen == t)
      ns do
            ld  a, r unless r_in_a
            ld  t, buflen unless buflen == t
            scf
            rla
            ld  r, a
    buffmul ld  hl, bufend-1 # multiply buffer[] * 2 + carry using BCD
            ld  b, t
    nextadd ld  a, [hl]
            adc a
            daa
            ld  [hl], a
            dec hl
            djnz nextadd
            jp  NC, nbufext  # no carry
            inc t            # extend buffer on carry
            inc b
            ld  [hl], b      # put 1 in new place
    nbufext sla r
            jp  NZ, buffmul
      end
    end
    ##
    # Converts arbitrary size unsigned integer (LSB) to bcd
    #
    # Uses: +a+, +bc+, +hl+, +b'+, +r+, +rr'+
    #
    # After conversion +c+ contains number of bytes used to store bcd number.
    # Subtract it from +bufend+ to get the first byte.
    #
    # Place integer address in +input+ of +size+ bytes and +bufend+ should point to the address
    # immediately following buffer end. Provide large enough buffer.
    #
    # * +bufend+:: must be an address (or a label)
    # * +input+:: may be an immediate address (or a label) or the same as +rr+, in this instance
    #   it is expected that +rr'+ will already contain +input+ address.
    # * +size+:: may be an integer or one of 8-bit registers except +a+.
    # * +r+:: temporary register (d or e)
    # * +rr'+:: temporary register (de or hl)
    def utobcd(bufend, input, size=4, r=d, rr=de)
      raise ArgumentError unless (!input.is_a?(Register) or input == rr) and
                          (size.is_a?(Integer) or [b, c, d, e, h, l].include?(size)) and
              [de, hl].include?(rr) and [d, e].include?(r)
      ns do
            xor a
            ld  [bufend - 1], a
            ld  a, size
            ld  c, 1
            exx
            ld  b, a
            if !input.is_a?(Register) and !size.is_a?(Register)
              ld  rr, input + size
            else
              ld  rr, input unless input == rr
              adda_to *rr.split
            end
      loopi dec  rr
            ld  a, [rr]
            exx
            utobcd_step(outbufend, r, c, c, true)
            exx
            djnz loopi
            exx
      end
    end
    ##
    # Reads each bcd digit as +a+ destroying content of a buffer in the process.
    #
    # Uses: +a+, +hl+, +b+.
    #
    # On first digit carry is 1 on subsequent is 0 before +block+.
    #
    # Block must not alter +hl+ or +b+.
    def bcdtoa(buffer, size, &block)
      raise ArgumentError unless (!buffer.is_a?(Register) or buffer == hl)
      ns do
            ld  b, size unless size == b
            ld  hl, buffer unless buffer == hl
            xor a
            scf
      loopa rld
            ns(&block)
            xor a
            rld
            ns(&block)
            inc hl
            xor a
            djnz loopa
      end
    end
  end
  include Z80
end
