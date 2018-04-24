class Z80MathInt
  module Macros
    ##
    # Adds +a+ to +h+|+l+.
    #
    # Uses: +a+, +h+, +l+
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
    # Uses: +a+, +r+, +h+, +l+, preserves +r+
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
    # Performs multiplication 16bit mh, ml * 8bit m using (m, hl, th|tl) -> hl,
    # breaks on carry out with CF=1, (optionally) adds result to hl.
    # 
    # Uses: +hl+, +m+, +mh+, +ml+, +th+, +tl+
    #
    # * +mh+::    register/value input multiplicant hi
    # * +ml+::    register/value input multiplicant lo
    # * +m+::     register input multiplicator
    # * +th+::    temporary register (d or b)
    # * +tl+::    temporary register (e or c)
    # * +clrhl+:: should hl be set (true) or accumulated (false)
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
    # Performs multiplication 16bit mh, ml * 8bit m using (m, hl, th|tl) -> hl,
    # (optionally) adds result to hl,
    # (optionally) multiplies multiplicator * 2.
    #
    # Uses: +hl+, +m+, +mh+, +ml+, +th+, +tl+
    #
    # * +mh+::     register/value input multiplicant hi
    # * +ml+::     register/value input multiplicant lo
    # * +m+::      register input multiplicator
    # * +th+::     register temporary (d or b)
    # * +tl+::     register temporary (e or c)
    # * +clrhl+::  +true+ if should +hl+ be set (true) or accumulated (false)
    # * +double+:: +true+ if should double (th|tl * 2)
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
    # multiply hl by mh|ml (b|c or d|e) and stores result in hl rr (bc | de)
    # *UNTESTED*
    def multi16_32(mh, ml, rr)
      ns do
            th = mh == b ? d : b
            tl = ml == c ? e : c
            ld  a, ml
            ex af,af
            xor a
            push af
            ld  tl, a
            ld  th, a
            ld  a, mh
            ld  ml, l
            ld  mh, h
            ld  h, th
            ld  l, tl
            scf
            adc a
      loop1 jr  NC, noadd1
            ex  (sp), hl
            add hl, th|tl
            ex  (sp), hl
            adc hl, mh|ml
      nadd1 srl mh
            rr  ml
            rr  th
            add a
            jp  NZ, loop1
            ex  af, af
            add a
      loop2 jr  NC, nadd2
            ex  (sp), hl
            add hl, th|tl
            ex  (sp), hl
            adc hl, mh|ml
      nadd2 srl ml
            rr  th
            rr  tl
            add a
            jp  NZ, loop2
            pop rr
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
    # Converts unsigned arbitrary size integer (LSB) to bcd
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
    # Reads each bcd digit as +a+ destroying content of a buffer
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
