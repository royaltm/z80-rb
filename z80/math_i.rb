class Z80MathInt
  module Macros
    # adds a to h,l
    # uses: +a+, +h+, +l+
    # +h+:: register input accumulator hi
    # +l+:: register input accumulator lo
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
    # subs r from h,l
    # uses: +a+, +r+, +h+, +l+
    # +r+:: register subtractor must not be +a+
    # +h+:: register input accumulator hi
    # +l+:: register input accumulator lo
    def sub_from(r, h, l) # 24
        if h == l or [r,h,l].include?(a)
            raise ArgumentError, "adda_to invalid arguments!"
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
    # performs multiplication 16bit mh, ml * 8bit m using (m, hl, th|tl) -> hl
    # breaks on carry out with CF=1
    # (optionally) adds result to hl
    # uses: +hl+, +m+, +mh+, +ml+, +th+, +tl+
    # +mh+::    register/value input multiplicant hi
    # +ml+::    register/value input multiplicant lo
    # +m+::     register input multiplicator
    # +th+::    register temporary (d or b)
    # +tl+::    register temporary (e or c)
    # +clrhl+:: should hl be set (true) or accumulated (false)
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
    # performs multiplication 16bit mh, ml * 8bit m using (m, hl, th|tl) -> hl
    # (optionally) adds result to hl
    # (optionally) multiplies multiplicator * 2
    # uses: +hl+, +m+, +mh+, +ml+, +th+, +tl+
    # +mh+::     register/value input multiplicant hi
    # +ml+::     register/value input multiplicant lo
    # +m+::      register input multiplicator
    # +th+::     register temporary (d or b)
    # +tl+::     register temporary (e or c)
    # +clrhl+::  should hl be set (true) or accumulated (false)
    # +double+:: should th|tl * 2 (true) or not (false)
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
  end
  include Z80
end
