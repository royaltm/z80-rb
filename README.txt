=ruby-Z80

==A Z80 assembler powered by ruby.

Author::    Rafał Michalski  (mailto:royaltm75@gmail.com)

  class Math
    module Macros
      # here your macros will be populated
    end
    include Z80

    macro :multi8c_m do |eoc, th, tl|  # multiply hl by a; stops on CARRY out
            ld  tl, l
            ld  th, h
            ld  hl,0
      loop1 srl a
            jr  NC, noadd
            add hl, th|tl
            jr  C, eoc
      noadd jr  Z, eoc
            sla tl
            rl  th
            jp  NC, loop1
    end
            ret

    ns :multi16 do       # multiply hl by bc and stores result in hl de
            ld  a, c
            ex af,af
            xor a
            push af
            ld  e, a
            ld  d, a
            ld  a, c
            ld  c, l
            ld  b, h
            ld  h, d
            ld  l, e
            scf
            adc a
      loop1 jr  NC, noadd1
            ex  (sp), hl
            add hl, de
            ex  (sp), hl
            adc hl, bc
      nadd1 srl b
            rr  c
            rr  d
            add a
            jp  NZ, loop1
            ex  af, af
            add a
      loop2 jr  NC, nadd2
            ex  (sp), hl
            add hl, de
            ex  (sp), hl
            adc hl, bc
      nadd2 srl c
            rr  d
            rr  e
            add a
            jp  NZ, loop2
            pop de
            ret
    end
  end
