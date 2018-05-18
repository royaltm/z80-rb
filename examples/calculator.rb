require 'z80'
class ZXMath
  module Macros
    def mul8(eh, el, th, tl)  # performs hl * a using (a, th, tl)
                              # stops on CARRY out
                              # result to (eh|el)
      raise ArgumentError unless th|tl != hl
      ns do |eoc|
              ld  tl, l
              ld  th, h
              ld  hl,0
        loop1 srl a
              jr  NC, noadd
              add hl, th|tl
              jr  C, eoc
        noadd jr  Z, ok
              sla tl
              rl  th
              jp  NC, loop1
        ok    label
              unless  eh == h and el == l
                ld  el, l
                ld  eh, h
              end
      end
    end
  end
  include Z80

  export mul
  mul   mul8(b, c, d, e)
      ret NC
      rst 0x08             # ERROR-1
      data  1, [0x0A]      # Error Report: Integer out of range
end

class Program
  include Z80
  include Z80::TAP

                ld   hl, [multiplicand]
                ld   a,  [multiplicator]
                jp   math.mul

                org 0x0020
  multiplicand  words 1
  multiplicator bytes 1

  import ZXMath, :math

end

calc = Program.new 0x8000

calc.save_tap('calculator', :append => true)

puts calc.debug

