# -*- coding: BINARY -*-
here = File.expand_path('../lib', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'zxlib/basic'

class MyZXMath
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

  import MyZXMath, :math

end

include ZXLib

calc = Program.new 0x8000

puts calc.debug
program = Basic.parse_source <<-END
  10 CLEAR 32767
  20 LOAD ""CODE
  30 INPUT "Multiplicand: ",x
  40 INPUT "Multiplicator: ",y
  50 POKE 32800,x-INT (x/256)*256
  60 POKE 32801,INT (x/256)
  70 POKE 32802,y
  80 PRINT "x: ", x, "y: ", y
  90 PRINT USR 32768
 100 GO TO 30
END
program.start = 10
program.save_tap 'examples/calculator'
calc.save_tap('examples/calculator', append: true)
puts "="*32
puts program
puts "="*32
Z80::TAP.parse_file('examples/calculator.tap') do |hb|
    puts hb.to_s
end
