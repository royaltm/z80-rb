# -*- coding: BINARY -*-
here = File.expand_path('../lib', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'zxlib/basic'
require 'zxlib/sys'

class MyZXCrc8
  module Macros
    def crc8(init, poly)  # CRC-8 *HL over DE bytes
                          # result to A
      raise ArgumentError if [h,l,d,e].include?(poly) ||
                             [h,l,d,e].include?(init)
      ns do
        if register?(poly) || direct_address?(poly)
                ld  c, poly unless poly == c
        elsif init == a
          raise ArgumentError
        else 
                ld  a, poly unless poly == a
                ld  c, a
        end
                ld  a, init unless init == a
        loop1   ex  af, af
                ld  a, d
                xor e
                jr  Z, restore

                ex  af, af
                xor [hl]  # next char
                inc hl
                dec de
                ld  b, 8
        loop8   add a, a  # shift CRC register
                jr  NC, nover
                xor c     # ^ gen polynomial
        nover   djnz loop8
                jp  loop1
        restore ex  af, af
      end
    end
  end

  include Z80

  export calc
  # calc   crc8(0x00, 0x07) # CRC-8/SMBUS check=244
  # calc   crc8(0x00, 0x9b) # CRC-8/LTE check=234
  calc   crc8(0xfd, 0x1d) # CRC-8/I-CODE check=126
         ld   b, 0
         ld   c, a
         ret
end

class Program
  include Z80
  include Z80::TAP

  # we'll import ZXLib::Sys library macros and labels but no code
  import        ZXLib::Sys, macros: true, labels: true, code: false

  # direct USR call
  start         ld   hl, string
                ld   de, +string
                jp   crc_8.calc

  # call via DEF FN
  call_var      find_def_fn_args(1, subroutine:false, cf_on_direct:true)
                jr   C, start
                report_error_unless Z, "Q Parameter error"
                read_arg_string b, c, d, e   # bc: *a$ de: LEN a$
                ld16 hl, bc
                jp   crc_8.calc

                org  0x0030
  string        data "123456789"

  import MyZXCrc8, :crc_8

end

include ZXLib

calc = Program.new 0x8000

puts calc.debug
program = Basic.parse_source <<-END
   1 DEF FN c(a$)=USR #{calc[:call_var]}
  10 CLEAR #{calc.org-1}
  20 LOAD ""CODE
  30 PRINT "CRC-8 of ""123456789"" is ";USR #{calc.org}
  40 INPUT "Text: ";a$
  50 PRINT """";a$;""""
  60 PRINT "CRC-8: ";FN c(a$)
 100 GO TO 40
END
program.start = 10
program.save_tap 'examples/crc'
calc.save_tap 'examples/crc', append: true, name: 'CRC-8'
puts "="*32
puts program
puts "="*32
Z80::TAP.parse_file('examples/crc.tap') do |hb|
    puts hb.to_s
end
