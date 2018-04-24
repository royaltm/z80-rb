here = File.expand_path('..', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'z80/math_i'
require 'z80/stdlib'

class Program
  include Z80
  include Z80::TAP

  macro_import  Z80MathInt
  macro_import  Z80Lib

  chanopen      addr 0x1601

  macro :store_i32 do |_, target, n|
                ld  ix, target
                ld  [ix + 0], n & 0x0ff
                ld  [ix + 1], (n >> 8) & 0x0ff
                ld  [ix + 2], (n >> 16) & 0x0ff
                ld  [ix + 3], (n >> 24) & 0x0ff
  end

  macro :store_i64 do |_, target, n|
                ld  ix, target
                ld  [ix + 0], n & 0x0ff
                ld  [ix + 1], (n >> 8) & 0x0ff
                ld  [ix + 2], (n >> 16) & 0x0ff
                ld  [ix + 3], (n >> 24) & 0x0ff
                ld  [ix + 4], (n >> 32) & 0x0ff
                ld  [ix + 5], (n >> 40) & 0x0ff
                ld  [ix + 6], (n >> 48) & 0x0ff
                ld  [ix + 7], (n >> 56) & 0x0ff
  end

  start         ld  a, 2
                call chanopen
                call print_num32
                clrmem8 somebigint, 4
                call print_num32
                clrmem8 somebigint, 4, 255
                call print_num32
                store_i32 somebigint, 1000000000
                call print_num32
                store_i32 somebigint, 1000
                call print_num32
                store_i32 somebigint, 100
                call print_num32
                store_i32 somebigint, 10
                call print_num32
                store_i32 somebigint, 1
                call print_num32
                clrmem8 somebigint, 8, 255
                ld  c, 8
                call print_num
                store_i64 somebigint, 10000000000000000000
                ld  c, 8
                call print_num
                ret

  print_num32   ld  c, 4
  print_num     label # size in c
                utobcd outbufend, somebigint, c
                ld  hl, outbufend
                sub_from c, h, l
                bcdtoa hl, c do |eoc|
                  jr  NC, pr1  # skip check if not first
                  jr  Z, eoc   # skip if 0
            pr1   add ?0.ord
                  rst 0x10
                end
                ld  a, 13
                rst 0x10
                ret

  somebigint    dw  1234567890 & 0x0ffff, (1234567890 & 0xffff0000) >> 16
                words 2
                bytes 10
  outbufend     label
end

math = Program.new 0x8000
puts math.debug
math.save_tap('math')
