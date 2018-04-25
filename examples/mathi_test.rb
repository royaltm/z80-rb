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
  pr_string     addr 0x203c

  macro :print_char do |_, char|
                ld  a, char.ord
                rst 0x10
  end
  macro :print_text do |eoc, text|
    raise ArgumentError unless text.is_a?(String)
                ld   hl, eoc
                push hl
                ld   de, text_p
                ld   bc, text.bytesize
                jp   pr_string
    text_p      bytes text
  end

  macro :load_i32_hl_bc do |_, n|
                ld  hl, (n >> 16) & 0x0ffff
                ld  bc, n & 0x0ffff
  end

  macro :store_int do |_, target, bitsize, n|
    raise ArgumentError unless [32,64,128].include?(bitsize)
    bytesize = bitsize >> 3
                ld  ix, target
    (0..bytesize).each do |i|
                ld  [ix + i], n & 0x0ff
                n >>= 8
    end
  end

  class << self
    def store_i32(target, n)
      store_int target, 32, n
    end

    def store_i64(target, n)
      store_int target, 64, n
    end

    def store_i128(target, n)
      store_int target, 128, n
    end
  end

  start         ld  a, 2
                call chanopen
                print_text "PRINT 0 (32, 64, 128): "
                clrmem8 somebigint, 16, 0
                ld  c, 4
                ld  a, ','.ord
                call prnum_term
                print_char ' '
                ld  c, 8
                call prnum_last_term
                print_char ' '
                ld  c, 16
                ld  a, 13
                call prnum_term

                print_text "PRINT abs(-1) (32, 64, 128):\r"
                clrmem8 somebigint, 16, 255
                ld  c, 4
                ld  a, ','.ord
                call prnum_term
                print_char ' '
                ld  c, 8
                call prnum_last_term
                print_char ' '
                ld  c, 16
                ld  a, 13
                call prnum_term

                print_text "1234567890 as number: "
                store_i32 somebigint, 1234567890
                call print_num32
                print_text "1000000000 as number: "
                store_i32 somebigint, 1000000000
                call print_num32
                print_text "1000, 100, 10 as numbers:\r"
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
                store_i128 somebigint, 100000000000000000000000000000000000000
                ld  c, 16
                call print_num

                print_text "\rmultiply 16bit*16bit -> 32bit\r455 * 36873 = "
                ld  hl, 455
                ld  bc, 36873
                call multiply32
                call print_num32
                print_text "36873 * 455 = "
                ld  hl, 36873
                ld  bc, 455
                call multiply32
                call print_num32
                print_text "75 * 65521 = "
                ld  hl, 75
                ld  bc, 65521
                call multiply32
                call print_num32
                print_text "65521 * 75 = "
                ld  hl, 65521
                ld  bc, 75
                call multiply32
                call print_num32
                print_text "1 * 65535 = "
                ld  hl, 1
                ld  bc, 65535
                call multiply32
                call print_num32
                print_text "65535 * 1 = "
                ld  hl, 65535
                ld  bc, 1
                call multiply32
                call print_num32
                print_text "2 * 32768 = "
                ld  hl, 2
                ld  bc, 32768
                call multiply32
                call print_num32
                print_text "32768 * 2 = "
                ld  hl, 32768
                ld  bc, 2
                call multiply32
                call print_num32
                print_text "0 * 65535 = "
                ld  hl, 0
                ld  bc, 65535
                call multiply32
                call print_num32
                print_text "65535 * 0 = "
                ld  hl, 65535
                ld  bc, 0
                call multiply32
                call print_num32
                print_text "65535 * 65535 = "
                ld  hl, 65535
                ld  bc, 65535
                call multiply32
                call print_num32

                print_text "\r8bit divisor\r65535 / 255 = "
                ld  hl, 65535
                ld  c, 255
                call div_euc8
                jp   C, error
                call print_16_8
                print_text "0 / 255 = "
                ld  hl, 0
                ld  c, 255
                call div_euc8
                jp   C, error
                call print_16_8
                print_text "65521 / 121 = "
                ld  hl, 65521
                ld  c, 121
                call div_euc8
                jp   C, error
                call print_16_8
                print_text "65521 / 1 = "
                ld  hl, 65521
                ld  c, 1
                call div_euc8
                jp   C, error
                call print_16_8
                print_text "254 / 255 = "
                ld  hl, 254
                ld  c, 255
                call div_euc8
                jp   C, error
                call print_16_8
                print_text "65535 / 0 = "
                ld  hl, 65535
                ld  c, 0
                call div_euc8
                jp   NC, error
                print_text "division by 0!\r  "
                call print_16_8


                print_text "\r16bit divisor:\r65535 / 255 = "
                ld  hl, 65535
                ld  de, 255
                call div_euc16
                jp   C, error
                call print_16_16
                print_text "65535 / 65535 = "
                ld  hl, 65535
                ld  de, 65535
                call div_euc16
                jp   C, error
                call print_16_16
                print_text "0 / 255 = "
                ld  hl, 0
                ld  de, 255
                call div_euc16
                jp   C, error
                call print_16_16
                print_text "0 / 65535 = "
                ld  hl, 0
                ld  de, 65535
                call div_euc16
                jp   C, error
                call print_16_16
                print_text "65521 / 121 = "
                ld  hl, 65521
                ld  de, 121
                call div_euc16
                jp   C, error
                call print_16_16
                print_text "65521 / 16807 = "
                ld  hl, 65521
                ld  de, 16807
                call div_euc16
                jp   C, error
                call print_16_16
                print_text "65534 / 65535 = "
                ld  hl, 65534
                ld  de, 65535
                call div_euc16
                jp   C, error
                call print_16_16
                print_text "65521 / 1 = "
                ld  hl, 65521
                ld  de, 1
                call div_euc16
                jp   C, error
                call print_16_16
                print_text "65535 / 0 = "
                ld  hl, 65535
                ld  de, 0
                call div_euc16
                jp   NC, error
                print_text "division by 0!\r  "
                call print_16_16

                print_text "\r32bit dividend:\r4294967295 / 65535 = "
                load_i32_hl_bc 0xffffffff
                ld  de, 0xffff
                call div_euc32
                jp   C, error
                call print_32_16
                print_text "0 / 65535 = "
                load_i32_hl_bc 0
                ld  de, 0xffff
                call div_euc32
                jp   C, error
                call print_32_16
                print_text "2147483647 / 16807 = "
                load_i32_hl_bc 2147483647
                ld  de, 16807
                call div_euc32
                jp   C, error
                call print_32_16
                print_text "2147483647 / 255 = "
                load_i32_hl_bc 2147483647
                ld  de, 255
                call div_euc32
                jp   C, error
                call print_32_16
                print_text "2147483647 / 1 = "
                load_i32_hl_bc 2147483647
                ld  de, 1
                call div_euc32
                jp   C, error
                call print_32_16
                print_text "4294967295 / 0 = "
                load_i32_hl_bc 0xffffffff
                ld  de, 0
                call div_euc32
                jp   NC, error
                print_text "division by 0!\r  "
                call print_32_16

                ret

  error         rst 0x08
                db  25          # Error Report: Q Parameter error

  print_16_8    ld  c, 2
                ld  a, ' '.ord
                call prnum_term
                ld  a, [sombigint1]
                ld  [somebigint], a
                ld  c, 1
                jp  print_num

  print_16_16   ld  c, 2
                ld  a, ' '.ord
                call prnum_term
                ld  hl, [sombigint1]
                ld  [somebigint], hl
                ld  c, 2
                jp  print_num

  print_32_16   ld  c, 4
                ld  a, ' '.ord
                call prnum_term
                ld  hl, [sombigint1]
                ld  [somebigint], hl
                ld  c, 2
                jp  print_num

  print_num32   ld  c, 4
  print_num     ld  a, 13 # terminator character
  prnum_term    ld  [terminator], a
  prnum_last_term   label
                utobcd outbufend, somebigint, c
                ld  hl, outbufend
                sub_from c, h, l
                bcdtoa hl, c do |eoc|
                  jr  NC, pr1  # skip check if not first
                  jr  Z, eoc   # skip if 0
            pr1   add ?0.ord
                  rst 0x10
                end
                ld  a, [terminator]
                rst 0x10
                ret
  terminator    db  13

  multiply32    exx
                push hl
                exx
                mul16_32
                ld  [somebigint[1]], hl
                exx
                ld  [somebigint], hl
                pop hl
                exx
                ret

  div_euc8      divmod8
                ld  [somebigint], hl
                ld  [sombigint1], a
                ret

  div_euc16     divmod16
                ld  [somebigint], hl
                ld  [sombigint1], bc
                ret

  div_euc32     push bc # lo16
                exx
                ex  [sp], hl  # save hl', load lo16
                exx
                divmod32_16
                ld  [sombigint1], bc
                ld  [somebigint[1]], hl
                exx
                ld  [somebigint], hl
                pop hl
                exx
                ret

  somebigint    words 2
  sombigint1    words 2
  sombigint2    words 4
  somebigbytes  union somebigint, 1
                bytes 10
  outbufend     label
end

math = Program.new 0x8000
puts math.debug
math.save_tap('math')
