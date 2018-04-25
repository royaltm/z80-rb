# -*- coding: BINARY -*-
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
                call  print_cstr
    text_p      bytes text
                db    0
  end

  macro :load_i32_hl_bc do |_, n|
                ld  hl, (n >> 16) & 0x0ffff
                ld  bc, n & 0x0ffff
  end

  macro :store_int do |_, target, bitsize, value|
    bytesize = bitsize >> 3
                memcpy target, source, bytesize
                jp  (hl)
    source      int bitsize, value
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

  macro :assert_equals do |eoc, left, right, bytesize|
    raise ArgumentError unless left.is_a?(Integer) and (1..255).include?(bytesize)
        ld    hl, right
        call  assert_cmp
        db  bytesize
        int bytesize*8, left
  end

  macro :multiply32_test do |_, x, y|
    raise ArgumentError unless x.is_a?(Integer) and (0..65535).include?(x) and
                               y.is_a?(Integer) and (0..65535).include?(y)
        print_text "#{x} * #{y} = "
        ld  hl, x
        ld  bc, y
        call multiply32
        assert_equals x*y, somebigint, 4
        call print_num32
  end

  macro :div_euc8_test do |_, x, y|
    raise ArgumentError unless x.is_a?(Integer) and (0..65535).include?(x) and
                               y.is_a?(Integer) and (0..255).include?(y)
        print_text "#{x} / #{y} = "
        ld  hl, x
        ld  c, y
        call div_euc8
    if y.zero?
        jp   NC, error
        print_text "division by 0!\r  "
    else
        jp   C, error
        assert_equals x/y, somebigint, 2
        assert_equals x%y, sombigint1, 1
    end
        call print_16_8
  end

  macro :div_euc16_test do |_, x, y|
    raise ArgumentError unless x.is_a?(Integer) and (0..65535).include?(x) and
                               y.is_a?(Integer) and (0..65535).include?(y)
        print_text "#{x} / #{y} = "
        ld  hl, x
        ld  de, y
        call div_euc16
    if y.zero?
        jp   NC, error
        print_text "division by 0!\r  "
    else
        jp   C, error
        assert_equals x/y, somebigint, 2
        assert_equals x%y, sombigint1, 2
    end
        call print_16_16
  end

  macro :div_euc32_test do |_, x, y|
    raise ArgumentError unless x.is_a?(Integer) and (0..0xffffffff).include?(x) and
                               y.is_a?(Integer) and (0..65535).include?(y)
        print_text "#{x} / #{y} = "
        load_i32_hl_bc x
        ld  de, y
        call div_euc32
    if y.zero?
        jp   NC, error
        print_text "division by 0!\r  "
    else
        jp   C, error
        assert_equals x/y, somebigint, 4
        assert_equals x%y, sombigint1, 2
    end
        call print_32_16
  end


  start         exx
                push hl
                ld  a, 2
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

                print_text "\rmultiply 16bit*16bit -> 32bit\r"
                multiply32_test 455, 36873
                multiply32_test 36873, 455
                multiply32_test 75, 65521
                multiply32_test 65521, 75
                multiply32_test 1, 65535
                multiply32_test 65535, 1
                multiply32_test 2, 32768
                multiply32_test 32768, 2
                multiply32_test 0, 65535
                multiply32_test 65535, 0
                multiply32_test 65535, 65535

                print_text "\r8bit divisor:\r"
                div_euc8_test 65535, 255
                div_euc8_test 0, 255
                div_euc8_test 65521, 121
                div_euc8_test 65521, 1
                div_euc8_test 254, 255
                div_euc8_test 65535, 0

                print_text "\r16bit divisor:\r"
                div_euc16_test 65535, 255
                div_euc16_test 65535, 65535
                div_euc16_test 0, 255
                div_euc16_test 0, 65535
                div_euc16_test 65521, 121
                div_euc16_test 65521, 16807
                div_euc16_test 65534, 65535
                div_euc16_test 65521, 1
                div_euc16_test 65535, 0

                print_text "\r32bit dividend:\r"
                div_euc32_test 4294967295, 65535
                div_euc32_test 0, 65535
                div_euc32_test 2147483647, 16807
                div_euc32_test 2147483647, 255
                div_euc32_test 2147483647, 1
                div_euc32_test 4294967295, 0

                pop  hl
                exx
                ret

  #
  # Subroutines
  #

  error         rst 0x08
                db  20          # Error Report: L BREAK into program

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
                ld de, somebigint
  prnum         utobcd bcdbufend, de, c
                ld  hl, bcdbufend
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

  multiply32    mul16_32
                ld  [somebigint[1]], hl
                exx
                ld  [somebigint], hl
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

  ns :print_cstr do
                pop  hl
                jr   skip1
    loop_pr     rst  0x10
    skip1       ld   a, [hl]
                inc  hl
                ora  a
                jr   NZ, loop_pr
                jp   (hl)
  end

  ns :assert_cmp do
                pop  de
                ld   a, [de]
                inc  de
                ld   b, a
                ld   c, a
                push hl
                push de
    aloop       ld   a, [de]
                cp   [hl]
                jr   NZ, assert_fail
                inc  de
                inc  hl
                djnz aloop
                ex   de, hl
                pop  de
                pop  de
                jp   (hl)
    assert_fail print_text "\rERROR: "
                ld    a,  32
                ld    [terminator], a
                pop   de
                push  bc
                call  prnum
                print_text "!= "
                pop   bc
                pop   de
                call  prnum
                jp    error
  end


  somebigint    words 2
  sombigint1    words 2
  sombigint2    words 4
  somebigbytes  union somebigint, 1

                bytes 20
  bcdbufend     label
end

math = Program.new 0x8000
puts math.debug
math.save_tap('math')
