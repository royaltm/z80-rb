# -*- coding: BINARY -*-
here = File.expand_path('..', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'z80/math_i'
require 'z80/stdlib'

class Program
  include Z80
  include Z80::TAP

  ###########
  # Exports #
  ###########

  export tests
  export rnd_seed_fn
  export test_math
  export test_rnd1
  export test_rnd2

  ###########
  # Imports #
  ###########

  macro_import  Z80MathInt
  macro_import  Z80Lib

  ########
  # Vars #
  ########

  # ROM procedures
  chanopen      addr 0x1601
  pr_string     addr 0x203C
  cl_all        addr 0x0DAF
  cl_line       addr 0x0E44
  call_jump     addr 0x162C

  # VARS
  frames        addr 0x5C78
  rnd_seed      addr 0x5C76
  bordcr        addr 0x5C48
  udg_default   addr 0xFF58

  # HW
  scr_p         addr 0x4000
  scrlen        6144
  attr_p        addr scr_p + scrlen
  attrlen       6912 - 6144

  ##########
  # Macros #
  ##########

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

  # ZF=0 if any key is being pressed
  macro :key_pressed? do
                xor  a
                inp  a, (254)
                cpl
                anda 0x1F
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

  macro :assert_equals_rr do |_, left, rr|
    raise ArgumentError unless [bc, de, hl, ix, iy].include?(rr)
        ld  [somebigint], rr
        assert_equals left, somebigint, 2
  end

  macro :assert_equals do |_, left, right, bytesize|
    raise ArgumentError unless left.is_a?(Integer) and (1..255).include?(bytesize)
        ld    hl, right
        call  assert_cmp
        db  bytesize
        int bytesize*8, left
  end

  macro :multiply32_test do |_, x, y|
    raise ArgumentError unless x.is_a?(Integer) and (0..65535).include?(x) and
                               y.is_a?(Integer) and (0..65535).include?(y)
        print_text "#{x}*#{y} = "
        ld  hl, x
        ld  bc, y
        call multiply32
        assert_equals x*y, somebigint, 4
        call print_num32
  end

  macro :div_euc8_test do |_, x, y|
    raise ArgumentError unless x.is_a?(Integer) and (0..65535).include?(x) and
                               y.is_a?(Integer) and (0..255).include?(y)
        print_text "#{x}/#{y} = "
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
        print_text "#{x}/#{y} = "
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
        print_text "#{x}/#{y} = "
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

  macro :benchmark do |_, title, &block|
                print_text "#{title} ..."
                halt
                memcpy somebigint, frames, 3
                ns(&block)
                di
                call bench_results
  end

  #########
  # Tests #
  #########

  tests         jr tests_ext
  rnd_seed_fn   jp next_rnd_fn
  test_math     jp math_test_ext
  test_rnd1     jp rnd_test1_ext
  test_rnd2     jp rnd_test2_ext

  tests_ext     call call_me_safe
                call math_test
                call rnd_test1
                call rnd_test2
                ret

  math_test_ext call call_me_safe
  ns :math_test do
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

                [0, 872, 873, 20097, 34952, 40195, 65535].each_with_index do |seed, i|
                  print_text "#{i.zero? ? "\r" : ''}RND(#{seed}) = "
                  ld   hl, seed
                  call next_rnd_hl
                  assert_equals_rr (seed+1)*75%65537-1, hl
                  call print_num16
                end

                benchmark "\rBench RND(0..65535)" do
                        ld   hl, 0
                  bloop push hl
                        rnd
                        pop  hl
                        inc  hl
                        ld   a, l
                        ora  h
                        jp   NZ, bloop
                end

                print_text "\rPress any key..."
                call    wait_key
                ret
  end

  rnd_test1_ext call call_me_safe
  ns :rnd_test1 do
                ld   a, 0b01000111
                call clear_screen
      key_loop  halt
                key_pressed?
                jr   NZ, key_loop
                call seed_entropy
                exx
                ld   [sp_save], sp
                di
      loop1     ld   sp, attr_p
                ld   bc, 12
      loop2     exx
                rnd
                push hl
                exx
                djnz loop2
                key_pressed?
                jr   NZ, brloop
                dec  c
                jr   NZ, loop2
                exx
                ld   a, r
                xor  h
                rrca
                xor  l
                rrca
                ld   h, a
                exx
                jr   loop1
      brloop    ld   sp, [sp_save]
                ei
                ret
  end

  rnd_test2_ext call call_me_safe
  ns :rnd_test2 do
                halt
                ld   a, 0b00111000
                call clear_screen
                halt
                call seed_entropy
                ld   [sp_save], sp
                di
                ld   sp, udg_default
                ld   bc, 12*4 # 4 screens
      loop1     exx
                call next_rnd
                push hl
                exx
                djnz loop1
                dec  c
                jr   NZ, loop1
                ld   hl, 0
                add  hl, sp
                ld   sp, [sp_save]
                ei
      key_loop  halt
                key_pressed?
                jr   NZ, key_loop
      loop2     push hl
                ld   a, 4
      loop3     halt
                ld   de, scr_p
                ld   bc, 6144
                ldir
                dec  a
                jr   NZ, loop3
                key_pressed?
                jr   NZ, cleanup
                pop  hl
                jr   loop2
      cleanup   pop  hl
                ret
  end


  ###############
  # Subroutines #
  ###############

  # return to BASIC via error ROM routine
  error         rst 0x08
                db  20          # Error Report: L BREAK into program

  # gets return address from stack saves FP-CALCULATOR STACK pointer
  # then calls return address, restores FP-CALCULATOR STACK pointer
  # then clears screen and returns to the caller of our caller
  call_me_safe  pop  hl
                exx
                push hl # save STACK pointer
                exx
                call call_jump
                pop  hl # restore STACK pointer
                exx
                call cleanup_scr
                ret

  # clear screen using CL-ALL and reset border
  cleanup_scr   label
                call cl_all
                ld   a, [bordcr]
                call set_border_cr
                ret

  # waits for any key being pressed
  wait_key      halt
                key_pressed?
                jr Z, wait_key
                ret

  # clears screen area with border and attributes set according to register a
  clear_screen  clrmem  scr_p, scrlen
                clrmem  attr_p, attrlen, a
  set_border_cr anda 0b00111000
                3.times { rrca }
                out  (254), a
                ret

  # print number stored in register hl
  # after the number a character from register a is being printed out
  print_hl      ld  [somebigint], hl
                ld  c, 2
                jp  prnum_term
  # print 16bit number stored in memory at +somebigint+
  # followed by 8bit number stored in memory at +sombigint1+
  # after the numbers an ENTER character is being printed out
  print_16_8    ld  c, 2
                ld  a, ' '.ord
                call prnum_term
                ld  a, [sombigint1]
                ld  [somebigint], a
                ld  c, 1
                jp  print_num
  # print 16bit number stored in memory at +somebigint+
  # followed by 16bit number stored in memory at +sombigint1+
  # after the numbers an ENTER character is being printed out
  print_16_16   ld  c, 2
                ld  a, ' '.ord
                call prnum_term
                ld  hl, [sombigint1]
                ld  [somebigint], hl
                ld  c, 2
                jp  print_num
  # print 32bit number stored in memory at +somebigint+
  # followed by 16bit number stored in memory at +sombigint1+
  # after the numbers an ENTER character is being printed out
  print_32_16   ld  c, 4
                ld  a, ' '.ord
                call prnum_term
                ld  hl, [sombigint1]
                ld  [somebigint], hl
                ld  c, 2
                jp  print_num
  # print 16bit number stored in memory at +somebigint+
  # after the number an ENTER character is being printed out
  print_num16   ld  c, 2
                jr  print_num
  # print 32bit number stored in memory at +somebigint+
  # after the number an ENTER character is being printed out
  print_num32   ld  c, 4
  # print number stored in memory at +somebigint+ with the number of bytes in c register
  # after the number an ENTER character is being printed out
  print_num     ld  a, "\r".ord
  # print number stored in memory at +somebigint+ with the number of bytes in c register
  # after the number a character from register a is being printed out
  prnum_term    ld  [terminator], a
  # print number stored in memory at +somebigint+ with the number of bytes in c register
  # after the number a +terminator+ character is being printed out
  prnum_last_term   label
                ld de, somebigint
  # print number pointed at by de register with the number of bytes in c register
  # after the number a +terminator+ character is being printed out
  prnum         utobcd bcdbufend, de, c
                exx
                ld  hl, bcdbufend
                sub_from c, h, l
                bcdtoa hl, c do |eoc|
                  jr  NC, pr1  # skip check if not first
                  jr  Z, eoc   # skip if 0
            pr1   add '0'.ord
                  rst 0x10
                end
                ld  a, [terminator]
                rst 0x10
                ret

  # Subroutine used by multiply32_test macro
  multiply32    mul16_32
                ld  [somebigint[1]], hl
                exx
                ld  [somebigint], hl
                exx
                ret

  # Subroutine used by div_euc8_test macro
  div_euc8      divmod8
                ld  [somebigint], hl
                ld  [sombigint1], a
                ret

  # Subroutine used by div_euc16_test macro
  div_euc16     divmod16
                ld  [somebigint], hl
                ld  [sombigint1], bc
                ret

  # Subroutine used by div_euc32_test macro
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

  # Prints string terminated with 0
  # The string data should be placed immediately after the call instruction.
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

  # Subroutine used by assert_equals macro
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

  # Subroutine used by benchmark macro
  ns :bench_results do
                ld    hl, [frames]
                ld    a,  [frames + 2]
                ei
                ex    af, af
                ld    de, [somebigint]
                ld    a, [somebigint[1]]
                ld    c, a
                ex    af, af
                cp    a
                sbc   hl, de
                sbc   a, c
                ld    [somebigint], hl
                ld    [somebigint[1]], a
                print_text "\x08\x08\x08completed:\r- in "
                ld    c, 3
                ld    a, ' '.ord
                call  prnum_term
                print_text "frames\r- in "
                ld    hl, [somebigint]
                ld    a, [somebigint[1]]
                exx   # hl -> hl'
                ld    h, 0
                ld    l, a
                ld    c, 50
                divmod32_8
                add   a # remainder * 2 (0..98)
                ld    [somebigint[1]], hl
                exx
                ld    [somebigint], hl
                push  af
                ld    c, 4
                ld    a, '.'.ord
                call  prnum_term
                pop   af # a=remainder*2
                ld    [somebigint], a
                cp    10
                jr    NC, skip0
                print_char '0'
    skip0       ld    c, 1
                ld    a, ' '.ord
                call  prnum_term
                print_text "seconds.\r"
                ret
  end

  # get next pseudo-random number seeding from VARS SEED and update the seed
  next_rnd_fn   ld   hl, [rnd_seed]
                call next_rnd_hl
                ld   c, l
                ld   b, h
                ret

  # create seed from frames and register r
  seed_entropy  ld hl, [frames]
                ld a, [frames + 2]
                xor h
                ld h, a
                xor l
                ld l, a
                ld a, r
                xor l
                ld l, a
                halt
                ld a, r
                xor h
                ld h, a
                jr set_rnd_seed
  # get next pseudo-random number and update the seed
  next_rnd      ld hl, [seed]
  # get next pseudo-random number seeded in hl and update the seed
  next_rnd_hl   rnd
  # set seed from hl
  set_rnd_seed  ld [seed], hl
                ret

  ########
  # Data #
  ########

  sp_save       words 1

  seed          words 1

  somebigint    words 2
  sombigint1    words 2
  sombigint2    words 4
  somebigbytes  union somebigint, 1

                bytes 20
  bcdbufend     label
  terminator    db  13

end

math = Program.new 0x8000
puts math.debug
puts "\nExports: "
Program.exports.each_key {|k| puts " #{k.to_s.ljust(15)}:  0x#{math[k].to_s 16} (#{math[k]})" }

math.save_tap('math')

Z80::TAP.read_chunk('examples/mathi_test.tap') do |hb|
    File.open('examples/mathi_test.tap', 'wb') do |f|
        f.write hb.to_tap
        f.write math.to_tap('math')
    end
end
