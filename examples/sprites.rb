# -*- coding: BINARY -*-
require 'z80'

class Sprites
  include Z80
  include Z80::TAP

  export :auto

  sprite1 bytes [
    0b00000111,0b00000000,
    0b00000011,0b10000000,
    0b00000011,0b11000000,
    0b00000111,0b11100000,
    0b10001101,0b10100001,
    0b11111111,0b11110011,
    0b11111111,0b11111111,
    0b11001111,0b11111111,
    0b00000111,0b11100001,
    0b00000111,0b11100000,
    0b00000111,0b11100000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00011111,0b11111000,
    0b00011111,0b11111000,
    0b00000111,0b11000000,

    0b00000111,0b00000000,
    0b00000011,0b10000000,
    0b00000011,0b11000000,
    0b00000111,0b11100001,
    0b00001101,0b10100001,
    0b11111111,0b11110011,
    0b11111111,0b11111111,
    0b11001111,0b11111111,
    0b10000111,0b11100000,
    0b00000111,0b11100000,
    0b00000111,0b11100000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00011111,0b11111000,
    0b00011111,0b11111000,
    0b00000011,0b11110000,

    0b00000011,0b10000000,
    0b00000011,0b10000000,
    0b00000011,0b11000000,
    0b00000111,0b11100001,
    0b00001101,0b10100001,
    0b11111111,0b11110011,
    0b11111111,0b11111111,
    0b11001111,0b11111110,
    0b10000111,0b11100000,
    0b00000111,0b11100000,
    0b00000111,0b11100000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00011111,0b11111000,
    0b00011111,0b11111000,
    0b00000001,0b11111000,

    0b00000001,0b11000000,
    0b00000001,0b11000000,
    0b00000011,0b11000000,
    0b00000111,0b11100001,
    0b00001101,0b10100011,
    0b01111111,0b11110111,
    0b11111111,0b11111110,
    0b11001111,0b11111100,
    0b10000111,0b11100000,
    0b10000111,0b11100000,
    0b00000111,0b11100000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00011111,0b11111000,
    0b00111111,0b11111000,
    0b00000000,0b11111100,

    0b00000000,0b11100000,
    0b00000001,0b11000000,
    0b00000011,0b11000001,
    0b00000111,0b11100001,
    0b00001101,0b10100011,
    0b00111111,0b11111110,
    0b01111111,0b11111110,
    0b11001111,0b11111000,
    0b11000111,0b11100000,
    0b10000111,0b11100000,
    0b10000111,0b11100000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00011111,0b11111000,
    0b00111111,0b11111000,
    0b00111000,0b00111100,

    0b00000000,0b11100000,
    0b00000001,0b11000000,
    0b00000011,0b11000000,
    0b00000111,0b11100001,
    0b00001101,0b10100011,
    0b00111111,0b11110111,
    0b11111111,0b11111110,
    0b11001111,0b11111000,
    0b11000111,0b11100000,
    0b10000111,0b11100000,
    0b00000111,0b11100000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00011111,0b11111000,
    0b00111111,0b11111000,
    0b00111100,0b00011100,

    0b00000001,0b11000000,
    0b00000001,0b11000000,
    0b00000011,0b11000000,
    0b00000111,0b11100001,
    0b00001101,0b10100001,
    0b11111111,0b11110011,
    0b11111111,0b11111111,
    0b11001111,0b11111100,
    0b10000111,0b11100000,
    0b00000111,0b11100000,
    0b00000111,0b11100000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00011111,0b11111000,
    0b00111111,0b11111000,
    0b00111110,0b00001100,

    0b00000011,0b10000000,
    0b00000011,0b10000000,
    0b00000011,0b11000000,
    0b00000111,0b11100000,
    0b10001101,0b10100001,
    0b11111111,0b11110011,
    0b11111111,0b11111111,
    0b11001111,0b11111111,
    0b10000111,0b11100000,
    0b00000111,0b11100000,
    0b00000111,0b11100000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00011111,0b11111000,
    0b00111111,0b11111000,
    0b00011111,0b10000000]

  sprite2 bytes [
    0b00000111,0b11100000,
    0b00011111,0b11111000,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b01111111,0b11111110,
    0b01110011,0b11001110,
    0b11110011,0b11001111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111000,0b00011111,
    0b11110000,0b00001111,
    0b11110000,0b00001111,
    0b11111000,0b00011111,
    0b01111111,0b11111110,
    0b01111111,0b11111110,
    0b01100110,0b01100110,

    0b00000111,0b11100000,
    0b00011111,0b11111000,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b01111111,0b11111110,
    0b01110011,0b11001110,
    0b11110011,0b11001111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111100,0b00111111,
    0b11110000,0b00001111,
    0b11110000,0b00001111,
    0b11111000,0b00011111,
    0b01111111,0b11111110,
    0b01111111,0b11111110,
    0b00110011,0b00110011,

    0b00000111,0b11100000,
    0b00011111,0b11111000,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b01111111,0b11111110,
    0b01110011,0b11001110,
    0b11111011,0b11011111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11110000,0b00001111,
    0b11110000,0b00001111,
    0b01111100,0b00111111,
    0b01111111,0b11111111,
    0b11111111,0b11111110,
    0b10011001,0b10011000,

    0b00000111,0b11100000,
    0b00011111,0b11111000,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b01111111,0b11111110,
    0b01110011,0b11001110,
    0b11111011,0b11011111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111100,0b00111111,
    0b11110000,0b00001111,
    0b11111100,0b00111111,
    0b01111111,0b11111110,
    0b01111111,0b11111110,
    0b11001100,0b11001100,

    0b00000111,0b11100000,
    0b00011111,0b11111000,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b01111111,0b11111110,
    0b01110011,0b11001110,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111000,0b00011111,
    0b11111111,0b11111111,
    0b01111111,0b11111110,
    0b01111111,0b11111110,
    0b01100110,0b01100110,

    0b00000111,0b11100000,
    0b00011111,0b11111000,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b01111111,0b11111110,
    0b01110011,0b11001110,
    0b11110111,0b11101111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111100,0b00111111,
    0b11111000,0b00011111,
    0b11111100,0b00111110,
    0b11111111,0b11111110,
    0b01111111,0b11111111,
    0b00110011,0b00110010,

    0b00000111,0b11100000,
    0b00011111,0b11111000,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b01111111,0b11111110,
    0b01110011,0b11001110,
    0b11110011,0b11001111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b11111100,0b01111111,
    0b11110000,0b00011111,
    0b11110000,0b00001111,
    0b01111000,0b00001111,
    0b11111111,0b11111110,
    0b11111111,0b11111100,
    0b01001100,0b11001100,

    0b00000111,0b11100000,
    0b00011111,0b11111000,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b01111111,0b11111110,
    0b01110011,0b11001110,
    0b11110011,0b11001111,
    0b11111111,0b11111111,
    0b11111100,0b00011111,
    0b11110000,0b00001111,
    0b11100000,0b00000111,
    0b11100000,0b00000111,
    0b11111000,0b00011111,
    0b01111111,0b11111110,
    0b01111111,0b11111110,
    0b01100110,0b01100110]

  sprite3 bytes [
    0b00001100,0b01100000,
    0b00011110,0b11111000,
    0b00111111,0b01111100,
    0b01101111,0b01110110,
    0b01100111,0b11100110,
    0b11000011,0b11000011,
    0b11000011,0b11100011,
    0b11001111,0b01111111,
    0b11111110,0b00111111,
    0b11111111,0b11111111,
    0b11100111,0b11110011,
    0b01100011,0b00000010,
    0b01110000,0b00001110,
    0b00111000,0b00011100,
    0b00011111,0b11111000,
    0b00000111,0b11100000,

    0b00001100,0b01100000,
    0b00011110,0b11111000,
    0b00111111,0b01111100,
    0b01101111,0b01110110,
    0b01100111,0b11100110,
    0b11000011,0b11000011,
    0b11000111,0b11110011,
    0b11001111,0b01111111,
    0b11111110,0b00111111,
    0b11111111,0b11111111,
    0b11100111,0b11110011,
    0b01110011,0b00000110,
    0b01111000,0b00001110,
    0b00111100,0b00111100,
    0b00011111,0b11111000,
    0b00000111,0b11100000,

    0b00001100,0b01100000,
    0b00011110,0b11111000,
    0b00111111,0b01111100,
    0b01101111,0b01110110,
    0b01100111,0b11100110,
    0b11000011,0b11000011,
    0b11000111,0b11110011,
    0b11011111,0b01111011,
    0b11111110,0b00111111,
    0b11111111,0b11111111,
    0b11111111,0b11110111,
    0b01110011,0b00000110,
    0b01111000,0b00001110,
    0b00111111,0b11111100,
    0b00011111,0b11111000,
    0b00000111,0b11100000,

    0b00001100,0b01100000,
    0b00011110,0b11111000,
    0b00111111,0b01111100,
    0b01101111,0b01110110,
    0b01100111,0b11100110,
    0b11000011,0b11000011,
    0b11000111,0b11110011,
    0b11011111,0b01111011,
    0b11111110,0b00111111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b01111011,0b00001110,
    0b01111100,0b01111110,
    0b00111111,0b11111100,
    0b00011111,0b11111000,
    0b00000111,0b11100000,

    0b00001100,0b01100000,
    0b00011110,0b11111000,
    0b00111111,0b01111100,
    0b01101111,0b01110110,
    0b01100111,0b11100110,
    0b11000011,0b11000011,
    0b11000111,0b11110011,
    0b11011111,0b01111011,
    0b11111110,0b00111111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b01111011,0b01111110,
    0b01111100,0b11111110,
    0b00111111,0b11111100,
    0b00011111,0b11111000,
    0b00000111,0b11100000,

    0b00001100,0b01100000,
    0b00011110,0b11111000,
    0b00111111,0b01111100,
    0b01101111,0b01110110,
    0b01100111,0b11100110,
    0b11000011,0b11000011,
    0b11000111,0b11110011,
    0b11001111,0b01111111,
    0b11111110,0b00111111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b01110011,0b00011110,
    0b01111000,0b00111110,
    0b00111111,0b11111100,
    0b00011111,0b11111000,
    0b00000111,0b11100000,

    0b00001100,0b01100000,
    0b00011110,0b11111000,
    0b00111111,0b01111100,
    0b01101111,0b01110110,
    0b01100111,0b11100110,
    0b11000011,0b11000011,
    0b11000111,0b11110011,
    0b11001111,0b01111111,
    0b11111110,0b00111111,
    0b11111111,0b11111111,
    0b11111111,0b11110111,
    0b01100011,0b00000110,
    0b01110000,0b00011110,
    0b00111000,0b00111100,
    0b00011111,0b11111000,
    0b00000111,0b11100000,

    0b00001100,0b01100000,
    0b00011110,0b11111000,
    0b00111111,0b01111100,
    0b01101111,0b01110110,
    0b01100111,0b11100110,
    0b11000011,0b11000011,
    0b11000011,0b11100011,
    0b11001111,0b01111111,
    0b11111110,0b00111111,
    0b10111111,0b11111101,
    0b11001111,0b11100011,
    0b01100011,0b00000110,
    0b01110000,0b00001110,
    0b00111000,0b00011100,
    0b00011111,0b11111000,
    0b00000111,0b11100000]

  sprite4 bytes [
    0b00000000,0b00000000,
    0b00000000,0b00000000,
    0b00000000,0b00000000,
    0b00000111,0b11100000,
    0b00111111,0b11111100,
    0b01111111,0b11100010,
    0b11111111,0b11111101,
    0b11100011,0b11000111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b01111111,0b11111110,
    0b00111111,0b11111100,
    0b00000111,0b11100000,
    0b00000000,0b00000000,
    0b00000000,0b00000000,
    0b00000000,0b00000000,

    0b00000000,0b00000000,
    0b00000000,0b00000000,
    0b00000000,0b00000000,
    0b00000111,0b11100000,
    0b00111111,0b11111100,
    0b01111111,0b11100010,
    0b11111111,0b11111101,
    0b11110011,0b11001111,
    0b11111111,0b11111111,
    0b11111111,0b11111111,
    0b01111111,0b11111110,
    0b00111111,0b11111100,
    0b00000111,0b11100000,
    0b00000000,0b00000000,
    0b00000000,0b00000000,
    0b00000000,0b00000000,

    0b00000000,0b00000000,
    0b00000000,0b00000000,
    0b00000111,0b11100000,
    0b00011111,0b11011000,
    0b00111111,0b11100100,
    0b00111111,0b11111100,
    0b01111111,0b11111110,
    0b01110011,0b11001110,
    0b01111101,0b10111110,
    0b01111111,0b11111110,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b00011111,0b11111000,
    0b00000111,0b11100000,
    0b00000000,0b00000000,
    0b00000000,0b00000000,

    0b00000000,0b00000000,
    0b00000011,0b11000000,
    0b00001111,0b11110000,
    0b00011111,0b11001000,
    0b00011111,0b11111000,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b00111001,0b10011100,
    0b00111101,0b10111100,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b00011111,0b11111000,
    0b00011111,0b11111000,
    0b00001111,0b11110000,
    0b00000011,0b11000000,
    0b00000000,0b00000000,

    0b00000011,0b11000000,
    0b00000111,0b11100000,
    0b00001111,0b11010000,
    0b00001111,0b11010000,
    0b00001111,0b11010000,
    0b00011111,0b11101000,
    0b00011101,0b10111000,
    0b00011101,0b10111000,
    0b00011101,0b10111000,
    0b00011111,0b11111000,
    0b00011111,0b11111000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00000111,0b11100000,
    0b00000011,0b11000000,

    0b00000011,0b11000000,
    0b00000111,0b10100000,
    0b00001111,0b11010000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00011111,0b11111000,
    0b00011101,0b10111000,
    0b00011101,0b10111000,
    0b00011111,0b11111000,
    0b00011111,0b11111000,
    0b00011111,0b11111000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00001111,0b11110000,
    0b00000111,0b11100000,
    0b00000011,0b11000000,

    0b00000000,0b00000000,
    0b00000011,0b11000000,
    0b00001111,0b10110000,
    0b00011111,0b11001000,
    0b00011111,0b11111000,
    0b00111111,0b11111100,
    0b00111101,0b10111100,
    0b00110011,0b11001100,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b00011111,0b11111000,
    0b00011111,0b11111000,
    0b00001111,0b11110000,
    0b00000011,0b11000000,
    0b00000000,0b00000000,

    0b00000000,0b00000000,
    0b00000000,0b00000000,
    0b00000111,0b11100000,
    0b00011111,0b11011000,
    0b00111111,0b11100100,
    0b00111111,0b11111100,
    0b01111111,0b11111110,
    0b01110001,0b10001110,
    0b01111111,0b11111110,
    0b01111111,0b11111110,
    0b00111111,0b11111100,
    0b00111111,0b11111100,
    0b00011111,0b11111000,
    0b00000111,0b11100000,
    0b00000000,0b00000000,
    0b00000000,0b00000000]
end

if __FILE__ == $0
  require 'zxlib/gfx'
  require 'zxlib/gfx/bobs'
  require 'zxlib/basic'

  class SpritesDemo
    include Z80
    include Z80::TAP

    # macro_import MathInt
    label_import ZXLib::Sys, macros: true
    macro_import ZXLib::Gfx
    macro_import ZXLib::Gfx::Bobs

    import              Sprites

    with_saved :show_sprite, :exx, hl, ret: true do
                        call  fn_argn
                        ret   C
    error_q             report_error_unless Z, 'Q Parameter error'
                        call  get_uint_de
                        cp16n d, e, 30
                        jr    NC, error_a
                        ld    c, e   # x
                        call  fn_argn.seek_next
                        jr    NZ, error_q.err
                        call  get_uint_de
                        cp16n d, e, 192-16
                        jr    NC, error_a
                        ld    b, e   # y
                        call  fn_argn.seek_next
                        jr    NZ, error_q.err
                        call  get_uint_de # bitmap
                        ld    a, b   # y
                        ytoscr a, ah:h, al:l, col:c, t:b, scraddr:0x4000
                        ex    de, hl
                        bobs_copy_pixels hl, 16, 2, target:de, scraddr:nil, subroutine:false
    end

    error_a             report_error 'A Invalid argument'

    fn_argn             find_def_fn_args 1, cf_on_direct:true

    get_uint_de         read_positive_int_value d, e
                        jr   NZ, error_a
                        inc  hl
                        ret
  end

  sprites_demo = SpritesDemo.new 0xF000
  puts sprites_demo.debug
  program = ZXLib::Basic.parse_source <<-EOB
   0 DEF FN g(c,y,a)=USR #{sprites_demo[:show_sprite]}
     BORDER 0: PAPER 0: INK 7: BRIGHT 1: INVERSE 0: OVER 0: FLASH 0: CLS
  10 FOR c=0 TO 3
     FOR r=0 TO 7
     RANDOMIZE FN g(c*4,r*20,#{sprites_demo[:sprite1]}+c*256+r*32)
     NEXT r
     NEXT c
     PAUSE 0
 100 FOR c=0 TO 3
     LET m=0
 110 IF LEN INKEY$ THEN GO TO 110
 120 RANDOMIZE FN g(c*4,0,#{sprites_demo[:sprite1]}+c*256+m)
     LET m=m+32: IF m>=256 THEN LET m=0
     PAUSE 1: IF NOT LEN INKEY$ THEN GO TO 120
     NEXT c
     GO TO 10
9998 STOP: RUN
9999 CLEAR #{sprites_demo.org-1}: LOAD ""CODE : RUN
EOB
  tap_name = 'examples/sprites.tap'
  program.save_tap(tap_name, line: 9999)
  sprites_demo.save_tap(tap_name, append:true, name:'sprites')

  sprites = Sprites.new 0xF000
  File.open('examples/sprites.bin', 'wb') {|f| f.write sprites.code }
  Z80::TAP.parse_file(tap_name) { |hb| puts hb.to_s }
end