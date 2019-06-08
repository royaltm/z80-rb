require 'z80/math_i'
require 'zxlib/gfx'
module ZXLib
  module Gfx
    ##
    #  Sprite drawing routines.
    #
    #  See also ZXLib::Gfx::Sprite8::Macros.
    #
    #  By default all drawing method routines are produced. To select only required routines
    #  define a ZXLib::Gfx::Sprite8::DRAW_METHODS constant before importing this file.
    #
    #      class ZXLib::Gfx::Sprite8
    #        DRAW_METHODS = [:xor]
    #      end
    #      require 'zxlib/gfx/sprite8'
    #
    class Sprite8
      include Z80 unless defined?(Program)

      macro_import MathInt
      macro_import ZXLib::Gfx

      export  draw_sprite8

      ##
      # Default screen address used by the routines.
      # You may change this value by overriding label +sprite8_screen_address+ when importing code.
      SCREEN_ADDRESS = 0x4000

      ##
      # Array of supported drawing methods: +:xor+, +:or+, +:set+, +:mask_or+
      DRAW_METHODS        = [:xor, :or, :set, :mask_or] unless const_defined?(:DRAW_METHODS)

      DRAW_METHOD_XOR     = DRAW_METHODS.include?(:xor) unless const_defined?(:DRAW_METHOD_XOR)
      DRAW_METHOD_OR      = DRAW_METHODS.include?(:or) unless const_defined?(:DRAW_METHOD_OR)
      DRAW_METHOD_SET     = DRAW_METHODS.include?(:set) unless const_defined?(:DRAW_METHOD_SET)
      DRAW_METHOD_MASK_OR = DRAW_METHODS.include?(:mask_or) unless const_defined?(:DRAW_METHOD_MASK_OR)

      ##
      # ==ZXLib::Gfx::Sprite8 Macros.
      module Macros
        ##
        #  Calculates coordinates and prepares registers for +draw_sprite8+
        #
        #  Uses:: +af+, +af'+, +bc+, +de+, +hl+
        #
        #  Input:
        #
        #  * +hl+:: sprite address
        #  * +a+::  sprite height (1..192)
        #  * +a'+:: sprite width in bytes (1..32)
        #  * +bc+:: x - coordinate (-32768..32767) [screen area: 0-255]
        #  * +de+:: y - coordinate (-32768..32767) [screen area: 0-191]
        #  * +f+:: flags for mode
        #  * +outofscreen+:: if no block is given then provide +label+, otherwise +ret+
        #
        #  Flags howto:
        #
        #         CF:     0      1       0              1
        #         ZF:     0      0       1              1
        #       mode:    OR    SET     XOR  AND+OR (mask)
        #      howto: ora 1  scf      cp a          cp  a
        #                    sbc a                  scf
        #
        #  When <tt>CF=ZF=1</tt> sprite address skip bytes from left margin calculations take mask into account.
        #
        #  Output:
        #
        #  * +de+:: sprite address
        #  * +h+::  vertical coordinate   (0..191)
        #  * +l+::  horizontal coordinate (0..255)
        #           except when h > 191 then l contains vertical coordinate and h | 0xf8 is a negative h-coordinate (from -1 to -7)
        #  * +b+::  skip first sprite lines (for negative vertical coordinate y < 0, +h+ should be 0 and +b+ should be -y)
        #  * +c+::  sprite byte width (1..32) (pixel width / 8)
        #  * +a'+:: sprite height (1..192)
        #  * +f'+:: flags for mode
        def gfx_sprite8_calculate_coords(outofscreen: :ret, **nsopts, &block)
          isolate do |eoc|
                    ex   af, af       # store CF and sprite height
                    push af           # sprite width
                    ld   a, d
                    ora  a
                    jp   Z, vnext1    # 0 <= de < 256
                    inc  a            # d == 0xff
                    jr   NZ, quit1    # de < -256
                    xor  a
                    sub  e
                    jr   Z, quit1     # de == -256
                    ld   d, a         # skip
                    ld   e, 0         # y = 0
                    jp   hnext1
            quit1   pop  af
            if block_given?
                    ns(:quitoos, **nsopts) do
                      yield eoc
                    end
            elsif label?(outofscreen)
            quitoos jp   outofscreen
            else
                    ret
            end
            vnext1  ld   a, e         # 0 <= de < 192
                    cp   192
                    jr   NC, quit1    # de >= 192
            hnext1  ld   a, b
                    ora  a            # 0<= bc < 256
                    jp   Z, hnext2    # bc on screen
                    inc  a
                    jr   NZ, quit1    # bc < -256

                    xor  a
                    sub  c            # x = -x
                    jr   Z, quit1     # x == -256
                    anda 0xf8
                    jr   Z, fskip     # -8 < x < 0
                    rrca
                    rrca
                    rrca
                    ld   b, a         # -x / 8
                    pop  af           # sprite width
                    sub  b            # width -= -x / 8
            if block_given? or label?(outofscreen)
                    jr   C, quitoos   # width <  -x / 8
                    jr   Z, quitoos   # width == -x / 8
            else
                    ret  C            # width <  -x / 8
                    ret  Z            # width == -x / 8
            end
                    ex   af, af       # new width
                    push af           # height + CZ
                    push de
                    ld   d, 0
                    ld   e, a         # height
                    jr   NC, mulh.muls1
                    jr   NZ, mulh.muls1 # height*2 (andor) C=1 Z=1
            mulh    mul8(d, e, b, tt:de, clrhl:false, double:true) # sprite address+= height * (-x / 8)
                    pop  de
                    pop  af
                    ex   af, af       # height + CZ
                    push af           # new width

            fskip   ld   a, c
                    ora  0xf8
                    ld   c, e
                    ld   e, a
            hnext2  ex   de, hl       # sprite -> de
                    ld   b, h         # skip first
                    ld   h, l         # h = y & 0xff
                    ld   l, c         # l = x & 0xff
                    pop  af
                    ld   c, a         # sprite width
          end
        end

      end

      sprite8_screen_address addr SCREEN_ADDRESS

      ##
      #  Draws a sprite using xor/or/clear/and+or with arbitrary height and width.
      #
      #  In <tt>and+or</tt> mode sprite bitmap bytes must be interwined with mask: b1 m1 b2 m2 b2 m2 ...
      #
      #  The data for sprites is laid verticaly: first 8-pixel column bytes, second 8-pixel column bytes, ...
      #
      #  This routine is optimised for vertical sprites with height > width and small square ones (width < 24 pixels).
      #
      #  Uses:: +af+, +af'+, +bc+, +de+, +hl+, +bc'+, +de'+, +hl'+, +ix+, +sp+ (stack depth: max 6 bytes),
      #         in and+or mode also +iy+.
      #
      #  Input:
      #
      #  * +de+:: sprite address
      #  * +h+::  vertical coordinate   (0..191)
      #  * +l+::  horizontal coordinate (0..255)
      #           except when h > 191 then l contains vertical coordinate and h | 0xf8 is a negative h-coordinate (from -1 to -7)
      #  * +b+::  skip first sprite lines (for negative vertical coordinate y < 0, +h+ should be 0 and +b+ should be -y)
      #  * +c+::  sprite byte width (1..32) (pixel width / 8)
      #  * +a'+:: sprite height (1..192)
      #  * +f'+:: flags for mode
      #
      #  Flags howto:
      #
      #         CF:     0      1       0              1
      #         ZF:     0      0       1              1
      #       mode:    OR    SET     XOR  AND+OR (mask)
      #      howto: ora 1  scf      cp a          cp  a
      #                    sbc a                  scf
      ns :draw_sprite8 do
                push bc             # save width and skip
                ld   a, h
                cp   192
                jp   C, skipneg
                anda 0x07
                jr   Z, skipad      # sanity check
                add  7              # negative vect
        skipad  ld   c, a           # C: negshift (0, 8..14)
                ytoscr l, ah:h, al:l, t:b, scraddr:sprite8_screen_address
                jp   skippos
                                    # HL: yx, HL: screen, C: shift (0..7), B: temp
        skipneg xytoscr h, l, ah:h, al:l, s:c, t:b, scraddr:sprite8_screen_address
        skippos push hl             # screen addr

                ld   b, 0
                ld   hl, maskshift
                add  hl, bc         # HL: -> (maskshift + shift)
                ld   a, [hl]        # A: rotate mask

                exx
                ld   e, a           # E: rotate mask
                pop  hl             # HL: screen addr
                exx                 # E': rotate mask, H'L': screen addr

                ex   af, af         # A: height + CZ
                ld   b, a           # B: height
                ld   a, c           # A: shift 0..14

        unless DRAW_METHOD_XOR or DRAW_METHOD_OR or DRAW_METHOD_SET or DRAW_METHOD_MASK_OR
          raise Syntax, "there must be at least one draw method selected"
        end

        if DRAW_METHOD_XOR or DRAW_METHOD_OR
          if DRAW_METHOD_SET or DRAW_METHOD_MASK_OR
                  jr   C, aocskip
          end
          if DRAW_METHOD_XOR and DRAW_METHOD_OR
                  jr   NZ, orskip
          end

          if DRAW_METHOD_XOR
                  add  a
                  jr   Z, fastxor
                  ld   hl, sprxor.start
                  push hl             # jump addr
                  ld   hl, jumpxor - 2
                  jp   skipall
          fastxor ld   hl, sprxor.fstcopy
                  jp   skpfast
          end

          if DRAW_METHOD_OR
          orskip  add  a
                  jr   Z, fastor
                  ld   hl, spror.start
                  push hl             # jump addr
                  ld   hl, jumpor - 2
                  jp   skipall
          fastor  ld   hl, spror.fstcopy
                  jp   skpfast
          end
        end

        if DRAW_METHOD_SET and DRAW_METHOD_MASK_OR
          aocskip jr   Z, andskip
        elsif DRAW_METHOD_SET or DRAW_METHOD_MASK_OR
          aocskip label
        end

        if DRAW_METHOD_SET
                add  a
                jr   Z, fastclr
                ld   hl, spclror.start
                push hl             # jump addr
                ld   hl, jumpclror - 2
                jp   skipall
        fastclr ld   hl, spclror.fstcopy
                jp   skpfast
        end

        if DRAW_METHOD_MASK_OR
        andskip ld   hl, spandor.start
                add  a
                jr   Z, fastaor
                push hl             # jump addr
                ld   hl, jumpandor - 4
                add  a
                adda_to h, l
                ld   a, [hl]
                ld   iyl, a
                inc  hl
                ld   a, [hl]
                ld   iyh, a
                inc  hl
                jp   skipal2
        fastaor ld   hl, spandor.fstcopy
                jp   skpfast
        end

        if DRAW_METHOD_XOR or DRAW_METHOD_OR or DRAW_METHOD_SET
          skipall adda_to h, l
        end
        skipal2 ld   a, [hl]
                ld   ixl, a
                inc  hl
                ld   a, [hl]
                ld   ixh, a
                pop  hl             # jump addr
        skpfast ld   a, b           # height
                pop  bc             # skip + width
                sub  b              # height - skip first lines
                ret  C              # return if skip first > height
                ret  Z              # return if skip first == height
                jp   (hl)
      end
      

      # hl' - screen
      #  e' - mask     (except fstcopy)
      # ix  - vectmask (except fstcopy)
      # iy  - vectbmap (except fstcopy)
      # de  - sprite
      #  a  - height
      #  c  - width
      #  b  - skip
      macro :spriteandor8 do
        fstcopy ld   h, 0     # 7
                ld   l, b     # 4 skip first
                add  hl, hl   # 11
                ex   de, hl   # 4
                ld   b, a     # 4 height
                exx           # 4
                jp   mainf0   # 10
        mainlpf exx           # 4
                pop  hl       # 10 screen addr
                inc  l        # 4
                ld   a, l     # 4
                anda 0x1f     # 7
                ret  Z        # 5 out of screen
        mainf0  push hl       # 11 save screen addr
                exx           # 4
                push bc       # 11 height + width
                add  hl, de   # 11 skip first
        copylp  ld   a, [hl]  # 7 bitmap
                inc  hl       # 6
                ex   af, af   # 4
                ld   a, [hl]  # 7 mask
                inc  hl       # 6
                exx           # 4
                cpl
                anda [hl]
                ld   c, a
                ex   af, af   # bitmap
                ora  c
                ld   [hl], a
                nextline h, l, quitf0, scraddr:sprite8_screen_address
                exx
                djnz copylp
                jp   quitf1
        quitf0  exx
                dec  b
                ld   c, b
                ld   b, 0
                add  hl, bc
                add  hl, bc
        quitf1  pop  bc
                dec  c
                jp   NZ, mainlpf
                pop  hl
                ret

        lastcol rrca          # 4*4
                rrca
                rrca
                rrca
                ld   d, a     # 4
                anda e        # 4
                xor  d        # 4
                ora  c        # 4
                ld   [hl], a  # 7
                nextline h, l, quit0, scraddr:sprite8_screen_address #27 /49 /59
                exx           # 4
                djnz shftlp   # 13/8
                pop  bc       # 10
                dec  c        # 4
                jp   NZ, mainlp # 10
                pop  hl
                ret

        shft4   rrca          # 4*4
        shft3   rrca
        shft2   rrca
        shft1   rrca
                ld   d, a     # 4
                anda e        # 4
                xor  d        # 4
                ora  c        # 4
                ld   [hl], a  # 7
                inc  l        # 4
                ld   a, d     # 4
                anda e        # 4
                ld   d, a     # 4
                ld   a, b     # 4
                cpl           # 4
                anda [hl]     # 7
                ora  d        # 4
                ld   [hl], a  # 7
                dec  l        # 4
                nextline h, l, quit0, scraddr:sprite8_screen_address #27 /49 /59
                exx           # 4
                djnz shftlp   # 13/8
                pop  bc       # 10
                dec  c        # 4
                jp   NZ, mainlp # 10
                pop  hl
                ret

        shft_4  rrca
        shft_3  rrca
        shft_2  rrca
        shft_1  rrca
                anda e
                ora  c
                ld   [hl], a
                nextline h, l, quit1, scraddr:sprite8_screen_address
                exx
                djnz shftlp
                jp   adjust1

        mainlp  exx           # 4
                pop  hl       # 10 screen addr
                inc  l        # 4
                ld   a, l     # 4
                anda 0x1f     # 7
                ret  Z        # 5 out of screen
        main0   push hl       # 11 save screen addr
                add  0xe1     # 7  check if last screen column
                jp   NC, skiplst # 7/12 not last column
                ld   bc, (lastcol - shft4).to_i # 10
                add  iy, bc   # 15
        skiplst exx           # 4
                push bc       # 11 height + width
                add  hl, de   # 11 skip first
        shftlp  ld   a, [hl]  # 7 bitmap
                inc  hl       # 6
                ex   af, af   # 4
                ld   a, [hl]  # 7 mask
                inc  hl       # 6
                exx           # 4
                jp   (ix)     # 8

        start   ld   h, 0     # 7
                ld   l, b     # 4 skip first
                add  hl, hl   # 11
                ex   de, hl   # 4
                ld   b, a     # 4 height
                exx           # 4
                ld   a, l     # 4
                anda 0x1f     # 7
                jp   main0    # 10

        lastco2 rlca          # 4*4
                rlca
                rlca
                ld   d, a     # 4
                anda e        # 4
                xor  d        # 4
                ora  c        # 4
                ld   [hl], a  # 7
                nextline h, l, quit0, scraddr:sprite8_screen_address #27 /49 /59
                exx           # 4
                djnz shftlp   # 13/8
                pop  bc       # 10
                dec  c        # 4
                jp   NZ, mainlp # 10
                pop  hl
                ret
                nop

        shft5   rlca
        shft6   rlca
        shft7   rlca
                ld   d, a
                anda e
                xor  d
                ora  c
                ld   [hl], a
                inc  l
                ld   a, d
                anda e
                ld   d, a
                ld   a, b
                cpl
                anda [hl]
                ora  d
                ld   [hl], a
                dec  l
                nextline h, l, quit0, scraddr:sprite8_screen_address
                exx
                djnz shftlp
                pop  bc
                dec  c
                jp   NZ, mainlp
                pop  hl
                ret
                nop

        shft_5  rlca
        shft_6  rlca
        shft_7  rlca
                anda e
                ora  c
                ld   [hl], a
                nextline h, l, quit1, scraddr:sprite8_screen_address
                exx
                dec  b
                jp   NZ, shftlp
                jp   adjust1

        shftm4  rrca          # 4*4
        shftm3  rrca
        shftm2  rrca
        shftm1  rrca
                ld   c, a     # 4
                anda e        # 4
                ld   b, a     # 4
                xor  c        # 4
                cpl           # 4
                anda [hl]     # 7
                ld   c, a     # 4
                ex   af, af   # 4 bitmap
                jp   (iy)     # 8

        shftm_4 rrca
        shftm_3 rrca
        shftm_2 rrca
        shftm_1 rrca
                anda e
                cpl
                anda [hl]
                ld   c, a
                ex   af, af # bitmap
                jp   (iy)

        shftm5  rlca
        shftm6  rlca
        shftm7  rlca
                ld   c, a
                anda e
                ld   b, a
                xor  c
                cpl
                anda [hl]
                ld   c, a
                ex   af, af # bitmap
                jp   (iy)
                nop

        shftm_5 rlca
        shftm_6 rlca
        shftm_7 rlca
                anda e
                cpl
                anda [hl]
                ld   c, a
                ex   af, af # bitmap
                jp   (iy)
        adjust1 inc  b
                exx
        quit1   ld   bc, (shft4 - shft_4).to_i
                add  iy, bc
                ld   c,  (shftm4 - shftm_4).to_i
                add  ix, bc
                pop  hl
                ex   [sp], hl   # screen addr
                exx
                dec  b
                ld   c, b
                ld   b, 0
                add  hl, bc
                add  hl, bc
                pop  bc
                dec  c
                exx
                ret  Z
                push hl
                jp   skiplst
        quit0   exx
                dec  b
                ld   c, b
                ld   b, 0
                add  hl, bc
                add  hl, bc
                pop  bc
                dec  c
                jp   NZ, mainlp
                pop  hl
                ret
        unless (lastcol - shft4).to_i == (lastco2 - shft5).to_i and (shft4 - shft_4).to_i == (shft5 - shft_5).to_i and (shftm4 - shftm_4).to_i == (shftm5 - shftm_5).to_i
          raise Syntax, "#{lastcol - shft4} == #{lastco2 - shft5} and #{shft4 - shft_4} == #{shft5 - shft_5} and #{shftm4 - shftm_4} == #{shftm5 - shftm_5}"
        end
      end


      # hl' - screen
      #  e' - mask
      # ix  - vect
      # de  - sprite
      #  a  - height
      #  c  - width
      #  b  - skip
      # type = :xor, :or
      macro :spritexoror8 do |_, type| # 141:max (shftlp each byte) + 109/134:last (mainlp each column)
        fstcopy ld   h, 0     # 7
                ld   l, b     # 4 skip first
                ex   de, hl   # 4
                ld   b, a     # 4 height
                exx           # 4
                jp   mainf0   # 10
        mainlpf exx           # 4
                pop  hl       # 10 screen addr
                inc  l        # 4
                ld   a, l     # 4
                anda 0x1f     # 7
                ret  Z        # 5 out of screen
        mainf0  push hl       # 11 save screen addr
                exx           # 4
                push bc       # 11 height + width
                add  hl, de   # 11 skip first
        copylp  ld   a, [hl]  # 7 bitmap
                inc  hl       # 6
                exx           # 4
                self.send type, [hl]
                ld   [hl], a
                nextline h, l, quitf0, scraddr:sprite8_screen_address
                exx
                djnz copylp
                jp   quitf1
        quitf0  exx
                dec  b
                ld   c, b
                ld   b, 0
                add  hl, bc
        quitf1  pop  bc
                dec  c
                jp   NZ, mainlpf
                pop  hl
                ret

        lastco2 rlca
                rlca
                rlca
                ld   c, a
                anda e
                xor  c
                self.send type, [hl]
                ld   [hl], a
                nextline h, l, quit0, scraddr:sprite8_screen_address
                exx
                djnz shftlp
                pop  bc
                dec  c
                jr   NZ, mainlp
                pop  hl
                ret
                nop             # pad

        shft5   rlca
        shft6   rlca
        shft7   rlca
                ld   c, a
                anda e
                ld   b, a
                xor  c
                self.send type, [hl]
                ld   [hl], a
                inc  l
                ld   a, b
                self.send type, [hl]
                ld  [hl], a
                dec  l
                nextline h, l, quit0, scraddr:sprite8_screen_address
                exx
                djnz shftlp
                pop  bc
                dec  c
                jr   NZ, mainlp
                pop  hl
                ret
                nop

        shft_5  rlca
        shft_6  rlca
        shft_7  rlca
                anda e
                self.send type, [hl]
                ld   [hl], a
                nextline h, l, quit1, scraddr:sprite8_screen_address
                exx
                djnz shftlp
                jp   adjust1

        start   ld   h, 0     # 7
                ld   l, b     # 4 skip first
                ex   de, hl   # 4
                ld   b, a     # 4 height
                exx           # 4
                ld   a, l     # 4
                anda 0x1f     # 7
                jp   main0    # 10
        mainlp  exx           # 4
                pop  hl       # 10 screen addr
                inc  l        # 4
                ld   a, l     # 4
                anda 0x1f     # 7
                ret  Z        # 5 out of screen
        main0   push hl       # 11 save screen addr
                add  0xe1     # 7  check if last screen column
                jp   NC, skiplst # 10 not last column
                ld   bc, (lastco2 - shft5).to_i # 10
                add  ix, bc   # 15
        skiplst exx           # 4
                push bc       # 11 height + width
                add  hl, de   # 11 skip first
        shftlp  ld   a, [hl]  # 7
                inc  hl       # 6
                exx           # 4
                jp   (ix)     # 8

        lastcol rrca          # 4*4
                rrca
                rrca
                rrca
                ld   c, a     # 4
                anda e        # 4
                xor  c        # 4
                self.send type, [hl]  # 7
                ld   [hl], a  # 7
                nextline h, l, quit0, scraddr:sprite8_screen_address # 27
                exx           # 4
                djnz shftlp   # 13/8
                pop  bc       # 10
                dec  c        # 4
                jr   NZ, mainlp # 12
                pop  hl
                ret

        shft4   rrca          # 4*4
        shft3   rrca
        shft2   rrca
        shft1   rrca
                ld   c, a     # 4
                anda e        # 4
                ld   b, a     # 4
                xor  c        # 4
                self.send type, [hl]  # 7
                ld   [hl], a  # 7
                inc  l        # 4
                ld   a, b     # 4
                self.send type, [hl]  # 7
                ld  [hl], a   # 7
                dec  l        # 4
                nextline h, l, quit0, scraddr:sprite8_screen_address # 27
                exx           # 4
                djnz shftlp   # 13/8
                pop  bc       # 10
                dec  c        # 4
                jr   NZ, mainlp # 12
                pop  hl
                ret

        shft_4  rrca
        shft_3  rrca
        shft_2  rrca
        shft_1  rrca
                anda e
                self.send type, [hl]
                ld   [hl], a
                nextline h, l, quit1, scraddr:sprite8_screen_address
                exx
                dec  b
                jp   NZ, shftlp
        adjust1 inc  b
                exx
        quit1   ld   bc, (shft4 - shft_4).to_i
                add  ix, bc
                pop  hl
                ex   [sp], hl   # screen addr
                exx
                dec  b
                ld   c, b
                ld   b, 0
                add  hl, bc
                pop  bc
                dec  c
                exx
                ret  Z
                push hl
                jp   skiplst
        quit0   exx
                dec  b
                ld   c, b
                ld   b, 0
                add  hl, bc
                pop  bc
                dec  c
                jp   NZ, mainlp
                pop  hl
                ret
          unless (lastcol - shft4).to_i == (lastco2 - shft5).to_i and (shft4 - shft_4).to_i == (shft5 - shft_5).to_i
            raise Syntax, "#{lastcol - shft4} == #{lastco2 - shft5} and #{shft4 - shft_4} == #{shft5 - shft_5}"
          end
      end


      # hl' - screen
      #  e' - mask
      # ix  - vect
      # de  - sprite
      #  a  - height
      #  c  - width
      #  b  - skip
      macro :spriteclror8 do
        fstcopy ld   h, 0     # 7
                ld   l, b     # 4 skip first
                ex   de, hl   # 4
                ld   b, a     # 4 height
                exx           # 4
                jp   mainf0   # 10
        mainlpf exx           # 4
                pop  hl       # 10 screen addr
                inc  l        # 4
                ld   a, l     # 4
                anda 0x1f     # 7
                ret  Z        # 5 out of screen
        mainf0  push hl       # 11 save screen addr
                exx           # 4
                push bc       # 11 height + width
                add  hl, de   # 11 skip first
        copylp  ld   a, [hl]  # 7 bitmap
                inc  hl       # 6
                exx           # 4
                ld   [hl], a
                nextline h, l, quitf0, scraddr:sprite8_screen_address
                exx
                djnz copylp
                jp   quitf1
        quitf0  exx
                dec  b
                ld   c, b
                ld   b, 0
                add  hl, bc
        quitf1  pop  bc
                dec  c
                jp   NZ, mainlpf
                pop  hl
                ret

        lastco2 rlca
                rlca
                rlca
                ld   c, a
                anda e
                xor  c
                ld   d, a
                ld   a, e
                anda [hl]
                ora  d
                ld   [hl], a
                nextline h, l, quit0, scraddr:sprite8_screen_address
                exx
                djnz shftlp
                pop  bc
                dec  c
                jr   NZ, mainlp
                pop  hl
                ret
                nop

        shft5   rlca
        shft6   rlca
        shft7   rlca
                ld   c, a
                anda e
                ld   b, a
                xor  c
                ld   d, a
                ld   a, e
                anda [hl]
                ora  d
                ld   [hl], a
                inc  l
                ld   a, e
                cpl
                anda [hl]
                ora  b
                ld   [hl], a
                dec  l
                nextline h, l, quit0, scraddr:sprite8_screen_address
                exx
                djnz shftlp
                pop  bc
                dec  c
                jp   NZ, mainlp
                pop  hl
                ret
                nop

        shft_5  rlca
        shft_6  rlca
        shft_7  rlca
                anda e
                ld   b, a
                ld   a, e
                cpl
                anda [hl]
                ora  b
                ld   [hl], a
                nextline h, l, quit1, scraddr:sprite8_screen_address
                exx
                djnz shftlp
                jp   adjust1

        mainlp  exx
                pop  hl       # screen addr
                inc  l
                ld   a, l
                anda 0x1f
                ret  Z        # out of screen
        main0   push hl       # save screen addr
                add  0xe1     # 7  check if last screen column
                jp   NC, skiplst # 7/12 not last column
                ld   bc, (lastco2 - shft5).to_i # 10
                add  ix, bc   # 15
        skiplst exx
                push bc       # height + width
                add  hl, de   # skip first
        shftlp  ld   a, [hl]
                inc  hl
                exx
                jp   (ix)

        start   ld   h, 0
                ld   l, b     # skip first
                ex   de, hl
                ld   b, a     # height
                exx
                ld   a, l     # 4
                anda 0x1f     # 7
                jp   main0
                
        lastcol rrca
                rrca
                rrca
                rrca
                ld   c, a
                anda e
                xor  c
                ld   d, a
                ld   a, e
                anda [hl]
                ora  d
                ld   [hl], a
                nextline h, l, quit0, scraddr:sprite8_screen_address
                exx
                djnz shftlp
                pop  bc
                dec  c
                jr   NZ, mainlp
                pop  hl
                ret

        shft4   rrca
        shft3   rrca
        shft2   rrca
        shft1   rrca
                ld   c, a
                anda e
                ld   b, a
                xor  c
                ld   d, a
                ld   a, e
                anda [hl]
                ora  d
                ld   [hl], a
                inc  l
                ld   a, e
                cpl
                anda [hl]
                ora  b
                ld   [hl], a
                dec  l
                nextline h, l, quit0, scraddr:sprite8_screen_address
                exx
                djnz shftlp
                pop  bc
                dec  c
                jp   NZ, mainlp
                pop  hl
                ret

        shft_4  rrca
        shft_3  rrca
        shft_2  rrca
        shft_1  rrca
                anda e
                ld   b, a
                ld   a, e
                cpl
                anda [hl]
                ora  b
                ld   [hl], a
                nextline h, l, quit1, scraddr:sprite8_screen_address
                exx
                dec  b
                jp   NZ, shftlp
        adjust1 inc  b
                exx
        quit1   ld   bc, (shft4 - shft_4).to_i
                add  ix, bc
                pop  hl
                ex   [sp], hl   # screen addr
                exx
                dec  b
                ld   c, b
                ld   b, 0
                add  hl, bc
                pop  bc
                dec  c
                exx
                ret  Z
                push hl
                jp   skiplst
        quit0   exx
                dec  b
                ld   c, b
                ld   b, 0
                add  hl, bc
                pop  bc
                dec  c
                jp   NZ, mainlp
                pop  hl
                ret
          unless (lastcol - shft4).to_i == (lastco2 - shft5).to_i and (shft4 - shft_4).to_i == (shft5 - shft_5).to_i
            raise Syntax, "#{lastcol - shft4} == #{lastco2 - shft5} and #{shft4 - shft_4} == #{shft5 - shft_5}"
          end
      end

      if DRAW_METHOD_MASK_OR
                    spriteandor8 :spandor
        jumpandor   words [spandor.shft1, spandor.shftm1,   spandor.shft2, spandor.shftm2,  spandor.shft3, spandor.shftm3,
                           spandor.shft4,   spandor.shftm4, spandor.shft5,  spandor.shftm5,  spandor.shft6,  spandor.shftm6,  spandor.shft7, spandor.shftm7]
        jumpandor_1 words [spandor.shft_1, spandor.shftm_1,  spandor.shft_2, spandor.shftm_2,  spandor.shft_3, spandor.shftm_3,  
                           spandor.shft_4,  spandor.shftm_4,  spandor.shft_5, spandor.shftm_5,  spandor.shft_6, spandor.shftm_6,  spandor.shft_7,  spandor.shftm_7]
      end

      if DRAW_METHOD_SET
                    spriteclror8 :spclror
        jumpclror   words [spclror.shft1,   spclror.shft2,   spclror.shft3,   spclror.shft4,   spclror.shft5,   spclror.shft6,  spclror.shft7]
        jumpclror_1 words [spclror.shft_1,  spclror.shft_2,  spclror.shft_3,  spclror.shft_4,  spclror.shft_5,  spclror.shft_6,  spclror.shft_7]
      end

      if DRAW_METHOD_OR
                    spritexoror8 :spror, :ora
        jumpor      words [spror.shft1,   spror.shft2,   spror.shft3,   spror.shft4,   spror.shft5,   spror.shft6, spror.shft7]
        jumpor_1    words [spror.shft_1,  spror.shft_2,  spror.shft_3,  spror.shft_4,  spror.shft_5,  spror.shft_6,  spror.shft_7]
      end

      if DRAW_METHOD_XOR
                    spritexoror8 :sprxor, :xor
        jumpxor     words [sprxor.shft1,  sprxor.shft2,  sprxor.shft3,  sprxor.shft4,  sprxor.shft5,  sprxor.shft6, sprxor.shft7]
        jumpxor_1   words [sprxor.shft_1, sprxor.shft_2, sprxor.shft_3, sprxor.shft_4, sprxor.shft_5, sprxor.shft_6, sprxor.shft_7]
      end

      unless label_defined? :maskshift
        maskshift   bytes [0x00, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE]
      end
    end
  end
end

# DEPRECATED
ZXGfxSprite8 = ZXLib::Gfx::Sprite8 unless defined?(ZXGfxSprite8) # :nodoc:
