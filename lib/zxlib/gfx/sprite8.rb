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
    #  define a ZXLib::Gfx::Sprite8::DRAW_METHODS constant before requiring this file.
    #
    #      module ZXLib
    #        module Gfx
    #          class Sprite8
    #            DRAW_METHODS = [:xor]
    #          end
    #        end
    #      end
    #      require 'zxlib/gfx/sprite8'
    #
    class Sprite8
      include Z80 unless defined?(Program)

      macro_import MathInt
      macro_import ZXLib::Gfx

      export  draw_sprite8

      ##
      # A default screen address used by the routines.
      # You may change this value by overriding label +sprite8_screen_address+ when importing Sprite8 code.
      SCREEN_ADDRESS = 0x4000

      ##
      # Array of supported drawing methods: +:xor+, +:or+, +:set+, +:mask_or+
      DRAW_METHODS        = [:xor, :or, :set, :mask_or] unless const_defined?(:DRAW_METHODS)

      DRAW_METHOD_XOR     = DRAW_METHODS.include?(:xor) unless const_defined?(:DRAW_METHOD_XOR)
      DRAW_METHOD_OR      = DRAW_METHODS.include?(:or) unless const_defined?(:DRAW_METHOD_OR)
      DRAW_METHOD_SET     = DRAW_METHODS.include?(:set) unless const_defined?(:DRAW_METHOD_SET)
      DRAW_METHOD_MASK_OR = DRAW_METHODS.include?(:mask_or) unless const_defined?(:DRAW_METHOD_MASK_OR)

      ##
      # Configures the method in which draw_sprite8 calculates the screen address from pixel coordinates.
      #
      # The routine is 60 bytes in size and can be set up in one of the following ways:
      #
      # * +:inline+:: The calculation is inline which is faster and backwards compatible but the calculation
      #               routine can't be re-used by Macros.gfx_sprite8_draw macro.
      # * +:subroutine+:: The calculation is set up as a subroutine, which can be reached via +draw_sprite8.calc_scr_addr+
      #                   label. See also Macros.gfx_sprite8_calculate_screen_address.
      # * +:external+:: The calculation routine is _NOT_ created. In this instance the only way to call +draw_sprite8+
      #                 is via the Macros.gfx_sprite8_draw macro.
      CALCULATE_SCREEN_ADDRESS = :inline unless const_defined?(:CALCULATE_SCREEN_ADDRESS)

      ## Controls if draw_sprite8 should check if the sprite pixel height is larger than "top lines to skip" parameter.
      CHECK_HEIGHT_SANITY = true unless const_defined?(:CHECK_HEIGHT_SANITY)

      ##
      # ==ZXLib::Gfx::Sprite8 Macros.
      #
      # Sprite8::Macros require:
      #
      #    macro_import MathInt
      #    macro_import Gfx
      module Macros
        ##
        # Creates a routine that calculates the screen address for Sprite8.draw_sprite8.
        #
        # The +h+ and +l+ registers should contain the pixel coordinates as described in Sprite8.draw_sprite8.
        #
        # As a result of executing the routine the +hl+ registers will hold the calculated screen address
        # and the +c+ register will hold the special bit right shift number.
        #
        # If the horizontal pixel coordinate (x) is positive the bit shift will be between 0 and 7.
        # If the x coordinate is negative and x is between -7 and -1 the bit shift will be between 8 and 14.
        #
        # Options:
        # * +scraddr+:: A screen memory address which must be a multiple of 0x2000 as an integer or a label.
        # * +subroutine+:: Whether to create a subroutine.
        def gfx_sprite8_calculate_screen_address(scraddr:SCREEN_ADDRESS, subroutine:false)
          isolate do |eoc|
                        ld   a, h
                        cp   192
                        jp   C, hvertical
                        anda 7              # -7..-1 -> 1..7
                        jr   Z, noadjust    # sanity check
                        add  7              # 1..7 -> 8..14
            noadjust    ld   c, a           # C: negshift (0, 8..14)
                        ytoscr l, ah:h, al:l, t:b, scraddr:scraddr
            if subroutine
                        ret
            else
                        jr   eoc
            end                           # HL<: yx, HL>: screen, C>: shift (0..7), B: temp
            hvertical   xytoscr h, l, ah:h, al:l, s:c, t:b, scraddr:scraddr
                        ret if subroutine
          end
        end
        ##
        # Creates a subroutine that calculates the screen address before jumping to Sprite8.draw_sprite8.
        #
        # This subroutine should be used if you want to access the calculated screen address just before
        # executing +draw_sprite8+.
        #
        # +draw_sprite8+:: A label addressing the Sprite8.draw_sprite8 subroutine.
        # +block+:: A block that creates a code to execute when the screen address has been calculated.
        #           The address is available in +hl+ registers. Additionally the +c+ register contains
        #           the special bit shift number. See gfx_sprite8_calculate_screen_address for details.
        #           The code must preserve the content of +c+, +de+ and +af'+ registers.
        #
        # See Sprite8.draw_sprite8 for the description of input registers and usage.
        #
        # Options:
        # * +scraddr+:: A screen memory address which must be a multiple of 0x2000 as an integer or a label.
        # * +calculate+:: If this option is set to +:subroutine+ then the screen address calculation routine
        #                 is being called (at +draw_sprite8.calc_scr_addr+) instead of inlining it.
        #                 In this instance the Sprite8::CALCULATE_SCREEN_ADDRESS constant must be set to +:subroutine+
        #                 before requiring the +sprite8+ module.
        #
        # Any other option is being passed over to the +block+ namespace.
        def gfx_sprite8_draw(draw_sprite8=self.draw_sprite8, scraddr:SCREEN_ADDRESS, calculate:CALCULATE_SCREEN_ADDRESS, **nsopts, &block)
          isolate do |eoc|
                        push bc             # save width and skip
            if calculate == :subroutine
                        call draw_sprite8.calc_scr_addr
            else
                        gfx_sprite8_calculate_screen_address(scraddr:scraddr)
            end
                        push hl             # HL: screen addr, C: negshift (0..14)
            if block_given?
                        ns(**nsopts) do
                          yield eoc
                        end
            end
                        jp   draw_sprite8.addr_on_stack
          end
        end
        ##
        # Creates a routine that calculates coordinates and prepares registers for Sprite8.draw_sprite8.
        #
        # +hl+:: An address of sprite data.
        # +a+::  A height of a sprite in pixel lines: [1, 192].
        # +a'+:: A width of a sprite in bytes ((pixel width + 7) / 8): [1, 32].
        # +bc+:: A horizontal (x) coordinate of a sprite's top-leftmost pixel as a 16-bit twos complement
        #        signed integer: [-32768, 32767] where the screen area is between: [0, 255].
        # +de+:: A vertical (y) coordinate of a sprite's top-leftmost pixel as a 16-bit twos complement
        #        signed integer: [-32768..32767] where the screen area is between: [0, 191].
        # +f+::  Flags specifying a drawing method (see below).
        #
        # Options:
        # * +outofscreen+:: What to do if the whole sprite is out of the screen area - if no +block+ is given then provide
        #                   a branching +label+, otherwise +ret+ is being executed.
        # * +block+:: Should create code to execute when the whole sprite is out of the screen area. The +eoc+ label
        #             provided to the +block+ points after the calculating routine. The code must not fall through!
        #
        # Any other option is being passed over to the +block+ namespace.
        #
        # Drawing methods:
        #
        #       mode:    OR    SET     XOR   AND+OR
        #         CF:     0      1       0        1
        #         ZF:     0      0       1        1
        #     assuming accumulator contains the non-zero number of sprite lines
        #     how to: ora a  ora a    cp a     cp a
        #                      scf              scf
        #
        # See Sprite8.draw_sprite8 for the description of output registers.
        #
        # _NOTE_:: The +outofscreen+ is invoked only when it would be impossible to formulate valid arguments
        #          for Sprite8.draw_sprite8, which is exactly when
        #          <tt>(x > 255) or (x + pixel width <= 0) or (y > 191) or (y < -255)</tt>.
        #
        # Uses: +af+, +af'+, +bc+, +de+, +hl+, stack: max 4 bytes.
        def gfx_sprite8_calculate_coords(outofscreen: :ret, **nsopts, &block)
          isolate do |eoc|
                    ex   af, af       # store CF and sprite height
                    push af           # sprite width
                    ld   a, d
                    ora  a
                    jp   Z, vnext1    # 0 <= de < 256
                    inc  a            # d == 0xff
                    jr   NZ, quit1    # de < -256
                    # xor  a          # a is already 0
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
        ##
        # Creates a routine that flips sprite pixel data horizontally (mirrors sprites).
        #
        # +hl+:: An address immediately following the source sprite data (sprite data address + sprite data size).
        # +de+:: A target address where the mirrored sprite data should be placed. The target memory area must not
        #        overlap with the source.
        # +c+::  A width of a sprite in bytes ((pixel width + 7) / 8) (0 is 256).
        # +b+::  A height of a sprite in pixel lines (0 is 256).
        #
        # Options:
        # * +subroutine+:: Whether to create a subroutine.
        #
        # After the routine finishes its operation:
        #
        # * +de+:: Will hold a memory address immediately following the flipped sprite data
        #          (flipped sprite data address + sprite data size).
        # * +hl+:: Will hold a memory address of the source sprite data.
        # * +b+::  Will hold a provided sprite height in pixel lines.
        # * +c+::  Will be 0.
        #
        # _NOTE_:: Sprite data must be laid out column-wise as expected by Sprite8.draw_sprite8.
        #          If data include the sprite mask the provided height should be twice the sprite height.
        #          In this instance the maximum sprite height to be mirrored is 128 pixel lines.
        #
        # Uses: +af+, +bc+, +de+, +hl+, stack: 4 bytes.
        def gfx_sprite8_flip_horizontally(subroutine:false)
          isolate do
            column_loop   push bc
                          sub_from b, h, l
                          push hl
                          scf
            row_loop      ld   c, [hl]
                          inc  hl
                          rl   c
            bit_swap_loop rra
                          sla  c
                          jr   NZ, bit_swap_loop
                          ld   [de], a
                          inc  de
                          djnz row_loop
                          pop  hl
                          pop  bc
                          dec  c
                          jr   NZ, column_loop
                          ret if subroutine
          end
        end
      end

      extend Macros

      sprite8_screen_address addr SCREEN_ADDRESS

      ##
      # Draws a sprite using one of the selected drawing methods with an arbitrary pixel height and width.
      #
      # Pixel data for sprites must be laid column-wise:
      #   1st 8-pixel column bytes
      #   2nd 8-pixel column bytes
      #   ...
      #
      # Example:
      #
      #   A sprite 16x2 pixels:
      #
      #    7 6 5 4 3 2 1 0 7 6 5 4 3 2 1 0
      #   ░░░░████████░░░░██░░░░░░░░░░░░██ 0
      #   ░░██░░░░░░░░██░░████████████████ 1
      #
      #   db    0b00111100, # 1st column
      #         0b01000010,
      #         
      #         0b10000001, # 2nd column
      #         0b11111111
      #
      # In an <tt>AND+OR</tt> (a.k.a. +MASK_OR+) mode bytes representing shape of a sprite must be intertwined with sprite's mask bytes:
      #   1st bitmap byte
      #   1st mask byte
      #   2nd bitmap byte
      #   2nd mask byte
      #   ...
      #
      # This routine is optimised for vertical sprites with pixel height > width and square ones with width <= 24 pixels.
      #
      # +de+:: An address of sprite data to be drawn.
      # +h+::  A screen vertical (y) coordinate of a sprite's top-leftmost pixel: [0, 191].
      # +l+::  A screen horizontal (x) coordinate of a sprite's top-leftmost pixel: [0, 255].
      # +b+::  How many pixel lines to skip from the sprite's top.
      # +c+::  A width of a sprite in bytes (columns) ((pixel width + 7) / 8): [1, 32].
      # +a'+:: A height of a sprite in pixel lines: [1, 192].
      # +f'+:: Flags specifying a drawing method (see below).
      #
      # To specify a negative vertical coordinate (y < 0) set +h+ (or +l+ see below) to +0+ and +b+ to
      # an absolute value of y.
      #
      # To specify a negative horizontal coordinate (x < 0):
      # * +de+ should point to the remaining visible sprite columns,
      # * +c+ should contain the number of visible sprite columns,
      # * if x modulo 8 is between -1 and -7 then +h+ should contain x modulo 8 as a twos complement negative number
      #   and +l+ should contain a vertical coordinate (y) instead.
      #
      # Drawing methods:
      #
      #       mode:    OR    SET     XOR   AND+OR
      #         CF:     0      1       0        1
      #         ZF:     0      0       1        1
      #     how to: ora 1    scf    cp a     cp a
      #                    sbc a              scf
      #
      # Uses:: +af+, +af'+, +bc+, +de+, +hl+, +bc'+, +de'+, +hl'+, +ix+, stack: max 6 bytes,
      #        in AND+OR mode also +iy+.
      ns :draw_sprite8 do
        unless CALCULATE_SCREEN_ADDRESS == :external
                      push bc             # save width and skip
          case CALCULATE_SCREEN_ADDRESS
          when :subroutine
                      call calc_scr_addr
          else
                      gfx_sprite8_calculate_screen_address(scraddr:sprite8_screen_address)
          end
          skip_addr   push hl             # HL: screen addr
                                          # C: shift 0..14
        end
        addr_on_stack ld   hl, maskshift

        select((maskshift + 14) & 0xFF){|m| m < 14 }.then do |_|
                      ld   b, 0
                      add  hl, bc         # HL: -> (maskshift + shift)
        end.else do
                      ld   a, c
                      add  l
                      ld   l, a           # HL: -> (maskshift + shift)
        end
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
            fastxor   ld   hl, sprxor.fstcopy
                      jp   skpfast
          end

          if DRAW_METHOD_OR
            orskip    add  a
                      jr   Z, fastor
                      ld   hl, spror.start
                      push hl             # jump addr
                      ld   hl, jumpor - 2
                      jp   skipall
            fastor    ld   hl, spror.fstcopy
                      jp   skpfast
          end
        end

        if DRAW_METHOD_SET and DRAW_METHOD_MASK_OR
          aocskip     jr   Z, andskip
        elsif DRAW_METHOD_SET or DRAW_METHOD_MASK_OR
          aocskip     label
        end

        if DRAW_METHOD_SET
                      add  a
                      jr   Z, fastclr
                      ld   hl, spclror.start
                      push hl             # jump addr
                      ld   hl, jumpclror - 2
                      jp   skipall
          fastclr     ld   hl, spclror.fstcopy
                      jp   skpfast
        end

        if DRAW_METHOD_MASK_OR
          andskip     ld   hl, spandor.start
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
          fastaor     ld   hl, spandor.fstcopy
                      jp   skpfast
        end

        if DRAW_METHOD_XOR or DRAW_METHOD_OR or DRAW_METHOD_SET
          skipall     adda_to h, l
        end
        skipal2       ld   a, [hl]
                      ld   ixl, a
                      inc  hl
                      ld   a, [hl]
                      ld   ixh, a
                      pop  hl             # jump addr
        skpfast       ld   a, b           # height
                      pop  bc             # skip + width
                      sub  b              # height - skip top lines
        if CHECK_HEIGHT_SANITY
                      ret  C              # return if skip top > height
                      ret  Z              # return if skip top == height
        end
                      jp   (hl)

        if CALCULATE_SCREEN_ADDRESS == :subroutine
          calc_scr_addr gfx_sprite8_calculate_screen_address(scraddr:sprite8_screen_address, subroutine:true)
        end
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
                ld   bc, lastcol - shft4 # 10
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
        export maskshift
        maskshift   bytes [0x00, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE]
      end
    end
  end
end

# DEPRECATED
ZXGfxSprite8 = ZXLib::Gfx::Sprite8 unless defined?(ZXGfxSprite8) # :nodoc:
