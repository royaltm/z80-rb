class ZXGfx8
    module Macros
        def adda_to(h, l) # adds a to rr, destroys a
            if h == l or [h,l].include?(a)
                raise ArgumentError, "xytoscr invalid arguments!"
            end
            ns do
                add  l
                ld   l, a
                adc  h
                sub  l
                ld   h, a
            end
        end
        #  advances to next screen line byte address using ah al registers
        #  (optionally) returns from subroutine if address goes out of screen area
        #  uses: a, ah, al
        #  ah: register  input/output: address high byte
        #  al: register  input/output: address low byte
        #  bcheck: boundary check flag
        #    false = disable checking
        #    true  = return if out of screen (default)
        #    label = jump to label if out of screen
        def nextline(ah, al, bcheck = true)
            ns do |eoc|
                inc  ah
                ld   a, ah
                anda 0x07
                jr   NZ,eoc
                ld   a, al
                add  0x20
                ld   al, a
                jr   C, bcheck ? over : eoc
                ld   a, ah
                sub  0x08
                ld   ah, a
                if bcheck
                    jp   eoc
            over    ld   a, ah
                    cp   0x58
                    if bcheck == true
                        ret  NC
                    else
                        jp   NC, bcheck
                    end
                end
            end
        end
        #  converts x,y coordinates to screen byte address and bits shift
        #  uses: a, y, x, s, t
        #  y: register  input: vertical-coordinate   (maybe a or same as h l s t)
        #  x: register  input: horizontal-coordinate (maybe same as l)
        #  h: register  output: address high
        #  l: register  output: address low
        #  s: register  output: bits shift
        #  t: register  temporary
        #
        #  y< a1 a2 h3 h2 h1 l3 l2 l1  x< x5 x4 x3 x2 x1 s3 s2 s1
        #  y> 0  1  0  a1 a2 l3 l2 l1  x> h3 h2 h1 x5 x4 x3 x2 x1  s> 0  0  0  0  0  s3 s2 s1
        def xytoscr(y, x, h, l, s, t)
            if y == x or [x,h,l,s,t].include?(a) or [h,l,s,t].uniq.size != 4 or [x,h,s,t].uniq.size != 4
                raise ArgumentError, "xytoscr invalid arguments!"
            end
            ns do
                if y == a 
                  ld   h, a
                else
                  ld   a, y
                end            # a= a a h h h l l l
                anda 0x07
                ld   s, a      # s= 0 0 0 0 0 l l l
                if y == a
                  xor  h
                else
                  xor  y       # a= a a h h h 0 0 0
                end
                rrca           # a= 0 a a h h h 0 0
                scf
                rra            # a= 1 0 a a h h h 0
                rrca
                ld   h, a      # h= 0 1 0 a a h h h
                anda 0x07
                ld   t, a      # t= 0 0 0 0 0 h h h
                xor  h         # a= 0 1 0 a a 0 0 0
                ora  s
                ld   h, a      # h= 0 1 0 a a l l l
                ld   a, x      # a= x x x x x s s s
                anda 0x07
                ld   s, a      # s= 0 0 0 0 0 s s s
                xor  x         # a= x x x x x 0 0 0
                ora  t         # a= x x x x x h h h
                3.times { rrca }
                ld   l, a      # l= h h h x x x x x
            end
        end
        #  converts 0,y coordinates to screen byte address
        #  uses: a, y, x, s, t
        #  y: register  input: vertical-coordinate (maybe same as h, t or l or even a)
        #  h: register  output: address high
        #  l: register  output: address low
        #  t: register  temporary
        #
        #  y< a1 a2 h3 h2 h1 l3 l2 l1
        #  y> 0  1  0  a1 a2 l3 l2 l1  x> h3 h2 h1 0  0  0  0  0
        def ytoscr(y, h, l, t)
            if [h,l,t].include?(a) or [h,l,t].uniq.size != 3
                raise ArgumentError, "ytoscr invalid arguments!"
            end
            ns do
                if y == a
                  ld   l, a
                else
                  ld   a, y
                end           # a= a a h h h l l l
                anda 0x07
                ld   t, a     # h= 0 0 0 0 0 l l l
                if y == a
                  xor  l
                else
                  xor  y       # a= a a h h h 0 0 0
                end
                rlca          # a= a h h h 0 0 0 a
                rlca          # a= h h h 0 0 0 a a
                ld   h, a     # b= h h h 0 0 0 a a
                anda 0xE0
                ld   l, a     # l= h h h 0 0 0 0 0
                xor  h        # a= 0 0 0 0 0 0 a a
                3.times { rlca }
                ora  t        # a= 0 0 0 a a l l l
                ora  0x40     # a= 0 1 0 a a l l l
                ld   h, a     # h= 0 1 0 a a l l l
            end
        end
    end
    include Z80

    # hl' - screen
    #  e' - mask
    # ix  - vectmask
    # iy  - vectbmap
    # de  - sprite
    #  a  - height
    #  c  - width
    #  b  - skip
    macro :spriteandor8 do
      shft4   rrca
      shft3   rrca
      shft2   rrca
      shft1   rrca
              ld   d, a
              anda e
              xor  d
              ora  c
              ld   [hl], a
              inc  l
              ld   a, l
              anda 0x1f
              jr   Z, nslin1
              ld   a, d
              anda e
              ld   d, a
              ld   a, b
              cpl
              anda [hl]
              ora  d
              ld   [hl], a
      nslin1  dec  l
              nextline h, l, quit0
              exx
              djnz shftlp
              pop  bc
              dec  c
              jr   NZ, mainlp
              pop  hl
              ret
      shft_4  rrca
      shft_3  rrca
      shft_2  rrca
      shft_1  rrca
              anda e
              ora  c
              ld   [hl], a
              nextline h, l, quit1
              exx
              djnz shftlp
              jp   adjust1
      shft0   cpl
              anda [hl]
              ld   c, a
              ex   af, af #bitmap
              ora  c
              ld   [hl], a
              nextline h, l, quit0
              exx
              djnz shftlp
              jp   quit01

      start   ld   h, 0
              ld   l, b     # skip first
              add  hl, hl
              ex   de, hl
              sub  b
              ld   b, a     # height
              exx
              jp   main0
      mainlp  exx
              pop  hl       # screen addr
              inc  l
              ld   a, l
              anda 0x1f
              ret  Z        # out of screen
      main0   push hl       # save screen addr
              exx
              push bc       # height + width
              add  hl, de   # skip first
      shftlp  ld   a, [hl]  # bitmap
              inc  hl
              ex   af, af
              ld   a, [hl]  # mask
              inc  hl
              exx
              jp   (ix)
      shft5   rlca
      shft6   rlca
      shft7   rlca
              ld   d, a
              anda e
              xor  d
              ora  c
              ld   [hl], a
              inc  l
              ld   a, l
              anda 0x1f
              jr   Z, nslin2
              ld   a, d
              anda e
              ld   d, a
              ld   a, b
              cpl
              anda [hl]
              ora  d
              ld   [hl], a
      nslin2  dec  l
              nextline h, l, quit0
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
              ora  c
              ld   [hl], a
              nextline h, l, quit1
              exx
              djnz shftlp
              jp   adjust1
      shftm4  rrca
      shftm3  rrca
      shftm2  rrca
      shftm1  rrca
              ld   c, a
              anda e
              ld   b, a
              xor  c
              cpl
              anda [hl]
              ld   c, a
              ex   af, af # bitmap
              jp   (iy)
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
              jp   NZ, main0
              ret
      quit0   exx
              dec  b
              ld   c, b
              ld   b, 0
              add  hl, bc
              add  hl, bc
      quit01  pop  bc
              dec  c
              jp   NZ, mainlp
              pop  hl
              ret
      end


    # hl' - screen
    #  e' - mask
    # ix  - vect
    # de  - sprite
    #  a  - height
    #  c  - width
    #  b  - skip
    # type = :xor, :or
    macro :spritexoror8 do |_, type|
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
              ld   a, l
              anda 0x1f
              jr   Z, nslin2
              ld   a, b
              self.send type, [hl]
              ld  [hl], a
      nslin2  dec  l
              nextline h, l, quit0
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
              nextline h, l, quit1
              exx
              djnz shftlp
              jp   adjust1
      shft0   self.send type, [hl]
              ld   [hl], a
              nextline h, l, quit0
              exx
              djnz shftlp
              pop  bc
              dec  c
              jr   NZ, mainlp
              pop  hl
              ret

      start   ld   h, 0
              ld   l, b     # skip first
              ex   de, hl
              sub  b
              ld   b, a     # height
              exx
              jp   main0
      mainlp  exx
              pop  hl       # screen addr
              inc  l
              ld   a, l
              anda 0x1f
              ret  Z        # out of screen
      main0   push hl       # save screen addr
              exx
              push bc       # height + width
              add  hl, de   # skip first
      shftlp  ld   a, [hl]
              inc  hl
              exx
              jp   (ix)
      shft4   rrca
      shft3   rrca
      shft2   rrca
      shft1   rrca
              ld   c, a
              anda e
              ld   b, a
              xor  c
              self.send type, [hl]
              ld   [hl], a
              inc  l
              ld   a, l
              anda 0x1f
              jr   Z, nslin1
              ld   a, b
              self.send type, [hl]
              ld  [hl], a
      nslin1  dec  l
              nextline h, l, quit0
              exx
              djnz shftlp
              pop  bc
              dec  c
              jr   NZ, mainlp
              pop  hl
              ret
      shft_4  rrca
      shft_3  rrca
      shft_2  rrca
      shft_1  rrca
              anda e
              self.send type, [hl]
              ld   [hl], a
              nextline h, l, quit1
              exx
              djnz shftlp
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
              jp   NZ, main0
              ret
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
    end

    # hl' - screen
    #  e' - mask
    # ix  - vect
    # de  - sprite
    #  a  - height
    #  c  - width
    #  b  - skip
    macro :spriteclror8 do
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
              ld   a, l
              anda 0x1f
              jr   Z, nslin2
              ld   a, e
              cpl
              anda [hl]
              ora  b
              ld   [hl], a
      nslin2  dec  l
              nextline h, l, quit0
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
              ld   b, a
              ld   a, e
              cpl
              anda [hl]
              ora  b
              ld   [hl], a
              nextline h, l, quit1
              exx
              djnz shftlp
              jp   adjust1
      shft0   ld   d, a
              ld   a, e
              anda [hl]
              ora  d
              ld   [hl], a
              nextline h, l, quit0
              exx
              djnz shftlp
              pop  bc
              dec  c
              jr   NZ, mainlp
              pop  hl
              ret

      start   ld   h, 0
              ld   l, b     # skip first
              ex   de, hl
              sub  b
              ld   b, a     # height
              exx
              jp   main0
      mainlp  exx
              pop  hl       # screen addr
              inc  l
              ld   a, l
              anda 0x1f
              ret  Z        # out of screen
      main0   push hl       # save screen addr
              exx
              push bc       # height + width
              add  hl, de   # skip first
      shftlp  ld   a, [hl]
              inc  hl
              exx
              jp   (ix)
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
              ld   a, l
              anda 0x1f
              jr   Z, nslin1
              ld   a, e
              cpl
              anda [hl]
              ora  b
              ld   [hl], a
      nslin1  dec  l
              nextline h, l, quit0
              exx
              djnz shftlp
              pop  bc
              dec  c
              jr   NZ, mainlp
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
              nextline h, l, quit1
              exx
              djnz shftlp
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
              jp   NZ, main0
              ret
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
    end

    #  uses: a b c d e h l ix a' b' c' d' e' h' l' (1 stack)
    #  hl:  sprite address
    #  a:   input: sprite height (1..192)
    #  a`:  input: sprite width in bytes (1..32)
    #  CF:  input: 0: xor mode, 1: and+or mode, 0: clear mode, 1: or mode
    #  ZF:  input: 1:                         , 0:
    #  bc:  x - coordinate (-32768..32767)
    #  de:  y - coordinate (-32768..32767)
    export    draw_sprite8_coords
    ns :draw_sprite8_coords do
            ex   af, af       # store CF and sprite height
            push af           # sprite width
            ld   a, d
            ora  a
            jp   Z, vnext1    # 0 <= de < 256
            inc  a            # d == 0xff
            jr   NZ, quit1    # de < -256
            ld   a, e
            neg
            jr   Z, quit1     # de == -256
            ld   d, a         # skip
            ld   e, 0         # y = 0
            jp   hnext1
    quit1   pop  af
            ret
    vnext1  ld   a, e         # 0 <= de < 192
            cp   192
            jr  NC, quit1     # de >= 192
    hnext1  ld   a, b
            ora  a            # 0<= bc < 256
            jp   Z, hnext2    # bc on screen
            inc  a
            jr   NZ, quit1    # bc < -256

            ld   a, c
            neg               # x = -x
            jr   Z, quit1     # x == -256
            anda 0xf8
            jr   Z, fskip     # -8 < x < 0
            rrca
            rrca
            rrca
            ld   b, a         # -x / 8
            pop  af           # sprite width
            sub  b            # width -= -x / 8
            ret  Z            # width == -x / 8
            ret  C            # width <  -x / 8

            ex   af, af       # new width
            push af           # height + CZ
            push de
            ld   d, 0
            ld   e, a         # height
            jr   NC, muls1
            jr   NZ, muls1
      loopm sla  e            # height*2 (andor) C=1 Z=1
            rl   d
      muls1 srl  b            # -x / 8
            jr  NC, noadd
            add hl, de        # sprite + height * (-x/8)
      noadd jr  NZ, loopm
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
    # HOWTO:
    # cp a  -> z1 c0 (xor)
    # cp a
    # scf   -> z1 c1 (and + or)
    # xor a
    # inc a -> z0 c0 (or)
    # scf
    # sbc a -> z0 c1 (clear + or)
    # callback draws on screen using xor 8-bit wide sprite with arbitrary height
    # uses: a b c d e h l ix iy a' b' c' d' e' h' l' (no stack)
    # de: sprite address
    # h:  input: vertical coordinate   (0..191)
    # l:  input: horizontal coordinate (0..255)
    # except when h > 191 then l contains vertical coordinate and h | 0xf8 is a negative h-coordinate (from -1 to -7)
    # a':  input: sprite height (1..192)
    # b:   input: skip first sprite lines
    # c:   input: sprite byte width
    # CF': input: 0: xor mode, 1: and+or mode, 0: or mode, 1: clear mode
    # ZF': input: 1:                         , 0:
    # in and+or mode sprite bitamp bytes must be interwined with mask: b1 m1 b2 m2 b2 m2 .....
    export  draw_sprite8
    ns :draw_sprite8 do
            push bc
            ld   a, h
            cp   192
            jp   C, skipneg
            anda 0x07
            jr   Z, skipad      # sanity check
            add  7              # negative vect
    skipad  ld   c, a           # negshift
            ytoscr l, h, l, b
            jp   skippos
    skipneg xytoscr h, l, h, l, c, b # hl -> yx, hl -> screen, c -> shift
    skippos push hl             # screen addr
 
            ld   a, c           # shift
            ld   hl, maskshift
            adda_to h, l
            ld   a, [hl]
            exx
            ld   e, a           # rotate mask
            pop  hl             # screen addr
            exx
            ex   af, af
            jr   C, aocskip
            jr   NZ, orskip

            ld   hl, sprxor.start
            push hl             # jump addr
            ld   hl, jumpxor
            jp   skipall

     orskip ld   hl, spror.start
            push hl             # jump addr
            ld   hl, jumpor
            jp   skipall

    aocskip jr   Z,  andskip
            ld   hl, spclror.start
            push hl             # jump addr
            ld   hl, jumpclror
            jp   skipall

    andskip ld   hl, spandor.start
            push hl             # jump addr
            ld   b, a           # height
            ld   a, c
            ld   hl, jumpandor
            add  a
            add  a
            adda_to h, l
            ld   a, [hl]
            ld   iyl, a
            inc  hl
            ld   a, [hl]
            ld   iyh, a
            inc  hl
            jp   skipal2

    skipall ld   b, a           # height
            ld   a, c           # shift
            add  a
            adda_to h, l
    skipal2 ld   a, [hl]
            ld   ixl, a
            inc  hl
            ld   a, [hl]
            ld   ixh, a
            pop  hl             # jump addr
            ld   a, b           # height
            pop  bc             # skip + width
            jp   (hl)
    end

            spriteandor8 :spandor
            spriteclror8 :spclror
            spritexoror8 :spror, :ora
            spritexoror8 :sprxor, :xor

    maskshift   data 1, [0x00, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE]
    jumpandor   data 2, [spandor.shft0, spandor.shft0,   spandor.shft1, spandor.shftm1,   spandor.shft2, spandor.shftm2,  spandor.shft3, spandor.shftm3,
       spandor.shft4,   spandor.shftm4, spandor.shft5,  spandor.shftm5,  spandor.shft6,  spandor.shftm6,  spandor.shft7, spandor.shftm7]
    jumpandor_1 data 2, [spandor.shft_1, spandor.shftm_1,  spandor.shft_2, spandor.shftm_2,  spandor.shft_3, spandor.shftm_3,  
       spandor.shft_4,  spandor.shftm_4,  spandor.shft_5, spandor.shftm_5,  spandor.shft_6, spandor.shftm_6,  spandor.shft_7,  spandor.shftm_7]
    jumpclror   data 2, [spclror.shft0,   spclror.shft1,   spclror.shft2,   spclror.shft3,   spclror.shft4,   spclror.shft5,   spclror.shft6,  spclror.shft7]
    jumpclror_1 data 2, [spclror.shft_1,  spclror.shft_2,  spclror.shft_3,  spclror.shft_4,  spclror.shft_5,  spclror.shft_6,  spclror.shft_7]
    jumpor      data 2, [spror.shft0,   spror.shft1,   spror.shft2,   spror.shft3,   spror.shft4,   spror.shft5,   spror.shft6, spror.shft7]
    jumpor_1    data 2, [spror.shft_1,  spror.shft_2,  spror.shft_3,  spror.shft_4,  spror.shft_5,  spror.shft_6,  spror.shft_7]
    jumpxor     data 2, [sprxor.shft0,  sprxor.shft1,  sprxor.shft2,  sprxor.shft3,  sprxor.shft4,  sprxor.shft5,  sprxor.shft6, sprxor.shft7]
    jumpxor_1   data 2, [sprxor.shft_1, sprxor.shft_2, sprxor.shft_3, sprxor.shft_4, sprxor.shft_5, sprxor.shft_6, sprxor.shft_7]
end
