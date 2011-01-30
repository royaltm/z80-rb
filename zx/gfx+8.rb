require 'z80/math_i.rb'
require 'zx/gfx.rb'
class ZXGfxMore
  include Z80 unless defined?(Program)
  import ZXGfx, :code => false, :labels => false, :macros => true
  import Z80MathInt, :code => false, :labels => false, :macros => true

  #  calculates coordinates and prepares registers for draw_sprite8 and other compatible gfx functions
  #  uses: a af' b c d e h l ix (stack)
  #  hl: input: sprite address
  #  a:  input: sprite height (1..192)
  #  a`: input: sprite width in bytes (1..32)
  #  bc: input: x - coordinate (-32768..32767) [screen area: 0-255]
  #  de: input: y - coordinate (-32768..32767) [screen area: 0-191]
  #  f:  input: flags -> f'
  #  ix: input: jump to ix
  #  CF: input: 0: xor mode, 1: and+or mode, 0: clear mode, 1: or mode
  #  ZF: input: 1:                         , 0:
  #      when CF=ZF=1 sprite address skip bytes from left margin calculations take mask into account
  #  de: output: sprite address
  #  h:  output: vertical coordinate   (0..191)
  #  l:  output: horizontal coordinate (0..255)
  #  except when h > 191 then l contains vertical coordinate and h | 0xf8 is a negative h-coordinate (from -1 to -7)
  #  b:  output: skip first sprite lines
  #  c:  output: sprite byte width
  #  a': output: sprite height (1..192)
  #  f': output: preserved f
  export    calculate_coords_jump8
  ns :calculate_coords_jump8 do
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
          ret
  vnext1  ld   a, e         # 0 <= de < 192
          cp   192
          jr  NC, quit1     # de >= 192
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
          ret  C            # width <  -x / 8
          ret  Z            # width == -x / 8

          ex   af, af       # new width
          push af           # height + CZ
          push de
          ld   d, 0
          ld   e, a         # height
          jr   NC, mulh.muls1
          jr   NZ, mulh.muls1 # height*2 (andor) C=1 Z=1
    mulh  mul8(d, e, b, d, e, false, true) # sprite address+= height * (-x / 8)
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
          jp   (ix)
  end
  
  #  HOWTO:
  #  cp a  -> z1 c0 (xor)
  #  cp a
  #  scf   -> z1 c1 (and + or)
  #  xor a
  #  inc a -> z0 c0 (or)
  #  scf
  #  sbc a -> z0 c1 (clear + or)
  #
  #  draws on screen using xor/or/clear/and+or 8-bit wide*n sprite with arbitrary height and width
  #  in and+or mode sprite bitamp bytes must be interwined with mask: b1 m1 b2 m2 b2 m2 .....
  #  the data for sprites is laid verticaly: first column, second column, ...
  #  optimised for height > width sprites and small square ones (size < 24 pixels)
  #  uses: all registers (stack)
  #  de:  input: sprite address
  #  h:   input: vertical coordinate   (0..191)
  #  l:   input: horizontal coordinate (0..255)
  #  except when h > 191 then l contains vertical coordinate and h | 0xf8 is a negative h-coordinate (from -1 to -7)
  #  b:   input: skip first sprite lines
  #  c:   input: sprite byte width
  #  a':  input: sprite height (1..192)
  #  CF': input: 0: xor mode, 1: and+or mode, 0: or mode, 1: clear mode
  #  ZF': input: 1:                         , 0:
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
          ex   af, af         # height + CZ
          ld   b, a           # height
          ld   a, c           # shift
          jr   C, aocskip
          jr   NZ, orskip

          add  a
          jr   Z, fastxor
          ld   hl, sprxor.start
          push hl             # jump addr
          ld   hl, jumpxor - 2
          jp   skipall
  fastxor ld   hl, sprxor.fstcopy
          jp   skpfast

  orskip  add  a
          jr   Z, fastor
          ld   hl, spror.start
          push hl             # jump addr
          ld   hl, jumpor - 2
          jp   skipall
  fastor  ld   hl, spror.fstcopy
          jp   skpfast

  aocskip jr   Z, andskip
          add  a
          jr   Z, fastclr
          ld   hl, spclror.start
          push hl             # jump addr
          ld   hl, jumpclror - 2
          jp   skipall
  fastclr ld   hl, spclror.fstcopy
          jp   skpfast

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
  skipall adda_to h, l
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
            nextline h, l, quitf0
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
            nextline h, l, quit0 #27 /49 /59
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
            nextline h, l, quit0 #27 /49 /59
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
            nextline h, l, quit1
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
            nextline h, l, quit0 #27 /49 /59
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
            nextline h, l, quit0
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
            nextline h, l, quit1
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
            nextline h, l, quitf0
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
            nextline h, l, quit0
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
            self.send type, [hl]
            ld   [hl], a
            inc  l
            ld   a, b
            self.send type, [hl]
            ld  [hl], a
            dec  l
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
            nextline h, l, quit0 # 27
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
            nextline h, l, quit0 # 27
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
            nextline h, l, quit1
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
            nextline h, l, quitf0
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
            nextline h, l, quit0
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
            nextline h, l, quit0
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
            nextline h, l, quit1
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
            nextline h, l, quit0
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
            nextline h, l, quit0
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
            nextline h, l, quit1
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

          spriteandor8 :spandor
          spriteclror8 :spclror
          spritexoror8 :spror, :ora
          spritexoror8 :sprxor, :xor

  unless label_defined? :maskshift
    maskshift   bytes [0x00, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE]
  end

  jumpandor   words [spandor.shft1, spandor.shftm1,   spandor.shft2, spandor.shftm2,  spandor.shft3, spandor.shftm3,
     spandor.shft4,   spandor.shftm4, spandor.shft5,  spandor.shftm5,  spandor.shft6,  spandor.shftm6,  spandor.shft7, spandor.shftm7]
  jumpandor_1 words [spandor.shft_1, spandor.shftm_1,  spandor.shft_2, spandor.shftm_2,  spandor.shft_3, spandor.shftm_3,  
     spandor.shft_4,  spandor.shftm_4,  spandor.shft_5, spandor.shftm_5,  spandor.shft_6, spandor.shftm_6,  spandor.shft_7,  spandor.shftm_7]
  jumpclror   words [spclror.shft1,   spclror.shft2,   spclror.shft3,   spclror.shft4,   spclror.shft5,   spclror.shft6,  spclror.shft7]
  jumpclror_1 words [spclror.shft_1,  spclror.shft_2,  spclror.shft_3,  spclror.shft_4,  spclror.shft_5,  spclror.shft_6,  spclror.shft_7]
  jumpor      words [spror.shft1,   spror.shft2,   spror.shft3,   spror.shft4,   spror.shft5,   spror.shft6, spror.shft7]
  jumpor_1    words [spror.shft_1,  spror.shft_2,  spror.shft_3,  spror.shft_4,  spror.shft_5,  spror.shft_6,  spror.shft_7]
  jumpxor     words [sprxor.shft1,  sprxor.shft2,  sprxor.shft3,  sprxor.shft4,  sprxor.shft5,  sprxor.shft6, sprxor.shft7]
  jumpxor_1   words [sprxor.shft_1, sprxor.shft_2, sprxor.shft_3, sprxor.shft_4, sprxor.shft_5, sprxor.shft_6, sprxor.shft_7]
end
