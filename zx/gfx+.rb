require 'z80/math_i.rb'
require 'zx/gfx'
class ZXGfxMore
  include Z80 unless defined?(Program)
  import ZXGfx, :code => false, :labels => false, :macros => true
  import Z80MathInt, :code => false, :labels => false, :macros => true

  #  calculates coordinates and prepares registers for draw_bitmap and other compatible gfx functions
  #  uses: a af' b c d e h l ix (stack)
  #  hl: input: sprite address
  #  a:  input: sprite height (1..192)
  #  a`: input: sprite width in bytes (1..32)
  #  bc: input: x - coordinate (-32768..32767) [screen area: 0-255]
  #  de: input: y - coordinate (-32768..32767) [screen area: 0-191]
  #  f:  input: flags -> f'
  #  ix: input: jump to ix
  #  de: output: sprite address
  #  h:  output: vertical coordinate   (0..191)
  #  l:  output: horizontal coordinate (0..255)
  #  except when h > 191 then l contains vertical coordinate and h | 0xf8 is a negative h-coordinate (from -1 to -7)
  #  b:  output: sprite byte width
  #  c:  output: skip first sprite bytes
  #  a': output: sprite height (1..192)
  #  f': output: preserved f
  export  calculate_coords_jump
  ns :calculate_coords_jump do
          push af           # sprite height + flags
          ld   a, d
          ora  a
          jp   Z, vnext1    # 0 <= de < 256
          inc  a            # d == 0xff
          jr   NZ, quit1    # de < -256
          xor  a
          sub  e            # 0 - (negativ offset)
          ld   e, a         # skip lines
          ex   [sp],hl      # sprite height <-> sprite addr
          ld   a, h         # height
          sub  e            # height - skip lines
          jr   C,quit1      # skip lines >  height
          jr   Z,quit1      # skip lines == height
          ld   h, a         # new height
          ex   [sp],hl      # sprite addr <-> new height
          ex   af, af       # height <-> width
          ld   d, a         # width
          ex   af, af
          ld   a, d         # width
          ld   d, 0
                            # sprite address+= skip lines * width
          mul8(d, e, a, d, e, false) #hl+= de*a
          ld   de, 0        # y = 0
          jp   hnext1       # de = 0, hl = sprite address, (sp) = height, a' = width, f' = ZC
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

          xor  a            # niepotrzebne skreœlic A ju¿ ma 0
          sub  c            # x = -x
          jr   Z, quit1     # x == -256
          anda 0xf8
          jr   Z, fskip     # -8 < x < 0
          rrca
          rrca
          rrca
          ld   d, a         # skip first bytes
          ex   af, af       # sprite width -> a
          sub  d            # width -= -x / 8
          jr   C,quit1      # width <  -x / 8
          jr   Z,quit1      # width == -x / 8
          ex   af, af       # new sprite width -> a'
  fskip   ld   a, c
          ora  0xf8
          ld   c, e
          ld   e, a
  hnext2  ex   de, hl       # sprite -> de
          ld   b, h         # skip first bytes
          ld   h, l         # h = y & 0xff
          ld   l, c         # l = x & 0xff
          pop  af
          ex   af, af       # sprite height + flags <-> sprite width
          ld   c, b         # skip first bytes
          ld   b, a         # sprite byte width
          jp   (ix)
  end
  #  HOWTO:
  #  cp a  -> z1 c0 (xor)
  #  cp a
  #  scf   -> z1 c1 (and)
  #  xor a
  #  inc a -> z0 c0 (or)
  #  scf
  #  sbc a -> z0 c1 (copy)
  #
  #  draws on screen using xor/or/and/copy bitmap with arbitrary height and width
  #  the data for sprites is laid horizontaly: first row, second row, ...
  #  optimised for width > height sprites and large square ones (size >= 24 pixels)
  #  uses: all registers (stack)
  #  de: input: sprite address
  #  h:  input: vertical coordinate   (0..191)
  #  l:  input: horizontal coordinate (0..255)
  #  except when h > 191 then l contains vertical coordinate and h | 0xf8 is a negative h-coordinate (from -1 to -7)
  #  b:  input: sprite byte width
  #  c:  input: skip first sprite bytes
  #  a': input: sprite height (1..192)
  #  CF': input: 0: xor mode, 1: and mode,  0: or mode, 1: copy mode
  #  ZF': input: 1:                         0:
  export draw_bitmap
  ns :draw_bitmap do
          push bc             # byte width | skip first
          ld   a, h
          cp   192
          jp   C, skipneg
          anda 0x07
  skipad  ld   c, a           # negshift
          ytoscr l, h, l, b
          ex   [sp], hl       # byte width | skip first (sp) <-> screen address hl
          ld   a, c           # shifts
          ora  a
          jr   Z, skippos
          dec  h              # byte width - 1
          add  7              # adjust to shfts table
          jp   skippos
          
  skipneg xytoscr h, l, h, l, c, b # hl -> yx, hl -> screen, c -> shift

          ld   a, l           # check sprite byte width
          ex   [sp], hl       # byte width | skip first (sp) <-> screen address hl
          ora  0xe0           # screen addr lo + high bits mask
          add  h              # add width
          jp   NC, noadjw     # no over screen?
          ld   b, a           # skip over
          neg
          add  h              # decrease width if out of screen
          ld   h, a           # new width if out of screen
          ld   a, l
          add  b              # skip first increase
          ld   l, a           # new skip first
          sub_from b, d, e    # sprite address - skip over
          
  noadjw  ld   a, c           # shift

  skippos exx
          ld   c, a           # save shift
          ld   hl, maskshift
          adda_to h, l
          ld   a, [hl]        # get mask
          exx

          ex   de, hl         # sprite address -> hl
          ld   b, d           # bytes width -> b
          ld   d, 0           # skip first bytes -> de

          ex   af, af         # mask <-> sprite height + flags
          ld   c, a           # sprite height -> c
          exx

          ld   a, c           # shift -> a
          jr   NZ, copyor
          jr   C, andmode
          ora  a
          jp   Z, bitmapxor0.start
          ld   hl, bitmapxor.start
          ld   de, bitmapxor
          jp   shftixy
andmode   ora  a
          jp   Z, bitmapand0.start
          ld   hl, bitmapand.start
          ld   de, bitmapand
          jp   shftixy
copyor    jr   C, copymode
          ora  a
          jp   Z, bitmapor0.start
          ld   hl, bitmapor.start
          ld   de, bitmapor
          jp   shftixy
copymode  ora  a
          jp   Z, bitmapcopy0.start
          ld   hl, bitmapcopy.start
          ld   de, bitmapcopy
shftixy   ex   [sp], hl # jump -> (sp), screen addr -> hl
          dec  a
          add  a
          adda_to d, e
          ld   a, [de]  # first shfts offset
          ld   b, d     # save hl -> de
          ld   c, e
          adda_to b, c  # get first offset
          ld   iyh, b   # load up to iy
          ld   iyl, c
          inc  de       # next shft offset
          ld   a, [de]  # get offset
          adda_to d, e  # adjust
          ld   ixh, d
          ld   ixl, e
          ex   af, af   # mask -> a
          ld   e, a     # mask -> e
          ret           # jp ((sp))
  # hl:  screen
  #  e:  mask
  # ix:  rotate proc
  # iy:  first skip rotate proc
  # hl': sprite
  # de': sprite skip first
  # b':  bytes width  (can not exceed screen)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # c':  lines height
  end

  unless label_defined? :maskshift
    maskshift bytes [0x00, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE, 0x80, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC, 0xFE]
  end

  # hl:  screen
  #  m:  mask
  # ix:  rotate proc
  # hl': sprite
  # de': sprite skip first
  # b':  bytes width (can not exceed screen)
  # c':  lines height (can not exceed screen)
  # q, p, m, t, y
  # macro :shiftandor do |_, n, back|
      # if n <= 4
        # n.times { rrca } # 4*4
      # else
        # n.times { rlca } # 4*4
      # end
      # ex   af, af   # 4
      # if n <= 4
        # n.times { rrca } # 4*4
      # else
        # n.times { rlca } # 4*4
      # end
      # jp   back     # 10
  # end

  # macro :spriteandor do |_, q, p, m, t, y|
  # vloop   nextline  h, l, false # 27 /49 /59
  # start   ld   q|p, 0  # 10
          # push hl      # 11
          # exx          # 4
          # push bc      # 11
          # add  hl, de  # 11 skip left first
  # hloop   ld   a, [hl] # 7  data
          # inc  hl      # 6
          # ex   af, af  # 4
          # ld   a, [hl] # 7  mask
          # inc  hl      # 6
          # exx          # 4
          # jp  (ix)     # 8
                     ## 4*8 + 4 + 10
  # back    ex   af, af  # 4
          # ld   t, a    # 4
          # anda m       # 8
          # ld   y, a    # 4 f0 mask
          # xor  t       # 4 0f
          # ora  p       # 4 ff mask + previous
          # ld   p, y    # 4 f0 mask -> previous
          # cpl          # 4
          # anda [hl]    # 7
          # ex   af, af  # 4

          # ld   t, a    # 4
          # anda m       # 8
          # ld   y, a    # 4 f0 bpln
          # xor  t       # 4 0f
          # ora  q       # 4 ff bpln + previous
          # ld   q, y    # 4 f0 bpln -> previous
          # ld   t, a    # 4
          # ex   af, af  # 4
          # ora  t       # 4
          # ld   [hl], a # 7
          # inc l        # 4
          # exx          # 4
          # djnz hloop   # 13/8

          # exx          # 4
          # ld   a, l    # 4
          # anda 0x1f    # 7
          # jr   Z, nslin1 # 7
          # ld   a, p    # 4
          # cpl          # 4
          # anda [hl]    # 7
          # ora  q       # 4
          # ld   [hl], a # 7
  # nslin1  exx          # 4
          # pop  bc      # 10
          # dec  c       # 4
          # exx          # 4
          # pop  hl      # 10
          # jp   NZ, vloop # 10
          # ret          # 10
  # end
  # hl:  screen
  #  m:  mask
  # ix:  rotate proc
  # iy:  (optional) first skip proc
  # hl': sprite
  # de': sprite skip first
  # b':  bytes width  (can not exceed screen)
  # c':  lines height
  # q, m, t, y
  macro :bitmapxoror do |_, q, m, t, y, type| # 100 (hloop each byte) + 135 (vloop each row)
    alter = proc do
      ns do
        case type
        when :xor
          xor [hl]
        when :or
          ora [hl]
        when :and
          cpl
          anda [hl]
        end
        ld  [hl], a
      end
    end
    loopproc = proc do
      ns do
              ld   t, a    # 4
              anda m       # 4
              ld   y, a    # 4 f0 bpln
              xor  t       # 4 0f
              ora  q       # 4 ff bpln + previous
              ld   q, y    # 4 f0 bpln -> previous
              alter[]      # 7/14/18
              inc l        # 4
              exx          # 4
              djnz hloop   # 13/8
              exx          # 4
              ld   a, l    # 4
              anda 0x1f    # 7
              jr   Z, nslin1 # 7/12
    skipmid   ld   a, q    # 4
              alter[]      # 7/14/18
    nslin1    ld   a, l    # 4
              exx          # 4
              pop  bc      # 10
              sub  b       # 4
              dec  c       # 4
              exx          # 4
              ld   l, a    # 4
      end
    end
    # jump offset table
    # shft1, shft1, shft2, shft2.... shft7, shft7, shfts1, shft1, shfts2, shft2
    bytes (1..28).map {|i|
      case
      when i <= 14 
        offset.send("shft#{(i+1) / 2}") + 29 - i
      when i.even?
        offset.send("shft#{(i-13) / 2}") + 29 - i
      else
        offset.send("shfts#{(i-13) / 2}") + 29 - i
      end
    }
    start     offset.start
    ns :offset do
      shft4   rrca
      shft3   rrca
      shft2   rrca
      shft1   rrca
      lpprc   loopproc[]
              ret  Z       # 5/10
      vloop   nextline  h, l, true # 27 /49 /59
      start   ld   q, 0    # 7
              exx          # 4
              push bc      # 11
              add  hl, de  # 11 skip left first

              ld   a, [hl] # 7  bmap
              inc  hl      # 6
              exx          # 4
              jp (iy)      # 8 iy == ix if no skip first part
      shfts5  rlca        # loose first part (-1 to -7)
      shfts6  rlca
      shfts7  rlca
              anda m       # 4
              ld   q, a    # 4 f0 bpln -> previous
              exx          # 4
              ld   a, b
              ora  a       # width == 0?
              jp   NZ, hloop
              exx
              jp   lpprc.skipmid
      shfts4  rrca
      shfts3  rrca
      shfts2  rrca
      shfts1  rrca
              anda m       # 4
              ld   q, a    # 4 f0 bpln -> previous
              exx          # 4
              ld   a, b
              ora  a       # width == 0?
              jp   NZ, hloop
              exx
              jp   lpprc.skipmid

      hloop   ld   a, [hl] # 7  bmap
              inc  hl      # 6
              exx          # 4
              jp  (ix)     # 8
                         # 4*4
      shft5   rlca
      shft6   rlca
      shft7   rlca
              loopproc[]
              jp   NZ, vloop # 10
              ret            # 10
    end
  end

  # (sp):  screen
  # hl': sprite
  # de': sprite skip first bytes
  # b':  bytes width
  # c':  lines height
  macro :bitmapxoror0 do |_, type|
    alter = proc do
      ns do
        case type
        when :xor
          xor [hl]
        when :or
          ora [hl]
        when :and
          cpl
          anda [hl]
        end
        ld  [hl], a
      end
    end
    start label
          #push hl     # 11 already on stack!
          exx          # 4
          jp   skip1   # 10
    vloop nextline  h, l, quit0 # 27 /49 /59
          ex   [sp],hl # 4  screen <-> skip first
          ex   de, hl  # 4  sprite <-> skip first
    skip1 add  hl, de  # 11 sprite+= skip left first
          ex   de, hl  # 4  skip first <-> sprite
          ex   [sp],hl # 4  skip first <-> screen
          push bc      # 11
    if type == :copy
          ld   c, b    # 4
          ld   b, 0    # 7
          push hl      # 11
          ex   de, hl  # 4
          ldir         # 21*(width-1)+16
          ex   de, hl  # 4
          pop  hl      # 10
          pop  bc      # 10
    else
    hloop ld   a, [de] # 7  bmap
          inc  de      # 6
          alter[]      # 14/18/7
          inc  l       # 4
          djnz hloop   # 13/8
          pop  bc      # 10
          ld   a, l    # 4
          sub  b       # 4
          ld   l, a    # 4
    end
          dec  c       # 4
          jp   NZ, vloop # 10
    quit0 pop hl       # 10
          ret          # 10
  end

  bitmapxoror :bitmapor, d, e, b, c, :or
  bitmapxoror :bitmapxor, d, e, b, c, :xor
  bitmapxoror :bitmapand, d, e, b, c, :and
  bitmapxoror :bitmapcopy, d, e, b, c, :copy
  bitmapxoror0 :bitmapor0, :or
  bitmapxoror0 :bitmapxor0, :xor
  bitmapxoror0 :bitmapand0, :and
  bitmapxoror0 :bitmapcopy0, :copy


  # hl screen
  # de memory
  # c  width
  # b  height
  macro :scrtomem do |eoc|
          ld   a, b
          ld   b, 0
    loop1 ex   af, af   # 4
          ld   a, c     # 4
          push hl       # 11
          ldir          # 21*(c-1)+16
          pop hl        # 10
          ld   c, a     # 4
          nextline  h, l, true # 27 /49 /59
          ex   af, af   # 4
          dec  a        # 4
          jp   NZ, loop1 # 10
  end

  # de screen
  # hl memory
  # c  width
  # b  height
  macro :memtosrc do |eoc|
          ld   a, b
          ld   b, 0
    loop1 ex   af, af   # 4
          ld   a, c     # 4
          push de       # 11
          ldir          # 21*(c-1)+16
          pop  de       # 10
          ld   c, a     # 4
          nextline  d, e, true # 27 /49 /59
          ex   af, af   # 4
          dec  a        # 4
          jp   NZ, loop1 # 10
  end

  export scrcopy
  ns :scrcopy do
  compat  ex   af, af   # compatibility hook for calculate_coords_jump
          ld   c, b     # byte width -> c
          jp   compskp
  compat8 ex   af, af   # compatibility hook for calculate_coords_jump8
          sub  b        # height=- skip first
          ret  C
          ret  Z
  compskp ld   b, a
          exx
          push de       # destination memory
          exx
          pop  de       # get destination memory
          ld   a, h
          cp   192
          jp   C, start
          push bc
          ytoscr l, h, l, b
          xor  a        # clear shift
          jp   skippos
  # h:  input: vertical coordinate   (0..191)
  # l:  input: horizontal coordinate (0..255)
  # de: input: destination memory
  # b:  input: bitmap height (1..192)
  # c:  input: bitmap byte width
  start   push bc
          xytoscr h, l, h, l, c, b # hl -> yx, hl -> screen, c -> shift
          ld   a, c
  skippos pop  bc
          ora  a
          jr   Z, noshft
          inc  c
  noshft  ld   a, l
          ora  0xe0
          add  c
          jp   NC, save
          neg
          add  c
          ld   c, a     # decrease width if out of screen
  save    ex   de, hl   # save screen address and width/height
          ld   [hl], c
          inc  hl
          ld   [hl], b
          inc  hl
          ld   [hl], e
          inc  hl
          ld   [hl], d
          inc  hl
          ex   de, hl
  copy    scrtomem
          ret
  end
  
  # hl: input: source memory with restore info header and data
  export restore
  ns :restore do
          ld   c, [hl]
          inc  hl
          ld   b, [hl]
          inc  hl
          ld   e, [hl]
          inc  hl
          ld   d, [hl]
          inc  hl
  copy    memtosrc
          ret
  end
end
