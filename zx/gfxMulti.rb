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
  macro :spritexoror do |_, fskip, q, m, t, y, type|
    vloop nextline  h, l, true # 27 /49 /59
    start ld   q, 0    # 7
          push hl      # 11
          exx          # 4
          push bc      # 11
          add  hl, de  # 11 skip left first

          if fskip       # loose first part (-1 to -7)
            ld   a, [hl] # 7  bmap
            inc  hl      # 6
            jp (iy)      # 8
      shfts6 rlca
      shfts7 rlca
            jp    fskip
      shfts5 rrca
      shfts4 rrca
      shfts3 rrca
      shfts2 rrca
      shfts1 rrca
      fskip exx          # 4
            anda m       # 4
            ld   q, a    # 4 f0 mask -> previous
            exx          # 4
            #dec  b       # 4 ????????????????????????????
          end

    hloop ld   a, [hl] # 7  bmap
          inc  hl      # 6
    here  jp  (ix)     # 8
                       # 4*4
    shft5 rlca
    shft6 rlca
    shft7 rlca

      loopproc = proc do
        ns do
            exx          # 4
            ld   t, a    # 4
            anda m       # 4
            ld   y, a    # 4 f0 bpln
            xor  t       # 4 0f
            ora  q       # 4 ff bpln + previous
            ld   q, y    # 4 f0 bpln -> previous
            self.send type, [hl]    # 7
            ld   [hl], a # 7
            inc l        # 4
            exx          # 4
            djnz hloop   # 13/8

            exx          # 4
            ld   a, l    # 4
            anda 0x1f    # 7
            jr   Z, nslin1 # 7
            ld   a, q    # 4
            self.send type, [hl]    # 7
            ld   [hl], a # 7
    nslin1  exx          # 4
            pop  bc      # 10
            dec  c       # 4
            exx          # 4
            pop  hl      # 10
            jp   NZ, vloop # 10
            ret          # 10
        end
      end
          loopproc
    shft4 rrca
    shft3 rrca
    shft2 rrca
    shft1 rrca
          loopproc
  end

  # hl screen
  # de memory
  # c  width
  # b  height
  macro :scrtomem do
          ld   a, l
          ora  0xe0
          add  c
          jp   NC, skip
          neg
          add  c
          ld   c, a
    skip  ld   a, b
          ld   b, 0
    loop1 ex   af, af   # 4
          ld   a, c     # 4
          ldir          # 21*(c-1)+16
          ld   c, a     # 4
          ld   a, l     # 4
          sub  c        # 4
          ld   l, a     # 4
          nextline  h, l, true # 27 /49 /59
          ex   af, af   # 4
          dec  a        # 4
          jp   NZ, loop1 # 10
          ret
  end

  # de screen
  # hl memory
  # c  width
  # b  height
  macro :memtosrc do
          ld   a, e
          ora  0xe0
          add  c
          jp   NC, skip
          neg
          add  c
          ld   c, a
    skip  ld   a, b
          ld   b, 0
    loop1 ex   af, af   # 4
          ld   a, c     # 4
          ldir          # 21*(c-1)+16
          ld   c, a     # 4
          ld   a, e     # 4
          sub  c        # 4
          ld   e, a     # 4
          nextline  d, e, true # 27 /49 /59
          ex   af, af   # 4
          dec  a        # 4
          jp   NZ, loop1 # 10
          ret
  end
