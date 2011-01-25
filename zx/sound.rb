require 'z80/math_i.rb'
require 'z80/stdlib.rb'
class AYSound
  include Z80
  import Z80MathInt, :code => false, :labels => false, :macros => true
  import Z80Lib, :code => false, :labels => false, :macros => true

  class SoundChan < Label
    note    byte
    volume  byte
  end

  class Sound < Label
    chanA   SoundChan
    chanB   SoundChan
    chanC   SoundChan
    octave  byte
  end
  
  sndinfo addr 0x6000, Sound
  keytable addr sndinfo + +sndinfo, 8*(3+5)

  export setup
  ns :setup do
          clrmem8(sndinfo, +sndinfo)
          memcpy(keytable, keypatt, +keytable)
          ld   e, 0
          xor  a
          call setvolume
          ld   a, 1
          call setvolume
          ld   a, 2
          call setvolume
          ld   bc, 0xfffd
          ld   a, 7
          out (c), a
          inp  a, (c)
          anda 0b11111000
          ora  0b00111000
          ld   b, 0xbf
          out (c), a
          ret  
  end
  
  export iterate
  ns :iterate do
          call keytonote
          jp setattdec
  end

  ns :keytonote do
          ld   a, 0xfe
          inp  a, (0xfe)  # check cshift
          rrca            # bit 0
          jr   C, musick
          ld   a, 7
          out  (0xfe), a  # white border
          ld   e, -1      # octave
          ld   a, 0xf7    # keys 1-5
          inp  a, (0xfe)
          cpl
          anda  0x1f      # xxx 5 4 3 2 1
          jr   Z, nopr1
  loop1   inc  e
          rrca
          jp   NC, loop1
  setoc   ld   a, e
          add  a
          add  a  # * 4
          ld   e, a
          add  a  # * 8
          add  e  # * 12
          ld   [sndinfo.octave], a
          ret
  nopr1   ld   e, 10
          ld   a, 0xef    # keys 0-6
          inp  a, (0xfe)
          cpl
          anda  0x10      # xxx 6 x x x x
          ret   Z
  loop2   dec  e
          rrca
          jp   NC, loop2
          jp   setoc
                          # check released keys
  musick  xor  a          # black border
          out  (0xfe), a
          ld   hl, keytable # port, mask, last state, 5*notes
          ld   b, 8       # 8 halfs (check for released keys)
  kloop1  ld   a, [hl]    # port
          inc  hl
          inp  a, (0xfe)
          anda [hl]       # mask only interesting
          inc  hl
          ld   e, a       # save masked keys (1 released, 0 pressed)
          xor  [hl]       # check difference
          anda e          # check if it differs by released only
          jr   NZ,  kcheck
          ld   a, 6
          adda_to h, l
          jp   knoskp
  kcheck  ld   e, a       # save released keys
          ora  [hl]
          ld   [hl], a    # save changed state
          ld   a, e       # get released keys
          ora  0b00100000 # stop bit
  kckloop inc  hl
          sra  a
          jr   Z, knoskp  # next
          jp   NC, kckloop # get next key
          ex   af, af     # save loop state
          ld   a, [hl]    # get key note
          ld   de, sndinfo.chanA
          ex   de, hl
          ld   c, 3       # 3 channels
  seek1l  cp   [hl]       # is same note?
          jr   NZ, s1lskp  # yes
          xor  a
          ld   [hl], a    # clear note
          jp   s1lskp2
  s1lskp  inc  hl
          inc  hl         # next chaninfo
          dec  c
          jp   NZ, seek1l
  s1lskp2 ex   de, hl
          ex   af, af     # restore loop state
          jp   kckloop
  knoskp  djnz kloop1
  end
  ns do
          call getfrchan
          ret  NC         # no free channels
          ld   e, a       # save free channel number (0..2)
                          # check pressed keys
          ld   hl, keytable # port, mask, last state, 5*notes
          ld   b, 8       # 8 halfs (check for released keys)
  kloop1  ld   a, [hl]    # port
          inc  hl
          inp  a, (0xfe)
          anda [hl]       # mask only interesting
          inc  hl
          cpl
          ld   c, a       # save masked keys (0 released, 1 pressed)
          cpl
          xor  [hl]       # check difference
          anda c          # check if it differs by pressed only
          jr   NZ,  kcheck
          ld   a, 6
          adda_to h, l
          jp   knoskp
  kcheck  ld   c, a       # save pressed keys
          xor  [hl]
          ld   [hl], a    # save changed state
          ld   a, c       # get pressed keys
          ora  0b00100000 # stop bit
  kckloop inc  hl
          sra  a
          jr   Z, knoskp  # next
          jp   NC, kckloop # get next key
          ex   af, af     # save loop state
          ld   d, [hl]    # get key note
          push de         # key note + chan number
          ld   a, e       # chan number
          exx
          ld   hl, sndinfo.chanA
          add  a
          adda_to h, l
          pop  bc
          ld   [hl], b    # allocate channel with key note
          call playnote   # b = note c = chan
          call getfrchan
          ret  NC         # no more free channels
          exx
          ld   e, a       # save free channel number (0..2)
          ex   af, af     # restore loop state
          jp   kckloop
  knoskp  djnz kloop1
          ret
  end
  ns :setattdec do
          ld   hl, sndinfo.chanA
          ld   d, 0
  loop1   xor  a
          cp   [hl]
          inc  hl
          ld   a, [hl]    # volume
          jr   NZ, attack
          ora  a
          jr   Z, lskip   # off
          sub  2
          jr   NC, lnext
          xor  a
          jp   lnext
  attack  add  16
          jr   NC, lnext
          ld   a, 0xff
  lnext   ld   [hl], a
          ld   e, a       # volume
          ld   a, d       # channel (0..2)
          call setvolume
  lskip   inc  hl
          inc  d
          ld   a, 3
          cp   d
          jr   NZ, loop1
          ret
  end
  
  ns :getfrchan do
          ld   e, 255     # look for lowest volume
          ld   d, 0       # chan picked (none)
          ld   hl, sndinfo.chanA
          xor  a
          ld   b, 3       # 3 channels
  seek1l  cp   [hl]
          inc  hl
          jr   NZ, chanbsy
          ld   a, e       # last vol
          cp   [hl]       # check chan volume
          jr   C, higher  # nope this has higher volume
          ld   d, b       # save 3 - n chan number
          ld   e, [hl]    # save new lower volume
  higher  xor  a
  chanbsy inc  hl
          djnz seek1l # seek free chan over
          ora  d          # was channel selected
          ret  Z          # no free channels game over man, CF=0
          cpl
          add  4          # channel num (0..2)
          scf             # CF = 1 FOUND
          ret
  end

  macro :getnote do # b = note (1..12) -> de
        ld hl, notes
        ld a, [sndinfo.octave]
        dec b
        add b
        add a
        jr  NC, ok1
        inc h
  ok1   adda_to h,l
        ld  e, [hl]
        inc hl
        ld  d, [hl]
  end

  ns :playnote do # b = note c = chan (0,1,2) (hl)
        getnote
        ld   a, c
        add  a
        ld   bc, 0xfffd
        out (c), a
        ld   b, 0xbf
        out (c), e
        inc  a
        ld   b, 0xff
        out (c), a
        ld   b, 0xbf
        out (c), d
        ret
  end
  
  ns :setvolume do # e = volume (0..255) a = chan (0,1,2)
        ld   bc, 0xfffd
        add  8
        out (c), a
        ld   b, 0xbf
        ld   a, e
        anda 0xf0
        4.times { rrca }
        out (c), a
        ret
  end  

  keypatt data 1, [
    0xfe, 0b11110, 0b11110,  0, 4, 6, 8, 9, #  CS  Z  X  C  V
    0x7f, 0b11100, 0b11100,  0, 0,15,13,11, #  BR SS  M  N  B
    0xfd, 0b10110, 0b10110,  0, 5, 7, 0,10, #   A  S  D  F  G
    0xbf, 0b11010, 0b11010,  0,17, 0,14,12, #  EN  L  K  J  H
    0xfb, 0b11111, 0b11111, 16,18,20,21,23, #   Q  W  E  R  T
    0xdf, 0b11111, 0b11111, 32,30,28,27,25, #   P  O  I  U  Y
    0xf7, 0b10110, 0b10110,  0,17,19, 0,22, #   1  2  3  4  5
    0xef, 0b11011, 0b11011,  31,29,0,26,24  #   0  9  8  7  6
  ]

  basefq = 3.54690/2.0*1_000_000 # 1_764750  1.78975
  notes data 2, xxx = (1..8).map {|oct|
    [220.0,
     233.3,
     246.94,
     261.63,
     277.2,
     293.66,
     311.1,
     329.63,
     349.23,
     370.0,
     392.0,
     415.3].map { |freq|
      (basefq / 16.0 / (freq*2**(oct-4))).to_i
     }
  }.flatten
  puts xxx.each_slice(12) {|s| $stderr.puts s.inspect}
end
