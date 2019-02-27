##
#  ==A module with Z80 Macros for common ZX Spectrum graphics tasks
#
#  Example:
#
#    require 'zxlib/gfx'
#
#    class Program
#      include Z80
#      macro_import  ZXGfx
#
#              # invert pixel at 100, 100
#              ld  l, 100
#              ld  a, 100
#              xytoscr(a, l, ah:h, al:l, s:b, t:c)
#              inc b
#              ld  a, 1
#      shift1  rrca
#              djnz shift1
#              xor  [hl]
#              ld   [hl], a
#              ret
#    end
class ZXGfx
  ##
  #  ==ZXGfx macros.
  module Macros
    ##
    # Returns true if v equals to 0 or only one bit is set.
    def only_one_bit_set_or_zero?(v)
        v && (v|(v-1))+1 == v << 1
    end
    ##
    #  Calculate constant screen address from x, y pixel position
    def xy_to_pixel_addr(x, y, scraddr:0x4000)
        scraddr + (
            (
                (
                    ((y & 0x07) << 3) | ((y & 0b111000) >> 3) | ((y & 0xffc0))
                ) << 5
            ) | ((x & 0xff) >> 3)
        )
    end
    ##
    # Advances to the next screen line byte address (down) using ah|al registers.
    # (optionally) returns from subroutine if address goes out of screen area.
    #
    # Modifies: +af+, +ah+, +al+
    #
    # * +ah+:: input/output register: address high byte
    # * +al+:: input/output register: address low byte
    # * +bcheck+:: boundary check flag:
    #              +false+ = disable checking,
    #              +true+  = issue +ret+ if out of screen (default)
    #              +label+ = jump to label if out of screen,
    #              +hl+|+ix+|+iy+ = jump to address in a register if out of screen
    # * +scraddr+:: screen memory address as an integer, must be a multiple of 0x2000
    #
    # if block is given and +bcheck+ == +true+ evaluates namespaced block instead of +ret+.
    # The code in the given block should not fall through and should end with a jump.
    #
    # T-states: 
    #
    # * when +bcheck+ is +false+:: 27:87.5% / 49:1.56% / 59:10.94%
    # * when +bcheck+ is +true+ or +label+::  27:87.5% / (70:2 / 75:1):1.56% / 62:10.94%
    def nextline(ah, al, bcheck = true, scraddr:0x4000, **nsopts, &block)
        if ah == al or [ah, al].include?(a) or
                (register?(bcheck) and ![hl_, ix_, iy_].include?(bcheck)) or
                (bcheck == hl and ([h, l].include?(ah) or [h, l].include?(al))) or
                (bcheck == ix and ([ixh, ixl].include?(ah) or [ixh, ixl].include?(al))) or
                (bcheck == iy and ([iyh, iyl].include?(ah) or [iyh, iyl].include?(al))) or
                ![ah, al].all?{|r| register?(r) } or
                !(Integer === scraddr and scraddr == (scraddr & 0xE000))
            raise ArgumentError, "nextline invalid arguments!"
        end
        ns do |eoc|
                    inc  ah
                    ld   a, ah
                    anda 0x07
                    jr   NZ, eoc
                    ld   a, al
                    add  0x20
                    ld   al, a
            if bcheck
                    ld   a, ah
                    jp   NC, restrh
                    cp   (scraddr >> 8)|0x18
                    jr   C, eoc
                case bcheck
                when true
                    if block_given?
                        ns(**nsopts, &block)
                    else
                        ret
                    end
                else
                    jp  bcheck
                end
            else
                    jr   C, eoc
                    ld   a, ah
            end
        restrh      sub  0x08
                    ld   ah, a
        end
    end
    ##
    # Moves up to the previous screen line byte address using ah|al registers.
    # (optionally) returns from subroutine if address goes out of screen area.
    #
    # Modifies: +af+, +ah+, +al+
    #
    # * +ah+:: input/output register: address high byte
    # * +al+:: input/output register: address low byte
    # * +bcheck+:: boundary check flag:
    #              +false+ = disable checking,
    #              +true+  = issue +ret+ if out of screen (default)
    #              +label+ = jump to label if out of screen,
    #              +hl+|+ix+|+iy+ = jump to address in a register if out of screen
    # * +scraddr+:: screen memory address as an integer, must be a multiple of 0x2000
    #
    # if block is given and +bcheck+ == +true+ evaluates namespaced block instead of +ret+.
    # The code in the given block should not fall through and should end with a jump.
    #
    # T-states: 
    #
    # * when +bcheck+ is +false+:: 27:87.5% / 49:1.56% / 59:10.94%
    # * when +bcheck+ is +true+ or +label+::  27:87.5% / (70:2 / 75:1):1.56% / 62:10.94%
    def prevline(ah, al, bcheck = true, scraddr:0x4000, **nsopts, &block)
        if ah == al or [ah, al].include?(a) or
                (register?(bcheck) and ![hl_, ix_, iy_].include?(bcheck)) or
                (bcheck == hl and ([h, l].include?(ah) or [h, l].include?(al))) or
                (bcheck == ix and ([ixh, ixl].include?(ah) or [ixh, ixl].include?(al))) or
                (bcheck == iy and ([iyh, iyl].include?(ah) or [iyh, iyl].include?(al))) or
                ![ah, al].all?{|r| register?(r) } or
                !(Integer === scraddr and scraddr == (scraddr & 0xE000))
            raise ArgumentError, "prevline invalid arguments!"
        end
        ns do |eoc|
                    ld   a, ah
                    dec  ah
                    anda 0x07
                    jr   NZ, eoc
                    ld   a, al
                    sub  0x20
                    ld   al, a
            if bcheck
                    ld   a, ah
                    jp   NC, restrh
                    cp   (scraddr >> 8)
                    jr   NC, eoc
                case bcheck
                when true
                    if block_given?
                        ns(**nsopts, &block)
                    else
                        ret
                    end
                else
                    jp  bcheck
                end
            else
                    jr   C, eoc
                    ld   a, ah
            end
        restrh      add  0x08
                    ld   ah, a
        end
    end
    ##
    # Converts y,x coordinates to the screen byte address and bits shift
    #
    # Modifies: +af+, +s+, +t+, +ah+, +al+
    #
    # * +y+:: input register: vertical-coordinate (the +a+ register or the same as: +h+, +l+, +s+ or +t+)
    # * +x+:: input register: horizontal-coordinate (may be same as: +l+)
    # * +ah+:: output register: address high
    # * +al+:: output register: address low
    # * +s+:: output register: bits shift 0..7
    # * +t+:: temporary register
    # * +scraddr+:: screen memory address as an integer, must be a multiple of 0x2000
    #
    # T-states: 101/104 depending on scraddr (101 for default 0x4000)
    #
    # y < a1 a2 h3 h2 h1 l3 l2 l1,  x < x5 x4 x3 x2 x1 s3 s2 s1,
    # h > S  S  S  a1 a2 l3 l2 l1,  l > h3 h2 h1 x5 x4 x3 x2 x1,  s > 0  0  0  0  0  s3 s2 s1
    def xytoscr(y, x, ah:h, al:l, s:b, t:c, scraddr:0x4000)
        if y == x or [x,ah,al,s,t].include?(a) or [ah,al,s,t].uniq.size != 4 or
                ![y, x, ah, al, s, t].all?{|r| r.is_a?(Register)} or
                !(Integer === scraddr and scraddr == (scraddr & 0xE000))
            raise ArgumentError, "xytoscr invalid arguments!"
        end
        isolate do
            if y == a
                    ld   ah, a
            else
                    ld   a, y
            end                 # a= H H h h h l l l
                    anda 0b00000111
                    ld   s, a   # s= 0 0 0 0 0 l l l
            if y == a
                    xor  ah
            else
                    xor  y      # a= H H h h h 0 0 0
            end
            if only_one_bit_set_or_zero?(scraddr)
                if (scraddr & 0x2000).zero?
                    rrca        # a= 0 H H h h h 0 0
                else
                    scf
                    rra         # a= 1 H H h h h 0 0
                end
                if (scraddr & 0x4000).zero?
                    rrca        # a= 0 S H H h h h 0
                else
                    scf
                    rra         # a= 1 S H H h h h 0
                end
                if (scraddr & 0x8000).zero?
                    rrca        # a= 0 S S H H h h h
                else
                    scf
                    rra         # a= 1 S S H H h h h
                end
            else
                    3.times { rrca }
                    ora (scraddr>>8)
            end
                    ld   ah, a  # h= S S S H H h h h
                    anda 0b00000111
                    ld   t, a   # t= 0 0 0 0 0 h h h
                    xor  ah     # a= S S S H H 0 0 0
                    ora  s
                    ld   ah, a  # h= S S S H H l l l
                    ld   a, x   # a= x x x x x s s s
                    anda 0b00000111
                    ld   s, a   # s= 0 0 0 0 0 s s s
                    xor  x      # a= x x x x x 0 0 0
                    ora  t      # a= x x x x x h h h
                    3.times { rrca }
                    ld   al, a  # l= h h h x x x x x
        end
    end
    ##
    # Converts 0,y coordinates to the screen byte address
    #
    # Modifies: +af+, +y+, +h+, +l+, +t+
    #
    # * +y+:: input register: vertical-coordinate (may be same as: +h+, +l+ or +a+)
    # * +ah+:: output register: address high
    # * +al+:: output register: address low
    # * +t+:: temporary register
    # * +col+:: optional 8-bit column number (0-31) as a register (must not be same as other arguments)
    # * +scraddr+:: screen memory address as an integer, must be a multiple of 0x2000
    #
    # T-states: 73/81 if +col+ is not nil
    #
    # y < a1 a2 h3 h2 h1 l3 l2 l1,
    # h > S  S  S  a1 a2 l3 l2 l1,  l > h3 h2 h1 0  0  0  0  0
    def ytoscr(y, ah:h, al:l, col:nil, t:c, scraddr:0x4000)
        if [ah,al,t].include?(a) or [ah,al,t].uniq.size != 3 or t == y or
                ![y, ah, al, t].all?{|r| r.is_a?(Register) } or
                (col.is_a?(Register) and [y, ah, al, t, a].include?(col)) or
                (!col.nil? and !col.is_a?(Register)) or
                !(Integer === scraddr and scraddr == (scraddr & 0xE000))
            raise ArgumentError, "ytoscr invalid arguments!"
        end
        isolate do
            if y == a
                    ld   al, a
            else
                    ld   a, y
            end                       # a= H H h h h l l l
                    anda 0b00000111
                    ld   t, a         # h= 0 0 0 0 0 l l l
            if y == a
                    xor  al
            else
                    xor  y
            end                       # a= H H h h h 0 0 0
                    rlca              # a= H h h h 0 0 0 H
                    rlca              # a= h h h 0 0 0 H H
                    ld   ah, a        # b= h h h 0 0 0 H H
                    anda 0b11100000   # a= h h h 0 0 0 0 0
                    add  col if col
                    ld   al, a        # l= h h h c c c c c
                    sub  col if col   # a= h h h 0 0 0 0 0
                    xor  ah           # a= 0 0 0 0 0 0 H H
                    3.times { rlca }
                    ora  t            # a= 0 0 0 H H l l l
            unless scraddr.zero?
                    ora  (scraddr>>8) # a= S S S H H l l l
            end
                    ld   ah, a        # h= S S S H H l l l
        end
    end
    ##
    # Converts row,col text coordinates to screen byte address
    #
    # Modifies: +af+, +r+, +c+, +h+, +l+
    #
    # * +r+:: input register: text row (may be same as: +l+)
    # * +c+:: input register or a integer: text column  (may be same as: +l+)
    # * +ah+:: output register: address high
    # * +al+:: output register: address low (may be same as: +c+, +r+)
    # * +r_already_in_a+:: when +true+ there's no need to load +r+ to +a+
    # * +scraddr+:: screen memory address as an integer, must be a multiple of 0x2000
    #
    # T-states: 43
    #
    # * T - 4: if +r+ already in +a+
    # * T + 4: if +c+ is a register
    # * T + 7: if +c+ is a non-zero integer
    # * T + 6: if scraddr has more than one bit set
    #
    # r < 0  0  0  5r 4r 3r 2r 1r,  c < 0  0  0  5c 4c 3c 2c 1c,
    # h > 0  1  0  5r 4r 0  0  0,   l < 3r 2r 1r 5c 4c 3c 2c 1c
    def rctoscr(r, c, ah:h, al:l, r_already_in_a:false, scraddr:0x4000)
      if r == ah or [r, c, ah, al].include?(a) or [r, c, ah].uniq.size != 3 or ah == al or
            ![r, ah, al].all?{|r| r.is_a?(Register)} or
            !(Integer === scraddr and scraddr == (scraddr & 0xE000))
        raise ArgumentError, "rctoscr invalid arguments!"
      end
      isolate do
                    ld   a, r unless r_already_in_a
                    anda 0b00011000 # a= 0  0  0  H  H  0  0  0
                    ora  (scraddr>>8) unless only_one_bit_set_or_zero?(scraddr)
                    ld   ah, a      # h= S  S  S  H  H  0  0  0
                    xor  (scraddr>>8) unless only_one_bit_set_or_zero?(scraddr)
                    xor  r          # a= 0  0  0  0  0  r  r  r
                    3.times {rrca}  # a= r  r  r  0  0  0  0  0
                    ora  c unless c == 0
                    ld   al, a      # l= r  r  r  c  c  c  c  c
            if only_one_bit_set_or_zero?(scraddr)
                nbit = Math.log2(scraddr>>8).to_i
                    set  nbit, ah   # h= S  S  S  H  H  0  0  0
            end unless scraddr.zero?
      end
    end
    ##
    # Converts row, col text coordinates to attribute address
    #
    # Modifies: +af+, +r+, +c+, +h+, +l+
    #
    # * +r+:: input register: text row (may be same as: +l+)
    # * +c+:: input register or a integer: text column  (may be same as: +l+)
    # * +ah+:: output register: address high
    # * +al+:: output register: address low (may be same as: +c+, +r+)
    # * +r_already_in_a+:: when +true+ there's no need to load +r+ to +a+
    # * +scraddr+:: screen memory address as an integer, must be a multiple of 0x2000
    #
    # T-states: 53
    #
    # * T - 4: if +r+ already in +a+
    # * T + 4: if +c+ is a register
    # * T + 7: if +c+ is a non-zero integer
    #
    # r < 0  0  0  5r 4r 3r 2r 1r,  c < 0  0  0  5c 4c 3c 2c 1c,
    # h > 0  1  0  1  1  0  5r 4r,  l < 3r 2r 1r 5c 4c 3c 2c 1c
    def rctoattr(r, c, ah:h, al:l, r_already_in_a:false, scraddr:0x4000)
      if r == ah or [r, c, ah, al].include?(a) or [r, c, ah].uniq.size != 3 or ah == al or
            ![r, ah, al].all?{|r| r.is_a?(Register)} or
            !(Integer === scraddr and scraddr == (scraddr & 0xE000))
        raise ArgumentError, "rctoattr invalid arguments!"
      end
      attraddr = scraddr + 0x1800
      isolate do
            ld   a, r unless r_already_in_a
            3.times {rrca}
            ld   r, a          # r= r r r 0 0 0 H H
            anda 0b00000011
            ora  (attraddr>>8)
            ld   ah, a         # h= S S S 1 1 0 H H
            xor  (attraddr>>8) # a= 0 0 0 0 0 0 H H
            xor  r             # a= r r r 0 0 0 0 0
            ora  c unless c == 0
            ld   al, a         # l= r r r c c c c c
      end
    end
    ##
    # Advances to the next text line coordinate (down 8 lines) using ah|al registers.
    # (optionally) returns from subroutine if address goes out of screen area.
    #
    # Modifies: +af+, +ah+, +al+
    #
    # * +ah+:: input/output register: address high byte
    # * +al+:: input/output register: address low byte
    # * +bcheck+:: boundary check flag:
    #              +false+ = disable checking,
    #              +true+  = issue +ret+ if out of screen (default)
    #              +label+ = jump to label if out of screen,
    #              +hl+|+ix+|+iy+ = jump to address in a register if out of screen
    # * +scraddr+:: screen memory address as an integer, must be a multiple of 0x2000
    #
    # if block is given and +bcheck+ == +true+ evaluates namespaced block instead of +ret+.
    #
    # T-states: 
    #
    # * when +bcheck+ is +false+:: 27:87.5% / 37:12.50%
    # * when +bcheck+ is +true+ or +label+::  27:87.5% / (49:2 / 55:1):12.50% / 54:12.50%
    def nextrow(ah, al, bcheck = true, scraddr:0x4000, **nsopts, &block)
        if ah == al or [ah, al].include?(a) or
                (register?(bcheck) and ![hl_, ix_, iy_].include?(bcheck)) or
                (bcheck == hl and ([h, l].include?(ah) or [h, l].include?(al))) or
                (bcheck == ix and ([ixh, ixl].include?(ah) or [ixh, ixl].include?(al))) or
                (bcheck == iy and ([iyh, iyl].include?(ah) or [iyh, iyl].include?(al))) or
                ![ah, al].all?{|r| register?(r) } or
                !(Integer === scraddr and scraddr == (scraddr & 0xE000))
            raise ArgumentError, "nextline invalid arguments!"
        end
        ns do |eoc|
                ld   a, al
                add  0x20
                ld   al, a
                jr   NC, eoc
        upperdn ld   a, ah
                add  0x08
                ld   ah, a
            if bcheck
                cp   (scraddr >> 8)|0x18
                case bcheck
                when true
                    if block_given?
                        jr   C, eoc
                        ns(**nsopts, &block)
                    else
                        ret  NC
                    end
                else
                    jp  NC, bcheck
                end
            end
        end
    end
    ##
    # Converts high byte screen address to attribute address
    #
    # Modifies: +af+, +o+
    #
    # * +s+: input register: hi byte screen address
    # * +o+: output register: hi byte attr address, may be same as +s+
    # * +scraddr+:: screen memory address as an integer, must be a multiple of 0x2000
    #
    # T-states: 
    #
    # * 34: when neither +s+ nor +o+ is the +a+ register
    # * 30: when +s+ is the +a+ register but not +o+
    # * 30: when +o+ is the +a+ register but not +s+
    # * 26: when each +s+ and +o+ is the +a+ register
    #
    # i < 0  1  0  2a 1a l  l  l,
    # o > 0  1  0  1  1  0  2a 1a
    def scrtoattr(s, o:s, scraddr:0x4000)
        raise ArgumentError, "scrtoattr invalid arguments!" unless [i, o].all?{|r| r.is_a?(Register)}
      attraddr = scraddr + 0x1800
      isolate do
            ld   a, s unless s == a
                            # a= S S S H H h h h
            anda 0b00011000 # a= 0 0 0 H H 0 0 0
            3.times {rrca}  # a= 0 0 0 0 0 0 H H
            ora  (attraddr >> 8)
                            # a= S S S 1 1 0 H H
            ld   o, a unless o == a
      end
    end

    def clear_screen_region_fast(address=hl, rows=c, cols=2, disable_interrupts:true, enable_interrupts:true, save_sp:true)
      cols = cols.to_i
      raise ArgumentError "cols must be even" if cols.odd?
      raise ArgumentError "cols must be less than or equal to 32" if cols > 32
      isolate do |eoc|
                      ld   [restore_sp + 1], sp if save_sp
                      ld   c, rows unless rows==c
                      di if disable_interrupts
                      ld   hl, address unless address == hl
                      ld   de, 0
                      ld   a, h        # calculate counter based on screen address modulo 8
                      anda 0b11111000 # (h & 0b11111000)
                      sub  h          # (h & 0b11111000) - h % 8
                      add  8          # 8 - h % 8
                      ld   b, a       # b: counter: 8 - h % 8
                      ld   a, c       # a: rows
                      dec  a          # a: rows - 1 (remaining rows)
                      sub  b          # a: rows - 1 - counter
                      jr   NC, loop1
                      ld   b, c       # b: counter = dy
        loop1         ld   sp, hl
                      inc  h
                      (cols>>1).times { push de }
                      djnz loop1
                      jr   C, quit
                      ex   af, af     # a': remaining rows
                      ld   a, l
                      add  0x20
                      ld   l, a
                      jr   C, skip_adj
                      ld   a, h
                      sub  0x08
                      ld   h, a
        skip_adj      ex   af, af     # a: remaining rows
                      ld   b, 8
                      sub  b
                      jr   NC, loop1
                      add  b
                      ld   b, a
                      inc  b
                      jp   loop1
        quit          label
        restore_sp    ld  sp, 0 if save_sp
                      ei if enable_interrupts
      end
    end
  end
  include Z80
end
