module ZXLib
  ##
  #  ==A module with Z80 Macros for common ZX Spectrum graphics tasks
  #
  #  Example:
  #
  #    require 'zxlib/gfx'
  #
  #    class Program
  #      include Z80
  #      macro_import  ZXLib::Gfx
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
  #
  #  ===The coordinate system.
  #
  #  All of the Gfx routines, including Bobs, Clip, Draw, and Sprite8, are using the following screen
  #  coordinates system:
  #
  #  * The coordinates (0,0) are indicating the top-left corner of the screen.
  #  * The vertical axis (y) is inversed - y increases towards the bottom of the screen.
  #    Hence e.g. 191 is the last visible pixel line and 23 is the last visible attributes cell row
  #    at the bottom.
  #  * The horizontal axis (x) is normal - x increases towards the right edge of the screen.
  #    Hence e.g. 255 is the last visible pixel column and 31 (or 63 in hi-res) is the last visible
  #    cell column on the far right.
  module Gfx
    ##
    #  ==ZXLib::Gfx macros.
    module Macros
      ##
      # Returns true if +v+ is a 0 or a positive integer with only one bit set in its binary representation.
      def only_one_bit_set_or_zero?(v)
          Integer === v && (v & (v - 1)) == 0
      end
      ##
      # Calculates a constant screen pixel byte address from the pixel coordinates.
      def xy_to_pixel_addr(x, y, scraddr:0x4000)
          (
              (
                  (
                      ((y & 0x07) << 3) | ((y & 0b111000) >> 3) | ((y & 0xffc0))
                  ) << 5
              ) | ((x & 0xff) >> 3)
          ) + scraddr
      end
      ##
      # Calculates a constant screen attribute address from the pixel coordinates.
      def xy_to_attr_addr(x, y, scraddr:0x4000)
          ( ( (y >> 3) << 5 ) | (x >> 3) ) + scraddr + 0x1800
      end
      ##
      # Creates a routine that changes a bit shift and the pixel address for a one pixel to the left.
      #
      # Modifies: +af+, +al+, +s+.
      #
      # * +al+:: A register holding the least significant byte of the pixel address to move.
      # * +s+:: A register holding a bit shift value of the pixel in the range: [0-7]
      #
      # T-states: 14/25.
      def prevpixel(al, s: a)
        unless register?(al) and al.bit8? and register?(s) and s.bit8? and al != s
          raise ArgumentError, "prevpixel: invalid arguments!"
        end
        isolate do |eoc|
                      dec  s
                      jp   P, eoc
                      ld   s, 7
                      dec  al
        end
      end
      ##
      # Creates a routine that changes a bit shift and the pixel address for a one pixel to the right.
      #
      # Modifies: +af+, +al+, +s+.
      #
      # * +al+:: A register holding the least significant byte of the pixel address to move.
      # * +s+:: A register holding a bit shift value of the pixel in the range: [0-7]
      #
      # T-states: 23/22 (s ≠ a: 31/30)
      def nextpixel(al, s: a)
        unless register?(al) and al.bit8? and register?(s) and s.bit8? and al != s
          raise ArgumentError, "nextpixel: invalid arguments!"
        end
        isolate do |eoc|
                      inc  s
                      ld   a, s unless s == a
                      anda 7
                      jr   NZ, eoc
                      ld   s, a unless s == a
                      inc  al
        end
      end
      ##
      # Creates a routine that advances to the next line (down) a screen address using ah|al registers.
      # Optionally returns from a subroutine if an address would advance beyond the screen area.
      #
      # Modifies: +af+, +ah+, +al+.
      #
      # * +ah+:: A register holding a high byte of a screen address to advance.
      # * +al+:: A register holding a low byte of a screen address to advance.
      # * +bcheck+:: Out of screen check flag:
      #              +false+ = disable checking,
      #              +true+  = issue +ret+ if out of screen,
      #              +label+ = jump to a label if out of screen,
      #              +hl+|+ix+|+iy+ = jump to an address in a register if out of screen.
      # * +scraddr+:: A screen memory address which must be a multiple of 0x2000 as an integer or a label,
      #               used only for an out of screen check.
      # * +hires+:: if +true+ the out of screen check will be valid on any of the hi-res screen pages,
      #             in this instance only 2 highest bits of +scraddr+ is important.
      #
      # If block is given and +bcheck+ is +true+ evaluates namespaced block instead of +ret+.
      # The code in the given block should not fall through and should end with a jump or a +ret+.
      #
      # T-states: 
      #
      # * when +bcheck+ is +false+:: 27:87.5% / 49:1.56% / 59:10.94%
      # * when +bcheck+ is +true+ or +label+::  27:87.5% / (70:2 / 75:1):1.56% / 62:10.94%
      def nextline(ah, al, bcheck = true, scraddr:0x4000, hires:false, **nsopts, &block)
          if ah == al or [ah, al].include?(a) or
                  (register?(bcheck) and ![hl_, ix_, iy_].include?(bcheck)) or
                  (bcheck == hl and ([h, l].include?(ah) or [h, l].include?(al))) or
                  (bcheck == ix and ([ixh, ixl].include?(ah) or [ixh, ixl].include?(al))) or
                  (bcheck == iy and ([iyh, iyl].include?(ah) or [iyh, iyl].include?(al))) or
                  ![ah, al].all?{|r| register?(r) } or 
                  (bcheck and !((Integer === scraddr and scraddr == (scraddr & 0xE000)) or direct_label?(scraddr)))
              raise ArgumentError, "nextline: invalid arguments!"
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
                  scrhiaddr = (scraddr >> 8)
                  scrhiaddr = (scrhiaddr|0x20) if hires
                      ld   a, ah
                      jp   NC, restrh
                      ora  0x20 if hires
                      cp   (scrhiaddr|0x18)
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
      # Creates a routine that moves up to the previous line a screen address using ah|al registers.
      # Optionally returns from a subroutine if an address would move out of the screen area.
      #
      # Modifies: +af+, +ah+, +al+.
      #
      # * +ah+:: A register holding a high byte of a screen address to move.
      # * +al+:: A register holding a low byte of a screen address to move.
      # * +bcheck+:: Out of screen check flag:
      #              +false+ = disable checking,
      #              +true+  = issue +ret+ if out of screen,
      #              +label+ = jump to a label if out of screen,
      #              +hl+|+ix+|+iy+ = jump to an address in a register if out of screen.
      # * +scraddr+:: A screen memory address which must be a multiple of 0x2000 as an integer or a label,
      #               used only for an out of screen check.
      # * +hires+:: if +true+ the out of screen check will be valid on any of the hi-res screen pages,
      #             in this instance only 2 highest bits of +scraddr+ is important.
      #
      # If block is given and +bcheck+ is +true+ evaluates namespaced block instead of +ret+.
      # The code in the given block should not fall through and should end with a jump or a +ret+.
      #
      # T-states: 
      #
      # * when +bcheck+ is +false+:: 27:87.5% / 49:1.56% / 59:10.94%
      # * when +bcheck+ is +true+ or +label+::  27:87.5% / (70:2 / 75:1):1.56% / 62:10.94%
      def prevline(ah, al, bcheck = true, scraddr:0x4000, hires:false, **nsopts, &block)
          if ah == al or [ah, al].include?(a) or
                  (register?(bcheck) and ![hl_, ix_, iy_].include?(bcheck)) or
                  (bcheck == hl and ([h, l].include?(ah) or [h, l].include?(al))) or
                  (bcheck == ix and ([ixh, ixl].include?(ah) or [ixh, ixl].include?(al))) or
                  (bcheck == iy and ([iyh, iyl].include?(ah) or [iyh, iyl].include?(al))) or
                  ![ah, al].all?{|r| register?(r) } or
                  (bcheck and !((Integer === scraddr and scraddr == (scraddr & 0xE000)) or direct_label?(scraddr)))
              raise ArgumentError, "prevline: invalid arguments!"
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
                  scrhiaddr = (scraddr >> 8)
                  scrhiaddr = (scrhiaddr|0x20) if hires
                      ld   a, ah
                      jp   NC, restrh
                      ora  0x20 if hires
                      cp   scrhiaddr
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
      # Creates a routine that converts y,x coordinates to a screen byte address and a bits shift.
      #
      # Modifies: +af+, +s+, +t+, +ah+, +al+.
      #
      # * +y+:: An 8-bit input register holding a vertical pixel coordinate.
      #         It must not be the same as the +s+ output register.
      # * +x+:: An 8-bit input register, except +accumulator+, holding a horizontal pixel coordinate.
      #         It must not be the same as +ah+, +s+ or +t+.
      #
      # Options:
      # * +ah+:: A register holding a high byte of a resulting address.
      # * +al+:: A register holding a low byte of a resulting address.
      # * +s+:: A register holding a resulting bits right shift: [0-7].
      #         This indicates how many bits the most significant bit should be shifted to the right to match the
      #         input coordinate x.
      # * +t+:: An 8-bit register for temporary operations.
      # * +scraddr+:: A screen memory address which must be a multiple of 0x2000 as an integer or a label.
      #
      # T-states: 101/104 depending on +scraddr+ (101 for 0x2000, 0x4000, 0x8000)
      #
      # y < a1 a2 h3 h2 h1 l3 l2 l1,  x < x5 x4 x3 x2 x1 s3 s2 s1,
      # h > S  S  S  a1 a2 l3 l2 l1,  l > h3 h2 h1 x5 x4 x3 x2 x1,  s > 0  0  0  0  0  s3 s2 s1
      def yxtoscr(y, x, ah:h, al:l, s:b, t:c, scraddr:0x4000)
          if y == x or y == s or [x,ah,al,s,t].include?(a) or [ah,al,s].uniq.size != 3 or [ah, s].include?(t) or 
                [ah, s, t].include?(x) or ![y, x, ah, al, s, t].all?{|r| register?(r) } or
                !((Integer === scraddr and scraddr == (scraddr & 0xE000)) or direct_label?(scraddr))
              raise ArgumentError, "yxtoscr: invalid arguments!"
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
      alias_method :xytoscr, :yxtoscr
      ##
      # Creates a routine that converts a vertical pixel coordinate to a screen byte address.
      #
      # Modifies: +af+, +ah+, +al+, +t+. +col+ only in +hires+ mode.
      #
      # * +y+:: An 8-bit input register holding a vertical pixel coordinate. It must not be the same as +t+.
      #
      # Options:
      # * +ah+:: A register holding a high byte of a resulting address.
      # * +al+:: A register holding a low byte of a resulting address.
      # * +col+:: An optional column number [0-31] ([0-63] in +hires+ mode) as a unique 8-bit register or
      #           an integer or a label.
      # * +t+:: An 8-bit register for temporary operations.
      # * +scraddr+:: A screen memory address which must be a multiple of 0x2000 as an integer or a label.
      # * +hires+:: Enable SCLD or ULAplus hi-res mode. If enabled, +col+ is interpreted as hi-res
      #             screen column and only 2 highest bits of +scraddr+ is being used to establish
      #             the primary screen memory address.
      #
      # T-states: 73/81/97/87 if +col+ is: +nil+/+register+/+register+ and +hires+/+number+.
      #
      # y < a1 a2 h3 h2 h1 l3 l2 l1,
      # h > S  S  S  a1 a2 l3 l2 l1,  l > h3 h2 h1 0  0  0  0  0
      def ytoscr(y, ah:h, al:l, col:nil, t:c, scraddr:0x4000, hires:false)
          if [ah,al,t].include?(a) or [ah,al,t].uniq.size != 3 or t == y or
                  ![y, ah, al, t].all?{|r| register?(r) } or
                  (register?(col) and [y, ah, al, t, a].include?(col)) or
                  (!col.nil? and !register?(col) and !address?(col)) or pointer?(col) or
                  !((Integer === scraddr and scraddr == (scraddr & 0xE000)) or direct_label?(scraddr))
              raise ArgumentError, "ytoscr: invalid arguments!"
          end
          scraddrhi = (scraddr>>8)
          scraddrhi = (scraddrhi & 0xC0) if hires
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
              if hires
                if register?(col)       # col= 0 0 c c c c c p
                      srl  col          # col= 0 0 0 c c c c c CF=page
                      rra               # a= p H H h h h 0 0
                      rlca              # a= H H h h h 0 0 p
                elsif col
                      scraddrhi = scraddrhi | ((col & 1) << 5)
                      col = (col >> 1)
                end
              end
                      rlca              # a= H h h h 0 0 p H
                      rlca              # a= h h h 0 0 p H H
                      ld   ah, a        # h= h h h 0 0 p H H
                      anda 0b11100000   # a= h h h 0 0 0 0 0
                      add  col if col
                      ld   al, a        # l= h h h c c c c c
                      sub  col if col   # a= h h h 0 0 0 0 0
                      xor  ah           # a= 0 0 0 0 0 p H H
                      3.times { rlca }
                      ora  t            # a= 0 0 p H H l l l
              unless scraddr == 0
                      ora  scraddrhi    # a= S S p H H l l l
              end
                      ld   ah, a        # h= S S p H H l l l
          end
      end
      ##
      # Creates a routine that converts row and column coordinates to a byte address of a top 8-pixel line.
      #
      # Modifies: +af+, +ah+, +al+.
      #
      # * +row+:: An 8-bit register as a text row [0-23]. +row+ must not be the same as +ah+.
      # * +col+:: An 8-bit register, except +accumulator+, as a text column [0-31] or an integer or a label.
      #           If it's a register then it must not be the same as +ah+.
      #           If +row+ is +accumulator+ then +col+ must not be the same as +al+.
      #
      # Options:
      # * +ah+:: A register holding a high byte of a resulting address.
      # * +al+:: A register holding a low byte of a resulting address.
      # * +scraddr+:: A screen memory address which must be a multiple of 0x2000 as an integer or a label.
      #
      # T-states: 43
      #
      # * T + 4: if +col+ is a register.
      # * T + 7: if +col+ is a non-zero integer.
      # * T + 6: if +scraddr+ has more than one bit set.
      #
      # r < 0  0  0  5r 4r 3r 2r 1r,  c < 0  0  0  5c 4c 3c 2c 1c,
      # h > 0  1  0  5r 4r 0  0  0,   l < 3r 2r 1r 5c 4c 3c 2c 1c
      def rctoscr(row, col=0, ah:h, al:l, scraddr:0x4000)
        unless register?(ah) and ah.bit8? and register?(al) and al.bit8? and ah != al and ![col, ah, al].include?(a) and
               ((address?(col) and !pointer?(col)) or (register?(col) and col != ah and col != row and (col != al or row != a))) and
               ((Integer === scraddr and scraddr == (scraddr & 0xE000)) or direct_label?(scraddr))
          raise ArgumentError, "rctoscr: invalid arguments!"
        end
        isolate do
          if row == a
                      ld   al, a
          else
                      ld   a, row
          end
                      anda 0b00011000 # a= 0  0  0  H  H  0  0  0
                      ora  (scraddr>>8) unless only_one_bit_set_or_zero?(scraddr)
                      ld   ah, a      # h= S  S  S  H  H  0  0  0
                      xor  (scraddr>>8) unless only_one_bit_set_or_zero?(scraddr)
          if row == a
                      xor  al         # a= 0  0  0  0  0  r  r  r
          else
                      xor  row        # a= 0  0  0  0  0  r  r  r
          end
                      3.times {rrca}  # a= r  r  r  0  0  0  0  0
                      ora  col unless col == 0
                      ld   al, a      # l= r  r  r  c  c  c  c  c
              if only_one_bit_set_or_zero?(scraddr)
                  nbit = Math.log2(scraddr>>8).to_i
                      set  nbit, ah   # h= S  S  S  H  H  0  0  0
              end unless scraddr == 0
        end
      end
      ##
      # Creates a routine that converts row and column coordinates to an address of a color attribute.
      #
      # Modifies: +af+, +ah+, +al+.
      #
      # * +row+:: An 8-bit register as a text row [0-23].
      # * +col+:: An 8-bit register, except +accumulator+, as a text column [0-31] or an integer or a label.
      #           If it's a register then it must not be the same as +ah+.
      #
      # Options:
      # * +ah+:: A register holding a high byte of a resulting address.
      # * +al+:: A register holding a low byte of a resulting address.
      # * +scraddr+:: A screen memory address which must be a multiple of 0x2000 as an integer or a label.
      #
      # T-states: 46/54/57/60 for +col+: +0+/+register+/+al+/+number+, T-4 if +row+ is +accumulator+.
      #
      # r < 0  0  0  5r 4r 3r 2r 1r,  c < 0  0  0  5c 4c 3c 2c 1c,
      # h > 0  1  0  1  1  0  5r 4r,  l < 3r 2r 1r 5c 4c 3c 2c 1c
      def rctoattr(row, col=0, ah:h, al:l, scraddr:0x4000)
        unless register?(ah) and ah.bit8? and register?(al) and al.bit8? and ah != al and ![col, ah, al].include?(a) and
               ((address?(col) and !pointer?(col)) or (register?(col) and col != ah and col != row)) and
               ((Integer === scraddr and scraddr == (scraddr & 0xE000)) or direct_label?(scraddr))
          raise ArgumentError, "rctoattr: invalid arguments!"
        end
        attraddr = scraddr + 0x1800
        isolate do
              ld   a, row unless row == a
              3.times {rrca}
              ld   ah, a               # h= L L L x x x H H
              anda 0b11100000          # a= L L L 0 0 0 0 0
              add  col unless col == 0 # a= L L L C C C C C
              ld   al, a               # l= L L L C C C C C
          if register?(col) && col != al
              sub  col unless col == 0 # a= L L L 0 0 0 0 0
              xor  ah                  # a= 0 0 0 x x x H H
          else
              ld   a, ah               # h= L L L x x x H H
              anda 0b00000011          # h= 0 0 0 0 0 0 H H
          end
              ora  (attraddr>>8)       # a= S S S 1 1 x H H
              ld   ah, a               # h= S S S 1 1 x H H
        end
      end
      ##
      # Creates a routine that advances to the next text row (down 8 pixels) a screen address using ah|al registers.
      # Optionally returns from a subroutine if an address would advance beyond the screen area.
      #
      # Modifies: +af+, +ah+, +al+.
      #
      # * +ah+:: A register holding a high byte of a screen address to advance.
      # * +al+:: A register holding a low byte of a screen address to advance.
      # * +bcheck+:: Out of screen check flag:
      #              +false+ = disable checking,
      #              +true+  = issue +ret+ if out of screen,
      #              +label+ = jump to label if out of screen,
      #              +hl+|+ix+|+iy+ = jump to an address in a register if out of screen.
      # * +scraddr+:: A screen memory address which must be a multiple of 0x2000 as an integer or a label.
      #
      # If block is given and +bcheck+ is +true+ evaluates namespaced block instead of +ret+.
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
                  !((Integer === scraddr and scraddr == (scraddr & 0xE000)) or direct_label?(scraddr))
            raise ArgumentError, "nextrow: invalid arguments!"
          end
          ns do |eoc|
                  ld   a, al
                  add  0x20
                  ld   al, a
                  jr   NC, eoc
                  ld   a, ah
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
      # Creates a routine that converts a high byte of a pixel address to a high byte of an address of a relevant attribute.
      #
      # Modifies: +af+, +o+.
      #
      # * +s+:: A register holding a high byte of an address to convert.
      #
      # Options:
      # * +o+:: A register holding a high byte of a resulting address. +o+ is the same as +s+ by default.
      # * +scraddr+:: A screen memory address which must be a multiple of 0x2000 as an integer or a label.
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
          unless [i, o].all?{|r| register?(r) } and
                 ((Integer === scraddr and scraddr == (scraddr & 0xE000)) or direct_label?(scraddr))
            raise ArgumentError, "scrtoattr: invalid arguments!" 
          end
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
      ##
      # Creates a routine that clears a rectangle on an ink/paper screen using unrolled PUSH instructions.
      #
      # _NOTE_:: Interrupts must be disabled prior to calling this routine or the +disable_intr+
      #          option must be set to +true+.
      #
      # * +address+:: An addres of a memory area to be cleared as a label, pointer, an integer or +hl+.
      #               The interpretation of the given address depends on the +addr_mode+ option.
      #               The starting address of the whole screen area must be a multiple of 0x2000.
      # * +lines+:: A number of pixel lines to be cleared as an 8-bit register or a label, pointer or an integer.
      # * +cols+:: A constant number of 8 pixel columns to be cleared as an integer: 1 to 32.
      # * +value+:: A pattern of 16 pixels to fill each line with as a label, pointer, an integer or +de+.
      #             For the odd number of columns the first and the last column will be the pattern's least
      #             significant byte.
      #
      # Options:
      # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
      #                    only if you have already disabled the interrupts.
      # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
      #                   if you need to perform more uninterrupted actions.
      # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
      #               +sp+ will point to the beginning of the last cleared line.
      # * +addr_mode+:: Determines the interpretation of the given +address+. See below.
      # * +scraddr+:: An optional screen memory address which must be a multiple of 0x2000 as an integer or a label.
      #               If provided the routine breaks execution when the bottom of the screen has been reached.
      # * +subroutine+:: Whether to create a subroutine.
      #
      # +addr_mode+ should be one of:
      #
      # * +:last+:: The +address+ should point to the last column of the topmost line of the area to be cleared.
      #             This is the fastest mode as the address needs to be adjusted in other modes.
      # * +:first+:: The +address+ should point to the first column of the topmost line of the area to be cleared.
      # * +:compat+:: The +address+ should point to the next byte after the last column of the topmost line of the
      #               area to be cleared.
      #
      # _NOTE_:: Restoring +sp+ register uses self-modifying code.
      #
      # Modifies: +af+, +af'+, +bc+, +de+, +hl+, optionally: +sp+.
      def clear_screen_region_fast(address=hl, lines=c, cols=2, value=0, disable_intr:true, enable_intr:true, save_sp:true, addr_mode: :compat, scraddr:nil, subroutine:false)
        raise ArgumentError, "invalid scraddr argument" unless scraddr.nil? or (Integer === scraddr and scraddr == (scraddr & 0xE000)) or direct_label?(scraddr)
        raise ArgumentError, "address should be an address or a pointer or hl" unless address == hl or address?(address)
        raise ArgumentError, "lines should be an integer or a label or a pointer or a register" unless (register?(lines) and lines.bit8?) or
                                                                                                       address?(lines)
        cols = cols.to_i
        raise ArgumentError, "cols must be less than or equal to 32" if cols > 32
        raise ArgumentError, "cols must be greater than or equal to 1" if cols < 1
        raise ArgumentError, "value should be an integer or a label or a pointer or de" unless value == de or address?(value)
        unless addr_mode.nil? or [:compat, :first, :last].include?(addr_mode)
          raise ArgumentError, "addr_mode should be either :compat, :first or :last"
        end
        save_sp = false if cols == 1
        fits_single_row = false
        if direct_address?(address)
          case addr_mode
          when :first
            address = address + cols - 1
          when :last
          else
            address = address - 1
          end
          fits_single_row = Integer === address && Integer === lines && lines <= (8 - (address>>8) % 8)
        end
        const_addr_not_right_edge = Integer === address && (address & 31) != 31

        isolate do
                        ld   c, lines if register?(lines) && lines != c
                        ld   de, value unless value == de

          if direct_address?(address)
            if (const_addr_not_right_edge or fits_single_row) and cols.even?
                        ld   hl, address + 1
            else
                        ld   hl, address
            end
            if fits_single_row
                        ld   b, lines
            else
                        ld   b, 8 - (address>>8) % 8
            end
          else
                        ld   hl, address unless address == hl
            if addr_mode == :first
              if cols <= 4
                        (cols-1).times { inc l }
              else
                        ld   a, cols-1
                        add  l
                        ld   l, a
              end
            elsif addr_mode != :last
                        dec  hl
            end
                        ld   a, h       # calculate counter based on screen address modulo 8
                        anda 0b11111000 # (h & 0b11111000)
                        sub  h          # (h & 0b11111000) - h % 8
                        add  8          # 8 - h % 8
                        ld   b, a       # b: counter: 8 - h % 8
          end
          # if (const_addr_not_right_edge or fits_single_row) and cols.even?
          #   hl: -> last screen column + 1
          # else
          #   hl: -> last screen column
          unless fits_single_row
            if register?(lines)
                        ld   a, c       # a: lines
                        dec  a          # a: lines - 1 (remaining lines)
            elsif pointer?(lines)
                        ld   a, lines
                        ld   c, a
                        dec  a          # a: lines - 1 (remaining lines)
            else
                        ld   a, lines - 1
            end
                        sub  b          # a: lines - 1 - counter
            unless Integer === lines && lines > 8
              ns do |eoc|
                        jr   NC, eoc
                if register?(lines) || pointer?(lines)
                        ld   b, c       # b: counter = dy
                else
                        ld   b, lines
                end
              end
            end
          end
                        di if disable_intr
                        ld   [restore_sp_p], sp if save_sp
          loop0         label
                        inc  hl unless const_addr_not_right_edge or fits_single_row or cols.odd?
          loop1         label
                        ld   [hl], e if cols.odd?
                        ld   sp, hl unless cols == 1
                        inc  h
                        (cols >> 1).times { push de }
                        djnz loop1
          unless fits_single_row
            if subroutine && !enable_intr && !save_sp
                        ret  C
            else
                        jr   C, quit
            end
                        dec  hl unless const_addr_not_right_edge or cols.odd?
                        ex   af, af     # a': remaining lines
                        ld   a, l
                        add  0x20
                        ld   l, a
                        jr   C, skip_adj unless scraddr
                        ld   a, h
                        jr   C, check_oos if scraddr
                        sub  0x08
                        ld   h, a
            skip_adj    ex   af, af     # a: remaining lines
                        ld   b, 8
                        sub  b
                        jr   NC, loop0
                        add  b
                        ld   b, a
                        inc  b
                        jp   loop0
            if scraddr
              check_oos cp   (scraddr >> 8)|0x18
                        jr   C, skip_adj
                        ret if subroutine && !enable_intr && !save_sp
            end
            quit        label unless subroutine && !enable_intr && !save_sp
          end
          if save_sp
          restore_sp    ld   sp, 0
          restore_sp_p  restore_sp + 1
          end
                        ei if enable_intr
                        ret if subroutine && (enable_intr || save_sp || fits_single_row)
        end
      end
      ##
      # Creates a routine that copies a rectangle of an ink/paper screen from or to a shadow screen.
      #
      # * +address+:: An address of a top-left corner of the screen memory area to be copied to as a label, pointer,
      #               or +de+.
      # * +lines+:: A number of pixel lines to be copied as an 8-bit register or a label or a pointer.
      # * +cols+:: A number of 8 pixel columns to be copied as an 8-bit register or a label or a pointer.
      #            If +cols+ is +a+ the +cols+ is taken from +a'+.
      #
      # _NOTE_:: Unless +cols+ is one of: +ixh+, +ixl+, +iyh+ or +iyl+ the routine uses self modifying code.
      #
      # Options:
      # * +tgtaddr+:: A target screen memory address which must be a multiple of 0x2000 as an integer or a label.
      # * +srcaddr+:: A source screen memory address which must be a multiple of 0x2000 as an integer or a label.
      # * +check_edge+:: Ensures that the region does not transgress the right edge of the screen decreasing
      #                  +address+ if necessary. Applies only if +address+ is not static.
      # * +break_oos+:: Breaks execution when the bottom of the screen has been reached.
      #                 +CF+ = 0 (NC) if the routine terminates prematurely due to reaching bottom of the screen.
      #                 Otherwise +CF+ = 1 if the whole rectangle has been copied.
      # * +subroutine+:: Whether to create a subroutine.
      #
      # Modifies: +af+, +af'+, +bc+, +bc'+, +de+, +hl+. Swaps registers unless out of screen.
      def copy_shadow_screen_region(address=de, lines=a, cols=c, tgtaddr:0x4000, srcaddr:0x6000, check_edge:true, break_oos:true, subroutine:false)
        raise ArgumentError, "invalid tgtaddr argument" unless (Integer === tgtaddr and tgtaddr == (tgtaddr & 0xE000)) or direct_label?(tgtaddr)
        raise ArgumentError, "invalid srcaddr argument" unless (Integer === srcaddr and srcaddr == (srcaddr & 0xE000)) or direct_label?(srcaddr)
        scrdiff = (srcaddr - tgtaddr) & 0xffff
        raise ArgumentError, "tgtaddr must be not be the same as srcaddr" if Integer === scrdiff and scrdiff == 0
        raise ArgumentError, "address should be an address or a pointer or de" unless address == de or address?(address)
        raise ArgumentError, "lines should be a label or a pointer or a register" unless (register?(lines) and lines.bit8?) or
                                                                                        address?(lines)
        raise ArgumentError, "cols should be a label or a pointer or a register" unless (register?(lines) and lines.bit8?) or
                                                                                        address?(lines)
        isolate do |eoc|
                          ld   a, lines unless lines == a
                          ex   af, af     # a': lines
          unless [ixh,ixl,iyh,iyl].include?(cols)
                          ld   a, cols unless cols == a
                          ld   [cols_p], a
                          ld   c, a if check_edge && cols != c
          end
                          ld   b, 0
                          ld   de, address unless address == de
          if direct_address?(address)
                          ld   a, d
                          add  scrdiff>>8
                          ld   h, a
                          ld   a, e
                          exx
                          ld   b, 8 - (address>>8) % 8
          else
            if check_edge
              ns do |eoc|
                          ld   a, e
                          ora  ~31
                if [ixh,ixl,iyh,iyl].include?(cols)
                          add  cols
                else
                          add  c
                end
                          jr   NC, eoc
                          cpl
                          adc  e
                          ld   e, a
              end
            end
                          ld   a, d       # calculate counter based on screen address modulo 8
                          add  scrdiff>>8
                          ld   h, a
                          anda 0b11111000 # (h & 0b11111000)
                          sub  h          # (h & 0b11111000) - h % 8
                          add  8          # 8 - h % 8
                          exx
                          ld   b, a       # b: counter: 8 - h % 8
                          exx
                          ld   a, e
                          exx
          end
                          ex   af, af     # a: lines, a': lo
                          ld   c, a       # c: lines
                          dec  a          # a: lines - 1 (remaining lines)
                          sub  b          # a: lines - 1 - counter
                          jr   NC, start
                          ld   b, c       # b: counter = c: lines

          start           ex   af, af     # a': remaining rows - 1; CF': 1 == last batch, a: lo
          rloop           exx
          loop0           ld   e, a
          loop1           ld   l, a
          if [ixh,ixl,iyh,iyl].include?(cols)
                          ld   c, cols
          else
            cols_a        ld   c, 0 # self-modified
            cols_p        cols_a + 1
          end
                          ldir
                          dec  de
                          inc  d
                          dec  hl
                          inc  h
                          exx
                          djnz rloop
                          ex   af, af     # a: remaining lines, a': lo
          if subroutine
                          ret  C
          else
                          jr   C, eoc
          end
                          ld   b, 8
                          sub  b
                          jr   NC, loop8
                          add  b
                          ld   b, a
                          inc  b

          loop8           exx
                          ex   af, af     # a: lo, a': remaining lines
                          add  0x20       # a: lo + 0x20
                          jr   C, loop0 unless break_oos
                          ld   e, a       # e: lo
                          ld   a, d
                          jr   C, check_ooscr if break_oos
                          sub  0x08
                          ld   d, a       # d: adjusted
                          add  scrdiff>>8
                          ld   h, a
                          ld   a, e
                          jp   loop1
          if break_oos
            check_ooscr   cp   (tgtaddr >> 8)|0x18
                          ld   a, e
                          jr   C, loop1
                          ret if subroutine
          end
        end
      end
      ##
      # Creates a routine that copies a rectangle of screen attributes from or to a shadow screen.
      #
      # * +address+:: An address of a top-left corner of the attributes memory area to be copied to as a label, pointer,
      #               an integer or +de+.
      # * +rows+:: A number of attribute rows to be copied as an 8-bit register or a label or a pointer.
      # * +cols+:: A number of attribute columns to be copied as as an 8-bit register or a label or a pointer.
      #            If +cols+ is +a+ the +cols+ is taken from +a'+.
      #
      # _NOTE_:: Unless +cols+ is one of: +ixh+, +ixl+, +iyh+ or +iyl+ the routine uses self modifying code.
      #
      # Options:
      # * +tgtaddr+:: A target screen memory address which must be a multiple of 0x2000 as an integer or a label.
      # * +srcaddr+:: A source screen memory address which must be a multiple of 0x2000 as an integer or a label.
      # * +check_edge+:: Ensures that the region does not transgress the right edge of the screen decreasing
      #                  +address+ if necessary. Applies only if +address+ is not static.
      # * +break_oos+:: Breaks execution when the bottom of the screen has been reached.
      # * +subroutine+:: Whether to create a subroutine.
      #
      # Modifies: +af+, +af'+, +bc'+, +bc+, +de+, +hl+. Swaps registers unless out of screen.
      def copy_shadow_attrs_region(address=de, rows=a, cols=c, tgtaddr:0x4000, srcaddr:0x6000, check_edge:true, break_oos:true, subroutine:false)
        raise ArgumentError, "invalid tgtaddr argument" unless (Integer === tgtaddr and tgtaddr == (tgtaddr & 0xE000)) or direct_label?(tgtaddr)
        raise ArgumentError, "invalid srcaddr argument" unless (Integer === srcaddr and srcaddr == (srcaddr & 0xE000)) or direct_label?(srcaddr)
        scrdiff = (srcaddr - tgtaddr) & 0xffff
        raise ArgumentError, "tgtaddr must be not be the same as srcaddr" if Integer === scrdiff and scrdiff == 0
        raise ArgumentError, "address should be an address or a pointer or de" unless address == de or address?(address)
        raise ArgumentError, "rows should be a label or a pointer or a register" unless (register?(rows) and rows.bit8?) or address?(rows)
        raise ArgumentError, "cols should be a label or a pointer or a register" unless (register?(cols) and cols.bit8?) or address?(cols)
        isolate do |eoc|
                          ld   a, rows unless rows == a
                          ex   af, af
                          ld   a, cols unless cols == a
          unless [ixh,ixl,iyh,iyl].include?(cols)
                          ld   [cols_p], a
                          ld   c, a if check_edge && cols != c
          end
                          exx
                          cpl
                          add  33
                          ld   c, a       # 32 - cols
                          ex   af, af     # a: rows
                          ld   b, a
                          exx
                          ld   de, address unless address == de
          if check_edge
            ns do |eoc|
                          ld   a, e
                          ora  ~31
              if [ixh,ixl,iyh,iyl].include?(cols)
                          add  cols
              else
                          add  c
              end
                          jr   NC, eoc
                          cpl
                          adc  e
                          ld   e, a
            end
          end
                          ld   b, 0
                          ld   a, d
                          jr   start0

          rowloop         ld   a, c # 32 - cols
                          exx
                          adda_to d, e
          if break_oos
                          cp   (tgtaddr>>8)|0x1B
            if subroutine
                          ret  NC
            else
                          jr   NC, eoc
            end
          end
          start0          add  scrdiff>>8
                          ld   h, a
                          ld   l, e
          if [ixh,ixl,iyh,iyl].include?(cols)
                          ld   c, cols
          else
            cols_a        ld   c, 0 # self-modified
            cols_p        cols_a + 1
          end
                          ldir
                          exx
                          djnz rowloop
                          ret if subroutine
        end
      end
      ##
      # Creates a routine that copies a rectangle of an ink/paper screen from or to a shadow screen using unrolled
      # LDI instructions.
      #
      # * +address+:: An address of a top-left corner of the screen memory area to be copied to as a label, pointer,
      #               an integer or +de+.
      # * +lines+:: A number of pixel lines to be copied as an 8-bit register or a label, pointer or an integer.
      # * +cols+:: A constant number of 8 pixel columns to be copied as an integer.
      #
      # Options:
      # * +tgtaddr+:: A target screen memory address which must be a multiple of 0x2000 as an integer or a label.
      # * +srcaddr+:: A source screen memory address which must be a multiple of 0x2000 as an integer or a label.
      # * +check_edge+:: Ensures that the region does not transgress the right edge of the screen decreasing
      #                  +address+ if necessary. Applies only if +address+ is not static.
      # * +break_oos+:: Breaks execution when the bottom of the screen has been reached.
      # * +size_limit_opt+:: If enabled, a small optimization is applied but the following condition must be met:
      #                      <tt>lines*(cols-1) < 256</tt>
      # * +subroutine+:: Whether to create a subroutine.
      #
      # Modifies: +af+, +af'+, +bc+, +de+, +hl+.
      def copy_shadow_screen_region_quick(address=de, lines=c, cols=32, tgtaddr:0x4000, srcaddr:0x6000, check_edge:true, break_oos:true, size_limit_opt:false, subroutine:false)
        raise ArgumentError, "invalid tgtaddr argument" unless (Integer === tgtaddr and tgtaddr == (tgtaddr & 0xE000)) or direct_label?(tgtaddr)
        raise ArgumentError, "invalid srcaddr argument" unless (Integer === srcaddr and srcaddr == (srcaddr & 0xE000)) or direct_label?(srcaddr)
        scrdiff = (srcaddr - tgtaddr) & 0xffff
        raise ArgumentError, "tgtaddr must be not be the same as srcaddr" if Integer === scrdiff and scrdiff == 0
        scrxor = if Integer === srcaddr and Integer === tgtaddr
          srcaddr ^ tgtaddr
        end
        raise ArgumentError, "address should be an address or a pointer or de" unless address == de or address?(address)
        raise ArgumentError, "lines should be an integer or a label or a pointer or a register" unless (register?(lines) and lines.bit8?) or
                                                                                                       address?(lines)
        cols = cols.to_i
        raise ArgumentError, "cols must be less than or equal to 32" if cols > 32
        raise ArgumentError, "cols must be greater than or equal to 1" if cols < 1
        fits_single_row = Integer === address && Integer === lines && lines <= (8 - (address>>8) % 8)
        isolate do |eoc|
                          ld   c, lines if register?(lines) && lines != c          
          if direct_address?(address)
                          ld   de, address
            if only_one_bit_set_or_zero?(scrxor)
              scrbitdiff = Math.log2(scrxor>>8).to_i
                          ld   h, d
              if tgtaddr < srcaddr
                          set  scrbitdiff, h
              else
                          res  scrbitdiff, h
              end
            else
                          ld   a, d
                          add  scrdiff>>8
                          ld   h, a
            end
            if fits_single_row
                          ld   b, lines
            else
                          ld   b, 8 - (address>>8) % 8
            end
          else
                          ld   de, address unless address == de
            if check_edge
              ns do |eoc|
                          ld   a, e
                          ora  ~31
                          add  cols
                          jr   NC, eoc
                          cpl
                          adc  e
                          ld   e, a
              end
            end
                          ld   a, d       # calculate counter based on screen address modulo 8
                          add  scrdiff>>8
                          ld   h, a
                          anda 0b11111000 # (h & 0b11111000)
                          sub  h          # (h & 0b11111000) - h % 8
                          add  8          # 8 - h % 8
                          ld   b, a       # b: counter: 8 - h % 8
          end

          unless fits_single_row
            if register?(lines)
                          ld   a, c       # a: lines
                          dec  a          # a: lines - 1 (remaining lines)
            elsif pointer?(lines)
                          ld   a, lines
                          ld   c, a
                          dec  a          # a: lines - 1 (remaining lines)
            else
                          ld   a, lines - 1
            end
                          sub  b          # a: lines - 1 - counter
            unless Integer === lines && lines > 8
              ns do |eoc|
                          jr   NC, eoc
                if register?(lines) || pointer?(lines)
                          ld   b, c       # b: counter = dy
                else
                          ld   b, lines
                end
              end
            end
                          ex   af, af     # a': remaining rows - 1; CF': 1 == last batch
          end
                          ld   c, 255 if size_limit_opt
                          ld   a, e
          copy_loop       ld   e, a
          copy_loop1      ld   l, a
                          ld   c, 255 unless size_limit_opt
                          (cols-1).times { ldi }
                          ld   l, [hl]
                          ex   de, hl
                          ld   [hl], e
                          ex   de, hl
                          inc  d
                          inc  h
                          djnz copy_loop
          if fits_single_row
                          ret if subroutine
          else
                          ex   af, af     # a': lo; a: remaining rows - 1; CF: 1 == last batch
            if subroutine
                          ret  C
            else
                          jr   C, eoc
            end
                          ld   b, 8
                          sub  b
                          jr   NC, copy_loop8
                          add  b
                          ld   b, a
                          inc  b

            copy_loop8    ex   af, af     # a': remaining rows - 1; CF': 1 == last batch, a: lo
                          add  0x20       # a: lo + 0x20
                          jr   C, copy_loop unless break_oos
                          ld   e, a       # e: lo
                          ld   a, d
                          jr   C, check_ooscr if break_oos
                          sub  0x08
                          ld   d, a       # d: adjusted
                          add  scrdiff>>8
                          ld   h, a
                          ld   a, e
                          jp   copy_loop1
            if break_oos
              check_ooscr cp   (tgtaddr >> 8)|0x18
                          ld   a, e
                          jr   C, copy_loop1
                          ret if subroutine
            end
          end
        end
      end
      ##
      # Creates a routine that copies a rectangle of screen attributes from or to a shadow screen using unrolled
      # LDI instructions.
      #
      # * +address+:: An address of a top-left corner of the attributes memory area to be copied to as a label, pointer,
      #               an integer or +de+.
      # * +rows+:: A number of attribute rows to be copied as an 8-bit register or a label, pointer or an integer.
      # * +cols+:: A constant number of attribute columns to be copied as an integer.
      #
      # Options:
      # * +tgtaddr+:: A target screen memory address which must be a multiple of 0x2000 as an integer or a label.
      # * +srcaddr+:: A source screen memory address which must be a multiple of 0x2000 as an integer or a label.
      # * +check_edge+:: Ensures that the region does not transgress the right edge of the screen decreasing
      #                  +address+ if necessary. Applies only if +address+ is not static.
      # * +break_oos+:: Breaks execution when the bottom of the screen has been reached.
      # * +size_limit_opt+:: If enabled, a small optimization is applied but the following condition must be met:
      #                      <tt>rows*cols < 256</tt>
      # * +subroutine+:: Whether to create a subroutine.
      #
      # Modifies: +af+, +bc+, +de+, +hl+.
      def copy_shadow_attrs_region_quick(address=de, rows=b, cols=32, tgtaddr:0x4000, srcaddr:0x6000, check_edge:true, break_oos:true, size_limit_opt:false, subroutine:false)
        raise ArgumentError, "invalid tgtaddr argument" unless (Integer === tgtaddr and tgtaddr == (tgtaddr & 0xE000)) or direct_label?(tgtaddr)
        raise ArgumentError, "invalid srcaddr argument" unless (Integer === srcaddr and srcaddr == (srcaddr & 0xE000)) or direct_label?(srcaddr)
        scrdiff = (srcaddr - tgtaddr) & 0xffff
        raise ArgumentError, "tgtaddr must be not be the same as srcaddr" if Integer === scrdiff and scrdiff == 0
        scrxor = if Integer === srcaddr and Integer === tgtaddr
          srcaddr ^ tgtaddr
        end
        raise ArgumentError, "address should be an address or a pointer or de" unless address == de or address?(address)
        raise ArgumentError, "rows should be an integer or a label or a pointer or a register" unless (register?(rows) and rows.bit8?) or
                                                                                                       address?(rows)
        cols = cols.to_i
        raise ArgumentError, "cols must be less than or equal to 32" if cols > 32
        raise ArgumentError, "cols must be greater than or equal to 1" if cols < 1
        fits_single_row = Integer === address && Integer === rows && rows == 1
        isolate do |eoc|
          unless fits_single_row
            if pointer?(rows)
                          ld   a, rows
                          ld   b, a
            else
                          ld   b, rows unless rows == b
            end
                          ld   c, 255 if size_limit_opt
          end
                          ld   de, address unless address == de
          if check_edge
            ns do |eoc|
                          ld   a, e
                          ora  ~31
                          add  cols
                          jr   NC, eoc
                          cpl
                          adc  e
                          ld   e, a
            end
          end
          if only_one_bit_set_or_zero?(scrxor)
            scrbitdiff = Math.log2(scrxor>>8).to_i
                          ld   h, d
            if tgtaddr < srcaddr
                          set  scrbitdiff, h
            else
                          res  scrbitdiff, h
            end
            if cols == 32 or fits_single_row
                          ld   l, e
            else
                          jr   start1
            end
          else
                          ld   a, d
            if cols == 32 or fits_single_row
                          add  scrdiff>>8
                          ld   h, a
                          ld   l, e
            else
                          jr   start0
            end
          end
          rowloop         label
          unless cols == 32 or fits_single_row
                          ld   a, 32-cols
                          adda_to d, e
            if break_oos
                          cp   (tgtaddr>>8)|0x1B
              if subroutine
                          ret  NC
              else
                          jr   NC, eoc
              end
            end
            start0        add  scrdiff>>8
                          ld   h, a
            start1        ld   l, e
          end
                          ld   c, 255 unless size_limit_opt || fits_single_row
                          cols.times { ldi }
                          djnz rowloop unless fits_single_row
                          ret if subroutine
        end
      end
    end

    include Z80
  end
end

# DEPRECATED
ZXGfx = ZXLib::Gfx unless defined?(ZXGfx) # :nodoc:
