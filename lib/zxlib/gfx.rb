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
      #
      # If block is given and +bcheck+ is +true+ evaluates namespaced block instead of +ret+.
      # The code in the given block should not fall through and should end with a jump or a +ret+.
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
                  (bcheck and !(label?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000))))
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
      #
      # If block is given and +bcheck+ is +true+ evaluates namespaced block instead of +ret+.
      # The code in the given block should not fall through and should end with a jump or a +ret+.
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
                  (bcheck and !(label?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000))))
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
      def xytoscr(y, x, ah:h, al:l, s:b, t:c, scraddr:0x4000)
          if y == x or y == s or [x,ah,al,s,t].include?(a) or [ah,al,s,t].uniq.size != 4 or
                [ah, s, t].include?(x) or ![y, x, ah, al, s, t].all?{|r| register?(r) } or
                !(label?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000)))
              raise ArgumentError, "xytoscr: invalid arguments!"
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
      # Creates a routine that converts a vertical pixel coordinate to a screen byte address.
      #
      # Modifies: +af+, +ah+, +al+, +t+.
      #
      # * +y+:: An 8-bit input register holding a vertical pixel coordinate. It must not be the same as +t+.
      #
      # Options:
      # * +ah+:: A register holding a high byte of a resulting address.
      # * +al+:: A register holding a low byte of a resulting address.
      # * +col+:: An optional column number [0-31] as a unique 8-bit register or an integer or a label.
      # * +t+:: An 8-bit register for temporary operations.
      # * +scraddr+:: A screen memory address which must be a multiple of 0x2000 as an integer or a label.
      #
      # T-states: 73/81/87 if +col+ is: +nil+/+register+/+number+.
      #
      # y < a1 a2 h3 h2 h1 l3 l2 l1,
      # h > S  S  S  a1 a2 l3 l2 l1,  l > h3 h2 h1 0  0  0  0  0
      def ytoscr(y, ah:h, al:l, col:nil, t:c, scraddr:0x4000)
          if [ah,al,t].include?(a) or [ah,al,t].uniq.size != 3 or t == y or
                  ![y, ah, al, t].all?{|r| register?(r) } or
                  (register?(col) and [y, ah, al, t, a].include?(col)) or
                  (!col.nil? and !register?(col) and !address?(col)) or pointer?(col) or
                  !(label?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000)))
              raise ArgumentError, "ytoscr: invalid arguments!"
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
                      ld   ah, a        # h= h h h 0 0 0 H H
                      anda 0b11100000   # a= h h h 0 0 0 0 0
                      add  col if col
                      ld   al, a        # l= h h h c c c c c
                      sub  col if col   # a= h h h 0 0 0 0 0
                      xor  ah           # a= 0 0 0 0 0 0 H H
                      3.times { rlca }
                      ora  t            # a= 0 0 0 H H l l l
              unless scraddr == 0
                      ora  (scraddr>>8) # a= S S S H H l l l
              end
                      ld   ah, a        # h= S S S H H l l l
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
               (label?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000)))
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
               (label?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000)))
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
                  !(label?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000)))
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
                 (label?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000)))
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
      # Creates a routine that clears a rectangle on an ink/paper screen memory using unrolled push instructions.
      #
      # _NOTE_:: Interrupts must be disabled prior to calling this routine or the +disable_intr+
      #          option must be set to +true+.
      #
      # * +address+:: An addres of memory area to be cleared as a label or an integer or +hl+.
      #               The starting address of the whole screen area must be a multiple of 0x2000.
      # * +rows+:: A number of pixel rows to be cleared as an 8-bit register or a label or an integer.
      # * +cols+:: An even number of 8 pixel columns to be cleared as an integer or an immediate label.
      #
      # Options:
      # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
      #                    only if you have already disabled the interrupts.
      # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
      #                   if you need to perform more uninterrupted actions.
      # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
      #               +sp+ will point to the beginning of the last cleared row.
      #
      # _NOTE_:: Restoring +sp+ register uses self-modifying code.
      #
      # Modifies: +af+, +af'+, +bc+, +de+, +hl+, optionally: +sp+.
      def clear_screen_region_fast(address=hl, rows=c, cols=2, disable_intr:true, enable_intr:true, save_sp:true)
        raise ArgumentError, "address should be a label or an integer or HL register pair" unless address == hl or address?(address)
        raise ArgumentError, "rows should be a label or an integer or a register" unless [a,b,c,d,e,h,l].include?(rows) or
                                                                                         (address?(rows) && !pointer?(rows))
        cols = cols.to_i
        raise ArgumentError, "cols must be even" if cols.odd?
        raise ArgumentError, "cols must be less than or equal to 32" if cols > 32
        raise ArgumentError, "cols must be greater than or equal to 2" if cols < 2
        isolate do |_|
                        di if disable_intr
                        ld   [restore_sp + 1], sp if save_sp
                        ld   c, rows unless rows==c
                        ld   hl, address unless address == hl
                        ld   de, 0
                        ld   a, h       # calculate counter based on screen address modulo 8
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
                        ei if enable_intr
        end
      end
    end

    include Z80
  end
end

# DEPRECATED
ZXGfx = ZXLib::Gfx unless defined?(ZXGfx) # :nodoc:
