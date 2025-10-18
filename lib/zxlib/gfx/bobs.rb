module ZXLib
  module Gfx
    ##
    # ==Bitmap objects related routines.
    #
    # See also ZXLib::Gfx::Bobs::Macros.
    #
    #   ░░░░░░░░████░░░░░░████░░░░░░░░░░
    #   ░░░░░░████████░░██████████░░░░░░
    #   ░░░░████████████░░██████████░░░░
    #   ░░████░░████████░░██████░░████░░
    #   ░░████░░░░████████████░░░░████░░
    #   ████░░░░░░░░████████░░░░░░░░████
    #   ████░░░░░░░░██████████░░░░░░████
    #   ████░░░░████████░░██████████████
    #   ██████████████░░░░░░████████████
    #   ████████████████████████████████
    #   ██████░░░░██████████████░░░░████
    #   ░░████░░░░░░████░░░░░░░░░░░░██░░
    #   ░░██████░░░░░░░░░░░░░░░░██████░░
    #   ░░░░██████░░░░░░░░░░░░██████░░░░
    #   ░░░░░░████████████████████░░░░░░
    #   ░░░░░░░░░░████████████░░░░░░░░░░
    class Bobs
      module Constants
        COMBINE_FX_NOP  = 0x00
        COMBINE_FX_SET  = 0x7E
        COMBINE_FX_AND  = 0xA6
        COMBINE_FX_XOR  = 0xAE
        COMBINE_FX_OR   = 0xB6
      end
      ##
      # ==ZXLib::Gfx::Bobs macros.
      #
      # Bobs::Macros require:
      #
      #    macro_import MathInt
      #    macro_import Gfx
      module Macros
        include Bobs::Constants
        ##
        # Creates a routine that copies bitmap pixel lines to (or from) the ink/paper screen as a rectangle
        # of pixels.
        #
        # No pixel shifting is performed on a bitmap.
        #
        # * +bitmap+:: A direct or indirect address of a bitmap data containing the pixel lines as a label,
        #              an integer or a 16-bit register +hl+ or +de+ depending on the +copy_back+ option.
        # * +lines+:: A number of pixel lines to be copied as an 8-bit register or a label or a pointer.
        #             The number 0 is treated as 256 and most likely will lead to UNDEFINED BEHAVIOUR.
        # * +cols+:: A number of 8 pixel columns to be copied as an 8-bit register or a label or a pointer.
        #            If an accumulator (+a+) is given to +cols+ then the value is expected in +a'+.
        #            The number 0 is treated as 65536 and will lead to UNDEFINED BEHAVIOUR, clobbering
        #            the whole memory in the process.
        #
        # _NOTE_:: Unless +cols+ is one of: +ixh+, +ixl+, +iyh+ or +iyl+ the routine uses self modifying code.
        #
        # Options:
        # * +screen+:: A direct or indirect address of the screen memory where the lines of the bitmap will
        #    be copied to (or from), as a label, an integer or a 16-bit register +de+ or +hl+ depending on
        #    the +copy_back+ option. The starting address of the entire screen memory must be a multiple
        #    of 0x2000. If this option is +nil+, an appropriate 16-bit register pair is chosen based on the
        #    +copy_back+ option.
        # * +copy_back+:: A boolean indicating whether to copy pixel data from the screen (+true+) or to
        #    the screen (+false+).
        # * +scraddr+:: An optional entire screen memory address which must be a multiple of 0x2000 as
        #    an integer or an immediate label. If provided the routine breaks execution when the bottom
        #    of the screen has been reached. +CF+ = 0 (NC) is signalled if the routine terminates prematurely
        #    due to reaching the bottom of the screen. Otherwise +CF+ = 1 if the whole bitmap has been copied.
        # * +subroutine+:: A boolean indicating whether to create a subroutine.
        #
        # The 16-bit registers that can be used for a +bitmap+ argument and a +screen+ option:
        #
        #   copy_back    bitmap  screen
        #      false       hl      de
        #      true        de      hl
        #
        # Modifies: +af+, +af'+, +bc+, +bc'+, +de+, +hl+. Swaps registers unless out of screen.
        def bobs_copy_pixels(bitmap=hl, lines=a, cols=c, screen:nil, copy_back:false, scraddr:nil, subroutine:false, **opts)
          raise ArgumentError, "bobs_copy_pixels: invalid scraddr argument" unless scraddr.nil? or
                                                                      direct_address?(scraddr) or
                                          (Integer === scraddr and scraddr == (scraddr & 0xE000))
          unless copy_back
            target = opts.delete(:target) # legacy option
            screen = target unless target.nil?
          end
          raise ArgumentError, "bobs_copy_pixels: invalid option used" unless opts.empty?
          unless screen.nil? or register?(screen) or address?(screen)
            raise ArgumentError, "bobs_copy_pixels: screen should be an address, a 16-bit register pair or nil"
          end
          unless register?(bitmap) or address?(bitmap)
            raise ArgumentError, "bobs_copy_pixels: bitmap should be an address or a 16-bit register pair"
          end
          unless (register?(lines) and lines.bit8?) or address?(lines)
            raise ArgumentError, "bobs_copy_pixels: lines should be an integer, a label or a pointer or a register"
          end
          unless (register?(cols) and cols.bit8?) or address?(cols)
            raise ArgumentError, "bobs_copy_pixels: cols should be an integer, a label or a pointer or a register"
          end
          bitmap_tt = if copy_back then de else hl end
          if register?(bitmap) and bitmap != bitmap_tt
            raise ArgumentError, "bobs_copy_pixels: invalid bitmap register used"
          end
          scrhi, scrlo = if copy_back
            hl # source
          else
            de # target
          end.split
          if register?(screen) && screen != (scrhi|scrlo)
            raise ArgumentError, "bobs_copy_pixels: invalid screen register used"
          end
          screen = scrhi|scrlo if screen.nil?
          isolate do |eoc|
                            ld   a, lines unless lines == a
                            ex   af, af     # a': total lines
            unless [ixh,ixl,iyh,iyl].include?(cols)
                            ld   a, cols unless cols == a
                            ld   [cols_p], a
            end
                            ld   b, 0       # ldir counter msb
                            ld   scrhi|scrlo, screen unless screen == (scrhi|scrlo)
                            ld   bitmap_tt, bitmap unless bitmap == bitmap_tt
            if address?(screen) and !pointer?(screen)
                            ld   a, scrlo   # screen.lo
                            exx
                            ld   b, 8 - (screen>>8) % 8
            else
                            ld   a, scrhi   # calculate counter based on screen address modulo 8
                            anda 0b11111000 # (h & 0b11111000)
                            sub  scrhi      # (h & 0b11111000) - h % 8
                            add  8          # 8 - h % 8
                            exx
                            ld   b, a       # b: counter: 8 - h % 8
                            exx
                            ld   a, scrlo   # screen.lo
                            exx
            end
                            ex   af, af     # a: total lines, a': screen.lo
                            ld   c, a       # c: total lines
                            dec  a          # a: total lines - 1 (remaining lines)
                            sub  b          # a: remaining lines - counter
                            jr   NC, start  # first batch last?
                            ld   b, c       # b: counter = lines (last batch)

            start           ex   af, af     # a': remaining rows - 1; CF': 1 == last batch, a: screen.lo
            rloop           exx
            loop0           ld   scrlo, a   # restore screen.lo
            loop1           label
            if [ixh,ixl,iyh,iyl].include?(cols)
                            ld   c, cols
            else
              cols_a        ld   c, 0       # self-modified code
              cols_p        cols_a + 1
            end
                            ldir            # copy line
                            dec scrhi|scrlo # backup screen address, so it points to the last column (1/3rd screen overflow correction)
                            inc  scrhi      # next screen line
                            exx
                            djnz rloop
                            ex   af, af     # a: remaining lines, a': screen.lo
            if subroutine                   # CF=1 if this was the last batch
                            ret  C
            else
                            jr   C, eoc
            end
                            ld   b, 8       # b: counter = 8 lines (next row)
                            sub  b          # remaining lines - counter
                            jr   NC, loop8  # next batch last?
                            add  b          # restore remaining lines
                            ld   b, a
                            inc  b          # b: counter = remaining lines + 1 (last batch)
                            # next row address
            loop8           exx
                            ex   af, af     # a: screen.lo, a': remaining lines + CF
                            add  0x20       # a: screen.lo + 0x20
                            jr   C, loop0 unless scraddr
                            ld   scrlo, a   # screen.lo
                            ld   a, scrhi   # screen.hi
                            jr   C, check_ooscr if scraddr
                            sub  0x08
                            ld   scrhi, a   # adjusted screen.hi
                            ld   a, scrlo   # screen.lo
                            jp   loop1
            if scraddr
              check_ooscr   cp   (scraddr >> 8)|0x18
                            ld   a, scrlo   # screen.lo
                            jr   C, loop1
                            ret if subroutine
            end
          end
        end
        ##
        # Creates a routine that combines bitmap pixel lines as a rectangle of pixels with the ink/paper
        # screen data.
        #
        # No pixel shifting is performed on a bitmap.
        #
        # * +bitmap+:: A direct or indirect address of a bitmap data containing the pixel lines as a label,
        #              an integer or +hl+.
        # * +lines+:: A number of pixel lines to be copied as an 8-bit register or a label or a pointer.
        #             The number 0 is treated as 256 and most likely will lead to UNDEFINED BEHAVIOUR.
        # * +cols+:: A number of 8 pixel columns to be copied as an 8-bit register or a label or a pointer.
        #            If an accumulator (+a+) is given to +cols+ then the value is expected in +a'+.
        #            The number 0 is treated as 65536 and will lead to UNDEFINED BEHAVIOUR, clobbering
        #            the whole memory in the process.
        #
        # _NOTE_:: Unless +cols+ is one of: +ixh+, +ixl+, +iyh+ or +iyl+ the routine uses self modifying code.
        #
        # Options:
        # * +target+:: A direct or indirect address of the screen memory to be combined with the lines of the
        #    bitmap as a label, an integer or +de+. The starting address of the entire screen memory must be
        #    a multiple of 0x2000.
        # * +scraddr+:: An optional entire screen memory address which must be a multiple of 0x2000 as
        #    an integer or an immediate label. If provided, the routine breaks execution when the bottom
        #    of the screen has been reached. +CF+ = 0 (NC) is signalled if the routine terminates prematurely
        #    due to reaching the bottom of the screen. Otherwise +CF+ = 1 if the whole bitmap has been copied.
        # * +subroutine+:: A boolean indicating whether to create a subroutine.
        #
        # The +mode+ option can be changed at run time in the resulting routine, by storing a new operation
        # under the +fx_a+ sub-label. The following constants can be used for this purpose: +COMBINE_FX_NOP+,
        # +COMBINE_FX_SET+, +COMBINE_FX_AND+, +COMBINE_FX_XOR+, +COMBINE_FX_OR+.
        #
        #                   ld   a, Gfx::Bobs::COMBINE_FX_AND
        #                   ld   [combine_bitmap.fx_a], a
        #   # ...
        #   combine_bitmap  bobs_combine_pixels(mode: :xor, subroutine: true)
        #
        #
        # Using +:set+ or +COMBINE_FX_SET+ mode has been included for completeness in this routine, but is
        # sub-optimal. For copying bitmaps to the screen, using #bobs_copy_pixels is preferable.
        #
        # Modifies: +af+, +af'+, +bc+, +bc'+, +de+, +hl+. Swaps registers unless out of screen.
        def bobs_combine_pixels(bitmap=hl, lines=a, cols=c, target:de, mode: :or, scraddr:nil, subroutine:false)
          raise ArgumentError, "bobs_combine_pixels: invalid scraddr argument" unless scraddr.nil? or
                                                                      direct_address?(scraddr) or
                                          (Integer === scraddr and scraddr == (scraddr & 0xE000))
          unless address?(target) or target == de
            raise ArgumentError, "bobs_combine_pixels: target should be an address or de"
          end
          unless address?(bitmap) or bitmap == hl
            raise ArgumentError, "bobs_combine_pixels: bitmap should be an address or hl"
          end
          unless (register?(lines) and lines.bit8?) or address?(lines)
            raise ArgumentError, "bobs_combine_pixels: lines should be an integer, a label or a pointer or a register"
          end
          unless (register?(cols) and cols.bit8?) or address?(cols)
            raise ArgumentError, "bobs_combine_pixels: cols should be an integer, a label or a pointer or a register"
          end
          fx = case mode
            when :or, COMBINE_FX_OR   then ->(tgt) { ora tgt }
            when :xor, COMBINE_FX_XOR then ->(tgt) { xor tgt }
            when :and, COMBINE_FX_AND then ->(tgt) { anda tgt }
            when :nop, COMBINE_FX_NOP then ->(tgt) { nop }
            when :set, COMBINE_FX_SET then ->(tgt) { ld  a, tgt }
            else
              raise ArgumentError, "bobs_combine_pixels: mode should be one of: :or, :xor, :and, :none, :set"
          end
          isolate do |eoc|
                            ld   a, lines unless lines == a
                            ex   af, af     # a': lines
            unless [ixh,ixl,iyh,iyl].include?(cols)
                            ld   a, cols unless cols == a
                            ld   [cols_p], a
            end
                            ld   hl, bitmap unless bitmap == hl
                            ld   de, target unless target == de
                            ld   c, e       # lo
            if address?(target) and !pointer?(target)
                            exx
                            ld   b, 8 - (target>>8) % 8
            else
                            ld   a, d       # calculate counter based on screen address modulo 8
                            anda 0b11111000 # (h & 0b11111000)
                            sub  d          # (h & 0b11111000) - h % 8
                            add  8          # 8 - h % 8
                            exx
                            ld   b, a       # b: counter: 8 - h % 8
            end
                            ex   af, af     # a: lines
                            ld   c, a       # c: lines
                            dec  a          # a: lines - 1 (remaining lines)
                            sub  b          # a: lines - 1 - counter
                            jr   NC, start
                            ld   b, c       # b: counter = c: lines

            start           ex   af, af     # a': remaining lines - 1, CF': 1 == last batch
            rloop           exx
            loop0           ld   e, c       # lo
            if [ixh,ixl,iyh,iyl].include?(cols)
                            ld   b, cols
            else
              cols_a        ld   b, 0       # self-modified code
              cols_p        cols_a + 1
            end
            cloop           ld   a, [de]
            fx_a            fx.call [hl]    # or, xor, and
                            inc  hl
                            ld   [de], a
                            inc  e
                            djnz cloop
                            inc  d
                            exx
                            djnz rloop
                            ex   af, af     # a: remaining lines
            if subroutine
                            ret  C
            else
                            jr   C, eoc
            end
                            ld   b, 8
                            sub  b
                            jr   NC, loop8  # b: 8, CF: 0
                            add  b
                            ld   b, a
                            inc  b          # b: remaining lines, CF: 1

            loop8           exx
                            ex   af, af     # a': remaining lines, CF': 1 == last batch
                            ld   a, c       # lo
                            add  0x20       # a: lo + 0x20
                            ld   c, a       # c: lo
                            jr   C, loop0 unless scraddr
                            ld   a, d
                            jr   C, check_ooscr if scraddr
                            sub  0x08
                            ld   d, a       # d: adjusted
                            jp   loop0
            if scraddr
              check_ooscr   cp   (scraddr >> 8)|0x18
                            jr   C, loop0
                            ret if subroutine
            end
          end
        end
        ##
        # Creates a routine that copies rows of attribute data to (or from) the screen forming a rectangle
        # on the screen.
        #
        # * +attrs+:: A direct or indirect address of attribute rows as a label, an integer or a 16-bit
        #             register +hl+ or +de+ depending on the +copy_back+ option.
        # * +rows+:: A number of attribute rows to be copied as an 8-bit register or a label or a pointer.
        #            The number 0 is treated as 256 and most likely will lead to UNDEFINED BEHAVIOUR.
        # * +cols+:: A number of attribute columns to be copied as an 8-bit register or a label or a pointer.
        #            If an accumulator (+a+) is given to +cols+ then the value is expected in +a'+.
        #            The number 0 is treated as 65536 and will lead to UNDEFINED BEHAVIOUR, clobbering
        #            the whole memory in the process.
        #
        # _NOTE_:: Unless +cols+ is one of: +ixh+, +ixl+, +iyh+ or +iyl+ the routine uses self modifying code.
        #
        # Options:
        # * +screen+:: A direct or indirect address of the screen attributes memory where the rows of the
        #    attributes will be copied to (or from), as a label, an integer or a 16-bit register +de+ or
        #    +hl+, depending on the +copy_back+ option. The starting address of the entire screen
        #    memory must be a multiple of 0x2000. If this option is +nil+, an appropriate 16-bit register
        #    pair is chosen based on the +copy_back+ option.
        # * +copy_back+:: A boolean indicating whether to copy attributes from the screen (+true+) or to
        #    the screen (+false+).
        # * +scraddr+:: An optional entire screen memory address which must be a multiple of 0x2000 as
        #    an integer or an immediate label. If provided, the routine breaks execution when the bottom
        #    of the screen has been reached. +CF+ = 0 (NC) is signalled if the routine terminates prematurely
        #    due to reaching the bottom of the screen. Otherwise +CF+ = 1 if all the attributes has been copied.
        # * +subroutine+:: A boolean indicating whether to create a subroutine.
        #
        # Modifies: +af+, +af'+, +bc+, +bc'+, +de+, +hl+. Swaps registers unless out of screen.
        def bobs_copy_attrs(attrs=hl, rows=a, cols=c, screen:nil, copy_back: false, scraddr:nil, subroutine:false, **opts)
          raise ArgumentError, "bobs_copy_attrs: invalid scraddr argument" unless scraddr.nil? or
                                                                      direct_address?(scraddr) or
                                          (Integer === scraddr and scraddr == (scraddr & 0xE000))
          unless copy_back
            target = opts.delete(:target) # legacy option
            screen = target unless target.nil?
          end
          unless screen.nil? or register?(screen) or address?(screen)
            raise ArgumentError, "bobs_copy_attrs: screen should be an address, a 16-bit register pair or nil"
          end
          unless register?(attrs) or address?(attrs)
            raise ArgumentError, "bobs_copy_attrs: attrs should be an address or a 16-bit register pair"
          end
          unless (register?(rows) and rows.bit8?) or address?(rows)
            raise ArgumentError, "bobs_copy_attrs: rows should be an integer, a label or a pointer or a register"
          end
          unless (register?(cols) and cols.bit8?) or address?(cols)
            raise ArgumentError, "bobs_copy_attrs: cols should be an integer, a label or a pointer or a register"
          end
          raise ArgumentError, "bobs_copy_attrs: invalid option used" unless opts.empty?
          attrs_tt = if copy_back then de else hl end
          if register?(attrs) and attrs != attrs_tt
            raise ArgumentError, "bobs_copy_attrs: invalid attrs register used"
          end
          scrhi, scrlo = if copy_back
            hl # source
          else
            de # target
          end.split
          if register?(screen) && screen != (scrhi|scrlo)
            raise ArgumentError, "bobs_copy_attrs: invalid screen register used"
          end
          screen = scrhi|scrlo if screen.nil?
          isolate do |eoc|
                            ld   a, rows unless rows == a
                            ex   af, af
                            ld   a, cols unless cols == a
            unless [ixh,ixl,iyh,iyl].include?(cols)
                            ld   [cols_p], a
            end
                            exx
                            cpl
                            add  33
                            ld   c, a       # 32 - cols
                            ex   af, af     # a: rows
                            ld   b, a
                            exx
                            ld   attrs_tt, attrs unless attrs == attrs_tt
                            ld   scrhi|scrlo, screen unless screen == (scrhi|scrlo)

                            ld   b, 0
                            jr   start0

            rowloop         ld   a, c # 32 - cols
                            exx
                            adda_to scrhi, scrlo
            if scraddr
                            cp   (scraddr>>8)|0x1B
              if subroutine
                            ret  NC
              else
                            jr   NC, eoc
              end
            end
            start0          label
            if [ixh,ixl,iyh,iyl].include?(cols)
                            ld   c, cols
            else
              cols_a        ld   c, 0 # self-modified code
              cols_p        cols_a + 1
            end
                            ldir
                            exx
                            djnz rowloop
                            ret if subroutine
          end
        end
        ##
        # Creates a routine that copies bitmap pixel lines to (or from) the ink/paper screen as a rectangle
        # of pixels using unrolled POP (or PUSH) instructions.
        #
        # No pixel shifting is performed on a texture.
        #
        # _NOTE_:: Interrupts must be disabled prior to calling this routine or the +disable_intr+
        #          option must be set to +true+.
        #
        # Copying back bitmap data from screen memory to bitmap memory is performed backwards - from the last
        # byte of memory to the first one.
        #
        # Note that when copying back bitmap data from the screen while the bitmap byte size is odd
        # (when both +cols+ and +lines+ are odd), and because a single +push+ instruction modifies 2 bytes
        # the first (odd) bitmap byte will remain in the +d+ register and has to be transfered to the
        # beginning of the bitmap outside of this routine, unless +copy_back+ option is +:padded+.
        # In this instance the last push will be performed modifying an additional pad byte, preceding in
        # memory the very first address of destination bitmap data. In this instance the bitmap data must
        # be padded by a single byte in front of it.
        #
        # * +bitmap+:: A direct or indirect address of a bitmap data containing the pixel lines as a label,
        #    an integer or a 16-bit register pair: +ix+, +iy+ or +sp+.
        # * +lines+:: A number of pixel lines to be copied as an 8-bit register, a label, pointer or an integer.
        #             The number 0 is treated as 256 and most likely will lead to UNDEFINED BEHAVIOUR.
        # * +cols+:: A constant number of 8 pixel columns to be copied as an integer.
        #
        # Options:
        # * +screen+:: A direct or indirect address of the screen memory where the lines of the bitmap will
        #    be copied to (or from), as a label, an integer or +hl+. The starting address of the entire screen
        #    memory must be a multiple of 0x2000.
        # * +copy_back+:: A boolean indicating whether to copy pixel data from the screen (+true+) or to the
        #    screen (+false+). If +copy_back+ is +true+, the +bitmap+ and +screen+ must point to the __last__
        #    byte of both the bitmap data and the screen area (bottom - rightmost corner).
        # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
        #    only if interrupts are disabled prior to entering this routine.
        # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
        #    if more uninterrupted actions need to performed after this routine completes execution.
        # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
        #    +sp+ will point behind the end of the last source line (or before the first if +copy_back+ is +true+).
        # * +scraddr+:: An optional entire screen memory address which must be a multiple of 0x2000 as
        #    an integer or an immediate label. If provided the routine breaks execution when the bottom
        #    of the screen has been reached. +CF+ = 0 (NC) is signalled if the routine terminates prematurely
        #    due to reaching the bottom of the screen. Otherwise +CF+ = 1 if the whole bitmap has been copied.
        # * +subroutine+:: A boolean indicating whether to create a subroutine.
        #
        # If +bitmap+ is a direct address, it can be later changed  at run time by storing a new bitmap address
        # at the +bitmap_p+ sub-label.
        #
        # _NOTE_:: Restoring +sp+ register uses self-modifying code.
        #
        # Modifies: +af+, +af'+, +bc+, +de+, +hl+, optionally: +sp+.
        def bobs_copy_pixels_fast(bitmap, lines=c, cols=32, screen:hl, copy_back:false, disable_intr:true, enable_intr:true, save_sp:true, scraddr:nil, subroutine:false, **opts)
          raise ArgumentError, "bobs_copy_pixels_fast: invalid scraddr argument" unless scraddr.nil? or direct_address?(scraddr) or
                                                            (Integer === scraddr and scraddr == (scraddr & 0xE000))
          unless [false, true, :padded].include?(copy_back)
            raise ArgumentError, "bobs_copy_pixels_fast: copy_back should be a boolean or :padded"
          end
          unless copy_back
            target = opts.delete(:target) # legacy option
            screen = target unless target.nil?
          end
          raise ArgumentError, "bobs_copy_pixels_fast: screen should be an address or a pointer or hl" unless screen == hl or
                                                                                                        address?(screen)
          unless [sp,ix,iy].include?(bitmap) or address?(bitmap)
            raise ArgumentError, "bobs_copy_pixels_fast: bitmap should be an address or a pointer or ix/iy/sp"
          end
          unless (register?(lines) and lines.bit8?) or address?(lines)
            raise ArgumentError, "bobs_copy_pixels_fast: lines should be an integer or a label or a pointer or a register" 
          end
          cols = cols.to_i
          raise ArgumentError, "bobs_copy_pixels_fast: cols must be less than or equal to 32" if cols > 32
          raise ArgumentError, "bobs_copy_pixels_fast: cols must be greater than or equal to 1" if cols < 1
          raise ArgumentError, "bobs_copy_pixels_fast: disable_intr should be a boolean" unless [true, false].include?(disable_intr)
          raise ArgumentError, "bobs_copy_pixels_fast: enable_intr should be a boolean" unless [true, false].include?(enable_intr)
          raise ArgumentError, "bobs_copy_pixels_fast: save_sp should be a boolean" unless [true, false].include?(save_sp)
          raise ArgumentError, "bobs_copy_pixels_fast: save_sp makes no sense if bitmap = sp" if bitmap == sp and save_sp
          raise ArgumentError, "bobs_copy_pixels_fast: subroutine requires save_sp" if subroutine and !save_sp
          raise ArgumentError, "bobs_copy_pixels_fast: interrupts should be disabled if bitmap = sp" if bitmap == sp and disable_intr
          fits_single_row = if Integer === screen && Integer === lines
            if copy_back
              lines <= (1 + (screen>>8) % 8)
            else
              lines <= (8 - (screen>>8) % 8)
            end
          else
            false
          end
          next_row = ->(loop0, quit=nil) do
            isolate do |eoc|
              quit = eoc if quit.nil?
                            ex   af, af     # a': screen.lo; a: remaining lines - 1; CF: 1 == last batch
                            jr   C, quit
                            ld   b, c       # 8
                            sub  b
                            jr   NC, not_last
                            add  b
                            ld   b, a
                            inc  b

              not_last      ex   af, af     # a': remaining lines - 1; CF': 1 == last batch, a: screen.lo
                if copy_back
                            sub  0x20       # a: screen.lo - 0x20
                else
                            add  0x20       # a: screen.lo + 0x20
                end
                            ld   l, a       # l: screen.lo
                            jr   C, loop0 unless scraddr
                            ld   a, h
                            jr   C, check_oos if scraddr
                if copy_back
                            add  c          # +8
                else
                            sub  c          # -8
                end
                            ld   h, a       # h: adjusted
                            ld   a, l
                            jp   loop0 if scraddr || quit == eoc
              if scraddr
                scrhiaddr = (scraddr >> 8)
                scrhiaddr = scrhiaddr|0x18 unless copy_back
                check_oos   cp   scrhiaddr
                            ld   a, l
                if copy_back
                            jr   NC, loop0
                            ccf  # flip C to signal oos
                else
                            jr   C, loop0
                end
                            jr   quit unless quit == eoc
              end
            end
          end

          isolate do
                            ld   c, lines if register?(lines) && lines != c          
            if address?(screen) and !pointer?(screen)
                            ld   hl, screen
              if fits_single_row
                            ld   b, lines
              elsif copy_back
                            ld   b, 1 + (screen>>8) % 8 # first row count: [1, 8]
              else
                            ld   b, 8 - (screen>>8) % 8 # first row count: [1, 8]
              end
            else
                            ld   hl, screen unless screen == hl
                            ld   a, h       # calculate counter based on screen address modulo 8
              if copy_back
                            anda 0b00000111 # (h & 0b00000111) h % 8
                            inc  a          # (1 + h % 8)
              else
                            anda 0b11111000 # (h & 0b11111000)
                            sub  h          # (h & 0b11111000) - h % 8
                            add  8          # (8 - h % 8)
              end
                            ld   b, a       # b: counter: (8 - h % 8) or (1 + h % 8)
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
                            ex   af, af     # a': remaining lines - 1; CF': 1 == last batch
                            ld   c, 8       # constant = 8
            end
                            ld   a, l       # a: screen.lo
                            ld   [restore_sp_p], sp if save_sp
                            di if disable_intr
            if direct_address?(bitmap)
              bitmap_a      ld   sp, bitmap
              bitmap_p      as   bitmap_a + 1
            else
                            ld   sp, bitmap unless bitmap == sp
            end
            if cols.even?
              rowloop       ld   l, a
              loop0         label
              ((cols-2)/2).times do
                if copy_back
                            ld   d, [hl]
                            dec  l
                            ld   e, [hl]
                            dec  l
                            push de
                else
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                            inc  l
                end # copy_back
              end # times
              if copy_back
                            ld   d, [hl]
                            dec  l
                            ld   e, [hl]
                            dec  h
                            push  de
              else
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                            inc  h
              end # copy_back
                            djnz rowloop
                            next_row.call loop0 unless fits_single_row
            else # cols.odd?
              evenline      ld   l, a
              evenloop      label
              ((cols-1)/2).times do
                if copy_back
                            ld   d, [hl]
                            dec  l
                            ld   e, [hl]
                            dec  l
                            push de
                else
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                            inc  l
                end # copy_back
              end # times
              if copy_back
                            ld   d, [hl]
                            dec  h
              else
                            pop  de
                            ld   [hl], e
                            inc  h
              end # copy_back
                            djnz oddline
              if copy_back == :padded
                            next_row.call oddloop unless fits_single_row
                            push de # last push with clobber
                            jp   quit
              elsif fits_single_row
                            jp   quit
              else
                            next_row.call oddloop, quit
                            # might fall thru here instead of jp oddloop
              end # copy_back
              oddline       ld   l, a
              oddloop       label
              if copy_back
                            ld   e, [hl]
                            push de
              else
                            ld   [hl], d
              end # copy_back
              ((cols-1)/2).times do
                if copy_back
                            dec  l
                            ld   d, [hl]
                            dec  l
                            ld   e, [hl]
                            push de
                else
                            pop  de
                            inc  l
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                end # copy_back
              end # times
              if copy_back
                            dec  h
              else
                            inc  h
              end # copy_back
                            djnz evenline
                            next_row.call evenloop
              quit          label
            end # cols.even?|odd?
            if save_sp
              restore_sp    ld  sp, 0
              restore_sp_p  as  restore_sp + 1
            end
                            ei if enable_intr
                            ret if subroutine
          end
        end
        ##
        # Creates a routine that copies rows of attribute data to (or from) the screen forming a rectangle
        # on the screen using unrolled POP (or PUSH) instructions.
        #
        # _NOTE_:: Interrupts must be disabled prior to calling this routine or the +disable_intr+
        #          option must be set to +true+.
        #
        # * +attrs+:: An address of attributes to be copied from as a label, pointer, an integer or one of the
        #             registers: +ix+, +iy+ or +sp+.
        # * +rows+:: A number of attribute rows to be copied as an 8-bit register or a label, pointer or an integer.
        #            The number 0 is treated as 256 and most likely will lead to UNDEFINED BEHAVIOUR.
        # * +cols+:: A constant number of attribute columns to be copied as an integer.
        #
        # Options:
        # * +screen+:: A direct or indirect address of the screen attributes memory where the attribute rows
        #    will be copied to (or from), as a label, an integer or a 16-bit register +hl+.
        #    The starting address of the entire screen memory must be a multiple of 0x2000.
        # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
        #    only if interrupts are disabled prior to entering this routine.
        # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
        #    if more uninterrupted actions need to performed after this routine completes execution.
        # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
        #               +sp+ will point behind the end of the last source row.
        # * +check_oos+:: If +true+ reduces number of rows if the bottom of the screen would have been exceeded.
        # * +subroutine+:: A boolean indicating whether to create a subroutine.
        #
        # If +attrs+ is a direct address, it can be later changed  at run time by storing a new bitmap address
        # at the +attrs_p+ sub-label.
        #
        # _NOTE_:: Restoring +sp+ register uses self-modifying code.
        #
        # Modifies: +af+, +bc+, +de+, +hl+, optionally: +sp+.
        def bobs_copy_attrs_fast(attrs, rows=a, cols=32, screen:hl, copy_back:false, disable_intr:true, enable_intr:true, save_sp:true, check_oos:false, subroutine:false, **opts)
          unless [false, true, :padded].include?(copy_back)
            raise ArgumentError, "bobs_copy_attrs_fast: copy_back should be a boolean or :padded"
          end
          unless copy_back
            target = opts.delete(:target) # legacy option
            screen = target unless target.nil?
          end
          raise ArgumentError, "bobs_copy_attrs_fast: screen should be an address or a pointer or hl" unless screen == hl or
                                                                                                        address?(screen)
          unless [sp,ix,iy].include?(attrs) or address?(attrs)
            raise ArgumentError, "bobs_copy_attrs_fast: attrs should be an address or a pointer or ix/iy/sp"
          end
          unless (register?(rows) and rows.bit8?) or address?(rows)
            raise ArgumentError, "bobs_copy_attrs_fast: rows should be an integer or a label or a pointer or a register" 
          end
          cols = cols.to_i
          raise ArgumentError, "bobs_copy_attrs_fast: cols must be less than or equal to 32" if cols > 32
          raise ArgumentError, "bobs_copy_attrs_fast: cols must be greater than or equal to 1" if cols < 1
          raise ArgumentError, "bobs_copy_attrs_fast: disable_intr should be a boolean" unless [true, false].include?(disable_intr)
          raise ArgumentError, "bobs_copy_attrs_fast: enable_intr should be a boolean" unless [true, false].include?(enable_intr)
          raise ArgumentError, "bobs_copy_attrs_fast: save_sp should be a boolean" unless [true, false].include?(save_sp)
          raise ArgumentError, "bobs_copy_attrs_fast: save_sp makes no sense if bitmap = sp" if bitmap == sp and save_sp
          raise ArgumentError, "bobs_copy_attrs_fast: check_oos should be a boolean" unless [true, false].include?(check_oos)
          raise ArgumentError, "bobs_copy_attrs_fast: subroutine requires save_sp" if subroutine and !save_sp
          raise ArgumentError, "bobs_copy_attrs_fast: interrupts should be disabled if bitmap = sp" if bitmap == sp and disable_intr
          isolate do |eoc|
            if check_oos
              if address?(rows) && pointer?(rows)
                            ld   a, rows
                            ld   c, a
              else
                            ld   c, rows unless rows == c
              end
                            ld   a, l         # llllllll
                            xor  h            # xxxxxxxx
                            anda 0b11100000   # xxx00000
                            xor  h            # lllhhhhh
                            3.times { rlca }  # hhhhhlll
              if copy_back
                            sub  0xC0
                            cp   c
                            jr   NC, skip_rows
              else
                            add  0x28
                            add  c
                            jr   NC, skip_rows
                            cpl
                            adc  c
              end # copy_back
                            ld   c, a
              skip_rows     ld   a, c
            else
                            ld   a, rows unless rows == a
            end # check_oos
            next_attr_line = if cols == 32
              if copy_back
                    proc {  dec  hl }
              else
                    proc {  inc  hl }
              end # copy_back
            else # cols != 32
                    proc {  add  hl, bc }
            end # cols == 32
            if copy_back
                            ld   bc, -(33 - cols)
            else
                            ld   bc, 33 - cols
            end unless cols == 32
                            ld   hl, screen unless screen == hl
                            ld   [restore_sp_p], sp if save_sp
                            di if disable_intr
            if direct_address?(attrs)
              attrs_a       ld   sp, attrs
              attrs_p       as   attrs_a + 1
            else
                            ld   sp, attrs unless attrs == sp
            end
                            jp   start0
            if cols.even?
              rowloop       label
                            next_attr_line.call
              start0        label
              ((cols-2)/2).times do
                if copy_back
                            ld   d, [hl]
                            dec  l
                            ld   e, [hl]
                            dec  l
                            push de
                else
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                            inc  l
                end # copy_back
              end
                if copy_back
                            ld   d, [hl]
                            dec  l
                            ld   e, [hl]
                            push de
                else
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                end # copy_back
                            dec  a
                            jr   NZ, rowloop
            else # cols.odd?
              evenloop      label
                            next_attr_line.call
              start0        label
              ((cols-1)/2).times do
                if copy_back
                            ld   d, [hl]
                            dec  l
                            ld   e, [hl]
                            dec  l
                            push de
                else
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                            inc  l
                end # copy_back
              end # times
                if copy_back
                            ld   d, [hl]
                else
                            pop  de
                            ld   [hl], e
                end # copy_back
                            dec  a
                if copy_back == :padded
                            jr   NZ, oddloop
                            push de # last push with clobber
                            jp   quit
                else
                            jr   Z, quit
                end # copy_back
              oddloop       label
                            next_attr_line.call
              if copy_back
                            ld   e, [hl]
                            push de
              else
                            ld   [hl], d
              end # copy_back
              ((cols-1)/2).times do
                if copy_back
                            dec  l
                            ld   d, [hl]
                            dec  l
                            ld   e, [hl]
                            push de
                else
                            pop  de
                            inc  l
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                end # copy_back
              end # times
                            dec  a
                            jr   NZ, evenloop
            end # cols.even?
            quit            label
            if save_sp
              restore_sp    ld  sp, 0
              restore_sp_p  as  restore_sp + 1
            end
                            ei if enable_intr
                            ret if subroutine
          end
        end
        ##
        # Creates a routine that renders 7 bitmap textures by shifting 7 times each source bitmap lines
        # by 1 bit to the right.
        #
        # Rendered bitmaps are extended to the right by 1 column (an octet).
        #
        # * +bitmap+:: An address of a source bitmap or +hl+.
        # * +lines+:: A number of lines or an 8-bit register holding the number of lines.
        #             The number 0 is treated as 256 and most likely will lead to UNDEFINED BEHAVIOUR.
        # * +cols+:: A number of 8-bit columns (octets) per line or an 8-bit register holding the number.
        #
        # Options:
        # * +target+:: A destination address where bitmaps should be rendered or +de+.
        # 
        # Modifies: +af+, +af'+, +bc+, +de+, +hl+ and some stack.
        def bobs_rshift_bitmap_pixels_7times(bitmap=hl, lines=c, cols=a, target:de)
          isolate do
            if address?(lines) && pointer?(lines)
                            ex   af, af if cols == a
                            ld   a, lines
                            ld   c, a
                            ex   af, af if cols == a
            else
                            ld   c, lines unless lines == c
            end
                            ld   a, cols unless cols == a
                            ld   hl, bitmap unless bitmap == hl
                            ld   de, target unless target == de
                            ld   b, 7
                            scf
            bitloop         push bc
                            push de
                            bobs_rshift_bitmap_pixels_once
                            adc  a, 0
                            pop  hl
                            pop  bc
                            djnz bitloop
          end
        end
        ##
        # Creates a routine that renders a bitmap texture by shifting each source bitmap lines by 1
        # bit to the right.
        #
        # * Carry Flag:: If set (CF=1) indicates that the target bitmap should be extended by
        #                one additional column holding the bits right-shifted out of the last
        #                source bitmap column. Otherwise (if CF=0) the carried out bits are lost.
        #                The Carry Flag value is preserved when the routine exits.
        #
        # * +bitmap+:: An address of a source bitmap or +hl+.
        # * +lines+:: A number of lines or an 8-bit register holding the number of lines.
        #             The number 0 is treated as 256 and most likely will lead to UNDEFINED BEHAVIOUR.
        # * +cols+:: A number of 8-bit columns (octets) per line or an 8-bit register holding the number.
        #
        # Options:
        # * +target+:: A destination address where bitmap should be rendered or +de+.
        # 
        # Modifies: +af+, +af'+, +bc+, +de+, +hl+.
        def bobs_rshift_bitmap_pixels_once(bitmap=hl, lines=c, cols=a, target:de)
          isolate do
            if address?(lines) && pointer?(lines)
                            ex   af, af if cols == a
                            ld   a, lines
                            ld   c, a
                            ex   af, af if cols == a
            else
                            ld   c, lines unless lines == c
            end
                            ld   a, cols unless cols == a
                            ld   hl, bitmap unless bitmap == hl
                            ld   de, target unless target == de
            line_start      ld   b, a
                            ex   af, af        # a': cols, CF': 1 extend cols (right margin)
                            ora  a             # CF: 0
            line_loop       ld   a, [hl]
                            inc  hl
                            rra
                            ld   [de], a
                            inc  de
                            djnz line_loop
                            ex   af, af
                            jr   NC, skip_last
                            ex   af, af
                            ld   a, 0
                            rra
                            ld   [de], a
                            inc  de
                            ex   af, af
            skip_last       dec  c
                            jr   NZ, line_start
          end
        end
        ##
        # Creates a jump table for the Macros#bobs_draw_pixels_fast routine.
        #
        # Provide a label (or a symbol) referencing a routine created by either Macros#bobs_draw_pixels_fast
        # or Macros#bobs_draw_pixels_fast_routines.
        #
        # Returns a label addressing the created jump table.
        #
        # Provide an address of a jump table as a +jump_table:+ option to +bobs_draw_pixels_fast+.
        #
        # _NOTE_:: The jump table must be aligned in memory so it does not cross 256-byte address pages.
        #          In other words, the high 8 bits of addresses of each jump table byte must be the same.
        #          Otherwise an error will be raised when trying to instanciate code.
        #
        # The jump table occupies 24 bytes of code.
        #
        # The content is lazy evaluated so the order of creating a table and a routine doesn't matter.
        def bobs_draw_pixels_fast_jump_table(draw_pixels_fast_label)
          unless draw_pixels_fast_label.nil?
            throw ArgumentError, "draw_pixels_fast_label must be a label" unless direct_label?(draw_pixels_fast_label)
            draw_pixels_fast_label = draw_pixels_fast_label.to_label(self)
          end
          ns do
            size = 8*3-1 # correction, jump_table may reside on the far end of the page
            select((label + size) & 0xFF){|x| x >= size }.else do
              raise ArgumentError, "jump_table data must fit on a single 256-byte page of memory"
            end
            (0..7).each do |index|
              line_rshiftN = if draw_pixels_fast_label.nil?
                define_label :"line_rshift#{index}"
              else
                draw_pixels_fast_label.send :"line_rshift#{index}"
              end
              define_label :"jump#{index}",  dw( line_rshiftN.loop0 )
              define_label :"mask#{index}",  db( 0xff >> index )
            end
          end
        end
        ##
        # Creates specialized routines for Macros#bobs_draw_pixels_fast that can be used externally
        # via jump table.
        #
        # _NOTE_:: +skip_cols+ option should match +skip_cols+ given to Macros#bobs_draw_pixels_fast
        #          if an 8-bit register (other than +e'+) or an indirect (a pointer) address is specified.
        #
        # * +next_row+:: An address of +next_row+ routine. Usually: +draw_pixels_fast_label.next_row+.
        # * +cols+:: A constant number of 8 pixel columns to be drawn as an integer from 1 to 32.
        #
        # Options:
        # * +mode+:: Drawing mode, one of: +:copy+, +:set+, +:or+, +:xor+, +:and+, +:nand+.
        #            See Macros#bobs_draw_pixels_fast.
        # * +skip_cols+:: If set, the number of bitmap 8-bit columns will be skipped after each line is drawn.
        #                 Set as an address, an integer or an 8-bit register from alternative set,
        #                 e.g. +e+ specifies +e'+. Also see the note above.
        # * +lclip+:: Skips drawing the leftmost screen column for non zero +bshift+ values if enabled.
        # * +rclip+:: Skips drawing the rightmost screen column for non zero +bshift+ values if enabled.
        # * +no0shift+:: If this option is not +nil+ skips creating the drawing routine for +bshift+ equal
        #   to 0. Provide a label to +draw_pixels_fast_label.quit+ or to an existing implementation, e.g.:
        #   +draw_pixels_fast_routines.line_rshift0.loop0+. Calling such a routine with +bshift+ register
        #   holding zero is safe but nothing will be drawn and CF flag will be reset upon return if +quit+
        #   was provided.
        # * +merge+:: Whether the <tt>line_rshift{0-7}</tt> labels should be merged with the current context.
        # * +jump_eoc+:: Whether the <code>jp next_row</code> should be appended at the end of the generated code.
        def bobs_draw_pixels_fast_routines(next_row, cols,
                                           mode: :set,
                                           skip_cols: nil,
                                           lclip: false,
                                           rclip: false,
                                           no0shift: nil,
                                           merge: false,
                                           jump_eoc: true)
          throw ArgumentError, "next_row must be a label" unless direct_label?(next_row)
          cols = cols.to_i
          raise ArgumentError, "cols must be less than or equal to 32" if cols > 32
          raise ArgumentError, "cols must be greater than or equal to 1" if cols < 1
          raise ArgumentError, "lclip should be a boolean" unless [true, false].include?(lclip)
          raise ArgumentError, "rclip should be a boolean" unless [true, false].include?(rclip)
          raise ArgumentError, "merge should be a boolean" unless [true, false].include?(merge)
          raise ArgumentError, "jump_eoc should be a boolean" unless [true, false].include?(jump_eoc)
          raise ArgumentError, "no0shift should be a nil or a label (quit)" unless no0shift.nil? or
                                                                              direct_label?(no0shift)
          next_row = next_row.to_label(self)
          t = b
          m = c
          fx = case mode
            when :or   then ->(tgt) { ora tgt }
            when :xor  then ->(tgt) { xor tgt }
            when :and  then ->(tgt) { anda tgt }
            when :nand then ->(tgt) { cpl; anda tgt }
            when :set, :copy then nil
            else
              raise ArgumentError, "mode should be one of: :or, :xor, :and, :set or :copy"
          end
          if !pointer?(skip_cols) && label_immediate?(skip_cols)
            ncols = skip_cols.to_i
          else
            ncols = skip_cols
          end
          raise ArgumentError, "skip_cols must not be negative" if Integer === ncols and ncols < 0
          skip_cols = case ncols # modifies: hl, sp (e: skip)
            when 0 then nil
            when 1..5
              n = if cols.odd? then ncols - 1 else ncols end
              lambda do
                (n/2).times { pop hl }
                            inc  sp if n.odd?
              end
            else
              unless address?(ncols) or (register?(ncols) and ncols.bit8? and ncols != a)
                raise ArgumentError, "skip_cols should be one of: nil, an integer, an address or an 8-bit alt register except an accumulator"
              end
              lambda do
                if address?(ncols) and !pointer?(ncols)
                  if Integer === ncols && cols.odd?
                            ld   hl, ncols - 1
                  else
                            ld   hl, ncols
                  end
                else
                            ld   l, e
                            ld   h, 0
                end
                            add  hl, sp
                            ld   sp, hl
              end
          end if ncols
          rotate_bits = ->(rshift) do
            if (1..4).include?(rshift)
                        rshift.times { rrca }
            elsif (5..7).include?(rshift)
                        (8-rshift).times { rlca }
            end
          end
          isolate merge:merge do
            # hl: screen, c': screen lo, b': counter, d': const 8, a': lines left - 1, CF': last row
            # e': ncols if skip_cols
            ns :line_rshift0 do
              if no0shift
                  loop0     as   no0shift    # created jump table will forward to this address
              else
                fx0 = if fx
                  ->(r) do
                            ld   a, r
                            fx.call [hl]
                            ld   [hl], a
                  end
                else
                  ->(r) {   ld [hl], r   }
                end

                rowloop     ld   a, c        # a: screen.lo
                            exx              # regs
                            ld   l, a        # l: screen.lo (restore)
                loop0       label
                (0...cols).step(2) do |col|
                  last_column = (col >= cols - 2)
                            pop  de
                            fx0.call e
                  if last_column && cols.odd?
                            dec  sp unless skip_cols && ncols.is_a?(Integer) # offset last column
                  else
                            inc  l
                            fx0.call d
                  end
                            inc  l unless last_column
                end
                            inc  h
                            exx
                            skip_cols.call if skip_cols
                            djnz rowloop
                            jp   next_row
              end
            end
            # hl: screen, m: mask, c': screen lo, b': counter, d': const 8, a': lines left - 1, CF': last row
            # e': ncols if skip_cols
            [1,7,2,6,3,5,4].each do |rshift|
              ns :"line_rshift#{rshift}" do
                rowloop     ld   a, c        # a: screen.lo (from c')
                            exx              # regs
                            ld   l, a        # l: screen.lo (restore)
                loop0       label
                (0...cols).step(2) do |col|
                  last_column = (col >= cols - 2)
                            pop  de          # de: FEDCBA98 76543210
                            ld   a, e        # a: 76543210
                            rotate_bits.call rshift
                  unless last_column and cols.odd? and rclip
                            ld   e, a        # e: 32107654 (e.g. rshift=4)
                  end

                  unless col.zero? and lclip
                    if !col.zero?            # t: BA98FEDC
                            xor  t           # a: xxxxxxxx (32107654 ^ BA98FEDC) t: BA98FEDC
                            anda m           # a: ____xxxx (____7654 ^ ____FEDC) m: mask 00001111
                            xor  t           # a: BA987654 (____7654 ^ ____FEDC ^ BA98FEDC) t: BA98FEDC
                    elsif mode == :set
                            ld   t, [hl]     # t: ssssssss
                            xor  t           # a: xxxxxxxx (32107654 ^ ssssssss) t: ssssssss
                            anda m           # a: ____xxxx (____7654 ^ ____ssss) m: mask 00001111
                            xor  t           # a: ssss7654 (____7654 ^ ____ssss ^ ssssssss) t: ssssssss
                    else
                            anda m           # a: ____7654 m: mask 00001111
                    end
                            fx.call [hl] unless fx.nil?
                            ld   [hl], a     # [hl]: ____7654(fx)[hl] (col=0)
                                             #    or ssss7654         (col=0 and :set)
                                             #    or BA987654(fx)[hl] (col>0)
                  end

                  cut_left  label
                  if last_column and cols.odd?
                            dec  sp unless skip_cols && ncols.is_a?(Integer) # offset last column
                    unless rclip
                      unless col.zero? and lclip
                            inc  l           # l: screen.lo (next column)
                      end
                      if mode == :set
                        unless col.zero? and lclip
                            ld   a, e        # a: 32107654
                        end
                            xor  [hl]        # a: xxxxxxxx (32107654 ^ ssssssss)
                            anda m           # a: ____xxxx (____7654 ^ ____ssss) m: 00001111
                      else
                        unless col.zero? and mode == :copy and !lclip
                          unless col.zero? and lclip
                            ld   a, e        # a: 32107654
                          end
                            anda m           # a: ____7654 m: 00001111
                        end
                      end
                                             # a: ____7654 (or ____7654 ^ ____ssss)
                            xor  e           # a: 3210____ (or 3210ssss) e: 32107654
                            fx.call [hl] unless fx.nil?
                            ld   [hl], a     # [hl]: 3210____(fx)[hl]
                    end
                  else
                            ld   a, d        # a: FEDCBA98
                            rotate_bits.call rshift
                            ld   d, a        # d: BA98FEDC (e.g. rshift=4)

                            xor  e           # a: xxxxxxxx (BA98FEDC ^ 32107654) e: 32107654
                            anda m           # a: ____xxxx (____FEDC ^ ____7654) m: mask 00001111
                            xor  e           # a: 3210FEDC (____FEDC ^ ____7654 ^ 32107654) e: 32107654
                    unless col.zero? and lclip
                            inc  l           # l: screen.lo (next column)
                    end
                            fx.call [hl] unless fx.nil?
                            ld   [hl], a     # [hl]: 3210FEDC(fx)[hl]

                    unless last_column and rclip
                            inc  l           # l: screen.lo (next column)

                      if last_column
                            ld   a, d        # a: BA98FEDC (e.g. rshift=4)
                        if mode == :set
                            xor  [hl]        # a: xxxxxxxx (BA98FEDC ^ ssssssss)
                        end
                            anda m           # a: ____xxxx (____FEDC [^ ____ssss]) m: mask 00001111
                            xor  d           # a: BA98ssss (____FEDC [^ ____ssss] ^ BA98FEDC) d: BA98FEDC
                            fx.call [hl] unless fx.nil?
                            ld   [hl], a     # [hl]: BA98____(fx)[hl]
                      else
                            ld   t, d        # t: BA98FEDC
                      end
                    end
                  end
                end
                            inc  h           # h: screen.hi (next line)
                            exx              # regs'
                            skip_cols.call if skip_cols
                            djnz rowloop     # b': line counter
                            jp   next_row if jump_eoc || rshift != 4
              end
            end
          end
        end
        ##
        # Creates a routine that draws bitmap pixel lines to the ink/paper screen as a rectangle object using
        # unrolled POP instructions.
        #
        # _NOTE_:: Interrupts must be disabled prior to calling this routine or the +disable_intr+
        #          option must be set to +true+.
        #
        # * +bitmap+:: A direct or indirect address of a bitmap data containing the pixel lines as a label,
        #              an integer or a 16-bit register pair: +hl'+, +ix+, +iy+ or +sp+.
        #              If +hl+ is specified the actual value will be read from alternative register +hl'+.
        # * +lines+:: A number of pixel lines to be drawn as an 8-bit register or a label, a pointer or an integer.
        #             The number 0 is treated as 256 and most likely will lead to UNDEFINED BEHAVIOUR.
        # * +cols+:: A constant number of 8 pixel columns to be drawn as an integer from 1 to 32.
        #
        # Options:
        # * +target+:: An address of a screen memory area to be copied to as a label, pointer, an integer or +hl+.
        #              The starting address of the whole screen area must be a multiple of 0x2000.
        # * +bshift+:: One of 8-bit registers: +b+, +c+, +d+, +e+.
        #              The value of the register specifies how many pixels to the right the bitmap must be shifted
        #              before drawing the bitmap on the screen. The actual value must be in the range from 0 up to 7.
        #              Values outside of the allowed range will resolve in UNDEFINED BEHAVIOUR.
        # * +mode+:: Drawing mode, one of: +:copy+, +:set+, +:or+, +:xor+, +:and+, +:nand+.
        # * +skip_cols+:: If set, the number of bitmap 8-bit columns will be skipped after each line is drawn.
        #                 Set as an address, an integer or an 8-bit register from alternative set,
        #                 e.g. +e+ specifies +e'+.
        # * +lclip+:: Skips drawing the leftmost screen column for non zero +bshift+ values if enabled.
        # * +rclip+:: Skips drawing the rightmost screen column for non zero +bshift+ values if enabled.
        # * +no0shift+:: Skips creating the drawing routine for +bshift+ zero if enabled.
        #                Calling such a routine with +bshift+ register holding zero is safe but nothing
        #                will be drawn and CF flag will be reset upon return.
        # * +tx+:: A temporary jump address register: +ix+ or +iy+.
        # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
        #    only if interrupts are disabled prior to entering this routine.
        # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
        #    if more uninterrupted actions need to performed after this routine completes execution.
        # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
        #               +sp+ will point behind the end of the last source line.
        # * +scraddr+:: An optional entire screen memory address which must be a multiple of 0x2000 as
        #    an integer or an immediate label. If provided the routine breaks execution when the bottom
        #    of the screen has been reached. +CF+ = 0 (NC) is signalled if the routine terminates prematurely
        #    due to reaching the bottom of the screen. Otherwise +CF+ = 1 if the whole bitmap has been drawn.
        # * +jump_table+:: A label, a pointer address or one of +de+/+bc+/+ix+/+iy+ register pairs referencing
        #    an external jump table created with Macros#bobs_draw_pixels_fast_jump_table. If not provided an
        #    internal jump table will be created instead. In this instance a +jump_table+ can be later changed
        #    at run time by storing a new jump table address at the +jump_table_p+ sub-label.
        # * +subroutine+:: A boolean indicating whether to create a subroutine.
        #
        # If +bitmap+ is a direct address, it can be later changed  at run time by storing a new bitmap address
        # at the +bitmap_p+ sub-label.
        #
        # _NOTE_:: Restoring +sp+ register uses self-modifying code.
        #
        # Drawing modes:
        #    
        #    Bitmap                 Screen (cols = 2, bshift = 4)
        #    76543210 76543210      76543210 76543210 76543210
        #                           ░██░███░ ░███░█░█ ░░██░░██  (original screen data)
        #    █░██░██░ ░███░░░█ ->>  ░░░░█░██ ░██░░███ ░░░█░░░░  :copy
        #    █░██░██░ ░███░░░█ ->>  ░██░█░██ ░██░░███ ░░░█░░██  :set
        #    █░██░██░ ░███░░░█ ->>  ░██░████ ░███░███ ░░██░░██  :or
        #    █░██░██░ ░███░░░█ ->>  ░██░░█░█ ░░░█░░█░ ░░█░░░██  :xor
        #    █░██░██░ ░███░░░█ ->>  ░░░░█░█░ ░██░░█░░ ░░░█░░░░  :and
        #    █░██░██░ ░███░░░█ ->>  ░██░░█░░ ░░░█░░░░ ░░█░░░██  :nand
        #
        # Modifies: +af+, +af'+, +bc+, +bc'+, +de+, +d'+, +hl+, (+hl'+, +e'+ if skip_cols is given).
        # Swaps registers unless out of screen.
        def bobs_draw_pixels_fast(bitmap, lines=a, cols=2,
                                  target:hl,
                                  bshift:b,
                                  mode: :set,
                                  skip_cols: nil, # e
                                  lclip: false,
                                  rclip: false,
                                  no0shift: false,
                                  tx:ix,
                                  disable_intr:true,
                                  enable_intr:true,
                                  save_sp:true,
                                  scraddr:nil,
                                  jump_table:nil,
                                  subroutine:false)
          raise ArgumentError, "bobs_draw_pixels_fast: invalid scraddr argument" unless scraddr.nil? or
                          direct_address?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000))
          raise ArgumentError, "bobs_draw_pixels_fast: target should be an address or a pointer or hl" unless target == hl or address?(target)
          unless [sp,ix,iy,hl].include?(bitmap) or address?(bitmap)
            raise ArgumentError, "bobs_draw_pixels_fast: bitmap should be an address or a pointer or ix/iy/sp/hl'"
          end
          raise ArgumentError, "bobs_draw_pixels_fast: bitmap should not be the same as jump_table" if bitmap == jump_table
          unless (register?(lines) and lines.bit8?) or address?(lines)
            raise ArgumentError, "bobs_draw_pixels_fast: lines should be a label or a pointer or an integer or a register" 
          end
          cols = cols.to_i
          raise ArgumentError, "bobs_draw_pixels_fast: cols must be less than or equal to 32" if cols > 32
          raise ArgumentError, "bobs_draw_pixels_fast: cols must be greater than or equal to 1" if cols < 1
          raise ArgumentError, "bobs_draw_pixels_fast: disable_intr should be a boolean" unless [true, false].include?(disable_intr)
          raise ArgumentError, "bobs_draw_pixels_fast: enable_intr should be a boolean" unless [true, false].include?(enable_intr)
          raise ArgumentError, "bobs_draw_pixels_fast: save_sp should be a boolean" unless [true, false].include?(save_sp)
          raise ArgumentError, "bobs_draw_pixels_fast: save_sp makes no sense if bitmap = sp" if bitmap == sp and save_sp
          raise ArgumentError, "bobs_draw_pixels_fast: subroutine requires save_sp" if subroutine and !save_sp
          raise ArgumentError, "bobs_draw_pixels_fast: interrupts should be disabled if bitmap = sp" if bitmap == sp and disable_intr
          raise ArgumentError, "bobs_draw_pixels_fast: bshift should be one of: b, c, d or e" unless [b,c,d,e].include?(bshift)
          raise ArgumentError, "bobs_draw_pixels_fast: tx should be one of: ix or iy" unless [ix, iy].include?(tx)
          raise ArgumentError, "bobs_draw_pixels_fast: lclip should be a boolean" unless [true, false].include?(lclip)
          raise ArgumentError, "bobs_draw_pixels_fast: rclip should be a boolean" unless [true, false].include?(rclip)
          raise ArgumentError, "bobs_draw_pixels_fast: no0shift should be a boolean" unless [true, false].include?(no0shift)
          txh, txl = tx.split
          t = b
          m = c
          fx = case mode
            when :or  then ->(tgt) { ora tgt }
            when :xor then ->(tgt) { xor tgt }
            when :and then ->(tgt) { anda tgt }
            when :nand then ->(tgt) { cpl; anda tgt }
            when :set, :copy then nil
            else
              raise ArgumentError, "bobs_draw_pixels_fast: mode should be one of: :or, :xor, :and, :set or :copy"
          end
          ncols = skip_cols
          skip_cols = case ncols # modifies: hl, sp (e: skip)
            when 0 then false
            when Integer then true
            else
              unless address?(ncols) or (register?(ncols) and ncols.bit8? and ncols != a)
                raise ArgumentError, "bobs_draw_pixels_fast: skip_cols should be one of: nil, an integer, an address or an 8-bit alt register except an accumulator"
              end
              true
          end if ncols
          local_jump_table = if jump_table.nil?
            true
          elsif [de, bc, ix, iy].include?(jump_table)
            jh, jl = jump_table.split
            if [jh, jl].include?(lines) or [jh, jl].include?(bshift)
              raise ArgumentError, "bobs_draw_pixels_fast: lines, bshift and jump_table should use different registers"
            end
            false
          else
            raise ArgumentError, "bobs_draw_pixels_fast: jump_table is not a label" unless label?(jump_table)
            jump_table = unwrap_pointer(jump_table).to_label(self) # in case a symbol was given
            false
          end

          isolate do
            jump_table = define_label(:jump_table) if local_jump_table # normalize in isolated ns
                            ld   a, lines unless lines == a
                            ex   af, af     # a': lines

                            ld   hl, target unless target == hl
            # calculate 1st row counter and remaining lines
                            ld   a, h       # calculate counter based on screen address modulo 8
                            anda 0b11111000 # (h & 0b11111000)
                            sub  h          # = - h % 8  [-7, 0]
                            exx             # regs'

            if skip_cols and register?(ncols)
                            ld   e, ncols unless ncols == e
            end
                            ld   d, 8       # d: 8 (const)
                            add  d          # = 8 - h % 8  [1, 8]
                            ld   b, a       # b: counter = 8 - h % 8  [1, 8]

                            ex   af, af     # a: lines, a': ...

                            ld   c, a       # c: lines
                            dec  a          # a: lines - 1
                            sub  b          # a: lines - 1 - counter
                            jr   NC, more_rows
                            ld   b, c       # b: counter = c: lines
              more_rows     ex   af, af     # a': lines left - 1, CF': last row?

            if skip_cols and address?(ncols) and pointer?(ncols)
                            ld   a, ncols
                            ld   e, a
            end
            # calculate routine address in jump_table from bshift
                            exx             # regs
                            ld   a, bshift  # rshift
                            add  a, a       # a: rshift * 2
                            add  bshift     # a: rshift * 3
            if register?(jump_table)
                            ld16 de, jump_table unless jump_table == de
            else
            jump_table_a    ld   de, jump_table
            jump_table_p    jump_table_a + 1 unless jump_table.pointer?
            end
                            add  e
                            ld   e, a       # de: ->routine
            # get routine address (tx) and bshift mask (m)
            if [sp, tx].include?(bitmap)    # bitmap already in sp or from tx, can't use it for jump_table
                            ld   [restore_sp_p], sp if save_sp
                            di if disable_intr
                            ld   sp, bitmap unless bitmap == sp

                            ld   a, [de]    # a: routine.loop0.lo
                            inc  e
                            ld   txl, a
                            ld   a, [de]    # a: routine.loop0.hi
                            inc  e
                            ld   txh, a     # tx: routine.loop0
                            ld   a, [de]    # a: mask
                            ld   m, a       # m: mask

                            ld   a, l       # a: screen.lo
                            exx             # regs'
            else
                            ex   de, hl     # hl: jump[], de: screen
                            ld   [restore_sp_p], sp if save_sp
                            di if disable_intr
                            ld   sp, hl     # sp: jump[]
                            ex   de, hl     # hl: screen
                            pop  tx         # tx: routine.loop0
                            pop  t|m        # m: mask

                            ld   a, l       # a: screen.lo
                            exx             # regs'
              if direct_address?(bitmap)
                bitmap_a    ld   sp, bitmap # bitmap: address
                bitmap_p    as   bitmap_a + 1
              else
                            ld   sp, bitmap # bitmap: ix/iy/hl' or indirect address (tx != bitmap)
              end
            end
                            ld   c, a       # c': screen.lo
                            exx             # regs
            # hl: screen, m: mask, c': screen lo, b': counter
            # d': const 8, a': lines left - 1, CF': last row
                            jp   (tx)       # pc: tx: routine.loop0

            if local_jump_table
              jump_table    bobs_draw_pixels_fast_jump_table nil
            end
            ###########################
            ##    bshift routines    ##
            ###########################
            no0shift = if no0shift
              quit
            else
              nil
            end
            bobs_draw_pixels_fast_routines(next_row, cols,
                                           mode:mode,
                                           skip_cols:ncols,
                                           lclip:lclip,
                                           rclip:rclip,
                                           no0shift: no0shift,
                                           merge: true,
                                           jump_eoc: false)
            ################################################
            ##                  next row                  ##
            ################################################
            ns :next_row do
            # check remaining lines and set up line counter for the next row
                            ex   af, af     # a: lines left - 1, CF: last row?
                            jr   C, quit
                            ld   b, d       # b: 8 (counter)
                            sub  b          # a: lines left - 1 - 8
                            jr   NC, not_last
                            add  b          # a: lines left - 1, CF: 1
                            ld   b, a       # b: lines left - 1
                            inc  b          # b: lines left (counter)
              not_last      ex   af, af     # a': lines left - 1, CF': last row?
            # calculate next row screen address
                            ld   a, c       # a: lo
                            add  0x20       # a: lo + 0x20
                            ld   c, a       # c': next row (screen.lo)
                            exx             # regs
                            ld   l, a       # l: next row (screen.lo)
                            jr   C, loop0fw unless scraddr
                            ld   a, h       # a: sreen.hi
                            jr   C, check_oos if scraddr
                            sub  8          # a: sreen.hi - 8
                            ld   h, a       # h: adjusted sreen.hi
              loop0fw       jp   (tx)
              if scraddr
                check_oos   cp   (scraddr >> 8)|0x18
                check_oos_p check_oos + 1
                            jr   NC, quit
                            jp   (tx)
              end
            end
            unless subroutine && !enable_intr && !save_sp
              quit          label
            end
            if save_sp
              restore_sp    ld  sp, 0
              restore_sp_p  as  restore_sp + 1
            end
                            ei if enable_intr
                            ret if subroutine
          end
        end
      end

      include Z80
    end
  end
end
