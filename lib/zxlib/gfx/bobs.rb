module ZXLib
  module Gfx
    ##
    # ==Bitmap objects related routines.
    #
    # See also ZXLib::Gfx::Bobs::Macros.
    class Bobs
      ##
      # ==ZXLib::Gfx::Bobs macros.
      #
      # Bobs::Macros require:
      #
      #    macro_import MathInt
      #    macro_import Gfx
      module Macros
        ##
        # Creates a routine that copies bitmap pixels to the ink/paper screen as a rectangle object.
        #
        # * +bitmap+:: An address of a bitmap to be copied from as a label, pointer, an integer or +hl+.
        # * +lines+:: A number of pixel lines to be copied as an 8-bit register or a label or a pointer.
        # * +cols+:: A number of 8 pixel columns to be copied as an 8-bit register or a label or a pointer.
        #
        # _NOTE_:: Unless +cols+ is one of: +ixh+, +ixl+, +iyh+ or +iyl+ the routine uses self modifying code.
        #
        # Options:
        # * +target+:: An address of a screen memory area to be copied to as a label, pointer, an integer or +de+.
        #              The starting address of the whole screen area must be a multiple of 0x2000.
        # * +scraddr+:: An optional screen memory address which must be a multiple of 0x2000 as an integer or a label.
        #               If provided the routine breaks execution when the bottom of the screen has been reached.
        #               +CF+ = 0 (NC) if the routine terminates prematurely due to reaching bottom of the screen.
        #               Otherwise +CF+ = 1 if the whole bitmap has been copied.
        # * +subroutine+:: Whether to create a subroutine.
        #
        # Modifies: +af+, +af'+, +bc+, +bc'+, +de+, +hl+. Swaps registers unless out of screen.
        def bobs_copy_pixels(bitmap=hl, lines=a, cols=c, target:de, scraddr:nil, subroutine:false)
          isolate do |eoc|
                            ld   a, lines unless lines == a
                            ex   af, af     # a': lines
            unless [ixh,ixl,iyh,iyl].include?(cols)
                            ld   a, cols unless cols == a
                            ld   [cols_p], a
            end
                            ld   b, 0
                            ld   hl, bitmap unless bitmap == hl
                            ld   de, target unless target == de
            if address?(target) and !pointer?(target)
                            ld   a, e
                            exx
                            ld   b, 8 - (target>>8) % 8
            else
                            ld   a, d       # calculate counter based on screen address modulo 8
                            anda 0b11111000 # (h & 0b11111000)
                            sub  d          # (h & 0b11111000) - h % 8
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
            loop1           label
            if [ixh,ixl,iyh,iyl].include?(cols)
                            ld   c, cols
            else
              cols_a        ld   c, 0       # self-code modified
              cols_p        cols_a + 1
            end
                            ldir
                            dec  de
                            inc  d
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
                            jr   C, loop0 unless scraddr
                            ld   e, a       # e: lo
                            ld   a, d
                            jr   C, check_ooscr if scraddr
                            sub  0x08
                            ld   d, a       # d: adjusted
                            ld   a, e
                            jp   loop1
            if scraddr
              check_ooscr   cp   (scraddr >> 8)|0x18
                            ld   a, e
                            jr   C, loop1
                            ret if subroutine
            end
          end
        end
        ##
        # Creates a routine that copies bitmap attributes to the screen as a rectangle object.
        #
        # * +attrs+:: An address of attributes to be copied from as a label, pointer, an integer or +hl+.
        # * +rows+:: A number of attribute rows to be copied as an 8-bit register or a label or a pointer.
        # * +cols+:: A number of attribute columns to be copied as an 8-bit register or a label or a pointer.
        #
        # _NOTE_:: Unless +cols+ is one of: +ixh+, +ixl+, +iyh+ or +iyl+ the routine uses self modifying code.
        #
        # Options:
        # * +target+:: An address of screen attributes memory area to be copied to as a label, pointer, an integer or +de+.
        #              The starting address of the whole screen area must be a multiple of 0x2000.
        # * +scraddr+:: An optional screen memory address which must be a multiple of 0x2000 as an integer or a label.
        #               If provided the routine breaks execution when the bottom of the screen has been reached.
        #               +CF+ = 0 (NC) if the routine terminates prematurely due to reaching bottom of the screen.
        #               Otherwise +CF+ = 1 if the whole bitmap has been copied.
        # * +subroutine+:: Whether to create a subroutine.
        #
        # Modifies: +af+, +af'+, +bc+, +bc'+, +de+, +hl+. Swaps registers unless out of screen.
        def bobs_copy_attrs(attrs=hl, rows=a, cols=c, target:de, scraddr:nil, subroutine:false)
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
                            ld   hl, attrs unless attrs == hl
                            ld   de, target unless target == de

                            ld   b, 0
                            jr   start0

            rowloop         ld   a, c # 32 - cols
                            exx
                            adda_to d, e
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
              cols_a        ld   c, 0 # self-code modified
              cols_p        cols_a + 1
            end
                            ldir
                            exx
                            djnz rowloop
                            ret if subroutine
          end
        end
        ##
        # Creates a routine that copies bitmap pixels to the ink/paper screen as a rectangle object using unrolled
        # POP instructions.
        #
        # _NOTE_:: Interrupts must be disabled prior to calling this routine or the +disable_intr+
        #          option must be set to +true+.
        #
        # * +bitmap+:: An address of a bitmap to be copied from as a label, a pointer, an integer or one of the registers:
        #              +ix+, +iy+ or +sp+.
        # * +lines+:: A number of pixel lines to be copied as an 8-bit register or a label, pointer or an integer.
        # * +cols+:: A constant number of 8 pixel columns to be copied as an integer.
        #
        # Options:
        # * +target+:: An address of a screen memory area to be copied to as a label, pointer, an integer or +hl+.
        #              The starting address of the whole screen area must be a multiple of 0x2000.
        # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
        #                    only if you have already disabled the interrupts.
        # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
        #                   if you need to perform more uninterrupted actions.
        # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
        #               +sp+ will point after the end of the last line.
        # * +scraddr+:: An optional screen memory address which must be a multiple of 0x2000 as an integer or a label.
        #               If provided the routine breaks execution when the bottom of the screen has been reached.
        #               +CF+ = 0 (NC) if the routine terminates prematurely due to reaching bottom of the screen.
        #               Otherwise +CF+ = 1 if the whole bitmap has been copied.
        # * +subroutine+:: Whether to create a subroutine.
        #
        # _NOTE_:: Restoring +sp+ register uses self-modifying code.
        #
        # Modifies: +af+, +af'+, +bc+, +de+, +hl+, optionally: +sp+.
        def bobs_copy_pixels_fast(bitmap, lines=c, cols=32, target:hl, disable_intr:true, enable_intr:true, save_sp:true, scraddr:nil, subroutine:false)
          raise ArgumentError, "invalid scraddr argument" unless scraddr.nil? or direct_address?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000))
          raise ArgumentError, "target should be an address or a pointer or hl" unless target == hl or address?(target)
          raise ArgumentError, "bitmap should be an address or a pointer or ix/iy/sp" unless [sp,ix,iy].include?(bitmap) or address?(bitmap)
          raise ArgumentError, "lines should be an integer or a label or a pointer or a register" unless (register?(lines) and lines.bit8?) or
                                                                                                         address?(lines)
          cols = cols.to_i
          raise ArgumentError, "cols must be less than or equal to 32" if cols > 32
          raise ArgumentError, "cols must be greater than or equal to 1" if cols < 1
          raise ArgumentError, "save_sp makes no sense if bitmap = SP" if bitmap == sp and save_sp
          fits_single_row = Integer === target && Integer === lines && lines <= (8 - (target>>8) % 8)
          next_row = ->(loop0, quit=nil) do
            isolate do |eoc|
              quit = eoc if quit.nil?
                            ex   af, af     # a': lo; a: remaining rows - 1; CF: 1 == last batch
              if subroutine && !enable_intr && !save_sp
                            ret  C
              else
                            jr   C, quit
              end
                            ld   b, c       # 8
                            sub  b
                            jr   NC, not_last
                            add  b
                            ld   b, a
                            inc  b

              not_last      ex   af, af     # a': remaining rows - 1; CF': 1 == last batch, a: lo
                            add  0x20       # a: lo + 0x20
                            ld   l, a       # l: lo
                            jr   C, loop0 unless scraddr
                            ld   a, h
                            jr   C, check_oos if scraddr
                            sub  c          # 8
                            ld   h, a       # h: adjusted
                            ld   a, l
                            jp   loop0 if scraddr || quit == eoc
              if scraddr
                check_oos   cp   (scraddr >> 8)|0x18
                            ld   a, l
                            jr   C, loop0
                if subroutine && !enable_intr && !save_sp
                            ret
                else
                            jr   quit unless quit == eoc
                end
              end
            end
          end

          isolate do
                            ld   c, lines if register?(lines) && lines != c          
            if address?(target) and !pointer?(target)
                            ld   hl, target
              if fits_single_row
                            ld   b, lines
              else
                            ld   b, 8 - (target>>8) % 8
              end
            else
                            ld   hl, target unless target == hl
                            ld   a, h       # calculate counter based on screen address modulo 8
                            anda 0b11111000 # (h & 0b11111000)
                            sub  h          # (h & 0b11111000) - h % 8
                            add  8          # (8 - h % 8)
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
                            ex   af, af
                            ld   c, 8
            end
                            ld   a, l
                            di if disable_intr
                            ld   [restore_sp_p], sp if save_sp
                            ld   sp, bitmap unless bitmap == sp
            if cols.even?
              rowloop       ld   l, a
              loop0         label
              ((cols-2)/2).times do
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                            inc  l
              end
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                            inc  h
                            djnz rowloop
                            next_row.call loop0 unless fits_single_row
            else
              evenline      ld   l, a
              evenloop      label
              ((cols-1)/2).times do
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                            inc  l
              end
                            pop  de
                            ld   [hl], e
                            inc  h
                            djnz oddline
                            next_row.call oddloop, quit unless fits_single_row

              oddline       ld   l, a
              oddloop       ld   [hl], d
              ((cols-1)/2).times do
                            pop  de
                            inc  l
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
              end
                            inc  h
                            djnz evenline
                            next_row.call evenloop unless fits_single_row
              quit          label
            end
            if save_sp
              restore_sp    ld  sp, 0
              restore_sp_p  restore_sp + 1
            end
                            ei if enable_intr
                            ret if subroutine && (enable_intr || save_sp)
          end
        end
        ##
        # Creates a routine that copies bitmap attributes to the screen as a rectangle object using unrolled
        # POP instructions.
        #
        # _NOTE_:: Interrupts must be disabled prior to calling this routine or the +disable_intr+
        #          option must be set to +true+.
        #
        # * +attrs+:: An address of attributes to be copied from as a label, pointer, an integer or one of the registers:
        #             +ix+, +iy+ or +sp+.
        # * +rows+:: A number of attribute rows to be copied as an 8-bit register or a label, pointer or an integer.
        # * +cols+:: A constant number of attribute columns to be copied as an integer.
        #
        # Options:
        # * +target+:: An address of screen attributes memory area to be copied to as a label, pointer, an integer or +hl+.
        #              The starting address of the whole screen area must be a multiple of 0x2000.
        # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
        #                    only if you have already disabled the interrupts.
        # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
        #                   if you need to perform more uninterrupted actions.
        # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
        #               +sp+ will point after the end of the last line.
        # * +check_oos+:: If +true+ reduces number of rows if the bottom of the screen would have been exceeded.
        # * +subroutine+:: Whether to create a subroutine.
        #
        # _NOTE_:: Restoring +sp+ register uses self-modifying code.
        #
        # Modifies: +af+, +bc+, +de+, +hl+, optionally: +sp+.
        def bobs_copy_attrs_fast(attrs, rows=a, cols=32, target:hl, disable_intr:true, enable_intr:true, save_sp:true, check_oos:false, subroutine:false)
          isolate do |eoc|
            if check_oos
              if pointer?(rows)
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
                            add  0x28
                            add  c
                            jr   NC, skip_rows
                            cpl
                            adc  c
                            ld   c, a
              skip_rows     ld   a, c
            else
                            ld   a, rows unless rows == a
            end
                            ld   bc, 33 - cols unless cols == 32
                            ld   hl, target unless target == hl
                            di if disable_intr
                            ld   [restore_sp_p], sp if save_sp
                            ld   sp, attrs unless attrs == sp
                            jr   start0
            if cols.even?
              rowloop       label
              if cols == 32
                            inc  hl
              else
                            add  hl, bc
              end
              start0        label
              ((cols-2)/2).times do
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                            inc  l
              end
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                            dec  a
                            jr   NZ, rowloop
            else
              evenloop      label
              if cols == 32
                            inc  hl
              else
                            add  hl, bc
              end
              start0        label
              ((cols-1)/2).times do
                            pop  de
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
                            inc  l
              end
                            pop  de
                            ld   [hl], e
                            dec  a
              if subroutine && !enable_intr && !save_sp
                            ret  Z
              else
                            jr   Z, quit
              end
              oddloop       label
              if cols == 32
                            inc  hl
              else
                            add  hl, bc
              end
                            ld   [hl], d
              ((cols-1)/2).times do
                            pop  de
                            inc  l
                            ld   [hl], e
                            inc  l
                            ld   [hl], d
              end
                            dec  a
                            jr   NZ, evenloop
            end
            quit            label
            if save_sp
              restore_sp    ld  sp, 0
              restore_sp_p  restore_sp + 1
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
            size = 8*3
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
        # * +no0shift+:: Skips creating the drawing routine for +bshift+ zero if not nil.
        #                Provide +:ret+ if the +bobs_draw_pixels_fast+ was created as a subroutine with
        #                both +enable_intr+ and +save_sp+ options specified as false.
        #                Otherwise provide a label to +draw_pixels_fast_label.quit+ or to an existing
        #                implementation: e.g. +draw_pixels_fast_routines.line_rshift0.loop0+.
        #                Calling such a routine with +bshift+ register holding zero is safe but nothing
        #                will be drawn and CF flag will be reset upon return if +:ret+ or +quit+ was provided.
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
          raise ArgumentError, "no0shift should be a nil or :ret or a label (quit)" unless no0shift.nil? ||
                                                                no0shift == :ret || direct_label?(no0shift)
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
                case no0shift
                when :ret
                  loop0     ret
                else
                  loop0     as   no0shift    # created jump table will forward to this address
                end
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
        # Creates a routine that draws bitmap pixels to the ink/paper screen as a rectangle object using unrolled
        # POP instructions, line by line.
        #
        # _NOTE_:: Interrupts must be disabled prior to calling this routine or the +disable_intr+
        #          option must be set to +true+.
        #
        # * +bitmap+:: An address of a bitmap to be drawn as a label, a pointer, an integer or one of the 16-bit
        #              registers: +hl'+, +ix+, +iy+ or +sp+.
        #              If +hl+ is specified the actual value will be read from alternative register +hl'+.
        # * +lines+:: A number of pixel lines to be drawn as an 8-bit register or a label, a pointer or an integer.
        #             The number must not be 0, otherwise the routine will resolve in UNDEFINED BEHAVIOUR.
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
        # * +tx+:: A temporary address register: +ix+ or +iy+.
        # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
        #                    only if you have already disabled the interrupts.
        # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
        #                   if you need to perform more uninterrupted actions.
        # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
        #               +sp+ will point after the end of the last line.
        # * +scraddr+:: An optional screen memory address which must be a multiple of 0x2000 as an integer or a label.
        #               If provided the routine breaks execution when the bottom of the screen has been reached.
        #               +CF+ = 0 (NC) if the routine terminates prematurely due to reaching bottom of the screen.
        #               Otherwise +CF+ = 1 if the whole bitmap has been drawn.
        # * +jump_table+:: A label, a pointer address or one of +de+/+bc+/+ix+/+iy+ register pairs referencing
        #                  an external jump table created with Macros#bobs_draw_pixels_fast_jump_table. If not
        #                  provided an internal jump table will be created instead. In this instance a +jump_table+
        #                  can be later changed at run time by storing a new jump table address at +jump_table_p+
        #                  sublabel.
        # * +subroutine+:: Whether to create a subroutine.
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
          raise ArgumentError, "invalid scraddr argument" unless scraddr.nil? or direct_address?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000))
          raise ArgumentError, "target should be an address or a pointer or hl" unless target == hl or address?(target)
          raise ArgumentError, "bitmap should be an address or a pointer or ix/iy/sp/hl'" unless [sp,ix,iy,hl].include?(bitmap) or address?(bitmap)
          raise ArgumentError, "bitmap should not be the same as jump_table" if bitmap == jump_table
          raise ArgumentError, "lines should be a label or a pointer or an integer or a register" unless (register?(lines) and lines.bit8?) or
                                                                                                         address?(lines)
          cols = cols.to_i
          raise ArgumentError, "cols must be less than or equal to 32" if cols > 32
          # raise ArgumentError, "cols must be even" unless cols.even?
          raise ArgumentError, "cols must be greater than or equal to 1" if cols < 1
          raise ArgumentError, "disable_intr should be a boolean" unless [true, false].include?(disable_intr)
          raise ArgumentError, "enable_intr should be a boolean" unless [true, false].include?(enable_intr)
          raise ArgumentError, "save_sp should be a boolean" unless [true, false].include?(save_sp)
          raise ArgumentError, "save_sp makes no sense if bitmap = sp" if bitmap == sp and save_sp
          raise ArgumentError, "interrupts should be disabled if bitmap = sp" if bitmap == sp and disable_intr
          raise ArgumentError, "bshift should be one of: b, c, d or e" unless [b,c,d,e].include?(bshift)
          raise ArgumentError, "tx should be one of: ix or iy" unless [ix, iy].include?(tx)
          raise ArgumentError, "lclip should be a boolean" unless [true, false].include?(lclip)
          raise ArgumentError, "rclip should be a boolean" unless [true, false].include?(rclip)
          raise ArgumentError, "no0shift should be a boolean" unless [true, false].include?(no0shift)
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
              raise ArgumentError, "mode should be one of: :or, :xor, :and, :set or :copy"
          end
          ncols = skip_cols
          skip_cols = case ncols # modifies: hl, sp (e: skip)
            when 0 then false
            when Integer then true
            else
              unless address?(ncols) or (register?(ncols) and ncols.bit8? and ncols != a)
                raise ArgumentError, "skip_cols should be one of: nil, an integer, an address or an 8-bit alt register except an accumulator"
              end
              true
          end if ncols
          local_jump_table = if jump_table.nil?
            true
          elsif [de, bc, ix, iy].include?(jump_table)
            jh, jl = jump_table.split
            raise ArgumentError, "lines, bshift and jump_table should use different registers" if [jh, jl].include?(lines) or
                                                                                                  [jh, jl].include?(bshift)
            false
          else
            raise ArgumentError, "jump_table must be a label" unless label?(jump_table)
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
                            di if disable_intr
                            ld   [restore_sp_p], sp if save_sp
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
                            di if disable_intr
                            ld   [restore_sp_p], sp if save_sp
                            ld   sp, hl     # sp: jump[]
                            ex   de, hl     # hl: screen
                            pop  tx         # tx: routine.loop0
                            pop  t|m        # m: mask

                            ld   a, l       # a: screen.lo
                            exx             # regs'
                            ld   sp, bitmap # bitmap: ix/iy/hl' or address (tx != bitmap)
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
              if subroutine && !enable_intr && !save_sp
                :ret
              else
                quit
              end
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
              if subroutine && !enable_intr && !save_sp
                            ret  C
              else
                            jr   C, quit
              end
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
                if subroutine && !enable_intr && !save_sp
                            ret  NC
                else
                            jr   NC, quit
                end
                            jp   (tx)
              end
            end
            unless subroutine && !enable_intr && !save_sp
              quit          label
            end
            if save_sp
              restore_sp    ld  sp, 0
              restore_sp_p  restore_sp + 1
            end
                            ei if enable_intr
                            ret if subroutine && (enable_intr || save_sp)
          end
        end
      end

      include Z80
    end
  end
end
