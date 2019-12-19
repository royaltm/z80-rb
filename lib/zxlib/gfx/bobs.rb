module ZXLib
  module Gfx
    ##
    #  Bitmap objects related routines.
    #
    #  See also ZXLib::Gfx::Bobs::Macros.
    class Bobs
      ##
      #  ==ZXLib::Gfx::Bobs macros.
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
        # * +target+:: An address of a screen memory area to be copied to as a label, pointer, an integer or +de+.
        #              The starting address of the whole screen area must be a multiple of 0x2000.
        #
        # _NOTE_:: Unless +cols+ is one of: +ixh+, +ixl+, +iyh+ or +iyl+ the routine uses self modifying code.
        #
        # Options:
        # * +scraddr+:: An optional screen memory address which must be a multiple of 0x2000 as an integer or a label.
        #               If provided the routine breaks execution when the bottom of the screen has been reached.
        #               +CF+ = 0 (NC) if the routine terminates prematurely due to reaching bottom of the screen.
        #               Otherwise +CF+ = 1 if the whole bitmap has been copied.
        # * +subroutine+:: Whether to create a subroutine.
        #
        # Modifies: +af+, +af'+, +bc+, +bc'+, +de+, +hl+. Swaps registers unless out of screen.
        def bobs_copy_pixels(bitmap=hl, lines=a, cols=c, target=de, scraddr:nil, subroutine:false)
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
        # * +target+:: An address of screen attributes memory area to be copied to as a label, pointer, an integer or +de+.
        #              The starting address of the whole screen area must be a multiple of 0x2000.
        #
        # _NOTE_:: Unless +cols+ is one of: +ixh+, +ixl+, +iyh+ or +iyl+ the routine uses self modifying code.
        #
        # Options:
        # * +scraddr+:: An optional screen memory address which must be a multiple of 0x2000 as an integer or a label.
        #               If provided the routine breaks execution when the bottom of the screen has been reached.
        #               +CF+ = 0 (NC) if the routine terminates prematurely due to reaching bottom of the screen.
        #               Otherwise +CF+ = 1 if the whole bitmap has been copied.
        # * +subroutine+:: Whether to create a subroutine.
        #
        # Modifies: +af+, +af'+, +bc+, +bc'+, +de+, +hl+. Swaps registers unless out of screen.
        def bobs_copy_attrs(attrs=hl, rows=a, cols=c, target=de, scraddr:nil, subroutine:false)
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
        # * +bitmap+:: An address of a bitmap to be copied from as a label, pointer, an integer or one of the registers:
        #              +ix+, +iy+ or +sp+.
        # * +lines+:: A number of pixel lines to be copied as an 8-bit register or a label, pointer or an integer.
        # * +cols+:: A constant number of 8 pixel columns to be copied as an integer.
        # * +target+:: An address of a screen memory area to be copied to as a label, pointer, an integer or +hl+.
        #              The starting address of the whole screen area must be a multiple of 0x2000.
        #
        # Options:
        # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
        #                    only if you have already disabled the interrupts.
        # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
        #                   if you need to perform more uninterrupted actions.
        # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
        #               +sp+ will point to the beginning of the last cleared line.
        # * +scraddr+:: An optional screen memory address which must be a multiple of 0x2000 as an integer or a label.
        #               If provided the routine breaks execution when the bottom of the screen has been reached.
        # * +subroutine+:: Whether to create a subroutine.
        #
        # _NOTE_:: Restoring +sp+ register uses self-modifying code.
        #
        # Modifies: +af+, +af'+, +bc+, +de+, +hl+, optionally: +sp+.
        def bobs_copy_pixels_fast(bitmap, lines=c, cols=32, target=hl, disable_intr:true, enable_intr:true, save_sp:true, scraddr:nil, subroutine:false)
          raise ArgumentError, "invalid scraddr argument" unless scraddr.nil? or label?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000))
          raise ArgumentError, "target should be a label or an integer or HL register pair" unless target == hl or address?(target)
          raise ArgumentError, "bitmap should be a label or an integer or IX/IY/SP register pair" unless [sp,ix,iy].include?(bitmap) or address?(bitmap)
          raise ArgumentError, "lines should be a label or a pointer or an integer or a register" unless (register?(lines) and lines.bit8?) or
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
        # * +target+:: An address of screen attributes memory area to be copied to as a label, pointer, an integer or +hl+.
        #              The starting address of the whole screen area must be a multiple of 0x2000.
        #
        # Options:
        # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
        #                    only if you have already disabled the interrupts.
        # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
        #                   if you need to perform more uninterrupted actions.
        # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
        #               +sp+ will point to the beginning of the last cleared line.
        # * +check_oos+:: If +true+ reduces number of rows if the bottom of the screen would have been exceeded.
        # * +subroutine+:: Whether to create a subroutine.
        #
        # _NOTE_:: Restoring +sp+ register uses self-modifying code.
        #
        # Modifies: +af+, +bc+, +de+, +hl+, optionally: +sp+.
        def bobs_copy_attrs_fast(attrs, rows=a, cols=32, target=hl, disable_intr:true, enable_intr:true, save_sp:true, check_oos:false, subroutine:false)
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

      end

      include Z80
    end
  end
end
