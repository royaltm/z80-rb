module ZXLib
  module Gfx
    ##
    #  ==Bitmap objects related routines.
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
        #               +sp+ will point to the beginning of the last cleared line.
        # * +scraddr+:: An optional screen memory address which must be a multiple of 0x2000 as an integer or a label.
        #               If provided the routine breaks execution when the bottom of the screen has been reached.
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
        #               +sp+ will point to the beginning of the last cleared line.
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
                            ex   af, af        # a': cols, CF: 1 extend cols (right margin)
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

        # TODO: dyn width skip; cols.odd?
        # mode = :copy, :set, :or, :xor, :and
        def bobs_draw_pixels_fast(bitmap, lines=a, cols=2, target:hl, bshift:b, mode: :set, tx:ix, disable_intr:true, enable_intr:true, save_sp:true, scraddr:nil, subroutine:false)
          raise ArgumentError, "invalid scraddr argument" unless scraddr.nil? or direct_address?(scraddr) or (Integer === scraddr and scraddr == (scraddr & 0xE000))
          raise ArgumentError, "target should be an address or a pointer or hl" unless target == hl or address?(target)
          raise ArgumentError, "bitmap should be an address or a pointer or ix/iy/sp/hl'" unless [sp,ix,iy,hl].include?(bitmap) or address?(bitmap)
          raise ArgumentError, "lines should be a label or a pointer or an integer or a register" unless (register?(lines) and lines.bit8?) or
                                                                                                         address?(lines)
          cols = cols.to_i
          raise ArgumentError, "cols must be less than or equal to 32" if cols > 32
          raise ArgumentError, "cols must be even" unless cols.even?
          raise ArgumentError, "cols must be greater than or equal to 1" if cols < 1
          raise ArgumentError, "save_sp makes no sense if bitmap = sp" if bitmap == sp and save_sp
          raise ArgumentError, "bshift should be one of: b, c, d or e" unless [b,c,d,e].include?(bshift)
          raise ArgumentError, "tx should be one of: ix or iy" unless [ix, iy].include?(tx)
          txh, txl = tx.split
          t = b
          m = c
          fx = case mode
            when :or then ->(tgt) { ora tgt }
            when :xor then ->(tgt) { xor tgt }
            when :and then ->(tgt) { anda tgt }
            when :set, :copy then nil
            else
              raise ArgumentError, "mode should be one of: :or, :xor, :and, :set or :copy"
          end
          isolate do
                            ld   a, lines unless lines == a
                            ex   af, af     # a': lines

                            ld   hl, target unless target == hl
                            ld   a, l
                            exx
                            ld   c, a       # c: lo
                            exx
                            ld   a, h       # calculate counter based on screen address modulo 8
                            anda 0b11111000 # (h & 0b11111000)
                            sub  h          # (h & 0b11111000) - h % 8
                            exx
            if cols <= 2
                            ld   d, 8
                            add  d
            else
                            add  8          # 8 - h % 8
            end
                            ld   b, a       # b: counter: 8 - h % 8

                            ex   af, af     # a: lines, a': lo
            if cols <= 2
                            ld   e, a       # e: lines
                            dec  a          # a: lines - 1
                            sub  b          # a: lines - 1 - counter
                            jr   NC, more_rows
                            ld   b, e       # b: counter = c: lines
              more_rows     ex   af, af     # a: lines left - 1, CF: last row
            else
                            dec  a          # a: lines - 1 (remaining lines)
                            ld   e, a       # lines - 1
                            cp   b          # a: lines - 1 - counter
                            jr   NC, more_rows
                            ld   b, e       # b: counter = c: lines
                            inc  b
              more_rows     ld   d, b       # d: last counter
            end

            if [hl, sp, tx].include?(bitmap)
                            di if disable_intr
                            ld   [restore_sp_p], sp if save_sp
                            ld   sp, bitmap unless bitmap == sp
            end
                            exx
                            ld   a, bshift  # rshift
                            add  a, a
                            add  bshift     # * 3
                            ld   de, jump_table
                            add  e
                            ld   e, a

            if [hl, sp, tx].include?(bitmap)
                            ld   a, [de]
                            inc  e
                            ld   txl, a
                            ld   a, [de]
                            inc  e
                            ld   txh, a
                            ld   a, [de] # mask
                            ld   m, a
            else
                            ex   de, hl
                            di if disable_intr
                            ld   [restore_sp_p], sp if save_sp
                            ld   sp, hl
                            ex   de, hl
                            pop  tx
                            pop  t|m
                            ld   sp, bitmap
            end
                            jp   (tx) # hl: screen, m: mask, b': counter, d': counter, e': lines left - 1, c': lo

            jump_table      label
            (0..7).each do |index|
                            dw   define_label(:"line_rshift#{index}").loop0
                            db   0xff >> index
            end
            ################################################
            rotate_bits = ->(rshift) do
              if (1..4).include?(rshift)
                          rshift.times { rrca }
              elsif (5..7).include?(rshift)
                          (8-rshift).times { rlca }
              end
            end
            ns :line_rshift0 do
              rowloop       ld   a, c
                            exx
                            ld   l, a
              loop0         label
              (0...cols).step(2) do |col|
                            pop  de
                if fx.nil?
                            ld   [hl], e
                else
                            ld   a, e
                            fx.call [hl]
                            ld   [hl], a
                end
                            inc  l
                if fx.nil?
                            ld   [hl], d
                else
                            ld   a, d
                            fx.call [hl]
                            ld   [hl], a
                end
                            inc  l unless col == cols-2
              end
                            inc  h
                            exx
                            djnz rowloop
                            jp   next_row
            end
            [1,7,2,6,3,5,4].each do |rshift|
              ns :"line_rshift#{rshift}" do
                rowloop     ld   a, c
                            exx
                            ld   l, a
                loop0       label
                (0...cols).step(2) do |col|
                            pop  de
                            ld   a, e        # a: 76543210
                            rotate_bits.call rshift
                            ld   e, a        # e: 32107654
                            anda m           # a: ....7654 b: mask 00001111
                            ld   t, a unless mode == :copy && col == 0 # c: ....7654
                  if col != 0
                            ex   af, af
                            ora  t
                  elsif mode == :set
                            ld   a, m        # b: mask 00001111
                            cpl              # a: mask 11110000
                            anda [hl]        # a: ssss0000
                            ora  t           # a: ssss7654
                  end
                            fx.call [hl] unless fx.nil?
                            ld   [hl], a
                            inc  l
                            ld   a, t unless mode == :copy && col == 0 # a: ....7654
                            xor  e           # a: 3210....
                            ld   e, a        # e: 3210....
                            ld   a, d        # a: 76543210
                            rotate_bits.call rshift
                            ld   d, a        # d: 32107654
                            anda m           # a: ....7654 b: mask 00001111
                            ld   t, a        # c: ....7654
                            ora  e           # a: 32107654
                            fx.call [hl] unless fx.nil?
                            ld   [hl], a
                            inc  l
                  if mode == :set and col == cols - 2
                            ld   a, m        # b: mask 00001111
                            anda [hl]        # a: ....ssss
                            xor  t           # a: ....xxxx
                            xor  d           # a: 3210ssss
                            ld   [hl], a
                  else
                            ld   a, t        # a: ....7654
                            xor  d           # d: 3210....
                    if col == cols-2
                            fx.call [hl] unless fx.nil?
                            ld   [hl], a
                    else
                            ex   af, af      # a':3210....
                    end
                  end
                end
                            inc  h
                            exx
                            djnz rowloop
                            jp   next_row unless rshift == 4
              end
            end
            ################################################
            if cols <= 2
              next_row      ex   af, af
              if subroutine && !enable_intr && !save_sp
                            ret  C
              else
                            jr   C, quit
              end
                            ld   b, d       # 8
                            sub  b
                            jr   NC, not_last
                            add  b
                            ld   b, a
                            inc  b
              not_last      ex   af, af
            else
              next_row      ld   a, e       # lines left - 1
                            sub  d          # last counter
              if subroutine && !enable_intr && !save_sp
                            ret  C
              else
                            jr   C, quit
              end
                            ld   e, a       # lines left
                            ld   b, 8
                            cp   b
                            jr   NC, not_last
                            ld   b, a
                            inc  b
              not_last      ld   d, b       # last counter
            end

                            ld   a, c       # a: lo
                            add  0x20       # a: lo + 0x20
                            ld   c, a
                            exx
                            ld   l, a       # l: lo
                            jr   C, loop0fw unless scraddr
                            ld   a, h
                            jr   C, check_oos if scraddr
                            sub  8          # 8
                            ld   h, a       # h: adjusted
                            ld   a, l
            loop0fw         jp   (tx)
            if scraddr
              check_oos     cp   (scraddr >> 8)|0x18
                            ld   a, l
              if subroutine && !enable_intr && !save_sp
                            ret  NC
              else
                            jr   NC, quit
              end
                            jp   (tx)
            end
            quit            label
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
