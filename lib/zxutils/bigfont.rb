# -*- coding: BINARY -*-
require 'z80'
require 'zxlib/gfx'
require 'zxlib/sys'

module ZXUtils
  ##
  # Z80 Macros producing routines to create and display 16x15 characters from a 8x8 font
  # (e.g: a default ROM font) applying a simple anti-aliasing algorithm.
  #
  #   original character  character with widened pixels       final result of the algorithm
  #   
  #    7 6 5 4 3 2 1 0     f e d c b a 9 8 7 6 5 4 3 2 1 0     f e d c b a 9 8 7 6 5 4 3 2 1 0
  #   ░░░░░░░░░░░░░░░░ 0  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ 0  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
  #   ░░░░████████░░░░ 1  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ 1  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
  #   ░░██░░░░░░░░██░░ 2  ░░░░░░░░████████████████░░░░░░░░ 2  ░░░░░░░░████████████████░░░░░░░░
  #   ░░██░░░░░░░░██░░ 3  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ 3  ░░░░░░████░░░░░░░░░░░░████░░░░░░
  #   ░░██░░██░░░░██░░ 4  ░░░░████░░░░░░░░░░░░░░░░████░░░░ 4  ░░░░████░░░░░░░░░░░░░░░░████░░░░
  #   ░░██░░░░██░░██░░ 5  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ 5  ░░░░████░░░░░░░░░░░░░░░░████░░░░
  #   ░░░░████████░░░░ 6  ░░░░████░░░░░░░░░░░░░░░░████░░░░ 6  ░░░░████░░░░░░░░░░░░░░░░████░░░░
  #   ░░░░░░░░░░░░░░░░ 7  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ 7  ░░░░████░░░░░░░░░░░░░░░░████░░░░
  #                       ░░░░████░░░░████░░░░░░░░████░░░░ 8  ░░░░████░░░░████░░░░░░░░████░░░░
  #                       ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ 9  ░░░░████░░░░░░████░░░░░░████░░░░
  #                       ░░░░████░░░░░░░░████░░░░████░░░░ a  ░░░░████░░░░░░░░████░░░░████░░░░
  #                       ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ b  ░░░░░░████░░░░████████████░░░░░░
  #                       ░░░░░░░░████████████████░░░░░░░░ c  ░░░░░░░░████████████████░░░░░░░░
  #                       ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ d  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
  #                       ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ e  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
  #
  # Author:: Rafał Michalski, (c) 2018-2021
  class BigFont
    include Z80

    ###########
    # Exports #
    ###########

    export print_char

    ###########
    # Imports #
    ###########

    macro_import  ZXLib::Gfx
    import        ZXLib::Sys, macros: true, labels: true, code: false

    ##########
    # Macros #
    ##########

    ##
    # ==ZXUtils::BigFont macros.
    module Macros
      ##
      # Each bit of the +a+ register is duplicated and placed in the +f1+ and +f2+ registers.
      #
      # Modifies: +af+, +f1+ and +f2+. Optionally uses +b+ register if +unroll+ is +false.
      #
      # Preserves the +a+ register's content.
      #
      # If +unroll+ is +true+ instead of looping, the instructions are being unrolled.
      #
      #      Input bits        Output bits
      #   a: 76543210   -> f1: 77665544, f2: 33221100
      def widen_pixels8_16(f1, f2, unroll:true)
        raise ArgumentError unless [f1, f2].all?{|r| register?(r)} and f1 != f2
        isolate do
          if unroll
            4.times do
                        rrca
                        rr  f2
                        sra f2
            end
            4.times do
                        rrca
                        rr  f1
                        sra f1
            end
          else
                        ld   b, 4
            wideloop1   rrca
                        rr   f2
                        sra  f2
                        djnz wideloop1
                        ld   b, 4
            wideloop2   rrca
                        rr   f1
                        sra  f1
                        djnz wideloop2
          end
        end
      end
      ##
      # Interlaces pixels from the +f1+ and +f2+ registers into the +a+ register.
      #
      # Evaluates the given +block+ after 8 pixels have been mixed.
      # The block is being evaluated by Z80::Program#ns and should not alter +f1+ or +f2+ registers.
      #
      # If +unroll+ is +true+ instead of looping, the instructions are being unrolled.
      #
      # Modifies: +af+, +f1+ and +f2+. Optionally uses the +b+ register if +unroll+ is +false.
      #
      #       Input bits                   Output bits
      #   f1: FEDCBA98, f2: 76543210 -> a: F7E6D5C4, B3A29180
      #   f1: FDB97531, f2: ECA86420 -> a: FEDCBA98, 76543210
      def interlace_pixels16(f1, f2, unroll:true, &block)
        # ->(a, b) { 8.times.inject(0) {|c,i| c|(((b>>i)&1)<<(i*2))|(((a>>i)&1)<<(i*2+1)) } }
        isolate do
          if unroll
            4.times do
                      rlc  f1
                      rla
                      rlc  f2
                      rla
            end
                      ns(&block)
            4.times do
                      rlc  f1
                      rla
                      rlc  f2
                      rla
            end
          else
                      ld   b, 4
            mixloop1  rlc  f1
                      rla
                      rlc  f2
                      rla
                      djnz mixloop1
                      ns(&block)
                      ld   b, 4
            mixloop2  rlc  f1
                      rla
                      rlc  f2
                      rla
                      djnz mixloop2
          end
        end
      end
      ##
      # Mixes two consecutive 8-pixel lines into the 16-pixel middle anti-aliasing line.
      # The resulting bits from this routine needs to be interlaced in order to be displayed.
      #
      # On input the +a+ and +r+ registers should hold the 1st and the 2nd line respectively.
      # As a result the +o+ and +a+ registers should hold the mixed output pixels.
      #
      # * +t1+, +t2+:: Temporary 8-bit registers.
      #
      # Modifies: +af+, +o+, +t1+, +t2+. Preserves the +r+ register.
      #
      #   8-bit pixels  16-bit pixels
      #   76543210      FEDCBA98 76543210 bit index
      #   11010110      11110011 00111100 1st input line
      #                 00011110 00111000 anti-aliasing line
      #   00100100      00001100 00110000 2nd input line
      #
      #   Display result bits     Actual result bits
      #   FEDCBA98 76543210 -> o: FDB97531, a: ECA86420
      def mix_lines8_16(r=b, o:c, t1:d, t2:e)
        raise ArgumentError if [a, r, o, t1, t2].uniq.size != 5
        # ->(a, r) {
        #   a1 = a | (a>>1) & 0xff
        #   a2 = a | (a<<1) & 0xff
        #   r1 = r | (r>>1) & 0xff
        #   r2 = r | (r<<1) & 0xff
        #   o, a = r1 & a1 & (a|r), r2 & a2 & (a|r) }
        isolate do
                    ld   t1, a   # a
                    ora  r       # a|r, clears CF=0
                    ld   t2, a   # a|r
                    ld   a, t1
                    rra          # (a >> 1) CF==0
                    ora  t1      # (a >> 1) | a, clears CF=0
                    ld   o, a    # a1 = (a >> 1) | a
                    ld   a, r    # r
                    rra          # (r >> 1) CF==0
                    ora  r       # r1 = (r >> 1) | r
                    anda o       # r1&=a1
                    anda t2      # r1&a1&=(a|r)
                    ld   o, a    # r1&a1&(a|r)

                    ld   a, t1   # a
                    add  a       # (a << 1)
                    ora  t1      # (a << 1) | a
                    ld   t1, a   # a2 = a | (a << 1)
                    ld   a, r    # r
                    add  a       # (r << 1)
                    ora  r       # r2 = (r << 1) | r
                    anda t1      # r2&=a2
                    anda t2      # r2&a2&=(a|r)
        end
      end
      ##
      # Outputs an enlarged 8x8 character with anti-aliasing into the screen memory.
      #
      # * The register +hl+ should hold the address of the character data to print.
      # * The register +hl'+ should hold the address of the destination screen memory.
      # * The register +c'+ should hold the height of the character to print, usually 8.
      #
      # * +compact+:: set to +false+ for a longer, but faster code
      # * +over+:: set to one of: +:xor+, +:and+, +:or+ for the character pixels to be combined with the screen memory;
      #            +false+ to overwrite the screen memory
      # * +scraddr+:: screen memory address as an integer, must be a multiple of 0x2000,
      #               this is being used to prevent overwriting out of screen memory area.
      #               Provide +nil+ if you don't care.
      # * +assume_chars_aligned+:: +true+ if every byte of a character resides on the same 256 byte size address page.
      #                            This is true for e.g. an 8 pixels height character addressed at the multiple of 8.
      # * +hires+:: indicates rendering on SCLD or ULAplus hi-res screen.
      #
      # +hires+ options:
      #
      # * +:odd+: the left character is always on the odd column (on screen 0).
      #           In this instance the +hl'+ register should address the screen 0.
      #           Adds at least 8 T-states to each character line.
      # * +:even+: the left character is always on the even column (on screen 1).
      #           In this instance the +hl'+ register should address the screen 1.
      #           Adds at least 16 T-states to each character line.
      # * +:any+ or +true+: rendering character on any of the 64 columns (slow).
      #                     Adds at least 59 T-states to each character line.
      #
      # Modifies: +af+, +bc+, +de+, +hl+, +af'+, +bc'+, +de'+, +hl'+
      def enlarge_char8_16(compact:true, over:false, scraddr:0x4000, assume_chars_aligned:true, hires:nil)
        combine = case over
        when :xor
          proc {|x| xor x }
        when :and
          proc {|x| anda x }
        else
          proc {|x| ora x }
        end

        case hires
        when :odd
          screen_right_column = proc{ set 5, h }
          screen_left_column  = proc{ res 5, h }
        when :even
          screen_right_column = proc{ res 5, h; inc l }
          screen_left_column  = proc{ set 5, h; dec l }
        when :any, true
          screen_right_column = proc do
            isolate do |eoc|
              ld   a, h
              xor  0x20
              ld   h, a
              anda 0x20
              jr   NZ, eoc
              inc  l
            end
          end
          screen_left_column = proc do
            isolate do |eoc|
              ld   a, h
              xor  0x20
              ld   h, a
              anda 0x20
              jr   Z, eoc
              dec  l
            end
          end
        when nil
          screen_right_column = proc{ inc l }
          screen_left_column  = proc{ dec l }
        else
          raise ArgumentError, "enlarge_char8_16: invalid hires argument!"
        end

        scraddr_hires = false
        unless compact
          case hires
          when :odd
            scraddr_right = (scraddr|0x2000)
            scraddr_left = (scraddr&~0x2000)
          when :even
            scraddr_right = (scraddr&~0x2000)
            scraddr_left = (scraddr|0x2000)
          else
            scraddr_hires = !hires.nil?
            scraddr_right = scraddr
            scraddr_left = scraddr
          end
        end if scraddr

        isolate do |eoc|         # hl' - screen adddress, c' source height, hl - character address
          bcheck = if scraddr
            if compact then true else eoc end
          else
            false
          end
                    ld   a, [hl] # 1st line
          if assume_chars_aligned
                    inc  l       # assuming characters are aligned to 8 bytes
          else
                    inc hl
          end
          mloop     exx
                    widen_pixels8_16 d, e, unroll:!compact
                    ex   af, af  # preserve the input line
          if over
                    ld   a, d
                    combine.call [hl]
                    ld   [hl], a # put on screen over
                    screen_right_column.call
                    ld   a, e
                    combine.call [hl]
                    ld   [hl], a # put on screen over (leave hl at 2nd half)
          else
                    ld   [hl], d # put on screen
                    screen_right_column.call
                    ld   [hl], e # put on screen (leave hl at 2nd half)
          end
          if compact
                    call next_line
          else
                    nextline h, l, bcheck, scraddr:scraddr_right, hires:scraddr_hires
          end
                    dec  c
                    jr   Z, eoc  # 15th line?
                    ex   af, af  # restore the input line
                    exx

                    ld   c, [hl] # next input line
          if assume_chars_aligned
                    inc  l       # assuming characters are aligned to 8 bytes
          else
                    inc hl
          end
                    mix_lines8_16 c, o:d, t1:b, t2:e # d: 1st half
                    ld   e, a    # e: 2nd half

                    interlace_pixels16(d, e, unroll:!compact) do
                      ex af, af  # save 1st half
                    end
                    exx
          if over
                    combine.call [hl]
                    ld   [hl], a # put on screen 2nd half over
                    screen_left_column.call
                    ex   af, af
                    combine.call [hl]
                    ld   [hl], a # put on screen 1st half over
          else
                    ld   [hl], a # put on screen 2nd half
                    screen_left_column.call
                    ex   af, af
                    ld   [hl], a # put on screen 1st half
          end
          if compact
                    call next_line
          else
                    nextline h, l, bcheck, scraddr:scraddr_left, hires:scraddr_hires
          end
                    exx
                    ld   a, c    # 2nd line
                    jp   mloop
          if compact
          next_line label
                    nextline h, l, bcheck, scraddr:scraddr, hires:hires do
                      pop af     # discard return address
                      jr  eoc
                    end
                    ret
          end
        end
      end
    end
    extend Macros

    ##############
    # PRINT CHAR #
    ##############

    ##
    # ZX Spectrum's ROM compatible CHAN output routine
    #
    # The +a+ register has the output code.
    ns :print_char do
      # The routine may modify the registers AF, AF', BC, DE, HL, IX. We should only preserve an alternative set
      # which is a regular set for the system. Modifying IY is not a good idea without disabling interrupts first.
      with_saved :exx, bc, de, hl, merge: true do
                      ld   de, [cursor]
                      ld   hl, flags
                      bit  0, [hl]
                      jp   NZ, at_control

                      cp   0x20             # control code ?
                      jp   C, control_char
                      ex   af, af           # save code

                      ld   a, d
                      cp   192              # out of screen ?
                      jp   NC, rom.error_5

                      push de               # save coordinates
                                            # calculate screen address in HL
                      ytoscr d, col:e, t:c
                      ld   c, 8             # 8 character lines
                      exx                   # save screen address and height

                      ex   af, af           # restore code
                      cp   0x80             # ASCII ?
                      jr   C, ascii_code

                      sub  0x90             # block graphic character ?
                      jr   NC, udg_code
                      ld   b, a
                      call rom.po_gr_1      # creates a block character in MEM-0
                      ld   hl, vars.membot
                      jr   output_char

      udg_code        ld   hl, [vars.udg]
                      jr   code2address

      ascii_code      ld   hl, [vars.chars]
      code2address    char_ptr_from_code hl, a, tt:de
                                            # hl' = screen address, c' = 8, hl = char address
      output_char     enlarge_char8_16(compact:true, assume_chars_aligned:false)

                      pop  de               # restore coordinates
                      inc  e
                      inc  e

      check_col       ld   a, e
                      cp   0x1f
                      jr   C, exit_save
      next_line       ld   e, 0
                      ld   a, 16
                      add  d
                      ld   d, a

      exit_save       ld   [cursor], de
      end # with_saved
                      ret

      control_char    cp   ?\r.ord      # ENTER
                      jr   NZ, skip_eol
                      ld   e, 0x20
                      jr   check_col
      skip_eol        cp   0x16         # AT (y, x)
                      jr   NZ, skip_at
                      ld   [hl], 0x03   # flags = AT_ROW
                      jr   exit_save
      skip_at         cp   0x17         # TAB (x)
                      jr   NZ, exit_save
                      ld   [hl], 0x01   # flags = AT_COL
                      jr   exit_save    # ignore anything else

      at_control      bit  1, [hl]
                      jr   Z, at_col_ctrl
                      ld   [hl], 0x01   # flags = AT_COL
                      ld   d, a         # set row
                      jr   exit_save
      at_col_ctrl     ld   [hl], 0x00   # flags = NONE
                      ld   e, a         # set col
                      jr   check_col

      cursor          words 1
      flags           bytes 1
    end
  end

  class BigFontHires
    include Z80

    ###########
    # Exports #
    ###########

    export print_char_hires

    ###########
    # Imports #
    ###########

    macro_import  ZXLib::Gfx
    import        ZXLib::Sys, macros: true, labels: true, code: false
    macro_import  BigFont

    #####################
    # PRINT CHAR HI-RES #
    #####################

    ##
    # ZX Spectrum's ROM compatible CHAN output routine, for hi-res mode.
    #
    # The +a+ register has the output code.
    ns :print_char_hires do
      # The routine may modify the registers AF, AF', BC, DE, HL, IX. We should only preserve an alternative set
      # which is a regular set for the system. Modifying IY is not a good idea without disabling interrupts first.
      with_saved :exx, bc, de, hl, merge: true do
                      ld   de, [cursor]
                      ld   hl, flags
                      bit  0, [hl]
                      jp   NZ, at_control

                      cp   0x20             # control code ?
                      jp   C, control_char
                      ex   af, af           # save code

                      ld   a, d
                      cp   192              # out of screen ?
                      jp   NC, rom.error_5

                      push de               # save coordinates
                                            # calculate screen address in HL
                      ytoscr d, col:e, t:c
                      ld   c, 8             # 8 character lines
                      exx                   # save screen address and height

                      ex   af, af           # restore code
                      cp   0x80             # ASCII ?
                      jr   C, ascii_code

                      sub  0x90             # block graphic character ?
                      jr   NC, udg_code
                      ld   b, a
                      call rom.po_gr_1      # creates a block character in MEM-0
                      ld   hl, vars.membot
                      jr   output_char

      udg_code        ld   hl, [vars.udg]
                      jr   code2address

      ascii_code      ld   hl, [vars.chars]
      code2address    char_ptr_from_code hl, a, tt:de
                                            # hl' = screen address, c' = 8, hl = char address
      output_char     enlarge_char8_16(compact:true, assume_chars_aligned:false, hires: :odd)

                      pop  de               # restore coordinates
                      inc  e

      check_col       ld   a, e
                      cp   0x20
                      jr   C, exit_save
      next_line       ld   e, 0
                      ld   a, 16
                      add  d
                      ld   d, a

      exit_save       ld   [cursor], de
      end # with_saved
                      ret

      control_char    cp   ?\r.ord      # ENTER
                      jr   NZ, skip_eol
                      ld   e, 0x20
                      jr   check_col
      skip_eol        cp   0x16         # AT (y, x)
                      jr   NZ, skip_at
                      ld   [hl], 0x03   # flags = AT_ROW
                      jr   exit_save
      skip_at         cp   0x17         # TAB (x)
                      jr   NZ, exit_save
                      ld   [hl], 0x01   # flags = AT_COL
                      jr   exit_save    # ignore anything else

      at_control      bit  1, [hl]
                      jr   Z, at_col_ctrl
                      ld   [hl], 0x01   # flags = AT_COL
                      ld   d, a         # set row
                      jr   exit_save
      at_col_ctrl     ld   [hl], 0x00   # flags = NONE
                      ld   e, a         # set col
                      jr   check_col

      cursor          words 1
      flags           bytes 1
    end
  end
end