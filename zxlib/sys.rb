# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'
##
#  ==A module with Z80 macros for common ZX Spectrum system tasks.
#
#  Contains:
#
#  * labels for some of ZX Spectrum 16/48K ROM routine addresses in a +rom+ namespace.
#  * labels for ZX Spectrum 16/48K memory layout in a +mem+ namespace.
#  * labels for ZX Spectrum 16/48K basic and system variables in a +vars+ namespace.
#  * Macros for various tasks tied to the ZX Spectrum hardware or ZX Basic.
#
#  Requires:
#    macro_import Z80MathInt
#
#  Example:
#
#    require 'zxlib/sys'
#
#    class Program
#      include Z80
#
#      macro_import Z80MathInt
#      import ZXSys, macros: true
#
#      open_chan   create_chan_and_open output: output_p, chan_name: 'X'
#                  ret
#      output_p    # ... do something with register A
#
#      # use memory hardware layout labels
#                  ld   de, mem.attrs
#                  ld   bc, mem.attrlen
#      # use IY register to address VARS table
#                  ld   a, [iy + vars.bordcr - vars_iy]
#      # use VARS table directly
#                  ld   hl, [vars.seed]
#      # call ROM routines
#                  call rom.break_key
#                  report_error_unless C, 'D BREAK - CONT repeats'
#    end
class ZXSys
    include Z80

    ##
    # A struct for ZX Spectrum +coords+ variable.
    class Coords < Label
        x byte
        y byte
    end

    ##
    # A struct for various ZX Spectrum variables.
    class Cursor < Label
        column byte
        line   byte
    end

    ##
    # A struct for ZX Spectrum +strms+ variable.
    class Strms < Label
        sys  word,  3
        user word, 16
    end

    ##
    # ZX Spectrum Basic and System variables.
    class Vars < Label
        kstate byte, 8 # 23552 Used in reading the keyboard.
        last_k byte    # 23560 Stores newly pressed key.
        repdel byte    # 23561 Time (in 50ths of a second in 60ths of a second in N. America) that a key must be held down before it repeats. This starts off at 35, but you can POKE in other values.
        repper byte    # 23562 Delay (in 50ths of a second in 60ths of a second in N. America) between successive repeats of a key held down: initially 5.
        defadd word    # 23563 Address of arguments of user defined function if one is being evaluated; otherwise 0.
        k_data byte    # 23565 Stores 2nd byte of colour controls entered from keyboard .
        tvdata word    # 23566 Stores bytes of coiour, AT and TAB controls going to television.
        strms  Strms   # 23568 Addresses of channels attached to streams.
        chars  word    # 23606 256 less than address of character set (which starts with space and carries on to the copyright symbol). Normally in ROM, but you can set up your own in RAM and make CHARS point to it.
        rasp   byte    # 23608 Length of warning buzz.
        pip    byte    # 23609 Length of keyboard click.
        err_nr byte    # 23610 1 less than the report code. Starts off at 255 (for 1) so PEEK 23610 gives 255.
        flags  byte    # 23611 Various flags to control the BASIC system.
        tv_fla byte    # 23612 Flags associated with the television.
        err_sp word    # 23613 Address of item on machine stack to be used as error return.
        list_s word    # 23615 Address of return address from automatic listing.
        mode   byte    # 23617 Specifies K, L, C. E or G cursor.
        newppc word    # 23618 Line to be jumped to.
        nsppc  byte    # 23620 Statement number in line to be jumped to. Poking first NEWPPC and then NSPPC forces a jump to a specified statement in a line.
        ppc    word    # 23621 Line number of statement currently being executed.
        subppc byte    # 23623 Number within line of statement being executed.
        bordcr byte    # 23624 Border colour * 8; also contains the attributes normally used for the lower half of the screen.
        e_ppc  word    # 23625 Number of current line (with program cursor).
        vars   word    # 23627 Address of variables.
        dest   word    # 23629 Address of variable in assignment.
        chans  word    # 23631 Address of channel data.
        curchl word    # 23633 Address of information currently being used for input and output.
        prog   word    # 23635 Address of BASIC program.
        nxtlin word    # 23637 Address of next line in program.
        datadd word    # 23639 Address of terminator of last DATA item.
        e_line word    # 23641 Address of command being typed in.
        k_cur  word    # 23643 Address of cursor.
        ch_add word    # 23645 Address of the next character to be interpreted: the character after the argument of PEEK, or the NEWLINE at the end of a POKE statement.
        x_ptr  word    # 23647 Address of the character after the ? marker.
        worksp word    # 23649 Address of temporary work space.
        stkbot word    # 23651 Address of bottom of calculator stack.
        stkend word    # 23653 Address of start of spare space.
        breg   byte    # 23655 Calculator's b register.
        mem    word    # 23656 Address of area used for calculator's memory. (Usually MEMBOT, but not always.)
        flags2 byte    # 23658 More flags.
        df_sz  byte    # 23659 The number of lines (including one blank line) in the lower part of the screen.
        s_top  word    # 23660 The number of the top program line in automatic listings.
        oldppc word    # 23662 Line number to which CONTINUE jumps.
        ospcc  byte    # 23664 Number within line of statement to which CONTINUE jumps.
        flagx  byte    # 23665 Various flags.
        strlen word    # 23666 Length of string type destination in assignment.
        t_addr word    # 23668 Address of next item in syntax table (very unlikely to be useful).
        seed   word    # 23670 The seed for RND. This is the variable that is set by RANDOMIZE.
        frames byte, 3 # 23672 3 byte (least significant first), frame counter. Incremented every 20ms. See Chapter 18.
        udg    word    # 23675 Address of 1st user defined graphic You can change this for instance to save space by having fewer user defined graphics.
        coords Coords  # 23677 Coordinate of last point plotted.
        p_posn byte    # 23679 33 column number of printer position
        pr_cc  word    # 23680 Full address of next position for LPRINT to print at (in ZX printer buffer). Legal values 5B00 - 5B1F. [Not used in 128K mode or when certain peripherals are attached]
        echo_e word    # 23682 33 column number and 24 line number (in lower half) of end of input buffer.
        df_cc  word    # 23684 Address in display file of PRINT position.
        dfccl  word    # 23686 Like DF CC for lower part of screen.
        s_posn Cursor  # 23688 33 column number for PRINT position, 24 line number for PRINT position.
        sposnl Cursor  # 23690 Like S POSN for lower part
        scr_ct byte    # 23692 Counts scrolls: it is always 1 more than the number of scrolls that will be done before stopping with scroll? If you keep poking this with a number bigger than 1 (say 255), the screen will scroll on and on without asking you.
        attr_p byte    # 23693 Permanent current colours, etc (as set up by colour statements).
        mask_p byte    # 23694 Used for transparent colours, etc. Any bit that is 1 shows that the corresponding attribute bit is taken not from ATTR P, but from what is already on the screen.
        attr_t byte    # 23695 Temporary current colours, etc (as set up by colour items).
        mask_t byte    # 23696 Like MASK P, but temporary.
        p_flag byte    # 23697 More flags.
        membot byte,30 # 23698 Calculator's memory area; used to store numbers that cannot conveniently be put on the calculator stack.
        nmiadd word    # 23728 This is the address of a user supplied NMI address which is read by the standard ROM when a peripheral activates the NMI. Probably intentionally disabled so that the effect is to perform a reset if both locations hold zero, but do nothing if the locations hold a non-zero value. Interface 1's with serial number greater than 87315 will initialize these locations to 0 and 80 to allow the RS232 "T" channel to use a variable line width. 23728 is the current print position and 23729 the width - default 80.
        ramtop word    # 23730 Address of last byte of BASIC system area.
        p_ramt word    # 23732 Address of last byte of physical RAM.
    end

    export :auto

    vars    addr 23552, Vars
    vars_iy addr vars.err_nr - vars

    isolate :rom do
        start       addr 0x0000 # THE 'START'
        error_1     addr 0x0008 # THE 'ERROR' RESTART
        print_a     addr 0x0010 # THE 'PRINT CHARACTER' RESTART
        get_char    addr 0x0018 # THE 'COLLECT CHARACTER' RESTART
        next_char   addr 0x0020 # THE 'COLLECT NEXT CHARACTER' RESTART
        fp_calc     addr 0x0028 # THE 'CALCULATE' RESTART
        bc_spaces   addr 0x0030 # THE 'CREATE BC SPACES' RESTART
        mask_int    addr 0x0038 # THE 'MASKABLE INTERRUPT' ROUTINE
        key_int     addr 0x0048 # KEY-INT part of the interrupt routine
        keyboard    addr 0x02BF # read KEYBOARD routine called from the system interrupt routine
        po_any      addr 0x0B24 # THE 'PRINT ANY CHARACTER' ROUTINE
        po_gr_1     addr 0x0B38 # The 16 2*2 mosaic characters 128-143 decimal are formed from bits 0-3 of the character
        error_5     addr 0x0C86 # Out of screen
        cl_all      addr 0x0DAF # CL-ALL
        error_j     addr 0x15C4 # Invalid I/O device
        chan_open   addr 0x1601 # THE 'OPEN CHANNEL' ROUTINE
        make_room   addr 0x1655 # ROM MAKE-ROOM routine
        free_mem    addr 0x1F1A # free memory in result BC
        break_key   addr 0x1F54 # returns CF=0 if shift+space (break) is being pressed
        pr_string   addr 0x203C # PR-STRING: start in DE, length in BC
        stk_store   addr 0x2AB6 # Pushes five registers on the calculator stack: A, E, D, C, B if there is enough room
        stk_fetch   addr 0x2BF1 # Pops five registers from the calculator stack: A, E, D, C, B
        stack_a     addr 0x2D28 # Pushes accumulator on the calculator stack as a small integer
        stack_bc    addr 0x2D2B # Pushes BC register pair on the calculator stack as a small integer
        print_fp    addr 0x2DE3 # Print a floating point number
        int_fetch   addr 0x2D7F # HL: point to FP-value, restores the twos complement number to normal in DE and a sign in C.
    end

    isolate :mem do
        font_p  addr 0x3D00
        romtop  addr 0X3FFF
        screen  addr 0x4000
        scrlen  32*192
        attrs   addr 0x5800
        attrlen 32*24
        rambot  addr 0x5B00
        pr_buf  addr 0x5B00
        vars    addr 0x5C00
        mmaps   addr 0x5CB6
    end

    module Macros
        ##
        # Returns to ZX Basic with the error report if condition is NOT met.
        #
        # * condition:: NZ, Z, NC, C, PO, PE, P, M
        # * error:: Error report signature as a number +0..9+ or a letter +A..R+
        def report_error_unless(condition, error)
            raise ArgumentError unless Condition === condition
            isolate do |eoc|
                if condition.jr_ok?
                        jr   condition, eoc
                else
                        jp   condition, eoc
                end
                err     report_error error
            end
        end
        ##
        # Returns to ZX Basic with the error report.
        #
        # * error:: Error report signature as a number +0..9+ or a letter +A..R+
        def report_error(error)
            errno = [*'0'..'9', *'A'..'R'].index(error.to_s.upcase[0])
            raise ArgumentError unless errno
            isolate do
                        rst  0x08
                        db   errno - 1
            end
        end
        ##
        # Setup custom interrupt handler using ZX Spectrum ROM's unused space as a IM2 mode jump table.
        #
        # * handler:: One of the 16bit register pair or an address or a pointer to the address
        #             of the handler routine.
        # * enable_interrupts:: If +true+ invoke +ei+ instruction at the end.
        #
        # Modifies: +a+, +i+, +hl+ if +handler+ is not a register pair.
        def setup_custom_interrupt_handler(handler, enable_interrupts:true)
            isolate do
                            ld  a, 0x18          # 18H is jr
                            ld  [0xFFFF], a
                            ld  a, 0xC3          # C3H is jp
                            ld  [0xFFF4], a
                set_handler label                # set handler part (may be used to only change the routine address)
                if [bc, de, hl, sp, ix, iy].include?(handler)
                            ld  [0xFFF5], handler
                else
                            ld  hl, handler
                            ld  [0xFFF5], hl
                end
                            ld  a, 0x39
                            ld  i, a             # load the accumulator with FF filled page in rom.
                            im2
                            ei if enable_interrupts
            end
        end
        ##
        # Restore interrupt handler ZX Spectrum ROM's standard IM1 mode.
        #
        # * enable_interrupts:: If +true+ invoke +ei+ instruction at the end.
        #
        # Modifies: +a+, +i+.
        def restore_rom_interrupt_handler(enable_interrupts:true)
            isolate do
                    im1
                    ld  a, 0x3F
                    ld  i, a
                    ei if enable_interrupts
            end
        end
        ##
        # Test for a key or keys being pressed.
        #
        # * line_mask:: Keyboard half-line mask, may be an 8 bit register.
        #               The default is 0 which means all available lines.
        # * key_mask:: Key mask (b0..b4), may be an 8 bit register.
        #              The default is 0b11111 which means all keys in a half-line.
        #
        #           key bits                           key bits
        #     line  b4,  b3,  b2,  b1,      b0   line  b4,  b3,  b2,   b1,      b0
        #     0xf7 [5], [4], [3], [2],     [1]   0xef [6], [7], [8],  [9],     [0]
        #     0xfb [T], [R], [E], [W],     [Q]   0xdf [Y], [U], [I],  [O],     [P]
        #     0xfd [G], [F], [D], [S],     [A]   0xbf [H], [J], [K],  [L], [ENTER]
        #     0xfe [V], [C], [X], [Z], [SHIFT]   0x7f [B], [N], [M], [SS], [SPACE]
        #
        # Modifies: +af+.
        #
        # Output:
        # * +ZF+=0:: if any of the specified keys is being pressed.
        # * +a+:: bits b0..b4=1 if a key is being pressed at any of the specified half-line.
        def key_pressed?(line_mask=0, key_mask=0x1f)
            isolate do
              if line_mask == 0
                    xor  a
              else
                    ld   a, line_mask unless line_mask == a
              end
                    inp  a, (254)
                    cpl
                    anda key_mask
            end
        end
        ##
        # Test for cursor keys being pressed.
        #
        # +t+:: a temporary register.
        #
        # Modifies: +af+, +t+.
        #
        # Output:
        # * +ZF+=0:: if any of the cursor keys is being pressed.
        # * +a+:: bits b0..b3=1 if a cursor key is being pressed.
        #
        #      b3  b2  b1  b0
        #     [←] [↓] [↑] [→]
        def cursor_key_pressed?(t:b)
            isolate do
                    key_pressed? 0xf7, 0x10 # key [5]
                    ld   t, a
                    key_pressed? 0xef, 0x1c # keys [6] [7] [8] . .
                    rrca
                    ora  t                  # keys [5] [6] [7] [8] .
                    rrca                    # keys [←] [↓] [↑] [→]
            end
        end
        ##
        # Calculates the address of the first byte of a character.
        # The calculated address will be available in the +hl+ register.
        #
        # * +chars+:: the address of a code=0 character as a +hl+ register, address,
        #             label or a label pointer e.g.: [vars.chars].
        # * +code+:: an 8-bit register or a code number (addresses and pointers works to).
        # * +tt+:: a 16-bit register except +hl+.
        #
        # Modifies: +af+, +tt+, +hl+
        def char_ptr_from_code(chars, code=a, tt:de)
          raise ArgumentError unless ((register?(chars) and chars == hl) or address?(chars)) and
                                     register?(tt) and !tt.bit8? and tt != hl
          th, tl = tt.split
          isolate do
                        ld   a, code unless code == a
                        ld   hl, chars unless chars == hl
                        3.times { rlca }
                        ld   tl, a
                        anda 0b00000111
                        ld   th, a
                        xor  tl
                        ld   tl, a
                        add  hl, th|tl
          end
        end
        ##
        # Creates ZX Spectrum CHAN entry and opens it as stream #N.
        #
        # * output:: output routine address or a 16bit register holding that address except +hl+
        # * input:: input routine address or a 16bit register holding that address
        # * strm_no:: stream number (4 - 15) or 8bit register name
        # * chan_name:: a channel name immediate value
        #
        # Optionally give namespace label a +name+.
        #
        # Modifies: +af+, +hl+, +bc+, +de+
        def create_chan_and_open(name = nil, output:, input: nil, strm_no: 4, chan_name: 'U')
            chan_name = String === chan_name ? chan_name.ord : chan_name.to_i
            raise ArgumentError, "output or input must not be the hl register" if output == hl or input == hl
            isolate name, use: [:rom, :vars] do
                input  = rom.error_j if input.nil?
                output = rom.error_j if output.nil?
                if register?(strm_no) or pointer?(strm_no)
                    unless strm_no == a
                        ld   a, strm_no   # stream to open
                    end
                        add  a, a         # calculate the strm vector offset in hl
                        ld   hl, vars.strms.user # hl points to STRMS #0
                        adda_to h, l
                        push hl
                end
                        push input if register?(input)
                        push output if register?(output)
                        ld   hl, [vars.prog]  # a new channel starts below prog
                        dec  hl          #
                        ld   bc, 0x0005  # make space
                        call rom.make_room
                        inc  hl          # hl points to 1st byte of new channel data
                if register?(output)
                        pop de
                else
                        ld   de, output
                end
                        ld   [hl], e
                        inc  hl
                        ld   [hl], d
                if register?(input)
                        pop de
                else
                        ld   de, input
                end
                        push hl          # save address of 2nd byte of new channel data
                        inc  hl
                        ld   [hl], e
                        inc  hl
                        ld   [hl], d
                        inc  hl
                        ld   a, chan_name
                        ld   [hl], a     # channel name
                        pop  hl          # get address of 2nd byte of output routine
                        ld   de, [vars.chans] # calculate the offset to the channel data
                        anda a           # and store it in de
                        sbc  hl, de
                        ex   de, hl      # de = our channel address - chans + 1
                    if register?(strm_no) or pointer?(strm_no)
                        pop  hl
                    else
                        ld   hl, vars.strms.user[strm_no]
                    end
                        ld   [hl], e      # lsb of 2nd byte of new channel data
                        inc  hl
                        ld   [hl], d      # msb of 2nd byte of new channel data
            end
        end
        ##
        # Looks for a ZX Spectrum CHAN entry determined by +output+, +input+ and a +chan_name+.
        #
        # * output:: output routine address or a 16bit register holding that address except +hl+
        # * input:: input routine address or a 16bit register holding that address
        # * strm_no:: stream number (4 - 15) or 8bit register name
        # * chan_name:: a channel name immediate value or a 8bit register
        # * buffer:: address of buffer (5 bytes) to use, by default it's a printer buffer at 23296
        #
        # Optionally give namespace label a +name+.
        #
        # ZF=1 if found, +hl+ points to the record address that matches
        # ZF=0 if not found, +hl+ points to the memory address immediately after the last record
        #
        # Modifies: +af+, +hl+, +bc+, +de+
        def chan_exists(name = nil, output: de, input: nil, chan_name: 'U', buffer: 23296)
            chan_name = chan_name.ord if String === chan_name
            raise ArgumentError, "output or input must not be hl register pair" if output == hl or input == hl
            isolate name, use: [:rom, :vars] do |eoc|
                input  = rom.error_j if input.nil?
                output = rom.error_j if output.nil?
                        ld   a, chan_name unless chan_name == a
                        ld   hl, buffer
                        push hl
                oh, ol = if register?(output)
                        output.split
                else
                        ld   de, output
                        [d, e]
                end
                        ld   [hl], ol
                        inc  hl
                        ld   [hl], oh
                        inc  hl
                ih, il = if register?(input)
                        input.split
                else
                        ld   de, input
                        [d, e]
                end
                        ld   [hl], il
                        inc  hl
                        ld   [hl], ih
                        inc  hl
                        ld   [hl], a
                        pop  de            # de record address
                        exx
                        push hl
                        ld   hl, [prog]
                        dec  hl
                        exx                # hl' stop search address
                        ld   bc, 5         # record length
                        ld   hl, [chan]    # CHAN
                        find_record h, l
                        jr   NZ, cleanup
                        sbc  hl, bc        # beginning of the record found
                cleanup exx
                        pop  hl
                        exx
            end
        end
        ##
        # Search for a record that matches large block of a memory.
        #
        # * +th|tl'+:: address of the last byte to search + 1 (preserved)
        # * +hl+:: target
        # * +de+:: source (preserved)
        # * +bc+:: record length (preserved)
        #
        # ZF=1 if found, +hl+ points immediately *after* the record that matches, CF=0
        # ZF=0 if not found, +hl+ points to the memory address immediately after the last record
        #
        # Modifies: +af+, +hl+ and +stack+
        def find_record(th=h, tl=l)
            isolate do |eoc|
                seekloop1   push  bc
                            push  de
                seekloop0   ld    a, [de]
                            inc   de
                            cpi
                            jr    NZ, adjust
                            ld    a, b
                            ora   c
                            jr    NZ, seekloop0
                            pop   de
                            pop   bc           # de - preserved, bc - preserved, hl to next record after found
                            jr    eoc          # ZF=1 found
                adjust      add   hl, bc       # next record
                            pop   de
                            pop   bc
                            ld    a, h
                            exx
                            cp    th
                            exx
                            jr    C,  seekloop1  # h < h'
                            jr    NZ, eoc    # h > h'
                            ld    a, l
                            exx
                            cp    tl
                            exx
                            jr    C,  seekloop1  # l < l'
                            ora   1              # ZF = 0
            end
        end
        ##
        # Read a signed integer from a ZX Basic's FP-value.
        #
        # +hl+:: must point to the 1st byte of the FP-value.
        # +th+:: most significant byte output register.
        # +tl+:: least significant byte output register.
        # +sgn+:: sign byte register.
        #
        # The twos complement is not being converted for a negative number.
        #
        # Result is being loaded into +th+|+tl+ and +sgn+.
        # ZF=0 signals the FP-value is not an integer.
        # +hl+ will always point to the last byte of the FP-value.
        #
        # Modifies: +af+, +hl+, +th+, +tl+ and +sgn+
        def read_integer_value(th=d, tl=e, sgn=c)
            raise ArgumentError if [h, l, a].include?(tl) or a == th or a == sgn
            isolate do
                            ld    a, [hl]
                            inc   hl
                            ld    sgn, [hl]
                            inc   hl
                            ld    tl, [hl]
                            inc   hl
                            ld    th, [hl]
                            inc   hl
                            ora   [hl]
            end
        end
        ##
        # Read a positive integer from a ZX Basic's FP-value.
        #
        # +hl+:: must point to the 1st byte of the FP-value.
        # +th+:: most significant byte output register.
        # +tl+:: least significant byte output register.
        #
        # Result is being loaded into +th+|+tl+.
        # ZF=0 signals the FP-value is not a positive integer.
        # +hl+ will always point to the last byte of the FP-value.
        #
        # Modifies: +af+, +hl+, +th+ and +tl+
        def read_positive_int_value(th=d, tl=e)
            raise ArgumentError if [h, l, a].include?(tl) or a == th
            isolate do
                            ld    a, [hl]
                            inc   hl
                            ora   [hl]
                            inc   hl
                            ld    tl, [hl]
                            inc   hl
                            ld    th, [hl]
                            inc   hl
                            ora   [hl]
            end
        end
        ##
        # Read a string address and its length from a ZX Basic's stringish FP-value.
        #
        # +hl+:: must point to the 1st byte of the FP-value.
        # +adh+:: most significant byte address output register.
        # +adl+:: least significant byte address output register.
        # +lenh+:: most significant byte length output register.
        # +lenl+:: least significant byte length output register.
        #
        # +hl+ will point to the last byte of the FP-value.
        #
        # Modifies: +hl+, +adh+, +adl+, +lenl+, +lenh+
        def read_arg_string(adh=d, adl=e, lenh=b, lenl=c)
            raise ArgumentError if [adh, adl, lenl].any? {|r| [h, l].include?(r) }
            isolate do
                            inc   hl
                            ld    adl, [hl]
                            inc   hl
                            ld    adh, [hl]
                            inc   hl
                            ld    lenl, [hl]
                            inc   hl
                            ld    lenh, [hl]
            end
        end
        ##
        # Get a DEF FN argument value address.
        #
        # * +argnum+:: 1-based argument index (0 is 256), may be a register or a number.
        # * +subroutine+:: if +true+ will use +ret+ instruction.
        # * +not_found+:: if +subroutine+ is +false+ and +not_found+ is defined, the routine
        #                 will jump to this address when argument was not found,
        #                 otherwise success is signalled with ZF=1.
        # * +cf_on_direct+:: if +true+ and DEFADD is not defined CF will be set.
        #
        # When +subroutine+ is +true+ or +not_found+ is +nil+, the success is signalled with +ZF+:
        #
        # * ZF=1 if found,
        # * ZF=0 if not found.
        #
        # +hl+ points to the argument value when found.
        #
        # Modifies: +af+, +hl+ and +b+ unless +argnum+ == 1
        def find_def_fn_args(argnum=b, subroutine:true, not_found:nil, cf_on_direct:false)
            isolate use: :vars do |eoc|
                            ld    b, argnum unless argnum == b or argnum == 1
                            ld    hl, [vars.defadd]
                            ld    a, h
                            ora   l
                            scf if cf_on_direct
                            jr    exit_on_zf
                if argnum != 1
                loop0       ld    a, 5              # skip argument value
                            adda_to  h, l
                end
                seek_next   ld    a, [hl]
                            inc   hl
                            cp    0x0E              # variable argument marker
                if argnum == 1
                    if subroutine
                            ret   Z
                    else
                            jr    Z, eoc
                    end
                else
                            jr    Z, found_arg
                end
                            cp    ?).ord            # arguments terminator
                exit_on_zf  jr    NZ, seek_next
                if subroutine
                            inc   a                 # ZF=0, not found
                            ret
                elsif not_found
                            jp    not_found
                else
                            inc   a                 # ZF=0, not found
                            jr    eoc if argnum != 1
                end
                if argnum != 1
                found_arg   djnz  loop0
                            ret if subroutine
                end
            end
        end
    end
end
