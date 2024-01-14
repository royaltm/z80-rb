# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'

module ZXLib
    ##
    #  ==A module with Z80 macros for common ZX Spectrum system tasks.
    #
    #  Contains:
    #
    #  * labels for some of ZX Spectrum 16/48K ROM routine addresses in the +rom+ namespace.
    #  * labels for ZX Spectrum 16/48K memory layout in the +mem+ namespace.
    #  * labels for ZX Spectrum 128K memory layout in the +mem128+ namespace.
    #  * labels for TC2048/2068 (SCLD) screen memory layout in the +memT2k+ namespace.
    #  * labels for ZX Spectrum 16/48K basic and system variables in the +vars+ namespace.
    #  * labels for ZX Spectrum 128K system variables in the +vars128+ namespace.
    #  * labels for ZX Interface I variables in the +if1vars+ namespace.
    #  * labels for I/O ports of ULA, ZX Printer in the +io+ namespace.
    #  * labels for I/O ports of 128k MMU and AY-3-891x in the +io128+ namespace.
    #  * labels for I/O ports of TC2068, TS2068, TC2048 in the +ioT2k+ namespace.
    #  * labels for I/O ports of ULAplus in the +io_plus+ namespace.
    #  * labels for I/O ports of AY-3-891x in the +io_ay+ namespace.
    #  * labels for I/O ports of Kempston mouse and joystick in the +kempston_io+ namespace.
    #  * labels for I/O ports of the Fuller Box in the +fuller_io+ namespace.
    #  * Macros for various tasks tied to the ZX Spectrum hardware or ZX Basic.
    #
    #  Example:
    #
    #    require 'zxlib/sys'
    #
    #    class Program
    #      include Z80
    #
    #      macro_import MathInt
    #      import ZXLib::Sys, macros: true
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
    class Sys
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
            kstate  byte, 8 # 23552 Used in reading the keyboard.
            last_k  byte    # 23560 Stores newly pressed key.
            repdel  byte    # 23561 Time (in 50ths of a second in 60ths of a second in N. America) that a key must be held down before it repeats. This starts off at 35, but you can POKE in other values.
            repper  byte    # 23562 Delay (in 50ths of a second in 60ths of a second in N. America) between successive repeats of a key held down: initially 5.
            defadd  word    # 23563 Address of arguments of user defined function if one is being evaluated; otherwise 0.
            k_data  byte    # 23565 Stores 2nd byte of colour controls entered from keyboard .
            tvdata  word    # 23566 Stores bytes of coiour, AT and TAB controls going to television.
            strms   Strms   # 23568 Addresses of channels attached to streams.
            chars   word    # 23606 256 less than address of character set (which starts with space and carries on to the copyright symbol). Normally in ROM, but you can set up your own in RAM and make CHARS point to it.
            rasp    byte    # 23608 Length of warning buzz.
            pip     byte    # 23609 Length of keyboard click.
            err_nr  byte    # 23610 1 less than the report code. Starts off at 255 (for 1) so PEEK 23610 gives 255.
            flags   byte    # 23611 Various flags to control the BASIC system.
            tv_flag byte    # 23612 Flags associated with the television.
            err_sp  word    # 23613 Address of item on machine stack to be used as error return.
            list_sp word    # 23615 Address of return address from automatic listing.
            mode    byte    # 23617 Specifies K, L, C. E or G cursor. 0='KLC', 1='E', 2='G'.
            newppc  word    # 23618 Line to be jumped to.
            nsppc   byte    # 23620 Statement number in line to be jumped to. Poking first NEWPPC and then NSPPC forces a jump to a specified statement in a line.
            ppc     word    # 23621 Line number of statement currently being executed.
            subppc  byte    # 23623 Number within line of statement being executed.
            bordcr  byte    # 23624 Border colour * 8; also contains the attributes normally used for the lower half of the screen.
            e_ppc   word    # 23625 Number of current line (with program cursor).
            vars    word    # 23627 Address of variables.
            dest    word    # 23629 Address of variable in assignment.
            chans   word    # 23631 Address of channel data.
            curchl  word    # 23633 Address of information currently being used for input and output.
            prog    word    # 23635 Address of BASIC program.
            nxtlin  word    # 23637 Address of next line in program.
            datadd  word    # 23639 Address of terminator of last DATA item.
            e_line  word    # 23641 Address of command being typed in.
            k_cur   word    # 23643 Address of cursor.
            ch_add  word    # 23645 Address of the next character to be interpreted: the character after the argument of PEEK, or the NEWLINE at the end of a POKE statement.
            x_ptr   word    # 23647 Address of the character after the ? marker.
            worksp  word    # 23649 Address of temporary work space.
            stkbot  word    # 23651 Address of bottom of calculator stack.
            stkend  word    # 23653 Address of start of spare space.
            breg    byte    # 23655 Calculator's b register.
            mem     word    # 23656 Address of area used for calculator's memory. (Usually MEMBOT, but not always.)
            flags2  byte    # 23658 More flags.
            df_sz   byte    # 23659 The number of lines (including one blank line) in the lower part of the screen.
            s_top   word    # 23660 The number of the top program line in automatic listings.
            oldppc  word    # 23662 Line number to which CONTINUE jumps.
            ospcc   byte    # 23664 Number within line of statement to which CONTINUE jumps.
            flagx   byte    # 23665 Various flags.
            strlen  word    # 23666 Length of string type destination in assignment.
            t_addr  word    # 23668 Address of next item in syntax table (very unlikely to be useful).
            seed    word    # 23670 The seed for RND. This is the variable that is set by RANDOMIZE.
            frames  byte, 3 # 23672 3 byte (least significant first), frame counter. Incremented every 20ms. See Chapter 18.
            udg     word    # 23675 Address of 1st user defined graphic You can change this for instance to save space by having fewer user defined graphics.
            coords  Coords  # 23677 Coordinate of last point plotted.
            p_posn  byte    # 23679 33 column number of printer position
            pr_cc   word    # 23680 Full address of next position for LPRINT to print at (in ZX printer buffer). Legal values 5B00 - 5B1F. [Not used in 128K mode or when certain peripherals are attached]
            echo_e  word    # 23682 33 column number and 24 line number (in lower half) of end of input buffer.
            df_cc   word    # 23684 Address in display file of PRINT position.
            dfccl   word    # 23686 Like DF CC for lower part of screen.
            s_posn  Cursor  # 23688 33 column number for PRINT position, 24 line number for PRINT position.
            sposnl  Cursor  # 23690 Like S POSN for lower part
            scr_ct  byte    # 23692 Counts scrolls: it is always 1 more than the number of scrolls that will be done before stopping with scroll? If you keep poking this with a number bigger than 1 (say 255), the screen will scroll on and on without asking you.
            attr_p  byte    # 23693 Permanent current colours, etc (as set up by colour statements).
            mask_p  byte    # 23694 Used for transparent colours, etc. Any bit that is 1 shows that the corresponding attribute bit is taken not from ATTR P, but from what is already on the screen.
            attr_t  byte    # 23695 Temporary current colours, etc (as set up by colour items).
            mask_t  byte    # 23696 Like MASK P, but temporary.
            p_flag  byte    # 23697 More flags. Bits: 7:P_PAPER_9 6:T_PAPER_9 5:P_INK_9 4:T_INK_9 3:P_INVERSE_1 2:T_INVERSE_1 1:P_OVER_1 0:T_OVER_1
            membot  byte,30 # 23698 Calculator's memory area; used to store numbers that cannot conveniently be put on the calculator stack.
            nmiadd  word    # 23728 This is the address of a user supplied NMI address which is read by the standard ROM when a peripheral activates the NMI. Probably intentionally disabled so that the effect is to perform a reset if both locations hold zero, but do nothing if the locations hold a non-zero value. Interface 1's with serial number greater than 87315 will initialize these locations to 0 and 80 to allow the RS232 "T" channel to use a variable line width. 23728 is the current print position and 23729 the width - default 80.
            ramtop  word    # 23730 Address of last byte of BASIC system area.
            p_ramt  word    # 23732 Address of last byte of physical RAM.
        end

        ##
        # ZX Interface 1 variables.
        class If1Vars < Label
            flags3  byte      # 23734 $5CB6            IY+$7C - Flags
            vector  word      # 23735 $5CB7            Address used to extend BASIC.
            sbrt    byte, 10  # 23737 $5CB9            10 bytes of Z80 code to Page ROM.
            baud    word      # 23747 $5CC3            BAUD=(3500000/(26*baud rate)) -2
            ntstat  byte      # 23749 $5CC5            Own network station number.
            iobord  byte      # 23750 $5CC6            Border colour during I/O
            ser_fl  word      # 23751 $5CC7            2 byte workspace used by RS232
            sector  word      # 23753 $5CC9            2 byte workspace used by Microdrive.
            chadd_  word      # 23755 $5CCB            Temporary store for CH_ADD
            ntresp  byte      # 23757 $5CCC            Store for network response code.
                              # ----- -----            ---------------------------------
            ntdest  byte      # 23758 $5CCD            Destination station number 0 - 64.
            ntsrce  byte      # 23759 $5CCE            Source station number.
            ntnumb  word      # 23760 $5CD0            Network block number 0 - 65535
            nttype  byte      # 23762 $5CD2            Header type block.
            ntlen   byte      # 23763 $5CD3            Data block length 0 - 255.
            ntdcs   byte      # 23764 $5CD4            Data block checksum.
            nthds   byte      # 23765 $5CD5            Header block checksum.
                              # ----- -----            ---------------------------------
            d_str1  word      # 23766 $5CD6            2 byte drive number 1 - 8.
            s_str1  byte      # 23768 $5CD8            Stream number 1 - 15. [ also 0 ]
            l_str1  byte      # 23769 $5CD9            Device type "M", "N", "T" or "B"
            n_str1  word, 2   # 23770 $5CDA            Length of filename.
                              # 23772 $5CDC (dynamic)  Address of filename.
                              # ----- -----            ---------------------------------
            d_str2  word      # 23774 $5CDE            2 byte drive   ; File type.
                              # 23775 $5CDF            number.        ; Length of  
            s_str2  byte      # 23776 $5CE0            Stream number. ; Data.
            l_str2  byte      # 23777 $5CE1            Device type.   ; Start of        
            n_str2  byte      # 23778 $5CE2            Length of      ; data.   \
                    byte      # 23779 $5CE3            filename.      ; Program  \ 
                    byte      # 23780 $5CE4 (dynamic)  Address of     ; length. ; Start of
                    byte      # 23781 $5CE5 (dynamic)  filename       ;         ; data.
                              # ----- -----            ---------------------------------
            hd_00   byte      # 23782 $5CE6            File type .      _  
            hd_0b   word      # 23783 $5CE7            Length of data.   /\
            hd_0d   word      # 23785 $5CE9            Start of data.   /
            hd_0f   word      # 23787 $5CEB            Program length. /
            hd_11   word      # 23789 $5CED            Line number.
                              # ----- -----            ---------------------------------
            copies  byte      # 23791 $5CEF            Number of copies made by SAVE.
            #
            # Acknowledgements (IF1 disassembly)
            # ----------------
            # Dr Ian Logan          for main ROM labels ( and half on Interface 1)
            # Dr Frank O'Hara       for main ROM labels.
            # Gianluca Carri        for Interface 1 v1.2 labels
        end

        ##
        # ZX Spectrum 128 variables.
        class Vars128 < Label
            swap    byte, 20  # 23296 $5B00 Swap paging subroutine.
            younger byte,  9  # 23316 $5B14 Return paging subroutine.
            onerr   byte, 18  # 23325 $5B1D Error handler paging subroutine.
            pin     byte,  5  # 23343 $5B2F RS232 input pre-routine.
            pout    byte, 22  # 23348 $5B34 RS232 token output pre-routine. This can be patched to bypass the control code filter.
            pout2   byte, 14  # 23370 $5B4A RS232 character output pre-routine.
            target  word      # 23384 $5B58 Address of subroutine to call in ROM 1.
            retaddr word      # 23386 $5B5A Return address in ROM 0.
            bank_m  byte      # 23388 $5B5C Copy of last byte output to I/O port $7FFD.
            ramrst  byte      # 23389 $5B5D Stores instruction RST $08 and used to produce a standard ROM error.
            ramerr  byte      # 23390 $5B5E Error number for use by RST $08 held in RAMRST.
            baud    word      # 23391 $5B5F Baud rate timing constant for RS232 socket. Default value of 11. [Name clash with ZX Interface 1 system variable at $5CC3]
            serfl   byte, 2   # 23393 $5B61 Second character received flag:
                              #               Bit 0   : 1=Character in buffer.
                              #               Bits 1-7: Not used (always hold 0).
                              # 23394 $5B62 Received Character.
            col     byte      # 23395 $5B63 Current column from 1 to WIDTH.
            width   byte      # 23396 $5B64 Paper column width. Default value of 80. [Name clash with ZX Interface 1 Edition 2 system variable at $5CB1]
            tvpars  byte      # 23397 $5B65 Number of inline parameters expected by RS232 (e.g. 2 for AT).
            flags3  byte      # 23398 $5B66 Flags: [Name clashes with the ZX Interface 1 system variable at $5CB6]
                              #               Bit 0: 1=BASIC/Calculator mode, 0=Editor/Menu mode.
                              #               Bit 1: 1=Auto-run loaded BASIC program. [Set but never tested by the ROM]
                              #               Bit 2: 1=Editing RAM disk catalogue.
                              #               Bit 3: 1=Using RAM disk commands, 0=Using cassette commands.
                              #               Bit 4: 1=Indicate LOAD.
                              #               Bit 5: 1=Indicate SAVE.
                              #               Bit 6: 1=Indicate MERGE.
                              #               Bit 7: 1=Indicate VERIFY.
            n_str1  byte, 10  # 23399 $5B67 Used by RAM disk to store a filename. [Name clash with ZX Interface 1 system variable at $5CDA]
                              #             Used by the renumber routine to store the address of the BASIC line being examined.
            hd_00   byte      # 23409 $5B71 Used by RAM disk to store file header information (see RAM disk Catalogue section below for details). [Name clash with ZX Interface 1 system variable at $5CE6]
                              #             Used as column pixel counter in COPY routine.
                              #             Used by FORMAT command to store specified baud rate.
                              #             Used by renumber routine to store the number of digits in a pre-renumbered line number reference. [Name clash with ZX Interface 1 system variable at $5CE7]
            hd_0b   word      # 23410 $5B72 Used by RAM disk to store header info - length of block.
                              #             Used as half row counter in COPY routine.
                              #             Used by renumber routine to generate ASCII representation of a new line number.
            hd_0d   word      # 23412 $5B74 Used by RAM disk to store file header information (see RAM disk Catalogue section below for details). [Name clash with ZX Interface 1 system variable at $5CE9]
            hd_0f   word      # 23414 $5B76 Used by RAM disk to store file header information (see RAM disk Catalogue section below for details). [Name clash with ZX Interface 1 system variable at $5CEB]
                              #             Used by renumber routine to store the address of a referenced BASIC line.
            hd_11   word      # 23416 $5B78 Used by RAM disk to store file header information (see RAM disk Catalogue section below for details). [Name clash with ZX Interface 1 system variable at $5CED]
                              #             Used by renumber routine to store existing VARS address/current address within a line.
            sc_00   byte      # 23418 $5B7A Used by RAM disk to store alternate file header information (see RAM disk Catalogue section below for details).
            sc_0b   word      # 23419 $5B7B Used by RAM disk to store alternate file header information (see RAM disk Catalogue section below for details).
            sc_0d   word      # 23421 $5B7D Used by RAM disk to store alternate file header information (see RAM disk Catalogue section below for details).
            sc_0f   word      # 23423 $5B7F Used by RAM disk to store alternate file header information (see RAM disk Catalogue section below for details).
            oldsp   word      # 23425 $5B81 Stores old stack pointer when TSTACK in use.
            sfnext  word      # 23427 $5B83 End of RAM disk catalogue marker. Pointer to first empty catalogue entry.
            sfspace byte, 3   # 23429 $5B85 Number of bytes free in RAM disk (3 bytes, 17 bit, LSB first).
            row01   byte      # 23432 $5B88 Stores keypad data for row 3, and flags:
                              #               Bit 0   : 1=Key '+' pressed.
                              #               Bit 1   : 1=Key '6' pressed.
                              #               Bit 2   : 1=Key '5' pressed.
                              #               Bit 3   : 1=Key '4' pressed.
                              #               Bits 4-5: Always 0.
                              #               Bit 6   : 1=Indicates successful communications to the keypad.
                              #               Bit 7   : 1=If communications to the keypad established.
            row23   byte      # 23433 $5B89 Stores keypad key press data for rows 1 and 2:
                              #               Bit 0: 1=Key ')' pressed.
                              #               Bit 1: 1=Key '(' pressed.
                              #               Bit 2: 1=Key '*' pressed.
                              #               Bit 3: 1=Key '/' pressed.
                              #               Bit 4: 1=Key '-' pressed.
                              #               Bit 5: 1=Key '9' pressed.
                              #               Bit 6: 1=Key '8' pressed.
                              #               Bit 7: 1=Key '7' pressed.
            row45   byte      # 23434 $5B8A Stores keypad key press data for rows 4 and 5:
                              #               Bit 0: Always 0.
                              #               Bit 1: 1=Key '.' pressed.
                              #               Bit 2: Always 0.
                              #               Bit 3: 1=Key '0' pressed.
                              #               Bit 4: 1=Key 'ENTER' pressed.
                              #               Bit 5: 1=Key '3' pressed.
                              #               Bit 6: 1=Key '2' pressed.
                              #               Bit 7: 1=Key '1' pressed.
            synret  word      # 23435 $5B8B Return address for ONERR routine.
            lastv   byte, 5   # 23437 $5B8D Last value printed by calculator.
            rnline  word      # 23442 $5B92 Address of the length bytes in the line currently being renumbered.
            rnfirst word      # 23444 $5B94 Starting line number when renumbering. Default value of 10.
            rnstep  word      # 23446 $5B96 Step size when renumbering. Default value of 10.
            strip1  byte, 32  # 23448 $5B98 Used as RAM disk transfer buffer (32 bytes to $5BB7).
                              #             Used to hold Sinclair stripe character patterns (16 bytes to $5BA7).
            tstbeg  byte, 71  # 23480 $5BB8 ...
            tstack  byte      # 23551 $5BFF Temporary stack (grows downwards). The byte at $5BFF is not actually used.
            #
            # Acknowledgements (128k disassembly)
            # ----------------
            # Matthew Wilson  (www.matthew-wilson.net/spectrum/rom/)
            # Andrew Owen     (cheveron-AT-gmail.com)
            # Geoff Wearmouth (gwearmouth-AT-hotmail.com)
            # Rui Tunes
            # Paul Farrow     (www.fruitcake.plus.com)
        end

        export :auto

        vars128 addr 23296, Vars128
        vars    addr 23552, Vars
        vars_iy vars.err_nr

        if1vars addr 23734, If1Vars

        isolate :rom do
            start       addr 0x0000 # THE 'START'
            error       addr 0x0008 # THE 'ERROR' RESTART
            print_a     addr 0x0010 # THE 'PRINT CHARACTER' RESTART
            get_char    addr 0x0018 # THE 'COLLECT CHARACTER' RESTART
            next_char   addr 0x0020 # THE 'COLLECT NEXT CHARACTER' RESTART
            fp_calc     addr 0x0028 # THE 'CALCULATE' RESTART
            bc_spaces   addr 0x0030 # THE 'CREATE BC SPACES' RESTART
            mask_int    addr 0x0038 # THE 'MASKABLE INTERRUPT' ROUTINE
            key_int     addr 0x0048 # KEY-INT part of the interrupt routine
            keyboard    addr 0x02BF # read KEYBOARD routine called from the system interrupt routine
            ld_bytes    addr 0x0556 # Load bytes: A - 0x00 header, 0xFF data; IX - address, DE - length, CF - 1=LOAD, 0=VERIFY; output: CF=1 ok
            sa_bytes    addr 0x04C2 # Save bytes: A - 0x00 header, 0xFF data; IX - address, DE - length
            po_any      addr 0x0B24 # THE 'PRINT ANY CHARACTER' ROUTINE
            po_gr_1     addr 0x0B38 # The 16 2*2 mosaic characters 128-143 decimal are formed from bits 0-3 of the character
            error_5     addr 0x0C86 # Out of screen
            cl_all      addr 0x0DAF # Clearing whole display area
            cl_line     addr 0x0E44 # THE 'CLEAR TEXT LINES' ROUTINE. The B register holds on entry the number of lines to be cleared 1-24.
            error_j     addr 0x15C4 # Invalid I/O device
            wait_key    addr 0x15D4 # THE 'INPUT CONTROL' SUBROUTINE
            wait_key1   addr 0x15DE # Routine INPUT-AD is called repeatedly until a character is available. Reports 8 End of file error when there is no more.
            error_8     addr 0x15E4 # End of file
            input_ad    addr 0x15E6 # THE 'INPUT ADDRESS' ROUTINE. Register A: data, CF=1: data available, CF=0,ZF=0: end of file
            chan_open   addr 0x1601 # THE 'OPEN CHANNEL' ROUTINE. A: stream #
            error_o     addr 0x160E # Invalid stream
            call_jump   addr 0x162C # jump to the routine (in HL). JP (HL)
            make_room   addr 0x1655 # ROM MAKE-ROOM routine
            close       addr 0x16E5 # THE 'CLOSE #' COMMAND
            close_hl    addr 0x16EB # Close stream. Address of the stream entry expected in HL.
            str_data    addr 0x171E # Stream data. Gets #stream from fp-stack. Returns HL: STRMS entry, BC: offset to channel data + 1 or 0 if closed.
            str_data_a  addr 0x1721 # Stream data. Stream # should be in A. Checks if A is in range 0..15. Reports O Invalid stream if not so.
            error_o_2   addr 0x1725 # Invalid stream
            str_data1   addr 0x1727 # Stream data. Stream # should be in A. No checks.
            line_addr   addr 0x196E # Get starting address of line, or line after
            cp_lines    addr 0x1980 # Compare line numbers
            next_one    addr 0x19B8 # Get next one
            differ      addr 0x19DD # Difference routine: BC = HL - DE, DE <-> HL
            reclaim_1   addr 0x19E5 # Handle reclaiming space: DE - start address of the space to reclaim, HL - address after the space
            reclaim_2   addr 0x19E8 # HL - start address of the space to reclaim, BC - size of the space to reclaim
            out_num_1   addr 0x1A1B # Prints a small integer in the range 0-9999. BC - an integer in the range 0-9999.
            out_num_2   addr 0x1A28 # Prints a space padded, small integer in the range 0-9999. HL - an address of the integer to print.
            error_0     addr 0x1BB0 # OK
            go_to       addr 0x1E67 # Handle GO TO command
            go_to_1     addr 0x1E6C # Handle GO TO command: expecting line in HL with line number check
            go_to_2     addr 0x1E73 # Handle GO TO command: expecting line in HL and statement number in D
            free_mem    addr 0x1F1A # free memory in result BC
            break_key   addr 0x1F54 # returns CF=0 if shift+space (break) is being pressed
            pr_string   addr 0x203C # PR-STRING: start in DE, length in BC
            plot        addr 0x22DC # Handle PLOT command
            plot_sub    addr 0x22E5 # The Plot subroutine: B: y, C: x
            line_draw   addr 0x2477 # Handle DRAW x,y command
            draw_line   addr 0x24B7 # THE 'LINE DRAWING' ROUTINE
            draw_line_1 addr 0x24BA # THE 'LINE DRAWING' ROUTINE: C: abs(dx), B: abs(dy), E: dx < 0 ? -1 : 1, D: dy < 0 ? -1 : 1
            end_calc    addr 0x2758 # H'L' register should point to this address when the USR routine terminates
            error_q     addr 0x288B # Parameter error
            stk_store   addr 0x2AB6 # Pushes five registers on the calculator stack: A, E, D, C, B if there is enough room
            stk_fetch   addr 0x2BF1 # Pops five registers from the calculator stack: A, E, D, C, B
            stack_a     addr 0x2D28 # Pushes accumulator on the calculator stack as a small integer
            stack_bc    addr 0x2D2B # Pushes BC register pair on the calculator stack as a small integer
            print_fp    addr 0x2DE3 # Print a floating point number
            int_fetch   addr 0x2D7F # HL: point to FP-value, restores the twos complement number to normal in DE and a sign in C.
            error_a     addr 0x34E7 # Invalid argument
            font_p      addr 0x3D00 # THE 'ZX SPECTRUM CHARACTER SET'
        end

        isolate :sys do
            pr_buf  addr 0x5B00
            vars    addr 0x5C00
            mmaps   addr 0x5CB6
        end

        isolate :mem, use: [:rom, :sys] do
            font_p  addr rom.font_p
            romtop  addr 0x3FFF
            screen  addr 0x4000
            scrlen  32*192
            attrs   addr 0x5800
            attrlen 32*24
            rambot  addr 0x5B00
            pr_buf  addr sys.pr_buf
            vars    addr sys.vars
            mmaps   addr sys.mmaps
        end

        # I/O and peripherals
        # https://www.worldofspectrum.org/faq/reference/48kreference.htm
        # https://www.worldofspectrum.org/faq/reference/peripherals.htm
        isolate :io do
            ula     addr 0x00FE #  in: 0b-E-Keybr out: 0b---EMBor
            printer addr 0x00FB #  in: 0bLC-----R Line starts: 1, Connected: 0, Ready for the next bit: 1
                                # out: 0bP----MS- Print: 1, Motor start|stop: 0|1, Slow motor: 1
        end

        isolate :fuller_io do
            ay_sel      addr 0x003F # out: select a register
            ay_inp      addr 0x003F # in: read the value of the selected register
            ay_out      addr 0x005F # out: write data to the selected register
            joystick    addr 0x007F # in: 0bF---RLDU joystick active bits=0
        end

        isolate :kempston_io do
            mouse_h     addr 0xFBDF # 
            mouse_v     addr 0xFFDF # 
            mouse_btn   addr 0xFADF # in: 0b------RL buttons active bits=0
            joystick    addr 0x001F # in: 0b---FUDLR joystick active bits=1
        end

        # ZX Spectrum 128k
        # https://www.worldofspectrum.org/faq/reference/128kreference.htm
        isolate :io128 do
            ay_sel  addr 0xFFFD # out: select a register 0-14
            ay_inp  addr 0xFFFD # in: read the value of the selected register
            ay_out  addr 0xBFFD # out: write data to the selected register
            mmu     addr 0x7FFD # 0b--DRSBnk D - disable mmu, R - rom, S - screen, Bnk - bank
        end

        # Timex TC2068, TS2068, TC2048 (only ula).
        # https://www.worldofspectrum.org/faq/reference/tmxreference.htm
        isolate :ioT2k do
            mmu     addr 0xF4 # determines which banks are to be paged in with each bit referring to the relevant bank (0-7 or 0'-7')
            ay_sel  addr 0xF5 # out: select a register 0-14, in: read the value of the selected register
            ay_inp  addr 0xF6 # in: read the value of the selected register
            ay_out  addr 0xF6 # out: write data to the selected register
            ula     addr 0xFF # 0bMIColScr M - MMU 0=DOCK, 1=EX-ROM; I - Interrupts 1=DI, 0=EI;
                              # Col - colors for hi-res
                              # Scr - screen modes: 000=screen 0, 001=screen 1, 010=hi-colour, 110=hi-res
        end

        # ULAplus
        # https://sinclair.wiki.zxnet.co.uk/wiki/ULAplus
        isolate :io_plus do
            reg  addr 0xBF3B # 0 - 63 palette entry select, 64 - group mode and bits 0-5 select screen mode:
                             # bits 0-2: 000=screen 0, 001=screen 1, 010=hi-colour, 110=hi-res (bank 5)
                             #           100=screen 0, 101=screen 1, 011=hi-colour, 111=hi-res (bank 7)
                             # bits 3-5 set colours in hi-res mode:
                             #   000 - Black on White     100 - Green on Magenta
                             #   001 - Blue on Yellow     101 - Cyan on Red
                             #   010 - Red on Cyan        110 - Yellow on Blue
                             #   011 - Magenta on Green   111 - White on Black
            dta addr 0xFF3B  # group mode: 0 - no palette, 1 - RGB palette, 2 - grayscale
            mode_group addr 0x40
            mode_mask  addr 0x3F
        end

        # Default AY-3-891x I/O ports
        isolate :io_ay, use: :io128 do
            ay_sel  addr io128.ay_sel
            ay_inp  addr io128.ay_inp
            ay_out  addr io128.ay_out
        end

        isolate :sys128, use: [:io128, :vars128] do
            mmu_value    addr vars128.bank_m # 0b00DRSBnk (0x5B5C)
            mmu_port     addr io128.mmu      # D - disable mmu, R - rom, S - screen, Bnk - bank
        end

        isolate :mem128, use: :sys128 do
            mmu_value    addr sys128.mmu_value
            swap_p       addr 0xC000
            screen_alt   addr 0xC000
            attrs_alt    addr 0xD800
        end

        isolate :memT2k do
            screen0      addr 0x4000
            attrs0       addr 0x5800
            screen1      addr 0x6000
            attrs1       addr 0x7800
            rambot       addr 0x7B00
        end

        ##
        # ==ZXLib::Sys Macros
        #
        # Some of the macros require:
        #
        #    require 'zxlib/math'
        #    # ...
        #      macro_import MathInt
        #      macro_import ::ZXLib::Math
        #
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
                    err     rst  0x08
                            db   errno - 1
                end
            end
            ##
            # Creates a routine that sets up custom interrupt handler using ZX Spectrum ROM's unused space
            # as a IM2 mode jump table.
            #
            # * handler:: One of the 16bit register pair or an address or a pointer to the address
            #             of the handler routine.
            # * enable_intr:: If +true+ invoke +ei+ instruction at the end.
            #
            # This routine uses a mode 2 interrupt. In this mode the address
            # of the interrupt routine is formed in the following way:
            # an 8-bit value found on the BUS (which in most ZX Spectrum models is 255)
            # is being added to register +i+ x 256, which form a vector table address.
            # A word (two bytes) is being read from that address, providing the address
            # where the call to the interrupt routine is made to.
            #
            # This routine makes these assumptions about the machine:
            # * The byte at the memory address 0 contains 243.
            # * The 2 consecutive bytes at the memory address +vector_page+ x 256 + 8-bit BUS value
            #   and the following one, both contain 255.
            # * The RAM memory between 65524 and 65535 will not be used for other purposes.
            #
            # _NOTE_:: The assumptions are true for most ZX Spectrum models and clones including Pentagon
            #          machines and Timex TC2048. However they are not true for Timex TC2068 or TS2068.
            #
            # Options:
            # * +vector_page+:: A most significant byte of the interrupt vector table address
            #                   loaded into +i+ register. The default is 0x3B so the address
            #                   of the routine is found at 0x3BFF in most ZX Spectrum models.
            #
            # Modifies: +a+, +i+, +hl+ if +handler+ is not a register pair.
            def setup_custom_interrupt_handler(handler, enable_intr:true, vector_page:0x3B)
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
                                ld  a, vector_page   # Supported by ZX Spectrum 128, +2, +2A, +3 and probably most clones.
                                ld  i, a             # load the accumulator with FF filled page in rom.
                                im2
                                ei if enable_intr
                end
            end
            ##
            # Restore interrupt handler ZX Spectrum ROM's standard IM1 mode.
            #
            # * enable_intr:: If +true+ invoke +ei+ instruction at the end.
            #
            # T-states: 28/24
            #
            # Modifies: +a+, +i+.
            def restore_rom_interrupt_handler(enable_intr:true)
                isolate do
                        im1
                        ld  a, 0x3F
                        ld  i, a
                        ei if enable_intr
                end
            end
            ##
            # Test for a key or keys being pressed.
            #
            # * +line_mask+:: Keyboard half-line mask, may be an 8 bit register.
            #                 The default is 0 which means all available lines.
            # * +key_mask+:: Key mask (b0..b4), may be an 8 bit register.
            #                The default is 0b11111 which means all keys in a half-line.
            #
            # Options:
            # * +io+:: A label containing +ula+ sub-label addressing ULA I/O bus.
            #
            #     line  key bits                     line  key bits
            #           b4,  b3,  b2,  b1,      b0         b4,  b3,  b2,   b1,      b0
            #     0xf7 [5], [4], [3], [2],     [1]   0xef [6], [7], [8],  [9],     [0]
            #     0xfb [T], [R], [E], [W],     [Q]   0xdf [Y], [U], [I],  [O],     [P]
            #     0xfd [G], [F], [D], [S],     [A]   0xbf [H], [J], [K],  [L], [ENTER]
            #     0xfe [V], [C], [X], [Z], [SHIFT]   0x7f [B], [N], [M], [SS], [SPACE]
            #
            # Modifies: +af+.
            #
            # Output:
            # * +ZF+=0:: (NZ) if any of the specified keys is being pressed.
            # * +a+:: bits b0..b4=1 if a key is being pressed at any of the specified half-line.
            def key_pressed?(line_mask=0, key_mask=0x1f, io:self.io)
                isolate do
                  if line_mask == 0
                        xor  a
                  else
                        ld   a, line_mask unless line_mask == a
                  end
                        inp  a, (io.ula)
                        cpl
                        anda key_mask
                end
            end
            ##
            # Test for cursor keys being pressed.
            #
            # Options:
            # * +t+:: A temporary 8-bit register.
            # * +io+:: A label containing +ula+ sub-label addressing ULA I/O bus.
            #
            # Modifies: +af+, +t+.
            #
            # Output:
            # * +ZF+=0:: if any of the cursor keys is being pressed.
            # * +a+:: bits b0..b3=1 if a cursor key is being pressed.
            #
            #      b3  b2  b1  b0
            #     [←] [↓] [↑] [→]
            def cursor_key_pressed?(t:b, io:self.io)
                isolate do
                        key_pressed? 0xf7, 0x10, io:io # key [5]
                        ld   t, a
                        key_pressed? 0xef, 0x1c, io:io # keys [6] [7] [8] . .
                        rrca
                        ora  t                         # keys [5] [6] [7] [8] .
                        rrca                           # keys [←] [↓] [↑] [→]
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
            # T-States: 46, +(0|4|7|13) depending on code, +(0|10|16) depending on chars.
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
            # Creates a ZX Spectrum CHAN entry and opens it as a stream #N.
            #
            # * output:: a routine address or a 16bit register holding that address, except +hl+.
            # * input:: a routine address or a 16bit register holding that address, except +hl+.
            # * strm_no:: a stream number (4 - 15) or an 8bit register name; +nil+ if none of the streams should be attached.
            # * chan_name:: a channel name (immediate value). if +nil+ or +0+ no channel name is being written.
            #
            # Optionally give the returned namespace label a +name+.
            #
            # Modifies: +af+, +hl+, +bc+, +de+.
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
                            # ld   hl, vars.strms.user # hl points to STRMS #0
                            # adda_to h, l
                            add  vars.strms.user
                            ld   l, a
                            ld   h, vars.strms>>8
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
                                             # save address of 2nd byte of new channel data
                            push hl unless strm_no.nil?
                            inc  hl
                            ld   [hl], e
                            inc  hl
                            ld   [hl], d
                            inc  hl
                    unless chan_name.zero?   # channel name
                            ld   [hl], chan_name
                    end
                    unless strm_no.nil?      # open #stream_no to channel
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
                            ld   hl, [vars.prog]
                            dec  hl
                            exx                # hl' stop search address
                            ld   bc, 5         # record length
                            ld   hl, [vars.chan]    # CHAN
                            find_record h, l
                            jr   NZ, cleanup
                            sbc  hl, bc        # beginning of the record found
                    cleanup exx
                            pop  hl
                            exx
                end
            end
            ##
            # Search for a record that matches a large block of memory.
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
            # Reads a signed integer from a ZX Basic's FP-value.
            #
            # +hl+:: must point to the 1st byte of the FP-value.
            # +th+:: most significant byte output register.
            # +tl+:: least significant byte output register.
            # +sgn+:: sign byte register.
            # +normal_negative+:: if the twos complement number should be normalized.
            # +t+:: a temporary register, used only with +normal_negative+ = +true+.
            #
            # If +t+ is the accumulator register the +a'+ register is being used.
            #
            # The result is being loaded into +th+|+tl+ and +sgn+.
            # ZF=0 (NZ) signals the FP-value is not an integer.
            # +hl+ will always point to the last byte of the FP-value.
            #
            # T-States: 59, normal_negative=true: (t=a') 91, (t=r) 87.
            #
            # Modifies: +af+, +hl+, +th+, +tl+ and +sgn+, optionally: +t+ or +af'+.
            def read_integer_value(th=d, tl=e, sgn=c, normal_negative:false, t:a)
                raise ArgumentError if [h,l,ixh,ixl,iyh,iyl,a].include?(tl) or !register?(tl) or
                                       [h,l,ixh,ixl,iyh,iyl,a].include?(th) or !register?(th) or
                                       [h,l,ixh,ixl,iyh,iyl,a].include?(sgn) or !register?(sgn) or
                                       (register?(t) and [h,l,th,tl,sgn,ixh,ixl,iyh,iyl].include?(t)) or !register?(t)
                isolate do |eoc|
                    if normal_negative
                        if t == a
                                ld    a, [hl]
                                ex    af, af
                        else
                                ld    t, [hl]
                        end
                                inc   hl
                                ld    sgn, [hl]
                                inc   hl
                                ld    a, [hl]
                                inc   hl
                                xor   sgn
                                sub   sgn
                                ld    tl, a
                                ld    a, [hl]
                                inc   hl
                                adc   a, sgn
                                xor   sgn
                                ld    th, a
                        if t == a
                                ex    af, af
                        else
                                ld    a, t
                        end
                                ora   [hl]
                    else
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
            end
            ##
            # Reads a positive integer from a ZX Basic's FP-value.
            #
            # +hl+:: must point to the 1st byte of the FP-value.
            # +th+:: most significant byte output register.
            # +tl+:: least significant byte output register.
            #
            # The result is being loaded into +th+|+tl+.
            # ZF=0 (NZ) signals the FP-value is not a positive integer.
            # +hl+ will always point to the last byte of the FP-value.
            #
            # T-States: 59.
            #
            # Modifies: +af+, +hl+, +th+ and +tl+.
            def read_positive_int_value(th=d, tl=e)
                raise ArgumentError if [h,l,ixh,ixl,iyh,iyl,a].include?(tl) or [h,l,ixh,ixl,iyh,iyl,a].include?(th)
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
            # Reads a 32-bit integer from a ZX Basic's FP-value.
            #
            # Requires: <tt>macro_import ::ZXLib::Math</tt>.
            #
            # +hl+:: must point to the 1st byte of the FP-value.
            # +t3+:: most significant 8-bit output register.
            # +t2+:: 8-bit output register.
            # +t1+:: 8-bit output register.
            # +t0+:: least significant 8-bit output register.
            #
            # The result is being loaded into +t3+|+t2+|+t1+|+t0+.
            # CF=1 signals that the FP-value is too big to fit into a 32-bit integer.
            # ZF=1 signals that the FP-value is positive. In this instance accumulator +a+ = 0.
            # ZF=0 signals that the FP-value is negative. In this instance accumulator +a+ = 0xFF.
            # If the value is negative the integer WON'T BE a twos complement number.
            #
            # +hl+ will always point to the last byte of the FP-value.
            #
            # Modifies: +af+, +af'+, +hl+, +bc+, +de+.
            def read_integer32_value(t3=d, t2=e, t1=b, t0=c)
                # legacy arguments handling (th, tl)
                if [t3, t2].all? {|t| [bc, de].include?(t)}
                    t1, t0 = t2.split
                    t3, t2 = t3.split
                end
                raise ArgumentError unless [t3,t2,t1,t0].uniq.size == 4 and
                                           [t3,t2,t1,t0].all? {|t| [b,c,d,e].include?(t)}
                isolate do
                                ld    a, [hl]
                                inc   hl
                                ld    t3, [hl]
                                inc   hl
                                ld    t2, [hl]
                                inc   hl
                                ld    t1, [hl]
                                inc   hl
                                ld    t0, [hl]
                                fp_to_integer32 t3, t2, t1, t0, exp:a
                end
            end
            ##
            # Reads a string address and its length from a ZX Basic's stringish FP-value.
            #
            # +hl+:: must point to the 1st byte of the FP-value.
            # +adh+:: most significant byte address output register.
            # +adl+:: least significant byte address output register.
            # +lenh+:: most significant byte length output register.
            # +lenl+:: least significant byte length output register.
            #
            # +hl+ will point to the last byte of the FP-value.
            #
            # T-States: 52.
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
            # Gets a DEF FN argument value address.
            #
            # Requires: <tt>macro_import MathInt</tt>.
            #
            # * +argnum+:: 1-based argument index (0 is 256), may be a register or a number.
            # * +subroutine+:: if +true+ will use +ret+ instruction.
            # * +not_found+:: if +subroutine+ is +false+ and +not_found+ is defined, the routine
            #                 will jump to this address when argument was not found,
            #                 otherwise success is signalled with ZF=1.
            # * +not_found_blk+:: if +subroutine+ is +false+ and +not_found+ is +nil+, the routine
            #                     produced by the block will be run. Make sure the routine doesn't
            #                     fall through though.
            # * +cf_on_direct+:: if +true+ and DEFADD is not defined CF will be set.
            #
            # When +subroutine+ is +true+ or +not_found+ is +nil+, the success is signalled with +ZF+:
            #
            # * ZF=1 if found,
            # * ZF=0 if not found.
            #
            # +hl+ points to the argument value when found.
            #
            # Modifies: +af+, +hl+ and optionally +b+ unless +argnum+ == 1.
            def find_def_fn_args(argnum=b, subroutine:true, not_found:nil, cf_on_direct:false, &not_found_blk)
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
                    elsif block_given?
                                ns(&not_found_blk)
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
            ##
            # Creates a routine that returns to the calling ZX-Basic's USR function an FP value.
            #
            # When returning from a user code an integer from the +bc+ register is being used as
            # a +USR+ function return value via STACK-BC routine.
            #
            # To return a floating point value use this routine instead of invoking +ret+.
            #
            # The FP value to be returned should be held in +a+, +e+, +d+, +c+, +b+ registers.
            #
            # Options:
            # * +pop_ret_address+:: Set to +false+ if the STACK-BC return address was already fetched
            #                       from the machine stack.
            # * +rom+:: A namespace label containing the ROM routine addresses as sub-labels.
            # * +restore_iy+:: A value to restore +iy+ register to. Set to +nil+ to not restore +iy+.
            # * +restore_hl_alt+:: A value to restore +hl'+ register to. Set to +nil+ to not restore +hl'+.
            def return_with_fp(pop_ret_address:true, rom:self.rom, restore_iy:self.vars_iy, restore_hl_alt:rom.end_calc)
                isolate do
                                pop   hl if pop_ret_address
                                ld    iy, restore_iy if restore_iy
                    if restore_hl_alt
                                exx
                                ld    hl, restore_hl_alt
                                exx
                    end
                                call  rom.stk_store
                                rst   rom.fp_calc
                                db    0x38 # end-calc  make HL = STKEND-5
                                ret
                end
            end
            ##
            # Selects an upper memory bank (0-7) and/or a screen memory page (0-1) to be displayed.
            # 
            # Options:
            # * +bank+:: Selects a memory bank available at 0xC000-0xFFFF as an integer or
            #            any of the 8-bit registers except +a+, or indirect memory address via a 16-bit register.
            #            In this instance you should pass +true+ to the +screen:+ option if bit-3 of 
            #            the +bank+ should select a screen to be displayed.
            # * +screen+:: 0 - Display screen from bank 5. 1 - Display screen from bank 7.
            #              +nil+ to preserve screen bit-3 from the +bank+ register.
            # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
            #                    only if you have already disabled the interrupts.
            # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
            #                   if you need to perform more uninterrupted actions.
            # * +mmu_port_in_bc+:: A boolean flag indicating if +bc+ registers already contain the +mmu_port+ I/O address.
            #                      If not the +bc+ will be loaded with +sys128.mmu_port+ value.
            # * +sys128+:: A label with +mmu_port+ sub-label addressing the 128k MMU I/O port (0x7FFD) and +mmu_value+
            #              addressing memory where the 128k ROM is storing the last value output to MMU I/O port (0x5B5C).
            #
            #     0xFFFF                                       screen: 0         screen: 1
            #     +--------+--------+--------+--------+--------+--------+--------+--------+
            #     | Bank 0 | Bank 1 | Bank 2 | Bank 3 | Bank 4 | Bank 5 | Bank 6 | Bank 7 |
            #     |        |        |(also at|        |        |(also at|        |        |
            #     |        |        | 0x8000)|        |        | 0x4000)|        | shadow |
            #     |        |        |        |        |        | screen |        | screen |
            #     +--------+--------+--------+--------+--------+--------+--------+--------+
            #     0xC000
            #
            # Memory banks 1,3,5 and 7 are contended, which reduces the speed of memory access in these banks.
            #
            # Modifies: +af+, +bc+, memory at +sys128.mmu_value+ (0x5B5C).
            def mmu128_select_bank(bank:nil, screen:nil, disable_intr:true, enable_intr:true, mmu_port_in_bc:false, sys128:self.sys128)
                mask = 0b11111111
                merg = 0b00000000
                unless bank.nil?
                    mask = mask & 0b11111000
                    if register?(bank)
                        merg = bank
                    else
                        merg = merg | (bank.to_i & 0b00000111)
                    end
                end
                unless screen.nil?
                    mask = mask & 0b11110111
                    unless register?(bank)
                        merg = merg | ((screen.to_i.zero? ? 0 : -1) & 0b00001000)
                    end
                end
                isolate do
                          ld   bc, sys128.mmu_port unless mmu_port_in_bc
                          di   if disable_intr
                          ld   a, [sys128.mmu_value] # previous value of port
                          anda mask
                          ora  merg unless merg == 0
                          ld   [sys128.mmu_value],a
                          ei   if enable_intr # directly after an EI, interrupts aren't accepted.
                          out  (c), a
                end
            end
            ##
            # Swap displayed screens.
            #
            # Options:
            # * +swap_bank+:: A boolean flag indicating that the routine should additionally swap screen memory banks at 0xC000.
            #                 For this to have a desired effect bank 5 or 7 should have been previously selected
            #                 e.g. with Macros.mmu128_select_bank.
            # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
            #                    only if you have already disabled the interrupts.
            # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
            #                   if you need to perform more uninterrupted actions.
            # * +mmu_port_in_bc+:: A boolean flag indicating if +bc+ registers already contain the +mmu_port+ I/O address.
            #                      If not the +bc+ will be loaded with +sys128.mmu_port+ value.
            # * +sys128+:: A label with +mmu_port+ sub-label addressing the 128k MMU I/O port (0x7FFD) and +mmu_value+
            #              addressing memory where the 128k ROM is storing the last value output to MMU I/O port (0x5B5C).
            #
            # Modifies: +af+, +bc+, memory at +sys128.mmu_value+ (0x5B5C).
            def mmu128_swap_screens(swap_bank:false, disable_intr:true, enable_intr:true, mmu_port_in_bc:false, sys128:self.sys128)
                swap_bits = if swap_bank then 0b00001010 else 0b00001000 end
                isolate do
                          ld   bc, sys128.mmu_port unless mmu_port_in_bc
                          di   if disable_intr
                          ld   a, [sys128.mmu_value] # previous value of port
                          xor  swap_bits
                          ld   [sys128.mmu_value],a
                          ei   if enable_intr # directly after an EI, interrupts aren't accepted.
                          out  (c), a
                end
            end

            ##
            # Moves Basic program and variables above the screen 1 (to 0x7B00).
            #
            # * +check_ensure+:: when +true+ checks if a call to MAKE-ROOM is needed.
            #
            # Modifies: +af+, +bc+, +de+, +hl+.
            def move_basic_above_scld_screen_memory(check_ensure:false)
                isolate use: [:memT2k, :vars, :rom] do |eoc|
                        ld   hl, memT2k.rambot
                        ld   de, [vars.prog]
                        dec  de
                        cp   a
                        sbc  hl, de
                        jr   C, eoc if check_ensure
                        jr   Z, eoc if check_ensure
                        ld16 bc, hl
                        ex   de, hl
                        call rom.make_room
                end
            end
        end
    end
end

# DEPRECATED
ZXSys = ZXLib::Sys unless defined?(ZXSys) # :nodoc:
