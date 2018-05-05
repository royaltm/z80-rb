##
#  ==A module with Z80 macros for common ZX Spectrum system tasks.
#
#  Example:
#
#    require 'zxlib/sys'
#
#    class Program
#      include Z80
#      import ZXSys, macros: true
#
#      start       create_chan_and_open output: output_p, chan_name: 'X'
#                  ret
#      output_p    # ... do something with a
#    end
class ZXSys
    include Z80

    class Coords < Label
        x byte
        y byte
    end

    class Cursor < Label
        column byte
        line   byte
    end

    class Strms < Label
        sys  word,  3
        user word, 16
    end

    export :auto
    isolate :rom do
        error_5       addr 0x0C86 # Out of screen
        cl_all        addr 0x0DAF # CL-ALL
        error_j       addr 0x15C4 # Invalid I/O device
        make_room     addr 0x1655 # ROM MAKE-ROOM routine
    end

    isolate :mem do
        font_p  addr 0x3D00
        romtop  addr 0X3FFF
        screen  addr 0x4000
        scrlen  32*192
        attrs   addr 0x5800
        attrlen 32*24
        pr_buf  addr 0x5B00
        vars    addr 0x5C00
        mmaps   addr 0x5CB6
    end

    isolate :vars do
        kstate  addr 23552, 8   # Used in reading the keyboard.
        last_k  addr 23560      # Stores newly pressed key.
        repdel  addr 23561      # Time (in 50ths of a second in 60ths of a second in N. America) that a key must be held down before it repeats. This starts off at 35, but you can POKE in other values.
        repper  addr 23562      # Delay (in 50ths of a second in 60ths of a second in N. America) between successive repeats of a key held down: initially 5.
        defadd  addr 23563, 2   # Address of arguments of user defined function if one is being evaluated; otherwise 0.
        k_data  addr 23565      # Stores 2nd byte of colour controls entered from keyboard .
        tvdata  addr 23566, 2   # Stores bytes of coiour, AT and TAB controls going to television.
                                                        # Addresses of channels attached to streams.
        strms   addr 23568, Strms
        chars   addr 23606, 2   # 256 less than address of character set (which starts with space and carries on to the copyright symbol). Normally in ROM, but you can set up your own in RAM and make CHARS point to it.
        rasp    addr 23608      # Length of warning buzz.
        pip     addr 23609      # Length of keyboard click.
        err_nr  addr 23610      # 1 less than the report code. Starts off at 255 (for 1) so PEEK 23610 gives 255.
        flags   addr 23611      # Various flags to control the BASIC system.
        tv_fla  addr 23612      # Flags associated with the television.
        err_sp  addr 23613, 2   # Address of item on machine stack to be used as error return.
        list_s  addr 23615, 2   # Address of return address from automatic listing.
        mode    addr 23617      # Specifies K, L, C. E or G cursor.
        newppc  addr 23618, 2   # Line to be jumped to.
        nsppc   addr 23620      # Statement number in line to be jumped to. Poking first NEWPPC and then NSPPC forces a jump to a specified statement in a line.
        ppc     addr 23621, 2   # Line number of statement currently being executed.
        subppc  addr 23623      # Number within line of statement being executed.
        bordcr  addr 23624      # Border colour * 8; also contains the attributes normally used for the lower half of the screen.
        e_ppc   addr 23625, 2   # Number of current line (with program cursor).
        vars    addr 23627, 2   # Address of variables.
        dest    addr 23629, 2   # Address of variable in assignment.
        chans   addr 23631, 2   # Address of channel data.
        curchl  addr 23633, 2   # Address of information currently being used for input and output.
        prog    addr 23635, 2   # Address of BASIC program.
        nxtlin  addr 23637, 2   # Address of next line in program.
        datadd  addr 23639, 2   # Address of terminator of last DATA item.
        e_line  addr 23641, 2   # Address of command being typed in.
        k_cur   addr 23643, 2   # Address of cursor.
        ch_add  addr 23645, 2   # Address of the next character to be interpreted: the character after the argument of PEEK, or the NEWLINE at the end of a POKE statement.
        x_ptr   addr 23647, 2   # Address of the character after the ? marker.
        worksp  addr 23649, 2   # Address of temporary work space.
        stkbot  addr 23651, 2   # Address of bottom of calculator stack.
        stkend  addr 23653, 2   # Address of start of spare space.
        breg    addr 23655      # Calculator's b register.
        mem     addr 23656, 2   # Address of area used for calculator's memory. (Usually MEMBOT, but not always.)
        flags2  addr 23658      # More flags.
        df_sz   addr 23659      # The number of lines (including one blank line) in the lower part of the screen.
        s_top   addr 23660, 2   # The number of the top program line in automatic listings.
        oldppc  addr 23662, 2   # Line number to which CONTINUE jumps.
        ospcc   addr 23664      # Number within line of statement to which CONTINUE jumps.
        flagx   addr 23665      # Various flags.
        strlen  addr 23666, 2   # Length of string type destination in assignment.
        t_addr  addr 23668, 2   # Address of next item in syntax table (very unlikely to be useful).
        seed    addr 23670, 2   # The seed for RND. This is the variable that is set by RANDOMIZE.
        frames  addr 23672, 3   # 3 byte (least significant first), frame counter. Incremented every 20ms. See Chapter 18.
        udg     addr 23675, 2   # Address of 1st user defined graphic You can change this for instance to save space by having fewer user defined graphics.
                                                        # coordinate of last point plotted.
        coords  addr 23677, Coords
        p_posn  addr 23679      # 33 column number of printer position
        pr_cc   addr 23680, 2   # Full address of next position for LPRINT to print at (in ZX printer buffer). Legal values 5B00 - 5B1F. [Not used in 128K mode or when certain peripherals are attached]
        echo_e  addr 23682, 2   # 33 column number and 24 line number (in lower half) of end of input buffer.
        df_cc   addr 23684, 2   # Address in display file of PRINT position.
        dfccl   addr 23686, 2   # Like DF CC for lower part of screen.
                                                        # 33 column number for PRINT position, 24 line number for PRINT position.
        s_posn  addr 23688, Cursor
                                                        # Like S POSN for lower part
        sposnl  addr 23690, Cursor
        scr_ct  addr 23692      # Counts scrolls: it is always 1 more than the number of scrolls that will be done before stopping with scroll? If you keep poking this with a number bigger than 1 (say 255), the screen will scroll on and on without asking you.
        attr_p  addr 23693      # Permanent current colours, etc (as set up by colour statements).
        mask_p  addr 23694      # Used for transparent colours, etc. Any bit that is 1 shows that the corresponding attribute bit is taken not from ATTR P, but from what is already on the screen.
        attr_t  addr 23695      # Temporary current colours, etc (as set up by colour items).
        mask_t  addr 23696      # Like MASK P, but temporary.
        p_flag  addr 23697      # More flags.
        membot  addr 23698, 30  # Calculator's memory area; used to store numbers that cannot conveniently be put on the calculator stack.
        nmiadd  addr 23728, 2   # This is the address of a user supplied NMI address which is read by the standard ROM when a peripheral activates the NMI. Probably intentionally disabled so that the effect is to perform a reset if both locations hold zero, but do nothing if the locations hold a non-zero value. Interface 1's with serial number greater than 87315 will initialize these locations to 0 and 80 to allow the RS232 "T" channel to use a variable line width. 23728 is the current print position and 23729 the width - default 80.
        ramtop  addr 23730, 2   # Address of last byte of BASIC system area.
        p_ramt  addr 23732, 2   # Address of last byte of physical RAM.
    end

    module Macros
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
            raise ArgumentError, "output or input must not be hl register pair" if output == hl or input == hl
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
    end
end
