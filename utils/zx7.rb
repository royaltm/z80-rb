# -*- coding: BINARY -*-
require 'tempfile'
##
# =ZX7 decoding routines.
#
# in ZX7::Macros
#
# Example:
#   require 'z80'
#   require 'utils/zx7'
#
#   class Program
#     include Z80
#
#     macro_import  ZX7
#
#                   ld  hl, source
#                   ld  de, destination
#                   call decompress
#                   # ...
#      dzx7_standard :decompress
#      # or
#      decompress   dzx7_standard
#   end
#
# Get compressor from:: World Of Spectrum: {ZX7}[http://www.worldofspectrum.org/infoseekid.cgi?id=0027996]
class ZX7
    COMMAND = File.expand_path(File.join('..', 'bin', 'zx7.exe'), __dir__)
    ##
    # ZX7.compress(data) -> data (zx7 compressed)
    def self.compress(data)
        begin
            file = Tempfile.new 'zx7-pack-', encoding: 'ascii-8bit'
            file.write data
            file.close
            unless File.executable?(ZX7::COMMAND)
                raise "Download: http://www.worldofspectrum.org/pub/sinclair/games-extras/ZX7_(WindowsExecutable).zip and unpack zx7.exe to bin directory."
            end
            system ZX7::COMMAND, file.path
            IO.read(file.path + '.zx7', mode: 'rb')
        ensure
            File.unlink(file.path + '.zx7') rescue false
            file.unlink
        end
    end
    module Macros
        # -----------------------------------------------------------------------------
        # ZX7 decoder by Einar Saukas, Antonio Villena & Metalbrain
        # "Standard" version (69 bytes only)
        # -----------------------------------------------------------------------------
        # Parameters:
        #   HL: source address (compressed data)
        #   DE: destination address (decompressing)
        # -----------------------------------------------------------------------------
        def dzx7_standard(name = nil)
            isolate(name) do
                            ld      a, 0x80
            copy_byte_loop  ldi                             # copy literal byte
            main_loop       call    next_bit
                            jr      NC, copy_byte_loop      # next bit indicates either literal or sequence

            # determine number of bits used for length (Elias gamma coding)
                            push    de
                            ld      bc, 0
                            ld      d, b
            len_size_loop   inc     d
                            call    next_bit
                            jr      NC, len_size_loop

            # determine length
            len_value_loop  call    NC, next_bit
                            rl      c
                            rl      b
                            jr      C, exit0                # check end marker
                            dec     d
                            jr      NZ, len_value_loop
                            inc     bc                      # adjust length

            # determine offset
                            ld      e, [hl]                 # load offset flag (1 bit) + offset value (7 bits)
                            inc     hl
                            sll     e                       # opcode for undocumented instruction "SLL E" aka "SLS E"
                            jr      NC, offset_end          # if offset flag is set, load 4 extra bits
                            ld      d, 0x10                 # bit marker to load 4 bits
            rld_next_bit    call    next_bit
                            rl      d                       # insert next bit into D
                            jr      NC, rld_next_bit        # repeat 4 times, until bit marker is out
                            inc     d                       # add 128 to DE
                            srl d                           # retrieve fourth bit from D
            offset_end      rr      e                       # insert fourth bit into E

            # copy previous sequence
                            ex      [sp], hl                # store source, restore destination
                            push    hl                      # store destination
                            sbc     hl, de                  # HL = destination - offset - 1
                            pop     de                      # DE = destination
                            ldir
            exit0           pop     hl                      # restore source address (compressed data)
                            jr      NC, main_loop
            next_bit        add     a, a                    # check next bit
                            ret     NZ                      # no more bits left?
                            ld      a, [hl]                 # load another group of 8 bits
                            inc     hl
                            rla
                            ret
            # -----------------------------------------------------------------------------
            end
        end
        # -----------------------------------------------------------------------------
        # ZX7 decoder by Einar Saukas & Urusergi
        # "Turbo" version (88 bytes, 25% faster)
        # -----------------------------------------------------------------------------
        # Parameters:
        #   HL: source address (compressed data)
        #   DE: destination address (decompressing)
        # -----------------------------------------------------------------------------
        def dzx7_turbo(name = nil)
            isolate(name) do
                            ld      a, 0x80
            copy_byte_loop  ldi                             # copy literal byte
            main_loop       add     a, a                    # check next bit
                            call    Z, load_bits            # no more bits left?
                            jr      NC, copy_byte_loop      # next bit indicates either literal or sequence

            # determine number of bits used for length (Elias gamma coding)
                            push    de
                            ld      bc, 1
                            ld      d, b
            len_size_loop   inc     d
                            add     a, a                    # check next bit
                            call    Z, load_bits            # no more bits left?
                            jr      NC, len_size_loop
                            jp      len_value_start

            # determine length
            len_value_loop  add     a, a                    # check next bit
                            call    Z, load_bits            # no more bits left?
                            rl      c
                            rl      b
                            jr      C, exit0                # check end marker
            len_value_start dec     d
                            jr      NZ, len_value_loop
                            inc     bc                      # adjust length

            # determine offset
                            ld      e, [hl]                 # load offset flag (1 bit) + offset value (7 bits)
                            inc     hl
                            sll     e                       # opcode for undocumented instruction "SLL E" aka "SLS E"
                            jr      NC, offset_end          # if offset flag is set, load 4 extra bits
                            add     a, a                    # check next bit
                            call    Z, load_bits            # no more bits left?
                            rl      d                       # insert first bit into D
                            add     a, a                    # check next bit
                            call    Z, load_bits            # no more bits left?
                            rl      d                       # insert second bit into D
                            add     a, a                    # check next bit
                            call    Z, load_bits            # no more bits left?
                            rl      d                       # insert third bit into D
                            add     a, a                    # check next bit
                            call    Z, load_bits            # no more bits left?
                            ccf
                            jr      C, offset_end
                            inc     d                       # equivalent to adding 128 to DE
            offset_end      rr      e                       # insert inverted fourth bit into E

            # copy previous sequence
                            ex      [sp], hl                # store source, restore destination
                            push    hl                      # store destination
                            sbc     hl, de                  # HL = destination - offset - 1
                            pop     de                      # DE = destination
                            ldir
            exit0           pop     hl                      # restore source address (compressed data)
                            jp      NC, main_loop

            load_bits       ld      a, [hl]                 # load another group of 8 bits
                            inc     hl
                            rla
                            ret
            # -----------------------------------------------------------------------------
            end
        end
        # -----------------------------------------------------------------------------
        # ZX7 decoder by Einar Saukas
        # "Mega" version (244 bytes, 30% faster)
        # -----------------------------------------------------------------------------
        # Parameters:
        #   HL: source address (compressed data)
        #   DE: destination address (decompressing)
        # -----------------------------------------------------------------------------
        def dzx7_mega(name=nil)
            isolate(name) do
                                ld      a, 0x80
            copy_byte_loop_ev   ldi                            # copy literal byte
            main_loop_ev        add     a, a                   # check next bit
                                jr      Z, load_bits1          # no more bits left?
                                jr      C, len_size_start_od   # next bit indicates either literal or sequence

            copy_byte_loop_od   ldi                            # copy literal byte
            main_loop_od        add     a, a                   # check next bit
                                jr      NC, copy_byte_loop_ev  # next bit indicates either literal or sequence

            len_size_start_ev   label
            # determine number of bits used for length (Elias gamma coding)
                                push    de
                                ld      bc, 1
                                ld      d, b
            len_size_loop_ev    inc     d
                                add     a, a                   # check next bit
                                jr      Z, load_bits2_ev       # no more bits left?
                                jr      NC, len_size_loop_ev
                                jp      len_value_start_ev

            len_size_start_od   label
            # determine number of bits used for length (Elias gamma coding)
                                push    de
                                ld      bc, 1
                                ld      d, b
            len_size_loop_od    inc     d
                                add     a, a                   # check next bit
                                jr      Z, load_bits2_od       # no more bits left?
                                jr      NC, len_size_loop_od
                                jp      len_value_start_od

            # determine length
            len_value_loop_ev   add     a, a                   # check next bit
                                jr      Z, load_bits3_ev       # no more bits left?
                                rl      c
                                rl      b
                                jr      C, exit_ev             # check end marker
            len_value_start_ev  dec     d
                                jr      NZ, len_value_loop_ev
                                inc     bc                     # adjust length
            offset_start_od     label
            # determine offset
                                ld      e, [hl]                # load offset flag (1 bit) + offset value (7 bits)
                                inc     hl
                                sll     e                      # opcode for undocumented instruction "SLL E" aka "SLS E"
                                jr      NC, offset_end_od      # if offset flag is set, load 4 extra bits
                                add     a, a                   # check next bit
                                rl      d                      # insert first bit into D
                                add     a, a                   # check next bit
                                jr      Z, load_bits4          # no more bits left?
                                rl      d                      # insert second bit into D
                                add     a, a                   # check next bit
                                rl      d                      # insert third bit into D
                                add     a, a                   # check next bit
                                jr      Z, load_bits5          # no more bits left?
                                ccf
                                jr      C, offset_end_od
            offset_inc_od       inc     d                      # equivalent to adding 128 to DE
            offset_end_od       rr      e                      # insert inverted fourth bit into E
            # copy previous sequence
                                ex      [sp], hl               # store source, restore destination
                                push    hl                     # store destination
                                sbc     hl, de                 # HL = destination - offset - 1
                                pop     de                     # DE = destination
                                ldir
                                pop     hl                     # restore source address (compressed data)
                                jp      main_loop_od

            load_bits1          ld      a, [hl]                # load another group of 8 bits
                                inc     hl
                                rla
                                jr      C, len_size_start_od   # next bit indicates either literal or sequence
                                jp      copy_byte_loop_od

            load_bits2_ev       ld      a, [hl]                # load another group of 8 bits
                                inc     hl
                                rla
                                jr      NC, len_size_loop_ev
                                jp      len_value_start_ev

            load_bits2_od       ld      a, [hl]                # load another group of 8 bits
                                inc     hl
                                rla
                                jr      NC, len_size_loop_od
                                jp      len_value_start_od

            load_bits3_ev       ld      a, [hl]                # load another group of 8 bits
                                inc     hl
                                rla
                                rl      c
                                rl      b
                                jp      NC, len_value_start_ev # check end marker
            exit_ev             pop     de
                                ret

            load_bits4          ld      a, [hl]                # load another group of 8 bits
                                inc     hl
                                rla
                                rl      d                      # insert second bit into D
                                add     a, a                   # check next bit
                                rl      d                      # insert third bit into D
                                add     a, a                   # check next bit
                                ccf
                                jr      C, offset_end_od
                                jp      offset_inc_od

            load_bits5          ld      a, [hl]                # load another group of 8 bits
                                inc     hl
                                rla
                                ccf
                                jr      C, offset_end_od
                                jp      offset_inc_od

            # determine length
            len_value_loop_od   add     a, a                   # check next bit
                                jr      Z, load_bits3_od       # no more bits left?
                                rl      c
                                rl      b
                                jr      C, exit_od             # check end marker
            len_value_start_od  dec     d
                                jr      NZ, len_value_loop_od
                                inc     bc                     # adjust length
            offset_start_ev     label
            # determine offset
                                ld      e, [hl]                # load offset flag (1 bit) + offset value (7 bits)
                                inc     hl
                                sll     e                      # opcode for undocumented instruction "SLL E" aka "SLS E"
                                jr      NC, offset_end_ev      # if offset flag is set, load 4 extra bits
                                add     a, a                   # check next bit
                                jr      Z, load_bits6          # no more bits left?
                                rl      d                      # insert first bit into D
                                add     a, a                   # check next bit
                                rl      d                      # insert second bit into D
                                add     a, a                   # check next bit
                                jr      Z, load_bits7          # no more bits left?
                                rl      d                      # insert third bit into D
                                add     a, a                   # check next bit
                                ccf
                                jr      C, offset_end_ev
            offset_inc_ev       inc     d                      # equivalent to adding 128 to DE
            offset_end_ev       rr      e                      # insert inverted fourth bit into E
            # copy previous sequence
                                ex      [sp], hl               # store source, restore destination
                                push    hl                     # store destination
                                sbc     hl, de                 # HL = destination - offset - 1
                                pop     de                     # DE = destination
                                ldir
                                pop     hl                     # restore source address (compressed data)
                                jp      main_loop_ev

            load_bits3_od       ld      a, [hl]                # load another group of 8 bits
                                inc     hl
                                rla
                                rl      c
                                rl      b
                                jp      NC, len_value_start_od # check end marker
            exit_od             pop     de
                                ret

            load_bits6          ld      a, [hl]                # load another group of 8 bits
                                inc     hl
                                rla
                                rl      d                      # insert first bit into D
                                add     a, a                   # check next bit
                                rl      d                      # insert second bit into D
                                add     a, a                   # check next bit
                                rl      d                      # insert third bit into D
                                add     a, a                   # check next bit
                                ccf
                                jr      C, offset_end_ev
                                jp      offset_inc_ev

            load_bits7          ld      a, [hl]                # load another group of 8 bits
                                inc     hl
                                rla
                                rl      d                      # insert third bit into D
                                add     a, a                   # check next bit
                                ccf
                                jr      C, offset_end_ev
                                jp      offset_inc_ev

            # -----------------------------------------------------------------------------
            end
        end
        # -----------------------------------------------------------------------------
        # "Smart" integrated RCS+ZX7 decoder by Einar Saukas (110 bytes)
        # -----------------------------------------------------------------------------
        # Parameters:
        #   HL: source address (compressed data)
        #   DE: destination address (decompressing)
        # -----------------------------------------------------------------------------
        def dzx7_smartrcs(name=nil)
            isolate(name) do
                                ld      a, 0x80
            copy_byte_loop      call    copy_byte          # copy literal byte
            main_loop           call    next_bit
                                jr      NC, copy_byte_loop # next bit indicates either literal or sequence

            # determine number of bits used for length (Elias gamma coding)
                                push    de
                                ld      bc, 0
                                ld      d, b
            len_size_loop           inc     d
                                call    next_bit
                                jr      NC, len_size_loop

            # determine length
            len_value_loop      call    NC, next_bit
                                rl      c
                                rl      b
                                jr      C, exit0           # check end marker
                                dec     d
                                jr      NZ, len_value_loop
                                inc     bc                 # adjust length

            # determine offset
                                ld      e, [hl]            # load offset flag (1 bit) + offset value (7 bits)
                                inc     hl
                                sll     e                  # opcode for undocumented instruction "SLL E" aka "SLS E"
                                jr      NC, offset_end     # if offset flag is set, load 4 extra bits
                                ld      d, 0x10            # bit marker to load 4 bits
            rld_next_bit        call    next_bit
                                rl      d                  # insert next bit into D
                                jr      NC, rld_next_bit   # repeat 4 times, until bit marker is out
                                inc     d                  # add 128 to DE
                                srl d                      # retrieve fourth bit from D
            offset_end          rr      e                  # insert fourth bit into E

            # copy previous sequence
                                ex      [sp], hl           # store source, restore destination
                                push    hl                 # store destination
                                sbc     hl, de             # HL = destination - offset - 1
                                pop     de                 # DE = destination
            copy_bytes          push    hl
                                ex      de, hl
                                call    convert
                                ex      de, hl
                                call    copy_byte
                                pop     hl
                                inc     hl
                                jp      PE, copy_bytes
                                pop     hl                 # restore source address (compressed data)
                                jr      main_loop

            next_bit            add     a, a               # check next bit
                                ret     NZ                 # no more bits left?
                                ld      a, [hl]            # load another group of 8 bits
                                inc     hl
                                rla
                                ret

            copy_byte           push    de
                                call    convert
                                ldi
            exit0               pop     de
                                inc     de
                                ret

            # Convert an RCS address 010RRccc ccrrrppp to screen address 010RRppp rrrccccc
            # (note: replace both EX AF,AF' with PUSH AF/POP AF if you want to preserve AF')
            convert             ex      af, af
                                ld      a, d               # A = 010RRccc
                                cp      0x58
                                jr      NC, skip
                                xor     e
                                anda    0xf8
                                xor     e                  # A = 010RRppp
                                push    af
                                xor     d
                                xor     e                  # A = ccrrrccc
                                rlca
                                rlca                       # A = rrrccccc
                                pop     de                 # D = 010RRppp
                                ld      e, a               # E = rrrccccc
            skip                ex      af, af
                                ret
            # -----------------------------------------------------------------------------
            end
        end
        # -----------------------------------------------------------------------------
        # "Agile" integrated RCS+ZX7 decoder by Einar Saukas (150 bytes)
        # -----------------------------------------------------------------------------
        # Parameters:
        #   HL: source address (compressed data)
        #   DE: destination address (decompressing)
        # -----------------------------------------------------------------------------
        def dzx7_agilercs(name=nil)
            isolate(name) do
                                ld      a, 0x80
            copy_byte_loop1     ex      af, af
                                call    copy_byte         # copy literal byte
            main_loop1          ex      af, af
                                add     a, a                    # check next bit
                                call    Z, load_bits      # no more bits left?
                                jr      NC, copy_byte_loop1 # next bit indicates either literal or sequence
                                jr      len_size_start

            copy_byte_loop      ldi                             # copy literal byte
            main_loop           add     a, a                    # check next bit
                                call    Z, load_bits      # no more bits left?
                                jr      NC, copy_byte_loop # next bit indicates either literal or sequence

            len_size_start      label
            # determine number of bits used for length (Elias gamma coding)
                                push    de
                                ld      bc, 1
                                ld      d, b
            len_size_loop       inc     d
                                add     a, a                    # check next bit
                                call    Z, load_bits      # no more bits left?
                                jr      NC, len_size_loop
                                jp      len_value_start

            # determine length
            len_value_loop      add     a, a                    # check next bit
                                call    Z, load_bits      # no more bits left?
                                rl      c
                                rl      b
                                jr      C, exit0           # check end marker
            len_value_start     dec     d
                                jr      NZ, len_value_loop
                                inc     bc                      # adjust length

            # determine offset
                                ld      e, [hl]                 # load offset flag (1 bit) + offset value (7 bits)
                                inc     hl
                                sll     e                # opcode for undocumented instruction "SLL E" aka "SLS E"
                                jr      NC, offset_end    # if offset flag is set, load 4 extra bits
                                add     a, a                    # check next bit
                                call    Z, load_bits      # no more bits left?
                                rl      d                       # insert first bit into D
                                add     a, a                    # check next bit
                                call    Z, load_bits      # no more bits left?
                                rl      d                       # insert second bit into D
                                add     a, a                    # check next bit
                                call    Z, load_bits      # no more bits left?
                                rl      d                       # insert third bit into D
                                add     a, a                    # check next bit
                                call    Z, load_bits      # no more bits left?
                                ccf
                                jr      C, offset_end
                                inc     d                       # equivalent to adding 128 to DE
            offset_end          rr      e                       # insert inverted fourth bit into E

            # copy previous sequence
                                ex      [sp], hl                # store source, restore destination
                                push    hl                      # store destination
                                sbc     hl, de                  # HL = destination - offset - 1
                                pop     de                      # DE = destination
                                ex      af, af
                                ld      a, h                    # A = 010RRccc
                                cp      0x58
                                jr      C, copy_bytes
                                ldir
                                pop     hl                      # restore source address (compressed data)
                                ex      af, af
                                jp      main_loop

            copy_bytes          push    hl
                                ex      de, hl
                                call    convert
                                ex      de, hl
                                call    copy_byte
                                pop     hl
                                inc     hl
                                jp      PE, copy_bytes
                                pop     hl                      # restore source address (compressed data)
                                jr      main_loop1

            load_bits           ld      a, [hl]                 # load another group of 8 bits
                                inc     hl
                                rla
                                ret

            copy_byte           push    de
                                call    convert
                                ldi
            exit0               pop     de
                                inc     de
                                ret

            # Convert an RCS address 010RRccc ccrrrppp to screen address 010RRppp rrrccccc
            # (Note: replace both EX AF,AF' with PUSH AF/POP AF if you want to preserve AF')
            convert             ld      a, d                    # A = 010RRccc
                                cp      0x58
                                ret     NC
                                xor     e
                                anda    0xf8
                                xor     e                       # A = 010RRppp
                                push    af
                                xor     d
                                xor     e                       # A = ccrrrccc
                                rlca
                                rlca                            # A = rrrccccc
                                pop     de                      # D = 010RRppp
                                ld      e, a                    # E = rrrccccc
                                ret
            # -----------------------------------------------------------------------------
            end
        end
    end
    include Z80
end
