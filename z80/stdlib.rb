class Z80Lib
    module Macros
        ##
        # Clears max 256 bytes of memory at dest 
        # Slower (does not use ld?r) but involves less registers.
        #
        # Modifies: +a+, +b+, +rr+
        #
        # +dest+::  destination address or same as +rr+
        # +size+::  size of area to clear as immadiate value or one of the 8bit registers except +a+
        # +value+:: fill byte as immediate value or one of the 8bit registers
        # +rr+::    16bit address register: +hl+, +bc+, +de+, +ix+, +iy+
        def clrmem8(dest, size=b, value = 0, rr = hl)
            isolate do
                if value == 0
                        xor a
                elsif value != a
                        ld  a, value
                end
                        ld  b, size if size and size != b
                        ld  rr, dest if dest and dest != rr
                loop1   ld  [rr], a
                        inc rr
                        djnz loop1
            end
        end
        ##
        # Clears memory at +dest+
        #
        # Modifies: +bc+, +de+, +hl+
        #
        # +dest+:: destination address or +hl+
        # +size+:: 16bit size of area to clear as immediate value or +bc+
        # +value+:: 8bit immediate value or one of the registers: +a+, +b+, +c+, +d+, +e+
        def clrmem(dest, size, value = 0)
            raise ArgumentError if (dest.is_a?(Register) and dest != hl) or
                                                         (size.is_a?(Register) and size != bc)
            isolate do
                        ld  hl, dest if dest and dest != hl
                        ld  [hl], value
                    if size and size != bc
                        ld  bc, size - 1
                    else
                        dec bc
                    end
                        ld  e, l
                        ld  d, h
                        inc de
                        ldir
            end
        end
        ##
        # Clears memory at +dest+ in a fast way using unrolled +ldi+
        #
        # Modifies: +bc+, +de+, +hl+
        #
        # +dest+:: destination address or +hl+
        # +size+:: static size of area to clear, should be a reasonably small immediate value
        # +value+:: 8bit immediate value or one of the registers: +a+, +b+, +c+, +d+, +e+
        def clrmem_quick(dest, size, value = 0)
            raise ArgumentError if (dest.is_a?(Register) and dest != de) or !immediate?(size)
            isolate do
                        ld  hl, dest if dest and dest != hl
                        ld  [hl], value
                if size > 1
                        ld16 de, hl
                        inc  de
                        (size.to_i - 1).times { ldi }
                end
            end
        end
        ##
        # Clears a memory area using unrolled +push+ with a tight loop. The address should point
        # to the next byte after the memory area to be cleared.
        #
        # Modifies: +b+, +hl+
        def clrmem_fastest(address=hl, chunks_count=b, chunk_size=2, disable_interrupts:true, enable_interrupts:true, save_sp:true)
            chunk_size = chunk_size.to_i
            raise ArgumentError "chunk_size must be even" if chunk_size.odd?
            raise ArgumentError "chunk_size must be less than 256" if chunk_size >= 256
            isolate do
                            ld  [restore_sp + 1], sp if save_sp
                            ld  b, chunks_count unless chunks_count==b || chunks_count <= 1
                            di if disable_interrupts
                            ld  sp, address unless address == sp
                            ld  hl, 0
                loop1       label
                            (chunk_size>>1).times { push hl }
                            djnz loop1 unless chunks_count <= 1
                restore_sp  ld  sp, 0 if save_sp
                            ei if enable_interrupts
            end
        end
        ##
        # Copies memory from +source+ to +dest+
        #
        # If +source+ or +dest+ and +size+ are absolute detects memory overlaps.
        #
        # Modifies: +bc+, +de+, +hl+
        #
        # +dest+::    destination address or +de+
        # +source+::  source address or +hl+
        # +size+::    size of area to copy or +bc+
        # +reverse+:: flag (true) use lddr, (false) use ldir
        def memcpy(dest=de, source=hl, size=bc, reverse = false)
            raise ArgumentError if (dest.is_a?(Register) and dest != de) or
                                    (source.is_a?(Register) and source != hl) or
                                    (size.is_a?(Register) and size != bc)
            isolate do
                if immediate?(dest) and immediate?(source) and immediate?(size) and
                        dest.to_i > source.to_i and source.to_i + size.to_i > dest.to_i
                    dest+= size.to_i - 1
                    source+= size.to_i - 1
                    reverse = true
                end
                        ld  de, dest if dest and dest != de
                        ld  hl, source if source and source != hl
                        ld  bc, size if size and size != bc
                if reverse
                        lddr
                else
                        ldir
                end
            end
        end
        ##
        # Copies memory from +source+ to +dest+ in a fast way using unrolled +ldi+
        #
        # If +source+ or +dest+ and +size+ are static detects memory overlaps.
        #
        # Modifies: +bc+, +de+, +hl+
        #
        # +dest+::    destination address or +de+
        # +source+::  source address or +hl+
        # +size+::    static size of area to clear, should be a reasonably small immediate value
        # +reverse+:: flag (true) use lddr, (false) use ldir
        def memcpy_quick(dest=de, source=hl, size=1, reverse = false)
            raise ArgumentError if (dest.is_a?(Register) and dest != de) or (source.is_a?(Register) and dest != hl) or
                                                         !immediate?(size) or size.to_i < 1
            size = size.to_i
            isolate do
                if immediate?(dest) and immediate?(source) and
                        dest.to_i > source.to_i and source.to_i + size.to_i > dest.to_i
                    dest+= size - 1
                    source+= size - 1
                    reverse = true
                end
                        ld  de, dest if dest and dest != de
                        ld  hl, source if source and source != hl
                if reverse
                    size.times { ldd }
                else
                    size.times { ldi }
                end
            end
        end
    end
    include Z80
end
