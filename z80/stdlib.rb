class Z80Lib
  module Macros
    ##
    # Clears max 256 bytes of memory at dest 
    # Slower (does not uses ld?r) but involves less registers.
    #
    # Uses: +a+, +b+, +rr+
    #
    # +dest+::  destination address or same as +rr+
    # +size+::  size of area to clear as immadiate value or one of the 8bit registers except +a+
    # +value+:: fill byte as immediate value or one of the 8bit registers
    # +rr+::    16bit address register: +hl+, +bc+, +de+, +ix+, +iy+
    def clrmem8(dest, size=b, value = 0, rr = hl)
      ns do
            if value == 0
              xor a
            elsif value != a
              ld  a, value
            end
            ld  b, size if size and size != b
            ld  rr, dest if dest and dest != rr
      loop1 ld  [rr], a
            inc rr
            djnz loop1
      end
    end
    ##
    # Clears memory at +dest+
    #
    # Uses: +bc+, +de+, +hl+
    #
    # +dest+:: destination address or +hl+
    # +size+:: 16bit size of area to clear as immediate value or +bc+
    # +value+:: 8bit immediate value or one of the registers: +a+, +b+, +c+, +d+, +e+
    def clrmem(dest, size, value = 0)
      ns do
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
    # Uses: +bc+, +de+, +hl+
    #
    # +dest+:: destination address or +hl+
    # +size+:: static size of area to clear, should be a reasonably small immediate value
    # +value+:: 8bit immediate value or one of the registers: +a+, +b+, +c+, +d+, +e+
    def clrmem_quick(dest, size, value = 0)
      ns do
            ld  hl, dest if dest and dest != hl
            ld  [hl], value
            if size > 1
              ld  e, l
              ld  d, h
              inc de
              size.to_i.times { ldi }
            end
      end
    end
    ##
    # Copies memory from +source+ to +dest+
    #
    # If +source+ or +dest+ and +size+ are static detects memory overlaps.
    #
    # Uses: +bc+, +de+, +hl+
    #
    # +dest+::    destination address or +de+
    # +source+::  source address or +hl+
    # +size+::    size of area to copy or +bc+
    # +reverse+:: flag (true) use lddr, (false) use ldir
    def memcpy(dest=de, source=hl, size=bc, reverse = false)
      ns do
        unless dest.is_a?(Register) or source.is_a?(Register)
            if dest.is_a?(Integer) and source.is_a?(Integer) and dest > source and
                size and !size.is_a?(Register) and source + size.to_i > dest
              dest+= size.to_i - 1
              source+= size.to_i - 1
              reverse = true
            end
        end
				ld	de, dest if dest and dest != de
				ld	hl, source if source and source != hl
				ld	bc, size if size and size != bc
        if reverse
          lddr
        else
          ldir
        end
      end
		end
  end
  include Z80
end
