class Z80Lib
  module Macros
    # clears max 256 bytes of memory at dest 
    # uses: +a+, +b+, +rr+
    # +dest+::  register16/value/nil destination address
    # +size+::  register/value/nil size of area to clear
    # +value+:: register/value/nil pad byte (nil: a contains pad byte)
    # +rr+::    register16 hl/bc/de/ix/iy tempororary
    def clrmem8(dest, size, value = 0, rr = hl)
      ns do
            ld  b, size if size and size != b
            ld  rr, dest if dest and dest != rr
            if value == 0
              xor a
            elsif value != a
              ld  a, value
            end
      loop1 ld  [rr], a
            inc rr
            djnz loop1
      end
    end
    # clears memory at dest 
    # uses: +bc+, +de+, +hl+
    # +dest+::  register16/value/nil destination address
    # +size+::  register/value/nil size of area to clear
    # +value+:: register/value/nil pad byte (nil: a contains pad byte)
    def clrmem(dest, size, value = 0)
      ns do
            if size
              ld  bc, size - 1
            else
              dec bc
            end
            ld  hl, dest if dest and dest != hl
            ld  e, l
            ld  d, h
            inc de
            ld  [hl], value
            ldir
      end
    end
    # copies memory from source to dest
    # if source/dest and size are static detects memory overlaps
    # uses: +bc+, +de+, +hl+
    # +dest+::      register16/value/nil destination address
    # +dest+::      register16/value/nil source address
    # +size+::      register16/value/nil size of area to clear
    # +reverse+::   flag (true) use lddr, (false) use ldir
    def memcpy(dest=de, source=hl, size=bc, reverse = false)
      ns do
        unless dest.is_a?(Register) or source.is_a?(Register)
            if dest.is_a?(Integer) and source.is_a?(Integer) and dest > source and
                size and !size.is_a?(Register) and source + size > dest
              dest+= size - 1
              source+= size - 1
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
