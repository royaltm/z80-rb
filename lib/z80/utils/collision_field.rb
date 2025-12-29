# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'

module Z80
    module Utils
        ##
        # =Z80::Utils::CollisionField
        #
        # Routines to modify and check collision fields in Z80::Utils::CollisionField::Macros.
        #
        # The collision fields is a binary matrix consisting of columns and rows.
        # Each field element can have one of two states: set or clear, and is represented by a single
        # data bit.
        class CollisionField
            ##
            # =Z80::Utils::CollisionField Macros
            #
            # CollisionField macros require:
            #
            #    macro_import MathInt
            module Macros
                ##
                # Returns a size in bytes required for the collision field data storage.
                #
                # +field_width+:: Must be a non-zero, positive integer and be a multiple of 8.
                # +field_height+:: Must be a non-zero, positive integer.
                #
                # Note that collision field routines can handle only limited sizes of collision fields.
                def collision_field_bytesize(field_width, field_height)
                    unless Integer === field_height and field_height >= 1 and
                           Integer === field_width and (field_width & 7) == 0 and field_width >= 8
                        raise ArgumentError, "collision_field_bytesize: invalid arguments"
                    end
                    field_width / 8 * field_height
                end
                ##
                # Creates precalculated mask data to be used with collision field routines.
                #
                # +data_type+:
                #
                #     :single_bit            10000000, 01000000 ... 00001000 ... 00000001
                #     :start_insert          11111111, 01111111 ... 00001111 ... 00000001
                #     :start_remove          00000000, 10000000 ... 11110000 ... 11111110
                #     :end_insert            10000000, 11000000 ... 11111000 ... 11111111
                #     :end_remove            01111111, 00111111 ... 00000111 ... 00000000
                #
                def collision_field_preshifted_mask_data(data_type)
                    case data_type
                    when :single_bit
                                # 10000000 01000000 ... 00001000 ... 00000001
                                bytes (0..7).map{|x|   0x80 >> x }
                    when :start_insert
                                # 11111111 01111111 ... 00001111 ... 00000001
                                bytes (0..7).map{|x|   0xFF >> x }
                    when :start_remove
                                # 00000000 10000000 ... 11110000 ... 11111110
                                bytes (0..7).map{|x| ~(0xFF >> x) }
                    when :end_insert
                                # 10000000 11000000 ... 11111000 ... 11111111
                                bytes (0..7).map{|x| ~(0x7F >> x) }
                    when :end_remove
                                # 01111111 00111111 ... 00000111 ... 00000000
                                bytes (0..7).map{|x|   0x7F >> x }
                    else
                        raise ArgumentError, "data_type should be one of: :single_bit, :start_insert, :end_insert, :start_remove, :end_remove"
                    end
                end
                ##
                # Creates a routine that checks whether a field element is set, using field coordinates.
                #
                # The procedure will return the result via the Z flag. The flag will be empty (NZ) if the
                # element causes a collision, or raised (Z) otherwise.
                #
                # +collision_field+:: An address of a field as a label or an intermediate pointer.
                # +rowcol+:: A pair of 16-bit registers, containing the +row|col+ coordinates of a field element.
                #
                # The range of the +col+ coordinate is [0-MAX] where MAX is +field_width+ - 1.
                # The range of the +row+ coordinate is [0-MAX] where MAX is the field height - 1.
                # Both +row+ and +col+ will have unused bits clipped out.
                #
                # Options:
                #
                # * +tt+:: +bc+ or +de+. Registers to be used when +rowcol+ is +hl+ and +bit_preshift+ is +nil+
                #           or is an address.
                # * +bit_preshift+:: An optional +:single_bit+ preshift table address as a label, an
                #                    intermediate pointer or one of the 16-bit registers. In each case,
                #                    the table must fit within a 256-byte page boundary.
                # * +field_width+:: The bit width of the collision field as a constant integer. See below.
                # * +custom_op+:: If a block of code is given, a custom code is injected in place of a
                #                 checking routine which can perform other actions on field data.
                #
                # If +bit_preshift+ is +nil+ the sub-routine will be created and a table will be appended
                # to it.
                #
                # The supported +field_width+ option values and maximum field heights for each value:
                #
                #    field_width   max field height
                #              8   256
                #             16   128
                #             32    64 (default)
                #             64    32
                #            128    16
                #            256     8
                #
                # +custom_op+ procedure will be given field data byte address and a bit mask address in
                # 2 pairs of 16-bit registers, depending on the given arguments:
                #
                #   address given in:   field byte  bit mask
                #   *                     rowcol       hl
                #   bit_preshift: de        hl         de
                #   bit_preshift: bc        hl         bc
                #         rowcol: hl        hl         tt
                #
                # There's a special case where both +collision_field_check_element+ and +collision_field_check_pixel+ are
                # required in the same program. In this case, in order to save a few bytes, the following
                # sub-routine combo can be produced this way:
                #
                #   rowcol_collides?  collision_field_check_element(collision_field, bc)
                #   pixel_collides?   collision_field_check_pixel(nil, bc, bit_preshift: rowcol_collides?.bit_preshift)
                #                     jr   rowcol_collides?.combo_finish
                #
                # In this instance both the +rowcol+ and the +yx+ arguments must be the same pair of registers
                # provided to both functions. The same applies to +tt+ and +bit_preshift+ options if used and
                # if +bit_preshift+ contains a register pair.
                #
                # Modifies: +af+, +hl+, +bc+ or +de+ depending on the +rowcol+, +bit_preshift+ or +tt+ selection.
                #
                # T-states (for +field_width+ = 32):
                #          rowcol  bit_preshift   collision_field
                #     102  *       bc||de||hl     direct address
                #     112  *       bc||de         intermediate
                #     112  *       direct         direct address
                #     122  *       nil (w/ret)    direct address
                #     116  de||bc  hl             intermediate
                #     118  de||bc  intermediate   direct address
                #     122  hl      direct         intermediate
                #     132  hl      nil (w/ret)    intermediate
                #     122  hl      intermediate   direct address
                #     126  de||bc  direct         intermediate
                #     136  de||bc  nil (w/ret)    intermediate
                #     132  *       intermediate   intermediate
                def collision_field_check_element(collision_field, rowcol, tt:de, bit_preshift:nil, field_width:32, &custom_op)
                    raise ArgumentError, "collision_field_check_element: invalid arguments" unless [bc, de].include?(tt) and
                                    [bc, de, hl].include?(rowcol) and
                                    (collision_field.nil? or address?(collision_field)) and
                                    !(collision_field.nil? and bit_preshift.nil?) and
                                    (bit_preshift.nil? or address?(bit_preshift) or [bc, de, hl].include?(bit_preshift)) and
                                    rowcol != bit_preshift and Integer === field_width
                    unless [8, 16, 32, 64, 128, 256].include?(field_width)
                            raise ArgumentError, "collision_field_check_element: unsupported field width: #{field_width}"
                    end
                    mask = (field_width >> 3)
                    left_shift = 0
                    while mask != 1 do
                        left_shift += 1
                        mask >>= 1
                    end
                    local_preshift = bit_preshift.nil?
                    mask_reg = if register?(bit_preshift)
                            bit_preshift
                    elsif rowcol == hl
                            tt
                    else
                            hl
                    end
                    field_reg = if mask_reg == hl
                            rowcol
                    else
                            hl
                    end
                    row, col = rowcol.split
                    mask_reghi, mask_reglo = mask_reg.split
                    field_reghi, field_reglo = field_reg.split
                    isolate do
                        bit_preshift = define_label(:bit_preshift) if local_preshift
                                    ld   a, col           # col
                                    anda 0b00000111       # field bit (col % 8)
                                    ld   mask_reg, bit_preshift unless bit_preshift == mask_reg
                                    add  a, mask_reglo
                                    ld   mask_reglo, a    # mask_reg: -> mask
                        if left_shift > 0
                            mask = (1 << left_shift) - 1
                                    ld   a, col           # col
                            3.times { rrca }
                                    anda mask             # a: col / 8
                                    ld   field_reglo, a   # col / 8
                        end
                        # 4+7+   4+4+4+7+3*4+4=46 (register bit_preshift)
                        # 4+7+10+4+4+4+7+3*4+4=56 (direct bit_preshift or nil)
                        # 4+7+16+4+4+4+7+3*4+4=62 (pointer bit_preshift and rowcol != hl)
                        # 4+7+20+4+4+4+7+3*4+4=66 (pointer bit_preshift and rowcol == hl)
                                    ld   a, row           # a: xxhhhhhh
                        if left_shift == 5
                                    anda 0b00000111
                                    3.times { rrca }      # a: hhh00000 row * 32
                        else
                            left_shift.times { add a, a } # a: hhhhhh00 row * (1, 2, 4, 8, 16)
                        end
                        unless collision_field.nil?
                    combo_finish    label
                            if left_shift > 0
                                    add  a, field_reglo   # a: (row * N) + col / 8
                            end
                            if pointer?(collision_field)
                                    ld   field_reg, collision_field
                                    adda_to field_reghi, field_reglo
                            else
                                    adda_to_n16 collision_field, oh: field_reghi, ol: field_reglo
                            end
                            if block_given?
                                    ns(&custom_op)
                            else
                                if field_reg == hl
                                    ld   a, [mask_reg]
                                    anda [field_reg]
                                else # mask_reg == hl
                                    ld   a, [field_reg]
                                    anda [mask_reg]
                                end
                            end
                        end
                        # 4+2*4+4+(20+20)+7+7=70 (pointer collision_field and bit_preshift not [bc, de] and row_col != hl)
                        # 4+2*4+4+(16+20)+7+7=66 (pointer collision_field and (bit_preshift in [bc, de] or row_col == hl))
                        # 4+2*4+4+(26)+7+7=56 (address collision_field)
                        if local_preshift
                                    ret
                                    # 10000000 01000000 ... 00001000 ... 00000001
                    bit_preshift    collision_field_preshifted_mask_data(:single_bit)
                        end
                        select((bit_preshift + 7) & 0xFF) {|x| x >= 7 }.else do
                            raise ArgumentError, "bit_preshift data must reside on a single 256-byte page of memory"
                        end if direct_address?(bit_preshift)
                    end
                end
                ##
                # Creates a routine that checks whether a pixel coordinate collides with an element
                # in the collision field.
                #
                # The procedure will return the result via the Z flag. The flag will be empty (NZ) if the
                # element causes a collision, or raised (Z) otherwise.
                #
                # +collision_field+:: An address of a field as a label or an intermediate pointer.
                # +yx+:: A pair of 16-bit registers, containing the +y|x+ coordinates of a pixel.
                #
                # Each field element is equivalent to an 8 by 8 pixel square.
                #
                # The range of the +x+ coordinate is [0-MAX], where MAX is the +field_width+ * 8 - 1.
                # The range of the +y+ coordinate is [0-MAX], where MAX is the field height * 8 - 1.
                # +x+ will have unused bits clipped out.
                #
                # Options:
                #
                # * +tt+:: +bc+ or +de+. Registers to be used when +yx+ is +hl+ and +bit_preshift+
                #          is +nil+ or is an address.
                # * +bit_preshift+:: An optional +:single_bit+ preshift table address as a label, an
                #                    intermediate pointer or one of the 16-bit registers. In each case,
                #                    the table must fit within a 256-byte page boundary.
                # * +field_width+:: The bit width of the collision field as a constant integer.
                #                   Supported values are: 8, 16 and 32.
                # * +custom_op+:: If a block of code is given, a custom code is injected in place of a
                #                 checking routine which can perform other actions on field data.
                #
                # If +bit_preshift+ is +nil+ the sub-routine will be created and a table will be appended
                # to it.
                #
                # +custom_op+ procedure will be given field data byte address and a bit mask address in
                # 2 pairs of 16-bit registers, depending on the given arguments:
                #
                #   address given in:   field byte  bit mask
                #   *                       yx         hl
                #   bit_preshift: de        hl         de
                #   bit_preshift: bc        hl         bc
                #             yx: hl        hl         tt
                #
                # Modifies: +af+, +hl+, +bc+ or +de+ depending on the +yx+, +bit_preshift+ or +tt+ selection.
                def collision_field_check_pixel(collision_field, yx, tt:de, bit_preshift:nil, field_width:32, &custom_op)
                    raise ArgumentError, "collision_field_check_pixel: invalid arguments" unless [bc, de].include?(tt) and
                                    [bc, de, hl].include?(yx) and
                                    (collision_field.nil? or address?(collision_field)) and
                                    !(collision_field.nil? and bit_preshift.nil?) and
                                    (bit_preshift.nil? or address?(bit_preshift) or [bc, de, hl].include?(bit_preshift)) and
                                    yx != bit_preshift and Integer === field_width
                    unless [8, 16, 32].include?(field_width)
                            raise ArgumentError, "collision_field_check_pixel: unsupported field width: #{field_width}"
                    end
                    mask = (field_width >> 3)
                    left_shift = 0
                    while mask != 1 do
                        left_shift += 1
                        mask >>= 1
                    end
                    local_preshift = bit_preshift.nil?
                    mask_reg = if register?(bit_preshift)
                            bit_preshift
                    elsif yx == hl
                            tt
                    else
                            hl
                    end
                    field_reg = if mask_reg == hl
                            yx
                    else
                            hl
                    end
                    y, x = yx.split
                    mask_reghi, mask_reglo = mask_reg.split
                    field_reghi, field_reglo = field_reg.split
                    isolate do
                        bit_preshift = define_label(:bit_preshift) if local_preshift
                                    ld   a, x           # a: hhbbbxxx
                            3.times { rrca }            # a: xxxhhbbb
                                    anda 0b00000111     # a: 00000bbb (x/8 % 8)
                                    ld   mask_reg, bit_preshift unless bit_preshift == mask_reg
                                    add  a, mask_reglo  # a: bit_preshift + (x/8 % 8)
                                    ld   mask_reglo, a  # mask_reg: -> mask
                        if left_shift > 0
                            mask = (1 << left_shift) - 1
                                    ld   a, x           # a: hhbbbxxx
                                2.times { rlca }        # a: bbbxxxhh
                                    anda mask           # a: 000000hh
                                    ld   field_reglo, a # x: 000000hh (x / 64)
                        end
                                    ld   a, y           # a: hhhhhxxx
                                    anda 0b11111000     # a: hhhhh000
                            (3-left_shift).times do
                                    rrca                # a: 0hhhhh00 (y / 8 * (1, 2, 4))
                            end
                        unless collision_field.nil?
                    combo_finish    label
                            if left_shift > 0
                                    add  a, field_reglo # a: 0hhhhhhh (y / 8 * N) + (x / 64)
                            end
                            if pointer?(collision_field)
                                    ld   field_reg, collision_field
                                    adda_to field_reghi, field_reglo
                            else
                                    adda_to_n16 collision_field, oh: field_reghi, ol: field_reglo
                            end
                            if block_given?
                                    ns(&custom_op)
                            else
                                if field_reg == hl
                                    ld   a, [mask_reg]
                                    anda [field_reg]
                                else # mask_reg == hl
                                    ld   a, [field_reg]
                                    anda [mask_reg]
                                end
                            end
                        end # collision_field.nil?
                        if local_preshift
                                    ret
                                    # 10000000 01000000 ... 00001000 ... 00000001
                    bit_preshift    collision_field_preshifted_mask_data(:single_bit)
                        end
                        select((bit_preshift + 7) & 0xFF) {|x| x >= 7 }.else do
                          raise ArgumentError, "bit_preshift data must reside on a single 256-byte page of memory"
                        end if direct_address?(bit_preshift)
                    end
                end
                ##
                # Creates a routine to calculate collision field address and parameters required by other routines
                # from field coordinates of the rectangle area.
                #
                # +collision_field+:: A direct or intermediate address or a label of the collision field.
                # +mask_preshift_start+:: A direct or intermediate address or a label of the  +:start_insert+
                #                    or +:start_remove+ preshift table. In each case, the table must fit within
                #                    a 256-byte page boundary.
                # +mask_preshift_end+:: A direct or intermediate address or a label of the  +:end_insert+
                #                    or +:end_remove+ preshift table. In each case, the table must fit within
                #                    a 256-byte page boundary.
                #
                # Options:
                # * +field_width+:: A column width of the collision field. Supported field width values are:
                #                   8, 16, 32, 64, 128, 256.
                # * +clip_col+:: Whether to clip unused bits of the +column+ value.
                #
                # The routine arguments are expected in the following registers:
                # * +e+:: a rectangle starting +column+ [0 - MAX], where MAX is +field_width+ - 1.
                # * +d+:: a rectangle starting +row+ [0 - MAX], where MAX is field height - 1.
                # * +c+:: a rectangle column +width+ [1 - MAX], where MAX is (+field_width+ - +column+).
                #
                # The routine result is found in the following registers:
                # * +hl+:: A collision field starting address.
                # *  +e+:: A bit mask covering elements of the starting column byte of each data row.
                # *  +d+:: A bit mask covering elements of the ending column byte of each data row.
                #
                # The following registers are only set if +field_width+ is larger than 8.
                # *  +a+:: A difference between the ending and starting column byte addresses multiplied by 8.
                #          Possible values: 0, 8, 16, ...(field_width - 8).
                # *  +f+:: +Z+ flag after testing if a == 0.
                #
                # The exact calculations performed:
                #
                #   hl = collision_field + clip8bits(row * (field_width / 8)) + opt_clip_max(column / 8)
                #    e = 0xFF >> (column % 8)
                #    d = ~(0x7F >> ((width + (column % 8) - 1)) % 8)
                #    a = (width + (column % 8) - 1) & (field_width - 8)
                #   ZF = (a == 0)
                #
                # Modifies: +af+, +af'+, +c+, +de+, +hl+
                # T-states:
                #   field_width  clip_col   clip_col
                #                  false      true
                #             8     133        133
                #            16     177        179
                #            32     181        183
                #            64     185        187
                #           128     189        191
                #           256     192        192
                #   +10 for +collision_field+ as an intermediate address.
                #   + 6 for each +mask_preshift_...+ as an intermediate address.
                def collision_field_rect_coords(collision_field, mask_preshift_start, mask_preshift_end, field_width:32, clip_col:false)
                    raise ArgumentError, "collision_field_rect_coords: invalid arguments" unless address?(collision_field) and
                                    address?(mask_preshift_start) and address?(mask_preshift_end) and
                                    Integer === field_width
                    unless [8, 16, 32, 64, 128, 256].include?(field_width)
                            raise ArgumentError, "collision_field_rect_coords: unsupported field width: #{field_width}"
                    end
                    mask = (field_width >> 3)
                    left_shift = 0
                    while mask != 1 do
                        left_shift += 1
                        mask >>= 1
                    end
                    row, col = de.split
                    width = c
                    isolate do
                        if (clip_col || left_shift.zero?) && left_shift != 5
                                    ld   a, col
                                    anda 0b00000111    # col % 8
                                    ld   hl, mask_preshift_start
                                    add  a, l
                                    ld   l, a
                                    ld   a, [hl]
                                    ex   af, af        # a': left mask
                        # 4+7+10+4+4+7+4=40
                                    ld   a, col
                                    anda 0b00000111    # col % 8
                                    add  a, width      # column width + (col % 8)
                                    dec  a             # column width + (col % 8) - 1
                                    ld   width, a      # column width + (col % 8) - 1
                                    anda 0b00000111    # field bit: ((width + (col % 8) - 1)) % 8)
                                    ld   hl, mask_preshift_end
                                    add  a, l
                                    ld   l, a          # hl: -> right mask
                        # 4+7+4+4+4+7+10+4+4=48
                            if left_shift > 0
                            mask = (1 << left_shift) - 1
                                    ld   a, col        # col
                            3.times { rrca }
                                    anda mask          # a: col / 8
                                    ld   col, a        # col / 8
                            end
                        # 4+3*4+7+4=27
                        # ( 8): 40+48=88
                        # (>8): 40+48+27=115
                        else
                                    ld   a, col        # col
                                    anda 0b00000111    # field bit
                                    ld   l, a          # l: col % 8
                                    xor  col           # eeeee000
                            3.times { rrca }           # a: col/8
                                    ld   col, a        # e: col/8
                        # 4+7+4+4+3*4+4=35
                                    ld   a, width      # column width
                                    add  l             # column width + (col % 8)
                                    ld   width, a      # column width + (col % 8)
                                    dec  width         # column width + (col % 8) - 1
                        # 4*4=16
                                    ld   a, l          # col % 8
                                    ld   hl, mask_preshift_start
                                    add  a, l
                                    ld   l, a
                                    ld   a, [hl]
                                    ex   af, af        # a': left mask
                        # 4+10+4+4+7+4=33
                                    ld   a, width      # column width + (col % 8) - 1
                                    anda 0b00000111    # field bit: ((width + (col % 8) - 1)) % 8)
                                    ld   hl, mask_preshift_end
                                    add  a, l
                                    ld   l, a          # hl: -> right mask
                        # 4+7+10+4+4=29
                        # 35+16+33+29=113
                        end
                                    ld   a, row        # row
                                    ld   row, [hl]     # right mask
                        if left_shift == 5
                                    anda 0b00000111
                                    3.times { rrca }   # a: hhh00000 row * 32
                        else                           # a: hhhhhh00 row * (1, 2, 4, 8, 16)
                            left_shift.times { add a, a }
                        end
                        if left_shift > 0
                                    add  a, col        # row * N + col/8
                        end
                        if pointer?(collision_field)
                                    ld   hl, collision_field
                                    adda_to h, l
                        else # 16+20=36
                                    adda_to_n16 collision_field, oh: h, ol: l
                        end # 26
                        # (8)     : 4+7+26=37
                        # (16-128): 4+7+(left_shift*4)+4+26=45,49,53,57(,61)
                        # (256)   : 4+7+7+3*4+4+26=60
                                    ex   af, af        # a: left mask
                                    ld   col, a        # left mask
                        if left_shift > 0
                            mask = ((1 << left_shift) - 1) << 3
                                    ld   a, width      # width + (col % 8) - 1
                                    # anda 0b00011000  # (width + (col % 8) - 1) & 24
                                    anda mask          # (width + (col % 8) - 1) & clipmask
                        end
                        # ( 8) : 4+4=8
                        # (>8) : 4+4+(4+7)=19
                                    # 11111111 01111111 ... 00001111 ... 00000001
                                    # 00000000 10000000 ... 11110000 ... 11111110
                        select((mask_preshift_start + 7) & 0xFF) {|x| x >= 7 }.else do
                            raise ArgumentError, "mask_preshift_start data must fit on a single 256-byte page of memory"
                        end
                                    # 10000000 11000000 ... 11111000 ... 11111111
                                    # 01111111 00111111 ... 00000111 ... 00000000
                        select((mask_preshift_end + 7) & 0xFF) {|x| x >= 7 }.else do
                          raise ArgumentError, "mask_preshift_end data must fit on a single 256-byte page of memory"
                        end
                    end
                end
                ##
                # Creates a routine to calculate collision field address and parameters required by other routines
                # from pixel coordinates of the rectangle area.
                #
                # Each field element is equivalent to an 8 by 8 pixel square.
                #
                # +collision_field+:: A direct or intermediate address or a label of the collision field.
                # +mask_preshift_start+:: A direct or intermediate address or a label of the  +:start_insert+
                #                    or +:start_remove+ preshift table. In each case, the table must fit within
                #                    a 256-byte page boundary.
                # +mask_preshift_end+:: A direct or intermediate address or a label of the  +:end_insert+
                #                    or +:end_remove+ preshift table. In each case, the table must fit within
                #                    a 256-byte page boundary.
                #
                # Options:
                # * +field_width+:: A column width of the collision field. Supported field width values are:
                #                   8, 16, 32.
                #
                # The routine arguments are expected in the following registers:
                # * +e+:: a rectangle starting pixel +x+ [0 - MAX], where MAX is +field_width+ * 8 - 1.
                # * +d+:: a rectangle starting pixel +y+ [0 - MAX], where MAX is field height * 8 - 1.
                # * +c+:: a rectangle pixel +width+ [1 - MAX], where MAX is (+field_width+ * 8 - +x+).
                # * +b+:: a rectangle pixel +height+ [1 - MAX], where MAX is (field height * 8 - +y+).
                #
                # The routine result is found in the following registers:
                # * +hl+:: A collision field starting address.
                # *  +e+:: A bit mask covering elements of the starting column byte of each data row.
                # *  +d+:: A bit mask covering elements of the ending column byte of each data row.
                # *  +b+:: A rectangle area row count.
                #
                # The following registers are only set if +field_width+ is larger than 8.
                # *  +a+:: A difference between the ending and starting column byte addresses multiplied by 8.
                #          Possible values: 0, 8, 16, ...(field_width - 8).
                # *  +f+:: +Z+ flag after testing if a == 0.
                #
                # The exact calculations performed:
                #
                #   hl = collision_field + (y / 8 * (field_width / 8)) + clip_x(x / 64)
                #    e = 0xFF >> (x / 8 % 8)
                #    d = ~(0x7F >> ((((x % 64) + width - 1) / 8) % 8)
                #    b = ((y % 8) + height + 7) / 8
                #    a = (((x % 64) + width - 1) / 8) & (field_width - 8)
                #
                # Modifies: +af+, +af'+, +bc+, +de+, +hl+.
                # T-states:
                #   field_width
                #             8    221
                #            16    255
                #            32    251
                #   +10 for +collision_field+ as an intermediate address
                #   + 6 for each +mask_preshift_...+ as an intermediate address
                def collision_field_rect_pixels(collision_field, mask_preshift_start, mask_preshift_end, field_width:32)
                    raise ArgumentError, "collision_field_rect_pixels: invalid arguments" unless address?(collision_field) and
                                    address?(mask_preshift_start) and address?(mask_preshift_end) and
                                    Integer === field_width
                    unless [8, 16, 32].include?(field_width)
                            raise ArgumentError, "collision_field_rect_pixels: unsupported field width: #{field_width}"
                    end
                    mask = (field_width >> 3)
                    left_shift = 0
                    while mask != 1 do
                        left_shift += 1
                        mask >>= 1
                    end
                    y, x = de.split
                    height, width = bc.split
                    isolate do
                                    ld   a, x           # a: hhbbbxxx
                            3.times { rrca }            # a: xxxhhbbb
                                    anda 0b00000111     # a: 00000bbb (x / 8 % 8)
                                    ld   hl, mask_preshift_start
                                    add  a, l           # a: bit_preshift + (x / 8 % 8)
                                    ld   l, a           # hl: -> mask
                                    ld   a, [hl]
                                    ex   af, af         # left preshift
                        # 4+3*4+7+10+4+4+7+4=52
                                    ld   a, x           # a: hhbbbxxx
                                    anda 0b00111111     # a: 00bbbxxx (x % 64)
                                    add  a, width       # (x % 64) + pixel width
                                    dec  a              # (x % 64) + pixel width - 1
                            3.times { rrca }            # a: xxxhhbbb
                                    ld   width, a       # width: ((x % 64) + width - 1) / 8
                                    anda 0b00000111     # field bit: (((x % 64) + width - 1) / 8) % 8
                                    ld   hl, mask_preshift_end
                                    add  a, l
                                    ld   l, a
                        # 4+7+4+4+3*4+4+7+10+4+4=60
                        if left_shift > 0
                            mask = (1 << left_shift) - 1
                                    ld   a, x           # a: hhbbbxxx
                            2.times { rlca }            # a: bbbxxxhh
                                    anda mask           # a: 000000hh
                                    ld   x, a           # x: 000000hh (x / 64)
                        end
                        # 4+2*4+7+4=23
                                    ld   a, y
                                    anda 0b00000111     # a: 00000hhh (y % 8)
                                    add  a, height      # (y % 8) + height
                                    add  7              # (y % 8) + height + 7
                                    anda 0b11111000     # a: hhhhh000
                            3.times { rrca }            # a: 000hhhhh
                                    ld   height, a      # ((y % 8) + height + 7) / 8
                        # 4+7+4+7+7+3*4+4=45
                                    ld   a, y           # a: hhhhhxxx
                                    ld   y, [hl]        # y: right preshift
                                    anda 0b11111000     # a: hhhhh000
                            (3-left_shift).times do
                                    rrca                # a: 0hhhhh00 (y / 8 * (1, 2, 4))
                            end
                        if left_shift > 0
                                    add  a, x           # (y / 8 * (field_width / 8)) + (x / 64)
                        end
                        if pointer?(collision_field)
                                    ld   field_reg, collision_field
                                    adda_to h, l
                        else # 16+20=36
                                    adda_to_n16 collision_field, oh: h, ol: l
                        end # 26
                        # ( 8) : 4+7+7+3*4+26=56
                        # (16) : 4+7+7+2*4+4+26=56
                        # (32) : 4+7+7+1*4+4+26=52
                                    ex   af, af         # a: left preshift
                                    ld   x, a           # left preshift
                        if left_shift > 0
                            mask = ((1 << left_shift) - 1) << 3
                                    ld   a, width       # a:  (width - (width % 64)) / 8
                                    # anda 0b00011000   # a: ((width - (width % 64)) / 8) & 24
                                    anda mask           # a: ((width - (width % 64)) / 8) & clipmask
                        end
                        # ( 8) : 4+4=8
                        # (>8) : 4+4+(4+7)=19
                                    # 11111111 01111111 ... 00001111 ... 00000001
                                    # 00000000 10000000 ... 11110000 ... 11111110
                        select((mask_preshift_start + 7) & 0xFF) {|x| x >= 7 }.else do
                          raise ArgumentError, "mask_preshift_start data must fit on a single 256-byte page of memory"
                        end
                                    # 10000000 11000000 ... 11111000 ... 11111111
                                    # 01111111 00111111 ... 00000111 ... 00000000
                        select((mask_preshift_end + 7) & 0xFF) {|x| x >= 7 }.else do
                          raise ArgumentError, "mask_preshift_end data must fit on a single 256-byte page of memory"
                        end
                    end
                end
                ##
                # Creates a routine that modifies a rectangle area of the collision field.
                #
                # +op+:: Operation to perform: +:insert+, +:remove+ or +:flip+. The operation should
                #        match the preshifted mask data type with the corresponding suffixes:
                #        either +_insert+ for +:insert+ or +:flip+ operations and +_remove+ for the +:remove+
                #        operation.
                #
                # Options:
                # * +field_width+:: A column width of the collision field. Supported field width values are:
                #                   8, 16, 32, 64, 128, 256.
                # * +unroll_max+:: Create unrolled code that handles up to the given number of bytes between
                #                  ending and starting column byte of a single row. Valid values are between
                #                  0 and up to +field_width+/8 - 2.
                # * +unrolled_only+:: Do not create code to handle rectangle widths exceeding the largest
                #                     width for which an unrolled handler was created.
                # * +self_modified+:: Whether to allow creating self-modifying code.
                #
                # For example, passing option values:
                #
                # * +field_width+: 32 and +unroll_max+: 2 creates only unrolled code that handles rectangles
                #   of the full field width.
                # * +field_width+: 32, +unroll_max+: 0 and +unrolled_only+ creates unrolled code that handles
                #   only rectangles up to the 8 field columns in width in any column starting position.
                #
                # The routine expects arguments in the following registers:
                # * +hl+:: A collision field starting address.
                # *  +e+:: A bit mask covering elements of the starting column byte of each data row.
                # *  +d+:: A bit mask covering elements of the ending column byte of each data row.
                # *  +b+:: A rectangle area row count.
                #
                # The following registers are only considered if +field_width+ is larger than 8.
                # *  +a+:: A difference between the ending and starting column byte addresses multiplied by 8.
                #          Possible values: 0, 8, 16, ...(field_width - 8).
                # *  +f+:: +Z+ flag after testing if a == 0.
                #
                # The routines created by Macros#collision_field_rect_coords or Macros#collision_field_rect_pixels
                # can be used to calculate the input arguments from the rectangle coordinates.
                #
                # Modifies: +af+, +af'+, +bc+, +de+, +hl+.
                def collision_field_modify_rectangle(op, field_width:32, unroll_max:0, unrolled_only:false, self_modified:true)
                    unless [8, 16, 32, 64, 128, 256].include?(field_width)
                        raise ArgumentError, "collision_field_modify_rectangle: unsupported field width: #{field_width}"
                    end
                    unless Integer === unroll_max and unroll_max >= 0
                        raise ArgumentError, "collision_field_modify_rectangle: invalid unroll_max option"
                    end
                    unroll_limit = (field_width/8 - 2)
                    if unroll_max > unroll_limit
                        raise ArgumentError, "collision_field_modify_rectangle: unroll_max exceeds capacity: #{unroll_max} > #{unroll_limit}"
                    end if unroll_limit >= 0

                    unroll_limit = unroll_max if unrolled_only

                    if unroll_max == unroll_limit - 1
                        unroll_max = unroll_limit
                    end
                    merge_op = case op
                    when :insert then ->(x) { anda x }
                    when :remove then ->(x) { ora x }
                    when :flip   then ->(x) { anda x }
                    else
                        raise ArgumentError, "collision_field_modify_rectangle: invalid op: #{op}"
                    end
                    set_val = case op
                    when :insert then 0xff
                    when :remove then 0x00
                    end
                    op = case op
                    when :insert then ->(x) { ora x }
                    when :remove then ->(x) { anda x }
                    when :flip   then ->(x) { xor x }
                    end
                    unroll = ->(nsname, inner_bytes) do
                        isolate(nsname) do |eoc|
                            if inner_bytes < unroll_limit
                                    cp   (inner_bytes+1) << 3
                                    jr   NZ, eoc
                            end
                            if (field_width/8 - inner_bytes - 1) > 4
                                    ld   a, b
                                    ld   bc, (field_width/8 - inner_bytes - 1)
                            loop0   ex   af, af
                                    ld   a, [hl]
                                    op.call e
                                    ld   [hl], a
                                    inc  hl
                            inner_bytes.times do
                                if set_val
                                    ld   [hl], set_val
                                else
                                    ld   a, [hl]
                                    cpl
                                    ld   [hl], a
                                end
                                    inc  hl
                            end
                                    ld   a, [hl]
                                    op.call d
                                    ld   [hl], a
                                    add  hl, bc
                                    ex   af, af
                                    dec  a
                                    jr   NZ, loop0
                            else
                                    ld   c, set_val if set_val && inner_bytes > 0
                            loop0   ld   a, [hl]
                                    op.call e
                                    ld   [hl], a
                                    inc  hl
                            inner_bytes.times do
                                if set_val
                                    ld   [hl], c
                                else
                                    ld   a, [hl]
                                    cpl
                                    ld   [hl], a
                                end
                                    inc  hl
                            end
                                    ld   a, [hl]
                                    op.call d
                                    ld   [hl], a
                                (field_width/8 - inner_bytes - 1).times do
                                    inc  hl
                                end
                                    djnz loop0
                            end
                                    ret
                        end
                    end
                    isolate do
                        ns :cols1 do |eoc|
                                    jr   NZ, eoc if field_width > 8
                                    ld   a, e
                                    merge_op.call d
                                    ld   c, a
                                    ld   de, field_width/8 if field_width > 8
                        loop1       ld   a, [hl]
                                    op.call c
                                    ld   [hl], a
                            if field_width > 8
                                    add  hl, de 
                            else
                                    inc  hl
                            end
                                    djnz loop1
                                    ret
                        end
                        if field_width > 8
                            (0..unroll_max).each do |inner_bytes|
                                    unroll.call(:"cols#{inner_bytes+2}", inner_bytes)
                            end
                            if unroll_max < unroll_limit
                                3.times { rrca }
                                    dec  a
                                    ld   [scan_width_p], a if self_modified
                                    cpl
                                    add  a, field_width/8
                                    ld   c, a     # skip cols
                                    ld   a, b     # scan height
                        looprow     ex   af, af   # a': scan height
                                    ld   a, [hl]
                                    op.call e
                                    ld   [hl], a
                                    inc  hl
                                if self_modified
                    scan_width_a    ld   b, 0     # scan width
                    scan_width_p    as   scan_width_a + 1
                                else
                                    ld   a, field_width/8 - 1
                                    sub  c     # scan skip
                                    ld   b, a
                                end
                        loopinner   label
                                if set_val
                                    ld   [hl], set_val
                                else
                                    ld   a, [hl]
                                    cpl
                                    ld   [hl], a
                                end
                                    inc  hl
                                    djnz loopinner
                                    ld   a, [hl]
                                    op.call d
                                    ld   [hl], a
                                    add  hl, bc   # skip cols
                                    ex   af, af   # a: scan height
                                    dec  a
                                    jr   NZ, looprow
                                    ret
                            end # if unroll_max < unroll_limit
                        end # if field_width > 8
                    end
                end
                ##
                # Creates a routine that checks whether a rectangle area collides with at least one element
                # in the collision field.
                #
                # The procedure will return the result via the Z flag. The flag will be empty (NZ) if the
                # element causes a collision, or raised (Z) otherwise.
                #
                # The preshifted mask data type used with this function should match +_insert+ suffix.
                #
                # Options:
                # * +field_width+:: A column width of the collision field. Supported field width values are:
                #                   8, 16, 32, 64, 128, 256.
                # * +unroll_max+:: Create unrolled code that handles up to the given number of bytes between
                #                  ending and starting column byte of a single row. Valid values are between
                #                  0 and up to +field_width+/8 - 2.
                # * +unrolled_only+:: Do not create code to handle rectangle widths exceeding the largest
                #                     width for which an unrolled handler was created.
                # * +self_modified+:: Whether to allow creating self-modifying code.
                #
                # For example, passing option values:
                #
                # * +field_width+: 32 and +unroll_max+: 2 creates only unrolled code that handles rectangles
                #   of the full field width.
                # * +field_width+: 32, +unroll_max+: 0 and +unrolled_only+ creates unrolled code that handles
                #   only rectangles up to the 8 field columns in width in any column starting position.
                #
                # The routine expects arguments in the following registers:
                # * +hl+:: A collision field starting address.
                # *  +e+:: A bit mask covering elements of the starting column byte of each data row.
                # *  +d+:: A bit mask covering elements of the ending column byte of each data row.
                # *  +b+:: A rectangle area row count.
                #
                # The following registers are only considered if +field_width+ is larger than 8.
                # *  +a+:: A difference between the ending and starting column byte addresses multiplied by 8.
                #          Possible values: 0, 8, 16, ...(field_width - 8).
                # *  +f+:: +Z+ flag after testing if a == 0.
                #
                # The routines created by Macros#collision_field_rect_coords or Macros#collision_field_rect_pixels
                # can be used to calculate the input arguments from the rectangle coordinates.
                #
                # Modifies: +af+, +af'+, +bc+, +de+, +hl+.
                def collision_field_check_rectangle(field_width:32, unroll_max:0, unrolled_only:false, self_modified:true)
                    unless [8, 16, 32, 64, 128, 256].include?(field_width)
                            raise ArgumentError, "collision_field_check_rectangle: unsupported field width: #{field_width}"
                    end
                    unless Integer === unroll_max and unroll_max >= 0
                        raise ArgumentError, "collision_field_check_rectangle: invalid unroll_max option"
                    end
                    unroll_limit = (field_width/8 - 2)
                    if unroll_max > unroll_limit
                        raise ArgumentError, "collision_field_check_rectangle: unroll_max exceeds capacity: #{unroll_max} > #{unroll_limit}"
                    end if unroll_limit >= 0

                    unroll_limit = unroll_max if unrolled_only

                    if unroll_max == unroll_limit - 1
                        unroll_max = unroll_limit
                    end
                    unroll = ->(nsname, inner_bytes) do
                        isolate(nsname) do |eoc|
                            if inner_bytes < unroll_limit
                                    cp   (inner_bytes+1) << 3
                                    jr   NZ, eoc
                            end
                            if (field_width/8 - inner_bytes - 1) > 4
                                    ld   a, b
                                    ld   bc, (field_width/8 - inner_bytes - 1)
                            loop0   ex   af, af
                                    ld   a, [hl]
                                    anda e
                                    ret  NZ
                                    inc  hl
                                inner_bytes.times do
                                    ora  [hl]
                                    ret  NZ
                                    inc  hl
                                end
                                    ld   a, [hl]
                                    anda d
                                    ret  NZ
                                    add  hl, bc
                                    ex   af, af
                                    dec  a
                                    jr   NZ, loop0
                            else
                            loop0   ld   a, [hl]
                                    anda e
                                    ret  NZ
                                    inc  hl
                                inner_bytes.times do
                                    ora  [hl]
                                    ret  NZ
                                    inc  hl
                                end
                                    ld   a, [hl]
                                    anda d
                                    ret  NZ
                                (field_width/8 - inner_bytes - 1).times do
                                    inc  hl
                                end
                                    djnz loop0
                            end
                                    ret
                        end
                    end
                    isolate do
                        ns :cols1 do |eoc|
                                    jr   NZ, eoc if field_width > 8
                                    ld   a, e
                                    anda d
                                    ld   c, a
                                    ld   de, field_width/8 if field_width > 8
                        loop1       ld   a, [hl]
                                    anda c
                                    ret  NZ
                            if field_width > 8
                                    add  hl, de 
                            else
                                    inc  hl
                            end
                                    djnz loop1
                                    ret
                        end
                        if field_width > 8
                            (0..unroll_max).each do |inner_bytes|
                                    unroll.call(:"cols#{inner_bytes+2}", inner_bytes)
                            end
                            if unroll_max < unroll_limit
                                3.times { rrca }
                                    dec  a
                                    ld   [scan_width_p], a if self_modified
                                    cpl
                                    add  a, field_width/8
                                    ld   c, a     # skip cols
                                    ld   a, b     # scan height
                        looprow     ex   af, af   # a': scan height
                                    ld   a, [hl]
                                    anda e
                                    ret  NZ
                                    inc  hl
                                if self_modified
                    scan_width_a    ld   b, 0     # scan width
                    scan_width_p    as   scan_width_a + 1
                                else
                                    ld   a, field_width/8 - 1
                                    sub  c     # scan skip
                                    ld   b, a
                                end
                        loopinner   ora  [hl]
                                    ret  NZ
                                    inc  hl
                                    djnz loopinner
                                    ld   a, [hl]
                                    anda d
                                    ret  NZ
                                    add  hl, bc   # skip cols
                                    ex   af, af   # a: scan height
                                    dec  a
                                    jr   NZ, looprow
                                    ret
                            end # if unroll_max < unroll_limit
                        end # if field_width > 8
                    end
                end
            end
            include Z80
        end
    end
end
