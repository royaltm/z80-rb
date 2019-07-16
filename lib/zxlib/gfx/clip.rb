require 'z80'
require 'z80/math_i'
require 'zxlib/gfx'
module ZXLib
  module Gfx
    ##
    #  ==A module with Z80 Macros for clipping lines.
    class Clip
      module Outcode
        INSIDE      = 0b00000000
        LEFT_MASK   = 0b00000001
        LEFT_BIT    = 0
        RIGHT_MASK  = 0b00000010
        RIGHT_BIT   = 1
        BOTTOM_MASK = 0b01000000
        BOTTOM_BIT  = 6
        TOP_MASK    = 0b10000000
        TOP_BIT     = 7
      end
      ##
      #  ==ZXLib::Gfx::Clip Macros for clipping lines to viewport rectangles.
      #
      #  * Macros.gfx_clip_line.
      #  * Macros.gfx_clip_coords_to_draw_line_args.
      module Macros
        include Z80::MathInt::Macros
        ##
        # Converts clipped 16-bit coordinates to the "draw line" routine arguments.
        #
        # The line endpoints are 16-bit signed integers: +x0+, +y0+ and +x1+, +y1+.
        #
        # This routine should be used after clipping the coordinates e.g. with Macros.gfx_clip_line.
        #
        # _NOTE_:: This routine expects the line endpoints to be clipped (each in the range: 0..255).
        #
        # +xx+:: the 16-bit register pair that holds +x1+ in the main and +x0+ in the alternative
        #        set: +bc+ or +de+.
        # +yy+:: the 16-bit register pair that holds +y1+ in the main and +y0+ in the alternative 
        #        set: +de+ or +bc+.
        #
        # When routine is over the alternative and main registers will be swapped.
        #
        # Option +args_type+ can be one of:
        # * +:zxlib+:: Prepares arguments for the Gfx::Draw::Macros.draw_line routine (default).
        # * +:rom+:: Prepares arguments for the +rom.draw_line_1+ routine (at 0x24BA):
        #              h|l: the starting y|x point - should be put into vars.coords
        #              b|c: abs(dy)|abs(dx) - absolute distances from the starting point
        #              d|e: sgn(dy)|sgn(dx) - the directions (-1 or 1) of the distances
        #
        # T-states: 24 + ( +zxlib+: 60 or +rom+: ~65 [54/65/65/76] )
        #
        # Modifies: +af+, +af'+, +hl'+, +bc'+, +de'+, swaps registers: +bc+, +de+, +hl+.
        def gfx_clip_coords_to_draw_line_args(xx=bc, yy=de, args_type: :zxlib)
          raise ArgumentError unless [bc,de].include?(xx) and [bc,de].include?(yy) and xx != yy
          _, xl = xx.split
          _, yl = yy.split
          isolate do |eoc|                    # xx: x1, yy: y1
                                ld   a, xl    # x1
                                ex   af, af
                                ld   a, yl    # y1
                                exx           # xx: x0, yy: y0
                                ld   h, yl    # y0
                                ld   l, xl    # x0
            case args_type
            when :zxlib
                                sub  h        # y1 - y0
                                ld   e, a     # dy
                                sbc  a
                                ld   d, a     # d: sgn(dy)
                                xor  e
                                sub  d
                                ld   e, a     # e: abs(dy)
                                ex   af, af   # x1
                                sub  l        # x1 - x0
                                ld   c, a     # dx
                                sbc  a
                                ld   b, a     # b: sgn(dx)
                                xor  c
                                sub  b
                                ld   c, a     # c: abs(dx)
            when :rom
                                sub  h        # y1 - y0
                                ld   de, 0x0101
                                jr   NC, dy_pos
                                2.times { dec  d }
                                neg
              dy_pos            ld   b, a     # abs(dy)
                                ex   af, af
                                sub  l        # x1 - x0
                                jr   NC, dx_pos
                                2.times { dec  e }
                                neg
              dx_pos            ld   c, a     # abs(dx)
            else
              raise ArgumentError, "args_type must be :zxlib or :rom"
            end
          end
        end
        ##
        # Creates a routine that computes the Outcode bits for the xx, yy point and the clipping region.
        # Stores result in +outcode+. Used by: Macros.gfx_clip_line.
        #
        # Modifies: +af+, +outcode+.
        def gfx_clip_compute_outcode(outcode, xx=bc, yy=de, xmax:ixh, xmin:ixl, ymax:iyh, ymin:iyl, jump_rel:true, subroutine:false)
          raise ArgumentError unless [bc,de,hl].include?(xx) and [bc,de,hl].include?(yy) and xx != yy
          xh, xl = xx.split
          yh, yl = yy.split
          raise ArgumentError if [xmin, xmax, ymin, ymax, outcode].any? {|mm| [xh,xl,yh,yl,a].include?(mm) }
          isolate do |eoc|
                                ld  outcode, Outcode::INSIDE
                                cmp_i16r xh, xl, 0, xmin, gt: x_fits_xmin, eq: x_fits_xmin, jump_rel:jump_rel
                                set Outcode::LEFT_BIT, outcode
              if jump_rel
                                jr  x_fits_xmax
              else
                                jp  x_fits_xmax
              end
              x_fits_xmin       cmp_i16r xh, xl, 0, xmax, lt: x_fits_xmax, eq: x_fits_xmax, jump_rel:jump_rel
                                set Outcode::RIGHT_BIT, outcode
              x_fits_xmax       cmp_i16r yh, yl, 0, ymin, gt: y_fits_ymin, eq: y_fits_ymin, jump_rel:jump_rel
                                set Outcode::BOTTOM_BIT, outcode
              if subroutine
                                ret
              elsif jump_rel
                                jr  eoc
              else
                                jp  eoc
              end
              y_fits_ymin       label
              if subroutine
                                cmp_i16r yh, yl, 0, ymax, lt: :ret, eq: :ret, jump_rel:jump_rel
              else
                                cmp_i16r yh, yl, 0, ymax, lt: eoc, eq: eoc, jump_rel:jump_rel
              end
                                set Outcode::TOP_BIT, outcode
                                ret if subroutine
          end
        end
        ##
        # Creates a routine that calculates +dx+ and +dy+.
        # Used by: Macros.gfx_clip_line.
        #
        # _NOTE_:: Swaps +bc+, +de+, +hl+ registers with their alternative counterparts (EXX).
        #
        # The routine expects (before EXX):
        # * +x0+ in +xx'+,
        # * +y0+ in +yy'+,
        # * +x1+ in +xx+,
        # * +y1+ in +yy+.
        #
        # The routine returns (after EXX):
        #    h  = abs(x1 - x0) / k
        #    l  = abs(y1 - y0) / k
        #    a' = sgn(x1 - x0) XOR sgn(y1 - y0)
        # Finds such +k+ that +h+ and +l+ does not overflow 8 bits.
        #
        # +sgn+ is a boolean: 0x00 for positive and 0xFF for negative.
        #
        # Modifies: +af+, +af'+, +hl+ and 2 levels of the stack, swaps registers: +bc+, +de+, +hl+.
        def gfx_clip_calculate_8bit_dx_dy_exx(xx=bc, yy=de, full_range_delta:true)
          raise ArgumentError unless [bc,de].include?(xx) and [bc,de].include?(yy) and xx != yy
          isolate do
                                push yy           # y1
                                push xx           # x1
                                exx
                                pop  hl           # x1
                                xor  a            # a: 0, CF: 0
                                sbc  hl, xx       # hl: dx = x1 - x0
            if full_range_delta
                                jp   PO, dx_fits_range
                                jp   M, dx_positive
                                jr   dx_negative
              dx_fits_range     label
            end
                                jp   P, dx_positive

            dx_negative         cpl               # a: -sgn
                                ex   af, af
                                neg16 h, l        # dx = -dx
                                ex   af, af

            dx_positive         ex   [sp], hl     # hl: y1, sp -> dx
                                ora  a            # CF: 0
                                sbc  hl, yy       # hl: dy = y1 - y0
            if full_range_delta
                                jp   PO, dy_fits_range
                                jp   M, dy_positive
                                jr   dy_negative
              dy_fits_range     label
            end
                                jp   P, dy_positive

            dy_negative         cpl               # a: -sgn
                                ex   af, af
                                neg16 h, l        # dy = -dy
                                ex   af, af

            dy_positive         ex   de, hl       # hl: xx or yy, de: dy
                                ex   [sp], hl     # hl: dx, sp -> xx or yy
                                ex   af, af       # a': sgn
                                ld   a, d
                                ora  h
                                jr   Z, fits      # bit8 fit
            fitloop             srl  d
                                rr   e            # dy = dy / 2
                                srl  h
                                rr   l            # dx = dx / 2
                                ld   a, d
                                ora  h
                                jr   NZ, fitloop
            fits                ld   h, e         # h: dy, l: dx
                                pop  de           # xx or yy
          end
        end
        ##
        # Creates a subroutine that clips a single dimension.
        # Used by: Macros.gfx_clip_line.
        #
        #    hl = a0 + sign * d1 * (minmax - b0) / d2
        #
        # Expects:
        # * +minmax+ in +a+,
        # * +sign+ in +a'+ as a boolean: 0x00 for positive and 0xFF for negative.
        #
        # Modifies: +af+, +af'+, +hl+ and 3 levels of the machine stack.
        def gfx_clip_dimension(a0:bc, b0:de, d1:l, d2:h, full_range_delta:true)
          raise ArgumentError unless [bc,de].include?(a0) and [bc,de].include?(b0) and a0 != b0
          raise ArgumentError unless [h,l].include?(d1) and [h,l].include?(d2) and d1 != d2
          a0h, a0l = a0.split
          b0h, b0l = b0.split
          minmax = a
          isolate do
                                push a0
                                ld   a0h, d1
                                ld   a0l, d2
                                ld   l, minmax
                                xor  a
                                ld   h, a
                                sbc  hl, b0
                                push b0
            if full_range_delta
                                jp   PO, arg_fits_range
                                jp   M, positive_arg
                                jr   negative_arg
              arg_fits_range    label
            end
                                jp   P, positive_arg
            negative_arg        ex   af, af
                                cpl                   # a: -sgn
                                ex   af, af
                                neg16 h, l
            positive_arg        push a0
                                mul8_24 h, l, a0h, t:a0l, tt:b0, clrahl:true  # a|hl = d1 * (minmax - b0)
                                pop  a0
                                ld   b0l, a
                                divmod24_8 b0l, h, l, a0l, check0:false # divide (a|hl) / d2
                                # TODO check CF, check a0h
                                ex   af, af           # a: sgn
                                ora  a
                                jr   Z, positive_result
                                neg16 h, l
            positive_result     pop  b0
                                pop  a0
                                add  hl, a0
          end
        end
        ##
        # Creates a subroutine for clipping lines to the rectangle viewport area using Cohen–Sutherland algorithm.
        #
        # The line endpoints are 16-bit signed integers: +x0+, +y0+ and +x1+, +y1+.
        # The viewport area is determined by four 8-bit unsigned integers: +xmax+, +xmin+, +ymax+, +ymin+.
        #
        # The routine verifies if any part of the line crosses the viewport area.
        # If it does the +Z+ flag is being set to 1. +ZF+ will be 0 if the line does not cross the viewport region.
        # The line endpoints will not be modified if both of them are already in the viewport region.
        #
        # On input the beginning coordinates: +x0+, +y0+ are expected to be held in main Z80 registers,
        # the end coordinates: +x1+, +y1+ are expected to be held in the alternative register set (swapped out).
        #
        # When routine is over the alternative and main registers will be swapped (+x1+, +y1+ will be in the main).
        #
        # +xx+:: the 16-bit register pair that initially holds +x0+ in the main and +x1+ in the alternative 
        #        set: +bc+ or +de+.
        # +yy+:: the 16-bit register pair that initially holds +y0+ in the main and +y1+ in the alternative 
        #        set: +de+ or +bc+.
        #
        # Each of the viewport options: +xmax+, +xmin+, +ymax+ and +ymin+ can be either an 8-bit half of index
        # register: +ix+, +iy+ (dynamic viewports) or a constant, as an 8-bit integer or a label.
        #
        # * +full_range_delta+ can be set to +false+ for a smaller and slightly faster code if you don't
        # expect values of |x1 - x0| or |y1 - y0| to be exceeding 32767.
        # * +compact+ can be set to +false+ to create larger (~143) but slightly faster code.
        #
        # Modifies: +af+, +af'+, +hl+, +hl'+, +bc+, +bc'+, +de+, +de'+ and the machine stack.
        def gfx_clip_line(xx=bc, yy=de, xmax:ixh, xmin:ixl, ymax:iyh, ymin:iyl, full_range_delta:true, compact:true)
          raise ArgumentError unless [bc,de].include?(xx) and [bc,de].include?(yy) and xx != yy
          xh, xl = xx.split
          yh, yl = yy.split
          isolate do
            if compact
                                call compute_outcode
                                ld   a, l
            else
                                gfx_clip_compute_outcode h, xx, yy, xmin:xmin, xmax:xmax, ymin:ymin, ymax:ymax # h: outcode0
                                ld   a, h                 # outcode0
            end
                                exx
                                ld   h, a                 # outcode0
            clip_loop1          label
            if compact
                                call compute_outcode
            else
                                gfx_clip_compute_outcode l, xx, yy, xmin:xmin, xmax:xmax, ymin:ymin, ymax:ymax # l: outcode1
            end
                                ld   a, h                 # outcode0
            clip_loop0          ora  l                    # outcode0 | outcode1
                                ret  Z                    # ZF:1 ready, bc:x1, de:y1, bc':x0, de':y0
                                ld   a, h                 # outcode0
                                anda l                    # outcode0 & outcode1
                                ret  NZ                   # ZF: 0 nothing to draw, move on
                                # xx: x1, yy: y1, xx': x0, yy': y0, h: outcode0, l: outcode1
                                gfx_clip_calculate_8bit_dx_dy_exx xx, yy, full_range_delta:full_range_delta
                                # xx: x0, yy: y0, xx': x1, yy': y1, h': outcode0, l': outcode1, h: dy, l: dx, a': sgn(dx)^sgn(dy)
                                ld   a, h
                                cp   l                    # dy - dx >= 0
                                exx
                                jr   C, left_right0       # dx > dy
            # TOP and BOTTOM outcode has priority
            top_bottom0         ld   a, h                 # outcode0
                                ora  a
                                jr   Z, top_bottom1
                                exx
                                rlca                      # CF: Outcode::TOP_BIT
                                jr   C, clip_area0.top
                                rlca                      # CF: Outcode::BOTTOM_BIT
                                jr   C, clip_area0.bottom
                                anda Outcode::LEFT_MASK<<2
                                jr   NZ, clip_area0.left
                                jp   clip_area0.right
            # LEFT and RIGHT outcode has priority
            left_right0         ld   a, h                 # outcode0
                                ora  a
                                jr   Z, left_right1
                                exx
                                rrca                      # CF: Outcode::LEFT_BIT
                                jr   C, clip_area0.left
                                rrca                      # CF: Outcode::RIGHT_BIT
                                jr   C, clip_area0.right
                                anda Outcode::TOP_MASK>>2
                                jr   NZ, clip_area0.top
                                jp   clip_area0.bottom

            ns :clip_area0 do
              {top:ymax, bottom:ymin}.each do |sublabel, yminmax|
                ns sublabel do
                                ld   a, yminmax
                                call clip_x_dimension
                                ld16 xx, hl           # x0 = x0 + sgn * dx * (yminmax - y0) / dy
                                ld   yh, 0
                                ld   yl, yminmax      # y0 = yminmax
                                jp   recomputecode0
                end
              end
              {left:xmin, right:xmax}.each do |sublabel, xminmax|
                ns sublabel do
                                ld   a, xminmax
                                call clip_y_dimension
                                ld16 yy, hl           # y0 = y0 + sgn * dy * (xminmax - x0) / dx
                                ld   xh, 0
                                ld   xl, xminmax      # x0 = xminmax
                                jp   recomputecode0
                end
              end
            end

            # TOP and BOTTOM outcode has priority
            top_bottom1         ld   a, l                 # outcode1
                                exx
                                rlca                      # CF: Outcode::TOP_BIT
                                jr   C, clip_area1.top
                                rlca                      # CF: Outcode::BOTTOM_BIT
                                jr   C, clip_area1.bottom
                                anda Outcode::LEFT_MASK<<2
                                jr   NZ, clip_area1.left
                                jp   clip_area1.right
            # LEFT and RIGHT outcode has priority
            left_right1         ld   a, l                 # outcode1
                                exx
                                rrca                      # CF: Outcode::LEFT_BIT
                                jr   C, clip_area1.left
                                rrca                      # CF: Outcode::RIGHT_BIT
                                jr   C, clip_area1.right
                                anda Outcode::TOP_MASK>>2
                                jr   NZ, clip_area1.top
                                jp   clip_area1.bottom

            ns :clip_area1 do
              {top:ymax, bottom:ymin}.each do |sublabel, yminmax|
                ns sublabel do
                                ld   a, yminmax
                                call clip_x_dimension
                                push hl
                                exx
                                pop  xx               # x1 = x0 + sgn * dx * (yminmax - y0) / dy
                                ld   yh, 0
                                ld   yl, yminmax      # y1 = yminmax
                                jp   clip_loop1
                end
              end
              {left:xmin, right:xmax}.each do |sublabel, xminmax|
                ns sublabel do
                                ld   a, xminmax
                                call clip_y_dimension
                                push hl
                                exx
                                pop  yy               # y1 = y0 + sgn * dy * (xminmax - x0) / dx
                                ld   xh, 0
                                ld   xl, xminmax      # x1 = xminmax
                                jp   clip_loop1
                end
              end
            end

            ns :recomputecode0 do
              if compact
                                call compute_outcode
                                ld   a, l
              else
                                gfx_clip_compute_outcode h, xx, yy, xmin:xmin, xmax:xmax, ymin:ymin, ymax:ymax # h: outcode0
                                ld   a, h             # outcode0
              end
                                exx
                                ld   h, a             # outcode0
                                jp   clip_loop0
            end

            # hl = x0 + a' * dx * (a - y0) / dy
            clip_x_dimension    gfx_clip_dimension a0:xx, b0:yy, d1:l, d2:h, full_range_delta:full_range_delta
                                ret
            # hl = y0 + a' * dy * (a - x0) / dx
            clip_y_dimension    gfx_clip_dimension a0:yy, b0:xx, d1:h, d2:l, full_range_delta:full_range_delta
                                ret
            if compact
              compute_outcode   gfx_clip_compute_outcode l, xx, yy, xmin:xmin, xmax:xmax, ymin:ymin, ymax:ymax, subroutine:true
            end
          end
        end
      end # Macros
    end # Clip
  end # Gfx
end # ZXLib
