require 'z80'
require 'z80/math_i'
require 'zxlib/gfx'
module ZXLib
  module Gfx
    ##
    #  ==A module with Z80 Macros for drawing lines and plotting pixels on the ZX Spectrum.
    #
    #  Example:
    #
    #    require 'zxlib/gfx/draw'
    #    require 'zxlib/basic'
    #
    #    include ZXLib
    #    
    #    class DrawLine
    #      include Z80
    #      include Z80::TAP
    #      macro_import    ZXLib::Gfx::Draw
    #    
    #      with_saved :start, :exx, hl, ret: true do
    #                      ld   hl, [points[0]]
    #                      ld   de, [points[1]]
    #                      prepare_args_draw_line_to
    #                      call draw.line
    #      end
    #    
    #      points          data ZXLib::Sys::Coords, 2, {x:0, y:0}, {x:255, y:191}
    #    
    #      draw            make_draw_line_subroutines(scraddr: 0x4000, check_oos: true)
    #    end
    #    drawline = DrawLine.new 0x8000
    #    puts drawline.debug
    #    program = Basic.parse_source <<-END
    #       10 INPUT "x0:";x0;" y0:";y0, "x1:";x1;" y1:";y1
    #          POKE #{drawline['points.x']}, x0: POKE #{drawline['points.y']}, y0
    #          POKE #{drawline['points.x']+2}, x1: POKE #{drawline['points.y']+2}, y1
    #          RANDOMIZE USR #{drawline[:start]}
    #          PAUSE 0: GO TO 10
    #     9998 STOP
    #     9999 CLEAR #{drawline.org-1}: LOAD ""CODE: RUN
    #    END
    #    program.save_tap 'drawline', line: 9999
    #    drawline.save_tap 'drawline', append: true
    #
    class Draw
      module Constants
        PLOT_FX_NOP  = 0x00
        PLOT_FX_NONE = 0x7E
        PLOT_FX_AND  = 0xA6
        PLOT_FX_XOR  = 0xAE
        PLOT_FX_OR   = 0xB6
      end

      ##
      #  ==ZXLib::Gfx::Draw macros for drawing lines and plotting pixels on the ZX Spectrum.
      module Macros
        include ZXLib::Gfx::Macros
        include Z80::MathInt::Macros
        include Draw::Constants
        ##
        # A convenient method to build drawing subroutines.
        # Returns a label with members including the routines and preshifted pixels data.
        def make_draw_line_subroutines(make_line:true, make_line_over:true, make_line_inversed:true, make_lines_to:true, scraddr:0x4000, check_oos:true)
          isolate do
            if make_line
              line_to           prepare_args_draw_line_to if make_lines_to
              line              draw_line(preshifted_pixel, preshifted_pixel_cover_left, preshifted_pixel_cover_right, fx: :or, pixel_type: :pixel, scraddr:scraddr, check_oos:check_oos, end_with: :ret)
            end
            if make_line_over
              line_over_to      prepare_args_draw_line_to if make_lines_to
              line_over         draw_line(preshifted_pixel, preshifted_pixel_cover_left, preshifted_pixel_cover_right, fx: :xor, pixel_type: :pixel, scraddr:scraddr, check_oos:check_oos, end_with: :ret)
            end
            if make_line_inversed
              line_inversed_to  prepare_args_draw_line_to if make_lines_to
              line_inversed     draw_line(preshifted_inversed_pixel, preshifted_inversed_pixel_cover_left, preshifted_inversed_pixel_cover_right, fx: :and, pixel_type: :mask, scraddr:scraddr, check_oos:check_oos, end_with: :ret)
            end

            preshifted_pixel                      preshifted_pixel_mask_data(:pixel)
            preshifted_inversed_pixel             preshifted_pixel_mask_data(:inversed_pixel)
            preshifted_pixel_cover_left           preshifted_pixel_mask_data(:pixel_cover_left)
            preshifted_inversed_pixel_cover_left  preshifted_pixel_mask_data(:inversed_pixel_cover_left)
            preshifted_pixel_cover_right          preshifted_pixel_mask_data(:pixel_cover_right)
            preshifted_inversed_pixel_cover_right preshifted_pixel_mask_data(:inversed_pixel_cover_right)
          end
        end
        ##
        # The common pixel mask data, precalculated to be used with other drawing routines.
        #
        # +data_type+:
        #
        #     :pixel                      10000000, 01000000 ... 00001000 ... 00000001
        #     :inversed_pixel             01111111, 10111111 ... 11110111 ... 11111110
        #     :pixel_cover_left           10000000, 11000000 ... 11111000 ... 11111111
        #     :inversed_pixel_cover_left  01111111, 00111111 ... 00000111 ... 00000000
        #     :pixel_cover_right          11111111, 01111111 ... 00001111 ... 00000001
        #     :inversed_pixel_cover_right 00000000, 10000000 ... 11110000 ... 11111110
        #
        # The data is being aligned to 8 bytes.
        def preshifted_pixel_mask_data(data_type)
                        org   align: 8
          case data_type
          when :pixel   # 10000000 01000000 ... 00001000 ... 00000001
                        bytes (0..7).map{|x|   0x80 >> x }
          when :inversed_pixel
                        # 01111111 10111111 ... 11110111 ... 11111110
                        bytes (0..7).map{|x| ~(0x80 >> x) }
          when :pixel_cover_left
                        # 10000000 11000000 ... 11111000 ... 11111111
                        bytes (0..7).map{|x| ~(0x7F >> x) }
          when :inversed_pixel_cover_left
                        # 01111111 00111111 ... 00000111 ... 00000000
                        bytes (0..7).map{|x|   0x7F >> x }
          when :pixel_cover_right
                        # 11111111 01111111 ... 00001111 ... 00000001
                        bytes (0..7).map{|x|   0xFF >> x }
          when :inversed_pixel_cover_right
                        # 00000000 10000000 ... 11110000 ... 11111110
                        bytes (0..7).map{|x| ~(0xFF >> x) }
          else
            raise ArgumentError, "should be one of: :pixel, :mask, :pixel_cover, :mask_cover"
          end
        end
        ##
        # The plot pixel routine.
        #
        # * +x+:: The input register: horizontal-coordinate.
        # * +y+:: The input register: vertical-coordinate.
        # * +preshift+:: a label from the preshifted_pixel_mask_data called with a +:pixel+ or a +:inversed_pixel+ argument.
        # * +fx+:: How to mix pixel with the screen: +:or+, +:xor+, +:and+, +:nop+, +:none+, +:write+, +:skip+.
        # * +with_attributes+:: Optionally writes to the screen attributes if not +false+.
        #   Pass +:overwrite+ to ignore the +color_mask+.
        # * +color_attr+:: an address, an immediate value or a 8-bit half of +ix+ or +iy+ register.
        # * +color_mask+:: an address, an immediate value or a 8-bit half of +ix+ or +iy+ register.
        # * +scraddr+:: a screen memory address as an integer, must be a multiple of 0x2000
        #
        # Unless +with_attributes:+ is +:overwrite+: +memory+ = (+memory+ & +color_mask+) | +color_attr+.
        #
        # Modifies: +af+, +bc+, +de+, +hl+.
        #
        # T-states::
        #
        # * without attributes:: 144/147 depending on +scraddr+ (144 for default 0x4000)
        # * overwrite attributes:: +(44/49/54/60) for +color_attr+ one of: n/ixh/[ address ]/[ix+n]
        # * merge attributes:: +62 +(0/1/6/12) for +color_mask+ one of: n/ixh/[ address ]/[ix+n];
        #                      +(0/1/14/20) for +color_attr+ one of: n/ixh/[ address ]/[ix+n]
        #
        # Unless +fx+ was given :write, the internal label: +plot_fx+ may be used to modify the fx function:
        #
        #   fx    value  Z80 mnemonic
        #   :or   0xB6   OR (HL)
        #   :xor  0xAE   XOR (HL)
        #   :and  0xA6   AND (HL)
        #   :none 0x7E   LD A,(HL) - may be used to temporary disable modifying screen memory
        #   :nop  0x00   NOP       - effect is the same as :write but gives possibility to modify fx
        #
        # Internal labels: +attr_a+ + +1+, +mask_a+ + +1+ may be used to modify +color_attr+ or +color_mask+ if
        # the arguments were given immediate numbers.
        #
        # The internal label +preshift_a+ + +1+ may be used to change the +preshift+ address.
        def plot_pixel(x, y, preshift, fx: :or, with_attributes:false, color_attr:ixl, color_mask:ixh, scraddr:0x4000)
          isolate do
                        xytoscr y, x, ah:h, al:l, s:c, t:b, scraddr:scraddr
            preshift_a  ld   de, preshift
            select(preshift & 7, &:zero?).then do |_|
                        ld   a, c
                        add  e
                        ld   e, a
                        ld   a, [de]
            end.else do
                raise ArgumentError, "preshift must be aligned to 8"
            end
            case fx
            when Integer
            plot_fx     db   fx
            when :or
            plot_fx     ora  [hl]
            when :xor
            plot_fx     xor  [hl]
            when :and
            plot_fx     anda [hl]
            when :none
            plot_fx     ld   a, [hl]
            when :nop
            plot_fx     nop
            when :write, :skip
            else
              raise ArgumentError, "unknown fx value"
            end
                        ld   [hl], a unless fx == :skip

            if with_attributes
                        scrtoattr h, o:h, scraddr:0x4000
              if with_attributes == :overwrite
                if immediate?(color_attr) && !pointer?(color_attr)
            attr_a      ld   [hl], color_attr
                else
                        ld   a, color_attr
                        ld   [hl], a
                end
              else # with_attributes <> :overwrite
            mask_a      ld   a, color_mask
                        anda [hl]
                if pointer?(color_attr)
                        ld   c, a
                        ld   a, color_attr
                        ora  c
                else
            attr_a      ora  color_attr
                end
                        ld   [hl], a

              end
            end # with_attributes
          end
        end
        ##
        # Prepares arguments for the draw_line routine based on two coordinates.
        #
        # Registers +hl+ and +de+ should contain the +yx+ coordinates of the 2 points that the line
        # should be drawn between.
        #
        # Modifies: +af+, +bc+, +de+, +hl+.
        #
        # As a result, the following registers will be loaded with:
        # * +hl+: the starting +yx+ point.
        # * +bc+: a sign and a +dx+ value.
        # * +de+: a sign and a +dy+ value.
        def prepare_args_draw_line_to
          y0, x0 = h, l
          y1, x1 = d, e
          isolate do
                          ld   a, x1
                          sub  x0                # x1 - x0
                          jr   NC, no_swap       # x0 <= x1
                          ex   de, hl
                          neg                    # a: abs(dx)
            no_swap       ld   b, 0              # b: sdx=0
                          ld   c, a              # c: dx
                          ld   a, y1
                          sub  y0                # y1 - y0
                          ld   d, b              # d: sdy=0
                          jr   NC, no_neg
                          dec  d                 # d: sdy=-1
                          neg
            no_neg        ld   e, a              # e: dy
          end
        end
        ##
        # The "draw line" routine.
        #
        # Expects registers:
        # * +hl+: the starting (+yx+) point.
        # * +bc+: a sign in +b+ and an absolute value of +dx+ in +c+.
        # * +de+: a sign in +d+ and an absolute value of +dy+ in +e+.
        #
        # Arguments:
        # * +preshift_pixel+:: a label from the preshifted_pixel_mask_data called with a +:pixel+ or a +:inversed_pixel+ argument.
        # * +preshift_cov_lt+:: a label from the preshifted_pixel_mask_data called with a +:pixel_cover_left+ or a +:inversed_pixel_cover_left+ argument.
        # * +preshift_cov_rt+:: a label from the preshifted_pixel_mask_data called with a +:pixel_cover_right+ or a +:inversed_pixel_cover_right+ argument.
        #
        # Options:
        # * +fx+:: How to mix pixels with the screen: +:or+, +:xor+, +:and+, +:nop+, +:none+, +:write+.
        # * +pixel_type+:: How to interpret the preshifted data as a +:pixel+ or a +:mask+.
        #                  For inversed preshifted data use +:mask+ (e.g. to be used with +fx+=+:and+).
        # * +scraddr+:: a screen memory address as an integer, must be a multiple of 0x2000.
        # * +check_oos+:: Checks if the line is about to be drawn out of the screen area and stops.
        # * +end_with+:: What to do when drawing is over: +:eoc+, +:ret+ or a label to jump to.
        #
        # Modifies: +af+, +bc+, +de+, +hl+, +af'+, +bc'+, +de'+, +hl'+, +ix+, stack: max 2 bytes.
        #
        # The sign should be a boolean flag: 0 for a positive sign and anything else for a negative sign.
        # The +dx+ and +dy+ values must never be a 2'complement.
        def draw_line(preshift_pixel, preshift_cov_lt, preshift_cov_rt, fx: :or, pixel_type: :pixel, scraddr:0x4000, check_oos:true, end_with: :eoc)
          isolate do |eoc|
            end_with = eoc if end_with == :eoc
                          xor  a                  # if sdx < 0 then x = x - abs(dx), y = y - dy
                          cp   b                  # 0 - sdx
                          jr   NC, dx_pos         # dx >= 0
                          ld   a, l               # a: x
                          sub  c                  # a: x - abs(dx)
                          jr   C, ooscr_error if check_oos # x - abs(dx) < 0
                          ld   l, a               # if sdy < 0 then y = y - abs(dy) else y = y + abs(dy)
                          ld   a, d
                          cpl
                          anda a                  # Z:sdx<>0, NZ:sdx=0
                          ld   d, a               # d: !sdx
                          ld   a, h               # a: y
                          jr   NZ, dy_pos         # sdx >= 0
                          sub  e                  # y = y - abs(dy)
                          jr   NC, set_y          # y + abs(dy) >= 0
              if check_oos
                ooscr_error label
                case end_with
                when :ret
                            ret
                else
                  raise ArgumentError, "end_with should be: :eoc, :ret or an address" unless address?(end_with)
                            jp   end_with
                end
              end
              dy_pos      add  e                  # y = y + abs(dy)
                          jr   C, ooscr_error if check_oos # y + abs(dy) > 255
              set_y       ld   h, a               # h: y + dy
              dx_pos      label
              if check_oos
                          ld   a, h
                          cp   192
                          jr   NC, ooscr_error
              end
                          ld   a, d               # a: sdy
                          ex   af, af             # a': sdy
                          ld   b, e               # b: dy we can ignore sdx from now on
                          xytoscr h, l, ah:h, al:l, s:e, t:d, scraddr:scraddr

                          ld   a, b               # dy
                          sub  c                  # dy - dx
                          jp   NC, dy_gte_dx      # dy >= dx 
                                                  # dx > dy
                          ld   a, c
                          cp   8
                          jr   C, dx_gt_dy        # dx < 8
                          ld   a, b               # dy
                          cp   64
                          jr   NC, dx_gt_dy       # dy >= 64
                          add  a                  # 2dy
                          add  a                  # 4dy
                          cp   c                  # 4dy - dx
                          jp   C, dx_gt_4dy       # dx > 4dy

              dx_gt_dy    ex   af, af             # a: sdy
                          anda a
                          ld   a, e               # a: xshift
                          jp   Z, rt_dn
              rt_up       draw_line_dx_gt_dy(preshift_cov_lt, direction: :up, fx:fx, pixel_type:pixel_type, scraddr:scraddr, check_oos:check_oos, end_with:end_with)
              rt_dn       draw_line_dx_gt_dy(preshift_cov_lt, direction: :down, fx:fx, pixel_type:pixel_type, scraddr:scraddr, check_oos:check_oos, end_with:end_with)
              dx_gt_4dy   ex   af, af             # a: sdy
                          anda a
                          ld   a, e               # a: xshift
                          jp   Z, rt4_dn
              rt4_up      draw_line_dx_gt_4dy(preshift_cov_rt, direction: :up, fx:fx, pixel_type:pixel_type, scraddr:scraddr, check_oos:check_oos, end_with:end_with)
              rt4_dn      draw_line_dx_gt_4dy(preshift_cov_rt, direction: :down, fx:fx, pixel_type:pixel_type, scraddr:scraddr, check_oos:check_oos, end_with:end_with)
              dy_gte_dx   sub  b
                          jp   Z, up_dn           # dx = 0
                          ex   af, af             # a: sdy
                          anda a
                          ld   a, e               # a: xshift
                          jp   Z, dn_rt
              up_rt       draw_line_dy_gte_dx(preshift_pixel, direction: :up_right, fx:fx, pixel_type:pixel_type, scraddr:scraddr, check_oos:check_oos, end_with:end_with)
              dn_rt       draw_line_dy_gte_dx(preshift_pixel, direction: :down_right, fx:fx, pixel_type:pixel_type, scraddr:scraddr, check_oos:check_oos, end_with:end_with)
              up_dn       ex   af, af             # a: sdy
                          anda a
                          ld   a, e               # a: xshift
                          jp   Z, dn
              up          draw_line_vertical(preshift_pixel, direction: :up, fx: fx, scraddr:scraddr, check_oos:check_oos, end_with:end_with)
              end_with = :eoc if end_with == eoc
              dn          draw_line_vertical(preshift_pixel, direction: :down, fx: fx, scraddr:scraddr, check_oos:check_oos, end_with:end_with)
          end
        end
        ##
        # Creates a routine optimized for drawing vertical lines.
        #
        # Expects registers:
        # * +hl+: the screen memory address at which the line should begin.
        # * +a+: the +x+ coordinate modulo 8.
        # * +b+: an absolute value of the delta +y+.
        #
        # Arguments:
        # * +preshift+:: a label from the preshifted_pixel_mask_data called with a +:pixel+ or a +:inversed_pixel+ argument.
        #
        # Options:
        # * +direction+:: one of the following symbols: +:down+, +:up+.
        # * +fx+:: How to mix pixels with the screen: +:or+, +:xor+, +:and+, +:nop+, +:none+, +:write+.
        # * +scraddr+:: a screen memory address as an integer, must be a multiple of 0x2000.
        # * +check_oos+:: Checks if the line is about to be drawn out of the screen area and stops.
        # * +end_with+:: What to do when drawing is over: +:eoc+, +:ret+ or a label to jump to.
        #
        # Modifies: +af+, +af'+, +b+, +de+, +hl+.
        def draw_line_vertical(preshift, direction: :down, fx: :or, scraddr:0x4000, check_oos:true, end_with: :eoc)
          dy = b
          isolate do |eoc|
            preshift_a    ld   de, preshift # 10000000 01000000 00100000 00010000 ... 00000001
            select(preshift & 7, &:zero?).then do |_|
                          add  e
                          ld   e, a
                          ld   a, [de]      # a: pixel mask
                          ld   e, a         # e: pixel mask
            end.else do
                raise ArgumentError, "preshift must be aligned to 8"
            end
                          ld   d, dy        # d: dy
                          ld   a, h         # calculate counter based on screen address modulo 8
            case direction
            when :down
                          anda 0b11111000   # (h & 0b11111000)
                          sub  h            # (h & 0b11111000) - h % 8
                          add  8            # 8 - h % 8
            when :up
                          anda 0b00000111   # (h & 0b00011111)
                          inc  a            # (h & 0b00011111) + 1
            else
              raise ArgumentError, "invalid direction argument"
            end
                          ld   b, a         # b: counter: 8 - h % 8
                          ld   a, d         # a: dy
                          sub  b            # a: dy - counter
                          jr   NC, fits1
                          ld   b, d         # b: counter = dy
                          inc  b            # b: counter = dy + 1
            fits1         ex   af, af       # a': rest of dy, CF=1 ? the last lines
            case direction
            when :down
                          dec  h
            loop1         inc  h            # screen down 1 line
            when :up
                          inc  h
            loop1         dec  h            # screen up 1 line
            end
            loop2         label
            unless fx == :write
                          ld   a, e         # pixel mask
            end
            case fx
            when Integer
            plot_fx       db   fx
            when :or
            plot_fx       ora  [hl]
            when :xor
            plot_fx       xor  [hl]
            when :and
            plot_fx       anda [hl]
            when :none
            plot_fx       ld   a, [hl]
            when :nop
            plot_fx       nop
            when :write
            else
              raise ArgumentError, "unknown fx value"
            end
            if fx == :write
                          ld   [hl], e
            else
                          ld   [hl], a
            end
                          djnz loop1

            next_8row     ex   af, af         # a: rest of dy, CF=1 ? was the last lines
            case end_with
            when :eoc
                          jr   C, eoc       # CF=1 ? over
            when :ret
                          ret  C            # CF=1 ? over
            else
                          jp   C, end_with  # CF=1 ? over
            end
                          ld   b, 8         # b: counter: next 8 lines
                          sub  b            # a: rest of dy -= 8
                          jr   NC, next_row
                          add  b            # restore rest of dy
                          ld   b, a         # counter = rest of dy
                          inc  b            # counter = rest of dy + 1
            next_row      ex   af, af       # a': rest of dy, CF=1 ? the last lines
                          ld   a, l         # next screen row
            case direction
            when :down
                          add  0x20
                          ld   l, a
                          jr   C, loop1 unless check_oos
                          ld   a, h
                          jr   C, oos_ck if check_oos
                          anda 0xf8         # h: ssspp000 l: rrrccccc
            when :up
                          sub  0x20
                          ld   l, a
                          jr   C, loop1 unless check_oos
                          ld   a, h
                          jr   C, oos_ck if check_oos
                          ora  0x07         # h: ssspp111 l: rrrccccc
            end
                          ld   h, a
                          jp   loop2

            if check_oos
              oos_ck      label
              case direction
              when :down
                          cp   ((scraddr >> 8)|0x18)-1
                          jr   C, loop1
              when :up
                          cp   (scraddr >> 8)+1
                          jr   NC, loop1
              end
              case end_with
              when :eoc
              when :ret
                          ret
              else
                          jp   end_with
              end
            end

          end
        end
        ##
        # Creates a routine for drawing lines with the delta +y+ larger than or equal to the delta +x+.
        #
        # Expects registers:
        # * +hl+: the screen memory address at which the line should begin.
        # * +a+: the +x+ coordinate modulo 8.
        # * +b+: an absolute value of the delta +y+.
        # * +c+: an absolute value of the delta +x+.
        #
        # NOTE:: the delta +y+ must be greater than or equal to the delta +x+.
        #
        # Arguments:
        # * +preshift+:: a label from the preshifted_pixel_mask_data called with a +:pixel+ or a +:inversed_pixel+ argument.
        #
        # Options:
        # * +direction+:: one of the following symbols: +:down_right+, +:up_right+, +:down_left+, +:up_left+.
        # * +fx+:: How to mix pixels with the screen: +:or+, +:xor+, +:and+, +:nop+, +:none+, +:write+.
        # * +pixel_type+:: How to interpret the preshifted data as a +:pixel+ or a +:mask+.
        #                  For inversed preshifted data use +:mask+ (e.g. to be used with +fx+=+:and+).
        # * +scraddr+:: a screen memory address as an integer, must be a multiple of 0x2000.
        # * +check_oos+:: Checks if the line is about to be drawn out of the screen area and stops.
        # * +end_with+:: What to do when drawing is over: +:eoc+, +:ret+ or a label to jump to.
        #
        # Uses: +af+, +af'+, +bc+, +de+, +hl+, stack: max 2 bytes, preserves: +c+.
        def draw_line_dy_gte_dx(preshift, direction: :down_right, fx: :or, pixel_type: :pixel, scraddr:0x4000, check_oos:true, end_with: :eoc)
          dy = b
          dx = c
          isolate do |eoc|
            preshift_a    ld   de, preshift # 10000000 01000000 00100000 00010000 ... 00000001
            select(preshift & 7, &:zero?).then do |_|
                          add  e
                          ld   e, a
                          ld   a, [de]    # a: pixel mask
                          ld   e, a       # e: pixel mask
            end.else do
                raise ArgumentError, "preshift must be aligned to 8"
            end
                          ld   a, dy
                          rra             # a: dy / 2
                          ex   af, af     # 'a: acc dx
                          ld   d, dy      # d: dy
                          ld   a, h       # calculate counter based on screen address modulo 8
            case direction
            when :down_left, :down_right
                          anda 0b11111000 # (h & 0b11111000)
                          sub  h          # (h & 0b11111000) - h % 8
                          add  8          # 8 - h % 8
            when :up_left, :up_right
                          anda 0b00000111 # (h & 0b00011111)
                          inc  a          # (h & 0b00011111) + 1
            else
              raise ArgumentError, "invalid direction argument"
            end
                          ld   b, a       # b: counter: 8 - h % 8
                          ld   a, d       # a: dy
                          sub  b          # a: dy - counter
                          push af         # (sp): a: rest of dy, CF=1 ? the last lines
                          jr   NC, fits1
                          ld   b, d       # b: counter = dy
                          inc  b          # b: counter = dy + 1
            case direction
            when :down_left, :down_right
            fits1         dec  h
            loop1         inc  h          # screen down 1 line
            when :up_left, :up_right
            fits1         inc  h
            loop1         dec  h          # screen up 1 line
            end
            loop2         label
            unless fx == :write
                          ld   a, e       # pixel mask
            end
            case fx
            when Integer
            plot_fx       db   fx
            when :or
            plot_fx       ora  [hl]
            when :xor
            plot_fx       xor  [hl]
            when :and
            plot_fx       anda [hl]
            when :none
            plot_fx       ld   a, [hl]
            when :nop
            plot_fx       nop
            when :write
            else
              raise ArgumentError, "unknown fx value"
            end
            if fx == :write
                          ld   [hl], e
            else
                          ld   [hl], a
            end
                          ex   af, af     # a: acc dx
                          add  dx         # a+= dx
                          jr   C, vh_step
                          cp   d          # a >= d
                          jr   C, no_vh   # a < d
            vh_step       sub  d          # a -= dy
            case direction
            when :down_right, :up_right
                          rrc  e          # mask >> 1
            when :down_left, :up_left
                          rlc  e          # mask << 1
            else
              raise ArgumentError, "direction should be :left or :right"
            end
            case pixel_type
            when :pixel
              pixel_cond  jr   C, next_col
            when :mask
              pixel_cond  jr   NC, next_col
            else
              raise ArgumentError, "pixel_type should be :pixel or :mask"
            end
            no_vh         ex   af, af     # a': acc(dx)
                          djnz loop1

            next_8row     pop  af         # (sp): a: rest of dy, CF=1 ? was the last lines
            case end_with
            when :eoc
                          jr   C, eoc       # CF=1 ? over
            when :ret
                          ret  C            # CF=1 ? over
            else
                          jp   C, end_with  # CF=1 ? over
            end
                          ld   b, 8       # b: counter: next 8 lines
                          sub  b          # a: rest of dy -= 8
                          push af         # (sp): a: rest of dy, CF=1 ? the last lines
                          jr   NC, next_row
                          add  b          # restore rest of dy
                          ld   b, a       # counter = rest of dy
                          inc  b          # counter = rest of dy + 1
            next_row      ld   a, l       # next screen row
            case direction
            when :down_left, :down_right
                          add  0x20
                          ld   l, a
                          jr   C, loop1 unless check_oos
                          ld   a, h
                          jr   C, oos_ck if check_oos
                          anda 0xf8       # h: ssspp000 l: rrrccccc
            when :up_left, :up_right
                          sub  0x20
                          ld   l, a
                          jr   C, loop1 unless check_oos
                          ld   a, h
                          jr   C, oos_ck if check_oos
                          ora  0x07       # h: ssspp111 l: rrrccccc
            end
                          ld   h, a
                          jp   loop2

            next_col      ex   af, af     # a': acc(dx)
            case direction
            when :down_right, :up_right
                          inc  l
              if check_oos
                          ld   a, l
                          anda 0b00011111
                          jr   Z, popeoc  # out of screen (column-wise)
              end
            when :down_left, :up_left
              if check_oos
                          ld   a, l
                          anda 0b00011111
                          jr   Z, popeoc  # out of screen (column-wise)
              end
                          dec  l
            end
                          djnz loop1
                          jp   next_8row

            if check_oos
              oos_ck      label
              case direction
              when :down_left, :down_right
                          cp   ((scraddr >> 8)|0x18)-1
                          jr   C, loop1
              when :up_left, :up_right
                          cp   (scraddr >> 8)+1
                          jr   NC, loop1
              end

              popeoc      pop  af
              case end_with
              when :eoc
              when :ret
                          ret
              else
                          jp   end_with
              end
            end
          end
        end
        ##
        # Creates a routine for drawing lines with the delta +x+ larger than the delta +y+.
        #
        # Expects registers:
        # * +hl+: the screen memory address at which the line should begin.
        # * +a+: the +x+ coordinate modulo 8.
        # * +b+: an absolute value of the delta +y+.
        # * +c+: an absolute value of the delta +x+.
        #
        # NOTE:: the delta +x+ must be greater than the delta +y+.
        #
        # Arguments:
        # * +preshift+:: a label from the preshifted_pixel_mask_data called with a +:pixel_cover_left+ or a +:inversed_pixel_cover_left+ argument.
        #
        # Options:
        # * +direction+:: one of the following symbols: +:down+, +:up+. The line is always drawn to the right.
        # * +fx+:: How to mix pixels with the screen: +:or+, +:xor+, +:and+, +:nop+, +:none+, +:write+.
        # * +pixel_type+:: How to interpret the preshifted data as a +:pixel+ or a +:mask+.
        #                  For inversed preshifted data use +:mask+ (e.g. to be used with +fx+=+:and+).
        # * +scraddr+:: a screen memory address as an integer, must be a multiple of 0x2000.
        # * +check_oos+:: Checks if the line is about to be drawn out of the screen area and stops.
        # * +end_with+:: What to do when drawing is over: +:eoc+, +:ret+ or a label to jump to.
        #
        # Uses: +af+, +af'+, +bc+, +de+, +hl+, +ix+, preserves: +c+.
        def draw_line_dx_gt_dy(preshift, direction: :down, fx: :or, pixel_type: :pixel, scraddr:0x4000, check_oos:true, end_with: :eoc)
          linescnt = dy = b
          dx = c
          yt = d
          lmask = e
          xrest = ixl
          colsleft = ixh
          combine_screen_pixels = proc do
            case fx
            when Integer
                          db   fx
            when :or
                          ora  [hl]
            when :xor
                          xor  [hl]
            when :and
                          anda [hl]
            when :none
                          ld   a, [hl]
            when :nop
                          nop
            when :write
                          label
            else
              raise ArgumentError, "unknown fx value"
            end
          end
          isolate do |eoc|
            bcheck = if check_oos
              case end_with
              when :eoc
                eoc
              when :ret
                true
              else
                end_with
              end
            else
              false
            end
            preshift_a    ld   de, preshift   # 10000000 11000000 ... 11111000 ... 11111111
            select(preshift & 7, &:zero?).then do |_|
                          add  e
                          ld   e, a
                          ld   a, [de]        # a: line mask
            end.else do
                raise ArgumentError, "preshift must be aligned to 8"
            end
                          ld   yt, dy         # yt: dy
                          ex   af, af         # 'a: lmask

                          ld   a, e           # reclaim xshift
                          anda 0x07           # assume preshift is 8 byte aligned

                          ex   af, af         # a: lmask, a': xshift
                          ld   lmask, a       # lmask
            case pixel_type
            when :pixel
                          add  a              # lmask << 1
                          cpl                 # rmask = ~(rmask<<1)
            when :mask
                          cpl                 # ~rmask
                          add  a              # rmask = (~rmask)<<1
            else
              raise ArgumentError, "pixel_type should be :pixel or :mask"
            end
                          ex   af, af         # 'a: rmask, a: xshift

                          add  dx             # a: xshift + dx
                          ld   b, a           # b: xshift + dx
            if check_oos
                          jr   C, dx_overflow # xshift + dx > 255
            end
                          anda 0x07
                          ld   xrest, a       # xrest: (xshift + dx) & 0x07
                          xor  b              # a: (xshift + dx) & 0xF8, ZF: 1 if first=last column, CF: 0

                          jr   Z, iter_last0
                          3.times {rrca}      # a: ((xshift + dx) & 0xF8)/8, sets CF=0
                          ld   colsleft, a    # colsleft: (xshift + dx)/8 (1..31)
            if check_oos
                          add  l              # a: colsleft + trash|column
                          anda 0b11100000
                          xor  l              # a: should be 000lllll if colsleft + column < 32
                          cp   32
                          jr   C, fits_x
              unfit_x     anda 0b00011111     # a: start column
                          cpl
                          add  32             # a: 31 - column
                          jr   Z, iter_last1  # 31 - column = 0
                          ld   colsleft, a    # colsleft: 31 - column
                          ld   xrest, 7
              fits_x      label
            end

                          ld   a, h           # calculate linescnt based on screen address modulo 8
            case direction
            when :down
                          anda 0b11111000     # (h & 0b11111000)
                          sub  h              # (h & 0b11111000) - h % 8
                          add  8              # 8 - h % 8
            when :up
                          anda 0b00000111     # (h & 0b00011111)
                          inc  a              # (h & 0b00011111) + 1
            else
              raise ArgumentError, "direction should be :up or :down"
            end
                          ld   linescnt, a    # linescnt: 8 - h % 8

                          ld   a, dx
                          srl  a              # CF: 0, a: dx / 2
                          jp   maskloop1

            if check_oos
              dx_overflow ld   a, l           # a: trash + column
                          jr   unfit_x
            end

            maskloop2     ld   a, lmask       # line has advanced
            case pixel_type
            when :pixel
                          xor  0xFF           # a: rmask = ~lmask
                          jr   Z, next_col1   # diagonal: next line and next column
            when :mask
                          anda a
                          jr   Z, next_col1   # diagonal: next line and next column
                          cpl                 # a: rmask = ~lmask
            end
                          ex   af, af         # a': rmask, a: acc(dy)
            maskloop0     label
            case pixel_type
            when :pixel
                          sra  lmask          # 11000000 >> CF : lmask: 11100000 CF: 0
                          jr   C, next_col0   # 11111111 CF: 1 (next column)
            when :mask
                          srl  lmask          # 00111111 >> CF : lmask: 00011111 CF: 1
                          jr   NC, next_col0  # 00000000 CF: 0 (next column)
            end
            maskloop1     add  yt             # a+= dy
                          jr   C, vh_step
                          cp   dx             # a >= dx
                          jr   C, maskloop0   # a < dx

            vh_step       sub  dx
                          ex   af, af         # a: rmask, a': acc(dy)
            case pixel_type
            when :pixel
                          anda lmask
            when :mask
                          ora  lmask
            end
            plot_fx1      combine_screen_pixels[]
                          ld   [hl], a
            ns do
              case direction
              when :down
                          inc  h
                                              # dec  linescnt
                          djnz maskloop2      # jr   NZ, maskloop2
                          ld   linescnt, 8    # reset 8-lines counter
                          ld   a, l           # next screen row
                          add  0x20
                          ld   l, a
                          jr   C, maskloop2 unless check_oos
                          ld   a, h
                          jr   C, oos_ck if check_oos
                          sub  0x08
                          ld   h, a
                          jp   maskloop2
                if check_oos
                  oos_ck  cp   (scraddr >> 8)|0x18
                          jr   C, maskloop2
                end
              when :up
                          dec  h
                                              # dec  linescnt
                          djnz maskloop2      # jr   NZ, maskloop2
                          ld   linescnt, 8    # reset 8-lines counter
                          ld   a, l           # next screen row
                          sub  0x20
                          ld   l, a
                          jr   C, maskloop2 unless check_oos
                          ld   a, h
                          jr   C, oos_ck if check_oos
                          add  0x08
                          ld   h, a
                          jp   maskloop2
                if check_oos
                  oos_ck  cp   (scraddr >> 8)
                          jr   NC, maskloop2
                end
              end
            end
            case end_with
            when :eoc
                          jp   eoc            # below bottom line
            when :ret
                          ret                 # below bottom line
            else
                          jp   end_with       # below bottom line
            end

            next_col0     ex   af, af         # a: rmask, a': acc(dy)
                                              # no need to AND lmask, lmask=0xFF (as mask: 0x00)
            plot_fx2      combine_screen_pixels[]
                          ld   [hl], a

            next_col1     inc  l
                          dec  colsleft       # check if next column is last
                          ld   a, lmask       # rmask: 0xFF (as mask: 0x00)
            case pixel_type
            when :pixel
                          ld   lmask, 0x80    # lmask: 10000000
            when :mask
                          ld   lmask, 0x7F    # lmask: 01111111
            end
                          jr   Z, iter_last2  # colsleft == 0
                          ex   af, af         # a': rmask, a: acc(dy)
                          jp   maskloop1

            iter_last0    ld   b, dx          # 0..7
                          inc  b              # b=1..8
                          ld   a, dx
                          rra                 # CF:0, a: dx / 2
                          jp   masklooplast1
            if check_oos
              iter_last1  ld   a, b           # dx + xshift
                          sub  dx
                          cpl
                          add  9              # 8 - xshift
                          ld   b, a
                          ld   a, dx
                          srl  a              # CF:0, a: dx / 2
                          jp   masklooplast1
            end
            iter_last2    ex   af, af         # a': rmask, a: acc(dy)
                          ld   b, xrest       # 0..7
                          inc  b              # 1..8
                          jp   masklooplast1

            vh_step_l     dec  b
                          jr   Z, last_write
                          sub  dx
                          ex   af, af         # a': acc(dy)
            case pixel_type
            when :pixel
                          anda lmask
            when :mask
                          ora  lmask
            end
            plot_fx3      combine_screen_pixels[]
                          ld   [hl], a
            case direction
            when :down
                        nextline h, l, bcheck, scraddr:scraddr
            when :up
                        prevline h, l, bcheck, scraddr:scraddr
            end

            makslooplast2 ld   a, lmask
                          cpl                 # a: rmask
                          ex   af, af         # a': rmask, a: acc(dy)
            masklooplast0 sra  lmask          # 1100000 >> CF : lmask: 111000000 CF: 0
            masklooplast1 add  yt             # a+= dy
                          jr   C, vh_step_l
                          cp   dx             # a >= dx
                          jr   NC, vh_step_l  # a < dx
                          djnz masklooplast0
            last_write    ex   af, af         # a: rmask, a': acc(dy)
            case pixel_type
            when :pixel
                          anda lmask
            when :mask
                          ora  lmask
            end
            plot_fx4      combine_screen_pixels[]
                          ld   [hl], a
            case end_with
            when :eoc
            when :ret
                          ret
            else
                          jp   end_with       # below bottom line
            end
          end
        end
        ##
        # Creates a routine for drawing lines with the delta +x+ 4 times larger than the delta +y+.
        #
        # Expects registers:
        # * +hl+: the screen memory address at which the line should begin.
        # * +a+: the +x+ coordinate modulo 8.
        # * +b+: an absolute value of the delta +y+.
        # * +c+: an absolute value of the delta +x+.
        #
        # NOTE:: the delta +x+ must be greater than the delta +y+.
        #        Performes best when the delta +x+ is at least 4 times the delta +y+.
        #
        # Arguments:
        # * +preshift+:: a label from the preshifted_pixel_mask_data called with a +:pixel_cover_right+ or a +:inversed_pixel_cover_right+ argument.
        #
        # Options:
        # * +direction+:: one of the following symbols: +:down+, +:up+. The line is always drawn to the right.
        # * +fx+:: How to mix pixels with the screen: +:or+, +:xor+, +:and+, +:nop+, +:none+, +:write+.
        # * +pixel_type+:: How to interpret the preshifted data as a +:pixel+ or a +:mask+.
        #                  For inversed preshifted data use +:mask+ (e.g. to be used with +fx+=+:and+).
        # * +scraddr+:: a screen memory address as an integer, must be a multiple of 0x2000.
        # * +check_oos+:: Checks if the line is about to be drawn out of the screen area and stops.
        # * +end_with+:: What to do when drawing is over: +:eoc+, +:ret+ or a label to jump to.
        #
        # Uses: +af+, +bc+, +de+, +hl+, +af'+, +bc'+, +de'+, +hl'+, +ix+, preserves: +c+.
        def draw_line_dx_gt_4dy(preshift, direction: :down, fx: :or, pixel_type: :pixel, scraddr:0x4000, check_oos:true, end_with: :eoc)
          dy = px = size = b
          c8 = dx = c
          quot = ixh
          dyt =  ixl
          tmp = quot_dy = d
          pmask = xcount = e
          combine_screen_pixels = proc do
            case fx
            when Integer
                          db   fx
            when :or
                          ora  [hl]
            when :xor
                          xor  [hl]
            when :and
                          anda [hl]
            when :none
                          ld   a, [hl]
            when :nop
                          nop
            when :write
                          label
            else
              raise ArgumentError, "unknown fx value"
            end
          end
          isolate do |eoc|
            if check_oos
              bcheck = case end_with
              when :eoc
                eoc
              when :ret
                true
              else
                end_with
              end
                          ex   af, af
                          ld   a, l
                          3.times { add  a }
                          ld   xcount, a
                          ex   af, af
            else
              bcheck = false
            end
                          exx
                          ld   c8, 8
                          ld   tmp, 0xF8
                          ld   px, a             # px': sss -> x
            select(preshift & 7, &:zero?).then do |_|
                          ld   hl, preshift
                          add  l
                          ld   l, a
                          ld   pmask, [hl]       # 11111111 01111111 ... 00001111 ... 00000001
            end.else do
                raise ArgumentError, "preshift must be aligned to 8"
            end
                          ld   a, px if check_oos
                          exx
            if check_oos
                          add  xcount
                          add  dx
                          ex   af, af            # a': x f': CF
            end
                          ld   tmp, dy
                          ld   xcount, dx
                          divmod xcount, tmp, check0:horizontal, check1: true
                                                 # xcount: int(dx/dy), a: dx%dy
                          ld   quot, xcount      # quot: int(dx/dy)
                          ld   size, xcount      # size: quot
                          neg                    # a: -(dx%dy)
                          add  dx                # a: int(dx/dy)*dy === -(dx%dy)+dx
                          ld   xcount, a         # quot_dy: int(dx/dy)*dy
                                                 # initial size: int(dx/dy)/2
                          srl  size              # size: quot/2 CF: quot%2
                          jr   NC, no_sub_dy     # initial acc(dy): dx/2 + int(dx/dy)/2*dy =
                                                 # = dx/2 + (int(dx/dy)*dy - int(dx/dy)%2*dy)/2
                                                 # = (int(dx/dy)*dy - int(dx/dy)%2*dy + dx)/2
                                                 # = (quot_dy - (quot%2)*dy + dx)/2
                          sub  tmp               # a: quot_dy - dy
            no_sub_dy     add  dx                # a: quot_dy - (quot%2)*dy + dx
                          rra                    # a: (quot_dy - (quot%2)*dy + dx)/2
                                                 # a: acc(dy)
                          ld   dyt, tmp          # dyt: dy
                          ld   quot_dy, xcount
                          ex   af, af            # a': acc(dy), a: x, f: CF
            if check_oos
                          jr   NC, fits_xdx
                          sub  dx
                          jr   not_fits_xdx
            end
            fits_xdx      ld   a, dx
                          cpl
            not_fits_xdx  ld   xcount, a         # xcount: 255 - dx
                          ex   af, af            # a: acc(dy), a': xcount
                          jp   start_loop

            horizontal    ld   xcount, a         # xcount=0, last sub-line
            if check_oos
                          ex   af, af            # a: x f: CF
                          jr   NC, fits_horizx
                          sub  dx
                          cpl                    # 255 - x
                          inc  a
                          jp   adjfit_horizx
            end
            fits_horizx   ld   a, dx             # size
                          inc  a
                          jr   Z, long_line
            adjfit_horizx exx
                          add  px                # a: px + size
                          jr   NC, last_chunk_h  # check for big size (where px + size >= 256)
                          sub  c8                # a: px + size - 8
                          jp   last_chunk_h2     # don't check for same same
            long_line     ld   b, 32
                          exx
                          ld   l, px
                          exx
                          jp   plot_fx3

            pixel_next_ln exx                    # a: lmask&pmask, a': acc(dy)
            plot_fx1      combine_screen_pixels[]
                          ld   [hl], a
            next_line     label
            case direction
            when :down
                          nextline h, l, bcheck, scraddr:scraddr
            when :up
                          prevline h, l, bcheck, scraddr:scraddr
            else
              raise ArgumentError, "direction should be :up or :down"
            end
                          ld   a, xcount         # check if over
                          ora  a
            case end_with
            when :eoc
                          jr   Z, eoc
            when :ret
                          ret  Z
            else
                          jp   Z, end_with
            end
                          ld   size, quot        # size: int(dx/dy)
                          ex   af, af            # a: acc(dy), a: xcount
                          add  quot_dy           # a: acc(dy)+int(dx/dy)*dy
                          jr   C, fits
            start_loop    cp   dx                # acc(dy) - dx
                          jr   NC, fits          # acc(dy) >= dx
            correct_loop  inc  size              # size+=1
                          add  dyt               # acc(dy) += dy
                          jr   C, fits
                          cp   dx                # acc(dy) - dx
                          jr   C, correct_loop   # acc(dy) < dx
            fits          sub  dx                # acc(dy) -= dx
                          ex   af, af            # a': acc(dy), a: xcount
                          add  size              # xcount+size >= 256
                          jr   C, last_chunk     # size -= xcount+size-256, xcount = 0

            last_chunk_bk ld   xcount, a         # xcount+=size
                          ld   a, size

                          exx
                          add  px                # a: px + size
            last_chunk_h  sub  c8                # a: px + size - 8
                          jr   C, same_same      # x: -7..-1 (+8 = 1..7)
            last_chunk_h2 ld   px, a             # x: 0..
                          ld   a, pmask          # pmask: 11111111 01111111 00111111..00000001

                          exx
            plot_fx2      combine_screen_pixels[]
                          ld   [hl], a
                          inc  l
                          exx

                          ld   a, px             # x: 0..
                          anda tmp               # tmp: 0xF8
                          jr   Z, skip_middle    # px: 0..7
                                                 # px: 8..
                          ld   l, a              # tmp: px & 0xF8
                          3.times { rrca }       # a: px/8

                          exx
                          ld   b, a              # counter: px/8
            plot_fx3      label
            case fx
            when :or, :write, :nop, PLOT_FX_OR, PLOT_FX_NOP
                          ld   a, 0xFF           # filler
            middle_loop   ld   [hl], a
            when :xor, PLOT_FX_XOR
            middle_loop   ld   a, [hl]
                          cpl
                          ld   [hl], a
            when :and, PLOT_FX_AND
                          ld   a, 0              # filler
            middle_loop   ld   [hl], a
            when :none, PLOT_FX_NONE
                          nop
                          nop
            middle_loop   nop
            end
                          inc  l
                          djnz middle_loop
                          exx

                          ld   a, l              # a: px & 0xF8
            skip_middle   xor  px                # a: 0..7
                          ld   px, a             # px: 0..7
                          jr   Z, diagonal       # a: 0
                          add  a, preshift       # a: 1..7 + preshift
                          ld   l, a
                          ld   a, [hl]           # pmask: -------- 01111111 00111111..00000001
                          ld   pmask, a
                          cpl                    # lmask: -------- 10000000 11000000..11111110
                          jp   pixel_next_ln

            diagonal      label
            case pixel_type
            when :pixel
                          cpl                    # a: 0xFF
            when :mask
                          nop
            end
                          ld   pmask, a          # pmask: 11111111
                          exx
                          jp   next_line

            same_same     add  c8
                          ld   px, a             # px: 1..7
                          add  a, preshift       # 1..7 + preshift
                          ld   l, a
                          ld   a, [hl]           # pmask: 11111111 01111111 00111111..00000001
                          ld   l, a
                          cpl                    # lmask: 00000000 10000000 11000000..11111110
            case pixel_type
            when :pixel
                          anda pmask
            when :mask
                          ora  pmask
            else
              raise ArgumentError, "pixel_type should be :pixel or :mask"
            end
                          ld   pmask, l          # pmask:          01111111 00111111..00000001
                          jp   pixel_next_ln

            last_chunk    jr   Z, last_chunk_bk  # (xcount+size)&0xff == 0
                          sbc  size              # CF: 1
                          cpl
                          ld   size, a           # size -= xcount+size-256, xcount = 0
                          xor  a
                          jp   last_chunk_bk
          end
        end

      end

      include Z80
    end # Draw
  end # Gfx
end # ZXLib

# DEPRECATED
ZXGfxDraw = ZXLib::Gfx::Draw unless defined?(ZXGfxDraw) # :nodoc:

if __FILE__ == $0
    # :stopdoc:
    require 'zxlib/basic'
    require 'zxutils/benchmark'

    class TestZXGfxDraw # :nodoc: all
        include Z80
        include Z80::TAP
        include ZXLib::Gfx::Draw::Constants

        DRAW_OUT_OF_SCREEN_SAFE = true

        macro_import MathInt
        macro_import ZXLib::Gfx::Draw
        import       ZXLib::Sys, macros: true, code: false

        import       ZXUtils::Benchmark, :benchmark, code: true, labels: true, macros: true

        get_bench_result    calculate_benchmark_tstates(benchmark.counter, benchmark.tsframe, benchmark.frames, benchmark.idle, benchmark.adjustment)
        estimate_tsframes   estimate_tstates_per_interrupt(vars.udg, benchmark.interrup_vec, benchmark.forward, benchmark.tsframe, benchmark.idle)

        with_saved :start, :exx, hl, :exx, ret: true do |eoc|
                      call fn_argn
                      jr   C, start
          error_q     report_error_unless Z, 'Q Parameter error'
                      call get_arg_u8            # x
                      ld   [coords.x], a
                      call fn_argn.seek_next
                      jr   NZ, error_q.err
                      call get_arg_u8            # y
                      ld   [coords.y], a
                      call fn_argn.seek_next
                      jr   NZ, error_q.err
                      call get_arg_i8            # dx
                      ld   b, e
                      ld   [dx], bc
                      call fn_argn.seek_next
                      jr   NZ, error_q.err
                      call get_arg_i8            # dy
                      ld   b, e
                      ld   [dy], bc
          start       label
                      call draw_fx
        end
        ns :draw_fx do
                      ld   a, [vars.p_flag]
                      ld   hl, draw.line
                      anda 0b00001010           # INVERSE 1 or OVER 1
                      jr   Z, set_fx
                      cp   0b00001010           # INVERSE 1 and OVER 1
                      jr   NZ, inverse_1
                      ret                       # do nothing
          inverse_1   anda 0b00001000           # INVERSE 1
                      jr   Z, over_1
                      ld   hl, draw.line_inversed
                      jr   set_fx
          over_1      ld   hl, draw.line_over
          set_fx      ld   [draw_jump + 1], hl

                      ld   hl, [coords]
                      ld   bc, [dx]
                      ld   de, [dy]
          draw_jump   jp   draw.line
        end
        coords        data ZXLib::Sys::Coords, {x:0, y:0}
        dx            dw 0
        dy            dw 0
        draw          make_draw_line_subroutines(scraddr:0x4000, check_oos:DRAW_OUT_OF_SCREEN_SAFE)
        draw_end      label
        ns :testing do
                                        #=76
                      ld   hl, [coords] # 16
                      ld   bc, [dx]     # 20
                      ld   de, [dy]     # 20
                      jp   draw_fx.draw_jump # 10+10
        end

        ns :draw_random do
                      ld   hl, [vars.seed]
          try_again1  call rand_seed_hl
          unless DRAW_OUT_OF_SCREEN_SAFE
                      ld   a, 191
                      cp   h
                      jr   C, try_again1
          end
                      push hl
          try_again2  call rand_seed_hl
          unless DRAW_OUT_OF_SCREEN_SAFE
                      ld   a, 191
                      cp   h
                      jr   C, try_again2
          end
                      ld   [vars.seed], hl
                      pop  de
                      prepare_args_draw_line_to
                      jp   draw_fx.draw_jump
        end

        rand_seed_hl  rnd
                      ret

        with_saved :draw_random_many, :exx, hl, :exx, ret:true do
                      call fn_argn
                      ld   a, 1
                      jr   C, skip_args
          error_q     report_error_unless Z, 'Q Parameter error'
                      call get_arg_u8
          skip_args   ld   b, a
          loop0       push bc
                      call draw_random
                      pop  bc
                      djnz loop0
        end

        with_saved :draw_many, :exx, hl, :exx, ret:true do
                      xor  a
          loop1       ld   hl, (96<<8)|128
                      ld   de, (-1<<8)|96
                      push af
                      sub  128
                      ld   b, 0
                      jr   NC, positive1
                      dec  b
                      neg
            positive1 ld   c, a
                      call draw_fx.draw_jump
                      pop  af
                      inc  a
                      jr   NZ, loop1

                      ld   a, -192
          loop2       ld   hl, (96<<8)|128
                      ld   bc, (0<<8)|127
                      push af
                      add  96
                      ld   d, 0
                      jr   C, positive2
                      dec  d
                      neg
            positive2 ld   e, a
                      call draw_fx.draw_jump
                      pop  af
                      inc  a
                      jr   NZ, loop2

                      xor  a
          loop3       ld   hl, (96<<8)|128
                      ld   de, (0<<8)|95
                      dec  a
                      push af
                      sub  128
                      ld   b, 0
                      jr   NC, positive3
                      dec  b
                      neg
            positive3 ld   c, a
                      call draw_fx.draw_jump
                      pop  af
                      jr   NZ, loop3

                      ld   a, 192
          loop4       ld   hl, (96<<8)|128
                      ld   bc, (-1<<8)|128
                      dec  a
                      push af
                      sub  96
                      ld   d, 0
                      jr   NC, positive4
                      dec  d
                      neg
            positive4 ld   e, a
                      call draw_fx.draw_jump
                      pop  af
                      jr   NZ, loop4
        end

        fn_argn       find_def_fn_args 1, cf_on_direct:true

        ns :get_arg_u8 do
                      read_positive_int_value(b, c)
                      inc  hl                       # point to a next argument possibly
                      report_error_unless Z, 'A Invalid argument'
          positive    ld   a, b
                      ora  a
          num_big     report_error_unless Z, '6 Number too big'
                      ld   a, c
                      ret
        end
        ns :get_arg_i8 do
                      read_integer_value(b, c, e)
                      inc  hl                       # point to a next argument possibly
                      report_error_unless Z, 'A Invalid argument'
                      cp   e
                      jr   NC, get_arg_u8.positive
                      neg16 b, c
                      jr   get_arg_u8.positive
        end
    end

    include ZXLib

    testdraw = TestZXGfxDraw.new 0x8000
    coords = testdraw['coords']
    draw = testdraw[:draw]
    draw_end = testdraw[:draw_end]
    draw_many = testdraw[:draw_many]
    draw_many_inv = testdraw[:draw_many_inv]
    draw_random_many = testdraw[:draw_random_many]
    bench = testdraw['benchmark.bench']
    tsframe = testdraw['benchmark.tsframe']
    getset_tsframe = testdraw['benchmark.getset_tsframe']
    get_idle = testdraw['benchmark.get_idle']
    get_bench_result = testdraw[:get_bench_result]
    estimate_tsframes = testdraw[:estimate_tsframes]
    start = testdraw[:start]
    testing = testdraw[:testing]
    line0 = (0..31).map {|x| "`BRIGHT #{x.odd? ? '1' : '0'}` "}.join('')
    line1 = (0..31).map {|x| "`BRIGHT #{x.even? ? '1' : '0'}` "}.join('')
    program = Basic.parse_source <<-END
       1 DEF FN n(x)=x-(65536 AND x>=32768): DEF FN b(a,c)=USR #{bench}: DEF FN i()=USR #{get_idle}: DEF FN r()=USR #{get_bench_result}
         DEF FN l(a,b)=(a AND a>=b)+(b AND a<b)
         DEF FN t()=USR #{getset_tsframe}+65536*PEEK #{tsframe+2}: REM get ts/frame
         DEF FN s(t)=USR #{getset_tsframe}+65536*PEEK #{tsframe+2}: REM set ts/frame
         DEF FN d(x,y,h,v)=USR #{start}
         DEF FN m(c)=USR #{draw_random_many}
      10 BORDER 1: CLS: FOR i=1 TO 11: PRINT "#{line0}";"#{line1}";: NEXT i
         OVER 1
      20 INPUT "x ";x;",y ";y,",dx ";dx;",dy ";dy
      30 RANDOMIZE FN d(x,y,dx,dy)
      40 PAUSE 0
      50 PLOT x, 175-y: DRAW dx,-dy
      60 GO TO 20
     100 RANDOMIZE USR #{draw_many}: PAUSE 0: STOP
     200 LET q=FN m(0): PAUSE 0: GO TO 200
     300 CLS: OVER 1
         FOR x=0 TO 191: RANDOMIZE FN d(0,x,x,0): NEXT x
         FOR x=192 TO 255: RANDOMIZE FN d(0,x-192,x,0): NEXT x
     999 STOP
    1000 RANDOMIZE FN d(x,y,dx,dy): RETURN
    3000 CLS
         LET frames=FN b(#{testing},256): LET res=FN r()-76: LET respix=INT (res/(FN l(dx,dy)+1)+.5)
         PRINT OVER 0;frames;"(";FN i();"):";res;" ~";respix
         RETURN
    4000 CLS: RESTORE: OVER 0: LET ntests=54: DIM w(ntests,2)
    4010 FOR i=1 TO ntests
         READ x,y,dx,dy: RANDOMIZE FN d(x,y,dx,dy): PRINT x;",";y;",";dx;",";dy;" ";
         GO SUB 3001
         LET w(i,1)=res: LET w(i,2)=respix
         NEXT i
         PRINT #0;"press any key...": PAUSE 0: INPUT ""
    4100 CLS: RESTORE: PRINT "x y dx  dy","t-states/ pixel"
         FOR i=1 TO ntests
         READ x,y,dx,dy: PRINT x;TAB 1;",";y;TAB 3;",";dx;TAB 7;",";dy;" ",w(i,1);TAB 24;"/ ";w(i,2)
         NEXT i
    5000 DATA 0,0,0,191,   0,0,1,191,   0,0,7,191,   0,0,15,191, 0,0,31,191, 0,0,63,191, 0,0,127,191, 0,0,137,191, 0,0,175,191, 0,0,191,191
         DATA 0,0,255,191, 0,0,255,175, 0,0,255,127, 0,0,255,63, 0,0,255,31, 0,0,255,15, 0,0,255,7,   0,0,255,3,   0,0,255,1,   0,0,255,0
         DATA 0,0,0,0,     0,0,1,0,     0,0,7,0,     0,0,8,0,    0,0,15,0,   0,0,31,0,   0,0,127,0,   0,0,137,0,   0,0,175,0,   0,0,191,0,  0,0,254,0, 0,0,255,0
         DATA 0,0,0,1,     0,0,0,7,     0,0,0,8,     0,0,0,15,   0,0,0,31,   0,0,0,87,   0,0,0,127,   0,0,0,137,   0,0,0,175,   0,0,0,191
         DATA 0,0,2,1,     0,0,4,1,     0,0,7,1,     0,0,8,1,    0,0,15,1,   0,0,31,1,   0,0,127,1,   0,0,137,1,   0,0,175,1,   0,0,191,1,  0,0,254,1, 0,0,255,1
    9998 STOP
    9999 CLEAR #{testdraw.org - 1}: LOAD ""CODE: GO TO 10
    END

    puts testdraw.debug
    puts program.to_source escape_keywords:true

    [:benchmark,
     :start,
     :draw,
     'draw.line',
     'draw.line_over',
     'draw.line_over.dn',
     'draw.line_inversed',
     :testing,
     :draw_many,
     :draw_random_many,
    ].each do |name|
        puts "#{name.to_s.ljust(18)}: 0x#{testdraw[name].to_s(16).rjust(4,?0)} : #{testdraw[name]}"
    end
    puts "draw size: #{draw_end - draw}"

    program.save_tap 'test.zxlib.gfx.draw.tap', name:'draw', line:9999
    testdraw.save_tap 'test.zxlib.gfx.draw.tap', name:'draw', append: true
end