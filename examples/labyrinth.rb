# -*- coding: BINARY -*-
here = File.expand_path('..', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'z80/math_i'
require 'z80/stdlib'
require 'zxlib/gfx'
require 'zxlib/sys'

class Program
  include Z80
  include Z80::TAP

  ###########
  # Exports #
  ###########

  export start

  ###########
  # Imports #
  ###########

  macro_import  Z80Lib
  macro_import  Z80MathInt
  macro_import  ZXGfx
  import        ZXSys, macros:true, code:false

  ##########
  # Macros #
  ##########

  # ZF=0 if any key is being pressed
  macro :key_pressed? do |_, line_mask=0, key_mask=0x1f|
    if line_mask == 0
                xor  a
    else
                ld   a, line_mask unless line_mask == a
    end
                inp  a, (254)
                cpl
                anda key_mask
  end

  # ZF=0 if any cursor key is being pressed
  macro :cursor_key_pressed? do |_, t:b|
                key_pressed? 0xf7, 0x10 # key [5]
                ld  t, a
                key_pressed? 0xef, 0x1c # keys [6] [7] [8] . .
                rrca
                ora t                   # keys [5] [6] [7] [8] .
                rrca                    # keys [←] [↓] [↑] [→]
  end

  macro :init_interrupts do |_, handleint|
      ld  a, 0x18          # 18H is jr
      ld  [0xFFFF], a
      ld  a, 0xC3          # C3H is jp
      ld  [0xFFF4], a
      ld  hl, handleint
      ld  [0xFFF5], hl
      ld  a, 0x39
      ld  i, a             # load the accumulator with FF filled page in rom.
      im2
      ei
  end

  macro :restore_interrupts do
      di
      ld  a, 0x3F
      ld  i, a
      im1
      ei
  end

  macro :color_labyrinth do |_, &block|
                ex   af, af
                call viewport_to_room
                ld   bc, [labyrinth.dimensions]
                ld   de, mem.attrs
                ld   a, 24
                cp   c
                jr   NC, loop1
                ld   c, a
    loop1       push bc
                ld   a, 32
                cp   b
                jr   NC, skip0
                ld   b, a
    skip0       ex   af, af
    loop0       block.call
                inc  hl
                jr   Z, skip_mark
                ld   [de], a
    skip_mark   inc  de
                djnz loop0
                ex   af, af
                inc  hl           # skip boundary
                pop  bc
                ld   a, 32
                sub  b
                jr   NC, fits_line
                neg
                adda_to h, l      # skip remaining rooms
                jr   next_iter
    fits_line   adda_to d, e
    next_iter   dec  c
                jr   NZ, loop1
                ret
  end

  ########
  # MAIN #
  ########

  class Labyrinth < Label
    height      byte
    width       byte
    dimensions  height word
    room_data   word  # pointer to labyrinth data
                      # required size: (width + 1) * (height + 2) - 1 bytes
    skip_rooms  word  # how many rooms skip while carving
    # calculated by init_labyrinth:
    nrooms      word  # number of rooms: width * height
    room_00     word  # pointer to inner rooms array (past the top boundary)
    bsize       word  # inner size with boundaries: (width + 1) * height
  end

  class Viewport < Label
      y       byte
      x       byte
  end

  start           jr   example

  labyrinth       data Labyrinth, { width: 31, height: 21, room_data: room_data }
  viewport        data Viewport, {y: 0, x: 0}

  ns :example do
                  call fn_argn
                  jr   NZ, skip_fn_args
                  call get_arg_int8
                  ld   [labyrinth.height], a
                  call fn_argn.seek_next
                  jr   NZ, skip_fn_args
                  call get_arg_int8
                  ld   [labyrinth.width], a
                  call fn_argn.seek_next
                  jr   NZ, skip_fn_args
                  call get_arg_int
                  ld   [labyrinth.skip_rooms], de
    skip_fn_args  call init_labyrinth
                  report_error_unless NC, '4 Out of memory'
                  report_error_unless NZ, 'B Integer out of range'
                  ld   hl, 0
                  ld   [viewport], hl

                  exx
                  push hl # save hl'

                  call clear_labyrinth
                  call carve_labyrinth
                  exx
                  push hl # save seed
                  jr   C, quit

                  ld   a, [vars.attr_p]
                  call clear_screen
                  ld   a, [vars.bordcr]
                  call set_border

                  call draw_labyrinth

                  ld   bc, 0              # no solving
                  init_interrupts refresh_on_int

                  ld   de, 0x7f01         # [SPC]
                  call wait_key

                  ld   a, [vars.bordcr]
                  cpl
                  call set_border
                  call solve_labyrinth
                  ld   a, [vars.bordcr]
                  call set_border

                  ld   de, 0x7b01         # [SPC] or [Q]
                  call wait_key

                  restore_interrupts

                  call release_key
                  ld   a, [vars.bordcr]
                  call set_border

    quit          pop bc  # return seed
                  exx

                  pop hl  # original hl'
                  exx
                  ret
  end

  ###############
  # Subroutines #
  ###############

  # clears screen area with border and attributes are set according to register A
  clear_screen  clrmem  mem.screen, mem.scrlen, 0
                clrmem  mem.attrs, mem.attrlen, a
                ret

  # set border taken from an attribute in register A
  set_border    anda 0b00111000
                3.times { rrca }
                out  (254), a
                ret

  # check if specified keys are down
  # d: key_line_mask, e: key_mask
  # return if none otherwise wait until key is released
  key_down?     key_pressed? d, e
                ret

  # waits until specified keys are being pressed
  # d: key_line_mask, e: key_mask
  wait_key      halt
                call key_down?
                jr   Z, wait_key
  # waits until no key is being pressed
  release_key   halt
                key_pressed?
                jr   NZ, release_key
                ret

  # waits until any key is being pressed
  wait_any_key  ld   de, 0x001f
                jr   wait_key

  # randomize from seed in hl -> hl
  next_rnd_hl   rnd
                ret

  # find DEF FN argument value
  # ZF=1 if found and +hl+ points to the FP-value
  fn_argn       find_def_fn_args 1
  # try to get positive integer from FP-value addressed by HL
  # +de+ holds a value on success
  get_arg_int   read_positive_int_value d, e
                inc  hl           # point to a next argument possibly
                ret  Z
                report_error 'Q Parameter error'
  # try to get positive integer from FP-value addressed by HL
  # +a+ holds a value on success
  get_arg_int8  call get_arg_int
                ld   a, d
                ora  a
                report_error_unless Z, '6 Number too big'
                ld   a, e
                ret

  macro :turn_lt do |_, mov:d|
          dec  mov
  end

  macro :turn_rt do |_, mov:d|
          inc  mov
  end

  macro :mark_room do |_, room=hl|
          set  6, [room]
  end

  macro :visit_room do |_, room=hl|
          set  5, [room]
  end

  macro :is_marked do |_, room=hl|
          bit  6, [room]
  end

  macro :is_visited do |_, room=hl|
          bit  5, [room]
  end

  macro :is_up_opened do |_, room=hl|
          bit  0, [room]
  end

  macro :is_left_opened do |_, room=hl|
          bit  1, [room]
  end

  macro :is_boundary do |_, room=hl|
          bit  7, [room]
  end

  macro :move_room do |eoc, width:e, mov:d|
          ld   a, mov
          anda 0b11
          jr   NZ, ck_rt
          # 0=up
          sub_from width, h, l
          dec  hl
          jr   eoc
    ck_rt dec  a
          jr   NZ, ck_dn
          # 1=right
          inc  hl
          jr   eoc
    ck_dn dec  a
          jr   NZ, is_lt
          # 2=down
          ld   a, width
          adda_to h, l
          inc  hl
          jr   eoc
          # 3=left
    is_lt dec  hl
  end

  # initializes labyrinth variables: room_00, nrooms and bsize
  # call after changing width, height or room_data
  # validates labyrinth dimensions
  # CF=1 if data doesn't fit in memory
  # ZF=1 if invalid dimensions (width==0 or height==0)
  ns :init_labyrinth do
                ld   bc, [labyrinth.dimensions] # c = height, b = width
                ld   a, c
                ora  a
                ret  Z                          # height == 0
                ld   a, b
                ora  a
                ret  Z                          # width == 0
                ld   hl, [labyrinth.room_data]
                mul8_c 0, c, b, tt:de, clrhl:false # room_data += height * width
                ret  C                          # overflow
                add  hl, bc                     # room_data += height
                ret  C                          # overflow

                push bc
                push hl
                ld   a, [labyrinth.width]
                add  a
                ld   c, a
                rl   b                          # bc: 2*width
                inc  bc                         # bc: 2*width + 1
                add  hl, bc                     # room_data += 2*width + 1 (up and bottom boundary)
                jr   C, no_test12
                ld   bc, 12
                add  hl, bc                     # room_data += 12 (interrupt routine handler)
    no_test12   pop  hl
                pop  bc
                ret  C                          # overflow

                ld   de, [labyrinth.room_data]
                sbc  hl, de                     # bsize = room_data + height * width + height - room_data
                ld   [labyrinth.bsize], hl
                sbc  hl, bc                     # nrooms = bsize - height
                ld   [labyrinth.nrooms], hl
                ld   a, [labyrinth.width]
                adda_to d, e
                inc  de
                ld   [labyrinth.room_00], de    # room_00 = room_data + width + 1
                ora  a                          # ZF = 0, CF = 0
                ret
  end

  ns :refresh_on_int do
    with_saved :no_ixy, :ex_af, af, merge: true do |eoc|
                    ld   a, c                      # check counter (in bc)
                    ora  b
                    jr   Z, in_viewport
                    call room_to_xy                # hl: current room address -> h: x, l: y
                    call xy_in_viewport?           # check if current room in viewport
                    jr   NC, in_viewport
                    call center_viewport_xy        # update viewport
                    jr   redraw_all
      in_viewport   cursor_key_pressed?
                    jr   Z, no_cursor_key
                    call update_viewport
      redraw_all    call draw_labyrinth
                    jr   eoc
      no_cursor_key ld   a, [vars.attr_p]
                    ora 0b01000000
                    xor 0b00111111
                    call color_labyrinth_visited
    end
                    ei
                    ret
  end

  # a = cursor bits b3 [←] [↓] [↑] [→] b0
  ns :update_viewport do
                ld   de, [viewport]              # e = y, d = x
                ld   bc, [labyrinth.dimensions]  # c = height, b = width
                rrca
    with_saved  af, bc, de do
                call C, right
    end
                rrca
    with_saved  af, bc, de do
                call C, up
    end
                rrca
    with_saved  af, bc, de do
                call C, down
    end
                rrca
                ret  NC                          # nothing pressed

    left        ld   a, d                        # x
                sub  1                           # x - 1
                jr   NC, assign_x                # x - 1 >= 0
                xor  a                           # x = 0
                jr   assign_x

    right       ld   a, b                        # width
                sub  32
                ret  C                           # width <  32
                ret  Z                           # width == 32
                ld   b, a                        # width -= 32
                ld   a, d                        # x
                add  1                           # x + 1
                ret  C                           # x + 1 > 255
                cp   b
                jr   C, assign_x                 # x + 1 < width - 32
                ld   a, b                        # x = width - 32
    assign_x    ld   [viewport.x], a
                ret

    up          ld   a, e                        # y
                sub  1                           # y - 1
                jr   NC, assign_y                # y - 1 >= 0
                xor  a
                jr   assign_y

    down        ld   a, c                        # height
                sub  24
                ret  C                           # height <  24
                ret  Z                           # height == 24
                ld   c, a                        # height -= 24
                ld   a, e                        # y
                add  1                           # y + 1
                ret  C                           # y + 1 > 255
                cp   c
                jr   C, assign_y                 # y + 1 < height - 24
                ld   a, c                        # y = height - 24
    assign_y    ld   [viewport.y], a
                ret
  end

  # input room address in hl, output h: x, l: y
  # WARNING: no boundary check
  ns :room_to_xy do
                ld   de, [labyrinth.room_00]     # start 0,0
                ora  a                           # CF=0
                sbc  hl, de                      # relative room address (index)
                ld   a, [labyrinth.width]
                inc  a                           # width + 1 (boundary)
                jr   Z, div_by_256               # width + 1 == 256
                ld   c, a
                divmod8 c, check0:false, check1:false # index / (width + 1) == y
    assign_x    ld   h, a                        # x
                ret
    div_by_256  ld   a, l                        # x
                ld   l, h                        # y
                jr   assign_x
  end

  # input h: x, l: y
  # output CF=1 not in viewport !(x >= vx && y>= vy && x < vx + 32 && y < vx + 24)
  # WARNING: no boundary check and no viewport validation
  ns :xy_in_viewport? do
                ld   bc, [viewport]              # b: vx, c: vy
                ld   a, l                        # y
                cp   c                           # y - vy
                ret  C                           # CF=1 if y < vy
                ld   a, h                        # x
                cp   b                           # x - vx
                ret  C                           # CF=1 if x < vx
                ex   de, hl
                ld   hl, (32<<8) | 24            # h: 32, l: 24
                add  hl, bc                      # we know that vx + 32 < 256 and vy + 24 < 256
                ex   de, hl                      # h: x, l: y, d: vx + 32, e: vy + 24
                ld   a, h                        # x
                cp   d                           # x - (vx + 32)
                ccf
                ret  C                           # CF=1 if x >= (vx + 32)
                ld   a, l                        # y
                cp   e                           # y - (vy + 24)
                ccf
                ret                              # CF=1 if y >= (vy + 24)
  end

  # input h: x, l: y
  # viewport.x: min(max(0, x - 16), max(0, width - 32))
  # viewport.y: min(max(0, y - 12), max(0, height - 24))
  ns :center_viewport_xy do
                ld   a, h                        # x
                sub  16                          # x-= 16
                jr   NC, skip_resetx
                xor  a                           # vx = 0
    skip_resetx ld   h, a                        # vx = max(0, x - 16)
                ld   a, l                        # y
                sub  12                          # y -= 12
                jr   NC, skip_resety
                xor  a                           # vy = 0
    skip_resety ld   l, a                        # vy = max(0, y - 12)
                ld   de, [labyrinth.dimensions]  # d: width, e: height
                ld   a, d                        # width
                sub  32                          # max_x = width - 32
                jr   NC, skip_max_x0             # width >= 32
                xor  a                           # max_x = 0 if width < 32
    skip_max_x0 cp   h                           # max_x - x
                jr   NC, skip_vx0                # max_x >= x
                ld   h, a                        # vx = max_x if max_x < x
    skip_vx0    ld   a, e                        # height
                sub  24                          # max_y = height - 24
                jr   NC, skip_max_y0             # height >= 24
                xor  a                           # max_y = 0 if height < 24
    skip_max_y0 cp   l                           # max_y - y
                jr   NC, skip_vy0                # max_y >= y
                ld   l, a                        # vy = max_y if max_y < y
    skip_vy0    ld   [viewport], hl              # set viewport
                ret
  end

  # returns room address from viewport
  # WARNING: no boundary check
  ns :viewport_to_room do
                ld   de, [viewport]              # d: x, e: y
                ld   hl, [labyrinth.room_00]     # start 0,0
                ld   a, d
                adda_to h, l                     # start += x
                ld   a, [labyrinth.width]
                ld   c, a
                ld   b, 0
                inc  bc                          # width + 1
                mul8 b, c, e, tt:bc, clrhl:false # start += (width + 1) * y
                ret
  end

  # returns random room address that is not a boundary room
  # hl': seed
  ns :random_start do
    try_again   exx
                call next_rnd_hl          # hl = rnd*65536
                push hl
                exx
                pop  hl
                ld   de, [labyrinth.bsize] # hl % de
                divmod16  check0:false, check1:false, modulo:true, quick8:true
                # bc = rnd*65536 % size
                ld   hl, [labyrinth.room_00]
                add  hl, bc
                is_boundary
                ret  Z
                jp   try_again
  end

  # creates random labyrinth
  # WARNING: labyrinth must have 0 < width < 256 and 0 < height < 256
  ns :carve_labyrinth do
                ld   hl, [vars.seed]
                exx
                ld   hl, [labyrinth.nrooms]
                dec  hl              # nrooms - 1
                ld   bc, [labyrinth.skip_rooms]
                ora  a               # CF = 0
                sbc  hl, bc
                ld16 bc, hl
                ret  C               # return if skip_rooms > nrooms - 1
                push bc              # save nrooms - skip_rooms

                call random_start    # hl room address
                mark_room
                exx
                call next_rnd_hl     # first random turn
                ld   a, h
                exx
                ld   d, a            # mov
                ld   a, [labyrinth.width]
                ld   e, a            # width

                pop  bc              # restore rooms to go
    main_loop   ld   a, c
                ora  b
                ret  Z
                push bc

                exx
    turn_again  call next_rnd_hl     # random turn
                ld   a, h
                anda 0b11            # turn 0 - 3
                jr   Z, turn_again   # turn == 0? -> rnd again
                sub  2               # turn (-1, 0, +1)
                exx
                add  d               # mov turn (-1, 0, +1)
                ld   d, a            # mov

    hunt_next   ld   b, 4
    another_dir push hl              # save room address
                move_room width:e, mov:d
                is_marked
                jr   Z, open_room
                pop  hl
                turn_rt mov:d        # try another direction
                djnz another_dir

                push de              # save mov and width
    seek_marked call random_start    # hl room address
                is_marked
                jr   Z, seek_marked  # randomize room until marked found
                pop  de              # restore mov and width
                jr   hunt_next

    ns :open_room do |eoc|
                ld   a, d
                anda 0b11            # mov
                jr   NZ, ck_open_1   # mov!=0
                ex   [sp], hl        # previous room
                set  0, [hl]         # open up
                pop  hl              # pop new room
                jr   eoc
      ck_open_1 xor  0b11
                jr   NZ, ck_open_2   # mov!=3
                ex   [sp], hl        # previous room
                set  1, [hl]         # open left
                pop  hl              # pop new room
                jr   eoc
      ck_open_2 pop  bc              # discard previous room
                dec  a
                jr   NZ, ck_open_3   # mov!=2
                set  0, [hl]         # open up (down from previous room)
                jr   eoc
      ck_open_3 set  1, [hl]         # open left (right from previous room)
    end
                mark_room
                pop  bc
                dec  bc              # rooms to go
                jp   main_loop
  end

  # clears labyrinth
  # WARNING: labyrinth must have 0 < width < 256 and 0 < height < 256
  ns :clear_labyrinth do
                ld   hl, [labyrinth.room_data]
                ld   de, [labyrinth.dimensions]
                ld   b, d # width
                inc  b    # width + 1
                clrmem8 hl, b, 0xC0
    clr_loop    clrmem8 hl, d, 0
                ld   [hl], 0xC0
                inc  hl
                dec  e
                jr   NZ, clr_loop
                clrmem8 hl, d, 0xC0
                ret
  end

  # sets attributes for visited rooms
  # WARNING: labyrinth must have 0 < width < 256
  color_labyrinth :color_labyrinth_visited do
    is_visited
  end

  # draws labyrinth on the screen
  # WARNING: labyrinth must have 0 < width < 256
  ns :draw_labyrinth do
                call viewport_to_room
                ld   bc, [labyrinth.dimensions]
                ld   de, mem.screen
                ld   a, [vars.attr_p]
                anda 0b00111111
                ora  0b01000000
                ex   af, af

                inc  c   # height with boundary

                ld   a, 32 # check screen width
                cp   b
                jr   NC, main_loop
                ld   b, a

    main_loop   push hl
                push de
                push bc
    hloop       ld   a, 0x80
                is_up_opened
                jr   NZ, is_openup
                ld   a, 0xFF
    is_openup   ld   [de], a
                inc  e
                inc  hl
                djnz hloop

                pop  bc     # b: visible width, c: height

                ld   a, [labyrinth.width]
                cp   32
                jr   NC, no_line_tr
                ex   de, hl # hl: screen
                ld   [hl], 0x80
    no_line_tr  pop  de     # screen
                dec  c
                jr   NZ, skip_ret
                pop  hl
                ret
    skip_ret    ld   l, c     # height - 1
                ld   h, a     # width
                ex   [sp], hl # hl: room, (sp): width|height

                push hl
                push de
                scrtoattr d
                ex   af, af
                ld   c, a     # attr_p | bright
                ex   af, af
    attrloop    ld   a, c
                is_visited
                jr   Z, not_visited
                xor  0b00111111
                jr   skip_markck
    not_visited is_marked
                jr   NZ, skip_markck
                anda 0b00111111
    skip_markck ld   [de], a
                inc  e
                inc  hl
                djnz attrloop
                pop  de    # screen
                pop  hl

                ld   a, [labyrinth.width]
                cp   32 # width - 32
                jr   C, no_crop_w # width < 32
                ld   a, 31 # vcounter = 31 if width >= 32
    no_crop_w   ld   c, a  # vcounter
                inc  c     # row width + boundary
    vloop       push de    # screen
                xor  a
                is_left_opened
                inc  hl
                jr   NZ, is_openleft
                ld   a, 0x80
    is_openleft ld   b, 7
    vdraw_loop  inc  d
                ld   [de], a
                djnz vdraw_loop
                pop  de
                inc  e
                dec  c
                jr   NZ, vloop

                pop  bc

                dec  e
                nextrow d, e, true
                ld   a, e
                anda 0b11100000
                ld   e, a

                ld   a, b
                sub  32
                jr   C, main_loop # width < 32
                inc  a
                adda_to h, l      # skip remaining rooms + boundary
                ld   b, 32
                jp   main_loop
  end

  ns :solve_labyrinth do
                di
                ld   [save_sp + 1], sp
                ld   hl, [vars.frames]
                exx
    seek_marked call random_start    # hl room address
                is_marked
                jr   Z, seek_marked  # randomize room until marked found
                ld   bc, 0
                ei
                ld   a, [labyrinth.width]
                ld   e, a
                ld   d, 0
    visit_loop  visit_room
                is_up_opened
                jr   Z, skip_up1
                push hl
                scf
                sbc  hl, de
                is_visited
                jr   NZ, skip_up0
                ex   [sp], hl
                inc  bc
                jr   skip_up1
    skip_up0    pop  hl
    skip_up1    is_left_opened
                jr   Z, skip_left1
                dec  hl
                is_visited
                jr   NZ, skip_left0
                push hl
                inc  bc
    skip_left0  inc  hl
    skip_left1  inc  hl
                ld   a, [hl]
                anda 0b00100010     # visited | left 
                xor  0b00000010     # left
                jr   NZ, skip_right
                push hl
                inc  bc
    skip_right  add  hl, de
                ld   a, [hl]
                anda 0b00100001     # visited | up
                xor  0b00000001     # up
                jr   NZ, skip_down
                push hl
                inc  bc
    skip_down   ld   a, c
                ora  b
                ret  Z
                dec  bc
                pop  hl

                push de
                ld   de, 0xfb01     # [Q]
                call key_down?
                jr   Z, check_spc
    save_sp     ld   sp, 0          # restore SP
                ld   bc, 0          # clear rooms to go
                ret
    check_spc   ld   de, 0x7f01     # [SPC]
                call key_down?
                jr   NZ, paused
    next_visit  pop  de
                halt
                jr   visit_loop
    paused      push bc
                ld   bc, 0
                call release_key
                call wait_key
                pop  bc
                jr   next_visit
  end

  ########
  # Data #
  ########

  # rooms:
  #   bit 7 - boundary (1=boundary)
  #   bit 6 - mark (1=marked)
  #   bit 5 - visit (1=visited)
  #   bit 1 - left (1=open)
  #   bit 0 - up   (1=open)
  room_data label
end

labyrinth = Program.new 0x8000

puts labyrinth.debug

%w[
  labyrinth.height
  labyrinth.width
  labyrinth.room_data
  labyrinth.skip_rooms
  room_data
].each do |label|
  puts "#{label.ljust(20)}: 0x#{labyrinth[label].to_s 16} - #{labyrinth[label]}"
end

Z80::TAP.read_chunk('examples/labyrinth.tap').save_tap 'labyrinth'
labyrinth.save_tap('labyrinth', append: true)
Z80::TAP.parse_file('labyrinth.tap') do |hb|
    puts hb.to_s
end
