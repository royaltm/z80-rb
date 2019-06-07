# -*- coding: BINARY -*-
here = File.expand_path('../lib', __dir__)
$:.unshift(here) unless $:.include?(here)

require 'z80'
require 'z80/math_i'
require 'z80/stdlib'
require 'zxlib/gfx'
require 'zxlib/sys'
require 'zxlib/basic'

class Program
  include Z80
  include Z80::TAP

  ###########
  # Exports #
  ###########

  export start
  export labyrinth
  export viewport

  ###########
  # Imports #
  ###########

  macro_import  Stdlib
  macro_import  MathInt
  macro_import  ZXGfx
  import        ZXSys, macros:true, code:false

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
                      # Calculated by init_labyrinth:
    nrooms      word  # number of rooms: width * height
    room_00     word  # pointer to inner rooms array (past the top boundary)
    bsize       word  # inner size with boundaries: (width + 1) * height
  end

  class Viewport < Label
      y       byte
      x       byte
  end

  int_counter     addr 0xFFFE

  ##
  # Labyrinth DEMO
  #
  # POKE 32770, height: POKE 32771, width: RANDOMIZE USR 32768
  # or
  # 1 DEF FN l(h,w,s) = USR 32768
  # RANDOMIZE FN l(height, width, skip)
  start           jr   demo

  labyrinth       data Labyrinth, { width: 31, height: 21, room_data: room_data }
  viewport        data Viewport, {y: 0, x: 0}

  ns :demo do
                  call fn_argn                # is this an FN call with arguments?
                  jr   NZ, skip_fn_args
                  call get_arg_int8           # get 1st argument as 8bit unsigned height
                  ld   [labyrinth.height], a
                  call fn_argn.seek_next      # check if exists next argument
                  jr   NZ, skip_fn_args
                  call get_arg_int8           # get 2nd argument as 8bit unsigned width
                  ld   [labyrinth.width], a
                  call fn_argn.seek_next      # check if exists next argument
                  jr   NZ, skip_fn_args
                  call get_arg_int            # get 3rd argument as 16bit unsigned skip rooms
                  ld   [labyrinth.skip_rooms], de

    skip_fn_args  call init_labyrinth         # initialize labyrinth variables
                  report_error_unless NC, '4 Out of memory'
                  report_error_unless NZ, 'B Integer out of range'

                  ld   hl, 0                  # initialize viewport
                  ld   [viewport], hl
                  ld   hl, int_counter        # initialize int1 refresh draw counter
                  ld   [hl], 2

                  exx
                  push hl                     # save H'L'

                  ld   a, [vars.attr_p]       # prepare screen
                  call clear_screen
                  call set_inv_brdcr          # inverse border

                  call clear_labyrinth        # clear labyrinth rooms

                  ld   hl, refresh_on_int1    # setup 1st interrupt routine
                  call init_maskint

                  call carve_labyrinth        # carve a labyrinth
                  exx
                  push hl                     # save last rng seed
                  jr   C, quit                # skip_rooms >= nrooms

                  call restr_maskint          # restore system interrupt routine

                  call set_brd_brdcr          # normal border

                  call draw_labyrinth         # draw labyrinth

                  ld   bc, 0                  # prevent viewport tracking
                  ld   hl, refresh_on_int2    # setup 2nd interrupt routine
                  call init_maskint.set_handler

                  call release_key            # wait until no key is being pressed
                  ld   de, 0x7f01             # wait until [SPACE] is pressed and released
                  call wait_key

                  call set_inv_brdcr          # inverse border
                  call solve_labyrinth        # solve labyrinth
                  call set_brd_brdcr          # normal border

                  ld   de, 0x7f01             # wait until [SPACE] is pressed and released
                  call wait_key

    quit          call restr_maskint          # restore system interrupt routine

                  sub  a
                  call set_mix_brdcr          # normal border

                  pop  bc                     # restore last rng seed as return value
                  exx

                  pop  hl                     # restore original hl'
                  exx
                  ret
  end

  ###############
  # Subroutines #
  ###############

  # Clears screen area with border and attributes are set according to register A.
  clear_screen  clrmem  mem.screen, mem.scrlen, 0
                clrmem  mem.attrs, mem.attrlen, a
                ret

  # Set inversed border.
  set_inv_brdcr scf
  # Set border according to CF=1 inversed, CF=0 normal.
  set_brd_brdcr sbc  a
  # Mix register A with BORDCR from ZX Spectrum VARS.
  set_mix_brdcr xor  [iy + vars.bordcr - vars_iy]
  # Set border taken from an attribute in register A.
  set_border    anda 0b00111000
                3.times { rrca }
                out  (254), a
                ret

  # Check if specified keys are down.
  # d: key_line_mask, e: key_mask
  # return if none otherwise wait until key is released
  key_down?     key_pressed? d, e
                ret

  # Waits until specified keys are being pressed.
  # d: key_line_mask, e: key_mask
  wait_key      halt
                call key_down?
                jr   Z, wait_key
  # Waits until no key is being pressed.
  release_key   halt
                key_pressed?
                jr   NZ, release_key
                ret

  # Waits until any key is being pressed.
  wait_any_key  ld   de, 0x001f
                jr   wait_key

  # Check for cursor key is being pressed.
  # ZF=0 if any cursor key is being pressed
  # A=bits 3-0: [←] [↓] [↑] [→] if 1 key is being pressed
  cursor_key?   cursor_key_pressed?
                ret

  # Setup maskable interrupts handler given in HL.
  init_maskint  setup_custom_interrupt_handler hl
                ret

  # Restore maskable interrupts to ROM handler.
  restr_maskint restore_rom_interrupt_handler
                ret

  # Return random number in HL from a given seed in HL.
  next_rnd_hl   rnd
                ret

  # Find DEF FN argument value.
  # ZF=1 if found and then HL points to the FP-value
  fn_argn       find_def_fn_args 1
  # Try to get positive integer from FP-value addressed by HL.
  # DE holds a value on success.
  get_arg_int   read_positive_int_value d, e
                inc  hl           # point to a next argument possibly
                ret  Z
                report_error 'A Invalid argument'
  # Try to get positive integer from FP-value addressed by HL.
  # A holds a value on success.
  get_arg_int8  call get_arg_int
                ld   a, d
                ora  a
                report_error_unless Z, '6 Number too big'
                ld   a, e
                ret

  #############
  # Labyrinth #
  #############

  macro :turn_ccw do |_, mov:d|
          dec  mov
  end

  macro :turn_cw do |_, mov:d|
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

  # Initializes labyrinth variables: room_00, nrooms and bsize.
  # Call it after changing width, height or room_data.
  # Validates labyrinth dimensions:
  # * CF=1 if data doesn't fit in memory,
  # * ZF=1 if invalid dimensions (width==0 or height==0).
  ns :init_labyrinth do
                ld   bc, [labyrinth.dimensions] # C: height, B: width
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
                rl   b                          # BC: 2*width
                inc  bc                         # BC: 2*width + 1
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

  # Clears labyrinth and prepares it for carving.
  # WARNING: labyrinth must have 0 < width < 256 and 0 < height < 256.
  ns :clear_labyrinth do
                ld   hl, [labyrinth.room_data]
                ld   de, [labyrinth.dimensions]
                ld   b, d                       # width
                inc  b                          # width + 1
                clrmem8 hl, b, 0xC0             # mark upper boundary row
    clr_loop    clrmem8 hl, d, 0                # clear row
                ld   [hl], 0xC0                 # mark right boundary room
                inc  hl
                dec  e                          # decrease height
                jr   NZ, clr_loop
                clrmem8 hl, d, 0xC0             # mark bottom boundary
                ret
  end

  # Restore system maskable interrupts and report BREAK
  break_abort   call restr_maskint
                report_error 'D BREAK - CONT repeats'

  # A maskable interrupt handler 1.
  # Redraws view, moves around viewport on cursor keys, suppress redrawing on space key.
  # Aborts on break.
  ns :refresh_on_int1 do
    with_saved :no_ixy, :ex_af, af, merge: true do |eoc|
                    ld   hl, int_counter
                    call cursor_key?
                    jr   Z, no_cursor_key
                    push hl
                    call move_viewport
                    pop  hl
                    jr   redraw0
      no_cursor_key dec  [hl]
                    jr   Z, redraw0
                    ld   de, 0x7f01     # [SPACE]
                    call key_down?
                    jr   Z, redraw1     # skip drawing if pressed
                    ld   [hl], 5
                    jr   eoc
      redraw0       ld   [hl], 2
      redraw1       call draw_labyrinth
                    call rom.break_key
                    jr   NC, break_abort
    end
                    ei
                    ret
  end

  # A maskable interrupt handler 2.
  # Redraws view, follows solving room in viewport, moves around viewport on cursor keys.
  # Refreshes attributes only if there are no viewport changes.
  # Aborts on break.
  ns :refresh_on_int2 do
    with_saved :no_ixy, :ex_af, af, merge: true do |eoc|
                    ld   a, c                      # check counter (in bc)
                    ora  b
                    jr   Z, in_viewport
                    call room_to_xy                # current room address -> H: x, L: y
                    call xy_in_viewport?           # check if current room in viewport
                    jr   NC, in_viewport
                    call center_viewport_xy        # update viewport
                    jr   redraw_all

      in_viewport   call cursor_key?
                    jr   Z, no_cursor_key
                    call move_viewport
      redraw_all    call draw_labyrinth
                    jr   eoc
      no_cursor_key ld   a, [vars.attr_p]
                    ora 0b01000000
                    xor 0b00111111
                    call color_labyrinth_visited
                    call rom.break_key
                    jr   NC, break_abort
    end
                    ei
                    ret
  end

  # Move viewport according to register A and labyrinth dimensions.
  # Cursor bits in A: b3 [←] [↓] [↑] [→] b0
  ns :move_viewport do
                ld   de, [viewport]              # E: y, D: x
                ld   bc, [labyrinth.dimensions]  # C: height, B: width
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

  # Convert room address in HL, to coordinates H: x, L: y
  # Prior to calling this function labyrinth variables must be successfully
  # initialized by calling +init_labyrinth+.
  # WARNING: no boundary check.
  ns :room_to_xy do
                ld   de, [labyrinth.room_00]          # start 0,0
                ora  a                                # CF=0
                sbc  hl, de                           # relative room address (index)
                ld   a, [labyrinth.width]
                inc  a                                # width + 1 (boundary)
                jr   Z, div_by_256                    # width + 1 == 256
                ld   c, a
                divmod8 c, check0:false, check1:false # index / (width + 1) == y
    assign_x    ld   h, a                             # x
                ret
    div_by_256  ld   a, l                             # x
                ld   l, h                             # y
                jr   assign_x
  end

  # Check if coordinates are visible.
  # Input in H: x, L: y
  # Output CF=1 not in viewport !(x >= vx && y>= vy && x < vx + 32 && y < vx + 24)
  # WARNING: no boundary check and no viewport validation.
  ns :xy_in_viewport? do
                ld   bc, [viewport]              # B: vx, C: vy
                ld   a, l                        # y
                cp   c                           # y - vy
                ret  C                           # CF=1 if y < vy
                ld   a, h                        # x
                cp   b                           # x - vx
                ret  C                           # CF=1 if x < vx
                ex   de, hl
                ld   hl, (32<<8) | 24            # H: 32, L: 24
                add  hl, bc                      # we know that vx + 32 < 256 and vy + 24 < 256
                ex   de, hl                      # H: x, L: y, D: vx + 32, E: vy + 24
                ld   a, h                        # x
                cp   d                           # x - (vx + 32)
                ccf
                ret  C                           # CF=1 if x >= (vx + 32)
                ld   a, l                        # y
                cp   e                           # y - (vy + 24)
                ccf
                ret                              # CF=1 if y >= (vy + 24)
  end

  # Center viewport on coordinates.
  # Input H: x, L: y
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
                ld   de, [labyrinth.dimensions]  # D: width, E: height
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

  # Returns room address in HL from viewport.
  # Prior to calling this function labyrinth variables must be successfully
  # initialized by calling +init_labyrinth+.
  # WARNING: no boundary check
  ns :viewport_to_room do
                ld   de, [viewport]              # D: x, E: y
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

  # Returns randomized room address that is not a boundary room.
  # Prior to calling this function labyrinth variables must be successfully
  # initialized by calling +init_labyrinth+.
  # Input: H'L': rng seed.
  # Output: HL: room address, H'L': updated rng seed
  ns :random_start do
    try_again   exx
                call next_rnd_hl              # HL: RND * 65536
                push hl
                exx
                pop  hl
                ld   de, [labyrinth.bsize]    # HL % DE
                divmod16  check0:false, check1:false, modulo:true, quick8:true
                ld   hl, [labyrinth.room_00]  # BC: RND * 65536 % bsize
                add  hl, bc                   # random room address
                is_boundary
                ret  Z                        # return when not a boundary room
                jp   try_again
  end

  # Creates randomized labyrinth using non-recursive Hunt'n'Seek algorythm.
  # Prior to calling this function labyrinth variables must be successfully
  # initialized by calling +init_labyrinth+ and room data must be prepared by
  # calling +clear_labyrinth+.
  # WARNING: A labyrinth must have 0 < width < 256 and 0 < height < 256.
  ns :carve_labyrinth do
                ld   hl, [vars.seed]
                exx
                ld   hl, [labyrinth.nrooms]
                dec  hl                   # nrooms - 1
                ld   bc, [labyrinth.skip_rooms]
                ora  a                    # CF = 0
                sbc  hl, bc               # nrooms - 1 - skip_rooms
                ret  C                    # return if skip_rooms > nrooms - 1
                push hl                   # save nrooms - 1 - skip_rooms

                call random_start         # HL: random room address
                mark_room                 # mark first room
                exx
                call next_rnd_hl          # first random direction
                ld   a, h
                exx
                ld   d, a                 # mov (2 lowest bits)
                ld   a, [labyrinth.width]
                ld   e, a                 # width

                pop  bc                   # restore rooms to go
    main_loop   ld   a, c
                ora  b
                ret  Z                    # return if no more rooms to carve
                push bc                   # save rooms to go

                exx
    turn_again  call next_rnd_hl          # random turn
                ld   a, h
                anda 0b11                 # turn: 0..3
                jr   Z, turn_again        # turn == 0 -> repeat
                exx
                sub  2                    # turn (-1, 0, +1)
                add  d                    # mov turn (-1: left, 0: ahead, +1: right)
                ld   d, a                 # mov (2 lowest bits)

    hunt_next   ld   b, 4                 # hunt unmarked adjacent room
    another_dir push hl                   # save room address
                move_room width:e, mov:d  # move to adjacent room by D & 0b11: 0 - up, 1 - right, 2 - down, 3 - left
                is_marked                 # adjacent room already marked?
                jr   Z, open_room         # no, then open a wall to the adjacent room
                pop  hl                   # restore room address
                turn_cw mov:d             # try another direction (turn direction clockwise)
                djnz another_dir          # repeat for all directions

                push de                   # save mov and width
    seek_marked call random_start         # seek random marked room
                is_marked
                jr   Z, seek_marked       # randomize room until marked found
                pop  de                   # restore mov and width
                jr   hunt_next            # hunt

    ns :open_room do |eoc|
                ld   a, d
                anda 0b11                 # mov
                jr   NZ, ck_open_1        # mov <> 0
                ex   [sp], hl             # exchange new with the previous room
                set  0, [hl]              # open upper wall from previous room
                pop  hl                   # pop new room
                jr   eoc
      ck_open_1 xor  0b11
                jr   NZ, ck_open_2        # mov <> 3
                ex   [sp], hl             # exchange new with the previous room
                set  1, [hl]              # open left wall from previous room
                pop  hl                   # pop new room
                jr   eoc
      ck_open_2 pop  bc                   # discard previous room
                dec  a
                jr   NZ, ck_open_3        # mov <> 2
                set  0, [hl]              # open up (down from previous room)
                jr   eoc
      ck_open_3 set  1, [hl]              # open left (right from previous room)
    end
                mark_room                 # mark new room
                pop  bc                   # restore rooms to go
                dec  bc                   # rooms to go - 1
                jp   main_loop
  end

  # Updates screen attributes for visited rooms using viewport.
  # Input: A: screen attribute value for a visited room.
  # Prior to calling this function labyrinth variables must be successfully
  # initialized by calling +init_labyrinth+.
  # WARNING: labyrinth must have 0 < width < 256
  ns :color_labyrinth_visited do
                ex   af, af           # save attribute
                call viewport_to_room
                ld   bc, [labyrinth.dimensions]
                ld   de, mem.attrs
                ld   a, 24            # limit height to the number of screen rows
                cp   c                # 24 - height
                jr   NC, loop1
                ld   c, a
    loop1       push bc               # save dimensions
                ld   a, 32            # limit width to the number of screen columns
                cp   b                # 32 - width
                jr   NC, skip0
                ld   b, a
    skip0       ex   af, af           # restore attribute
    loop0       is_visited
                inc  hl
                jr   Z, skip_color
                ld   [de], a          # color visited
    skip_color  inc  de               # next attribute address
                djnz loop0            # repeat min(width, 32) times
                ex   af, af           # save attribute
                inc  hl               # skip boundary room
                pop  bc               # restore dimensions
                ld   a, 32
                sub  b                # 32 - width
                jr   NC, fits_line    # width <= 32
                neg                   # width > 32
                adda_to h, l          # so skip remaining rooms -(32 - width)
                jr   next_iter
    fits_line   adda_to d, e          # otherwise skip to the next attribute row
    next_iter   dec  c
                jr   NZ, loop1        # repeat min(height, 24) times
                ret
  end

  # Draws labyrinth on the screen using viewport.
  # Prior to calling this function labyrinth variables must be successfully
  # initialized by calling +init_labyrinth+.
  # WARNING: labyrinth must have 0 < width < 256
  ns :draw_labyrinth do
                call viewport_to_room
                ld   bc, [labyrinth.dimensions]
                ld   de, mem.screen
                ld   a, [vars.attr_p]
                anda 0b00111111       # clear bright and flash bits
                ora  0b01000000       # apply bright bit
                ex   af, af           # save attribute

                inc  c                # C: height with boundary

                ld   a, 32            # B: min(width, 32)
                cp   b
                jr   NC, main_loop
                ld   b, a

    main_loop   push hl               # save room address
                push de               # save screen address
                push bc               # save min(width, 32), (height + 1)
    hloop       ld   a, 0x80          # draw upper walls
                is_up_opened
                jr   NZ, is_openup
                ld   a, 0xFF          # closed wall
    is_openup   ld   [de], a
                inc  e                # next screen column
                inc  hl               # next room
                djnz hloop            # repeat min(width, 32) times

                pop  bc               # restore min(width, 32), (height + 1)

                ld   a, [labyrinth.width]
                cp   32               # width - 32
                jr   NC, no_line_tr   # width >= 32
                ex   de, hl           # HL: screen address
                ld   [hl], 0x80       # right boundary pixel
    no_line_tr  pop  de               # restore screen address
                dec  c                # height -= 1
                jr   NZ, skip_ret     # height > 0
                pop  hl               # otherwise return
                ret
    skip_ret    ld   l, c             # height
                ld   h, a             # width
                ex   [sp], hl         # HL: restored room address, (SP): width << 8 | height

                push hl               # save room address
                push de               # save screen address
                scrtoattr d           # convert D to screen attribute address MSB
                ex   af, af           # restore attribute value
                ld   c, a             # attr_p | bright
                ex   af, af           # save attribute
    attrloop    ld   a, c             # update row of screen attributes
                is_visited
                jr   Z, not_visited
                xor  0b00111111       # inverse attribute for visited room
                jr   skip_markck
    not_visited is_marked
                jr   NZ, skip_markck
                anda 0b00111111       # clear bright bit for unmarked room
    skip_markck ld   [de], a
                inc  e
                inc  hl               # next room
                djnz attrloop         # repeat min(width, 32) times
                pop  de               # restore screen address
                pop  hl               # restore room address

                ld   a, [labyrinth.width]
                cp   32               # width - 32
                jr   C, no_crop_w     # width < 32
                ld   a, 31
    no_crop_w   ld   c, a             # C: min(width, 31)
                inc  c                # C: min(width, 31) + 1
    vloop       push de               # save screen address
                xor  a
                is_left_opened
                inc  hl               # next room
                jr   NZ, is_openleft
                ld   a, 0x80          # pixel for closed left wall
    is_openleft ld   b, 7
    vdraw_loop  inc  d                # screen line down
                ld   [de], a          # paint remaining 7 lines of the room with a left wall
                djnz vdraw_loop       # repeat 7 times
                pop  de               # restore screen address
                inc  e                # next screen column
                dec  c
                jr   NZ, vloop        # repeat min(width, 31) + 1 times

                pop  bc               # B: width, C: height

                dec  e                # go back to the last drawn screen column
                nextrow d, e, true    # advance to the next screen line, return when out of screen
                ld   a, e
                anda 0b11100000
                ld   e, a             # set screen column to 0

                ld   a, b             # width
                sub  32               # width - 32
                jr   C, main_loop     # width < 32
                inc  a
                adda_to h, l          # skip remaining rooms + boundary
                ld   b, 32            # B: min(width, 32)
                jp   main_loop
  end

  # Solve labyrinth by visiting recursively each exit starting at a random room.
  # Prior to calling this function labyrinth must be successfully carved by
  # calling +carve_labyrinth+.
  # WARNING: may exit by RST 8 when out of stack space.
  # The HL register contains the currently visited room address and BC
  # contains the number of rooms to be visited that has been pushed on the stack.
  # The interrupt routine can utilize this to refresh the screen.
  ns :solve_labyrinth, use: vars do
                di                    # disable interrupts
                ld   hl, [vars.frames]
                exx
    seek_marked call random_start     # HL: room address
                is_marked
                jr   Z, seek_marked   # randomize until marked room found
                ld   bc, 0            # reset "rooms left to visit" counter
                ei                    # enable interrupts
                ld   a, [labyrinth.width]
                ld   e, a
                ld   d, 0             # DE: labyrinth.width
    visit_loop  visit_room            # mark room as visited
                is_up_opened
                jr   Z, skip_up1
                push hl               # save current room
                scf
                sbc  hl, de           # go up
                is_visited            # check if already visited
                jr   NZ, skip_up0     # yes
                ex   [sp], hl         # no, so exchange up room with the current room on the stack
                inc  bc               # increase "rooms left" counter
                jr   skip_up1
    skip_up0    pop  hl               # restore current room if not visiting up room
    skip_up1    is_left_opened
                jr   Z, skip_left1
                dec  hl               # go left
                is_visited            # check if already visited
                jr   NZ, skip_left0   # yes
                push hl               # no, so push left room on the stack
                inc  bc               # increase "rooms left" counter
    skip_left0  inc  hl               # go back
    skip_left1  inc  hl               # go right
                ld   a, [hl]          # get room data  
                anda 0b00100010       # visited | left 
                xor  0b00000010       # left
                jr   NZ, skip_right   # left is closed or already visited
                push hl               # otherwise push right room on the stack
                inc  bc               # increase "rooms left" counter
    skip_right  add  hl, de           # go down
                ld   a, [hl]          # get room data
                anda 0b00100001       # visited | up
                xor  0b00000001       # up
                jr   NZ, skip_down    # up is closed or already visited
                push hl               # otherwise push down room on the stack
                inc  bc               # increase "rooms left" counter
    skip_down   ld   a, c             # check if there are rooms left to visit
                ora  b
                ret  Z                # no, then finish

                push de               # save width

                ld   de, 0x7f01       # check [SPACE] key
                call key_down?
                jr   Z, next_visit

    paused      push bc               # pause solving until [SPACE] is pressed again
                ld   bc, 0
                call release_key
                call wait_key
                pop  bc

    next_visit  di                    # disable interrupts so we can use HL
                ld   hl, [vars.stkend]
                ld   de, 40           # check space between STKEND and SP
                add  hl, de
                jr   C, oom_abort
                sbc  hl, sp
                jr   NC, oom_abort    # less than 40 bytes, so report OOM

                pop  de               # restore width

                pop  hl               # next room to visit
                ei                    # enable interrupts
                halt                  # wait 'til the next frame
                dec  bc               # decrease "rooms left" counter
                jr   visit_loop       # repeat

    oom_abort   call restr_maskint
                report_error '4 Out of memory'
  end

  ########
  # Data #
  ########

  # Rooms:
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

program = Basic.parse_source <<-END
   1 DEF FN l(h,w,s)=USR 32768
   5 BRIGHT 0: FLASH 0: PAPER 1: BORDER 1: INK 5: CLS
   6 PRINT "[`FLASH 1`SPACE`FLASH 0`] - start or continue;`TAB 10`speed up carving;`TAB 10`pause/resume solving","[5][6][7][8] - move view"
  10 INPUT "height (1-255)",height
  20 INPUT "width (1-255)",width
  30 INPUT "skip rooms:",skip
  40 RANDOMIZE FN l(height,width,skip)
  50 GO TO 10
9999 CLEAR 32767: LOAD "labyrinth"CODE : RUN
END
program.start = 9999
puts "="*32
puts program.to_source
puts "="*32

program.save_tap 'examples/labyrinth'
labyrinth.save_tap 'examples/labyrinth', append: true

Z80::TAP.parse_file('examples/labyrinth.tap') do |hb|
    puts hb.to_s
end
