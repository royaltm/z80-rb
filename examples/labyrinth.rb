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
    if line_mask.zero?
                xor  a
    else
                ld   a, line_mask
    end
                inp  a, (254)
                cpl
                anda key_mask
  end

  # ZF=0 if any cursor key is being pressed
  macro :cursor_key_pressed? do |_|
                key_pressed? 0xf7, 0x10 # key [5]
                ld  b, a
                key_pressed? 0xef, 0x1c # keys [6] [7] [8] . .
                rrca
                ora b                   # keys [5] [6] [7] [8] .
                rrca                    # keys [←] [↓] [↑] [→]
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
                  call init_labyrinth
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

                  ld   a, [vars.attr_p]
                  call clear_screen
                  ld   a, [vars.bordcr]
                  call set_border

    key_loop      call color_labyrinth
                  call draw_labyrinth
                  call wait_key
                  cursor_key_pressed?
                  jr   Z, no_cursor_key
                  call update_viewport
                  jr   key_loop

    no_cursor_key call release_key
                  pop bc  # return seed
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

  # waits until no key is being pressed
  release_key   halt
                key_pressed?
                jr   NZ, release_key
                ret

  # waits until a key is being pressed
  wait_key      halt
                key_pressed?
                jr   Z, wait_key
                ret

  # randomize from seed in hl -> hl
  next_rnd_hl   rnd
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

  macro :is_marked do |_, room=hl|
          bit  6, [room]
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
                ld   c, a
                add  a
                rl   b                          # width += width
                inc  bc                         # width += 1
                add  hl, bc                     # room_data += 2*width + 1 (up and bottom boundary)
                pop  hl
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

  # returns room address from viewport
  # WARNING: no boundary check
  ns :viewport_to_room do
                ld   de, [viewport]              # e = y, d = x
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
                call random_start    # hl room address
                mark_room
                exx
                call next_rnd_hl     # first random turn
                ld   a, h
                exx
                ld   d, a            # mov
                ld   a, [labyrinth.width]
                ld   e, a            # width
                ld   bc, [labyrinth.nrooms]

    main_loop   dec  bc              # rooms to go
                ld   a, c
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

  # brightens screen attributes for marked rooms
  # WARNING: labyrinth must have 0 < width < 256
  ns :color_labyrinth do
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
                jr   NC, loop0
                ld   b, a
    loop0       is_marked
                inc  hl
                jr   Z, skip_mark
                ld   a, [de]
                ora  0b01000000
                ld   [de], a
    skip_mark   inc  de
                djnz loop0
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

  # draws labyrinth on the screen
  # WARNING: labyrinth must have 0 < width < 256
  ns :draw_labyrinth do
                call viewport_to_room
                ld   bc, [labyrinth.dimensions]
                ld   de, mem.screen

                inc  c   # height with boundary

                ld   a, 32 # check screen width
                cp   b
                jr   NC, main_loop
                ld   b, a

    main_loop   push de
                push hl
    hloop       ld   a, 0x80
                is_up_opened
                inc  hl
                jr   NZ, is_openup
                ld   a, 0xFF
    is_openup   ld   [de], a
                inc  e
                djnz hloop

                ld   a, [labyrinth.width]
                ld   b, a
                cp   32
                jr   NC, no_line_tr
                ex   de, hl
                ld   [hl], 0x80
    no_line_tr  pop  hl
                pop  de    # screen
                dec  c
                ret  Z

                push bc

                ld   c, b
                ld   a, 31 # check screen width
                cp   c
                jr   NC, no_crop_w
                ld   c, a
    no_crop_w   inc  c     # row width + boundary
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

  ########
  # Data #
  ########

  # rooms:
  #   bit 7 - boundary (1=boundary)
  #   bit 6 - mark (1=marked)
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
  room_data
].each do |label|
  puts "#{label.ljust(20)}: 0x#{labyrinth[label].to_s 16} - #{labyrinth[label]}"
end

Z80::TAP.read_chunk('examples/labyrinth.tap').save_tap 'labyrinth'
labyrinth.save_tap('labyrinth', append: true)
Z80::TAP.parse_file('labyrinth.tap') do |hb|
    puts hb.to_s
end
