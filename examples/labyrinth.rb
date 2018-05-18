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
  label_import  ZXSys

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

  start           jr   example

  labyrinth       data Labyrinth, { width: 31, height: 21, room_data: room_data }

  ns :example do
                  exx
                  push hl # save hl'

                  call init_labyrinth
                  jr   NZ, continue
                  rst  0x08
                  db   0xA # ERROR: B Integer Out Of Range
    continue      call clear_labyrinth
                  call carve_labyrinth
                  exx
                  push hl # save seed

                  ld   a, [vars.attr_p]
                  call clear_screen
                  ld   a, [vars.bordcr]
                  call set_border

                  call color_labyrinth
                  call draw_labyrinth

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
          set  6, [hl]
  end

  macro :is_marked do |_, room=hl|
          bit  6, [hl]
  end

  macro :is_boundary do |_, room=hl|
          bit  7, [hl]
  end

  macro :move_room do |eoc, rwidth:e, mov:d|
          ld   a, mov
          anda 0b11
          jr   NZ, ck_rt
          # 0=up
          sub_from rwidth, h, l
          jr   eoc
    ck_rt dec  a
          jr   NZ, ck_dn
          # 1=right
          inc  hl
          jr   eoc
    ck_dn dec  a
          jr   NZ, is_lt
          # 2=down
          ld   a, rwidth
          adda_to h, l
          jr   eoc
          # 3=left
    is_lt dec  hl
  end

  # initializes labyrinth variables: room_00, nrooms and bsize
  # call after changing width, height or room_data
  # validates labyrinth dimensions
  # ZF=1 if invalid dimensions (width==0 or height==0)
  ns :init_labyrinth do
                ld   hl, [labyrinth.room_data]
                ld   bc, [labyrinth.dimensions] # c = height, b = width
                ld   a, c
                ora  a
                ret  Z
                ld   a, b
                ora  a
                ret  Z
                inc  a
                adda_to h, l
                ld   [labyrinth.room_00], hl # room_00 = room_data + width + 1
                mul8 0, c, b, tt:de, clrhl:true, double:false
                ld   [labyrinth.nrooms], hl
                add  hl, bc   # bsize = nrooms + height
                ld   [labyrinth.bsize], hl
                ora  a        # ZF = 0
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
                ld   e, a
                inc  e               # width + 1 (rowskip width)
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
                move_room rwidth:e, mov:d
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
  # WARNING: labyrinth must have 0 < width <= 31
  ns :color_labyrinth do
                ld   hl, [labyrinth.room_00]
                ld   de, mem.attrs
                ld   bc, [labyrinth.dimensions]
                ld   a, 24
                cp   c
                jr   NC, loop1
                ld   c, a
    loop1       push bc
    loop0       is_marked
                inc  hl
                jr   Z, skip_mark
                ld   a, [de]
                ora  0b01000000
                ld   [de], a
    skip_mark   inc  de
                djnz loop0
                inc  hl
                ld   a, 32
                pop  bc
                sub  b
                adda_to d, e
                dec  c
                jr   NZ, loop1
                ret
  end

  # draws labyrinth on the screen
  # WARNING: labyrinth must have 0 < width <= 31
  ns :draw_labyrinth do
                ld   hl, [labyrinth.room_00]
                ld   de, mem.screen
                ld   bc, [labyrinth.dimensions]
                inc  c   # height with boundary

    main_loop   push de
                push hl
    hloop       ld   a, 0x80
                bit  0, [hl]
                inc  hl
                jr   NZ, is_openup
                ld   a, 0xFF
    is_openup   ld   [de], a
                inc  de
                djnz hloop
                ex   de, hl
                ld   [hl], 0x80

                pop  hl
                pop  de   # screen
                nextline d, e, true

                dec  c
                ret  Z
                ld   a, [labyrinth.width]
                ld   b, a
                push bc

                ld   c, b
                inc  c    # row width with boundary
    vloop       push de
                xor  a
                bit  1, [hl]
                inc  hl
                jr   NZ, is_openleft
                ld   a, 0x80
    is_openleft ld   b, 7
    vdraw_loop  ld   [de], a
                ex   af, af
                nextline d, e, vdraw_over
                ex   af, af
                djnz vdraw_loop
    vdraw_over  dec  c
                jr   Z, quit_vert
                pop  de
                inc  de
                jr   vloop

    quit_vert   pop  af   # drop saved screen
                ld   a, b
                anda a    # is b == 0?
                pop  bc
                ret  NZ   # nope? return now
                sub_from b, d, e
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
['labyrinth.height', 'labyrinth.width', 'labyrinth.room_data'].each do |label|
  puts "#{label.ljust(20)}: 0x#{labyrinth[label].to_s 16} - #{labyrinth[label]}"
end

# labyrinth.save_tap('labyrinth')

Z80::TAP.read_chunk('examples/loader.tzx') do |hb|
    File.open('examples/labyrinth.tap', 'wb') do |f|
        f.write hb.to_tap
        f.write labyrinth.to_tap('labyrinth')
    end
end
