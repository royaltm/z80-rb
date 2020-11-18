# -*- coding: BINARY -*-
require 'z80'
require 'z80/stdlib'
require 'zxlib/sys'
module ZXUtils
  ##
  # =ZXUtils::Gallery.
  #
  # A program to load from tape and display various ZX Spectrum screen formats.
  #
  # Supported ULAplus formats:
  #
  #        format type      size      mode      colors
  #   /------------------^-------^------------^--------\
  #   | standard screen  |  6912 | screen 0   |     15 |
  #   | standard plus    |  6976 | screen 0   |     64 |
  #   | standard HAM256  |  7680 | screen 0   |    256 |
  #   | high color       | 12288 | hi-color   |     15 |
  #   | high resolution  | 12289 | hi-res     |      2 |
  #   | hi-color plus    | 12352 | hi-color   |     64 |
  #   | hi-color HAM256  | 13056 | hi-color   |    256 |
  #   | stand. interlace | 13824 | screen 0/1 |     15 |
  #   \------------------------------------------------/
  #
  # See: https://faqwiki.zxnet.co.uk/wiki/ULAplus#Extension_to_the_SCR_format
  #
  # This gem provides a tool *zxgallery* that can be used to create a gallery .TAP file from .SCR files.
  #
  # Example:
  #
  # The following will produce *test_gallery.tap* file with the program and the .SCR files as TAP bytes.
  #
  #   zxgallery *.scr -o test_gallery.tap
  # 
  # This program can be also used as a library.
  #
  # Example:
  #   require 'zxlib/basic'
  #   require 'zxutils/gallery'
  # 
  #   class GalleryProg
  #     include Z80
  #     include Z80::TAP
  # 
  #     import              ZXUtils::Gallery
  #     screen              import_file "some_screen.scr"
  #     screen_end          label
  #   end
  # 
  #   program_code = GalleryProg.new 0xC000
  #   start = program_code[:start]
  #   screen = program_code[:screen]
  #   screen_end = program_code[:screen_end]
  #   program = ZXLib::Basic.parse_source <<-EOB
  #      1 DEF FN d(a,s)=USR #{start}: DEF FN l(a)=USR #{start}: DEF FN x()=USR #{start}
  #     20 BORDER 0: PAPER 0: INK 7: CLS
  #     30 RANDOMIZE FN d(#{screen},#{screen_end - screen})
  #     99 BORDER 7: PAPER 7: INK 0: CLS
  #   9998 STOP: GO TO 10
  #   9999 CLEAR #{0x6000 - 1}: LOAD ""CODE : PRINT USR #{start}: RUN
  #   EOB
  #   program.save_tap("gallery", line: 9999)
  #   program_code.save_tap("gallery", append: true)
  class Gallery
    include Z80

    ##
    # =ZXUtils::Gallery Formats
    #
    # The sizes of the supported .SCR file formats.
    module Formats
      SCR_SIZE = 32*192 + 32*24
      SCR_HIGHCOLOR_SIZE = 32*192 + 32*192
      SCR_HIGHRES_SIZE = 32*192 + 32*192 + 1
      SCR_PLUS_SIZE = SCR_SIZE + 64
      SCR_HIGHCOLOR_PLUS_SIZE = SCR_HIGHCOLOR_SIZE + 64
      SCR_256_SIZE = SCR_SIZE + 64*12
      SCR_HIGHCOLOR_256_SIZE = SCR_HIGHCOLOR_SIZE + 64*12
      SCR_INTERLACE_SIZE = SCR_SIZE + SCR_SIZE
    end
    include Formats
    ## The size of the largest .SCR file format supported.
    SCR_MAX_SIZE = SCR_INTERLACE_SIZE

    export start
    export clear
    export ham256_int
    export interlace_int
    export hw_detect_and_setup

    macro_import Stdlib
    label_import ZXLib::Sys, macros: true

    macro :screen_mode do |_, screen_mode, palette: false|
      if screen_mode == 0
                        xor   a
      else
                        ld    a, screen_mode
      end
      if palette
                        call  set_screen_mode_plus
      else
                        call  set_screen_mode
      end
    end

    ##
    # :call-seq:
    #       USR start
    #       DEF FN x()=USR start
    #       DEF FN l(a)=USR start
    #       DEF FN d(a,s)=USR start
    #
    # Gallery API.
    #
    # This endpoint should be invoked from the ZX Basic directly via USR or indirectly via FN.
    #
    #   REM checks for hardware version: 48k or 128k and sets up appropriate delays for HAM256
    #   LET detected=USR start: REM returns 48, 128 or 0 if could not determine
    #   
    #   REM sets screen mode to 0, resets palette mode, clears screen attributes
    #   RANDOMIZE FN x()
    #
    #   REM loads the file from the tape and returns its size
    #   LET size=FN l(address): REM provide an address to load the screen at
    #
    #   REM displays the screen
    #   RANDOMIZE FN d(address,size): REM provide an address of the screen and its size
    ns :start do
                        call  find_def_fn_arg            # is this an FN call with arguments?
                        jr    C, hw_detect_and_setup     # called directly via USR
                        jr    NZ, clear                  # called via FN ()
    end
                                                         # called via FN (a, ...)
    read_args           call  get_uint_arg               # bc: screen address
                        call  find_def_fn_arg.seek_next  # check if exists next argument
                        push  bc                         # screen address
                        jr    NZ, load_screen            # called via FN (a)
                                                         # called via FN (a, s)
                        call  get_uint_arg               # bc: screen length

    show_screen         pop   hl                         # hl: screen address
                        halt                             # vsync to avoid color flicker
                        di
                        call  check_size_and_show
                        ei
                        call  wait_key
                        ex    af, af
                        call  clear
                        ex    af, af
                        ret   NC
                        report_error 'D BREAK - CONT repeats'

    ns :clear do
                        halt # vsync to avoid color flicker
                        di
                        screen_mode 0
                        clrmem mem.attrs, mem.attrlen, 0
                        ei
                        ret
    end

    # workspace and data address on stack
    ns :load_screen do
                        pop   ix               # ix: workspace address
      header_loop       push  ix               # save address
                        ld    de, 17           # header descriptor length
                        xor   a                # load header
                        scf
                        call  rom.ld_bytes
                        pop   ix               # restore address
                        jr    NC, header_loop
                        ld    a, 3             # a: 3 (CODE type)
                        cp    [ix + 0]         # check descriptor type
                        jr    NZ, header_loop  # no match? try again
                        push  ix               # save address

                        dec   a                # a: 2
                        call  rom.chan_open    # open #2
                        pop   hl               # descriptor address
                        ld    b, 10            # name length
      print_loop        inc   hl
                        ld    a, [hl]          # print name
                        rst   0x10
                        djnz  print_loop
                        ld    a, ?\r.ord
                        rst   0x10

                        inc   hl
                        ld    e, [hl]          # data length LSB
                        inc   hl
                        ld    d, [hl]          # data length MSB
                        push  de               # save length
                        ld    a, 0xFF          # load data
                        scf
                        call  rom.ld_bytes
                        report_error_unless C, "R Tape loading error"
                        pop   bc               # bc: screen length
                        ret
    end

    ns :hw_detect_and_setup do
                        di
                        ld    a, 0xC9
                        ld    [0xFFFF], a
                        ld    a, 0x3B
                        ld    i, a
                        im2
      dloop             xor   a
                        ei
                        halt
                        ld    r, a
                        ei
                        halt
                        ld    a, r       # A=59 on 48k and A=57 on 128k
                        cp    58
                        jr    Z, dloop
                        cp    59
                        jr    Z, detected48k
                        cp    57
                        jr    Z, detected128k
                        # just don't change if unknown hardware
                        ld    bc, 0      # return value
      exit_bc           push  bc
                        screen_mode 0
                        pop   bc
                        ei
                        ret

      detected48k       ld    hl, 480
                        ld    a, 23
                        ld    bc, 48     # return value
                        jr    set_delays

      detected128k      ld    hl, 481
                        ld    a, 28
                        ld    bc, 128    # return value
      set_delays        ld    [ham256_int.delay_a], hl
                        ld    [ham256_int.pad_delay_a], a
                        jr    exit_bc
    end

    # HL: screen address, BC: screen length
    ns :check_size_and_show do
      [
        SCR_SIZE, show_normal,
        SCR_HIGHCOLOR_SIZE, show_highcolor,
        SCR_HIGHRES_SIZE, show_highres,
        SCR_PLUS_SIZE, show_plus,
        SCR_HIGHCOLOR_PLUS_SIZE, show_highcolor_plus,
        SCR_256_SIZE, show_256,
        SCR_HIGHCOLOR_256_SIZE, show_highcolor_256,
        SCR_INTERLACE_SIZE, show_interlace,
      ].each_slice(2)
      .sort_by { |size,| size }
      .group_by { |size,| size >> 8 }
      .each_pair do |match_msb, entries|
        ns do |eoc|
                        ld    a, b
                        cp    match_msb
                        jr    NZ, eoc
                        ld    a, c
          entries.each do |match_size, show|
            # puts match_size.to_s(16)
            match_lsb = match_size & 0xFF
            if match_lsb == 0
                        ora   a
            else
                        cp    match_lsb
            end
                        jp    Z, show
          end
        end
      end
      error_exit        ei
                        report_error "Q Parameter error"
    end

    find_def_fn_arg     find_def_fn_args 1, cf_on_direct: true

    get_uint_arg        read_positive_int_value b, c
                        inc  hl  # point to a next argument possibly
                        ret  Z
    error_a             report_error 'A Invalid argument'

    # CF: 1=BREAK, 0=other key
    ns :wait_key do
                        call  release_key
                        ret   C
      wait_loop         call  check_key_pressed
                        jr    Z, wait_loop
                        # jr    release_key
    end

    ns :release_key do
                        call  check_key_pressed
                        ret   Z
                        call  rom.break_key
                        ccf
                        ret   C
                        jr    release_key
    end

    ns :check_key_pressed do
                        halt
                        key_pressed?
                        ret
    end

    ns :show_normal do
                        screen_mode 0
      copy_screen       memcpy mem.screen, hl, mem.scrlen + mem.attrlen
                        ret
    end

    ns :show_highcolor do
                        screen_mode 2
      copy_screens      memcpy memT2k.screen0, hl, mem.scrlen
                        memcpy memT2k.screen1, hl, mem.scrlen
                        ret
    end

    ns :show_highres do
                        screen_mode 6
                        call  show_highcolor.copy_screens
                        ld    a, [hl]
                        ora   6
                        # jp    set_screen_mode
    end
    # A: screen mode on bits 0-5
    ns :set_screen_mode do
                        anda  io_plus.mode_mask
                        out   (ioT2k.ula), a

                        ld    bc, io_plus.reg
                        ora   io_plus.mode_group
                        out   (c), a     # select group mode | screen mode
                        xor   a          # palette mode off
      set_palette       ld    b, io_plus.dta >> 8
                        out   (c), a     # set palette mode

                        restore_rom_interrupt_handler enable_intr: false
                        ret
    end
    # A: screen mode on bits 0-5
    ns :set_screen_mode_plus do
                        anda  io_plus.mode_mask
                        ld    ixl, a     # save screen mode
                        out   (ioT2k.ula), a

                        ld    de, (io_plus.reg & 0xFF00) | ((io_plus.dta >> 8) & 0x00FF)
                        ld    c, io_plus.reg & 0xFF

                        xor   a          # black palette entries
                        ex    af, af     # save the entry
                        xor   a          # enumerate from zero

      cloop             ld    b, d       # register port
                        out   (c), a     # palette entry # to write
                        ld    b, e       # data port
                        ex    af, af     # save the count
                        out   (c), a     # set to black
                        ex    af, af     # restore the count
                        inc   a          # next palette entry
                        cp    io_plus.mode_group
                        jr    NZ, cloop  # repeat until all 64 have been done

                        ld    b, d       # select register port
                        ora   ixl        # include screen mode
                        out   (c), a     # select mode group | screen mode
                        ld    a, 1       # RGB mode (2 for grey)
                        jr    set_screen_mode.set_palette
    end

    ns :show_plus do
                        screen_mode 0, palette: true
                        call  show_normal.copy_screen
                        jr    read_screen_palette
    end

    ns :show_highcolor_plus do
                        screen_mode 2, palette: true
                        call  show_highcolor.copy_screens
                        # jr    read_screen_palette
    end
    # HL: palette
    ns :read_screen_palette do
                        ld    de, (io_plus.reg & 0xFF00) | (((io_plus.dta >> 8) + 1) & 0x00FF)
                        ld    c, io_plus.reg & 0xFF
                        xor   a          # count starts at zero

      ploop             ld    b, d       # register port
                        out   (c), a     # palette entry to write
                        ld    b, e
                        outi             # set the palette value <- [hl]
                        inc   a          # next palette entry
                        cp    64         # are we nearly there yet?
                        jr    C, ploop   # repeat until all 64 have been done
                        ret
    end

    ns :show_256 do
                        screen_mode 0, palette: true
                        call  show_normal.copy_screen
      enable_ham256     ld    [ham256_int.palette_a], hl # start of palette entries
                        ld    hl, ham256_int
                        jr    setup_interrupt
    end

    ns :show_highcolor_256 do
                        screen_mode 2, palette: true
                        call  show_highcolor.copy_screens
                        jr    show_256.enable_ham256
    end

    ns :show_interlace do
                        screen_mode 0
                        call  show_normal.copy_screen
                        memcpy memT2k.screen1, hl, mem.scrlen + mem.attrlen
                        ld    hl, interlace_int
                        # jp    setup_interrupt
    end

    ns :setup_interrupt do
                        setup_custom_interrupt_handler hl, enable_intr: false, vector_page: 0x3B
                        ret
    end

    ns :increase_frames do
                        ld   hl, vars.frames        # ++frames
                        inc  [hl]
                        ret  NZ
                        inc  l
                        inc  [hl]
                        ret  NZ
                        inc  l
                        inc  [hl]
                        ret
    end

    with_saved :interlace_int, af, bc, hl, ret: :after_ei do |_|
                        ld    a, [vars.frames]
                        anda  1
                        out   (ioT2k.ula), a

                        ld    bc, io_plus.reg
                        ora   io_plus.mode_group
                        out   (c), a     # select group mode | screen mode
                        call  increase_frames
    end

  # 43
  # 4*11+4+4+4*11=96
    with_saved :ham256_int, :all_but_ixiy, :ex_af, :exx, :all_but_ixiy, ret: :after_ei do |_|
                        ld    de, [0]     # padding 20 t-states
      delay_p           ld    hl, 480     # wait until just before the row
      delay_a           delay_p + 1

      delay             dec   hl
                        ld    a, h
                        ora   l
                        jr    NZ, delay   # wait until near top of main screen

      palette_p         ld    hl, 0       # colour data
      palette_a         palette_p + 1
                        ld    de, 0xbf00  # D = register select, E = data port (+1 due to OUTI)
                        ld    c, 0x3b     # C = ZXI base port

                        xor   a           # first CLUT entry is zero

                        exx
                        ld    de, 0x3f0c  # D = counter mask for 2 rows, E = 12 pairs of rows

      loop1             ld    b, 16       # B=16 (x2 from below) entries to set

      loop2             exx
      2.times do
                        ld    b, d        # register port
  # 43+96+20+10+(6+4+4+12)*480-5+10+10+7+4+4+10+7+4+4
  # 12704 12805 .. 14219 .. 14338 .. 15853 (16106) .. 16288
  # 42+96+20+10+(6+4+4+12)*481-5+10+10+7+4+4+10+7+4+4
  # 12729 12830 .. 14244 .. 14363                     16377 (-16313=64)
                        out   (c), a      # set CLUT
                        ld    b, e        # data port
                        outi              # set colour
                        inc   a           # next CLUT entry
      end
                        exx
                        djnz  loop2       # finish row

                        anda  d
                        jr    NZ, loop1   # finish pair

      pad_delay_p       ld    b, 23       # timing for 48k
      pad_delay_a       pad_delay_p + 1
      delayme           djnz  delayme     # delay before next row
                        nop               # 4 T-states

                        dec   e
                        jr    NZ, loop1   # finish screen

    end
                        call  increase_frames
                        # jp    rom.mask_int
  end
end


if __FILE__ == $0
  require 'zxlib/basic'

  $screen = ARGV.shift || File.expand_path(
      File.join('..', '..', 'examples', 'screens', 'test_ham256.scr'),
      __dir__)

  class GalleryProg
    include Z80
    include Z80::TAP

    import              ZXUtils::Gallery
    screen              import_file $screen
    screen_end          label
  end

  program_code = GalleryProg.new 0xC000
  start = program_code[:start]
  screen = program_code[:screen]
  screen_end = program_code[:screen_end]
  program = ZXLib::Basic.parse_source <<-EOB
     1 DEF FN d(a,s)=USR #{start}: DEF FN l(a)=USR #{start}: DEF FN x()=USR #{start}
    20 BORDER 0: PAPER 0: INK 7: CLS
    30 RANDOMIZE FN d(#{screen},#{screen_end - screen})
    99 BORDER 7: PAPER 7: INK 0: CLS
  9998 STOP: GO TO 10
  9999 CLEAR #{0x6000 - 1}: LOAD ""CODE : PRINT USR #{start}: RUN
  EOB

  puts program_code.debug[0..408]
  puts "code size: #{ZXUtils::Gallery.code.bytesize}"
  # puts program_code.labels
  puts program

  program.save_tap("test.gallery", name: "gallery", line: 9999)
  program_code.save_tap("test.gallery", name: "gallery", append: true)

  Z80::TAP.parse_file("test.gallery.tap") { |hb| puts hb.to_s }
end
