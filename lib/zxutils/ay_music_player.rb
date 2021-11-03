require 'zxutils/ay_music'

module ZXUtils
  ##
  # ===AY-3-8910/8912 music player
  #
  # The music module player based on ZXUtils::AYMusic engine.
  #
  # ZXUtils::MusicBox provides a Ruby DSL for creating music modules for the AYMusic engine.
  #
  # The player expects a music module in the format produced by MusicBox::Song.to_player_module.
  #
  # ====The format of the music module
  #
  # The +delta+ is an address of the second (most significant) delta's byte subtracted from the addressed entry.
  # When the +delta+ value will be added to the address of the 2nd delta's byte address it should resolve to
  # the indicated entry address.
  #
  # +index_table+ can have 0 or more, up to 128, entries.
  #
  #    +---------------------------+
  #    | track_a delta             | 2 bytes
  #    +---------------------------+
  #    | track_b delta             | 2 bytes
  #    +---------------------------+
  #    | track_c delta             | 2 bytes
  #    +---------------------------+
  #    | index_table entry 1 delta | 2 bytes
  #    +---------------------------+
  #                 ...
  #    +---------------------------+
  #    | index_table entry N delta | 2 bytes
  #    +---------------------------+
  #    |  AYMusic data for tracks, |
  #    |  envelopes, masks, chords |
  #    +---------------------------+
  #
  # ====Use player from your program
  #
  # 1. Call AYMusicPlayer.setup routine only once. It gets overwritten by the data it creates for the player.
  # 2. Call AYMusicPlayer.init to initialize music module. This may be called unlimited times.
  # 3. Call AYMusic.play on each tick to play the music.
  # 4. Call AYMusicPlayer.mute_sound to silence the AY's sound.
  #
  #   require 'zxutils/ay_music_player'
  #   class Program
  #       include Z80
  #       include Z80::TAP
  #       include ZXUtils
  #   
  #       start         call player.setup
  #                     ld   hl, music_module
  #                     call player.init
  #       forever       halt
  #                     di
  #                     push iy
  #                     call player.play
  #                     pop  iy
  #                     ei
  #                     jr   forever
  #       music_module  import_file "examples/test_music.tap"
  #                     import AYMusicPlayer, :player
  #       # reserve memory here which will be used by the player
  #       # the end of the reserved space is indicated by the player.sincos_end label
  #       # there may be 0 or more bytes available between player.workspace_end and player.sincos
  #       # depending on the code address as player.sincos must be aligned to 256 bytes.
  #   end
  #   
  #   program = Program.new(0x8000)
  #   puts program.debug
  #   program.save_tap('miniplayer')
  #
  class AYMusicPlayer
    include Z80
    include Z80::TAP

    export setup
    export mute_sound
    export init
    export play
    export music
    export music_end
    export workspace_end
    export sincos
    export sincos_end

    SinCos      = AYMusic::SinCos
    SinCosTable = AYMusic::SinCosTable

    import              ZXLib::Sys, macros: true, code: false
    macro_import        MathInt
    macro_import        Utils::SinCos
    macro_import        ZXLib::AYSound
    macro_import        AYMusic

    play          music.play

    ## The struct of MusicTracks.tracks_info
    class TrackInfo < Label
      track_a     word
      track_b     word
      track_c     word
    end

    ## The struct representing music module header.
    class MusicTracks < Label
      track_info        TrackInfo
      index_table       word, 128
    end

    AY_MUSIC_OVERRIDE = { io_ay: io_ay,
                          index_table: index_table,
                          sincos: sincos, sincos_end: sincos_end } # :nodoc:

    ##
    # Initialize music module.
    #
    # Relocates index table and sets the tracks' cursors to the initial positions.
    # Module data is not being modified.
    #
    # * +hl+:: should point to the MusicTracks data.
    #
    # You may also call the +init.restart+ entry to reset tracks to the beginning
    # of the last initialized module.
    #
    # _NOTE_:: This routine will disable and enable interrupts before it's finished.
    #
    # Modifies: +af+, +bc+, +de+, +hl+, +ix+ and 1 stack entry.
    ns :init do
                        ld   de, track_info
                        ld   a, 3
                        call relocate16
                        ld   a, 128
                        ld   de, index_table
                        call relocate16
      restart           di
                        call music.init
      track_info        data TrackInfo, 1
                        ei
      if AYMusic::READ_ONLY_CODE
                        ld   hl, index_table
                        ld   [music.music_control.index_table], hl
      end
                        ret
    end

    # hl - source, de - destination, a - entries 
    ns :relocate16 do # :nodoc:
      rloop             ld   c, [hl]
                        inc  hl
                        ld   b, [hl]
                        add  hl, bc
                        # TODO: error if C
                        ex   de, hl
                        ld   [hl], e
                        inc  hl
                        ld   [hl], d
                        inc  hl
                        ex   de, hl
                        sbc  hl, bc
                        inc  hl
                        dec  a
                        jr   NZ, rloop
                        ret
    end

    ##
    # Mutes sound.
    #
    # Modifies: +af+, +bc+.
    ns :mute_sound do
                        ay_init(io_ay:io_ay)
                        ret
    end

    index_table         label 2

    ##
    # Sets up the player.
    #
    # Call this _ONCE_ the player code has been loaded to create required tables for the music player.
    #
    # _NOTE_:: The following code gets overwritten once it is being called.
    #
    # _NOTE_:: This routine will disable and enable interrupts before it's finished.
    #
    # Modifies: +af+, +bc+, +de+, +hl+, +af'+, +bc'+, +de'+, +hl'+, +ix+ and 1 stack entry.
    ns :setup do
                        call setup_tone_progress
                        ay_music_note_to_fine_tone_cursor_table_factory(music.note_to_cursor, play:music.play)
                        ay_expand_notes(music.notes, octaves:8, half_tones:12)
                        create_sincos_from_sintable(sincos, sintable:sintable)
      sintable          bytes neg_sintable256_pi_half_no_zero_lo
    end

    padding = index_table[128].to_i - pc
    sanity_pad          bytes padding

    if -padding > AYMusic::MINISTACK_SIZE
      ns use: :index_table do
        ministack       union index_table[128] + AYMusic::MINISTACK_SIZE, 2
        AY_MUSIC_OVERRIDE[:ministack] = ministack
      end
      workspace_end     music.music_control[1]
    else
      workspace_end     music.workspace_end
    end

    ###########
    # AYMusic #
    ###########

    import              AYMusic, :music, override: AY_MUSIC_OVERRIDE
    music_end           label

    NOTES = ay_tone_periods(min_octave:0, max_octave:0, notes_hz:equal_tempered_scale_notes_hz(hz: 440), clock_hz:ZXLib::AYSound::CLOCK_HZ) # :nodoc:
                        dw NOTES[11]*2
                        dw NOTES[0...12]

    # The following code gets overwritten on setup.
    setup_tone_progress ay_music_tone_progress_table_factory(music.fine_tones, hz: 440, subroutine: true)

    sincos              union workspace_end, SinCos, align: 0x100
    sincos_end          sincos + (+SinCosTable)
    end_of_code         label
  end

  ##
  # ===AY-3-8910/8912 Basic player
  #
  # This is a wrapper over AYMusicPlayer with interfaces suitable to be used directly from the ZX Spectrum's BASIC.
  # 
  #   require 'zxutils/ay_music_player'
  #   require 'zxlib/basic'
  #   include ZXLib
  #   include ZXUtils
  #
  #   player = AYBasicPlayer.new 0xEED3
  #   puts player.debug
  #   program = Basic.parse_source <<-EOC
  #      1 DEF FN m(a)=USR #{player[:init_music]}
  #     10 CLS: PRINT "Insert tape with a music module"
  #     20 LOAD ""CODE 32768
  #    100 REM initialize music
  #    110 RANDOMIZE FN m(32768)
  #    200 REM play in loop
  #    210 PRINT USR #{player[:play_loop]}
  #    299 STOP
  #    300 REM play interval
  #    310 LET ticks=USR #{player[:play_interval]}: PAUSE 1: GO TO 310
  #    400 REM mute sound
  #    410 RANDOMIZE USR #{player[:mute_sound]}
  #    499 STOP
  #    500 REM current counter
  #    510 PRINT USR #{player[:get_counter]}
  #   9998 STOP: RUN
  #   9999 CLEAR 32767: LOAD "player"CODE: RANDOMIZE USR #{player[:init_music]}: RUN
  #   EOC
  #   puts program.to_source escape_keywords: true
  #   program.save_tap "player", line: 9999
  #   player.save_tap "player", append: true
  class AYBasicPlayer
    include Z80
    include Z80::TAP

    export setup
    export mute_sound
    export init_music
    export init
    export play_loop
    export play_interval
    export get_counter
    export check_key
    export release_key
    export music
    export music_end
    export workspace_end
    export sincos
    export sincos_end

    import              ZXLib::Sys, macros: true, code: false
    macro_import        ZXLib::AYSound
    macro_import        AYMusic

    ##
    # Initializes music track. Sets up the player.
    #
    # To setup the player (once):
    #
    #   RANDOMIZE USR #{player[:init_music]}
    #
    # To initialize a music module:
    #
    #   1 DEF FN m(a)=USR #{player[:init_music]}
    #   LOAD ""CODE 40000
    #   PRINT FN m(40000)
    ns :init_music do
                        find_def_fn_args 1, subroutine:false, cf_on_direct:false do
                            with_saved :once, :exx, hl, ret: true do
                              call  setup
                              ld    hl, 0x19CF # 0xCF 0x19 : report_error 'Q Parameter error'
                              ld    [once], hl
                            end
                        end
                        read_positive_int_value d, e
                        ex   de, hl
                        jr   Z, init
                        report_error 'A Invalid argument'
    end

    ##
    # Plays music track in a loop until any key has been pressed.
    ns :play_loop do
                        call release_key
      forever           halt
                        call play_interval
                        call check_key
                        jr  Z, forever
                        call release_key
                        push bc
                        call mute_sound
                        pop  bc
                        ret
    end

    check_key           key_pressed?
                        ret
    release_key         call check_key
                        ret  Z
                        jr   release_key

    ##
    # Plays single music track tick. Call repeatedly on equal intervals to play music.
    #
    # Returns the current value of the music counter.
    ns :play_interval do
                        exx
                        push hl
                        di
                        push iy
                        ay_music_preserve_io_ports_state(music.music_control, music.play, bc_const_loaded:false, io_ay:io_ay)
                        call music.play
                        pop  iy
                        ei
                        pop hl
                        exx
    end

    ##
    # Returns the current value of the music counter.
    ns :get_counter do
                        ld   bc, [music.music_control.counter]
                        ret
    end

    import              AYMusicPlayer, override: { io_ay: io_ay }
    end_of_code         label
  end
end


if __FILE__ == $0
  require 'zxlib/basic'
  include ZXUtils

  player = AYBasicPlayer.new 0xEED3
  puts player.debug
  puts "AYMusic size: #{player[:music_end] - player[:music]}"
  puts "TRACK_STACK_DEPTH: #{AYMusic::TRACK_STACK_DEPTH}"
  puts "TRACK_STACK_SIZE : #{AYMusic::TRACK_STACK_SIZE}"
  puts "TRACK_STACK_TOTAL: #{AYMusic::TRACK_STACK_TOTAL}"
  puts "MINISTACK_DEPTH:   #{AYMusic::MINISTACK_DEPTH}"
  puts "MINISTACK_SIZE:    #{AYMusic::MINISTACK_SIZE}"
  puts "workspace_end - end_of_code: #{player[:workspace_end] - player[:end_of_code]}"
  puts "sincos_end - end_of_code: #{player[:sincos_end] - player[:end_of_code]}"
  puts "sincos - workspace_end: #{player[:sincos] - player[:workspace_end]}"
  %w[
    +setup
    init_music
    init
    mute_sound
    get_counter
    play_loop
    play_interval
    music.index_table
    setup
    end_of_code
    music
    music_end
    music.init
    music.play
    music.notes
    music.note_to_cursor
    music.fine_tones
    music.workspace
    music.track_stack_end
    music.music_control
    music.ministack
    workspace_end
    music.sincos
    sincos
    music.sincos_end
    sincos_end
  ].map {|l| [l.to_s, player[l]]}.sort_by{|_,v| v}.each do |label, value|
    puts "#{label.ljust(30)}: 0x#{'%04x'%value} - #{value}"
  end

  include ZXLib
  program = Basic.parse_source <<-EOC
     1 DEF FN m(a)=USR #{player[:init_music]}
    10 CLS: PRINT "Insert tape with a music module"'"and press any key": PAUSE 0
    20 LOAD ""CODE 32768
   100 REM initialize music
   110 RANDOMIZE FN m(32768)
   200 REM play in loop
   210 PRINT USR #{player[:play_loop]}
   299 STOP
   300 REM play interval
   310 LET ticks=USR #{player[:play_interval]}: PAUSE 1: GO TO 310
   400 REM mute sound
   410 RANDOMIZE USR #{player[:mute_sound]}
   499 STOP
   500 REM current counter
   510 PRINT USR #{player[:get_counter]}
  9998 STOP: RUN
  9999 CLEAR 32767: LOAD "player"CODE: RANDOMIZE USR #{player[:init_music]}: RUN
  EOC
  puts program.to_source escape_keywords: true

  tap_name = 'test.zxutils.ay_music_player.tap'
  program.save_tap tap_name, name:'player', line: 9999
  player.save_tap tap_name, name:'player', append: true
  puts "TAP #{tap_name}:"
  Z80::TAP.parse_file(tap_name) do |hb|
      puts hb.to_s
  end
end
