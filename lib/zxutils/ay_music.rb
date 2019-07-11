require 'z80'
require 'z80/stdlib'
require 'z80/utils/sincos'
require 'zxlib/sys'
require 'zxlib/ay_sound'

module ZXUtils
  ##
  # ===The AY-3-8910/8912 music engine
  #
  # Low-level but highly configurable music player routines and Macros.
  # See also: ZXUtils::AYMusicPlayer and ZXUtils::AYBasicPlayer.
  #
  # To play music with AYMusic you'll need:
  # * Some static tables.
  # * Some workspace memory.
  # * Music data.
  #
  # ZXUtils::MusicBox provides a Ruby DSL for creating music for the AYMusic engine.
  #
  # ====A memory map of the AYMusic's workspace.
  #
  # By default the workspace addresses follows immediately the static tables which are allocated after
  # the end of the AYMusic code.
  # All of the workspace and static tables' labels can be overridden to better fit your program's memory layout.
  #
  #   <-  TRACK_STACK_TOTAL  ->                 <- +MusicControl ->      <- MINISTACK_SIZE ->
  #   +---------------------+-+                 +-----------------+      +------------------+
  #   |  Tracks' loop/yield |0|                 |  Music Control  |      | Player's machine |
  #   |        Stacks       | |                 |                 |      |    code stack    |
  #   +---------------------+-+                 +-----------------+      +------------------+
  #   ^                      ^ ^                ^                                            ^
  #   workspace              | track_stack_end  music_control                        ministack
  #         empty_instrument-+                                                   workspace_end
  #
  # ====Static tables
  #
  # * A note to AY-3-891x tone pitch table. Optionally override +notes+ label to point to that table.
  # * A note to fine tones cursor table. Optionally override +note_to_cursor+ label to point to that table.
  # * A fine tones table for tone progression. Optionally override +fine_tones+ label to point to that table.
  # * A 1kb Z80::Utils::SinCos::SinCosTable which must be aligned to 256 bytes (the address must be divisible by 256).
  #   Optionally override +sincos+ label to point to that table.
  #
  # The "note to AY-3-891x tone pitch" table is a 96 words each representing a tone in a 12-notes, 8-octaves music scale.
  # The values should be 12-bit tone period values as expected by the AY-3-891x specification.
  # 
  # There are two ways to create such a table:
  #
  # * using ZXLib::AYSound::Macros.ay_tone_periods macro to create a full static table.
  # * using ZXLib::AYSound::Macros.ay_tone_periods macro to create a one octave and extrapolate it with
  #   ZXLib::AYSound::Macros.ay_expand_notes routine.
  #
  # The "note to fine tones cursor" table may be created with the Macros.ay_music_note_to_fine_tone_cursor_table_factory
  # routine.
  #
  # The "fine tones" table may be created with the Macros.ay_music_tone_progress_table_factory routine.
  #
  # The SinCosTable can be created with the Z80::Utils::SinCos::Macros.create_sincos_from_sintable routine.
  #
  # ====Music data
  #
  # * Some tracks, at least 3. See AYMusic.init how to initialize tracks.
  # * Some delta envelopes, mask envelopes, chords.
  # * An index lookup table. Optionally override +index_table+ label to point to that table.
  #
  # =====Index lookup table
  #
  # The index lookup table consists of words (2 bytes each) containing addresses of tracks, instruments, envelopes
  # and chords. Each entry is indexed from 1 (1st entry) to 128. The maximum number of entries supported currently
  # is 128. However if your music uses fewer entries, the lookup table may be shorter.
  #
  # Instruments are just like tracks, but some commands should not be used in them, e.g. playing notes.
  # For each channel, the instrument track is being executed in parallel with the current track.
  #
  # =====Envelopes, Masks and Chords
  #
  # Delta envelopes, masks and chords consist of a loop offset byte, followed by bytes describing an envelope,
  # followed by 0. The loop offset should point to the argument's 1st byte (relative to the 1st argument) to which
  # the envelope should loop when it's over. The loop offset = 0 means repeat the whole envelope.
  #
  # Each delta envelope argument consist of 2 bytes:
  # * A counter: from 1 up to 255.
  # * A delta as a twos complement byte. 
  #
  # The counter indicates for how many ticks the following delta should be applied.
  # The delta is being added to the current envelope value in the range: 0..255. If the value exceeds 255 it's being clipped to 255. If the value drops below 0 it's being clipped to 0.
  #
  # If the delta envelope is being applied to a volume, the current highest 4 bits of the envelope value is being applied to a AY-3-891x's channels' volume.
  # If the delta envelope is being applied to a noise pitch, the current highest 5 bits of the envelope value is being applied to a AY-3-891x's channels' volume.
  #
  # Each mask envelope argument consist of 2 bytes:
  # * A counter: from 1 up to 255.
  # * An 8-bit mask.
  #
  # The counter indicates for how many ticks the following bits should be applied in turn.
  # Each bit from the mask is being applied after each tick, starting from the most (leftmost)
  # significant bit (7). The bits are being rolled left, creating a virtually infinite bitmap.
  #
  # Each chord argument consist of a single byte.
  # * A delay value on bits 7-5 (1..7).
  # * A half-tone delta value on bits 4-0 (0..31).
  #
  # The delay indicates for how many ticks the following tone will be played. The delta is a half-tone delta up from the currently played note.
  #
  # =====Tracks
  # 
  # Tracks consist of commands. Each command consist of 1 or more bytes. Some commands have additional data embedded in the 1st byte.
  # At the start each of the 3 AY-3-8912 tone channel has a single main track assigned. Each volume or tone related command on
  # the assigned track is always tied to that channel.
  # Each of the 3 channels may have an instrument track attached which will be run in parallel to the main track on that channel.
  # Depending on the play mode the "Play note" command may reset the instrument track to its beginning.
  #
  # The list of commands:
  #
  #   Head    Data                Description
  #     0     -                   Terminate a track.
  #                               After this command a track is considered finished. If the control was delegated to 
  #                               this track from another track the control is being given back to the yielding track.
  #                               For instrument tracks it just freezes the track, but in play mode 1 the track will
  #                               be restarted on each "Play a note" command.
  #     1- 96 -                   Play a note.
  #                               A note: 1: a0, 2: a#0, 3: b0, 4: c0, 5: c#0, 6: d0, 7: d#0, 8: e0, 9: f0, 10: f#0,
  #                                       11: g0, 12: g#0, 13: a1, ... , 96: g#8
  #   128-159 -                   Set noise pitch: (head - 128) translates to pitch: 0..31.
  #   160-175 -                   Set volume level: (head - 160) translates to volume: 0..15.
  #   176-255 -                   Wait ticks: (head - 175) translates to delay: 1..80 ticks.
  #    97     index:1             Set instrument. Sets indicated track as an instrument.
  #                               Followed by a 1 byte lookup index (0: set empty instrument, 1-128: from the lookup table).
  #                               The instrument begins to play on next played note in mode 1.
  #    98     args:1|2            Wait more ticks (between 81 and 20736). Followed by 1 or 2 bytes.
  #                               If delay is in the range: 81..256 ticks only one byte argument follows: ticks - 1 [80..255].
  #                               If delay is in the range: 257..20736 ticks the first argument byte is: ((ticks - 1) >> 8) - 1 [0..79]
  #                               and the second argument byte is: (ticks - 1) & 255 [0..255].
  #    99     -                   Sets play mode 1. In this mode "play note" command resets instrument's track cursor
  #                               to its beginning. Instrument track plays in parallel to the main track.
  #   100     -                   Sets play mode 2. In this mode "play note" only changes the frequency of the note.
  #                               Instrument track continues to play in parallel.
  #   101     duration:2          Sets AY-3-891x envelope duration. 2 byte duration follows (LSB/fine first).
  #   102     shape:1             Sets AY-3-891x envelope shape. A 1 byte shape follows. See ZXLib::AYSound for envelope shapes.
  #   103     index:1             Start a volume envelope.
  #                               Followed by a 1 byte lookup index (0: disable envelope, 1-128: from the lookup table).
  #   104     index:1             Start a noise envelope.
  #                               Followed by a 1 byte lookup index (0: disable envelope, 1-128: from the lookup table).
  #   105     index:1             Start a chord.
  #                               Followed by a 1 byte lookup index (0: disable chord, 1-128: from the lookup table).
  #   106     index:1             Start and apply mask envelope to a AY-3-891x envelope volume control.
  #                               Bit = 1 is envelope, 0 is volume.
  #                               Followed by a 1 byte lookup index (0: disable mask, 1-128: from the lookup table).
  #   107     index:1             Start and apply mask envelope to a channel's tone on/off control.
  #                               Bit = 1 is off, 0 is on.
  #                               Followed by a 1 byte lookup index (0: disable mask, 1-128: from the lookup table).
  #   108     index:1             Start and apply mask envelope to a channel's noise control.
  #                               Bit = 1 is off, 0 is on.
  #                               Followed by a 1 byte lookup index (0: disable mask, 1-128: from the lookup table).
  #   109     step:2              Set vibrato step. Followed by 2 bytes (LSB first) of a step value multiplied.
  #                               An argument value 0..65536 translates to delta angle: 0..360 degrees.
  #   110     angle:2             Set vibrato angle. Followed by 2 bytes (LSB first) of a angle value.
  #                               An argument value 0..65536 translates to: 0..360 degrees.
  #   111     amplitude:1         Set vibrato amplitude. Followed by a 1 byte follows multiplied by 255.
  #                               An argument value 0..255 translates to an amplitude: 0.0..1.0
  #   112     -                   Disables vibrato.
  #   113     ticks:1             Set note progress period. Subsequent "play note" commands will change the tone gradually.
  #                               Followed by a 1 byte ticks value.
  #                               0 - ignores tone progress (fast): no internal tone progress variables are being updated
  #                               on following "play notes",
  #                               1 - immediate tone change, ... 255 - slowest tone change (during 255 ticks).
  #                               Before using a higher than 1 tone progress first set it to 1 and "play a note"
  #                               to update the "from tone" progress variables.
  #   114     delta:2, counter:2  Set tone progress variables directly.
  #                               Followed by 2 bytes delta (LSB first) and 2 bytes repeat counter (LSB first).
  #                               As a side effect this command sets "note progress period" value to 0.
  #                               One may play notes safely after that, but until the progress is finished the note
  #                               frequency will be ignored.
  #                               After the progress is finished the frequency of the last played note will be played.
  #                               The value of delta is twos complement 16-bit (-32768..32767) value. The tone progress
  #                               cursor ranges from 0..65535 and translates from the linear value to geometric
  #                               progression from the lowest playable frequency to the highest.
  #                               Tone progress delta from half-tones delta:
  #                                 delta = (delta_halftones * 256.0 * 32.0 / 12.0) & 0xffff
  #   115     -                   Enables AY-3-891x envelope volume control.
  #                               Disables direct volume control including AYMusic's controlled envelope.
  #   116     -                   Disables AY-3-891x envelope volume control.
  #                               Enables direct volume control including AYMusic's controlled envelope.
  #   117     -                   Disables tone output.
  #   118     -                   Enables tone output.
  #   119     -                   Disables noise output.
  #   120     -                   Enables noise output.
  #   121     index:1             Yields temporary control to another track (like a go sub) until it's finished.
  #                               Followed by a 1 byte lookup index (0: no-op, 1-128: from the lookup table).
  #   122     counter:1, offset:1 A loop. Moves track cursor back back counter times to the given offset.
  #                               Followed by a 1 byte counter (counter = 0 is loop forever) and a 1 byte LSB 8-bit
  #                               of a 16-bit twos complement negative byte offset relative to the beginning of
  #                               the command. (0..255) is (-256..-1)
  #   123-127                     Reserved (DO NOT USE! the results would be unexpected and most probably 'll crash
  #                               program execution).
  class AYMusic
    include Z80
    include ZXLib::AYSound::Registers
    include ZXLib::AYSound::EnvelopeControl

    # http://pages.mtu.edu/~suits/chords.html

    ##
    # The maximum recursion depth for loops and sub-tracks yielding. 20 by default.
    #
    # AYMusic uses the stack space ending at +track_stack_end+ label.
    # Each stack entry has the size of TrackStackEntry.
    # The last entry on the stack is a marker that is all 0.
    # There are 6 stacks for each channel track and channel instrument.
    # Both +yield+ and +loop+ commands use the same stack for their own purposes.
    # The sum of the recusion of sub-track yields and loop nesting level must not exceed
    # the value defined by TRACK_STACK_DEPTH.
    #
    # To change the default:
    #    module ZXUtils
    #      class AYMusic
    #        TRACK_STACK_DEPTH = 30
    #      end
    #    end
    #    require 'zxutils/ay_music'
    TRACK_STACK_DEPTH = 20 unless const_defined?(:TRACK_STACK_DEPTH)
    ##
    # Set to +true+ to create a slightly slower but ROM applicaple code.
    #
    # To change the default:
    #    module ZXUtils
    #      class AYMusic
    #        READ_ONLY_CODE = true
    #      end
    #    end
    #    require 'zxutils/ay_music'
    READ_ONLY_CODE = false unless const_defined?(:READ_ONLY_CODE)

    ## Re-exported Z80::Utils::SinCos::SinCosTable
    SinCosTable = Utils::SinCos::SinCosTable

    ## Re-exported Z80::Utils::SinCos::SinCos
    SinCos      = Utils::SinCos::SinCos

    ##
    # ====AYMusic engine utilities.
    #
    # _NOTE_:: Some of the AYMusic Macros require Z80::MathInt::Macros and some require ZXLib::AYSound::Macros
    #          to be imported.
    #
    #    require 'zxutils/ay_music'
    #
    #    class Program
    #      include Z80
    #      macro_import    MathInt
    #      label_import    ZXLib::Sys
    #      macro_import    ZXLib::AYSound
    #      macro_import    ZXUtils::AYMusic
    #      ...
    #    end
    module Macros
      ##
      # Creates a routine that initializes music tracks and optionally the index lookup table.
      #
      # Provide addresses of the main tracks as +track_a+, +track_b+ and +track_c+.
      #
      # Options:
      # * +index_table+:: An address of the index lookup table associated with tracks as an integer,
      #                   a label or a pointer. _Required_ if AYMusic::READ_ONLY_CODE is +true+.
      # * +init+:: A label addressing the AYMusic.init routine.
      # * +play+:: A label addressing the AYMusic.play routine.
      # * +music_control+:: A label of the type MusicControl addressing the data structure used by the AYMusic routines.
      # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
      #                    only if you have already disabled the interrupts.
      # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
      #                   if you need to perform more uninterrupted actions.
      #
      # Modifies: +af+, +bc+, +de+, +hl+, +ix+.
      def ay_music_init(track_a, track_b, track_c, index_table:nil, init:self.init, play:self.play, music_control:self.music_control, disable_intr:true, enable_intr:true)
        raise ArgumentError unless address?(track_a) and !pointer?(track_a) and
                                   address?(track_b) and !pointer?(track_b) and
                                   address?(track_c) and !pointer?(track_c) and
                                   (index_table.nil? or address?(index_table)) and
                                   label?(init) and label?(play) and label?(music_control)
        raise ArgumentError, "index_table is required with READ_ONLY_CODE" if AYMusic::READ_ONLY_CODE and index_table.nil?
        isolate do
                            di if disable_intr
                            call init
                            dw   track_a, track_b, track_c
                            ei if enable_intr
          if index_table
                            ld   hl, index_table unless index_table == hl
            if AYMusic::READ_ONLY_CODE
                            ld   [music_control.index_table], hl
            else
                            ld   [play.index_table_p], hl
            end
          end
        end
      end
      ##
      # Creates a routine that reads a state of the I/O ports from the AY-3-891x chip
      # and stores it into the player's register mirror.
      #
      # Execute this code before each play iteration if you care about preserving
      # the AY general purpose I/O ports' state.
      #
      # * +music_control+:: A label of the type MusicControl addressing the data structure.
      # * +play+:: A label addressing the AYMusic player's iteration routine.
      #
      # Options:
      # * +bc_const_loaded+:: If ZXLib::AYSound::Macros.ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io_ay+:: A label containing +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x output and select/input I/O bus.
      #
      # _NOTE_:: This macro requires ZXLib::AYSound::Macros to be also imported.
      #
      # Modifies: +af+, +bc+, +de+.
      def ay_music_preserve_io_ports_state(music_control, play, bc_const_loaded:false, io_ay:self.io_ay)
        isolate do
                            ay_get_register_value(AYMusic::MIXER, a, bc_const_loaded:bc_const_loaded, io_ay:io_ay)
                            ld   de, music_control.ay_registers.mixer
                            ld   b, 0b00111111
                            call play.apply_mask_de
        end
      end
      ##
      # Creates a routine that builds a note-to-fine tones index table.
      #
      # * +note_to_cursor+:: An address of the table to be built (max 192 bytes).
      #
      # Options:
      # * +play+:: An optional label addressing the AYMusic player's iteration routine (code re-use optimisation).
      # * +num_notes+:: An optional number of notes to cover which is by default its maximum value: 96.
      # * +subroutine+:: Pass +true+ to use +ret+ instruction on finish.
      #
      # _NOTE_:: This macro requires Z80::MathInt::Macros to be also imported if +play+ option is not provided.
      #
      # Modifies: +af+, +bc+, +de+, +hl+ and 1 level of stack.
      def ay_music_note_to_fine_tone_cursor_table_factory(note_to_cursor, play:nil, num_notes:AYMusic::MAX_NOTES_COUNT, subroutine:false)
        raise ArgumentError, "num_notes should be between 1 and #{AYMusic::MAX_NOTES_COUNT}" unless (1..AYMusic::MAX_NOTES_COUNT).include?(num_notes)
        isolate do |eoc|
                            ld   bc, (0<<8)|12
                            ld   hl, note_to_cursor
          floop             ld   a, b         # index = note * 256 * 32 / 12
                            cp   num_notes
          if subroutine
                            ret  NC
          else
                            jr   NC, eoc
          end
                            push bc
                            3.times { rrca }  # note * 32
                            ld   d, a         # 000ddddd eee00000
                            anda 0b11100000
                            ld   e, a
                            xor  d
                            ld   d, a
                            ex   de, hl
          if play.nil?                        # note * 32 / 12
                            divmod h, c, check0:false, check1:false, optimize: :size
                            divmod l, c, clrrem:false, optimize: :size
          else
                            call play.divmod_hl_c
          end
                            ld   h, l         # note * 256 * 32 / 12
                            ld   l, 0
          if play.nil?
                            divmod l, c, clrrem:false, optimize: :size
          else
                            call play.divmod_hl_c.divmod_rem_l_c
          end
                            ex   de, hl
                            ld   [hl], e
                            inc  hl
                            ld   [hl], d
                            inc  hl
                            pop  bc
                            inc  b
                            jr   floop
        end
      end
      ##
      # Creates a routine that builds a fine tones to AY-3-891x tone periods table.
      #
      # * +fine_tones+:: An address of the fine tones table to be built (512 bytes).
      #
      # Options:
      # * +hz+:: base (middle) frequency in Hz.
      # * +clock_hz+:: AY-3-891x clock frequency in Hz.
      # * +subroutine+:: Pass +true+ to use +ret+ instruction on finish.
      #
      # _NOTE_:: This macro requires Z80::MathInt::Macros to be also imported.
      #
      # Modifies: +af+, +af'+, +hl+, +hl'+, +b+, +bc'+, +de+, +de'+.
      def ay_music_tone_progress_table_factory(fine_tones, hz: 440, clock_hz: ZXLib::AYSound::CLOCK_HZ, subroutine:false)
        # CLOCK_HZ=3.5469/2 * 1_000_000
        # STEPS=256
        # aaa=->(notes, steps) {2**(notes.to_f/steps.to_f/12.0)}
        # a=aaa[-12,STEPS]
        #
        # x0=(CLOCK_HZ / (16.0 * 440.0/STEPS.to_f)).round
        # xf0=(CLOCK_HZ / (16.0 * 440.0/(STEPS.to_f/8)))
        # fc=->(steps){[2**(-1.0/steps.to_f),(CLOCK_HZ/(16.0*440.0/steps.to_f)).round,(CLOCK_HZ/(16.0*440.0/(steps.to_f/8)))]}
        # a,x0,xf0 = fc[STEPS]
        # a0=(a*65536).round
        # d1=->(fi){delta=0;(STEPS+1).times.inject(x0){|x,i| puts "#{i.to_s.rjust(4)}:#{x1=x>>3} #{x2=(xf0*a**i).round} #{delta+=(x2-x1).abs}"; ((x*a0)+fi)>>16};delta}
        # dd=->(fi){delta=0;(STEPS+1).times.inject(x0){|x,i| x1=x>>3; x2=(xf0*a**i).round; delta+=(x2-x1).abs; ((x*a0)+fi)>>16}; delta }
        # dd[16384+8192-673]
        # (-1024..1024).map{|i| [i,dd[16384+8192+i]]}
        #
        # a=2**(1.0/steps.to_f)
        # a0=((a-1.0)*65536).round
        # x0=(CLOCK_HZ / (16.0 * 440.0/STEPS.to_f)/2.0).round
        # xf0=(CLOCK_HZ / (16.0 * 440.0/(STEPS.to_f/8))/2.0)
        # d1=->(fi){delta=0;(STEPS+1).times.inject(x0){|x,i| puts "#{i.to_s.rjust(4)}:#{x1=(x+4)>>3} #{x2=(xf0*a**i).round} #{delta+=(x2-x1).abs}"; x+(((x*a0)+fi)>>16)};delta}
        # dd=->(fi){delta=0;(STEPS+1).times.inject(x0){|x,i| x1=(x+4)>>3; x2=(xf0*a**i).round; delta+=(x2-x1).abs; x+(((x*a0)+fi)>>16)};delta}
        # (0..1024).map{|i| [i,dd[16384+2048+i]]}
        # d1[16384+2048+594]
        steps = 256
        af = 2**(-1.0/steps.to_f)
        a0 = (af*65536).round
        fi = 16384+8192-673 # calculated numerically for given steps
        # xf = (clock_hz/(16.0*hz.to_f/(steps.to_f/8.0)))
        x0 = (clock_hz/(16.0*hz.to_f/steps.to_f)).round
        # x=((x*a0)+fi)>>16; x>>3
        isolate do |eoc|
                            ld   b, steps
                            exx
                            ld   de, fine_tones
                            ld   hl, x0
          mloop             ld16 bc, hl
                            ld   a, 3
          adjloop           srl  b
                            rr   c
                            dec  a
                            jr   NZ, adjloop
                            ex   de, hl
                            ld   [hl], c
                            inc  hl
                            ld   [hl], b
                            inc  hl
                            ex   de, hl
                            exx
                            dec  b
          if subroutine
                            ret  Z
          else
                            jr   Z, eoc
          end
                            exx
                            ld   bc, a0
                            mul16_32 bc, tt:de, clrhlhl:fi, optimize: :size
                            jr   mloop
        end
      end
      ##
      # Creates a routine that detects if the currently playing tracks has ended.
      #
      # +ZF+ is 1 if all of the tracks has reached the end.
      #
      # * +music_control+:: A label of the type MusicControl addressing the data structure.
      #
      # Modifies: +af+, +hl+.
      def ay_music_finished?(music_control)
        isolate do |eoc|
          (0..2).each do |ch|
                            xor  a
                            ld   hl, [music_control.chans[ch].track.cursor]
                            cp   [hl]
                            jr   NZ, eoc
                            ld   hl, [music_control.chans[ch].track.delay]
                            ld   a, l
                            ora  h
                            jr   NZ, eoc
          end
        end
      end
    end

    ###########
    # Exports #
    ###########

    export init
    export play
    export index_table
    export notes
    export note_to_cursor
    export fine_tones
    export workspace
    export track_stack_end
    export music_control
    export empty_instrument
    export ministack
    export workspace_end
    export sincos
    export sincos_end

    ###########
    # Structs #
    ###########

    # data: loop_offset(,counter,delta)*,0 (init: counter=1, cursor=[loop_offset], loop_at=cursor+loop_offset+1)
    class EnvelopeControl < Label
      counter       byte # repeat - 0 - disabled
      current_value byte
      cursor        word # -> [repeat, 8-bit delta]
      loop_at       word
    end

    # data: loop_offset(,counter,mask)*,0 (init: counter=1, cursor=[loop_offset]+1, loop_at=cursor+loop_offset)
    class MaskControl < Label
      counter       byte # repeat - 0 - disabled
      current_mask  byte
      cursor        word # # -> [repeat, mask]
      loop_at       word
    end

    # data: loop_offset(,delay<<5|+ delta 0..31)*,0 (init: counter=1, cursor=[loop_offset]+1, loop_at=cursor+loop_offset)
    class ChordControl < Label
      counter       byte # delay - 0 - disabled
      current_offs  byte # current note offset
      cursor        word # -> delay<<5|+ delta 0..31
      loop_at       word
    end

    # data: delta,counter,current
    class ToneProgressControl < Label
      delta_lo      byte
      delta_hi      byte
      delta         delta_lo word # added to current
      counter_lo    byte
      counter_hi    byte
      counter       counter_lo word # how many iterations left
      current_lo    byte
      current_hi    byte
      current       current_lo word # cursor on fine tones table 0boooccccc cccfffff o - octave, c - cursor, f - fraction
    end

    # data: step,angle,amplitude (init: enabled:-1)
    class VibratoControl < Label
      enabled       byte # a boolean
      step          word # added to angle * 256.0
      angle         word # angle * 256.0
      amplitude     byte # amplitude in tone period, tp += sin(angle)*ampl
    end

    ## The AY-3-891x register mirror.
    class AYRegisterMirror < Label
      tone_pitch_a      word
      tone_pitch_b      word
      tone_pitch_c      word
      tone_pitch        tone_pitch_a word, 3
      noise_pitch       byte
      mixer             byte
      volume_a          byte
      volume_b          byte
      volume_c          byte
      volume            volume_a byte, 3
      envelope_duration word
      envelope_shape    byte
    end

    ## The data type of the track stack entry.
    class TrackStackEntry < Label
      counter           byte # 0: a sub or a terminator
      signature         word # a loop_to signature or a return cursor from sub track
    end

    ## The music track control structure
    class TrackControl < Label
      track_stack       word               # stack pointer for loops and subs: sp -> TrackStackEntry
      delay_lo          byte
      delay_hi          byte
      delay             delay_lo word      # how many iterations wait before going on
      cursor_lo         byte
      cursor_hi         byte
      cursor            cursor_lo word     # cursor pointing to the current track instruction
    end

    ## The instrument track control structure
    class InstrumentControl < Label
      track             TrackControl
      start_lo          byte
      start_hi          byte
      start             start_lo word      # a cursor where instrument is being restarted on note played
      NO_RESTART_ON_PLAY_NOTE_BIT=0
      flags             byte
      note_progress     byte               # 0 - ignore tone progress when playing notes, 1 and more tone progress counter
    end

    ## The single channel control structure.
    class ChannelControl < Label
      volume_envelope   EnvelopeControl
      tone_progress     ToneProgressControl
      chord_progress    ChordControl
      current_note      byte
      vibrato_control   VibratoControl
      mask_ay_env_ctrl  MaskControl
      ay_envelope_on    byte # 0: manual volume control, -1: ay envelope control
      mask_tone_ctrl    MaskControl
      tone_off          byte # 0: tone audible, -1: tone off
      mask_noise_ctrl   MaskControl
      noise_off         byte # 0: noise audible, -1: noise off
      track             TrackControl
      instrument        InstrumentControl
    end

    ##
    # The main music control structure.
    #
    # The most important is the +music_control.counter+ word entry which can be used to synchronize
    # music with some external code.
    #
    # Other usefull entries to watch are:
    #
    # * +music_control.ay_registers+: a mirror of AY-3-891x registers (0 to 13).
    # * +music_control.chan_a.current_note+: a currently played note index (0..95) on channel A
    # * +music_control.chan_b.current_note+: a currently played note index (0..95) on channel B
    # * +music_control.chan_c.current_note+: a currently played note index (0..95) on channel C
    # * +music_control.chan_a.volume_envelope.current_value+: a current volume level (0..255) on channel A
    # * +music_control.chan_b.volume_envelope.current_value+: a current volume level (0..255) on channel B
    # * +music_control.chan_c.volume_envelope.current_value+: a current volume level (0..255) on channel C
    # * +music_control.noise_envelope.current_value+: a current noise pitch (0..255)
    # * +music_control.chan_a.chord_progress.counter+: == 0 - no chord on channel A, <> 0 chord active on channel A
    # * +music_control.chan_a.chord_progress.current_offs+: if chord is active, half-tone delta of currently played note (0..31) on channel A
    # * +music_control.chan_b.chord_progress.counter+: == 0 - no chord on channel B, <> 0 chord active on channel B
    # * +music_control.chan_b.chord_progress.current_offs+: if chord is active, half-tone delta of currently played note (0..31) on channel B
    # * +music_control.chan_c.chord_progress.counter+: == 0 - no chord on channel C, <> 0 chord active on channel C
    # * +music_control.chan_c.chord_progress.current_offs+: if chord is active, half-tone delta of currently played note (0..31) on channel C
    class MusicControl < Label
      ay_registers      AYRegisterMirror
      counter_lo        byte # a counter tracks can synchronize to with the sync command
      counter_hi        byte # an MSB of the counter, can be used externally
      counter           counter_lo word
      noise_envelope    EnvelopeControl
      chan_a            ChannelControl
      chan_b            ChannelControl
      chan_c            ChannelControl
      chans             chan_a ChannelControl, 3
      if READ_ONLY_CODE
        saved_sp        word
        index_table     word
      end
    end

    ## The maximum number of half-tones playable with the AY-3-891x.
    MAX_NOTES_COUNT = 8*12

    ## The single music track stack size calculated from TRACK_STACK_DEPTH.
    TRACK_STACK_SIZE  = (TRACK_STACK_DEPTH + 1) * TrackStackEntry.to_i + 1

    ## All music tracks stack size calculated from TRACK_STACK_SIZE.
    TRACK_STACK_TOTAL = 6 * TRACK_STACK_SIZE

    ## The depth of the player's machine code stack.
    MINISTACK_DEPTH = 6 # 12 bytes

    ## The byte size required for the player's machine code stack.
    MINISTACK_SIZE = 2*MINISTACK_DEPTH

    ###########
    # Imports #
    ###########

    label_import        ZXLib::Sys
    macro_import        Stdlib
    macro_import        MathInt
    macro_import        Utils::SinCos
    macro_import        ZXLib::AYSound

    ##########
    # Labels #
    ##########

    # sub-tracks, chords and envelopes index table
    # this can be overridden run-time (if READ_ONLY_CODE==false) by changing 2 bytes at +play.index_table_p+
    # see Macros.ay_music_init
    index_table         addr 0xC000, 2

    track_stack_size    addr TRACK_STACK_SIZE

    # used as ix indexes and sizes only
    channel_control     addr 0, ChannelControl
    instrument_control  addr 0, InstrumentControl

    ##########
    # Macros #
    ##########

    # inp hl: tone progress control (moves hl past it)
    # ZF:0 and bc: current tone
    macro :do_tone_progress do |_|
                        ld   sp, hl
                        pop  de                   # delta
                        pop  bc                   # counter, sp: -> current
                        ld   a, c
                        ora  b
                        jr   Z, no_change         # on target
                        dec  bc
                        pop  hl                   # current, sp: -> ToneProgress[1]
                        add  hl, de               # hl: 0boooccccc cccfffff
                        push hl                   # -> current
                        push bc                   # -> counter, sp: -> counter
                        pop  bc                   # sp: -> current
                        xor  a                    # a: 0
                        ld   c, a                 # c: 0
                      3.times do                  # a|hl = hl * 8
                        add  hl, hl               # hl: 0bcccccccc fffff000
                        rla                       # a: 0b00000ooo
                      end
                        ld   l, h
                        ld   h, c                 # hl: 0b00000000 cccccccc
                        add  hl, hl               # hl: 0b0000000c ccccccc0
                        ld   bc, fine_tones
                        add  hl, bc               # hl: fine_tones + 0bc ccccccc0
                        ld   c, [hl]
                        inc  hl
                        ld   b, [hl]              # de: tone period
                        inc  a                    # a: 1..9
        octave_loop     inc  bc                   # adjust to the current octave
                        srl  b
                        rr   c                    # (bc+1)/2 === (bc/2.0).round
                        dec  a
                        jr   NZ, octave_loop
                        inc  a                    # mark success (NZ)
      no_change         ld   hl, 2
                        add  hl, sp
                        ld   sp, ministack
    end

    ########
    # Main #
    ########

    ##
    # Call to initialize music structures and reset counter, track and instrument cursors.
    #
    # _NOTE_:: Stop interrupts (+di+) first before calling this routine.
    #
    # 3 words of track addresses must follow:
    #
    #         di
    #         call music.init
    #         dw   track1, track2, track3
    #         ei
    #         ...
    #
    #   index_table       dw instrument1, instrument2, ... etc
    #   track1            data (track 1 data)
    #   track2            data (track 2 data)
    #   track3            data (track 3 data)
    #
    # _NOTE_:: When AYMusic::READ_ONLY_CODE is +true+ make sure to _always_ populate
    #          +music_control.index_table+ entry _after_ calling +init+:
    #   if AYMusic::READ_ONLY_CODE
    #     ld   hl, index_table
    #     ld   [music.music_control.index_table], hl
    #   end
    #
    # Alternatively use Macros.ay_music_init which takes care of the above caveats.
    #
    # Modifies: +af+, +bc+, +de+, +hl+, +ix+.
    ns :init do
                        # clear control data
                        clrmem music_control, +music_control
                        # initialize pointers
                        pop  de             # 3 words must follow: A,B,C track.cursor addresses
      if READ_ONLY_CODE
                        ld   [music_control.saved_sp], sp
      else
                        ld   [restore_sp_p], sp
      end
                        ld   ix, music_control.chan_a.instrument.flags

                        ld   hl, track_stack_end
                        ld   a, 3 # number of channels
      init_loop         ld   sp, ix         # instrument.flags
                        ld   bc, empty_instrument
                        push bc             # instrument.start
                        push bc             # instrument.track.cursor
                        # mark end of stack with zeroes
                        ld   b, +track_stack_end
      mark_stack_end1   dec  hl
                        ld   [hl], 0
                        djnz mark_stack_end1
                        2.times { dec sp }  # instrument.track.delay
                        push hl             # instrument.track.track_stack

                        ex   de, hl
                        ld   c, [hl]
                        inc  hl
                        ld   b, [hl]        # bc: user supplied track_cursor
                        inc  hl
                        push bc             # track.cursor
                        ex   de, hl

                        ld   bc, -track_stack_size + (+track_stack_end)
                        add  hl, bc         # decrease stack pointer
                        # mark end of stack with zeroes
                        ld   b, +track_stack_end
      mark_stack_end2   dec  hl
                        ld   [hl], 0
                        djnz mark_stack_end2
                        2.times { dec sp }  # track.delay
                        push hl             # track.track_stack

                        ld   b, (-track_stack_size + (+track_stack_end)) >> 8
                        add  hl, bc         # decrease stack pointer

                        ld   bc, +channel_control
                        add  ix, bc
                        dec  a
                        jr   NZ, init_loop
      if READ_ONLY_CODE
                        ld   sp, [music_control.saved_sp]
      else
        restore_sp      ld   sp, 0
        restore_sp_p    restore_sp + 1
      end
                        ex   de, hl
                        jp   (hl)
    end
    ##
    # Call this routine, in turns, to play the music.
    #
    # _NOTE_:: Stop interrupts (+di+) first before calling this routine.
    #          Because it modifies all of the available Z80 registers (except I and R),
    #          care must be taken to restore registers: IY and H'L' when calling from
    #          the ZX Spectrum's Basic.
    #
    # Example:
    #
    #   forever           ei
    #                     halt
    #                     di
    #                     push iy
    #                     call music.play
    #                     pop  iy
    #                     jr   forever
    ns :play do
      if READ_ONLY_CODE
                        ld   [music_control.saved_sp], sp
      else
                        ld   [restore_sp_p], sp
      end
                        ld   sp, ministack
                        ay_io_load_const_reg_bc
                        xor  a                       # a=0
                        ld   hl, music_control.ay_registers # output registers' values
      rloop             ld   e, [hl]
                        inc  hl
                        ay_set_register_value(a, e, bc_const_loaded:true)
                        inc  a
                        cp   +music_control.ay_registers
                        jr   C, rloop

                        inc  [hl]                    # update counter_lo
                        jr   NZ, skip_high
                        inc  hl
                        inc  [hl]                    # update counter_hi
      skip_high         label
                        ld   de, music_control.chan_a
                        call track_progress
                        ld   de, music_control.chan_b
                        call track_progress
                        ld   de, music_control.chan_c
                        call track_progress

      sound_progress    label
                        ld   hl, music_control.noise_envelope
                        call envelope_progress       # a: current value
                        3.times { rrca }
                        anda 0x1F                    # noise mask
                        jr   NZ, skip_min_noise
                        inc  a                       # min noise pitch=1
      skip_min_noise    exx
                        ld   hl, music_control.ay_registers.tone_pitch_a
                        ld   de, music_control.ay_registers.noise_pitch
                        ld   [de], a                 # noise_pitch
                        inc  de                      # mixer
                        inc  de                      # volume_a/next volume
                        ld   a, 0b00110110           # channel counter and mixer mask
      channel_prog_loop exx
                        ex   af, af                  # save counter and mixer mask
                        call envelope_progress       # a: current value
                        4.times { rrca }
                        exx
                        ld   b, 0xF0
                        call apply_mask_de           # volume
                        exx
      tone_progress     do_tone_progress             # if ZF:NZ then bc: tone period
                        jr   Z, skip_tone_progr      # it's no-op or at target
                        ld   de, +ChordControl + 1
                        add  hl, de                  # skip chord_progress and current_note
                        jr   vibrato_ctrl_ck         # bc: tone period
      skip_tone_progr   call chord_progress          # a: note offset
                        add  a, [hl]                 # current_note + note offset
                        inc  hl
                        ex   de, hl
                        call get_note_tone_period_bc # bc: tone period
                        ex   de, hl
      vibrato_ctrl_ck   ld16 de, bc                  # save tone period
                        call vibrato_progress        # bc: tone period delta
                        ex   de, hl
                        jr   NC, skip_vibr_progr
                        add  hl, bc                  # hl: tone period + delta
      skip_vibr_progr   push hl                      # save tone period
                        ex   de, hl
                        exx
                        pop  bc
                        ld   [hl], c                 # tone_pitch
                        inc  hl
                        ld   [hl], b                 # tone_pitch
                        inc  hl                      # next tone pitch
                        exx
                        call mask_progress           # mask_ay_env_ctrl
                        exx
                        ld   b, 0b11101111           # env mask
                        call apply_mask_de           # volume
                        exx
      skip_mask_env     call mask_progress           # mask_tone_ctrl
                        ld   b, 0b11111000
                        call apply_mixer_mask
      skip_mask_tone    call mask_progress           # mask_noise_ctrl
                        ld   b, 0b11000111
                        call apply_mixer_mask
      skip_mask_noise   ld   bc, channel_control[1] - (channel_control.track)
                        add  hl, bc                  # skip track control
                        exx
                        inc  de                      # next ay_register.volume_(a|b|c)
                        ex   af, af
                        sll  a                       # rotate left mixer mask <- 1
                        jp   NC, channel_prog_loop
      if READ_ONLY_CODE
                        ld   sp, [music_control.saved_sp]
      else
        restore_sp      ld   sp, 0
        restore_sp_p    restore_sp + 1
      end
                        ret

      ns :track_progress do
                        ld16 ix, de                  # music_control.chan_X
                        ex   de, hl
                        ld   bc, channel_control.track.track_stack
                        add  hl, bc                  # music_control.chan_X.track.track_stack
                        call track_item_proc         # hl -> channel_control.track.track_stack
                        inc  hl                      # skip instrument.track.cursor_hi
                        call track_item_proc         # hl -> channel_control.instrument.track.track_stack
                        ret
      end

      ns :track_item_proc do                         # ix: music_control.chan_X
                        push hl                      # hl: music_control.chan_X.track.track_stack
                        pop  iy                      # iy -> track_control.track_stack
                        2.times { inc hl }           # hl -> track_control.delay
        track_item_loop ld   a, [hl]                 # delay_lo
                        inc  hl                      # hl -> track_control.delay_hi
                        ora  [hl]                    # delay_hi
                        jr   NZ, countdown
                        inc  hl                      # hl -> track_control.cursor_lo
                        ld   e, [hl]                 # cursor_lo
                        inc  hl                      # hl -> track_control.cursor_hi
                        ld   d, [hl]                 # de: cursor
                        push hl                      # hl -> track_control.cursor_hi
                        call process_item
                        pop  hl                      # hl -> track_control.cursor_hi
                        ld   [hl], d
                        dec  hl                      # hl -> track_control.cursor_lo
                        ld   [hl], e
                        2.times { dec hl }           # hl -> track_control.delay
                        jr   track_item_loop

        countdown       ld   b, [hl]                 # track_control.delay_hi
                        dec  hl
                        ld   c, [hl]                 # track_control.delay_lo
                        dec  bc                      # delay -= 1
        set_delay       ld   [hl], c                 # track_control.delay_lo
                        inc  hl
                        ld   [hl], b                 # track_control.delay_hi
                        2.times { inc hl }           # hl -> track_control.cursor_hi
                        ret                          # delay in process

        end_of_track    ld   sp, iy                  # sp: track_control.track_stack
                        pop  hl                      # track return address pointer
                        inc  hl                      # skip over counter
                        ld   e, [hl]
                        inc  hl
                        ld   d, [hl]                 # de: return track cursor
                        inc  hl
                        ld   a, e
                        ora  d                       # is it though?
                        jr   Z, nowhere_to_go
                        push hl                      # put back track_control.track_stack
        nowhere_to_go   ld   sp, ministack[-4]
                        ret  NZ                      # return from sub track
                        pop  hl                      # pop return address from process_item
                        pop  hl                      # pop track_control.cursor_hi
                        ret                          # return back from track_item_proc

        # de: track cursor (arg/return)
        process_item    ld   a, [de]                 # a: track item
                        anda a
                        jr   Z, end_of_track         # end of track
                        inc  de                      # advance track cursor
                        cp   97
                        jr   C, play_note
                        cp   176
                        jr   NC, wait_some
                        cp   160
                        jr   NC, set_volume
                        cp   128
                        jr   NC, set_noise
                        ld   hl, cmd_table1 - 97
                        ld   c, a
                        ld   b, 0
                        add  hl, bc
                        ld   c, [hl]
                        add  hl, bc
                        jp   (hl)                    # b: 0, de: track cursor
      end

      ############
      # Commands #
      ############

      ns :wait_some do # a: delay + 175
                        sub 175
                        3.times { dec hl } # delay_lo
                        ld  [hl], a
                        ret
      end

      ns :set_volume do # a: volume + 160
                        sub  160
                        ld   c, a
                        4.times { add a, a }
                        ora  c
                        ld   [ix + channel_control.volume_envelope.current_value], a
                        ret
      end

      ns :set_noise do # a: pitch + 128
                        sub  128
                        ld   c, a
                        3.times { add  a, a }
                        srl  c
                        srl  c
                        ora  c
                        ld   [music_control.noise_envelope.current_value], a
                        ret
      end

      # current_tone_progress = (note*256*32/12)
      # delta = (target - current) / counter
      ns :play_note do # a: note + 1
                        dec  a
                        ld   [ix + channel_control.current_note], a
                        exx
                        ld   hl, note_to_cursor
                        call get_hl_table_entry_bc   # bc: target cursor
                        ld   a, [ix + channel_control.instrument.note_progress]
                        cp   1
                        jr   C, skip_progress             # completely ignore progress
                        jr   NZ, calc_progress
                        ld   [ix + channel_control.tone_progress.current_lo], c
                        ld   [ix + channel_control.tone_progress.current_hi], b
                        ld   bc, 0
                        jr   set_counter                  # just store current value and clear progress
        calc_progress   ld   l, [ix + channel_control.tone_progress.current_lo]
                        ld   h, [ix + channel_control.tone_progress.current_hi]
                        sbc  hl, bc                       # current - target
                        ld   c, a                         # counter (note_progress)
                        sbc  a                            # a: 0 if current >= target, -1 if current < target
                        ld   e, a                         # e: sgn
                        call complement16_hle             # hl: -hl if e == -1
                        ld   a, e
                        cpl                               # a: -1 if current >= target, 0 if current < target
                        ld   e, a                         # e: sgn
                        call divmod_hl_c
                        call complement16_hle             # hl: -hl if e == -1
                        ld   [ix + channel_control.tone_progress.delta_lo], l
                        ld   [ix + channel_control.tone_progress.delta_hi], h
        set_counter     ld   [ix + channel_control.tone_progress.counter_lo], c
                        ld   [ix + channel_control.tone_progress.counter_hi], b # b: 0 after divmod_hl_c
        skip_progress   exx
                        bit  InstrumentControl::NO_RESTART_ON_PLAY_NOTE_BIT, [ix + channel_control.instrument.flags]
                        ret  NZ                           # don't reset instrument

                        ld   sp, ix
                        ld   hl, channel_control.instrument.start_lo
                        add  hl, sp
                        ld   sp, hl
                        pop  bc  # ix + channel_control.instrument.start
                        ld   sp, hl
                        push bc  # ix + channel_control.instrument.track.cursor
                        ld   bc, 0
                        push bc  # ix + channel_control.instrument.track.delay
                        ld   sp, ministack[-4]

                        # ld   a, [ix + channel_control.instrument.start_lo]
                        # ld   [ix + channel_control.instrument.track.cursor_lo], a
                        # ld   a, [ix + channel_control.instrument.start_hi]
                        # ld   [ix + channel_control.instrument.track.cursor_hi], a
                        # xor  a
                        # ld   [ix + channel_control.instrument.track.delay_lo], a
                        # ld   [ix + channel_control.instrument.track.delay_hi], a
                        ret
      end

      ns :wait_more_continue do
                        ld   a, [de]        # arg1
                        inc  de
                        cp   80             # a >= 80
                        jr   NC, no_2nd_arg
                        ld   b, a           # a < 80
                        inc  b              # (arg + 1) * 256
                        ld   a, [de]        # arg2
                        inc  de
        no_2nd_arg      ld   c, a           # bc: 0|arg1 or (arg1+1)|arg2
                        pop  hl             # pop return address from process_item
                        pop  hl             # hl -> track_control.cursor_hi
                        ld   [hl], d        # cursor_hi
                        dec  hl             # hl -> track_control.cursor_lo
                        ld   [hl], e        # cursor_lo
                        2.times { dec hl }  # hl -> track_control.delay
                        jp   track_item_proc.set_delay
      end

      cmd_table1        label
                        data :pc,  set_instrument  # track only
                        data :pc,  wait_more
                        data :pc,  set_play_mode_1
                        data :pc,  set_play_mode_2
                        data :pc,  set_envelope_duration
                        data :pc,  set_envelope_shape
                        data :pc,  set_volume_envelope_index
                        data :pc,  set_noise_envelope_index
                        data :pc,  set_chord_index
                        data :pc,  set_mask_env_index
                        data :pc,  set_mask_tone_index
                        data :pc,  set_mask_noise_index
                        data :pc,  set_vibrato_step
                        data :pc,  set_vibrato_angle
                        data :pc,  set_vibrato_amplitude
                        data :pc,  disable_vibrato
                        data :pc,  set_note_progress
                        data :pc,  set_tone_progress
                        data :pc,  set_ay_envelope_ctrl_on_off
                        data :pc,  set_ay_envelope_ctrl_on_off
                        data :pc,  set_tone_off_on
                        data :pc,  set_tone_off_on
                        data :pc,  set_noise_off_on
                        data :pc,  set_noise_off_on
                        data :pc,  sub_track
                        data :pc,  loop_next
                        # data :pc,  reserved0
                        # data :pc,  reserved1
                        # data :pc,  reserved2
                        # data :pc,  reserved3
                        # data :pc,  reserved4
                        # here be dragons

      # track only
      ns :set_instrument do
                        call get_index_table_entry_bc
                        jr   NZ, skip_empty
                        ld   bc, empty_instrument
        skip_empty      ld   [ix + channel_control.instrument.start_lo], c
                        ld   [ix + channel_control.instrument.start_hi], b
                        ret
      end

      # assert b == 0
      # arg1: 0..79 -> ticks = ((arg1 + 1)<<8)|arg2 [delay: 257..20736]
      # arg1: 80..255 -> ticks = arg1 (no arg2)     [delay: 81..256]
      # doesn't decrease delay before returning from track_item_proc so virtually delay = ticks + 1
      ns :wait_more do
                        jr   wait_more_continue
      end

      # track only
      ns :set_play_mode_1 do
                        res  InstrumentControl::NO_RESTART_ON_PLAY_NOTE_BIT, [ix + channel_control.instrument.flags]
                        ret
      end

      # track only
      ns :set_play_mode_2 do
                        set  InstrumentControl::NO_RESTART_ON_PLAY_NOTE_BIT, [ix + channel_control.instrument.flags]
                        ret
      end

      ns :set_envelope_duration do # envelope duration
                        ex   de, hl
                        ld   e, [hl]
                        inc  hl
                        ld   d, [hl]
                        inc  hl
                        ex   de, hl
                        ld   [music_control.ay_registers.envelope_duration], hl
                        ret
      end

      ns :set_envelope_shape do # envelope shape
                        ld   a, [de]
                        inc  de
                        ld   [music_control.ay_registers.envelope_shape], a
                        ret
      end

      ns :set_volume_envelope_index do
                        push ix # channel_control.volume_envelope
                        jr   set_noise_envelope_index.init_envelope
      end

      ns :set_noise_envelope_index do
                        ld   hl, music_control.noise_envelope
                        push hl
        init_envelope   call get_index_table_entry_bc
                        pop  hl
                        jr   Z, disable_ctrl
                        ld   [hl], 1 # counter
                        inc  hl      # current_value
                        inc  hl      # cursor
                        ld   [hl], c # cursor_lo
                        inc  hl
                        ld   [hl], b # cursor_hi
                        inc  hl      # loop_at
                        ld   a, [bc] # loop_offset
                        adda_to b, c # cursor + loop_offset
                        inc  bc      # cursor + loop_offset + 1
                        ld   [hl], c # loop_at_lo
                        inc  hl
                        ld   [hl], b # loop_at_hi
                        ret
        disable_ctrl    ld   [hl], a # counter - disables ctrl, a==0
                        ret
      end

      ns :set_chord_index do
                        ld   hl, channel_control.chord_progress
        init_ctrl       ld16 bc, ix
                        add  hl, bc
                        push hl
                        call get_index_table_entry_bc
                        pop  hl
                        jr   Z, set_noise_envelope_index.disable_ctrl
                        ld   [hl], 1 # counter
                        inc  hl      # current_offs/current_mask
                        inc  hl      # cursor
                        ld   a, [bc] # loop_offset
                        inc  bc      # cursor += 1
                        ld   [hl], c # cursor_lo
                        inc  hl
                        ld   [hl], b # cursor_hi
                        inc  hl      # loop_at
                        adda_to b, c # cursor + loop_offset
                        ld   [hl], c # loop_at_lo
                        inc  hl
                        ld   [hl], b # loop_at_hi
                        ret
      end

      ns :set_mask_env_index do
                        ld   hl, channel_control.mask_ay_env_ctrl
                        jr   set_chord_index.init_ctrl
      end


      ns :set_mask_tone_index do
                        ld   hl, channel_control.mask_tone_ctrl
                        jr   set_chord_index.init_ctrl
      end

      ns :set_mask_noise_index do
                        ld   hl, channel_control.mask_noise_ctrl
                        jr   set_chord_index.init_ctrl
      end

      ns :set_vibrato_step do
                        ld   hl, channel_control.vibrato_control.step
        enable_set_vib  ld16 bc, ix
                        add  hl, bc

                        ex   de, hl
                        ldi
                        ldi
                        ex   de, hl

                        # ld   a, [de]
                        # inc  de
                        # ld   [hl], a
                        # inc  hl
                        # ld   a, [de]
                        # inc  de
                        # ld   [hl], a

        enable_vibrato  ld   [ix + channel_control.vibrato_control.enabled], -1
                        ret
      end

      ns :set_vibrato_angle do
                        ld   hl, channel_control.vibrato_control.angle
                        jr   set_vibrato_step.enable_set_vib
      end

      ns :set_vibrato_amplitude do
                        ld   a, [ix + channel_control.current_note]
                        call get_note_tone_period_bc
                        dec  hl
                        dec  hl
                        ld   a, [hl]
                        dec  hl
                        ld   l, [hl]
                        ld   h, a
                        ora  a       # CF: 0
                        sbc  hl, bc  # notes[note-1] - notes[note]
                        ld   a, [de] # amplitude
                        inc  de
                        mul  l, a, tt:bc, clrhl:true, signed_k:false
                        ld   [ix + channel_control.vibrato_control.amplitude], h
                        jr   set_vibrato_step.enable_vibrato
      end

      ns :disable_vibrato do
                        ld   [ix + channel_control.vibrato_control.enabled], b # b: 0
                        ret
      end

      ns :set_note_progress do
                        ld   a, [de]
                        inc  de
                        ld   [ix + channel_control.instrument.note_progress], a
                        ret
      end

      ns :set_tone_progress do
                        ld   [ix + channel_control.instrument.note_progress], b # b: 0
                        ld   hl, channel_control.tone_progress.delta
                        ld16 bc, ix
                        add  hl, bc
                        ex   de, hl
                        ld   bc, 4
                        ldir
                        ex   de, hl
                        # ld   a, [de]
                        # inc  de
                        # ld   [ix + channel_control.tone_progress.delta_lo], a
                        # ld   a, [de]
                        # inc  de
                        # ld   [ix + channel_control.tone_progress.delta_hi], a
                        # ld   a, [de]
                        # inc  de
                        # ld   [ix + channel_control.tone_progress.counter_lo], a
                        # ld   a, [de]
                        # inc  de
                        # ld   [ix + channel_control.tone_progress.counter_hi], a
                        ret
      end

      ns :set_ay_envelope_ctrl_on_off do # a: head
                        rrca
                        sbc  a, a
                        ld   [ix + channel_control.ay_envelope_on], a
                        ret
      end

      ns :set_tone_off_on do # a: head
                        rrca
                        sbc  a, a
                        ld   [ix + channel_control.tone_off], a
                        ret
      end

      ns :set_noise_off_on do # a: head
                        rrca
                        sbc  a, a
                        ld   [ix + channel_control.noise_off], a
                        ret
      end

      ns :sub_track do # index
                        jr   sub_track_continue
      end

      ns :loop_next do # count(1), -jump_relative(1)
                        ld   sp, iy                  # track.track_stack
                        pop  hl                      # hl: loop stack address
                        ld   sp, hl                  # sp -> track_stack.counter
                        inc  sp                      # sp -> track_stack.signature
                        pop  bc                      # bc: signature
                        ld   a, c
                        cp   e
                        jr   NZ, add_level           # not our loop, add another one
                        ld   a, b
                        cp   d
                        jr   NZ, add_level           # not our loop, add another one
                        dec  [hl]                    # counter -= 1 on track_stack.counter
                        jr   Z, loop_over
                        ld   sp, hl                  # sp: track_stack.counter
        jump_to_addr    ex   de, hl
                        inc  hl
                        ld   e, [hl]                 # rel_jump
                        ld   d, -1                   # extend rel_jump as negative -256..-1
                        2.times { dec hl }           # relative to beggining of command
                        add  hl, de                  # jump to current - rel_jump (extended twos complement negative value)
                        ex   de, hl                  # de -> looped to instruction address
        loop_over_back  label                        # ld   [iy], sp (iy: track.track_stack)
        if READ_ONLY_CODE
                        ld   hl, 0
                        add  hl, sp
                        ld   sp, iy
                        pop  bc
                        push hl
        else
                        ld   [store_sp + 2], iy      # set target sp address (iy: track.track_stack)
          store_sp      ld   [channel_control.track.track_stack], sp
        end
                        ld   sp, ministack[-4]
                        ret
        add_level       ld   sp, hl                  # loop stack address
                        ld   a, [de]                 # counter
                        anda a
                        jr   Z, jump_to_addr         # loop forever
                        push de                      # signature
                        push af                      # [sp] = a; sp-= 2
                        inc  sp                      # sp += 1
                        jr   jump_to_addr
        loop_over       label                        # end loop
                        2.times { inc  de }          # skip over counter, jump_rel
                        jr   loop_over_back
      end

      ns :sub_track_continue do # index
                        call get_index_table_entry_bc
                        ret  Z                       # sub 0 does nothing
                        ld   sp, iy                  # track.track_stack
                        pop  hl                      # hl: track stack address
                        dec  hl
                        ld   [hl], d
                        dec  hl
                        ld   [hl], e                 # save de as a return track pointer
                        dec  hl
                        ld   [hl], 0                 # "not a loop" marker (loop.counter)
                        push hl                      # puts sub stack address back
                        ld16 de, bc
                        ld   sp, ministack[-4]
                        ret
      end

      ###############
      # Subroutines #
      ###############

      ns :apply_mixer_mask do # a: value, b: ~mask, a': channel ~mask 0b00110110
                          ex   af, af
                          ld   c, a                    # save channel ~mask
                          ora  b                       # channel ~mask | ~mask
                          ld   b, a                    # b: channel ~mask | ~mask
                          ld   a, c                    # restore channel ~mask
                          ex   af, af
                          ld   de, music_control.ay_registers.mixer
      end

      ns :apply_mask_de do # de: target address, a: value, b: ~mask (bits = 0 - new value, 1 - preserve)
                          ld   c, a                    # c: value to set
                          ld   a, [de]
                          xor  c                       # original ^ value
                          anda b                       # (original ^ value) & ~mask
                          xor  c                       # ((original ^ value) & ~mask) ^ value
                          ld   [de], a
                          ret
      end

      ns :get_index_table_entry_bc do
                          ld   a, [de] # instrument index 0..128
                          inc  de
                          anda a # index is 0
                          ret  Z # assume HL is >= 256, see below: adda_to h, l
                          dec  a
      end
      if READ_ONLY_CODE
                          ld   hl, [music_control.index_table]
      else
        index_table_a     ld   hl, index_table
        index_table_p     index_table_a + 1
      end
                          jr   get_hl_table_entry_bc

      ns :get_note_tone_period_bc do
                          ld   hl, notes
      end
      ns :get_hl_table_entry_bc do
                          add  a, a
                          adda_to h, l # ZF: 0 if (hl + a) & 0xFF00 <> 0
                          ld   c, [hl]
                          inc  hl
                          ld   b, [hl]
                          ret
      end

      complement16_hle    twos_complement16_by_sgn(h, l, e, th:h, tl:l)
                          ret

      ns :divmod_hl_c do
                          divmod h, c, check0:false, check1:false, optimize: :size
        divmod_rem_l_c    divmod l, c, clrrem:false, optimize: :size
                          ret
      end

      # inp hl: envelope control (moves hl past it, updates current value)
      # out a: current value
      ns :envelope_progress do
                        ld   sp, hl
                        pop  bc               # b: value, c: counter
                        ld   a, c
                        anda a                # check counter
                        ld   a, b             # current_value
                        jr   Z, no_change
                        pop  hl               # cursor
                        dec  c                # counter =- 1
                        jr   Z, cursor_next
        restart         push hl               # cursor
                        add  a, [hl]          # -> current + delta
                        bit  7, [hl]          # check delta sign
                        jr   NZ, minus_delta
                        jr   C, clip_value
                        scf
        minus_delta     jr   C, set_value
        clip_value      sbc  a, a
        set_value       ld   b, a             # new value
        no_change       push bc               # set counter and value, move sp to beginning
                        ld   hl, +EnvelopeControl
                        add  hl, sp           # hl: sp + sizeof EnvelopeControl
                        ld   sp, ministack[-1]
                        ret
        cursor_next     inc  hl               # cursor+=1 -> counter
        get_cursor      ld   a, [hl]          # -> next counter
                        anda a
                        jr   Z, cursor_reset
                        ld   c, a             # new counter
                        ld   a, b             # current value
                        inc  hl               # cursor+=1 -> delta
                        jr   restart
        cursor_reset    pop  hl               # loop_at
                        push hl               # back at cursor
                        jr   get_cursor
      end

      # inp hl: tone chord control (moves hl past ChordControl)
      # a: current note offset
      ns :chord_progress do
                        ld   a, [hl]
                        anda a
                        jr   Z, adjust_exit
        proceed         dec  [hl]         # counter
                        jr   Z, cursor_next
                        inc  hl
                        ld   a, [hl]      # current_offs
                        dec  hl
        adjust_exit     ld   bc, +ChordControl
                        add  hl, bc
                        ret
        cursor_next     ld   sp, hl
                        pop  af           # move sp to cursor
                        pop  bc           # cursor
        restart         ld   a, [bc]      # delta<<5|note_offset
                        anda a
                        jr   Z, reset_cursor
                        inc  bc
                        push bc           # cursor
                        ld   c, a
                        anda 0x1F
                        ld   b, a         # b: note offset
                        xor  c            # counter
                        3.times { rlca }  # reposition counter
                        ld   c, a         # c: counter
                        push bc           # note offset|counter
                        ld   a, b         # a: note offset
                        ld   sp, ministack[-1]
                        jr   adjust_exit
        reset_cursor    pop  bc           # loop_at
                        push bc           # back at loop_at
                        jr   restart
      end

      # inp hl: envelope control (moves hl past VibratoControl)
      # CF:1 out bc: current tone period delta
      ns :vibrato_progress do
                        ld   a, [hl]
                        anda a
        raise "sanity: chord control size differs from vibrato control size" unless ChordControl.to_i == VibratoControl.to_i
                        jr   Z, chord_progress.adjust_exit
                        inc  hl
                        ld   sp, hl
                        pop  bc                 # step
                        pop  hl                 # angle
                        add  hl, bc
                        push hl                 # angle
                        ld   a, h               # angle
                        sincos_from_angle(sincos, h, l)
                        ld   c, [hl]
                        inc  l
                        ld   b, [hl]
                        inc  sp                 # move sp past angle (next will pop hi angle byte and ampl)
                        pop  af                 # a: ampl, f: ignore angle hi, sp: VibratoControl[1]
                        mul8 b, c, a, tt:bc, clrhl:true, double:false
                        ld   c, h
                        sla  h
                        sbc  a
                        ld   b, a
                        ld   hl, 0
                        add  hl, sp
                        ld   sp, ministack[-1]
                        scf
                        ret
      end

      # inp hl: envelope control (moves hl past MaskControl and constant mask boolean value byte)
      # out a: current mask value 0|-1
      ns :mask_progress do
                        ld   a, [hl]
                        anda a
                        jr   Z, adjust_exit
                        ld   sp, hl
                        pop  bc               # c: counter, b: current
                        dec  c                # c: counter -= 1
                        jr   Z, cursor_next
        restart         rlc  b                # rotate mask left
                        sbc  a, a             # 0 or -1
                        push bc               # put c: counter + b: current
                        ld   hl, +MaskControl + 1 # skip constant mask value
                        add  hl, sp           # hl: sp + sizeof MaskControl
                        ld   sp, ministack[-1]
                        ret

        adjust_exit     ld   bc, +MaskControl
                        add  hl, bc
                        ld   a, [hl]          # get constant mask value instead
                        inc  hl
                        ret

        cursor_next     pop  hl               # cursor
        get_cursor      ld   a, [hl]          # -> next counter
                        anda a
                        jr   Z, reset_cursor
                        ld   c, a             # c: counter
                        inc  hl               # cursor+=1
                        ld   b, [hl]          # -> next mask
                        inc  hl               # cursor+=1
                        push hl               # put back cursor
                        jr   restart
        reset_cursor    pop  hl               # loop_at
                        push hl               # back at cursor
                        jr   get_cursor
      end

    end # :play

    # notes to tone durations table
    notes               label(2)[1] # max count 96 (8 octaves)
    # note to fine tones cursor table
    note_to_cursor      union notes[MAX_NOTES_COUNT], 2 # max count 96
    # fine tones durations table
    fine_tones          union note_to_cursor[MAX_NOTES_COUNT], 2 # count 256
    # the workspace label is only for the importer to know where the default workspace data begins, if they don't override workspace labels
    workspace           union fine_tones[256], 1
    # music tracks' loop and sub stack spaces
    track_stack_end     union workspace + TRACK_STACK_TOTAL, TrackStackEntry
    empty_instrument    track_stack_end[-1]
    # music control area
    music_control       union track_stack_end[0], MusicControl
    # a dedicated stack space for :play routine
    ministack           union music_control[1] + MINISTACK_SIZE, 2 # depth: MINISTACK_DEPTH, size: MINISTACK_SIZE
    # the workspace_end label is only for the importer to know where the default workspace data ends, if they don't override labels above
    workspace_end       union ministack, 1
    # a SinCosTable
    sincos              union workspace_end, SinCos, align: 0x100
    sincos_end          sincos + (+SinCosTable)
  end
end

if __FILE__ == $0
  # :stopdoc:
  require 'zxutils/zx7'
  require 'zxlib/basic'
  require_relative '../../examples/test_music'

  class MusicTest
    MusicInstance = TestMusic.new
    MusicData = MusicInstance.to_program
    include Z80
    include Z80::TAP
    include ZXUtils

    SinCos      = AYMusic::SinCos
    SinCosTable = AYMusic::SinCosTable

    import              ZXLib::Sys, macros: true, labels: true, code: false
    macro_import        MathInt
    macro_import        Stdlib
    macro_import        Utils::SinCos
    macro_import        ZXLib::AYSound
    macro_import        AYMusic

    io_ay = io128
    # io_ay = fuller_io
    # io_ay = ioT2k

    sincos              addr 0xE000, SinCos
    sincos_end          addr sincos + (+SinCosTable)
    ministack           addr sincos[0], 2
    # note to fine tones cursor table
    note_to_cursor      addr sincos_end, 2 # max count 96
    # fine tones durations table
    fine_tones          addr note_to_cursor[96], 2 # count 256
    track_stack_bottom  addr fine_tones[256], AYMusic::TrackStackEntry
    track_stack_end     addr track_stack_bottom + AYMusic::TRACK_STACK_TOTAL, AYMusic::TrackStackEntry
    empty_instrument    addr track_stack_end[-1]
    music_control       addr track_stack_end[0], AYMusic::MusicControl
    workspace_end       addr music_control[1]

    AY_MUSIC_OVERRIDE = { io_ay: io_ay,
                          index_table: index_table,
                          sincos: sincos,
                          note_to_cursor: note_to_cursor,
                          fine_tones: fine_tones,
                          track_stack_end: track_stack_end,
                          empty_instrument: empty_instrument,
                          music_control: music_control,
                          ministack: ministack }

    with_saved :demo, :exx, hl, :exx, ret: :after_ei do
                        di
                        call mute
                        call make_sincos
      ns :extend_notes do
        if AYMusic::READ_ONLY_CODE
                        ay_expand_notes music.notes, octaves:8
        else
                        ay_expand_notes_faster music.notes, octaves:8, save_sp:true, disable_intr:false, enable_intr:false
        end
      end
      ns :tone_progress_table_factory do
                        ay_music_tone_progress_table_factory music.fine_tones
      end
      ns :note_to_fine_tone_cursor_table_factory do
                        ay_music_note_to_fine_tone_cursor_table_factory music.note_to_cursor, play: music.play
      end
                        # deliberately poison workspace
                        clrmem track_stack_bottom, workspace_end-track_stack_bottom, 255
      ns :music_init do
                        ay_music_init track_a, track_b, track_c, index_table:index_table, init:music.init, play:music.play, music_control:music.music_control, disable_intr:false, enable_intr:false
      end
      forever           ei
                        halt
                        di
                        push iy
                        xor  a
                        out  (io.ula), a
                        ay_music_preserve_io_ports_state music.music_control, music.play, bc_const_loaded:false, io_ay:io_ay
                        call music.play
                        ld   a, 6
                        out  (io.ula), a
                        pop  iy
                        ld   de, [music_control.counter]
                        cp16n d, e, MusicTest::MusicInstance.channel_track(0).ticks_counter
                        jr   NC, quit
                        key_pressed?
                        jr   Z, forever
      quit              call mute
                        ld16 bc, de
                        ld   a, [vars.bordcr]
                        3.times { rrca }
                        anda 7
                        out  (io.ula), a
    end

    ns :mute do
                        ay_init(io_ay:io_ay)
                        ret
    end

    make_sincos         create_sincos_from_sintable sincos, sintable:sintable

    sintable            bytes   neg_sintable256_pi_half_no_zero_lo
    sintable_end        label

    song                import MusicData
    song_end            label

                        import AYMusic, :music, override: AY_MUSIC_OVERRIDE
    music_end           label

    NOTES = ay_tone_periods(min_octave:0, max_octave:7, notes_hz:equal_tempered_scale_notes_hz, clock_hz:ZXLib::AYSound::CLOCK_HZ)

    (1...NOTES.length).each do |i|
      puts "#{NOTES[i-1].to_s.rjust(4)}-#{NOTES[i].to_s.rjust(4)} delta: #{NOTES[i-1]-NOTES[i]} ratio: #{(NOTES[i-1].to_f/NOTES[i])}"
    end
                        dw NOTES[11]*2
                        dw NOTES[0...12]
                        # words 7*12
  end

  include ZXLib
  include ZXUtils

  music = MusicTest.new 0x8000
  puts music.debug
  puts MusicTest::MusicInstance.channel_tracks.map.with_index {|t,ch| "channel: #{ch} ticks: #{t.ticks_counter}" }
  puts "music size: #{music[:music_end] - music[:music]}"
  puts "TRACK_STACK_TOTAL: #{AYMusic::TRACK_STACK_TOTAL}"
  puts "TRACK_STACK_SIZE : #{AYMusic::TRACK_STACK_SIZE}"
  puts "TRACK_STACK_DEPTH: #{AYMusic::TRACK_STACK_DEPTH}"
  %w[
    +demo.extend_notes +demo.tone_progress_table_factory +demo.note_to_fine_tone_cursor_table_factory +demo.music_init
    index_table
    music.notes
    ministack
    sincos sincos_end
    music.sincos
    sintable sintable_end
    song song_end +song
    music music_end +music
    note_to_cursor
    fine_tones
    track_stack_end empty_instrument
    music_control music.music_control
    music.init
    music.play music.play.play_note
    +music_control
    mute
    demo
    track_a track_b track_c
  ].sort_by {|l| music[l] }.each do |label|
    puts "#{label.ljust(30)}: 0x#{'%04x'%music[label]} - #{music[label]}"
  end

  ZX7.compress music.code[(music[:music]-music.org),(music[:music_end] - music[:music])]
  ZX7.compress music.code[0, music[:make_sincos]-music.org]
  ZX7.compress music.code
  program = Basic.parse_source <<-EOC
    10 PRINT USR #{music[:demo]}
  9998 STOP: GO TO 10
  9999 CLEAR #{music.org-1}: LOAD ""CODE: RUN
  EOC
  puts program.to_source escape_keywords: true
  program.save_tap "testaymus", line: 9999
  music.save_tap "testaymus", append: true, name: "music"
  puts "TAP: testaymus.tap:"
  Z80::TAP.parse_file('testaymus.tap') do |hb|
      puts hb.to_s
  end
end