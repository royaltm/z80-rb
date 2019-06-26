# -*- coding: BINARY -*-
module ZXUtils
  module MusicBox
    ##
    # ===MusicBox TrackConfigCommands
    #
    # Common Track and Multitrack commands for changing track configuration options.
    module TrackConfigCommands
      DEFAULT_TEMPO = 128
      DEFAULT_FIRST_OCTAVE_NOTE = :a
      ##:call-seq:
      #       first_octave_note [note]
      #
      # Gets or establishes which music +note+ +:a+ to +:g!+ begins an octave.
      # By default the first music note in an octave is: +:a+.
      def first_octave_note(note=nil)
        @first_octave_note = note if note
        @first_octave_note
      end
      ##:call-seq:
      #       tempo [ticks]
      #
      # Gets or alters the tempo +ticks+.
      #
      # The +ticks+ value is being used as a base for the notes/pause duration.
      #
      # See CommonInstrumentCommands.pause.
      def tempo(ticks=nil)
        if ticks
          raise ArgumentError, "ticks should be a positive integer" unless Integer === ticks and ticks >= 1
          @tempo = ticks
        end
        @tempo
      end
    end
    ##
    # ===MusicBox CommonInstrumentCommands
    #
    # Common Instrument and Track commands.
    module CommonInstrumentCommands
      ##:call-seq:
      #       wait ticks
      #       w ticks
      #
      # Pauses the current track execution for +ticks+ number of ticks.
      # The +ticks+ value should be a positive integer as an Integer or a Rational.
      #
      # For the TrackConfigCommands.tempo related pause see CommonInstrumentCommands.pause.
      def wait(ticks)
        @commands << PauseCommand.new(ticks)
        @commands.length
      end
      alias_method :w, :wait
      ##:call-seq:
      #       pause length, *additional_lengths
      #       p length, *additional_lengths
      #
      # Pauses the current track execution for a +length+ period.
      # The +length+ value should be a positive integer.
      #
      # The number of +ticks+ paused is being calculated based on the TrackConfigCommands.tempo value.
      #
      #   ticks = tempo / length
      #
      # For the tempo unrelated pause see CommonInstrumentCommands.wait.
      #
      # In the music theory the length of:
      # * 1 means Semibreve or Whole note.
      # * 2 means Minim or Half note.
      # * 4 means Crotchet or Quarter note.
      # * 8 means Quaver or Eighth note.
      # * 16 means Semiquaver or Sixteenth note.
      # * 32 means Demisemiquaver or Thirty-second note.
      # * 64 means Hemidemisemiquaver or Sixty-fourth note.
      #
      # You may provide as many lengths as additional arguments as necessary. E.g.:
      #    pause 1, 2, 4 # will pause for the whole and half and quarter note.
      def pause(length, *length_exts)
        ticks = Rational(@tempo, length)
        length_exts.each do |len_ext|
          ticks += Rational(@tempo, len_ext)
        end
        wait(ticks)
      end
      alias_method :p, :pause
      ##:call-seq:
      #       noise pitch_level
      #       n pitch_level
      #
      # Sets noise pitch level: 0 to 31.
      def noise(level)
        @commands << NoisePitchCommand.new(level)
        @commands.length
      end
      alias_method :n, :noise
      ##:call-seq:
      #       volume level
      #       v level
      #
      # Sets volume level for the current channel: 0 to 15.
      def volume(level)
        @commands << VolumeLevelCommand.new(level)
        @commands.length
      end
      alias_method :v, :volume
      ##:call-seq:
      #       mark name
      #       m name
      #
      # Marks a point in the track and gives it a +name+ as a symbol or a string.
      # You can later use CommonInstrumentCommands.loop_to with a marked point name.
      def mark(name)
        @commands << MarkCommand.new(name)
        @commands.length
      end
      alias_method :m, :mark
      ##:call-seq:
      #       loop_to name, [repeat=nil]
      #       lt name, [repeat=nil]
      #
      # Loops execution from the marked point +name+. Repeats +repeat+ times.
      # If +repeat+ is +nil+ or missing loops _forever_.
      #
      # See also CommonInstrumentCommands.mark and CommonInstrumentCommands.repeat.
      def loop_to(mark_name, repeat=nil)
        @commands << LoopCommand.new(mark_name, repeat)
        @commands.length
      end
      alias_method :lt, :loop_to
      ##:call-seq:
      #       repeat [repeat=nil] {|| ... }
      #       rpt [repeat=nil] {|| ... }
      #
      # Repeats the execution of the commands in the given block +repeat+ times.
      # If +repeat+ is +nil+ or missing repeats _forever_.
      #
      # See also CommonInstrumentCommands.mark and CommonInstrumentCommands.loop_to.
      def repeat(times=nil, mark:nil, &block)
        mark_cmd = MarkCommand.new(mark)
        @commands << mark_cmd
        if times != 0
          yield
          unless @commands.last.equal?(mark_cmd) or times == 1
            loop_to mark_cmd.mark_name, times
          end
        end
        @commands.length
      end
      alias_method :rpt, :repeat
      ##:call-seq:
      #       mode1
      #       m1
      #
      # Switches to play mode 1. This is the default mode.
      # In this mode after playing a note the instrument track, if set, starts executing from the beginning.
      #
      # See also CommonInstrumentCommands.mode2 and TrackCommands.set_instrument.
      def mode1
        @commands << Command.new(Command::Headers::CMD_PLAY_MODE_1)
        @commands.length
      end
      alias_method :m1, :mode1
      ##:call-seq:
      #       mode2
      #       m2
      #
      # Switches to play mode 2.
      # In this mode after playing a note the instrument track, if set, continues executing uninterrupted.
      #
      # See also CommonInstrumentCommands.mode1 and TrackCommands.set_instrument.
      def mode2
        @commands << Command.new(Command::Headers::CMD_PLAY_MODE_2)
        @commands.length
      end
      alias_method :m2, :mode2
      ##:call-seq:
      #       envelope_duration duration
      #       envdur duration
      #       envd duration
      #
      # Sets the AY-891x automatic volume envelope duration: 1 to 65535.
      def envelope_duration(duration)
        @commands << AYEnvelopeDurationCommand.new(duration)
        @commands.length
      end
      alias_method :envdur, :envelope_duration
      alias_method :envd, :envelope_duration
      ##:call-seq:
      #       envelope_shape shape
      #       envsh shape
      #       envs shape
      #
      # Sets the shape of the AY-891x automatic volume envelope: 0 to 15.
      # You may use ZXLib::AYSound::EnvelopeControl constants.
      def envelope_shape(shape)
        @commands << AYEnvelopeShapeCommand.new(shape)
        @commands.length
      end
      alias_method :envsh, :envelope_shape
      alias_method :envs, :envelope_shape
      ##:call-seq:
      #       start_volume_envelope name
      #       ve name
      #
      # Applies an envelope defined by SongCommands.envelope to the volume level
      # at the current channel.
      def start_volume_envelope(envelope_name)
        @commands << EnvelopeCommand.new(:volume, envelope_name)
        @commands.length
      end
      alias_method :ve, :start_volume_envelope
      ##:call-seq:
      #       volume_envelope_off
      #       veo
      #
      # Turns off, if any, an envelope applied to the volume level at the current channel.
      def volume_envelope_off
        start_volume_envelope nil
      end
      alias_method :veo, :volume_envelope_off
      ##:call-seq:
      #       start_noise_envelope name
      #       ne name
      #
      # Applies an envelope defined by SongCommands.envelope to the noise pitch level.
      def start_noise_envelope(envelope_name)
        @commands << EnvelopeCommand.new(:noise, envelope_name)
        @commands.length
      end
      alias_method :ne, :start_noise_envelope
      ##:call-seq:
      #       noise_envelope_off
      #       neo
      #
      # Turns off, if any, an envelope applied to the noise pitch level.
      def noise_envelope_off
        start_noise_envelope nil
      end
      alias_method :neo, :noise_envelope_off
      ##:call-seq:
      #       start_chord name
      #       ce name
      #
      # Applies a chord defined by SongCommands.chord to the currently played note
      # at the current channel.
      def start_chord(chord_name)
        @commands << ChordCommand.new(chord_name)
        @commands.length
      end
      alias_method :ce, :start_chord
      ##:call-seq:
      #       chord_off
      #       ceo
      #
      # Turns off, if any, a chord applied to the played note at the current channel.
      def chord_off
        start_chord nil
      end
      alias_method :ceo, :chord_off
      ##:call-seq:
      #       mask_ay_volume_envelope
      #       me
      #
      # Applies a mask defined by SongCommands.mask to the current channel's envelope bit
      # controlling the AY-891x automatic volume envelope.
      # When the current mask bit has value 1: turns on the envelope. When 0: turns it off.
      def mask_ay_volume_envelope(mask_name)
        @commands << MaskCommand.new(:volume, mask_name)
        @commands.length
      end
      alias_method :me, :mask_ay_volume_envelope
      ##:call-seq:
      #       mask_ay_volume_envelope_off
      #       meo
      #
      # Turns off, if any, a mask applied to the current channel's envelope bit.
      def mask_ay_volume_envelope_off
        mask_ay_volume_envelope nil
      end
      alias_method :meo, :mask_ay_volume_envelope_off
      ##:call-seq:
      #       mask_tone
      #       mt
      #
      # Applies a mask defined by SongCommands.mask to the current channel's mixer
      # controlling the tone output.
      # When the current mask bit has value 1: turns off the tone. When 0: turns it on.
      def mask_tone(mask_name)
        @commands << MaskCommand.new(:tone, mask_name)
        @commands.length
      end
      alias_method :mt, :mask_tone
      ##:call-seq:
      #       mask_tone_off
      #       mto
      #
      # Turns off, if any, a mask applied to the current channel's mixer controlling
      # the tone output.
      def mask_tone_off
        mask_tone nil
      end
      alias_method :mto, :mask_tone_off
      ##:call-seq:
      #       mask_noise
      #       mn
      #
      # Applies a mask defined by SongCommands.mask to the current channel's mixer
      # controlling the noise output.
      # When the current mask bit has value 1: turns off the noise. When 0: turns it on.
      def mask_noise(mask_name)
        @commands << MaskCommand.new(:noise, mask_name)
        @commands.length
      end
      alias_method :mn, :mask_noise
      ##:call-seq:
      #       mask_noise_off
      #       mno
      #
      # Turns off, if any, a mask applied to the current channel's mixer controlling
      # the noise output.
      def mask_noise_off
        mask_noise nil
      end
      alias_method :mno, :mask_noise_off
      ##:call-seq:
      #       vibrato_step
      #       vs
      #
      # Enables the current channel's tone vibrato and sets the distortion angle progression step.
      #
      # +step+:: A floating point value of a tone distortion angle progression during a single tick.
      #          The value of 256.0 corresponds to the full phase of the distortion.
      #          The higher the absolute value of the +step+, the faster vibrato progresses.
      #
      # Assuming the starting angle is 0, if the value of the +step+ is positive the tone frequency
      # ascends first then descends. If it's negative the tone first descends before ascending.
      #
      # See also: CommonInstrumentCommands.vibrato_angle.
      def vibrato_step(step=1.0)
        @commands << VibratoStepCommand.new(step)
        @commands.length
      end
      alias_method :vs, :vibrato_step
      ##:call-seq:
      #       vibrato_angle
      #       vg
      #
      # Enables the current channel's tone vibrato and sets the current phase angle.
      #
      # +angle+:: A positive floating point value of a tone distortion angle.
      #           The value of 256.0 corresponds to the full phase of the distortion.
      #
      # The distortion level of the tone is calculated from:
      #   amplitude * sinus(PI * angle / 128.0)
      #
      # See also: CommonInstrumentCommands.vibrato_step and CommonInstrumentCommands.vibrato_amplitude.
      def vibrato_angle(angle=0.0)
        @commands << VibratoAngleCommand.new(angle)
        @commands.length
      end
      alias_method :vg, :vibrato_angle
      ##:call-seq:
      #       vibrato_amplitude
      #       va
      #
      # Enables the current channel's tone vibrato and sets the distortion amplitude.
      #
      # +amplitude+:: A positive floating point value of a tone distortion amplitude.
      #               Currently the valid values are in the range from 0 to 1, where
      #               1 corresponds to the single half-tone frequency interval.
      #
      # The distortion level of the tone is calculated from:
      #   amplitude * sinus(PI * angle / 128.0)
      #
      # See also: CommonInstrumentCommands.vibrato_step and CommonInstrumentCommands.vibrato_angle.
      def vibrato_amplitude(amplitude=1.0)
        @commands << VibratoAmplitudeCommand.new(amplitude)
        @commands.length
      end
      alias_method :va, :vibrato_amplitude
      ##:call-seq:
      #       vibrato_off
      #       vo
      #
      # Turns off, if any, the current channel's tone vibrato distortion.
      def vibrato_off
        @commands << Command.new(Command::Headers::CMD_VIBRATO_OFF)
        @commands.length
      end
      alias_method :vo, :vibrato_off
      ##:call-seq:
      #       note_progress period
      #       np period
      #
      # Enables the smooth tone frequency progression of the notes played on the current channel.
      #
      # +period+:: A number of ticks after which the tone reaches the next played note's frequency.
      # 
      # If the +period+ is 0, the tracking of the frequency of the played notes is being disabled.
      # To turn on the frequency progression first set +period+ to 1, play at least one note
      # and then set the desired progression period for the following notes being played.
      def note_progress(period)
        @commands << NoteProgressPeriodCommand.new(period)
        @commands.length
      end
      alias_method :np, :note_progress
      ##:call-seq:
      #       tone_progress delta, counter
      #       tp delta, counter
      #
      # Enables and controls the tone frequency progression of the current channel's tone.
      #
      # +delta+:: A floating point value representing a number of half-tones to go up or down
      #           the frequency scale.
      # +counter+:: An positive integer number indicating how many ticks it takes to reach
      #             the desired +delta+ interval.
      #
      # To set the starting frequency first set <tt>note_progress 1</tt>, play at least one note
      # and then set the desired +delta+ and +counter+ with the +tone_progress+ command.
      #
      # _NOTE_:: This command is an alternative to CommonInstrumentCommands.note_progress and as such
      #          can't be used with it at the same time on the same channel: +tone_progress+ sets
      #          the <tt>note_progress 0</tt> under the hood. Any notes being played while this
      #          command is in effect will not be heard for the +counter+ ticks. After that the tone
      #          frequency will change to the last played note's.
      def tone_progress(delta, counter)
        @commands << ToneProgressCommand.new(delta, counter)
        @commands.length
      end
      alias_method :tp, :tone_progress
      ##:call-seq:
      #       enable_ay_volume_ctrl
      #       variable_volume
      #       vv
      #
      # Enables the AY-891x automatic volume envelope control of the current channel.
      def enable_ay_volume_ctrl
        @commands << Command.new(Command::Headers::CMD_ENABLE_AY_VOLUME_CTRL)
        @commands.length
      end
      alias_method :variable_volume, :enable_ay_volume_ctrl
      alias_method :vv, :enable_ay_volume_ctrl
      ##:call-seq:
      #       disable_ay_volume_ctrl
      #       fixed_volume
      #       fv
      #
      # Turns off the AY-891x automatic volume envelope control of the current channel.
      def disable_ay_volume_ctrl
        @commands << Command.new(Command::Headers::CMD_DISABLE_AY_VOLUME_CTRL)
        @commands.length
      end
      alias_method :fixed_volume, :disable_ay_volume_ctrl
      alias_method :fv, :disable_ay_volume_ctrl
      ##:call-seq:
      #       tone_off
      #       t0
      #
      # Turns off the current channel's tone output.
      def tone_off
        @commands << Command.new(Command::Headers::CMD_DISABLE_TONE)
        @commands.length
      end
      alias_method :t0, :tone_off
      ##:call-seq:
      #       tone_on
      #       t1
      #
      # Turns on the current channel's tone output.
      def tone_on
        @commands << Command.new(Command::Headers::CMD_ENABLE_TONE)
        @commands.length
      end
      alias_method :t1, :tone_on
      ##:call-seq:
      #       noise_off
      #       n0
      #
      # Turns off the current channel's noise output.
      def noise_off
        @commands << Command.new(Command::Headers::CMD_DISABLE_NOISE)
        @commands.length
      end
      alias_method :n0, :noise_off
      ##:call-seq:
      #       noise_on
      #       n1
      #
      # Turns on the current channel's noise output.
      def noise_on
        @commands << Command.new(Command::Headers::CMD_ENABLE_NOISE)
        @commands.length
      end
      alias_method :n1, :noise_on
    end

    ##
    # ===MusicBox InstrumentCommands
    #
    # Instrument only commands.
    module InstrumentCommands
      include TrackConfigCommands
      include CommonInstrumentCommands
      def self.extended(klass) # :nodoc:
        if klass.respond_to?(:new)
          klass.module_eval do
            @commands ||= []
            @tempo ||= DEFAULT_TEMPO
            @first_octave_note ||= DEFAULT_FIRST_OCTAVE_NOTE
          end
        end
        constants.each do |c|
          klass.const_set c, const_get(c) unless klass.const_defined?(c)
        end
      end
      ##:call-seq:
      #       sub_instrument instrument_name
      #       sub instrument_name
      #
      # Yields execution of the instrument to another with the given +instrument_name+ as a symbol or string.
      # When the sub-instrument execution is finished, the yielding instrument will resume execution.
      def sub_instrument(instrument_name)
        @commands << SubInstrumentCommand.new(instrument_name)
        @commands.length
      end
      alias_method :sub, :sub_instrument
    end

    ##
    # ===MusicBox TrackCommands
    #
    # Track commands for playing notes, setting instruments and yielding to other tracks.
    #
    # For the other available track commands see: TrackConfigCommands, CommonInstrumentCommands.
    module TrackCommands
      include TrackConfigCommands
      include CommonInstrumentCommands
      def self.extended(klass) # :nodoc:
        if klass.respond_to?(:new)
          klass.module_eval do
            @commands ||= []
            @tempo ||= DEFAULT_TEMPO
            @first_octave_note ||= DEFAULT_FIRST_OCTAVE_NOTE
          end
        end
        constants.each do |c|
          klass.const_set c, const_get(c) unless klass.const_defined?(c)
        end
      end
      ##:call-seq:
      #       play note_name, octave, *pause_lengths
      #       note_name octave, *pause_lengths
      #
      # To play notes a series of commands has been created:
      #
      #   note_name  corresponding note
      #   a          A
      #   a!         A#
      #   b_         Bb
      #   b          B
      #   c          C
      #   c!         C#
      #   d_         Db
      #   d          D
      #   d!         D#
      #   e_         Eb
      #   e          E
      #   f          F
      #   f!         F#
      #   g_         Gb
      #   g          G
      #   g!         G#
      #   a_         Ab
      #
      # +octave+:: must be a number: 0 to 7.
      # +pause_lenghs+:: may be one or more pause lengths, see CommonInstrumentCommands.pause.
      #
      # E.g. to play a middle C# Half note:
      #
      #   c! 3, 2
      #
      # _NOTE_:: To set the octave boundary please consult TrackConfigCommands.first_octave_note.
      #          By default an octave starts from A.
      #
      # In the case if you set:
      #
      #   first_octave_note :c
      #
      # The middle C# will be at:
      #
      #   c! 4, 2
      def play(note_name, octave, *pause_lengths)
        @commands << NoteCommand.new(note_name, octave, @first_octave_note)
        pause(*pause_lengths) unless pause_lengths.empty?
      end
      %w[a a! b c c! d d! e f f! g g!].map(&:to_sym).each do |note_name|
        define_method(note_name) do |octave, *pause_lengths|
          play note_name, octave, *pause_lengths
        end
      end
      %w[b_ a!  d_ c!  e_ d!  g_ f!  a_ g!].each_slice(2) do |a, b|
        alias_method a, b
      end
      ##:call-seq:
      #       set_instrument instrument_name
      #       i instrument_name
      #
      # Sets an instrument for the next notes played.
      #
      # +instrument_name+:: A symbol or string with the instrument name to set.
      #
      # To turn off an instrument, set another one or use TrackCommands.set_empty_instrument.
      def set_instrument(instrument_name)
        @commands << InstrumentCommand.new(instrument_name)
        @commands.length
      end
      alias_method :i, :set_instrument
      ##:call-seq:
      #       set_empty_instrument
      #       ei
      #
      # Turns off any instrument previously set up with TrackCommands.set_instrument for the
      # following played notes.
      def set_empty_instrument
        set_instrument(nil)
      end
      alias_method :ei, :set_empty_instrument
      ##:call-seq:
      #       sub_track track_name
      #       sub track_name
      #
      # Yields execution of the track to another sub-track with the given +track_name+ as a symbol or string.
      # When the track execution is finished, the yielding track will resume execution.
      def sub_track(track_name)
        @commands << SubTrackCommand.new(track_name)
        @commands.length
      end
      alias_method :sub, :sub_track
    end # TrackCommands

    ##
    # ===MusicBox Track
    #
    # A track consists of the ZXUtils::AYMusic commands.
    #
    # To create a custom track you need to include the Track module in your class representing 
    # the given music track.
    #
    #    class MusicTrack1
    #        include ZXUtils::MusicBox::Track
    #        #... track commands follow
    #    end
    #
    # Such a track can be included with the MusicBox::SongCommands.import_track command of the Song.
    #
    # Alternatively use MusicBox::SongCommands.track command to define tracks directly in the Song.
    #
    # For the list of available commands see TrackCommands and CommonInstrumentCommands.
    module Track
      Marker = ::Struct.new :name, :offset
      def self.included(klass) # :nodoc:
        klass.extend TrackCommands if klass.respond_to?(:new)
      end
      ## A compiled track's body as a binary string.
      attr_reader :code
      ## A hash containging the markers with the corresponding code offsets.
      attr_reader :markers
      ## A resolver used by the compilation process.
      attr_reader :resolver
      ##
      # Instances of the derived classes are being created internally by the MusicBox::Song compilation process.
      def initialize(resolver)
        @resolver = resolver
        @markers = ROHash.new
        @ticks_counter = 0
        @rational_counter = Rational(0,1)
        @code = ''
        @last_pause_delay = Rational(0,1)
        @last_pause_ticks = 0
        @last_pause_range = 0...0
        self.class.instance_variable_get('@commands'.freeze).each { |c| self << c }
      end
      ##
      # Adds a track's tick counter value to the given +counter+ and returns it.
      def ticks_counter(counter=0)
        @ticks_counter && (@ticks_counter + counter)
      end
      ##
      # Returns the size in bytes of the track's compiled body.
      def bytesize
        code.bytesize
      end

      def <<(cmd) # :nodoc:
        raise "no commands allowed after forever loop" if @ticks_counter.nil?
        if cmd.is_a?(Class) and cmd.ancestors.include?(Track)
          begin
            original_markers = @markers
            @markers = ROHash.new
            cmd.instance_variable_get('@commands'.freeze).each { |c| self << c }
          ensure
            @markers = original_markers
          end
        elsif cmd.is_a?(Command)
          if cmd.marker?
            @markers[cmd.mark_name] = Marker.new(cmd.mark_name, @code.bytesize)
            @last_pause_range = 0...0
          elsif cmd.loop?
            unless (loop_marker = @markers[cmd.loop_mark_name])
              raise "marker not found: #{cmd.loop_mark_name.inspect}"
            end
            if cmd.counter == 0
              @ticks_counter = nil
            else
              delta_counter = Command.ticks_counter_from_code(@code, @resolver, start_offset:loop_marker.offset)
              if delta_counter == 0
                raise "Can't create a loop to: #{loop_marker.name.inspect} without any pause in the loop body - it would dead-lock the player"
              end
              @ticks_counter += delta_counter * cmd.counter
            end
            @code << cmd.compile(loop_marker.offset, @code.bytesize)
          elsif cmd.pause?
            if @last_pause_range.max == @code.bytesize - 1
              @rational_counter -= @last_pause_delay
              @ticks_counter -= @last_pause_ticks
              delay_add = @last_pause_delay
              pause_range = @last_pause_range
            else
              delay_add = 0
              pause_range = @code.bytesize..-1
            end
            data, ticks = cmd.compile(@rational_counter, delay_add)
            @code[pause_range] = data
            @last_pause_delay = cmd.delay + delay_add
            @last_pause_ticks = ticks
            @last_pause_range = pause_range.begin...@code.bytesize
            @rational_counter += @last_pause_delay
            @ticks_counter += @last_pause_ticks
          else
            track = if cmd.sub_track?
              @resolver.get_item(@resolver.track_index(cmd.track_name))
            elsif cmd.sub_instrument?
              @resolver.get_item(@resolver.instrument_index(cmd.instrument_name))
            end
            unless track.nil?
              @ticks_counter = track.ticks_counter(@ticks_counter)
            end
            @code << cmd.compile(@resolver)
          end
        else
          raise ArgumentError, "only tracks and commands are supported"
        end
        self
      end
    end # Track

    ##
    # ===MusicBox Instrument
    #
    # An instrument consists of the ZXUtils::AYMusic commands.
    #
    # To create a custom instrument you need to include the Instrument module in your class
    # representing the given music track.
    #
    #    class InstrumentFoo1
    #        include ZXUtils::MusicBox::Instrument
    #        #... instrument commands follow
    #    end
    #
    # Such an instrument can be included with the MusicBox::SongCommands.import_instrument command of the Song.
    #
    # Alternatively use MusicBox::SongCommands.instrument command to define instruments directly in the Song.
    #
    # For the list of available commands see InstrumentCommands and CommonInstrumentCommands.
    module Instrument
      include Track
      def self.included(klass) # :nodoc:
        if klass.respond_to?(:new)
          klass.extend InstrumentCommands if klass.respond_to?(:new)
        end
      end
    end
    ##
    # ===MusicBox EmptyTrack
    #
    # An empty track used by the MusicBox::Song compilation. Should stay empty.
    class EmptyTrack
      include Track
    end
  end
end
