# -*- coding: BINARY -*-
module ZXUtils
  module MusicBox
    module TrackConfigCommands
      DEFAULT_TEMPO = 128
      DEFAULT_FIRST_OCTAVE_NOTE = :a
      def first_octave_note(note=nil)
        @first_octave_note = note if note
        @first_octave_note
      end

      def tempo(tempo=nil)
        if tempo
          raise ArgumentError, "tempo should be a positive integer" unless Integer === tempo and tempo >= 1
          @tempo = tempo
        end
        @tempo
      end
    end

    module CommonInstrumentCommands
      def wait(ticks)
        @commands << PauseCommand.new(ticks)
        @commands.length
      end
      alias_method :w, :wait

      def pause(duration, *duration_exts)
        ticks = Rational(@tempo, duration)
        duration_exts.each do |dur_ext|
          ticks += Rational(@tempo, dur_ext)
        end
        wait(ticks)
      end
      alias_method :p, :pause

      def noise(level)
        @commands << NoisePitchCommand.new(level)
        @commands.length
      end
      alias_method :n, :noise

      def volume(level)
        @commands << VolumeLevelCommand.new(level)
        @commands.length
      end
      alias_method :v, :volume

      def mark(name)
        @commands << MarkCommand.new(name)
        @commands.length
      end
      alias_method :m, :mark

      def loop_to(mark_name, repeat=nil)
        @commands << LoopCommand.new(mark_name, repeat)
        @commands.length
      end
      alias_method :lt, :loop_to

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

      def mode1
        @commands << Command.new(Command::Headers::CMD_PLAY_MODE_1)
        @commands.length
      end
      alias_method :m1, :mode1

      def mode2
        @commands << Command.new(Command::Headers::CMD_PLAY_MODE_2)
        @commands.length
      end
      alias_method :m2, :mode2

      def envelope_duration(duration)
        @commands << AYEnvelopeDurationCommand.new(duration)
        @commands.length
      end
      alias_method :envdur, :envelope_duration
      alias_method :envd, :envelope_duration

      def envelope_shape(shape)
        @commands << AYEnvelopeShapeCommand.new(shape)
        @commands.length
      end
      alias_method :envsh, :envelope_shape
      alias_method :envs, :envelope_shape

      def start_volume_envelope(envelope_name)
        @commands << EnvelopeCommand.new(:volume, envelope_name)
        @commands.length
      end
      alias_method :ve, :start_volume_envelope

      def volume_envelope_off
        start_volume_envelope nil
      end
      alias_method :veo, :volume_envelope_off

      def start_noise_envelope(envelope_name)
        @commands << EnvelopeCommand.new(:noise, envelope_name)
        @commands.length
      end
      alias_method :ne, :start_noise_envelope

      def noise_envelope_off
        start_noise_envelope nil
      end
      alias_method :neo, :noise_envelope_off

      def start_chord(chord_name)
        @commands << ChordCommand.new(chord_name)
        @commands.length
      end
      alias_method :ce, :start_chord

      def chord_off
        start_chord nil
      end
      alias_method :ceo, :chord_off

      def mask_ay_volume_envelope(mask_name)
        @commands << MaskCommand.new(:volume, mask_name)
        @commands.length
      end
      alias_method :me, :mask_ay_volume_envelope

      def mask_ay_volume_envelope_off
        mask_ay_volume_envelope nil
      end
      alias_method :meo, :mask_ay_volume_envelope_off

      def mask_tone(mask_name)
        @commands << MaskCommand.new(:tone, mask_name)
        @commands.length
      end
      alias_method :mt, :mask_tone

      def mask_tone_off
        mask_tone nil
      end
      alias_method :mto, :mask_tone_off

      def mask_noise(mask_name)
        @commands << MaskCommand.new(:noise, mask_name)
        @commands.length
      end
      alias_method :mn, :mask_noise

      def mask_noise_off
        mask_noise nil
      end
      alias_method :mno, :mask_noise_off

      def vibrato_step(step=1.0)
        @commands << VibratoStepCommand.new(step)
        @commands.length
      end
      alias_method :vs, :vibrato_step

      def vibrato_angle(angle=0.0)
        @commands << VibratoAngleCommand.new(angle)
        @commands.length
      end
      alias_method :vg, :vibrato_angle

      def vibrato_amplitude(amplitude=1.0)
        @commands << VibratoAmplitudeCommand.new(amplitude)
        @commands.length
      end
      alias_method :va, :vibrato_amplitude

      def vibrato_off
        @commands << Command.new(Command::Headers::CMD_VIBRATO_OFF)
        @commands.length
      end
      alias_method :vo, :vibrato_off

      # 0: ignore progress, 1: no progress, 2 and more: progress counter
      def note_progress(period)
        @commands << NoteProgressPeriodCommand.new(period)
        @commands.length
      end
      alias_method :np, :note_progress

      # ad-hoc tone progress, sets note progress to 0
      # until it's finish playing notes only changes tone played after tone progress is done
      def tone_progress(delta, counter)
        @commands << ToneProgressCommand.new(delta, counter)
        @commands.length
      end
      alias_method :tp, :tone_progress

      def enable_ay_volume_ctrl
        @commands << Command.new(Command::Headers::CMD_ENABLE_AY_VOLUME_CTRL)
        @commands.length
      end
      alias_method :variable_volume, :enable_ay_volume_ctrl
      alias_method :vv, :variable_volume

      def disable_ay_volume_ctrl
        @commands << Command.new(Command::Headers::CMD_DISABLE_AY_VOLUME_CTRL)
        @commands.length
      end
      alias_method :fixed_volume, :disable_ay_volume_ctrl
      alias_method :fv, :fixed_volume

      def tone_off
        @commands << Command.new(Command::Headers::CMD_DISABLE_TONE)
        @commands.length
      end
      alias_method :t0, :tone_off

      def tone_on
        @commands << Command.new(Command::Headers::CMD_ENABLE_TONE)
        @commands.length
      end
      alias_method :t1, :tone_on

      def noise_off
        @commands << Command.new(Command::Headers::CMD_DISABLE_NOISE)
        @commands.length
      end
      alias_method :n0, :noise_off

      def noise_on
        @commands << Command.new(Command::Headers::CMD_ENABLE_NOISE)
        @commands.length
      end
      alias_method :n1, :noise_on
    end

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

      def sub_instrument(instrument_name)
        @commands << SubInstrumentCommand.new(instrument_name)
        @commands.length
      end
      alias_method :sub, :sub_instrument
    end

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

      %w[a a! b c c! d d! e f f! g g!].map(&:to_sym).each do |note_name|
        define_method(note_name) do |octave, *durations|
          @commands << NoteCommand.new(note_name, octave, @first_octave_note)
          pause(*durations) unless durations.empty?
        end
      end
      %w[b_ a!  d_ c!  e_ d!  g_ f!  a_ g!].each_slice(2) do |a, b|
        alias_method a, b
      end

      def set_instrument(instrument_name)
        @commands << InstrumentCommand.new(instrument_name)
        @commands.length
      end
      alias_method :i, :set_instrument

      def set_empty_instrument
        set_instrument(nil)
      end
      alias_method :ei, :set_empty_instrument

      def sub_track(track_name)
        @commands << SubTrackCommand.new(track_name)
        @commands.length
      end
      alias_method :sub, :sub_track
    end # TrackCommands

    module Track
      Marker = ::Struct.new :name, :offset
      def self.included(klass) # :nodoc:
        klass.extend TrackCommands if klass.respond_to?(:new)
      end

      attr_reader :code, :markers, :resolver

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

      def <<(cmd)
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

      def ticks_counter(counter=0)
        @ticks_counter && (@ticks_counter + counter)
      end

      def bytesize
        code.bytesize
      end
    end # Track

    module Instrument
      include Track
      def self.included(klass) # :nodoc:
        if klass.respond_to?(:new)
          klass.extend InstrumentCommands if klass.respond_to?(:new)
        end
      end
    end

    class EmptyTrack
      include Track
    end
  end
end
