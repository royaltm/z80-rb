# -*- coding: BINARY -*-
module ZXUtils
  module MusicBox
    class Command
      module Headers
        CMD_TERMINATE                =   0
        CMD_NOTE                     =   1
        CMD_SET_INSTRUMENT           =  97
        CMD_WAIT_MORE                =  98
        CMD_PLAY_MODE_1              =  99
        CMD_PLAY_MODE_2              = 100
        CMD_SET_AY_ENVELOPE_DURATION = 101
        CMD_SET_AY_ENVELOPE_SHAPE    = 102
        CMD_SET_VOLUME_ENVELOPE      = 103
        CMD_SET_NOISE_ENVELOPE       = 104
        CMD_SET_CHORD                = 105
        CMD_SET_VOLUME_CTRL_MASK     = 106
        CMD_SET_TONE_CTRL_MASK       = 107
        CMD_SET_NOISE_CTRL_MASK      = 108
        CMD_SET_VIBRATO_STEP         = 109
        CMD_SET_VIBRATO_ANGLE        = 110
        CMD_SET_VIBRATO_AMPLITUDE    = 111
        CMD_VIBRATO_OFF              = 112
        CMD_SET_NOTE_PROGRESS_PERIOD = 113
        CMD_SET_TONE_PROGRESS        = 114
        CMD_ENABLE_AY_VOLUME_CTRL    = 115
        CMD_DISABLE_AY_VOLUME_CTRL   = 116
        CMD_DISABLE_TONE             = 117
        CMD_ENABLE_TONE              = 118
        CMD_DISABLE_NOISE            = 119
        CMD_ENABLE_NOISE             = 120
        CMD_SUB_TRACK                = 121
        CMD_LOOP                     = 122
        CMD_RESERVED0                = 123
        CMD_RESERVED1                = 124
        CMD_RESERVED2                = 125
        CMD_RESERVED3                = 126
        CMD_RESERVED4                = 127
        CMD_NOISE_PITCH              = 128
        CMD_VOLUME_LEVEL             = 160
        CMD_WAIT                     = 175
      end
      include Headers

      TERMINATOR = CMD_TERMINATE.chr.freeze

      attr_reader :head, :fmt, :args

      module MetaCommand
        def compile(*_); ''.freeze; end
      end

      def initialize(head, fmt='', *args)
        @head = head
        @fmt = fmt
        @args = args
      end

      def compile(*_)
        [@head].pack('C'.freeze) + @args.pack(@fmt)
      end

      def loop?;      false; end

      def marker?;    false; end

      def pause?;     false; end

      def sub_track?; false; end

      def sub_instrument?; false; end

      def self.ticks_counter_from_code(code, resolver, start_offset:0, counter:0)
        raise ArgumentError if start_offset < 0 or start_offset > code.bytesize
        return counter if start_offset == code.bytesize
        byte_iter = code.slice(start_offset..-1).each_byte.with_index
        loop do
          head, index = begin
            byte_iter.next
          rescue StopIteration
            return counter
          end
          offset = start_offset + index
          case head
          when CMD_TERMINATE
            return counter
          when CMD_LOOP
            loop_counter, _ = byte_iter.next
            return nil if loop_counter.zero?
            loop_delta, _ = byte_iter.next
            loop_offset = offset + (-256|loop_delta)
            counter_after_loop_iteration = ticks_counter_from_code(code.slice(0, offset), resolver, start_offset:loop_offset, counter:counter)
            counter += loop_counter * (counter_after_loop_iteration - counter)
          when CMD_WAIT+1..CMD_WAIT+80
            ticks = head - CMD_WAIT
            counter += ticks
          when CMD_WAIT_MORE
            arg1, _ = byte_iter.next
            ticks = if arg1 < 80
              arg2, _ = byte_iter.next
              (((arg1 + 1) << 8)|arg2) + 1
            else
              arg1 + 1
            end
            counter += ticks
          when CMD_SUB_TRACK
            track_index, _ = byte_iter.next
            track = resolver.get_item(track_index)
            unless track.nil?
              counter = track.ticks_counter(counter)
            end
          when CMD_SET_TONE_PROGRESS
            4.times { byte_iter.next }
          when CMD_SET_AY_ENVELOPE_DURATION, CMD_SET_VIBRATO_STEP, CMD_SET_VIBRATO_ANGLE
            2.times { byte_iter.next }
          when CMD_SET_INSTRUMENT, CMD_SET_AY_ENVELOPE_SHAPE, CMD_SET_VOLUME_ENVELOPE, CMD_SET_NOISE_ENVELOPE,
               CMD_SET_CHORD, CMD_SET_VOLUME_CTRL_MASK, CMD_SET_TONE_CTRL_MASK, CMD_SET_NOISE_CTRL_MASK,
               CMD_SET_VIBRATO_AMPLITUDE, CMD_SET_NOTE_PROGRESS_PERIOD
            byte_iter.next
          when CMD_RESERVED0..CMD_RESERVED4
            raise "invalid command: #{head}"
          end
        end
      end
    end

    class MarkCommand < Command
      include MetaCommand
      attr_reader :mark_name
      def initialize(name=nil)
        name = name.to_s if Symbol === name
        raise ArgumentError, "marker name should be a string, a symbol or nil" unless String === name or name.nil?
        @mark_name = name || __id__
      end
      def marker?; true; end
    end

    class LoopCommand < Command
      attr_reader :loop_mark_name, :counter
      def initialize(loop_mark_name, repeat=nil)
        raise ArgumentError, "repeat: 2 - 256 or nil" unless (2..256).include?(repeat) or repeat.nil?
        @loop_mark_name = loop_mark_name
        @counter = repeat.nil? ? 0 : repeat - 1
        super(CMD_LOOP)
      end
      def compile(loop_offset, code_offset)
        delta = loop_offset - code_offset
        raise ArgumentError, "can't create a forward loop" if delta > 0
        raise ArgumentError, "can't create an empty loop" if delta == 0
        [@head, @counter, delta].pack('CCc')
      end
      def loop?; true; end
    end

    class PauseCommand < Command
      attr_reader :delay
      def initialize(ticks)
        ticks = ticks.to_r
        raise ArgumentError, "ticks should be a positive rational or integer" unless ticks > 0
        @delay = ticks
        super(CMD_WAIT)
      end
      def compile(rational_counter, delay_add=0)
        ticks = (rational_counter + delay + delay_add).round - rational_counter.round
        bytes = []
        while ticks > 20736
          bytes << CMD_WAIT_MORE << 79 << 255
          ticks -= 20736
        end
        if ticks > 256
          bytes << CMD_WAIT_MORE << (((ticks - 1) >> 8) - 1) << ((ticks - 1) & 255)
        elsif ticks > 80
          bytes << CMD_WAIT_MORE << (ticks - 1)
        elsif ticks > 0
          bytes << (@head + ticks)
        end
        [bytes.pack('C*'), ticks]
      end
      def pause?; true; end
    end

    class IndexCommand < Command
      attr_reader :name, :resolve_method
      def initialize(head, name, resolve_method)
        name = name.to_s if Symbol === name
        raise ArgumentError, "name should be a string, a symbol or nil" unless String === name or name.nil?
        @name = name
        raise ArgumentError, "resolve_method: should be a symbol of the resolver method name" unless Symbol === resolve_method
        @resolve_method = resolve_method
        super(head)
      end
      def compile(resolver)
        index = resolver.send(@resolve_method, @name)
        [@head, index].pack('CC')
      end
    end

    class SubTrackCommand < IndexCommand
      alias_method :track_name, :name
      def initialize(track_name)
        super(CMD_SUB_TRACK, track_name, :track_index)
      end
      def sub_track?; true; end
    end

    class SubInstrumentCommand < IndexCommand
      alias_method :instrument_name, :name
      def initialize(instrument_name)
        super(CMD_SUB_TRACK, instrument_name, :instrument_index)
      end
      def sub_instrument?; true; end
    end

    class InstrumentCommand < IndexCommand
      def initialize(instrument_name)
        super(CMD_SET_INSTRUMENT, instrument_name, :instrument_index)
      end
    end

    class EnvelopeCommand < IndexCommand
      def initialize(type, envelope_name)
        head = case type
        when :volume then CMD_SET_VOLUME_ENVELOPE
        when :noise then CMD_SET_NOISE_ENVELOPE
        else
          raise ArgumentError, "type should be :volume or :noise"
        end
        super(head, envelope_name, :envelope_index)
      end
    end

    class ChordCommand < IndexCommand
      def initialize(chord_name)
        super(CMD_SET_CHORD, chord_name, :chord_index)
      end
    end

    class MaskCommand < IndexCommand
      def initialize(type, mask_name)
        head = case type
        when :volume then CMD_SET_VOLUME_CTRL_MASK
        when :tone then CMD_SET_TONE_CTRL_MASK
        when :noise then CMD_SET_NOISE_CTRL_MASK
        else
          raise ArgumentError, "type should be :volume, :tone or :noise"
        end
        super(head, mask_name, :mask_index)
      end
    end

    class AYEnvelopeDurationCommand < Command
      def initialize(duration)
        raise ArgumentError, "duration not in range: 0 - 65535" unless (0..65535).include?(duration)
        super(CMD_SET_AY_ENVELOPE_DURATION, 'v', duration)
      end
    end

    class AYEnvelopeShapeCommand < Command
      def initialize(shape)
        raise ArgumentError, "shape not in range: 0 - 15" unless (0..15).include?(shape)
        super(CMD_SET_AY_ENVELOPE_SHAPE, 'C', shape)
      end
    end

    class VibratoStepCommand < Command
      def initialize(step=1.0)
        step = step.to_f
        raise ArgumentError, "step should be a finite number" unless step.finite?
        step = (step * 256.0).round
        super(CMD_SET_VIBRATO_STEP, 'v', step)
      end
    end

    class VibratoAngleCommand < Command
      def initialize(angle=0.0)
        angle = angle.to_f
        raise ArgumentError, "angle should be a finite number" unless angle.finite?
        angle = (angle * 256.0).round.abs
        super(CMD_SET_VIBRATO_ANGLE, 'v', angle)
      end
    end

    class VibratoAmplitudeCommand < Command
      def initialize(amplitude=1.0)
        amplitude = amplitude.to_f
        raise ArgumentError, "amplitude not in range: 0.0 - 1.0" unless amplitude >= 0.0 and amplitude <= 1.0
        amplitude = (amplitude * 255.0).round
        super(CMD_SET_VIBRATO_AMPLITUDE, 'C', amplitude)
      end
    end

    class NoteProgressPeriodCommand < Command
      def initialize(period)
        raise ArgumentError, "period not in tange: 0 - 255" unless (0..255).include?(period)
        super(CMD_SET_NOTE_PROGRESS_PERIOD, 'C', period)
      end
    end

    class ToneProgressCommand < Command
      def initialize(delta, counter)
        raise ArgumentError, "counter not in range: 1 - 65535" unless (1..65535).include?(counter)
        delta = delta.to_f
        raise ArgumentError, "delta should be a finite number" unless delta.finite?
        delta = (delta * 256.0 * 32.0 / 12.0 / counter).round
        raise ArgumentError, "absolute delta too big or counter too small" unless (-32768..32767).include?(delta)
        super(CMD_SET_TONE_PROGRESS, 'vv', delta, counter)
      end
    end

    class NoteCommand < Command
      NOTES = %w[a a! b c c! d d! e f f! g g!].map(&:to_sym).each_with_index.to_h
      def initialize(note, octave, first_octave_note=:a)
        raise ArgumentError, "octave must be an integer" unless Integer === octave
        octave_index = NOTES[first_octave_note]
        raise ArgumentError, "first_octave_note should be one of the notes as a symbol" unless octave_index
        note_index = NOTES[note]
        raise ArgumentError, "note should be one of the notes as a symbol" unless note_index
        octave -= 1 unless octave_index == 0 or note_index < octave_index
        raise ArgumentError, "octave not in range: 0 - 7" unless (0..7).include?(octave)
        value = note_index + octave * NOTES.length
        super(CMD_NOTE + value)
      end
    end

    class VolumeLevelCommand < Command
      def initialize(volume)
        raise ArgumentError, "volume not in range: 0 - 15" unless (0..15).include?(volume)
        super(CMD_VOLUME_LEVEL + volume)
      end
    end

    class NoisePitchCommand < Command
      def initialize(pitch)
        raise ArgumentError, "pitch not in range: 0 - 31" unless (0..31).include?(pitch)
        super(CMD_NOISE_PITCH + pitch)
      end
    end
  end
end
