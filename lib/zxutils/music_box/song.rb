# -*- coding: BINARY -*-
module ZXUtils
  module MusicBox
    module SongCommands
      include MultitrackCommands
      def self.extended(klass) # :nodoc:
        if klass.respond_to?(:new)
          klass.module_eval do
            @instruments ||= ROHash.new
            @tracks      ||= ROHash.new
            @multitracks ||= ROHash.new
            @envelopes   ||= ROHash.new
            @chords      ||= ROHash.new
            @masks       ||= ROHash.new
          end
        end
        constants.each do |c|
          klass.const_set c, const_get(c) unless klass.const_defined?(c)
        end
        MultitrackCommands.extended(klass)
      end

      def track(name, &block)
        first_octave_note_ = @first_octave_note
        tempo_ = @tempo
        @tracks[name.to_sym] = Class.new do |klass|
          include Track
          first_octave_note(first_octave_note_)
          tempo(tempo_)
          klass.module_eval(&block)
        end
      end

      def import_track(name, track)
        raise ArgumentError unless track.is_a?(Class) and track.ancestors.include?(Track)
        @tracks[name.to_sym] = track
      end

      def instrument(name, &block)
        first_octave_note_ = @first_octave_note
        tempo_ = @tempo
        @instruments[name.to_sym] = Class.new do |klass|
          include Instrument
          tempo(tempo_)
          klass.module_eval(&block)
        end
      end

      def import_instrument(name, track)
        raise ArgumentError unless track.is_a?(Class) and track.ancestors.include?(Instrument)
        @instruments[name.to_sym] = track
      end

      def multitrack(name, &block)
        first_octave_note_ = @first_octave_note
        tempo_ = @tempo
        @multitracks[name.to_sym] = Class.new do |klass|
          include Multitrack
          first_octave_note(first_octave_note_)
          tempo(tempo_)
          klass.module_eval(&block)
        end
      end

      def import_multitrack(name, multitrack)
        raise ArgumentError unless multitrack.is_a?(Class) and multitrack.ancestors.include?(Multitrack)
        @multitracks[name.to_sym] = multitrack
      end

      def envelope(name, *args)
        @envelopes[name.to_sym] = Envelope.new(*args)
      end

      # def import_envelope(name, envelope)
      #   raise ArgumentError unless envelope.is_a?(Envelope)
      #   @envelopes[name.to_sym] = envelope
      # end

      def chord(name, *args)
        @chords[name.to_sym] = Chord.new(*args)
      end

      # def import_chord(name, chord)
      #   raise ArgumentError unless chord.is_a?(Chord)
      #   @chords[name.to_sym] = chord
      # end

      def mask(name, *args)
        @masks[name.to_sym] = Mask.new(*args)
      end

      # def import_mask(name, mask)
      #   raise ArgumentError unless mask.is_a?(Mask)
      #   @masks[name.to_sym] = mask
      # end
    end # SongCommands

    module Song
      include Multitrack
      def self.included(klass) # :nodoc:
        klass.extend SongCommands if klass.respond_to?(:new)
      end

      attr_reader :item_table

      def initialize
        @item_table = {}
        pargs = self.class.module_eval do
          { track_klasses:      @tracks,
            multitrack_klasses: @multitracks,
            instrument_klasses: @instruments,
            envelopes:          @envelopes,
            chords:             @chords,
            masks:              @masks }
        end
        super(Resolver.new @item_table, **pargs)
      end

      def unused_item_names
        { multitracks: resolver.unused_multitrack_names,
          tracks: resolver.unused_track_names,
          instruments: resolver.unused_instrument_names,
          envelopes: resolver.unused_envelope_names,
          chords: resolver.unused_chord_names,
          masks: resolver.unused_mask_names }
      end

      def to_player_module
        to_module.to_player_module
      end

      def to_program
        to_module.to_program
      end

      def to_module
        body = ''
        track_offsets = @channel_tracks.map do |track|
          offset = body.bytesize
          body << track.code << Command::TERMINATOR
          offset...body.bytesize
        end
        index_offsets = @item_table.each_key.map do |item|
          offset = body.bytesize
          body << item.code << Command::TERMINATOR
          offset...body.bytesize
        end
        SongModule.new track_offsets, index_offsets, item_table.values, body
      end

      class SongModule
        attr_reader :track_offsets, :index_offsets, :index_items, :code
        def initialize(track_offsets, index_offsets, index_items, code)
          raise ArgumentError unless index_offsets.length == index_items.length and
                                     index_offsets.all? {|r| Range === r } and
                                     track_offsets.length == 3 and
                                     track_offsets.all? {|r| Range === r } and
                                     index_items.all? {|i| Resolver::IndexItem === i } and
                                     String === code
          @track_offsets = track_offsets
          @index_offsets = index_offsets
          @index_items = index_items
          @code = code
        end

        def to_player_module
          PlayerModule.new(*@track_offsets.map(&:first), @index_offsets.map(&:first), @code)
        end

        def to_program
          code = @code
          track_offsets = @track_offsets
          index_offsets = @index_offsets
          index_items = @index_items
          index_labels = Array.new(index_offsets.length)
          Class.new do
            include Z80
            include Z80::TAP

            export :auto
            track_a       data code.slice(track_offsets[0])
            track_b       data code.slice(track_offsets[1])
            track_c       data code.slice(track_offsets[2])
            index_items.zip(index_offsets).
            sort_by {|item, _| "#{item.type}.#{item.name}"}.
            chunk {|item, _| item.type}.
            each do |type, items|
              isolate type do
                items.each do |item, offsets|
                  index_labels[item.index - 1] = define_label item.name, data(code.slice(offsets))
                end
              end
            end
            raise "sanity error" if index_labels.length != index_offsets.length or index_labels.any?(&:nil?)
            index_table   words index_labels
          end
        end
      end

      class PlayerModule
        include Z80::TAP
        attr_reader :code
        attr_accessor :org
        def initialize(track1_offset, track2_offset, track3_offset, index_offsets, code, org=32768)
          @org = org
          relative_offset = 2 * (3 + index_offsets.length) + 1
          @code = [track1_offset, track2_offset, track3_offset].concat(index_offsets).map do |offset|
            relative_offset -= 2
            offset + relative_offset
          end.pack('v*') + code
        end
      end
    end # Song

    class Envelope
      attr_reader :code

      def initialize(*args)
        loop_offset = nil
        bytes = []
        args.each do |arg|
          case arg
          when Array
            bytes.concat Array(tuple_to_value(*arg))
          when :loop
            raise ArgumentError, "There's more than one loop marker" unless loop_offset.nil?
            loop_offset = bytes.length
          else
            raise ArgumentError, "A tuple of [counter, value] is expected or :loop"
          end
        end
        loop_offset = loop_offset.to_i
        raise ArgumentError, "Loop offset too large" if loop_offset > 255
        @code = [loop_offset].concat(bytes).pack('c*')
      end

      def tuple_to_value(count, delta)
        raise ArgumentError, "Invalid counter argument: #{count}" unless (1..255).include?(count)
        step = (delta.to_f * 255.0 / count).round
        raise ArgumentError, "Value argument is too large: #{delta}/#{count}: #{step}" unless (-128..127).include?(step)
        [count, step]
      end
    end

    class Chord < Envelope
      def tuple_to_value(count, delta)
        raise ArgumentError, "Counter: #{count} not in range: 1..7" unless (1..7).include?(count)
        raise ArgumentError, "Value: #{delta} not in range: 0..31" unless (0..31).include?(delta)
        [(count<<5)|delta]
      end
    end

    class Mask < Envelope
      def tuple_to_value(count, mask)
        raise ArgumentError, "Counter: #{count} not in range: 1..255" unless (1..255).include?(count)
        raise ArgumentError, "Value: mask excess 8 bits" unless (0..255).include?(mask)
        [count, mask]
      end
    end
  end
end
