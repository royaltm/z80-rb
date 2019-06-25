# -*- coding: BINARY -*-
module ZXUtils
  module MusicBox
  # :stopdoc:
    class Resolver
      IndexItem = ::Struct.new :name, :index, :type
      CHANNEL_NAMES = [:a, :b, :c].freeze
      def initialize(item_table, track_klasses:, multitrack_klasses:, instrument_klasses:, envelopes:, chords:, masks:)
        @item_table = item_table
        @indexed_items = {}
        @tracks = {}
        @track_klasses = track_klasses
        @multitrack_klasses = multitrack_klasses
        @instruments = {}
        @instrument_klasses = instrument_klasses
        @envelopes = envelopes
        @chords = chords
        @masks = masks
      end

      def envelope_index(name)
        return 0 if name.nil?
        raise "Envelope not found: #{name}" unless (envelope = @envelopes[name.to_sym])
        get_or_set(envelope, name, :envelope)
      end

      def chord_index(name)
        return 0 if name.nil?
        raise "Chord not found: #{name}" unless (chord = @chords[name.to_sym])
        get_or_set(chord, name, :chord)
      end

      def mask_index(name)
        return 0 if name.nil?
        raise "Mask not found: #{name}" unless (mask = @masks[name.to_sym])
        get_or_set(mask, name, :mask)
      end

      def instrument_index(name)
        return 0 if name.nil?
        name = name.to_sym
        unless (track = @instruments[name])
          raise "Instrument not found: #{name}" unless (track_klass = @instrument_klasses[name])
          @instruments[name] = :guard
          @instruments[name] = (track = track_klass.new self)
        end
        raise "Instrument called recursively: #{name}" if track == :guard
        get_or_set(track, name, :instrument)
      end

      def track_index(name)
        return 0 if name.nil?
        name = name.to_sym
        unless (track = @tracks[name])
          raise "Track not found: #{name}" unless (track_klass = @track_klasses[name])
          @tracks[name] = :guard
          @tracks[name] = (track = track_klass.new self)
        end
        raise "Sub track called recursively: #{name}" if track == :guard
        get_or_set(track, name, :track)
      end

      def multitrack_to_channel_track_name(name, channel)
        raise ArgumentError unless (0..2).include?(channel)
        return nil if name.nil?
        channel_track_name = :"#{name}__#{CHANNEL_NAMES[channel]}"
        name = name.to_sym
        unless @tracks.has_key?(channel_track_name)
          raise "Multitrack not found: #{name}" unless (multitrack_klass = @multitrack_klasses[name])
          multitrack = multitrack_klass.new self
          CHANNEL_NAMES.each do |ch|
            ch_name = :"#{name}__#{ch}"
            raise "Multitrack channel name in a collision with a track name: #{ch_name}" if @tracks.has_key?(ch_name)
            @tracks[ch_name] = :guard
            @tracks[ch_name] = multitrack.channel_track(ch)
          end
        end
        track = @tracks[channel_track_name]
        raise "Sub track called recursively: #{name}" if track == :guard
        channel_track_name
      end

      def get_item(index)
        return nil if index == 0
        @indexed_items[index] or raise "Resolver can't find an item: #{index}"
      end

      private

      def get_or_set(item, name, type)
        index = (@item_table[item] ||= IndexItem.new(name.to_s, @item_table.size + 1, type)).index
        @indexed_items[index] = item
        raise "Too many entries in a lookup table: #{name}" if index > 128
        index
      end

      public

      def unused_multitrack_names
        @multitrack_klasses.keys.reject do |k|
          (0..2).any? {|i| @tracks.has_key?(:"#{k}__#{CHANNEL_NAMES[i]}") }
        end
      end

      def unused_track_names
        @track_klasses.keys - @tracks.keys
      end

      def unused_instrument_names
        @instrument_klasses.keys - @instruments.keys
      end

      def unused_envelope_names
        @envelopes.reject {|n, o| @item_table.has_key?(o) }.keys
      end

      def unused_chord_names
        @chords.reject {|n, o| @item_table.has_key?(o) }.keys
      end

      def unused_mask_names
        @masks.reject {|n, o| @item_table.has_key?(o) }.keys
      end
    end
  end
end
