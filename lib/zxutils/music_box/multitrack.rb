# -*- coding: BINARY -*-
module ZXUtils
  module MusicBox
    module MultitrackCommands
      include TrackConfigCommands
      TracksEntry              = ::Struct.new :a, :b, :c
      SubMultitrackEntry       = ::Struct.new :multitrack_name
      SynchronizeChannelsEntry = ::Struct.new :a, :b, :c
      def self.extended(klass) # :nodoc:
        if klass.respond_to?(:new)
          klass.module_eval do
            @channels ||= []
            @tempo ||= DEFAULT_TEMPO
            @first_octave_note ||= DEFAULT_FIRST_OCTAVE_NOTE
          end
        end
        constants.each do |c|
          klass.const_set c, const_get(c) unless klass.const_defined?(c)
        end
      end

      def self.channel_name_to_index(channel)
        case channel
        when 0, 'a'.freeze, 'A'.freeze, :a, :A then 0
        when 1, 'b'.freeze, 'B'.freeze, :b, :B then 1
        when 2, 'c'.freeze, 'C'.freeze, :c, :C then 2
        else
          raise ArgumentError, "Couldn't guess which channel you've had in mind: #{channel.inspect}"
        end
      end

      ## Specify ranges of ticks behind for tracks' synchronization.
      def synchronize_channels(a:nil, b:nil, c:nil)
        @channels << SynchronizeChannelsEntry.new(a, b, c)
        @channels.length
      end

      def channel(ch_name, &block)
        ch_index = MultitrackCommands.channel_name_to_index ch_name
        tracks = if (last_entry = @channels.last) and TracksEntry === last_entry and last_entry[ch_index].nil?
          last_entry
        else
          TracksEntry.new.tap {|t| @channels << t }
        end
        first_octave_note_ = @first_octave_note
        tempo_ = @tempo
        tracks[ch_index] = Class.new do |klass|
          include Track
          first_octave_note(first_octave_note_)
          tempo(tempo_)
          klass.module_eval(&block)
        end
      end

      def for_channels(*chs, &block)
        chs.map {|ch| MultitrackCommands.channel_name_to_index ch }.
        each {|ch| channel(ch, &block) }
      end
      alias_method :for_ch, :for_channels

      def all_channels(&block)
        (0..2).each {|ch| channel(ch, &block) }
      end
      alias_method :all_ch, :all_channels

      def ch_a(&block)
        channel 0, &block
      end

      def ch_b(&block)
        channel 1, &block
      end

      def ch_c(&block)
        channel 2, &block
      end

      def sub_track(multitrack_name)
        @channels << SubMultitrackEntry.new(multitrack_name)
        @channels.length
      end
      alias_method :sub, :sub_track

      def wait(ticks)
        @channels << PauseCommand.new(ticks)
        @channels.length
      end
      alias_method :w, :wait

      def mark(name)
        @channels << MarkCommand.new(name)
        @channels.length
      end
      alias_method :m, :mark

      def loop_to(mark_name, repeat=nil)
        @channels << LoopCommand.new(mark_name, repeat)
        @channels.length
      end
      alias_method :lt, :loop_to

      def repeat(times=nil, mark:nil, &block)
        mark_cmd = MarkCommand.new(mark)
        @channels << mark_cmd
        yield
        unless @channels.last.equal? mark_cmd
          loop_to mark_cmd.mark_name, times
        end
        @channels.length
      end
      alias_method :rpt, :repeat
    end # MultitrackCommands

    module Multitrack
      def self.included(klass) # :nodoc:
        klass.extend MultitrackCommands if klass.respond_to?(:new)
      end

      attr_reader :channel_tracks, :resolver

      def initialize(resolver)
        @resolver = resolver
        @channel_tracks = Array.new(3) { |_| EmptyTrack.new(@resolver) }
        @ticks_behind_channels = [0..,0..,0..]
        self.class.instance_variable_get('@channels'.freeze).each { |e| self << e }
      end

      def channel_track(channel)
        channel = case channel
        when Symbol
          Resolver::CHANNEL_NAMES.index(channel)
        when Integer
          channel
        end
        raise ArgumentError, "unrecognized channel: #{channel}" unless channel
        @channel_tracks[channel]
      end

      def <<(entry)
        counters = @channel_tracks.map(&:ticks_counter)
        return unless maxcnt = counters.compact.max
        counters.each_with_index do |cnt, index|
          ticks_behind = @ticks_behind_channels[index]
          if (ticks = maxcnt - cnt) > 0
            if ticks_behind === ticks
              @channel_tracks[index] << PauseCommand.new(ticks)
            elsif ticks >= ticks_behind.begin
              raise "Channel #{index} synchronization failed. Ticks behind: #{ticks}, allowed: #{ticks_behind}"
            end
          end
        end
        case entry
        when MultitrackCommands::TracksEntry
          entry.entries.map { |t| t || EmptyTrack }.zip(@channel_tracks).map do |nt, mt|
            mt << nt
          end
        when Command
          @channel_tracks.each {|mt| mt << entry }
        when MultitrackCommands::SubMultitrackEntry
          @channel_tracks.each_with_index do |mt, ch|
            track_name = @resolver.multitrack_to_channel_track_name(entry.multitrack_name, ch)
            mt << SubTrackCommand.new(track_name)
          end
        when MultitrackCommands::SynchronizeChannelsEntry
          entry.each_with_index do |r, i|
            next if r.nil?
            raise ArgumentError, "Expected a range of integers" unless Range === r and Integer === r.begin
            @ticks_behind_channels[i] = r
          end
        else
          raise NotImplementedError
        end
        self
      end
    end # Multitrack
  end
end
