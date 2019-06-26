# -*- coding: BINARY -*-
module ZXUtils
  module MusicBox
    ##
    # ===MusicBox MultitrackCommands
    #
    # Commands for multi-tracks.
    #
    # For the other available commands see: TrackConfigCommands.
    module MultitrackCommands
      include TrackConfigCommands
      TracksEntry              = ::Struct.new :a, :b, :c # :nodoc:
      SubMultitrackEntry       = ::Struct.new :multitrack_name # :nodoc:
      SynchronizeChannelsEntry = ::Struct.new :a, :b, :c # :nodoc:
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
      ##
      # Returns a channel index: 0 to 2 for the given +channel+ name.
      #
      # A +channel+ can be an integer: 0 to 2 or one of the symbols or strings: +:a+, +:b+, +:c+.
      def self.channel_name_to_index(channel)
        case channel
        when 0, 'a'.freeze, 'A'.freeze, :a, :A then 0
        when 1, 'b'.freeze, 'B'.freeze, :b, :B then 1
        when 2, 'c'.freeze, 'C'.freeze, :c, :C then 2
        else
          raise ArgumentError, "Couldn't guess which channel you've had in mind: #{channel.inspect}"
        end
      end
      ##:call-seq:
      #       synchronize_channels a:nil, b:nil, c:nil
      #
      # Specify ranges of allowed ticks for each channel's track synchronization.
      # If the given track is behind the most advanced track N ticks it will be synchronized only
      # if the N matches the given range. However if the upper bound of the range is given, when exceeded
      # a compilation error will occur.
      #
      # If the value is +nil+ the previous range, set for the channel is not being changed.
      #
      # Example:
      #   synchronize_channels a:2..10, b:0.., c:5...5
      # The above command will allow channel A to fall behind by no more than 10 ticks and will be synchronized
      # only if it lags 2 or more ticks behind.
      # It will also allow channel B to fall behind by any number of ticks and will always be synchronized.
      # It will also restrict channel C to fall behind by a maximum 4 number of ticks and will never be synchronized.
      #
      # By default a:0.., b:0.., c:0.. is set.
      def synchronize_channels(a:nil, b:nil, c:nil)
        @channels << SynchronizeChannelsEntry.new(a, b, c)
        @channels.length
      end
      ##:call-seq:
      #       channel channel_name {|| ... }
      #
      # Creates a track fragment for the given +channel_name+.
      #
      # Provide a block with commands suitable for MusicBox::Track.
      #
      # A +channel_name+ can be an integer: 0 to 2 or one of the symbols or strings: +:a+, +:b+, +:c+.
      #
      # The track will be synchronized accordingly with the other channel's tracks.
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
      ##:call-seq:
      #       for_channels *channel_names {|| ... }
      #       for_ch *channel_names {|| ... }
      #
      # Creates a track fragments with the same commands for the channels indicated by +channel_names+.
      #
      # Provide a block with commands suitable for MusicBox::Track.
      #
      # A +channel_names+ can be integers: 0 to 2 or the symbols or strings: +:a+, +:b+, +:c+.
      #
      # The tracks will be synchronized accordingly with the other channel's tracks.
      def for_channels(*chs, &block)
        chs.map {|ch| MultitrackCommands.channel_name_to_index ch }.
        each {|ch| channel(ch, &block) }
      end
      alias_method :for_ch, :for_channels
      ##:call-seq:
      #       all_channels {|| ... }
      #       all_ch {|| ... }
      #
      # Creates a track fragments with the same commands for all the channels.
      #
      # Provide a block with commands suitable for MusicBox::Track.
      #
      # The tracks will be synchronized accordingly with the other channel's tracks.
      def all_channels(&block)
        (0..2).each {|ch| channel(ch, &block) }
      end
      alias_method :all_ch, :all_channels
      ##:call-seq:
      #       ch_a {|| ... }
      #
      # Creates a track fragment for the A channel.
      #
      # Provide a block with commands suitable for MusicBox::Track.
      # The track will be synchronized accordingly with the other channel's tracks.
      def ch_a(&block)
        channel 0, &block
      end
      ##:call-seq:
      #       ch_b {|| ... }
      #
      # Creates a track fragment for the B channel.
      #
      # Provide a block with commands suitable for MusicBox::Track.
      # The track will be synchronized accordingly with the other channel's tracks.
      def ch_b(&block)
        channel 1, &block
      end
      ##:call-seq:
      #       ch_c {|| ... }
      #
      # Creates a track fragment for the C channel.
      #
      # Provide a block with commands suitable for MusicBox::Track.
      # The track will be synchronized accordingly with the other channel's tracks.
      def ch_c(&block)
        channel 2, &block
      end
      ##:call-seq:
      #       sub_track multitrack_name
      #       sub multitrack_name
      #
      # Yields execution of the tracks to another multi-track with the given +multitrack_name+ as a symbol or a string.
      # When the sub-tracks execution is finished, the yielding multi-track will resume execution.
      # The tracks will be synchronized accordingly with the other channel's tracks.
      def sub_track(multitrack_name)
        @channels << SubMultitrackEntry.new(multitrack_name)
        @channels.length
      end
      alias_method :sub, :sub_track
      ##:call-seq:
      #       wait ticks
      #       w ticks
      #
      # Pauses tracks execution for +ticks+ number of ticks.
      # The +ticks+ value should be a positive integer as an Integer or a Rational.
      #
      # For the TrackConfigCommands.tempo related pause see MultitrackCommands.pause.
      # The tracks will be synchronized accordingly with the other channel's tracks.
      def wait(ticks)
        @channels << PauseCommand.new(ticks)
        @channels.length
      end
      alias_method :w, :wait
      ##:call-seq:
      #       pause length, *additional_lengths
      #       p length, *additional_lengths
      #
      # Pauses tracks execution for a +length+ period.
      # The +length+ value should be a positive integer.
      #
      # The number of +ticks+ paused is being calculated based on the TrackConfigCommands.tempo value.
      #
      #   ticks = tempo / length
      #
      # For the tempo unrelated pause see MultitrackCommands.wait.
      # The tracks will be synchronized accordingly with the other channel's tracks.
      def pause(length, *length_exts)
        ticks = Rational(@tempo, length)
        length_exts.each do |len_ext|
          ticks += Rational(@tempo, len_ext)
        end
        wait(ticks)
      end
      alias_method :p, :pause
      ##:call-seq:
      #       mark name
      #       m name
      #
      # Marks a point in tracks and gives it a +name+ as a symbol or a string.
      # You can later use MultitrackCommands.loop_to with a marked point name.
      # The tracks will be synchronized accordingly with the other channel's tracks.
      def mark(name)
        @channels << MarkCommand.new(name)
        @channels.length
      end
      alias_method :m, :mark
      ##:call-seq:
      #       loop_to name, [repeat=nil]
      #       lt name, [repeat=nil]
      #
      # Loops execution from the marked point +name+. Repeats +repeat+ times.
      # If +repeat+ is +nil+ or missing loops _forever_.
      #
      # See also MultitrackCommands.mark and MultitrackCommands.repeat.
      # The tracks will be synchronized accordingly with the other channel's tracks.
      def loop_to(mark_name, repeat=nil)
        @channels << LoopCommand.new(mark_name, repeat)
        @channels.length
      end
      alias_method :lt, :loop_to
      ##:call-seq:
      #       repeat [repeat=nil] {|| ... }
      #       rpt [repeat=nil] {|| ... }
      #
      # Repeats the execution of the commands in the given block +repeat+ times.
      # If +repeat+ is +nil+ or missing repeats _forever_.
      #
      # See also MultitrackCommands.mark and MultitrackCommands.loop_to.
      # The tracks will be synchronized accordingly with the other channel's tracks.
      def repeat(times=nil, mark:nil, &block)
        mark_cmd = MarkCommand.new(mark)
        @channels << mark_cmd
        if times != 0
          yield
          unless @channels.last.equal?(mark_cmd) or times == 1
            loop_to mark_cmd.mark_name, times
          end
        end
        @channels.length
      end
      alias_method :rpt, :repeat
    end # MultitrackCommands
    ##
    # ===MusicBox Multitrack
    #
    # A multi-track consists of the three tracks, each one for each of the AY-3-891x channels.
    #
    # Each track of a multi-track is a MusicBox::Track class.
    #
    # Before each multi-track command the tracks are being synchronized by applying wait command
    # whenever the track falls behind other tracks in the number of execution ticks.
    # To control synchronization see command: MultitrackCommands.synchronize_channels.
    #
    # To create a custom multi-track you need to include the Multitrack module in your class representing
    # the given multi-track.
    #
    #    class ChannelTracks1
    #        include ZXUtils::MusicBox::Multitrack
    #        #... multi-track commands follow
    #    end
    #
    # Such a multi-track can be included with the MusicBox::SongCommands.import_multitrack command of the Song.
    #
    # Alternatively use MusicBox::SongCommands.multitrack command to define multi-tracks directly in the Song.
    #
    # ====Commands
    #
    # For the list of available commands see MultitrackCommands.
    module Multitrack
      def self.included(klass) # :nodoc:
        klass.extend MultitrackCommands if klass.respond_to?(:new)
      end
      ## An array containing compiled tracks for each channel.
      attr_reader :channel_tracks
      ## A resolver used by the compilation process.
      attr_reader :resolver
      ##
      # Instances of the derived classes are being created internally by the MusicBox::Song compilation process.
      def initialize(resolver)
        @resolver = resolver
        @channel_tracks = Array.new(3) { |_| EmptyTrack.new(@resolver) }
        @ticks_behind_channels = [0..,0..,0..]
        self.class.instance_variable_get('@channels'.freeze).each { |e| self << e }
      end
      ##
      # Returns an instance of the compiled track for the given +channel+.
      # A +channel+ can be an integer: 0 to 2 or one of the symbols or strings: +:a+, +:b+, +:c+.
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

      def <<(entry) # :nodoc:
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
