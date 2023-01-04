# -*- coding: BINARY -*-
module ZXUtils
	module MusicBox
		##
		# ===MusicBox SongCommands
		#
		# Commands for a Song.
		#
		# For the other available commands see: MultitrackCommands and TrackConfigCommands.
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
			##
			# Creates a track with the given +name+ as a symbol or a string.
			#
			# Give a block of code containing track commands. See MusicBox::Track.
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
			##
			# Imports a MusicBox::Track class with the given +name+ as a symbol or a string.
			def import_track(name, track)
				raise ArgumentError unless track.is_a?(Class) and track.ancestors.include?(Track)
				@tracks[name.to_sym] = track
			end
			##
			# Creates an instrument with the given +name+ as a symbol or a string.
			#
			# Give a block of code containing instrument commands. See MusicBox::Instrument.
			def instrument(name, &block)
				first_octave_note_ = @first_octave_note
				tempo_ = @tempo
				@instruments[name.to_sym] = Class.new do |klass|
					include Instrument
					tempo(tempo_)
					klass.module_eval(&block)
				end
			end
			##
			# Imports a MusicBox::Instrument class with the given +name+ as a symbol or a string.
			def import_instrument(name, track)
				raise ArgumentError unless track.is_a?(Class) and track.ancestors.include?(Instrument)
				@instruments[name.to_sym] = track
			end
			##
			# Creates a multi-track with the given +name+ as a symbol or a string.
			#
			# Give a block of code containing multi-track commands. See MusicBox::Multitrack.
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
			##
			# Imports a MusicBox::Multitrack class with the given +name+ as a symbol or a string.
			def import_multitrack(name, multitrack)
				raise ArgumentError unless multitrack.is_a?(Class) and multitrack.ancestors.include?(Multitrack)
				@multitracks[name.to_sym] = multitrack
			end
			##
			# Creates an envelope with the given +name+ as a symbol or a string.
			# Provide +args+ for the MusicBox::Envelope.new.
			#
			# See MusicBox::Envelope.
			def envelope(name, *args)
				@envelopes[name.to_sym] = Envelope.new(*args)
			end
			##
			# Imports a MusicBox::Envelope instance with the given +name+ as a symbol or a string.
			def import_envelope(name, envelope)
				raise ArgumentError unless envelope.is_a?(Envelope)
				@envelopes[name.to_sym] = envelope
			end
			##
			# Creates a chord with the given +name+ as a symbol or a string.
			# Provide +args+ for the MusicBox::Chord.new.
			#
			# See MusicBox::Chord.
			def chord(name, *args)
				@chords[name.to_sym] = Chord.new(*args)
			end
			##
			# Imports a MusicBox::Chord instance with the given +name+ as a symbol or a string.
			def import_chord(name, chord)
				raise ArgumentError unless chord.is_a?(Chord)
				@chords[name.to_sym] = chord
			end
			##
			# Creates a mask with the given +name+ as a symbol or a string.
			# Provide +args+ for the MusicBox::Mask.new.
			#
			# See MusicBox::Mask.
			def mask(name, *args)
				@masks[name.to_sym] = Mask.new(*args)
			end
			##
			# Imports a MusicBox::Mask instance with the given +name+ as a symbol or a string.
			def import_mask(name, mask)
				raise ArgumentError unless mask.is_a?(Mask)
				@masks[name.to_sym] = mask
			end
		end # SongCommands
		##
		# ===MusicBox Song
		#
		# A song is a special Multitrack that also organizes other multi-tracks, sub-tracks, instruments, envelopes,
		# masks and chords.
		#
		# To create a custom song you need to include the Song module in your class.
		#
		#    class MySong
		#        include ZXUtils::MusicBox::Song
		#        #... song commands follow
		#    end
		#
		# To compile a song just instantiate it:
		#   mysong = MySong.new
		#   puts mysong.channel_tracks.map(&:ticks_counter)
		#   puts mysong.channel_tracks.map(&:max_recursion_depth)
		#
		# To validate recursion depth of tracks and instruments do:
		#   mysong.validate_recursion_depth!
		#
		# Convert to a program class:
		#   require 'z80'
		#   puts mysong.to_program.new(0x8000).debug
		#
		# Or save as a player module:
		#   mysong.to_player_module.save_tap('mysong')
		#
		# ====Commands
		#
		# For the description of available commands see MultitrackCommands and SongCommands.
		module Song
			include Multitrack
			def self.included(klass) # :nodoc:
				klass.extend SongCommands if klass.respond_to?(:new)
			end
			##
			# A hash containing compiled and used items as keys and item descriptors as values.
			attr_reader :item_table
			##
			# Creates and instance of the song.
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
			##
			# Checks if maximal recursion depth of tracks and instruments is not exceeding the given threshold.
			#
			# Provide the maximum allowed +track_stack_depth+. You may want to use ZXUtils::AYMusic::TRACK_STACK_DEPTH constant.
			#
			# Raises an error when recursion depth is exceeding +track_stack_depth+.
			def validate_recursion_depth!(track_stack_depth=20)
				max_recursion_depth = 0
				check_level = proc do |track, &block|
					max_recursion_depth = track.max_recursion_depth if track.max_recursion_depth > max_recursion_depth
					block.call if max_recursion_depth > track_stack_depth
				end
				channel_tracks.each_with_index do |track, ch_num|
					check_level.call(track) do
						raise "too many recursions on track_#{Resolver::CHANNEL_NAMES[ch_num]} depth: #{max_recursion_depth} > #{track_stack_depth}"
					end
				end
				instruments.each do |name, instrument|
					check_level.call(instrument) do
						raise "too many recursions on instrument :#{name} depth: #{max_recursion_depth} > #{track_stack_depth}"
					end
				end
				max_recursion_depth
			end
			##
			# Returns a hash of instruments used in a song.
			# Keys are instrument names and values are Instrument instances.
			def instruments
				resolver.instruments
			end
			##
			# Returns a hash with unused item names in each of the item category.
			def unused_item_names
				{ multitracks: resolver.unused_multitrack_names,
					tracks: resolver.unused_track_names,
					instruments: resolver.unused_instrument_names,
					envelopes: resolver.unused_envelope_names,
					chords: resolver.unused_chord_names,
					masks: resolver.unused_mask_names }
			end
			##
			# Returns an instance of the PlayerModule from the compiled Song instance.
			def to_player_module
				to_module.to_player_module
			end
			##
			# Returns an ad-hoc Z80::Program class containing the compiled Song.
			# See SongModule.to_program.
			def to_program
				to_module.to_program
			end
			##
			# Returns an instance of the SongModule from the compiled Song instance.
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
			##
			# ===MusicBox Song SongModule
			#
			# An instance of this class can be created by calling Song.to_module instance method
			# of the compiled song.
			#
			# The instance of this class can produce an instance of PlayerModule or a Z80::Program.
			# 
			# Example:
			#
			#   require 'zxutils/ay_music'
			#   require 'zxlib/basic'
			#   require_relative 'examples/test_music'
			#   
			#   class Music
			#     include Z80
			#     include Z80::TAP
			#     include ZXUtils
			#   
			#     MusicData = TestMusic.new.to_program
			#   
			#     macro_import        MathInt
			#     macro_import        Utils::SinCos
			#     import              ZXLib::Sys, macros: true
			#     macro_import        ZXLib::AYSound
			#     macro_import        AYMusic
			#   
			#     with_saved :start, :exx, hl, ret: :after_ei do
			#                         call make_sincos
			#                         ay_expand_notes( music.notes, octaves:8 )
			#                         ay_music_tone_progress_table_factory( music.fine_tones )
			#                         ay_music_note_to_fine_tone_cursor_table_factory( music.note_to_cursor, play: music.play )
			#                         di
			#                         call music.init
			#                         dw   track_a, track_b, track_c
			#       forever           ei
			#                         halt
			#                         di
			#                         push iy
			#                         call music.play
			#                         pop  iy
			#                         key_pressed?
			#                         jr   Z, forever
			#                         ay_init
			#     end
			#   
			#     make_sincos         create_sincos_from_sintable music.sincos, sintable:sintable
			#     sintable            bytes   neg_sintable256_pi_half_no_zero_lo
			#   
			#     song                import MusicData
			#     song_end            label
			#   
			#     import              AYMusic, :music, override: { index_table: index_table }
			#     music_end           label
			#   
			#     NOTES = ay_tone_periods(min_octave:0, max_octave:0)
			#                         dw NOTES[11]*2
			#     notes               dw NOTES
			#   end
			#   
			#   music = Music.new 0x8000
			#   puts music.debug
			#   program = ZXLib::Basic.parse_source <<-EOC
			#     10 RANDOMIZE USR #{music[:start]}
			#   9998 STOP: GO TO 10
			#   9999 CLEAR #{music.org-1}: LOAD ""CODE: RUN
			#   EOC
			#   puts program.to_source escape_keywords: true
			#   program.save_tap "music", line: 9999
			#   music.save_tap "music", append: true
			#   Z80::TAP.parse_file('music.tap') do |hb|
			#       puts hb.to_s
			#   end
			class SongModule
				## A compiled song module body as a binary string.
				attr_reader :code
				## An array containing offset ranges determining the position within the SongModule.code each of the three channel tracks.
				attr_reader :track_offsets
				## An array containing offset ranges determining the position within the SongModule.code each of the indexed items.
				attr_reader :index_offsets
				## An array containing descriptors of each of the indexed items.
				attr_reader :index_items
				def initialize(track_offsets, index_offsets, index_items, code) # :nodoc:
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
				##
				# Returns an instance of the PlayerModule from the compiled SongModule instance.
				def to_player_module
					PlayerModule.new(*@track_offsets.map(&:first), @index_offsets.map(&:first), @code)
				end
				##
				# Returns an ad-hoc Z80::Program class containing the compiled SongModule.
				#
				# The returned program exports the following labels:
				# * track_a
				# * track_b
				# * track_c
				# * index_table
				# That can be passed to the the ZXUtils::AYMusic engine.
				# For the complete example, see SongModule.
				#
				# Additionally these label namespaces are also being exported:
				# * track
				# * instrument
				# * envelope
				# * chord
				# * mask
				# And each song's item is being sub-labeled in the according namespaces, so you can
				# identify them in the +debug+ output or use it for some other purpose.
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
			##
			# ===MusicBox Song PlayerModule
			#
			# A PlayerModule instance contains a compiled Song in the form suitable for the ZXUtils::AYMusicPlayer.
			#
			# An instance of this class can be created by calling Song.to_player_module instance method
			# of the compiled song or SongModule.to_player_module instance method.
			#
			# Use Z80::TAP.save_tap included method to save the module as a +.tap+ file.
			class PlayerModule
				include Z80::TAP
				## A compiled song module body, including the relocation table, as a binary string suitable for the ZXUtils::AYMusicPlayer.
				attr_reader :code
				## An address the module will be saved at when using one of the Z80::TAP methods.
				attr_accessor :org
				def initialize(track1_offset, track2_offset, track3_offset, index_offsets, code, org=32768) # :nodoc:
					@org = org
					relative_offset = 2 * (3 + index_offsets.length) + 1
					@code = [track1_offset, track2_offset, track3_offset].concat(index_offsets).map do |offset|
						relative_offset -= 2
						offset + relative_offset
					end.pack('v*') + code
				end
			end
		end # Song
		##
		# ===MusicBox Envelope
		#
		# Instances of this class represent the envelopes applicable to the volume level or the noise pitch.
		class Envelope
			# A compiled body of the evelope as a binary string.
			attr_reader :code
			##:call-seq:
			#       Envelope.new [counter, delta], ...
			#       Envelope.new [counter, delta], ..., :loop, ..., [counter, delta]
			#
			# Creates an instance of the Envelope with the given tuples shaping the envelope.
			#
			# +counter+:: How many ticks should pass to reach the desired +delta+: 1 to 255.
			# +delta+:: A floating point number in the range: from -1 to 1.
			# +:loop+:: An indicator where to go back when the end of the envelope is being reached.
			#           If not present the whole envelope is being repeated.
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
				raise ArgumentError, "Envelope requires at least one tuple argument" if bytes.empty?
				loop_offset = loop_offset.to_i
				raise ArgumentError, "Loop indicator must be placed before at least one argument tuple" if loop_offset == bytes.length
				raise ArgumentError, "Loop offset too large" if loop_offset > 255
				@code = [loop_offset].concat(bytes).pack('c*')
			end

			private

			def tuple_to_value(count, delta)
				raise ArgumentError, "Invalid counter argument: #{count}" unless (1..255).include?(count)
				step = (delta.to_f * 255.0 / count).round
				raise ArgumentError, "Value argument is too large: #{delta}/#{count}: #{step}" unless (-128..127).include?(step)
				[count, step]
			end
		end
		##
		# ===MusicBox Chord
		#
		# Instances of this class represent the chords applicable to the played note's tone.
		class Chord < Envelope
			##:call-seq:
			#       Chord.new [counter, nhalftones], ...
			#       Chord.new [counter, nhalftones], ..., :loop, ..., [counter, nhalftones]
			#
			# Creates an instance of the Chord with the given tuples defining the chord.
			#
			# +counter+:: For how many ticks the following +nhalftones+ should be added to the base note: 1 to 7.
			# +nhalftones+:: A number of half-tones added to the base note: 0 to 31.
			# +:loop+:: An indicator where to go back when the end of the chord is being reached.
			#           If not present the whole chord is being repeated.
			def initialize(*args)
				super
			end
			private
			def tuple_to_value(count, delta)
				raise ArgumentError, "Counter: #{count} not in range: 1..7" unless (1..7).include?(count)
				raise ArgumentError, "Value: #{delta} not in range: 0..31" unless (0..31).include?(delta)
				[(count<<5)|delta]
			end
		end
		##
		# ===MusicBox Mask
		#
		# Instances of this class represent the bit masks applicable to the channel's mixer tone or noise
		# control bits or the volume envelope control bit.
		class Mask < Envelope
			##:call-seq:
			#       Mask.new [counter, bitmask], ...
			#       Mask.new [counter, bitmask], ..., :loop, ..., [counter, bitmask]
			#
			# Creates an instance of the Mask with the given tuples defining bits for the mask.
			#
			# +counter+:: For how many ticks apply the bits of the following +bitmask+: 1 to 255.
			# +bitmask+:: An 8-bit integer representing the 8 mask values: 0b00000000 to 0b11111111.
			#             The bits are being applied for each tick in turn starting from the most (leftmost)
			#             significant (7th) bit of the +bitmask+ value down to the least significant one.
			#             If the +counter+ is higher than 8 the bits are being applied repeatedly.
			# +:loop+:: An indicator where to go back when the end of the mask is being reached.
			#           If not present the whole mask is being repeated.
			def initialize(*args)
				super
			end
			private
			def tuple_to_value(count, mask)
				raise ArgumentError, "Counter: #{count} not in range: 1..255" unless (1..255).include?(count)
				raise ArgumentError, "Value: mask excess 8 bits" unless (0..255).include?(mask)
				[count, mask]
			end
		end
	end
end
