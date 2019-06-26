# -*- coding: BINARY -*-
require 'z80/tap' unless defined?(Z80::TAP)

module ZXUtils
  ##
  # ===MusicBox
  #
  # MusicBox is a Ruby Domain Specific Language designed to create AY-3-891x music.
  #
  # The music can be compiled to a ZXUtils::AYMusicPlayer module or a Z80::Program class
  # suitable for importing by another Z80 program.
  #
  # * MusicBox::Song
  # * MusicBox::Multitrack
  # * MusicBox::Track
  # * MusicBox::Instrument
  # * MusicBox::Envelope
  # * MusicBox::Chord
  # * MusicBox::Mask
  #
  # See also: ZXUtils::AYMusic, ZXUtils::AYMusicPlayer and ZXUtils::AYBasicPlayer.
  #
  #   require 'zxutils/music_box'
  #   
  #   class MySong
  #     include ZXUtils::MusicBox::Song
  #   
  #     tempo 128
  #   
  #     all_channels do
  #       tone_on; noise_off; volume 15
  #       set_instrument :instr1
  #     end
  #   
  #     channel :a do
  #       g 3, 4; e 3, 4; e 3, 4
  #       f 3, 4; d 3, 4; d 3, 4
  #       c 3, 8; e 3, 8; g 3, 4
  #     end
  #   
  #     channel :b do
  #       g 2, 4; e 2, 4; e 2, 4
  #       f 2, 4; d 2, 4; d 2, 4
  #       c 2, 8; e 2, 8; g 2, 4
  #     end
  #   
  #     channel :c do
  #       g 1, 4; e 1, 4; e 1, 4
  #       f 1, 4; d 1, 4; d 1, 4
  #       c 1, 8; e 1, 8; g 1, 4
  #     end
  #   
  #     all_channels do
  #       volume 0; pause 4
  #     end
  #   
  #     channel :a do
  #       g 3, 4; e 3, 4; e 3, 4
  #       f 3, 4; d 3, 4; d 3, 4
  #       c 3, 8; e 3, 8; c 3, 2
  #     end
  #   
  #     channel :b do
  #       g 2, 4; e 2, 4; e 2, 4
  #       f 2, 4; d 2, 4; d 2, 4
  #       c 2, 8; e 2, 8; c 2, 2
  #     end
  #   
  #     channel :c do
  #       g 1, 4; e 1, 4; e 1, 4
  #       f 1, 4; d 1, 4; d 1, 4
  #       c 1, 8; e 1, 8; c 1, 2
  #     end
  #   
  #     envelope :decay1, [1, 0.49], [64, -1.0], :loop, [255, 0.0]
  #   
  #     instrument :instr1 do
  #       volume 10; start_volume_envelope :decay1
  #     end
  #   end
  #   
  #   mysong = MySong.new
  #   mysong.to_player_module.save_tap 'mysong'
  #   
  #   require 'z80'
  #   puts mysong.to_program.new(0x8000).debug
  module MusicBox
    # :stopdoc:
    class ROHash < Hash # :nodoc:
      def []=(key, value)
        raise "already defined: #{key}" if has_key?(key)
        super
      end
    end
  end
end

require_relative 'music_box/command'
require_relative 'music_box/resolver'
require_relative 'music_box/track'
require_relative 'music_box/multitrack'
require_relative 'music_box/song'
