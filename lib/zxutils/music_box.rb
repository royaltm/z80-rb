# -*- coding: BINARY -*-
require 'z80/tap' unless defined?(Z80::TAP)

module ZXUtils
  module MusicBox
    class ROHash < Hash
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
