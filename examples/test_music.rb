require 'zxlib/ay_sound'
require 'zxutils/music_box'

class TestMusic
  include ZXUtils::MusicBox::Song
  include ZXLib::AYSound::EnvelopeControl
  tempo 128

  all_channels { tone_off; noise_off; volume 0 }

  mark :noise_test1

  ch_a do
    noise 31; noise_on; volume 15; p 2; noise_off; volume 0
  end

  mark :noise_test2

  ch_c do
    noise_on; volume 15; p 2; noise_off; volume 0
  end

  mark :noise_test3

  ch_b do
    noise_on; volume 15; p 2; noise_off; volume 0
  end

  mark :noise_test4

  ch_a do
    noise 0
    noise_on; volume 15; p 1; noise_off; volume 0
  end
  ch_b do
    p 4; noise_on; volume 15; p 1; noise_off; volume 0
  end
  ch_c do
    p 2; noise_on; volume 15; p 1; noise_off; volume 0
  end

  mark :noise_test5

  ch_a do
    start_noise_envelope :wave1
    noise_on; volume 0; start_volume_envelope :wave2
  end
  ch_b do
    p 2; noise_on; volume 0; start_volume_envelope :wave2
  end
  ch_c do
    p 1; noise_on; volume 0; start_volume_envelope :wave2
  end

  p 1, 1, 2

  ch_a do
    start_volume_envelope :decay3
  end
  ch_b do
    p 2; start_volume_envelope :decay3
  end
  ch_c do
    p 4; start_volume_envelope :decay3
  end

  p 1

  ch_a { noise_envelope_off; noise 0 }

  all_channels { noise_off; volume_envelope_off; volume 0; }

  mark :scale_test

  all_channels { tone_on; noise_off; volume 15 }

  sub :play_scale

  all_channels { set_instrument :instr1 }

  sub :play_scale

  all_channels do
    set_empty_instrument
    volume_envelope_off
    volume 10
    envelope_duration 11
    envelope_shape ALTERNATE|CONTINUE
    enable_ay_volume_ctrl
  end

  sub :play_scale

  all_channels { set_instrument :instr_alter }

  sub :play_scale

  all_channels { disable_ay_volume_ctrl; mask_ay_volume_envelope_off }

  all_channels { set_instrument :instr2 }

  sub :play_scale

  all_channels { volume_envelope_off; chord_off; set_instrument :instr3; note_progress 1 }

  repeat(2) do
    sub :play_scale
    all_channels { mode2; note_progress tempo/4 }
  end

  all_channels { volume_envelope_off; vibrato_off; note_progress 0 }

  for_ch :a, :c do
    volume 0
  end

  ch_b do
    volume 15
    sub :play_chords
  end

  all_channels { chord_off; volume 0 }

  wait 50

  ##############
  # SUB TRACKS #
  ##############

  multitrack :play_scale do
    ch_a do
      a 0, 2; a! 0, 2; b 0, 2; c 0, 2; c! 0, 2; d 0, 2; d! 0, 2; e 0, 2; f 0, 2; f! 0, 2; g 0, 2; g! 0, 2;
    end
    ch_b do
      a 3, 2; a! 3, 2; b 3, 2; c 3, 2; c! 3, 2; d 3, 2; d! 3, 2; e 3, 2; f 3, 2; f! 3, 2; g 3, 2; g! 3, 2;
    end
    ch_c do
      a 6, 2; a! 6, 2; b 6, 2; c 6, 2; c! 6, 2; d 6, 2; d! 6, 2; e 6, 2; f 6, 2; f! 6, 2; g 6, 2; g! 6, 2;
    end
  end

  track :play_chords do
    pch :c, 3, :d!, 3, :g, 3; p 1
    pch :c, 3, :e , 3, :g, 3; p 1
    pch :c, 3, 2,
        :e, 3, 3,
        :g, 3, 4
    p 1
    pch :c, 4, :d!, 4, :g, 4; p 1
  end

  ###############
  # INSTRUMENTS #
  ###############

  instrument :instr1 do
    volume 10; start_volume_envelope :decay1
  end

  instrument :instr2 do
    sub :instr1; start_chord :chord1
  end

  instrument :instr3 do
    volume 10; start_volume_envelope :decay2; vibrato_step 10; vibrato_angle 0.25; vibrato_amplitude 1.0
  end

  instrument :instr_alter do
    mask_ay_volume_envelope :alter1
  end

  #############
  # ENVELOPES #
  #############

  envelope :decay1, [1, 0.49], [64, -1.0], :loop, [255, 0.0]
  envelope :decay2, [4, 0.5], [32, -0.75], :loop, [8, 0.5], [8, -0.5]
  envelope :decay3, [32, 1.0], [255, -1.0]
  envelope :wave1, [128, 1.0], [128, -1.0]
  envelope :wave2, [16, 1.0], :loop, [16, -0.7], [16, 0.7]
  chord :chord1, [2, 0], :loop, [1, 4], [1, 7]
  mask :alter1, [8, 0b11110000], [8, 0b11001100], :loop, [8, 0b10101010]
end

if __FILE__ == $0
  music = TestMusic.new
  puts music.to_program.new(0x8000).debug
  puts music.validate_recursion_depth!
  puts music.channel_tracks.map.with_index {|t, ch| "channel: #{ch} ticks: #{t.ticks_counter}" }
  puts "Unused items:"
  music.unused_item_names.each do |category, names|
    unless names.empty?
      puts "  #{category}:"
      puts names.map {|name| "   - :#{name}" }
    end
  end

  puts "By type:"
  music.to_module.index_items.sort_by {|item| item.type}.chunk {|item| item.type}.
  each do |type, items|
    puts " - #{type}s".to_s.ljust(15) + ": #{items.length.to_s.rjust(3)}"
  end
  music.to_player_module.save_tap 'examples/test_music'
  Z80::TAP.parse_file('examples/test_music.tap') do |hb|
      puts hb.to_s
  end
end
