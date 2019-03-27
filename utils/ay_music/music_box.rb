# track record
# alias_method :l, :label
# alias_method :i, :instrument
# alias_method :instr, :instrument
# alias_method :s, :sync
# alias_method :m1, :mode1 (reset instrument on played note)
# alias_method :m2, :mode2 (don't reset instrument on played note)
# alias_method :envdur, :envelope_duration
# alias_method :envd, :envelope_duration
# alias_method :envsh, :envelope_shape
# alias_method :envs, :envelope_shape
# alias_method :ve, :volume_envelope
# alias_method :ne, :noise_envelope
# alias_method :ce, :chord_envelope
# alias_method :me, :mask_envelope
# alias_method :mt, :mask_tone
# alias_method :mn, :mask_noise
# alias_method :vs, :vibrato_step
# alias_method :vg, :vibrato_angle
# alias_method :va, :vibrato_amplitude
# alias_method :vo, :vibrato_off
# alias_method :np, :note_progress
# alias_method :tp, :tone_progress
# alias_method :vv, :variable_volume
# alias_method :fv, :fixed_volume
# alias_method :t0, :tone_off
# alias_method :t1, :tone_on
# alias_method :n0, :noise_off
# alias_method :n1, :noise_on
# alias_method :sub, :sub_track
# alias_method :lt, :loop_to
# alias_method :n, :noise
# alias_method :v, :volume
# alias_method :w, :wait
# 0       - terminate (ret/loop/etc)
# 1-96    - play note c c! d d! e f f! g g! a a! b
# 128-159 - set noise n 
# 160-175 - set volume
# 176-255 - wait (1-80)
#  97     - set instrument + 1 byte index
#  98     - sync + 1 byte counter
#  99     - set mode 1 - play note - resets instrument
# 100     - set mode 2 - play note - changes tune not resets instrument
# 101     - set envelope duration + 2 bytes
# 102     - set envelope shape + 1 byte
# 103     - envelope volume EnvelopeControl + 1 byte index
# 104     - envelope noise EnvelopeControl + 1 byte index
# 105     - chord control + 1 byte index
# 106     - mask control (envelope) + 1 byte index
# 107     - mask control tone + 1 byte index
# 108     - mask control noise + 1 byte index
# 109     - set vibrato step + 2 bytes
# 110     - set vibrato angle + 2 bytes
# 111     - set vibrato amplitude + 1 byte (adapts to current note)
# 112     - disable vibrato
# 113     - set note progress counter + 1 byte counter (0 - ignores tone progress)
# 114     - set tone progress + 2 bytes delta + 2 bytes counter (sets note_progress to 0), may play note safely after that
# 115     - set_ignore_volume -1
# 116     - set_ignore_volume 0
# 117     - set_tone_off -1
# 118     - set_tone_off 0
# 119     - set_noise_off -1
# 120     - set_noise_off 0
# 121     - go sub, 1 byte index
# 122     - loop next, 1 byte counter, 1 byte LSB 8-bit twos complement negative offset
module MusicBox
  class Track
    attr_reader :name, :instruments
    attr_accessor :counter
    def initialize(name, &block)
      @name = name.to_sym if name
      @track = []
      @labels = {}
      @octave = 3
      @tempo = 64
      @instruments = nil
      @counter = 0
      @last_wait_index = nil
      if block_given?
        add(&block)
      end
    end

    def instruments=(instruments)
      raise ArgumentError, "not an Instrument" unless instruments.respond_to?(:index)
      @instruments = instruments
    end

    def instrument_to_index(instr)
      index = if Integer === instr
        instr
      else
        @instruments && @instruments.index(instr)
      end
      raise ArgumentError, "index: 0 - 128 or a valid instrument name required: #{instr.inspect}" unless (0..128).include?(index)
      index
    end

    def add(&block)
      instance_exec(&block)
    end

    def to_track
      @track + [0]
    end

    def to_program_data(program, name=@name)
      program.data(1, to_track).tap do |label|
        program.send(name.to_sym, label) if name
      end
    end

    def octave(number)
      @octave = number
    end
    alias_method :o, :octave

    def label(name)
      name = name.to_sym
      # raise ArgumentError, "label: #{name} already exists at: ##{@labels[name]}" if @labels.has_key?(name)
      @last_wait_index = nil
      @labels[name] = @track.length
    end
    alias_method :l, :label

    def tempo(ticks=nil)
      @tempo = ticks if ticks
      @tempo
    end

    %w[a a! b c c! d d! e f f! g g!].each_with_index do |name, index|
      define_method(name) do |octave=@octave, duration=nil, *durext|
        note = octave*12 + index
        raise ArgumentError, "invalid note: #{note}" unless (0..95).include?(note)
        @track << note + 1
        pause(duration, *durext) if duration
      end
    end
    %w[b_ a!  d_ c!  e_ d!  g_ f!  a_ g!].each_slice(2) do |a, b|
      alias_method a, b
    end

    def instrument(instrument)
      @track << 97 << instrument_to_index(instrument)
    end
    alias_method :i, :instrument
    alias_method :instr, :instrument

    def mode1; @track << 99; end
    alias_method :m1, :mode1

    def mode2; @track << 100; end
    alias_method :m2, :mode2

    def envelope_duration(duration)
      raise ArgumentError, "duration: 0 - 65535" unless (0..65535).include?(duration)
      @track << 101 << (duration & 0xff) << (duration >> 8)
    end
    alias_method :envdur, :envelope_duration
    alias_method :envd, :envelope_duration

    def envelope_shape(shape); @track << 102 << (shape & 0x0f); end
    alias_method :envsh, :envelope_shape
    alias_method :envs, :envelope_shape

    def volume_envelope(index)
      @track << 103 << instrument_to_index(index)
    end
    alias_method :ve, :volume_envelope

    def noise_envelope(index)
      @track << 104 << instrument_to_index(index)
    end
    alias_method :ne, :noise_envelope

    def chord_envelope(index)
      @track << 105 << instrument_to_index(index)
    end
    alias_method :ce, :chord_envelope

    def mask_envelope(index)
      @track << 106 << instrument_to_index(index)
    end
    alias_method :me, :mask_envelope

    def mask_tone(index)
      @track << 107 << instrument_to_index(index)
    end
    alias_method :mt, :mask_tone

    def mask_noise(index)
      @track << 108 << instrument_to_index(index)
    end
    alias_method :mn, :mask_noise

    def vibrato_step(step=1.0)
      step = (step.to_f * 256.0).round
      @track << 109 << (step & 0xff) << ((step >> 8) & 0xff)
    end
    alias_method :vs, :vibrato_step

    def vibrato_angle(angle=0.0)
      angle = (angle.to_f * 256.0).round.abs
      @track << 110 << (angle & 0xff) << (angle >> 8)
    end
    alias_method :vg, :vibrato_angle

    def vibrato_amplitude(amplitude=1.0)
      amplitude = (amplitude.to_f * 255.0).round.abs
      @track << 111 << (amplitude & 0xff)
    end
    alias_method :va, :vibrato_amplitude

    def vibrato_off; @track << 112; end
    alias_method :vo, :vibrato_off

    # 0: ignore progress, 1: no progress, 2 and more: progress counter
    def note_progress(counter)
      raise ArgumentError, "counter: 0 - 255" unless (0..255).include?(counter)
      @track << 113 << counter
    end
    alias_method :np, :note_progress

    # ad-hoc tone progress, sets note progress to 0
    # until it's finish playing notes only changes tone played after tone progress is done
    def tone_progress(delta, counter)
      raise ArgumentError, "counter must be > 0 and < 65536" unless (1..65535).include?(counter)
      delta = (delta.to_f * 256.0 * 32.0 / 12.0 / counter).round
      raise ArgumentError, "delta too big or counter too small" unless (-32768..32767).include?(delta)
      @track << 114 << (delta & 0xff) << ((delta >> 8) & 0xff) << (counter & 0xff) << (counter >> 8)
    end
    alias_method :tp, :tone_progress

    def variable_volume; @track << 115; end
    alias_method :vv, :variable_volume

    def fixed_volume; @track << 116; end
    alias_method :fv, :fixed_volume

    def tone_off; @track << 117; end
    alias_method :t0, :tone_off

    def tone_on; @track << 118; end
    alias_method :t1, :tone_on

    def noise_off; @track << 119; end
    alias_method :n0, :noise_off

    def noise_on; @track << 120; end
    alias_method :n1, :noise_on

    def sub_track(index)
      @track << 121 << instrument_to_index(index)
    end
    alias_method :sub, :sub_track

    def repeat(counter=nil)
      if counter == 1
        return yield
      end
      raise ArgumentError, "counter: 2 - 256" unless (2..256).include?(counter) or counter.nil?
      @last_wait_index = nil
      offset = @track.length
      yield
      counter = counter.nil? ? 0 : counter - 1
      @track << 122 << counter
      offset = offset - @track.length
      raise ArgumentError, "repeat block too large" if offset < -256
      @track << (offset & 0xff)
    end
    alias_method :rpt, :repeat

    def loop_to(name, counter=nil)
      return @track if counter == 1
      raise ArgumentError, "counter: 2 - 256" unless (2..256).include?(counter) or counter.nil?
      offset = @labels[name.to_sym]
      raise ArgumentError, "no such musick track label: #{name}" unless offset
      counter = counter.nil? ? 0 : counter - 1
      @track << 122 << counter
      offset = offset - @track.length
      raise ArgumentError, "loop label too far away" if offset < -256
      @track << (offset & 0xff)
    end
    alias_method :lt, :loop_to

    def noise(level); @track << 128 + (level & 0x1f); end
    alias_method :n, :noise

    def volume(level); @track << 160 + (level & 0x0f); end
    alias_method :v, :volume

    def sync(sync_to)
      sync_to = sync_to.to_i & 0xff
      @counter += (sync_to - @counter) & 0xff
      @track << 98 << sync_to
    end
    alias_method :s, :sync

    def pause(duration, *durexts)
      time = (@tempo/duration.to_f).round
      durexts.each do |durext|
        time += (@tempo/durext.to_f).round
      end
      wait time
    end
    alias_method :p, :pause

    def wait(ticks)
      raise ArgumentError, "ticks must be > 0" unless Integer === ticks && ticks > 0
      @counter += ticks
      if @last_wait_index == @track.length - 1
        ticks += @track.pop - 175
      end
      while ticks > 80
        @last_wait_index = @track.length
        @track << 255
        ticks -= 80
      end
      unless ticks == 0
        @last_wait_index = @track.length
        @track << 175 + ticks
      end
    end
    alias_method :w, :wait

  end

  class EnvelopeData
    attr_reader :args, :count

    def item_size; 2; end

    def validate_args(args)
      raise ArgumentError, "args must be even" unless args.length.even?
      res = []
      args.each_slice(2) do |cnt, val|
        raise ArgumentError, "invalid count argument: #{cnt}" unless (1..255).include?(cnt)
        step = (val.to_f * 255.0 / cnt).round
        raise ArgumentError, "value argument is too large: #{val}/#{cnt}: #{step}" unless (-128..127).include?(step)
        res << cnt << step
      end
      res
    end

    def initialize(repeat, *args)
      args.flatten!
      raise ArgumentError, "give some more args" if args.empty?
      @args = validate_args args
      @count = @args.length / item_size
      @loop_offset = case repeat
      when :all
        0
      when :last
        (count - 1) * item_size
      when (-128..-1)
        repeat += count
        raise ArgumentError, "repeat offset too small" if repeat < 0
        repeat * item_size
      when (0..127)
        raise ArgumentError, "repeat offset too big" if repeat >= count - 1
        repeat * item_size
      when Integer
        raise ArgumentError, "repeat offset too large or negative" if repeat >= count - 1 || repeat < 0
      else
        raise ArgumentError, "repeat should be an offset or :all or :last"
      end
    end

    def to_track
      [@loop_offset] + @args + [0]
    end

    def to_program_data(program, name=nil)
      program.data(1, to_track).tap do |label|
        program.send(name.to_sym, label) if name
      end
    end
  end

  class MaskData < EnvelopeData
    def validate_args(args)
      raise ArgumentError, "args must be even" unless args.length.even?
      res = []
      args.each_slice(2) do |cnt, msk|
        raise ArgumentError, "invalid count argument: #{cnt}" unless (1..255).include?(cnt)
        raise ArgumentError, "mask argument is too large" unless (0..255).include?(msk)
        res << cnt << msk
      end
      res
    end
  end

  class ChordData < EnvelopeData
    def item_size; 1; end

    def validate_args(args)
      raise ArgumentError, "args must be even" unless args.length.even?
      res = []
      args.each_slice(2) do |cnt, dlt|
        raise ArgumentError, "invalid count argument: #{cnt}" unless (1..7).include?(cnt)
        raise ArgumentError, "note delta argument is too large" unless (0..31).include?(dlt)
        res << ((cnt<<5)|dlt)
      end
      res
    end
  end

  class Instruments
    def initialize(*labels)
      @labels = labels.map do |label|
        label = label.to_sym if String === label
        name = if Symbol === label
          label.to_s
        elsif label.respond_to?(:to_name)
          label.to_name
        else
          raise ArgumentError, "not a label: #{label.inspect}"
        end
        [name, label]
      end.to_h
    end

    def self.normalize_name(name)
      if name.respond_to?(:to_name) then name.to_name else name.to_s end
    end

    def has_key?(name)
      @labels.has_key?( Instruments.normalize_name(name) )
    end

    def [](name)
      @labels[ Instruments.normalize_name(name) ]
    end

    def index(name)
      index = @labels.keys.index( Instruments.normalize_name(name) )
      index && index + 1
    end

    def to_program_data(program, name=nil)
      program.data(2, @labels.values).tap do |label|
        program.send(name.to_sym, label) if name
      end
    end
  end

  class LazyInstruments < SimpleDelegator
    def initialize(instruments)
      super
    end
  end

  module Helpers
    def self.extended(klass) # :nodoc:
        klass.instance_variable_set '@instruments'.freeze, LazyInstruments.new(Instruments.new)
    end

    def instruments(*labels)
      instruments = Instruments.new(*labels)
      @instruments.__setobj__(instruments)
      instruments.to_program_data(self)
    end

    def music_track(name, &block)
      Track.new(name).tap do |track|
        track.instruments = @instruments
        track.add(&block)
      end.to_program_data(self)
    end

    def music_envelope_data(repeat, *args)
      EnvelopeData.new(repeat, *args).to_program_data(self)
    end

    def music_mask_data(repeat, *args)
      MaskData.new(repeat, *args).to_program_data(self)
    end

    def music_chord_data(repeat, *args)
      ChordData.new(repeat, *args).to_program_data(self)
    end
  end
end
