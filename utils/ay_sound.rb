# -*- coding: BINARY -*-
require 'z80'
require 'zxlib/sys'
##
# ==AYSound.
#
# Macros to help program the AY-3-8910/8912 sound chipsets.
#
# _From_:: http://www.armory.com/~rstevew/Public/SoundSynth/Novelty/AY3-8910/start.html
#
# Registers
# The AY-3-8910/8912 contains 16 internal registers as follows: 
# Register        Function                        Range
# 
#  0              Channel A fine pitch            8-bit (0-255)
#  1              Channel A course pitch          4-bit (0-15)
#  2              Channel B fine pitch            8-bit (0-255)
#  3              Channel B course pitch          4-bit (0-15)
#  4              Channel C fine pitch            8-bit (0-255)
#  5              Channel C course pitch          4-bit (0-15)
#  6              Noise pitch                     5-bit (0-31)
#  7              Mixer                           8-bit (see below)
#  8              Channel A volume                4-bit (0-15, see below)
#  9              Channel B volume                4-bit (0-15, see below)
# 10              Channel C volume                4-bit (0-15, see below)
# 11              Envelope fine duration          8-bit (0-255)
# 12              Envelope course duration        8-bit (0-255)
# 13              Envelope shape                  4-bit (0-15)
# 14              I/O port A                      8-bit (0-255)
# 15              I/O port B                      8-bit (0-255)
# Notes: 
# 
#   The AY-3-8912 does not contain register 15.
#   The volume registers (8, 9 and 10) contain a 4-bit setting but if bit 5 is set then that channel uses the envelope defined by register 13 and ignores its volume setting.
# 
# 
# The mixer (register 7) is made up of the following bits (low = enabled): 
# Bit: 7        6        5        4        3        2        1        0
#    _         _
#    I/O       I/O   Noise    Noise    Noise     Tone     Tone     Tone
#      B        A        C        B        A        C        B        A
# 
# The AY-3-8912 ignores bit 7 of this register.
# 
# Envelopes
# The AY-3-8910/8912 contains the following preset envelopes or waveforms (set using control register 13). Note that these affect volume only and not the pitch: 
#  0      \__________     single decay then off
# 
#  4      /|_________     single attack then off
# 
#  8      \|\|\|\|\|\     repeated decay
# 
#  9      \__________     single decay then off
# 
# 10      \/\/\/\/\/\     repeated decay-attack
#           _________
# 11      \|              single decay then hold
# 
# 12      /|/|/|/|/|/     repeated attack
#          __________
# 13      /               single attack then hold
# 
# 14      /\/\/\/\/\/     repeated attack-decay
# 
# 15      /|_________     single attack then off
#
class AYSound

  ###########
  # Exports #
  ###########

  # export ay

  ###########
  # Imports #
  ###########

  # import        ZXSys, macros: true, labels: true, code: false

  ##########
  # Macros #
  ##########

  # https://faqwiki.zxnet.co.uk/wiki/AY-3-8912
  # https://www.worldofspectrum.org/faq/reference/tmxreference.htm
  CLOCK_HZ_128 = 3.5469/2 * 1_000_000 # 1.77345 MHz
  CLOCK_HZ_48 = 3.5/2 * 1_000_000 # 1.75 MHz
  CLOCK_HZ_TIMEX = 1.76475*1_000_000 # Timex TS2068 (1.76475 MHz) 3.52800/2
  CLOCK_HZ = CLOCK_HZ_128

  module EnvelopeControl
    HOLD          = 1
    HOLD_BIT      = 0
    ALTERNATE     = 2
    ALTERNATE_BIT = 1
    ATTACK        = 4
    ATTACK_BIT    = 2
    CONTINUE      = 8
    CONTINUE_BIT  = 3
  end
  include EnvelopeControl

  module Registers
    TP_FINE_A      =  0
    TP_COARSE_A    =  1
    TP_FINE_B      =  2
    TP_COARSE_B    =  3
    TP_FINE_C      =  4
    TP_COARSE_C    =  5
    NOISE_PERIOD   =  6
    MIXER          =  7
    VOLUME_A       =  8
    VOLUME_B       =  9
    VOLUME_C       = 10
    ENV_PER_FINE   = 11
    ENV_PER_COARSE = 12
    ENV_SHAPE      = 13
    IO_A           = 14
    IO_B           = 15
  end
  include Registers

  ##
  # ==AYSound macros.
  module Macros
    ##
    # Returns a table of equal tempered 12 half-tone scale based on a given frequency.
    # _See_:: http://pages.mtu.edu/~suits/NoteFreqCalcs.html
    # n0: -9 for C
    def equal_tempered_scale_notes_hz(frequency: 440, n0:0, steps:12)
      (0...steps).map {|n| frequency.to_f * 2.0**((n + n0).to_f/steps.to_f) }
    end

    ##
    # Returns a tone period table for the AY-3-891x chip.
    #
    # Options:
    # * +min_octave:: Minimal octave number.
    # * +max_octave:: Maximal octave number.
    # * +notes_hz+:: A table of tone frequencies (in Hz) in the middle (4) octave.
    # * +clock_hz+:: AY-3-891x clock frequency in Hz.
    # 
    # Middle (frequency table base) octave is 4.
    def ay_tone_periods(min_octave:0, max_octave:7, notes_hz:self.equal_tempered_scale_notes_hz, clock_hz: AYSound::CLOCK_HZ)
      (min_octave..max_octave).map do |octave|
        notes_hz.map do |hz|
          hz = (hz * 2.0**(octave-4))
          puts "#{'%.2f' % hz} Hz, octave: #{octave}"
          tp = (clock_hz / (16.0 * hz)).round
          raise ArgumentError, "tone period out of range: #{tp} (#{hz} Hz)" unless (1..4095).include?(tp)
          tp
        end
      end.flatten
    end

    def ay_io_load_const_reg_bc(io=self.io128)
      port_bit_diff = io.ay_out.to_i ^ io.ay_sel.to_i
      if (port_bit_diff & 0xFF00).zero?
                      ld   b, io.ay_out>>8
      elsif (port_bit_diff & 0x00FF).zero?
                      ld   c, io.ay_out
      end
    end

    def ay_io_swap_sel2out_bc(io=self.io128)
      port_bit_diff = io.ay_out.to_i ^ io.ay_sel.to_i
      if (port_bit_diff & 0xFF00).zero?
                      ld   c, io.ay_out
      elsif (port_bit_diff & 0x00FF).zero?
                      ld   b, io.ay_out>>8
      else
                      ld   bc, io.ay_out
      end
    end

    def ay_io_swap_out2sel_bc(io=self.io128)
      port_bit_diff = io.ay_sel.to_i ^ io.ay_out.to_i
      if (port_bit_diff & 0xFF00).zero?
                      ld   c, io.ay_sel
      elsif (port_bit_diff & 0x00FF).zero?
                      ld   b, io.ay_sel>>8
      else
                      ld   bc, io.ay_sel
      end
    end

    def ay_get_register_value(regn=a, regv=e, bc_const_loaded:false, io:self.io128)
      raise ArgumentError unless register?(regv)
      isolate do
        if Integer === regn || [a,b,c].include?(regn)
                      ld   a, regn unless [0, a].include?(regn)
        end
        if bc_const_loaded
                      ay_io_swap_out2sel_bc(io)
        else
                      ld   bc, io.ay_sel
        end
        if Integer === regn || [a,b,c].include?(regn)
          if regn == 0
                      out (c), 0
          else
                      out (c), a
          end
        else
                      out (c), regn
        end
                      inp regv, (c)
      end
    end

    def ay_set_register_value(regn=a, regv=e, bc_const_loaded:false, io:self.io128, &output_value)
      raise ArgumentError if [b,c].include?(regv) or (regv == a and regn != 0)
      isolate do |eoc|
        if Integer === regn || [a,b,c].include?(regn)
                      ld   a, regn unless [0, a].include?(regn)
        end
        if bc_const_loaded
                      ay_io_swap_out2sel_bc(io)
        else
                      ld   bc, io.ay_sel
        end
        if Integer === regn || [a,b,c].include?(regn)
          if regn == 0
                      out (c), 0
          else
                      out (c), a
          end
        else
                      out (c), regn
        end
                      ay_io_swap_sel2out_bc(io)
        if Integer === regv
          if regv.zero?
                      out (c), 0
          else
                      ld   a, regv
                      out (c), a
          end
        elsif block_given?
                      output_value.call(eoc)
        else
                      out (c), regv
        end
      end
    end

    def ay_init(io:self.io128)
      isolate do
                      ld   e, 3
                      ld   a, e
                      add  AYSound::VOLUME_A
                      ay_io_load_const_reg_bc(io)
        vol_res_loop  dec  a
                      ay_set_register_value(a, 0, bc_const_loaded:true, io:io)
                      dec  e
                      jr   NZ, vol_res_loop
                      ay_get_set_mixer(a, bc_const_loaded:true, io:io) do |_|
                        anda 0b11111000
                        ora  0b00111000
                      end
      end
    end

    def ay_get_set_mixer(vinp=a, vout=vinp, bc_const_loaded:false, io:self.io128)
      isolate do |eoc|
                      ay_get_register_value(AYSound::MIXER, vinp, bc_const_loaded:bc_const_loaded, io:io)
                      yield eoc
                      ay_io_swap_sel2out_bc(io)
                      out (c), vout
      end
    end

    def ay_set_tone_period(ch=a, tph:d, tpl:e, bc_const_loaded:false, io:self.io128)
      isolate do
        if Integer === ch
          if ch.zero?
                      xor  a
          else
                      ld   a, ch << 1
          end
        else
                      ld   a, ch unless ch == a
                      add  a
        end
                      ay_set_register_value(a, tpl, bc_const_loaded:bc_const_loaded, io:io)
                      inc  a
                      ay_set_register_value(a, tph, bc_const_loaded:true, io:io)
      end
    end


    ##
    # Set channel volume
    #
    # *+ch+:: Channel number 0-2
    # *+vol+:: Volume level or an 8-bit register except +b+, +c+ or +a+.
    def ay_set_volume(ch=a, vol=e, vol_8bit:false, bc_const_loaded:false, io:self.io128)
      raise ArgumentError if [a,b,c].include?(vol)
      isolate do
        if Integer === ch
                      ld   a, ch + AYSound::VOLUME_A
        else
                      ld   a, ch unless ch == a
                      add  AYSound::VOLUME_A
        end
        ay_set_register_value(a, vol, bc_const_loaded:bc_const_loaded, io:io) do |_|
          if Integer === vol
            if vol.zero?
                      out  (c), 0
            else
              if vol_8bit
                      ld   a, (vol>>4) & 0x0F
              else
                      ld   a, vol & 0x0F
              end
                      out  (c), a
            end
          else
                      ld   a, vol
            if vol_8bit
                      4.times { rrca }
            end
                      anda 0x0F
                      out  (c), a
          end
        end
      end
    end

    def ay_set_envelope_duration(dh=d, dl=e, bc_const_loaded:false, io:self.io128)
      isolate do
                      ld   a, AYSound::ENV_PER_FINE
                      ay_set_register_value(a, dl, bc_const_loaded:bc_const_loaded, io:io)
                      inc  a
                      ay_set_register_value(a, dh, bc_const_loaded:true, io:io)
      end
    end

    def ay_get_set_env_shape(sinp=a, sout=sinp, bc_const_loaded:false, io:self.io128)
      isolate do |eoc|
                      ay_get_register_value(AYSound::ENV_SHAPE, sinp, bc_const_loaded:bc_const_loaded, io:io)
                      yield eoc
                      ay_io_swap_sel2out_bc(io)
                      out (c), sout
      end
    end

    def ay_set_noise_pitch(pitch=e, pitch_8bit:false, bc_const_loaded:false, io:self.io128)
      isolate do
        ay_set_register_value(AYSound::NOISE_PERIOD, pitch, bc_const_loaded:bc_const_loaded, io:io) do |_|
          if Integer === pitch
            if pitch_8bit
                      ld   a, (pitch>>3) & 0x1F
            else
                      ld   a, pitch & 0x1F
            end
                      out  (c), a
          elsif pitch_8bit
                      ld   a, pitch
                      3.times { rrca }
          else
                      out (c), pitch
          end
        end
      end
    end
  end # Macros
  include Z80
end

if __FILE__ == $0
    require 'zxlib/basic'
    # :stopdoc:

    class AYTest
      include Z80
      include Z80::TAP
      include AYSound::Registers
      include AYSound::EnvelopeControl

      label_import    ZXSys
      macro_import    AYSound
      macro_import    Z80MathInt

      NOTES = ay_tone_periods

      ns :demo, use: :io128 do
                      call mute                      
                      # ay_set_volume(0, 15, bc_const_loaded:true)
                      ay_set_register_value(VOLUME_A, 0x10, bc_const_loaded:true)
                      ld   de, 10
                      ay_set_envelope_duration(d, e, bc_const_loaded:true)
                      ay_set_register_value(ENV_SHAPE, ALTERNATE|ATTACK|CONTINUE, bc_const_loaded:true)
                      ld   de,  NOTES.length
        mloop         push de
                      ld   a, d
                      call get_note
                      ay_set_tone_period(0, tph:d, tpl:e, bc_const_loaded:true)
                      ld   a, 5
                      call wait_periods
                      pop  de
                      inc  d
                      dec  e
                      jr   NZ, mloop
                      call mute
                      ret
      end

      ns :mute, use: :io128 do
                      ay_init
                      ret
      end

      ns :wait_periods do
        wloop         halt
                      dec  a
                      jr   NZ, wloop
                      ret
      end

      # A: octave*12+note
      ns :get_note do
                      ld   hl, notes
                      add  a
                      adda_to h,l
                      ld   e, [hl]
                      inc  hl
                      ld   d, [hl]
                      ret
      end

      notes           words NOTES
    end

    aytest = AYTest.new 0x8000
    puts aytest.debug

    program = Basic.parse_source <<-EOC
      10 RANDOMIZE USR #{aytest[:demo]}
    9998 STOP: GO TO 10
    9999 CLEAR #{aytest.org-1}: LOAD ""CODE: RUN
    EOC
    puts program.to_source escape_keywords: true
    program.save_tap "testay", line: 9999
    aytest.save_tap "testay", append: true
    puts "TAP: testay.tap:"
    Z80::TAP.parse_file('testay.tap') do |hb|
        puts hb.to_s
    end
end