# -*- coding: BINARY -*-
require 'z80'
##
# ==ZXLib::AYSound.
#
# Macros to help program the AY-3-8910/8912 sound chipsets.
#
# _Sources_::
# * http://www.armory.com/~rstevew/Public/SoundSynth/Novelty/AY3-8910/start.html
# * https://faqwiki.zxnet.co.uk/wiki/AY-3-8912
#
# ===Registers
#
# The AY-3-8910/8912 contains 16 internal registers as follows: 
#   Register        Function                        Range
#   
#    0              Channel A fine pitch            8-bit (0-255)
#    1              Channel A coarse pitch          4-bit (0-15)
#    2              Channel B fine pitch            8-bit (0-255)
#    3              Channel B coarse pitch          4-bit (0-15)
#    4              Channel C fine pitch            8-bit (0-255)
#    5              Channel C coarse pitch          4-bit (0-15)
#    6              Noise pitch                     5-bit (0-31)
#    7              Mixer                           8-bit (see below)
#    8              Channel A volume                4-bit (0-15, see below)
#    9              Channel B volume                4-bit (0-15, see below)
#   10              Channel C volume                4-bit (0-15, see below)
#   11              Envelope fine duration          8-bit (0-255)
#   12              Envelope coarse duration        8-bit (0-255)
#   13              Envelope shape                  4-bit (0-15)
#   14              I/O port A                      8-bit (0-255)
#   15              I/O port B                      8-bit (0-255)
#
# Notes:: The AY-3-8912 does not contain register 15.
# The volume registers (8, 9 and 10) contain a 4-bit setting but if bit 5 is set then that channel uses the envelope defined by register 13 and ignores its volume setting.
#   
# 
# The mixer (register 7) is made up of the following bits (low = enabled): 
#   Bit: 7        6        5        4        3        2        1        0
#      _         _
#      I/O       I/O   Noise    Noise    Noise     Tone     Tone     Tone
#        B        A        C        B        A        C        B        A
# 
# The AY-3-8912 ignores bit 7 of this register.
# 
# ===Envelope shapes
#
# The AY-3-8910/8912 contains the following preset envelopes or waveforms (set using control register 13). Note that these affect volume only and not the pitch: 
#   0      \__________     single decay then off
#  
#   4      /|_________     single attack then off
#  
#   8      \|\|\|\|\|\     repeated decay
#  
#   9      \__________     single decay then off
#  
#  10      \/\/\/\/\/\     repeated decay-attack
#            _________
#  11      \|              single decay then hold
#  
#  12      /|/|/|/|/|/     repeated attack
#           __________
#  13      /               single attack then hold
#  
#  14      /\/\/\/\/\/     repeated attack-decay
#  
#  15      /|_________     single attack then off
#
module ZXLib
  class AYSound
    ## ZX Spectrum 128k AY-891x frequency
    CLOCK_HZ_128 = 3.5469/2 * 1_000_000 # 1.77345 MHz

    ## ZX Spectrum 48k AY-891x frequency
    CLOCK_HZ_48 = 3.5/2 * 1_000_000 # 1.75 MHz

    ##
    # Timex 2068 AY-891x frequency
    # https://www.worldofspectrum.org/faq/reference/tmxreference.htm
    CLOCK_HZ_TIMEX = 1.76475*1_000_000 # Timex TS2068 (1.76475 MHz) 3.52800/2

    ## Default AY-891x frequency
    CLOCK_HZ = CLOCK_HZ_128 unless const_defined?(:CLOCK_HZ)

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

    ## An array of musical notes as strings.
    NOTE_SYMBOLS = %w[A A# B C C# D D# E F F# G G#]

    ##
    # ==ZXLib::AYSound macros.
    #
    #    require 'zxlib/ay_sound'
    #    require 'zxlib/sys'
    #
    #    class Program
    #      include Z80
    #      label_import    ZXLib::Sys
    #      macro_import    ZXLib::AYSound
    #      ...
    #    end
    module Macros
      ##
      # Returns an array of equal tempered scale frequencies from a given base frequency.
      # _See_:: http://pages.mtu.edu/~suits/NoteFreqCalcs.html
      #
      # Options:
      # * +hz+:: base frequency in Hz.
      # * +n0+:: an index from the base frequency to the first note in the table: 0 is for "A", -9 for "C".
      # * +steps+:: how many half tones, 12 is the default.
      def equal_tempered_scale_notes_hz(hz:440, n0:0, steps:12)
        (0...steps).map {|n| hz.to_f * 2.0**((n + n0).to_f/steps.to_f) }
      end
      ##
      # Converts a frequency given in hertzs to AY-891x tone period value.
      #
      # Options:
      # * +clock_hz+:: AY-3-891x clock frequency in Hz.
      def ay_hz2tp(hz, clock_hz:AYSound::CLOCK_HZ)
        (clock_hz / (16.0 * hz)).round
      end
      ##
      # Returns a tone period array for the AY-3-891x chip.
      #
      # Options:
      # * +min_octave+:: Minimal octave number, 0-based.
      # * +max_octave+:: Maximal octave number, 0-based.
      # * +notes_hz+:: An array of tone frequencies (in Hz) in the middle (4) octave.
      # * +clock_hz+:: AY-3-891x clock frequency in Hz.
      # 
      # Middle (frequency table base) octave is 4.
      def ay_tone_periods(min_octave:0, max_octave:7, notes_hz:self.equal_tempered_scale_notes_hz, clock_hz:AYSound::CLOCK_HZ)
        idx = 0
        (min_octave..max_octave).map do |octave|
          notes_hz.map.with_index do |hz, n|
            hz = (hz * 2.0**(octave-4))
            tp = ay_hz2tp(hz, clock_hz:clock_hz)
            # puts "#{idx.to_s.rjust(3,' ')}: #{('%.2f' % hz).rjust(7)} Hz, tp: #{tp.to_s.rjust(4)}, #{octave} #{AYSound::NOTE_SYMBOLS[n]}"
            idx += 1
            raise ArgumentError, "tone period out of range: #{tp} (#{hz} Hz)" unless (1..4095).include?(tp)
            tp
          end
        end.flatten
      end
      ##
      # Creates a routine for expanding the note to tone period table to a higher number of octaves.
      #
      # * +notes+:: An address of the lowest octave of the notes to expand.
      # * +octaves+:: How many octaves should be expanded to, must be between 2 and 8.
      # * +half_tones+:: How many half-tones in one actave.
      # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
      #               +sp+ will point to the beginning of the last octave of the table.
      # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
      #                    only if you have already disabled the interrupts.
      # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
      #                   if you need to perform more uninterrupted actions.
      #
      # Example:
      #
      #           ay_extend_notes(notes)
      #           ...
      #
      #    notes  dw ay_tone_periods(min_octave:0, max_octave:0)
      #           words 7*12
      def ay_extend_notes(notes=hl, octaves:8, half_tones: 12, save_sp:true, disable_intr:true, enable_intr:true)
        raise ArgumentError, "octaves out of range: #{octaves} (2-8)" unless (2..8).include?(octaves)
        isolate do
                        ld   [restore_sp + 1], sp if save_sp
                        ld   b, (octaves-1)*half_tones
                        ld   hl, notes unless notes==hl
                        di if disable_intr
                        ld   sp, hl
                        ld   de, half_tones*2
                        add  hl, de
          eloop         pop  de
                        inc  de
                        srl  d
                        rr   e
                        ld   [hl], e
                        inc  hl
                        ld   [hl], d
                        inc  hl
                        djnz eloop
          restore_sp    ld   sp, 0 if save_sp
                        ei if enable_intr
        end
      end
      ##
      # Creates a routine that loads a constant 8-bit part of the AY-891x I/O addresses into +b+ or +c+
      # register.
      #
      # Run this routine before running one of:
      #
      # * ay_io_swap2out_bc
      # * ay_io_swap2inp_bc
      # * ay_io_swap2sel_bc
      #
      # to get a full AY-891x I/O address of the specific chip function into +bc+ register pair.
      #
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
      def ay_io_load_const_reg_bc(io=self.io128)
        ay_reg_combined = ((io.ay_out ^ io.ay_sel) | (io.ay_inp ^ io.ay_sel))
        select(ay_reg_combined & 0xFF00, &:zero?).then do |eoc|
              ld   b, io.ay_sel >> 8
        end.else_select(ay_reg_combined & 0x00FF, &:zero?).then do |eoc|
              ld   c, io.ay_sel & 0x00FF
        end.else do
          raise ArgumentError, "ay_out, ay_inp and ay_sel should have different only 8-bit msb or lsb"
        end
      end
      ##
      # Creates a routine that loads a specific 8-bit part of the AY-891x +output+ addresses into +b+ or +c+
      # register.
      #
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
      #
      # Example:
      #          ay_io_load_const_reg_bc
      #          ...
      #          ay_io_swap2sel_bc
      #          ld   a, ZXLib::AYSound::VOLUME_A
      #          out  (c), a        # select VOLUME_A register
      #          ay_io_swap2out_bc
      #          out  (c), e        # set a value of the selected register
      def ay_io_swap2out_bc(io=self.io128)
        ay_reg_combined = ((io.ay_out ^ io.ay_sel) | (io.ay_inp ^ io.ay_sel))
        select(ay_reg_combined & 0xFF00, &:zero?).then do |eoc|
              ld   c, io.ay_out & 0x00FF
        end.else_select(ay_reg_combined & 0x00FF, &:zero?).then do |eoc|
              ld   b, io.ay_out >> 8
        end.else do
          raise ArgumentError, "ay_out, ay_inp and ay_sel should have different only 8-bit msb or lsb"
        end
      end
      ##
      # Creates a routine that loads a specific 8-bit part of the AY-891x +input+ addresses into +b+ or +c+
      # register.
      #
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
      #
      # Example:
      #          ay_io_load_const_reg_bc
      #          ...
      #          ay_io_swap2sel_bc
      #          ld   a, ZXLib::AYSound::VOLUME_A
      #          out  (c), a        # select VOLUME_A register
      #          ay_io_swap2inp_bc
      #          inp  e, (c)        # get a value from the selected register
      def ay_io_swap2inp_bc(io=self.io128)
        ay_reg_combined = ((io.ay_out ^ io.ay_sel) | (io.ay_inp ^ io.ay_sel))
        select(ay_reg_combined & 0xFF00, &:zero?).then do |eoc|
              ld   c, io.ay_inp & 0x00FF
        end.else_select(ay_reg_combined & 0x00FF, &:zero?).then do |eoc|
              ld   b, io.ay_inp >> 8
        end.else do
          raise ArgumentError, "ay_out, ay_inp and ay_sel should have different only 8-bit msb or lsb"
        end
      end
      ##
      # Creates a routine that loads a specific 8-bit part of the AY-891x +select+ addresses into +b+ or +c+
      # register.
      #
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
      #
      # Example:
      #          ay_io_load_const_reg_bc
      #          ...
      #          ay_io_swap2sel_bc
      #          ld   a, ZXLib::AYSound::MIXER
      #          out  (c), a        # select MIXER register
      #          ay_io_swap2inp_bc
      #          inp  a, (c)        # get a value from the selected register
      #          anda 0b11000000
      #          ora  0b00111000    # apply mask
      #          ay_io_swap2out_bc
      #          out  (c), a        # set a value of the selected register
      def ay_io_swap2sel_bc(io=self.io128)
        ay_reg_combined = ((io.ay_out ^ io.ay_sel) | (io.ay_inp ^ io.ay_sel))
        select(ay_reg_combined & 0xFF00, &:zero?).then do |eoc|
              ld   c, io.ay_sel & 0x00FF
        end.else_select(ay_reg_combined & 0x00FF, &:zero?).then do |eoc|
              ld   b, io.ay_sel >> 8
        end.else do
          raise ArgumentError, "ay_out, ay_inp and ay_sel should have different only 8-bit msb or lsb"
        end
      end
      ##
      # Creates a routine that reads a specific AY-891x register's value.
      #
      # * +regn+:: A AY-891x register index as an integer or a 8-bit CPU register.
      # * +regv+:: An 8-bit CPU register to receive a value from a AY-891x register.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
      def ay_get_register_value(regn=a, regv=e, bc_const_loaded:false, io:self.io128)
        raise ArgumentError unless register?(regv)
        isolate do
          if Integer === regn || [a,b,c].include?(regn)
                        ld   a, regn unless [0, a].include?(regn)
          end
          if bc_const_loaded
                        ay_io_swap2sel_bc(io)
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
                        ay_io_swap2inp_bc(io)
                        inp regv, (c)
        end
      end
      ##
      # Creates a routine that writes a value to a specific AY-891x register.
      #
      # If the block is given, the code it creates will be evaluated after the AY-891x register
      # has been selected and +bc+ registers has been loaded with an "output" AY-891x I/O address.
      # The +regv+ value will not be output in this instance and the +regv+ argument will be ignored.
      # Instead the code created by the block should perform writing out the value with <tt>out (c), regv</tt>.
      #
      # * +regn+:: A AY-891x register index as an integer or a 8-bit CPU register.
      # * +regv+:: A value to write to a AY-891x register as an integer or an 8-bit CPU register
      #            except +b+, +c+ or +a+.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
      def ay_set_register_value(regn=a, regv=e, bc_const_loaded:false, io:self.io128)
        raise ArgumentError if [b,c].include?(regv) or (regv == a and regn != 0)
        isolate do |eoc|
          if Integer === regn || [a,b,c].include?(regn)
                        ld   a, regn unless [0, a].include?(regn)
          end
          if bc_const_loaded
                        ay_io_swap2sel_bc(io)
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
                        ay_io_swap2out_bc(io)
          if block_given?
                        yield eoc
          elsif Integer === regv
            if regv.zero?
                        out (c), 0
            else
                        ld   a, regv
                        out (c), a
            end
          else
                        out (c), regv
          end
        end
      end
      ##
      # Creates a routine that sets volume of all AY-891x sound channels to 0, disables noise on all channels
      # and enables tone output on all channels.
      #
      # This effectively mutes all sound.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
      def ay_init(bc_const_loaded:false, io:self.io128)
        isolate do
                        ld   a, AYSound::VOLUME_C
                        ay_io_load_const_reg_bc(io) unless bc_const_loaded
          vol_res_loop  ay_set_register_value(a, 0, bc_const_loaded:true, io:io)
                        dec  a
                        cp   AYSound::MIXER
                        jr   NZ, vol_res_loop
                        ay_get_register_value(a, a, bc_const_loaded:true, io:io)
                        anda 0b11000000
                        ora  0b00111000
                        ay_io_swap2out_bc(io)
                        out  (c), a
        end
      end
      ##
      # Creates a routine that gets the AY-891x mixer's value applies a block of code and sets the mixer value back.
      # The block of code should not modify +bc+ register pair.
      #
      # * +vinp+:: An 8-bit CPU register to receive the mixer's value.
      # * +vout+:: An 8-bit CPU register that will contain a value which will be written back to the mixer.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
      #
      # Example:
      #          ay_get_set_mixer(a) do |eoc|
      #            bit  0, a
      #            # jumping to the +eoc+ label will cancel writing mixer's value back.
      #            jr   NZ, eoc
      #            ora  0b00000001
      #          end
      def ay_get_set_mixer(vinp=a, vout=vinp, bc_const_loaded:false, io:self.io128)
        isolate do |eoc|
                        ay_get_register_value(AYSound::MIXER, vinp, bc_const_loaded:bc_const_loaded, io:io)
                        yield eoc
                        ay_io_swap2out_bc(io)
                        out (c), vout
        end
      end
      ##
      # Creates a routine that sets a AY-891x channel's tone period.
      #
      # * +ch+:: A channel number 0..2 as an integer, label, or an 8-bit CPU register.
      # * +tph+:: The most significant 4 bits of the 12-bit tone period value as an integer or an 8-bit CPU register
      #           except +b+, +c+ or +a+. This value is known as the coarse tone period value.
      # * +tpl+:: The least significant 8 bits of the 12-bit tone period value as an integer or an 8-bit CPU register
      #           except +b+, +c+ or +a+. This value is known as the fine tone period value.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
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
      # Creates a routine that sets a AY-891x channel's volume level.
      #
      # * +ch+:: A channel number 0..2 as an integer, label, or an 8-bit CPU register.
      # * +vol+:: A volume level or an 8-bit register except +b+, +c+ or +a+.
      #
      # Options:
      # * +vol_8bit+:: True if the volume is in the range: 0..255. False if the volume is in the range: 0..15.
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
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
              end
            else
                        ld   a, vol
              if vol_8bit
                        4.times { rrca }
              end
                        anda 0x0F
            end
                        out  (c), a
          end
        end
      end
      ##
      # Creates a routine that sets a AY-891x noise pitch.
      #
      # * +pitch+:: A pitch level or an 8-bit register except +b+, +c+ or +a+.
      #
      # Options:
      # * +pitch_8bit+:: True if the pitch is in the range: 0..255. False if the pitch is in the range: 0..31.
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
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
                        out  (c), a
            else
                        out  (c), pitch
            end
          end
        end
      end
      ##
      # Creates a routine that sets a AY-891x envelope duration.
      #
      # * +dh+:: The most significant 8 bits of the 16-bit envelope duration value as an integer or an 8-bit CPU register
      #          except +b+, +c+ or +a+. This value is known as the envelope coarse duration value.
      # * +dl+:: The least significant 8 bits of the 16-bit envelope duration value as an integer or an 8-bit CPU register
      #          except +b+, +c+ or +a+. This value is known as the envelope fine duration value.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
      def ay_set_envelope_duration(dh=d, dl=e, bc_const_loaded:false, io:self.io128)
        isolate do
                        ld   a, AYSound::ENV_PER_FINE
                        ay_set_register_value(a, dl, bc_const_loaded:bc_const_loaded, io:io)
                        inc  a
                        ay_set_register_value(a, dh, bc_const_loaded:true, io:io)
        end
      end
      ##
      # Creates a routine that gets the AY-891x envelope shape's value applies a block of code and sets the value back.
      # The block of code should not modify +bc+ register pair.
      #
      # * +sinp+:: An 8-bit CPU register to receive the shape's value.
      # * +sout+:: An 8-bit CPU register that will contain a value which will be written back to the register.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sublabels addressing the AY-891x I/O bus.
      def ay_get_set_env_shape(sinp=a, sout=sinp, bc_const_loaded:false, io:self.io128)
        isolate do |eoc|
                        ay_get_register_value(AYSound::ENV_SHAPE, sinp, bc_const_loaded:bc_const_loaded, io:io)
                        yield eoc
                        ay_io_swap2out_bc(io)
                        out (c), sout
        end
      end
    end # Macros

    include Z80
  end
end

if __FILE__ == $0
    require 'zxlib/basic'
    require 'zxlib/sys'
    # :stopdoc:

    class AYTest
      include Z80
      include Z80::TAP
      include ZXLib::AYSound::Registers
      include ZXLib::AYSound::EnvelopeControl

      label_import    ZXLib::Sys
      macro_import    ZXLib::AYSound
      macro_import    MathInt

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

    include ZXLib

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