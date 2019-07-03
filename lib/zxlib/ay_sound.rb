# -*- coding: BINARY -*-
require 'z80'

module ZXLib
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
  class AYSound
    ## ZX Spectrum 128k AY-3-891x frequency
    CLOCK_HZ_128 = 3.5469/2 * 1_000_000 # 1.77345 MHz

    ## ZX Spectrum 48k AY-3-891x frequency
    CLOCK_HZ_48 = 3.5/2 * 1_000_000 # 1.75 MHz

    ##
    # Timex 2068 AY-3-891x frequency
    # https://www.worldofspectrum.org/faq/reference/tmxreference.htm
    CLOCK_HZ_TIMEX = 1.76475*1_000_000 # Timex TS2068 (1.76475 MHz) 3.52800/2

    ## Default AY-3-891x frequency
    CLOCK_HZ = CLOCK_HZ_128 unless const_defined?(:CLOCK_HZ)

    ## Bit masks and bit numbers for the AY-3-891x envelope shape control register +Registers::ENV_SHAPE+.
    module EnvelopeControl
      HOLD             = 0b00000001
      HOLD_BIT         = 0
      ALTERNATE        = 0b00000010
      ALTERNATE_BIT    = 1
      ATTACK           = 0b00000100
      ATTACK_BIT       = 2
      CONTINUE         = 0b00001000
      CONTINUE_BIT     = 3
    end
    include EnvelopeControl

    ## Bit masks and bit numbers for the AY-3-891x volume registers: +VOLUME_A+, +VOLUME_B+, +VOLUME_C+.
    module VolumeControl
      VOLUME_MASK          = 0b00001111
      ENVELOPE_CONTROL     = 0b00010000
      ENVELOPE_CONTROL_BIT = 4
    end
    include VolumeControl

    ## Bit masks and bit numbers for the AY-3-891x mixer register +Registers::MIXER+.
    module Mixer
      TONE_CONTROL_MASK   = 0b00000111
      TONE_CONTROL_A      = 0b00000001
      TONE_CONTROL_A_BIT  = 0
      TONE_CONTROL_B      = 0b00000010
      TONE_CONTROL_B_BIT  = 1
      TONE_CONTROL_C      = 0b00000100
      TONE_CONTROL_C_BIT  = 2
      NOISE_CONTROL_MASK  = 0b00111000
      NOISE_CONTROL_A     = 0b00001000
      NOISE_CONTROL_A_BIT = 3
      NOISE_CONTROL_B     = 0b00010000
      NOISE_CONTROL_B_BIT = 4
      NOISE_CONTROL_C     = 0b00100000
      NOISE_CONTROL_C_BIT = 5
      IO_CONTROL_MASK     = 0b11000000
      IO_CONTROL_A        = 0b01000000
      IO_CONTROL_A_BIT    = 6
      IO_CONTROL_B        = 0b10000000
      IO_CONTROL_B_BIT    = 7
    end
    include Mixer

    ## Constants with the names of the AY-3-8910 registers.
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
    # The AYSound Macros provide functions to create note tables and some basic routines to interact with
    # the AY-3-891x registers.
    #
    # To interact with the AY-3-891x registers you may either:
    # 1. Import labels from the ZXLib::Sys and optionally override the +io_ay+ option of the appropriate
    #    routines with either the +fuller_io+ for the Fuller Box or +ioT2k+ for the Timex 2068 I/O ports.
    #    The default for +io_ay+ is +io128+ (ZX Spectrum 128k).
    # 2. Create a namespace label +io_ay+ with the corresponding sub-labels:
    #
    #   ns :io_ay do
    #     ay_sel  addr 0xFFFD # out: select a register 0-15.
    #     ay_inp  addr 0xFFFD # in: read the value of the selected register
    #     ay_out  addr 0xBFFD # out: write data to the selected register
    #   end
    #
    # To override a default +io_ay+ for the AYSound Macros create a class method which returns the appropriate namespace label.
    #
    #   def self.io_ay
    #     case $select_io_ay
    #     when :fuller then fuller_io
    #     when :timex then ioT2k
    #     when :k128 then io128
    #     else
    #       # this will return io_ay namespace label defined previously, e.g. by ZXLib::Sys
    #       define_label :io_ay
    #     end
    #   end
    #
    # Example:
    #
    #   require 'zxlib/ay_sound'
    #   require 'zxlib/sys'
    #   require 'zxlib/basic'
    #   
    #   class PlayScales
    #     include Z80
    #     include Z80::TAP
    #     include ZXLib
    #   
    #     label_import    Sys
    #     macro_import    AYSound
    #     macro_import    MathInt
    #   
    #     NOTES = ay_tone_periods
    #   
    #     start           call mute_sound
    #                     ay_set_volume 0, 15, bc_const_loaded:true
    #                     xor  a
    #     play_loop       push af
    #                     call get_note_de
    #                     ay_set_tone_period(0, tph:d, tpl:e, bc_const_loaded:true)
    #                     ld   a, 10
    #                     call wait_periods
    #                     pop  af
    #                     inc  a
    #                     cp   NOTES.length
    #                     jr   C, play_loop
    #                     call mute_sound
    #                     ret
    #   
    #     mute_sound      ay_init
    #                     ret
    #   
    #     ns :wait_periods do
    #       wloop         halt
    #                     dec  a
    #                     jr   NZ, wloop
    #                     ret
    #     end
    #   
    #     get_note_de     ld   hl, notes
    #                     add  a
    #                     adda_to h,l
    #                     ld   e, [hl]
    #                     inc  hl
    #                     ld   d, [hl]
    #                     ret
    #   
    #     notes           words NOTES
    #   end
    #   
    #   playscales = PlayScales.new 0xC000
    #   puts playscales.debug
    #   program = ZXLib::Basic.parse_source <<-EOC
    #     10 RANDOMIZE USR #{playscales[:start]}
    #   9998 STOP: GO TO 10
    #   9999 CLEAR #{playscales.org-1}: LOAD ""CODE: RUN
    #   EOC
    #   program.save_tap "playscales", line: 9999
    #   playscales.save_tap "playscales", append: true
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
      # Converts a frequency given in Hz to AY-3-891x tone period value.
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
      # * +min_octave+:: A minimal octave number, 0-based.
      # * +max_octave+:: A maximal octave number, 0-based.
      # * +notes_hz+:: An array of tone frequencies (in Hz) in the 5th octave (0-based: 4).
      #                By default equal_tempered_scale_notes_hz is being used to generate frequencies.
      # * +clock_hz+:: The AY-3-891x clock frequency in Hz.
      def ay_tone_periods(min_octave:0, max_octave:7, notes_hz:self.equal_tempered_scale_notes_hz, clock_hz:AYSound::CLOCK_HZ)
        (min_octave..max_octave).map do |octave|
          notes_hz.map do |hz|
            hz = (hz * 2.0**(octave-4))
            tp = ay_hz2tp(hz, clock_hz:clock_hz)
            raise ArgumentError, "tone period out of range: #{tp} (#{hz} Hz)" unless (1..4095).include?(tp)
            tp
          end
        end.flatten
      end
      ##
      # Creates a routine for expanding the note to AY-3-891x tone period table to a higher number of octaves.
      #
      # Elements of the table are 2-byte words holding the AY-3-891x tone period values for each consecutive
      # note.
      #
      # * +notes+:: An address of the beginning of the lowest octave of the notes to expand.
      #             The memory should be pre-filled with tone period values for the 1st octave.
      #             See: Macros.ay_tone_periods.
      # Options:
      # * +octaves+:: A number of octaves, between 2 and 8, the notes should be expanded to.
      # * +half_tones+:: A number of half-tones per one octave.
      #
      # T-states: 41 + 98*(octaves-1)*half_tones (default options: 8273)
      #
      # Example:
      #
      #               ay_expand_notes(notes)
      #               ...
      #
      #    notes      dw ay_tone_periods(min_octave:0, max_octave:0)
      #    notes_end  union notes[8*12], 2
      #
      # Modifies: +af+, +bc+, +de+, +hl+.
      def ay_expand_notes(notes=hl, octaves:8, half_tones:12)
        raise ArgumentError, "octaves out of range: #{octaves} (2-8)" unless (2..8).include?(octaves)
        raise ArgumentError, "half_tones should be an Integer" unless Integer === half_tones
        num_notes_to_extend = (octaves-1)*half_tones
        raise ArgumentError, "half_tones out of range" unless (1..256).include?(num_notes_to_extend)
        isolate do
                        ld   hl, notes unless notes==hl
                        ld16 de, hl
                        ld   bc, half_tones*2
                        add  hl, bc
                        ld   a, num_notes_to_extend
          eloop         ex   de, hl
                        ld   c, [hl]
                        inc  hl
                        ld   b, [hl] # bc = notes[i]
                        inc  hl
                        ex   de, hl
                        inc  bc
                        srl  b
                        rr   c       # bc = (bc + 1)/2
                        ld   [hl], c
                        inc  hl
                        ld   [hl], b # notes[i+half_tones] = (notes[i] + 1)/2
                        inc  hl
                        dec  a
                        jr   NZ, eloop
        end
      end
      ##
      # Creates a routine for expanding the note to AY-3-891x tone period table to a higher number of octaves.
      # This is a faster version of the Macros.ay_expand_notes that uses a stack pointer register for reading
      # notes, so it requires that the interrupts are disabled while it's being run.
      #
      # Elements of the table are 2-byte words holding the AY-3-891x tone period values for each consecutive
      # note.
      #
      # * +notes+:: An address of the beginning of the lowest octave of the notes to expand.
      #             The memory should be pre-filled with tone period values for the 1st octave.
      #             See: Macros.ay_tone_periods.
      # Options:
      # * +octaves+:: A number of octaves, between 2 and 8, the notes should be expanded to.
      # * +half_tones+:: A number of half-tones per one octave.
      # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
      #               +sp+ will point to the beginning of the last octave of the table.
      # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide
      #                    +false+ only if you have already disabled the interrupts.
      # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
      #                   if you need to perform more uninterrupted actions.
      #
      # _NOTE_:: Restoring the +sp+ register uses self-modifying code.
      #
      # T-states: 39 + 71*(octaves-1)*half_tones + 30*save_sp + 4*disable_intr + 4*enable_intr (default options: 6041)
      #
      # Example:
      #
      #               ay_expand_notes_faster(notes)
      #               ...
      #
      #    notes      dw ay_tone_periods(min_octave:0, max_octave:0)
      #    notes_end  union notes[8*12], 1
      #
      # Modifies: +af+, +b+, +de+, +hl+ and optionally +sp+.
      def ay_expand_notes_faster(notes=hl, octaves:8, half_tones:12, save_sp:true, disable_intr:true, enable_intr:true)
        raise ArgumentError, "can't enable interrupts without restoring the sp register first" if enable_intr and !save_sp
        raise ArgumentError, "octaves out of range: #{octaves} (2-8)" unless (2..8).include?(octaves)
        raise ArgumentError, "half_tones should be an Integer" unless Integer === half_tones
        num_notes_to_extend = (octaves-1)*half_tones
        raise ArgumentError, "half_tones out of range" unless (1..256).include?(num_notes_to_extend)
        isolate do
                        ld   b, num_notes_to_extend
                        ld   hl, notes unless notes==hl
                        ld   [restore_sp + 1], sp if save_sp
                        di if disable_intr
                        ld   sp, hl
                        ld   de, half_tones*2
                        add  hl, de
          eloop         pop  de      # de = notes[i]
                        inc  de
                        srl  d
                        rr   e       # de = (de + 1)/2
                        ld   [hl], e
                        inc  hl
                        ld   [hl], d # notes[i+half_tones] = (notes[i] + 1)/2
                        inc  hl
                        djnz eloop
          restore_sp    ld   sp, 0 if save_sp
                        ei if enable_intr
        end
      end
      ##
      # Creates a routine that loads a constant 8-bit part of the AY-3-891x I/O addresses into +b+ or +c+
      # register.
      #
      # Run this routine before running one of:
      #
      # * ay_io_swap2out_bc
      # * ay_io_swap2inp_bc
      # * ay_io_swap2sel_bc
      #
      # to get a full AY-3-891x I/O address of the specific chip function into +bc+ register pair.
      #
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Modifies: +b+ or +c+.
      def ay_io_load_const_reg_bc(io_ay=self.io_ay)
        ay_reg_combined = ((io_ay.ay_out ^ io_ay.ay_sel) | (io_ay.ay_inp ^ io_ay.ay_sel))
        isolate do
          select(ay_reg_combined & 0xFF00, &:zero?).then do |eoc|
                ld   b, io_ay.ay_sel >> 8
          end.else_select(ay_reg_combined & 0x00FF, &:zero?).then do |eoc|
                ld   c, io_ay.ay_sel & 0x00FF
          end.else do
            raise ArgumentError, "ay_out, ay_inp and ay_sel should have different only 8-bit msb or lsb"
          end
        end
      end
      ##
      # Creates a routine that loads a specific 8-bit part of the AY-3-891x +output+ addresses into +b+ or +c+
      # register.
      #
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Example:
      #          ay_io_load_const_reg_bc
      #          ...
      #          ay_io_swap2sel_bc
      #          ld   a, ZXLib::AYSound::VOLUME_A
      #          out  (c), a        # select VOLUME_A register
      #          ay_io_swap2out_bc
      #          out  (c), e        # set a value of the selected register
      #
      # Modifies: +b+ or +c+.
      def ay_io_swap2out_bc(io_ay=self.io_ay)
        ay_reg_combined = ((io_ay.ay_out ^ io_ay.ay_sel) | (io_ay.ay_inp ^ io_ay.ay_sel))
        select(ay_reg_combined & 0xFF00, &:zero?).then do |eoc|
              ld   c, io_ay.ay_out & 0x00FF
        end.else_select(ay_reg_combined & 0x00FF, &:zero?).then do |eoc|
              ld   b, io_ay.ay_out >> 8
        end.else do
          raise ArgumentError, "ay_out, ay_inp and ay_sel should have different only 8-bit msb or lsb"
        end
      end
      ##
      # Creates a routine that loads a specific 8-bit part of the AY-3-891x +input+ addresses into +b+ or +c+
      # register.
      #
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Example:
      #          ay_io_load_const_reg_bc
      #          ...
      #          ay_io_swap2sel_bc
      #          ld   a, ZXLib::AYSound::VOLUME_A
      #          out  (c), a        # select VOLUME_A register
      #          ay_io_swap2inp_bc
      #          inp  e, (c)        # get a value from the selected register
      #
      # Modifies: +b+ or +c+.
      def ay_io_swap2inp_bc(io_ay=self.io_ay)
        ay_reg_combined = ((io_ay.ay_out ^ io_ay.ay_sel) | (io_ay.ay_inp ^ io_ay.ay_sel))
        select(ay_reg_combined & 0xFF00, &:zero?).then do |eoc|
              ld   c, io_ay.ay_inp & 0x00FF
        end.else_select(ay_reg_combined & 0x00FF, &:zero?).then do |eoc|
              ld   b, io_ay.ay_inp >> 8
        end.else do
          raise ArgumentError, "ay_out, ay_inp and ay_sel should have different only 8-bit msb or lsb"
        end
      end
      ##
      # Creates a routine that loads a specific 8-bit part of the AY-3-891x +select+ addresses into +b+ or +c+
      # register.
      #
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
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
      #
      # Modifies: +b+ or +c+.
      def ay_io_swap2sel_bc(io_ay=self.io_ay)
        ay_reg_combined = ((io_ay.ay_out ^ io_ay.ay_sel) | (io_ay.ay_inp ^ io_ay.ay_sel))
        select(ay_reg_combined & 0xFF00, &:zero?).then do |eoc|
              ld   c, io_ay.ay_sel & 0x00FF
        end.else_select(ay_reg_combined & 0x00FF, &:zero?).then do |eoc|
              ld   b, io_ay.ay_sel >> 8
        end.else do
          raise ArgumentError, "ay_out, ay_inp and ay_sel should have different only 8-bit msb or lsb"
        end
      end
      ##
      # Creates a routine that reads a specific AY-3-891x register's value.
      #
      # * +regn+:: A AY-3-891x register index as an integer or a 8-bit CPU register.
      # * +regv+:: An 8-bit CPU register to receive a value from a AY-3-891x register.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Modifies: +af+, +bc+ and +regv+.
      def ay_get_register_value(regn=a, regv=e, bc_const_loaded:false, io_ay:self.io_ay)
        raise ArgumentError unless register?(regv)
        isolate do
          if Integer === regn || [a,b,c].include?(regn)
                        ld   a, regn unless [0, a].include?(regn)
          end
          if bc_const_loaded
                        ay_io_swap2sel_bc(io_ay)
          else
                        ld   bc, io_ay.ay_sel
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
                        ay_io_swap2inp_bc(io_ay)
                        inp regv, (c)
        end
      end
      ##
      # Creates a routine that writes a value to a specific AY-3-891x register.
      #
      # If the block is given, the code it creates will be evaluated after the AY-3-891x register
      # has been selected and +bc+ registers has been loaded with an "output" AY-3-891x I/O address.
      # The +regv+ value will not be output in this instance and the +regv+ argument will be ignored.
      # Instead the code created by the block should perform writing out the value with <tt>out (c), regv</tt>.
      #
      # * +regn+:: A AY-3-891x register index as an integer or a 8-bit CPU register.
      # * +regv+:: A value to write to a AY-3-891x register as an integer or an 8-bit CPU register
      #            except +b+, +c+ or +a+.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Modifies: +af+, +bc+.
      def ay_set_register_value(regn=a, regv=e, bc_const_loaded:false, io_ay:self.io_ay)
        raise ArgumentError if [b,c].include?(regv) or (regv == a and regn != 0)
        isolate do |eoc|
          if Integer === regn || [a,b,c].include?(regn)
                        ld   a, regn unless [0, a].include?(regn)
          end
          if bc_const_loaded
                        ay_io_swap2sel_bc(io_ay)
          else
                        ld   bc, io_ay.ay_sel
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
                        ay_io_swap2out_bc(io_ay)
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
      # Creates a routine that sets volume of all AY-3-891x sound channels to 0, disables noise on all channels
      # and enables tone output on all channels.
      #
      # This effectively mutes all sound.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Modifies: +af+, +bc+.
      def ay_init(bc_const_loaded:false, io_ay:self.io_ay)
        isolate do
                        ld   a, AYSound::VOLUME_C
                        ay_io_load_const_reg_bc(io_ay) unless bc_const_loaded
          vol_res_loop  ay_set_register_value(a, 0, bc_const_loaded:true, io_ay:io_ay)
                        dec  a
                        cp   AYSound::MIXER
                        jr   NZ, vol_res_loop
                        ay_get_register_value(a, a, bc_const_loaded:true, io_ay:io_ay)
                        anda 0b11000000
                        ora  0b00111000
                        ay_io_swap2out_bc(io_ay)
                        out  (c), a
        end
      end
      ##
      # Creates a routine that gets the AY-3-891x mixer's value applies a block of code and sets the mixer value back.
      # The block of code should not modify +bc+ register pair.
      #
      # * +vinp+:: An 8-bit CPU register to receive the mixer's value.
      # * +vout+:: An 8-bit CPU register that will contain a value which will be written back to the mixer.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Example:
      #          ay_get_set_mixer(a) do |eoc|
      #            bit  0, a
      #            # jumping to the +eoc+ label will cancel writing mixer's value back.
      #            jr   NZ, eoc
      #            ora  0b00000001
      #          end
      #
      # Modifies: +af+, +bc+ and +vinp+.
      def ay_get_set_mixer(vinp=a, vout=vinp, bc_const_loaded:false, io_ay:self.io_ay)
        isolate do |eoc|
                        ay_get_register_value(AYSound::MIXER, vinp, bc_const_loaded:bc_const_loaded, io_ay:io_ay)
                        yield eoc
                        ay_io_swap2out_bc(io_ay)
                        out (c), vout
        end
      end
      ##
      # Creates a routine that sets a AY-3-891x channel's tone period.
      #
      # * +ch+:: A channel number 0..2 as an integer, label, or an 8-bit CPU register.
      # * +tph+:: The most significant 4 bits of the 12-bit tone period value as an integer or an 8-bit CPU register
      #           except +b+, +c+ or +a+. This value is known as the coarse tone period value.
      # * +tpl+:: The least significant 8 bits of the 12-bit tone period value as an integer or an 8-bit CPU register
      #           except +b+, +c+ or +a+. This value is known as the fine tone period value.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Modifies: +af+, +bc+.
      def ay_set_tone_period(ch=a, tph:d, tpl:e, bc_const_loaded:false, io_ay:self.io_ay)
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
                        ay_set_register_value(a, tpl, bc_const_loaded:bc_const_loaded, io_ay:io_ay)
                        inc  a
                        ay_set_register_value(a, tph, bc_const_loaded:true, io_ay:io_ay)
        end
      end
      ##
      # Creates a routine that sets a AY-3-891x channel's volume level.
      #
      # * +ch+:: A channel number 0..2 as an integer, label, or an 8-bit CPU register.
      # * +vol+:: A volume level or an 8-bit register except +b+, +c+ or +a+.
      #
      # Options:
      # * +vol_8bit+:: True if the volume is in the range: 0..255. False if the volume is in the range: 0..15.
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Modifies: +af+, +bc+.
      def ay_set_volume(ch=a, vol=e, vol_8bit:false, bc_const_loaded:false, io_ay:self.io_ay)
        raise ArgumentError if [a,b,c].include?(vol)
        isolate do
          if Integer === ch
                        ld   a, ch + AYSound::VOLUME_A
          else
                        ld   a, ch unless ch == a
                        add  AYSound::VOLUME_A
          end
          ay_set_register_value(a, vol, bc_const_loaded:bc_const_loaded, io_ay:io_ay) do |_|
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
      # Creates a routine that sets a AY-3-891x noise pitch.
      #
      # * +pitch+:: A pitch level or an 8-bit register except +b+, +c+ or +a+.
      #
      # Options:
      # * +pitch_8bit+:: True if the pitch is in the range: 0..255. False if the pitch is in the range: 0..31.
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Modifies: +af+, +bc+.
      def ay_set_noise_pitch(pitch=e, pitch_8bit:false, bc_const_loaded:false, io_ay:self.io_ay)
        isolate do
          ay_set_register_value(AYSound::NOISE_PERIOD, pitch, bc_const_loaded:bc_const_loaded, io_ay:io_ay) do |_|
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
      # Creates a routine that sets a AY-3-891x envelope duration.
      #
      # * +dh+:: The most significant 8 bits of the 16-bit envelope duration value as an integer or an 8-bit CPU register
      #          except +b+, +c+ or +a+. This value is known as the envelope coarse duration value.
      # * +dl+:: The least significant 8 bits of the 16-bit envelope duration value as an integer or an 8-bit CPU register
      #          except +b+, +c+ or +a+. This value is known as the envelope fine duration value.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Modifies: +af+, +bc+.
      def ay_set_envelope_duration(dh=d, dl=e, bc_const_loaded:false, io_ay:self.io_ay)
        isolate do
                        ld   a, AYSound::ENV_PER_FINE
                        ay_set_register_value(a, dl, bc_const_loaded:bc_const_loaded, io_ay:io_ay)
                        inc  a
                        ay_set_register_value(a, dh, bc_const_loaded:true, io_ay:io_ay)
        end
      end
      ##
      # Creates a routine that gets the AY-3-891x envelope shape's value applies a block of code and sets the value back.
      # The block of code should not modify +bc+ register pair.
      #
      # * +sinp+:: An 8-bit CPU register to receive the shape's value.
      # * +sout+:: An 8-bit CPU register that will contain a value which will be written back to the register.
      #
      # Options:
      # * +bc_const_loaded+:: If ay_io_load_const_reg_bc has been already run and the +bc+ registers' content is preserved since.
      # * +io_ay+:: A label with +ay_sel+, +ay_inp+ and +ay_out+ sub-labels addressing the AY-3-891x I/O bus.
      #
      # Modifies: +af+, +bc+ and +sinp+.
      def ay_get_set_env_shape(sinp=a, sout=sinp, bc_const_loaded:false, io_ay:self.io_ay)
        isolate do |eoc|
                        ay_get_register_value(AYSound::ENV_SHAPE, sinp, bc_const_loaded:bc_const_loaded, io_ay:io_ay)
                        yield eoc
                        ay_io_swap2out_bc(io_ay)
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

      ns :demo do
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

      ns :mute do
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