require 'utils/ay_sound'
require 'utils/sincos'
require 'z80/stdlib'

##
# ===AY music engine
class AYMusic
  include Z80
  include AYSound::Registers
  include AYSound::EnvelopeControl
  SinCosTable = Z80SinCos::SinCosTable
  SinCos      = Z80SinCos::SinCos
  ##
  # ====AY music engine utils
  module Macros
    ## Execute this code before each play iteration if you care about preserving the AY general purpose I/O ports' state.
    def ay_preserve_io_ports_state(music_control, play, bc_const_loaded:true, io:self.io128)
      isolate do
                          ay_get_register_value(AYMusic::MIXER, a, bc_const_loaded:bc_const_loaded, io:io)
                          ld   de, music_control.ay_registers.mixer
                          ld   b, 0b00111111
                          call play.apply_mask_de
      end
    end
    # CLOCK_HZ=3.5469/2 * 1_000_000
    # aaa=->(notes, steps) {2**(notes.to_f/steps.to_f/12.0)}
    # a=aaa[-12,STEPS]
    # x0=(CLOCK_HZ / (16.0 * 440.0/STEPS.to_f)).round
    # xf0=(CLOCK_HZ / (16.0 * 440.0/(STEPS.to_f/8)))
    #
    # STEPS=256
    # fc=->(steps){[2**(-1.0/steps.to_f),(CLOCK_HZ/(16.0*440.0/steps.to_f)).round,(CLOCK_HZ/(16.0*440.0/(steps.to_f/8)))]}
    # a,x0,xf0 = fc[STEPS]
    # a0=(a*65536).round
    # d1=->(fi){delta=0;(STEPS+1).times.inject(x0){|x,i| puts "#{i.to_s.rjust(4)}:#{x1=x>>3} #{x2=(xf0*a**i).round} #{delta+=(x2-x1).abs}"; ((x*a0)+fi)>>16};delta}
    # dd=->(fi){delta=0;(STEPS+1).times.inject(x0){|x,i| x1=x>>3; x2=(xf0*a**i).round; delta+=(x2-x1).abs; ((x*a0)+fi)>>16}; delta }
    # dd[16384+8192-673]
    # (-1024..1024).map{|i| [i,dd[16384+8192+i]]}
    #
    # a=2**(1.0/steps.to_f)
    # a0=((a-1.0)*65536).round
    # x0=(CLOCK_HZ / (16.0 * 440.0/STEPS.to_f)/2.0).round
    # xf0=(CLOCK_HZ / (16.0 * 440.0/(STEPS.to_f/8))/2.0)
    # d1=->(fi){delta=0;(STEPS+1).times.inject(x0){|x,i| puts "#{i.to_s.rjust(4)}:#{x1=(x+4)>>3} #{x2=(xf0*a**i).round} #{delta+=(x2-x1).abs}"; x+(((x*a0)+fi)>>16)};delta}
    # dd=->(fi){delta=0;(STEPS+1).times.inject(x0){|x,i| x1=(x+4)>>3; x2=(xf0*a**i).round; delta+=(x2-x1).abs; x+(((x*a0)+fi)>>16)};delta}
    # (0..1024).map{|i| [i,dd[16384+2048+i]]}
    # d1[16384+2048+594]
    # (note*256*32/12)
    def ay_note_to_fine_tone_cursor_table_factory(note_to_cursor, play:nil, num_notes:8*12)
      raise ArgumentError, "num_notes should be between 1 and 96" unless (1..8*12).include?(num_notes)
      isolate do |eoc|
                          ld   bc, 12
                          ld   hl, note_to_cursor
        floop             ld   a, b
                          cp   num_notes
                          jr   NC, eoc
                          push bc
                          3.times { rrca } # eeeddddd
                          ld   d, a
                          anda 0b11100000
                          ld   e, a
                          xor  d
                          ld   d, a
                          ex   de, hl
        if play.nil?
                          divmod h, c, check0:false, check1:false, optimize: :size
                          divmod l, c, clrrem:false, optimize: :size
        else
                          call play.divmod_hl_c
        end
                          ld   h, l
                          ld   l, 0
        if play.nil?
                          divmod l, c, clrrem:false
        else
                          call play.divmod_hl_c.divmod_rem_l_c
        end
                          ex   de, hl
                          ld   [hl], e
                          inc  hl
                          ld   [hl], d
                          inc  hl
                          pop  bc
                          inc  b
                          jr   floop
      end
    end

    def ay_tone_progress_table_factory(fine_tones, hz: 440, clock_hz: AYSound::CLOCK_HZ)
      steps = 256
      af = 2**(-1.0/steps.to_f)
      a0 = (af*65536).round
      fi = 16384+8192-673 # calculated numerically for given steps
      # xf = (clock_hz/(16.0*hz.to_f/(steps.to_f/8.0)))
      x0 = (clock_hz/(16.0*hz.to_f/steps.to_f)).round
      # x=((x*a0)+fi)>>16; x>>3
      isolate do |eoc|
                          ld   b, steps
                          exx
                          ld   de, fine_tones
                          ld   hl, x0
        mloop             ld16 bc, hl
                          ld   a, 3
        adjloop           srl  b
                          rr   c
                          dec  a
                          jr   NZ, adjloop
                          ex   de, hl
                          ld   [hl], c
                          inc  hl
                          ld   [hl], b
                          inc  hl
                          ex   de, hl
                          exx
                          dec  b
                          exx
                          jp   Z, eoc
                          ld   bc, a0
                          mul16_32(bc, tt:de, clrhlhl:fi, optimize: :size)
                          jp   mloop
      end
    end
  end

  export init
  export play
  export notes
  export music_control

  # data: loop_offset(,counter,delta)*,0 (init: counter=1, cursor=[loop_offset], loop_at=cursor+loop_offset+1)
  class EnvelopeControl < Label
    counter       byte # repeat - 0 - disabled
    current_value byte
    cursor        word # -> [repeat, 8-bit delta]
    loop_at       word
  end

  # data: loop_offset(,counter,mask)*,0 (init: counter=1, cursor=[loop_offset]+1, loop_at=cursor+loop_offset)
  class MaskControl < Label
    counter       byte # repeat - 0 - disabled
    current_mask  byte
    cursor        word # # -> [repeat, mask]
    loop_at       word
  end

  # data: loop_offset(,delay<<5|+ delta 0..31)*,0 (init: counter=1, cursor=[loop_offset]+1, loop_at=cursor+loop_offset)
  class ChordControl < Label
    counter       byte # delay - 0 - disabled
    current_offs  byte # current note offset
    cursor        word # -> delay<<5|+ delta 0..31
    loop_at       word
  end

  # data: delta,counter,current
  class ToneProgressControl < Label
    delta_lo      byte
    delta_hi      byte
    delta         delta_lo word # added to current
    counter_lo    byte
    counter_hi    byte
    counter       counter_lo word # how many iterations left
    current_lo    byte
    current_hi    byte
    current       current_lo word # cursor on fine tones table 0boooccccc cccfffff o - octave, c - cursor, f - fraction
  end

  # data: step,angle,amplitude (init: enabled:-1)
  class VibratoControl < Label
    enabled       byte # a boolean
    step          word # added to angle * 256.0
    angle         word # angle * 256.0
    amplitude     byte # amplitude in tone period, tp += sin(angle)*ampl
  end

  ## AY-3-8912 register mirror
  class AYRegisterMirror < Label
    tone_pitch_a      word
    tone_pitch_b      word
    tone_pitch_c      word
    noise_pitch       byte
    mixer             byte
    volume_a          byte
    volume_b          byte
    volume_c          byte
    envelope_duration word
    envelope_shape    byte
  end

  class TrackStackEntry < Label
    counter           byte # 0: a sub or a terminator
    signature         word # a loop_to signature or a return cursor from sub track
  end

  TRACK_STACK_DEPTH = 20
  TRACK_STACK_SIZE  = (TRACK_STACK_DEPTH + 1) * TrackStackEntry.to_i + 1
  TRACK_STACK_TOTAL = 6 * TRACK_STACK_SIZE

  ## A music track control structure
  class TrackControl < Label
    track_stack       word               # stack pointer for loops and subs: sp -> TrackStackEntry
    delay             byte               # how many iterations wait before going on
    cursor_lo         byte
    cursor_hi         byte
    cursor            cursor_lo word     # cursor pointing to the current track instruction
  end

  ## An instrument track control structure
  class InstrumentControl < Label
    track             TrackControl
    start_lo          byte
    start_hi          byte
    start             start_lo word      # a cursor where instrument is being restarted on note played
    NO_RESTART_ON_PLAY_NOTE_BIT=0
    flags             byte
    note_progress     byte # 0 - ignore tone progress when playing notes, 1 and more tone progress counter
  end

  class ChannelControl < Label
    volume_envelope   EnvelopeControl
    tone_progress     ToneProgressControl
    chord_progress    ChordControl
    current_note      byte
    vibrato_control   VibratoControl
    mask_env_ctrl     MaskControl
    ignore_volume     byte # 0: manual volume control, -1: ay envelope control
    mask_tone_ctrl    MaskControl
    tone_off          byte # 0: tone audible, -1: tone off
    mask_noise_ctrl   MaskControl
    noise_off         byte # 0: noise audible, -1: noise off
    track             TrackControl
    instrument        InstrumentControl
  end

  class MusicControl < Label
    ay_registers      AYRegisterMirror
    counter           byte              # a counter tracks can synchronize to
    noise_envelope    EnvelopeControl
    chan_a            ChannelControl
    chan_b            ChannelControl
    chan_c            ChannelControl
  end

  label_import        ZXSys
  macro_import        Z80Lib
  macro_import        Z80MathInt
  macro_import        Z80SinCos
  macro_import        AYSound

  # a track and envelopes index table
  instrument_table    addr 0xF000, 2

  # note to fine tones cursor table
  note_to_cursor      addr instrument_table[128], 2 # max count 96
  # fine tones durations table
  fine_tones          addr note_to_cursor[96], 2 # count 256
  track_stack_end     addr fine_tones[256] + TRACK_STACK_TOTAL, TrackStackEntry
  track_stack_size    addr TRACK_STACK_SIZE
  empty_instrument    addr track_stack_end[-1]
  sincos              addr 0xEC00, SinCos

  music_control       addr 0xE800, MusicControl

  ministack           addr 0xE800, 2 # depth: 6 (12 bytes)

  channel_control     addr 0, ChannelControl
  instrument_control  addr 0, InstrumentControl

  # stop interrupts first
  # 3 words of track.cursor addresses must follow
  ns :init, use: :io128 do
                      # clear control data
                      clrmem music_control, +music_control
                      # initialize pointers
                      pop  hl             # 3 words of track.cursor addresses must follow
                      ld   [restore_sp + 1], sp
                      ld   ix, music_control.chan_a.instrument.flags
                      ld   de, track_stack_end[-1]
                      ld   a, 3
    init_loop         ld   sp, ix         # instrument.flags
                      ld   bc, empty_instrument
                      push bc             # instrument.start
                      push bc             # instrument.track.cursor
                      dec  sp             # instrument.track.delay
                      push de             # instrument.track.track_stack
                      ld   c, [hl]
                      inc  hl
                      ld   b, [hl]        # bc: cursor
                      inc  hl
                      push bc             # track.cursor
                      ld   bc, -track_stack_size
                      ex   de, hl
                      add  hl, bc         # decrease stack pointer
                      dec  sp             # track.delay
                      push hl             # track.track_stack
                      add  hl, bc         # decrease stack pointer
                      ex   de, hl
                      ld   bc, +channel_control
                      add  ix, bc
                      dec  a
                      jr   NZ, init_loop
    restore_sp        ld   sp, 0
                      jp   (hl)
  end

  # inp hl: tone progress control (moves hl past it)
  # ZF:0 and bc: current tone
  macro :do_tone_progress do |_|
                      ld   sp, hl
                      pop  de                   # delta
                      pop  bc                   # counter, sp: -> current
                      ld   a, c
                      ora  b
                      jr   Z, no_change         # on target
                      dec  bc
                      pop  hl                   # current, sp: -> ToneProgress[1]
                      add  hl, de               # hl: 0boooccccc cccfffff
                      push hl                   # -> current
                      push bc                   # -> counter, sp: -> counter
                      pop  bc                   # sp: -> current
                      xor  a                    # a: 0
                      ld   c, a                 # c: 0
    3.times do                              # a|hl = hl * 8
                      add  hl, hl               # hl: 0bcccccccc fffff000
                      rla                       # a: 0b00000ooo
    end
                      ld   l, h
                      ld   h, c                 # hl: 0b00000000 cccccccc
                      add  hl, hl               # hl: 0b0000000c ccccccc0
                      ld   bc, fine_tones
                      add  hl, bc               # hl: fine_tones + 0bc ccccccc0
                      ld   c, [hl]
                      inc  hl
                      ld   b, [hl]              # de: tone period
                      inc  a                    # a: 1..9
      octave_loop     inc  bc                   # adjust to the current octave
                      srl  b
                      rr   c                    # (bc+1)/2 === (bc/2.0).round
                      dec  a
                      jr   NZ, octave_loop
                      inc  a                    # mark success (NZ)
    no_change         ld   hl, 2
                      add  hl, sp
                      ld   sp, ministack
  end

  ns :play, use: :io128 do
                      ld   [restore_sp + 1], sp
                      ld   sp, ministack
                      ay_io_load_const_reg_bc
                      xor  a                       # a=0
                      ld   hl, music_control.ay_registers # output registers' values
    rloop             ld   e, [hl]
                      inc  hl
                      ay_set_register_value(a, e, bc_const_loaded:true)
                      inc  a
                      cp   +music_control.ay_registers
                      jr   C, rloop

                      inc  [hl]                    # update counter

                      ld   de, music_control.chan_a
                      call track_progress
                      ld   de, music_control.chan_b
                      call track_progress
                      ld   de, music_control.chan_c
                      call track_progress

    sound_progress    label
                      ld   hl, music_control.noise_envelope
                      call envelope_progress       # a: current value
    skip_noise_env    3.times { rrca }
                      anda 0x1F                    # noise mask
                      jr   NZ, skip_min_noise
                      inc  a                       # min noise pitch=1
    skip_min_noise    exx
                      ld   hl, music_control.ay_registers.tone_pitch_a
                      ld   de, music_control.ay_registers.noise_pitch
                      ld   [de], a                 # noise_pitch
                      inc  de                      # mixer
                      inc  de                      # volume_a/next volume
                      ld   a, 0b00110110           # channel counter and mixer mask
    channel_prog_loop exx
                      ex   af, af                  # save counter and mixer mask
                      call envelope_progress       # a: current value
                      4.times { rrca }
                      exx
                      ld   b, 0xF0
                      call apply_mask_de           # volume
                      exx
    tone_progress     do_tone_progress             # if ZF:NZ then bc: tone period
                      jr   Z, skip_tone_progr      # it's no-op or at target
                      ld   de, +ChordControl + 1
                      add  hl, de                  # skip chord_progress and current_note
                      jr   vibrato_ctrl_ck         # bc: tone period
    skip_tone_progr   call chord_progress          # a: note offset
                      add  a, [hl]                 # current_note + note offset
                      inc  hl
                      ex   de, hl
                      call get_note_tone_period_bc # bc: tone period
                      ex   de, hl
    vibrato_ctrl_ck   ld16 de, bc                  # save tone period
                      call vibrato_progress        # bc: tone period delta
                      ex   de, hl
                      jr   NC, skip_vibr_progr
                      add  hl, bc                  # hl: tone period + delta
    skip_vibr_progr   push hl                      # save tone period
                      ex   de, hl
                      exx
                      pop  bc
                      ld   [hl], c                 # tone_pitch
                      inc  hl
                      ld   [hl], b                 # tone_pitch
                      inc  hl                      # next tone pitch
                      exx
                      call mask_progress           # mask_env_ctrl
                      exx
                      ld   b, 0b11101111           # env mask
                      call apply_mask_de           # volume
                      exx
    skip_mask_env     call mask_progress           # mask_tone_ctrl
                      ld   b, 0b11111000
                      call apply_mixer_mask
    skip_mask_tone    call mask_progress           # mask_noise_ctrl
                      ld   b, 0b11000111
                      call apply_mixer_mask
    skip_mask_noise   ld   bc, channel_control[1] - (channel_control.track)
                      add  hl, bc                  # skip track control
                      exx
                      inc  de                      # next ay_register.volume_(a|b|c)
                      ex   af, af
                      sll  a                       # rotate left mixer mask <- 1
                      jp   NC, channel_prog_loop
    restore_sp        ld   sp, 0
                      ret

    ns :track_progress do
                      ld16 ix, de                  # music_control.chan_X
                      ex   de, hl
                      ld   bc, channel_control.track.track_stack
                      add  hl, bc                  # music_control.chan_X.track.track_stack
                      call track_item_proc         # hl: channel_control.track.track_stack
                      inc  hl                      # hl: skip instrument.track.cursor_hi
                      call track_item_proc         # hl: channel_control.instrument.track.track_stack
                      ret
    end

    ns :track_item_proc do # hl: TrackControl
                      push hl
                      pop  iy                      # iy: track_control.track_stack
                      inc  hl
                      inc  hl
      track_item_loop ld   a, [hl]                 # track_control.delay
                      anda a
                      jr   NZ, countdown
                      inc  hl                      # track_control.cursor
                      ld   e, [hl]
                      inc  hl
                      ld   d, [hl]                 # de: cursor
                      push hl                      # track_control.cursor_hi
                      call process_item
                      pop  hl                      # track_control.cursor_hi
                      ld   [hl], d
                      dec  hl
                      ld   [hl], e
                      dec  hl                      # track_control.delay
                      jr   track_item_loop

      countdown       dec  [hl]                    # track_control.delay
                      2.times { inc hl }           # track_control.cursor_hi
                      ret                          # delay in process

      end_of_track    ld   sp, iy                  # sp: track_control.track_stack
                      pop  hl                      # track return address pointer
                      inc  hl                      # ignore counter
                      ld   e, [hl]
                      inc  hl
                      ld   d, [hl]                 # track return cursor
                      inc  hl
                      ld   a, e
                      ora  d                       # is it though?
                      jr   Z, nowhere_to_go
                      push hl                      # put back track_control.track_stack 
      nowhere_to_go   ld   sp, ministack[-4]
                      ret  NZ
                      pop  hl                      # pop return address
                      pop  hl                      # pop track_control.cursor_hi
                      ret                          # return back from track_item_proc

      process_item    label
                      ld   a, [de]                 # track item
                      anda a
                      jr   Z, end_of_track         # end of track
                      inc  de                      # advance track cursor
                      cp   97
                      jr   C, play_note
                      cp   176
                      jr   NC, wait_some
                      cp   160
                      jr   NC, set_volume
                      cp   128
                      jr   NC, set_noise
                      ld   hl, cmd_table1 - 97
                      ld   c, a
                      ld   b, 0
                      add  hl, bc
                      ld   c, [hl]
                      add  hl, bc
                      jp   (hl)
    end

    # current_tone_progress = (note*256*32/12)
    # delta = (target - current) / counter
    ns :play_note do # a: note + 1
                      dec  a
                      ld   [ix + channel_control.current_note], a
                      exx
                      ld   hl, note_to_cursor
                      call get_indexed_table_entry_bc   # bc: target cursor
                      ld   a, [ix + channel_control.instrument.note_progress]
                      cp   1
                      jr   C, skip_progress             # completely ignore progress
                      jr   NZ, calc_progress
                      ld   [ix + channel_control.tone_progress.current_lo], c
                      ld   [ix + channel_control.tone_progress.current_hi], b
                      ld   bc, 0
                      jr   set_counter                  # just store current value and clear progress
      calc_progress   ld   l, [ix + channel_control.tone_progress.current_lo]
                      ld   h, [ix + channel_control.tone_progress.current_hi]
                      sbc  hl, bc                       # current - target
                      ld   c, a                         # counter (note_progress)
                      sbc  a                            # a: 0 if current >= target, -1 if current < target
                      ld   e, a                         # e: sgn
                      call complement16_hle             # hl: -hl if e == -1
                      ld   a, e
                      cpl                               # a: -1 if current >= target, 0 if current < target
                      ld   e, a                         # e: sgn
                      call divmod_hl_c
                      call complement16_hle             # hl: -hl if e == -1
                      ld   [ix + channel_control.tone_progress.delta_lo], l
                      ld   [ix + channel_control.tone_progress.delta_hi], h
      set_counter     ld   [ix + channel_control.tone_progress.counter_lo], c
                      ld   [ix + channel_control.tone_progress.counter_hi], b # b: 0 after divmod_hl_c
      skip_progress   exx
                      bit  InstrumentControl::NO_RESTART_ON_PLAY_NOTE_BIT, [ix + channel_control.instrument.flags]
                      ret  NZ                           # don't reset instrument
                      ld   c, [ix + channel_control.instrument.start_lo]
                      ld   [ix + channel_control.instrument.track.cursor_lo], c
                      ld   c, [ix + channel_control.instrument.start_hi]
                      ld   [ix + channel_control.instrument.track.cursor_hi], c
                      ld   [ix + channel_control.instrument.track.delay], 0
                      ret
    end

    ns :wait_some do # a: delay + 175
                      sub 175
                      2.times { dec hl } # delay
                      ld  [hl], a
                      ret
    end

    ns :set_volume do # a: volume + 160
                      sub  160
                      4.times { add a, a }
                      ld   [ix + channel_control.volume_envelope.current_value], a
                      ret
    end
                      # ld   hl, music_control
    ns :set_noise do # a: pitch + 128
                      sub  128
                      3.times { add a, a }
                      ld   [music_control.noise_envelope.current_value], a
                      ret
    end

    cmd_table1        label
                      data :pc,  set_instrument # track only
                      data :pc,  sync_counter   # track only
                      data :pc,  set_play_mode_1 # track only
                      data :pc,  set_play_mode_2 # track only
                      data :pc,  set_envelope_duration
                      data :pc,  set_envelope_shape
                      data :pc,  set_volume_envelope_index
                      data :pc,  set_noise_envelope_index
                      data :pc,  set_chord_index
                      data :pc,  set_mask_env_index
                      data :pc,  set_mask_tone_index
                      data :pc,  set_mask_noise_index
                      data :pc,  set_vibrato_step
                      data :pc,  set_vibrato_angle
                      data :pc,  set_vibrato_amplitude
                      data :pc,  disable_vibrato
                      data :pc,  set_note_progress
                      data :pc,  set_tone_progress
                      data :pc,  set_ignore_volume
                      data :pc,  set_ignore_volume
                      data :pc,  set_tone_off
                      data :pc,  set_tone_off
                      data :pc,  set_noise_off
                      data :pc,  set_noise_off
                      data :pc,  sub_track
                      data :pc,  loop_next
                      # here be dragons

    # track only
    ns :set_instrument do
                      call get_instrument_table_entry_bc
                      jr   NZ, skip_empty
                      ld   bc, empty_instrument
      skip_empty      ld   [ix + channel_control.instrument.start_lo], c
                      ld   [ix + channel_control.instrument.start_hi], b
                      # ld   [ix + channel_control.instrument.track.cursor_lo], empty_instrument
                      # ld   [ix + channel_control.instrument.track.cursor_hi], empty_instrument>>8
                      ret
    end

    # track only
    ns :sync_counter do
                      ld   a, [de] # counter value to wait for
                      inc  de
                      ld   hl, music_control.counter
                      sub  [hl]    # target - current
                      ld   [ix + channel_control.track.delay], a
                      ret
    end

    # track only
    ns :set_play_mode_1 do
                      res  InstrumentControl::NO_RESTART_ON_PLAY_NOTE_BIT, [ix + channel_control.instrument.flags]
                      ret
    end

    # track only
    ns :set_play_mode_2 do
                      set  InstrumentControl::NO_RESTART_ON_PLAY_NOTE_BIT, [ix + channel_control.instrument.flags]
                      ret
    end

    ns :set_envelope_duration do # envelope duration
                      ex   de, hl
                      ld   e, [hl]
                      inc  hl
                      ld   d, [hl]
                      inc  hl
                      ex   de, hl
                      ld   [music_control.ay_registers.envelope_duration], hl
                      ret
    end

    ns :set_envelope_shape do # envelope shape
                      ld   a, [de]
                      inc  de
                      ld   [music_control.ay_registers.envelope_shape], a
                      ret
    end

    ns :set_volume_envelope_index do
                      push ix # channel_control.volume_envelope
                      jr   set_noise_envelope_index.init_envelope
    end

    ns :set_noise_envelope_index do
                      ld   hl, music_control.noise_envelope
                      push hl
      init_envelope   call get_instrument_table_entry_bc
                      pop  hl
                      jr   Z, disable_ctrl
                      ld   [hl], 1 # counter
                      inc  hl      # current_value
                      inc  hl      # cursor
                      ld   [hl], c # cursor_lo
                      inc  hl
                      ld   [hl], b # cursor_hi
                      inc  hl      # loop_at
                      ld   a, [bc] # loop_offset
                      adda_to b, c # cursor + loop_offset
                      inc  bc      # cursor + loop_offset + 1
                      ld   [hl], c # loop_at_lo
                      inc  hl
                      ld   [hl], b # loop_at_hi
                      ret
      disable_ctrl    ld   [hl], a # counter - disables ctrl, a==0
                      ret
    end

    ns :set_chord_index do
                      ld   hl, channel_control.chord_progress
      init_ctrl       ld16 bc, ix
                      add  hl, bc
                      push hl
                      call get_instrument_table_entry_bc
                      pop  hl
                      jr   Z, set_noise_envelope_index.disable_ctrl
                      ld   [hl], 1 # counter
                      inc  hl      # current_offs/current_mask
                      inc  hl      # cursor
                      ld   a, [bc] # loop_offset
                      inc  bc      # cursor += 1
                      ld   [hl], c # cursor_lo
                      inc  hl
                      ld   [hl], b # cursor_hi
                      inc  hl      # loop_at
                      adda_to b, c # cursor + loop_offset
                      ld   [hl], c # loop_at_lo
                      inc  hl
                      ld   [hl], b # loop_at_hi
                      ret
    end

    ns :set_mask_env_index do
                      ld   hl, channel_control.mask_env_ctrl
                      jr   set_chord_index.init_ctrl
    end


    ns :set_mask_tone_index do
                      ld   hl, channel_control.mask_tone_ctrl
                      jr   set_chord_index.init_ctrl
    end

    ns :set_mask_noise_index do
                      ld   hl, channel_control.mask_noise_ctrl
                      jr   set_chord_index.init_ctrl
    end

    ns :set_vibrato_step do
                      ld   hl, channel_control.vibrato_control.step
      enable_set_vib  ld16 bc, ix
                      add  hl, bc
                      ld   a, [de]
                      inc  de
                      ld   [hl], a
                      inc  hl
                      ld   a, [de]
                      inc  de
                      ld   [hl], a
      enable_vibrato  ld   [ix + channel_control.vibrato_control.enabled], -1
                      ret
    end

    ns :set_vibrato_angle do
                      ld   hl, channel_control.vibrato_control.angle
                      jr   set_vibrato_step.enable_set_vib
    end

    ns :set_vibrato_amplitude do
                      ld   a, [ix + channel_control.current_note]
                      call get_note_tone_period_bc
                      dec  hl
                      dec  hl
                      ld   a, [hl]
                      dec  hl
                      ld   l, [hl]
                      ld   h, a
                      ora  a # CF: 0
                      sbc  hl, bc  # notes[note-1] - notes[note]
                      ld   a, [de] # amplitude
                      inc  de
                      mul  l, a, tt:bc, clrhl:true, signed_k:false
                      ld   [ix + channel_control.vibrato_control.amplitude], h
                      jr   set_vibrato_step.enable_vibrato
    end

    ns :disable_vibrato do
                      ld   [ix + channel_control.vibrato_control.enabled], 0
                      ret
    end

    ns :set_note_progress do
                      ld   a, [de]
                      inc  de
                      ld   [ix + channel_control.instrument.note_progress], a
                      ret
    end

    ns :set_tone_progress do
                      ld   hl, channel_control.tone_progress.delta
                      ld16 bc, ix
                      add  hl, bc
                      ex   de, hl
                      ld   bc, 4
                      ldir
                      ex   de, hl
                      # ld   a, [de]
                      # inc  de
                      # ld   [ix + channel_control.tone_progress.delta_lo], a
                      # ld   a, [de]
                      # inc  de
                      # ld   [ix + channel_control.tone_progress.delta_hi], a
                      # ld   a, [de]
                      # inc  de
                      # ld   [ix + channel_control.tone_progress.counter_lo], a
                      # ld   a, [de]
                      # inc  de
                      # ld   [ix + channel_control.tone_progress.counter_hi], a
                      ld   [ix + channel_control.instrument.note_progress], 0
                      ret
    end

    ns :set_ignore_volume do
                      rrca
                      sbc  a, a
                      ld   [ix + channel_control.ignore_volume], a
                      ret
    end

    ns :set_tone_off do
                      rrca
                      sbc  a, a
                      ld   [ix + channel_control.tone_off], a
                      ret
    end

    ns :set_noise_off do
                      rrca
                      sbc  a, a
                      ld   [ix + channel_control.noise_off], a
                      ret
    end

    ns :sub_track do # index
                      jr   sub_track_continue
    end

    ns :loop_next do # count(1), address(2)
                      ld   sp, iy                  # track.track_stack
                      pop  hl                      # hl: loop stack address
                      ld   sp, hl                  # sp: track_stack.counter
                      inc  sp                      # sp: track_stack.signature
                      pop  bc                      # bc: signature
                      ld   a, c
                      cp   e
                      jr   NZ, add_level           # not our loop, add another one
                      ld   a, b
                      cp   d
                      jr   NZ, add_level           # not our loop, add another one
                      dec  [hl]                    # counter -= 1 on track_stack.counter
                      jr   Z, loop_over
                      ld   sp, hl                  # sp: track_stack.counter
      jump_to_addr    ex   de, hl
                      inc  hl
                      ld   e, [hl]                 # jump to current + 2nd argument (twos complement negative value)
                      ld   d, -1
                      add  hl, de
                      ex   de, hl
      loop_over_back  ld   [store_sp + 2], iy      # set target sp address (iy: track.track_stack)
      store_sp        ld   [channel_control.track.track_stack], sp
                      ld   sp, ministack[-4]
                      ret
      add_level       ld   sp, hl                  # loop stack address
                      ld   a, [de]                 # counter
                      anda a
                      jr   Z, jump_to_addr         # loop forever
                      push de                      # signature
                      push af                      # [sp] = a; sp-= 2
                      inc  sp                      # sp += 1
                      jr   jump_to_addr
      loop_over       label                        # end loop
                      2.times { inc  de }
                      jr   loop_over_back
    end

    ns :sub_track_continue do # index
                      call get_instrument_table_entry_bc
                      ret  Z                       # sub 0 does nothing
                      ld   sp, iy                  # track.track_stack
                      pop  hl                      # hl: track stack address
                      dec  hl
                      ld   [hl], d
                      dec  hl
                      ld   [hl], e                 # save de as a return track pointer
                      dec  hl
                      ld   [hl], 0                 # not a loop marker (loop.counter)
                      push hl                      # puts sub stack address back
                      ld16 de, bc
                      ld   sp, ministack[-4]
                      ret
    end

    ns :apply_mixer_mask do # a: value, b: ~mask, a': channel ~mask 0b00110110
                        ex   af, af
                        ld   c, a                    # save channel ~mask
                        ora  b                       # channel ~mask | ~mask
                        ld   b, a                    # b: channel ~mask | ~mask
                        ld   a, c                    # restore channel ~mask
                        ex   af, af
                        ld   de, music_control.ay_registers.mixer
    end

    ns :apply_mask_de do # de: address, a: value, b: ~mask (0 - new value, 1 - preserve)
                        ld   c, a                    # c: value to set
                        ld   a, [de]
                        xor  c                       # original ^ value
                        anda b                       # (original ^ value) & ~mask
                        xor  c                       # ((original ^ value) & ~mask) ^ value
                        ld   [de], a
                        ret
    end

    ns :get_instrument_table_entry_bc do # a: 1-128
                        ld   a, [de] # instrument index 0..128
                        inc  de
                        anda a # index is 0
                        ret  Z # assume hl is >= 256
                        dec  a
                        ld   hl, instrument_table
                        jr   get_indexed_table_entry_bc
    end

    ns :get_note_tone_period_bc do
                        ld   hl, notes
    end
    ns :get_indexed_table_entry_bc do
                        add  a, a
                        adda_to h, l
                        ld   c, [hl]
                        inc  hl
                        ld   b, [hl]
                        ret
    end

    complement16_hle    twos_complement16_by_sgn(h, l, e, th:h, tl:l)
                        ret

    ns :divmod_hl_c do
                        divmod h, c, check0:false, check1:false, optimize: :size
      divmod_rem_l_c    divmod l, c, clrrem:false, optimize: :size
                        ret
    end

    # inp hl: envelope control (moves hl past it, updates current value)
    # out a: current value
    ns :envelope_progress do
                    ld   sp, hl
                    pop  bc               # b: value, c: counter
                    ld   a, c
                    anda a                # check counter
                    ld   a, b             # current_value
                    jr   Z, no_change
                    pop  hl               # cursor
                    dec  c                # counter =- 1
                    jr   Z, cursor_next
      restart       push hl               # cursor
                    add  a, [hl]          # -> current + delta
                    bit  7, [hl]          # check delta sign
                    jr   NZ, minus_delta
                    jr   C, min_max_value
                    scf
      minus_delta   jr   C, set_value
      min_max_value sbc  a, a
      set_value     ld   b, a             # new value
      no_change     push bc               # set counter and value, move sp to beginning
                    ld   hl, +EnvelopeControl
                    add  hl, sp           # hl: sp + sizeof EnvelopeControl
                    ld   sp, ministack[-1]
                    ret
      cursor_next   inc  hl               # cursor+=1 -> counter
      get_cursor    ld   a, [hl]          # -> next counter
                    anda a
                    jr   Z, cursor_reset
                    ld   c, a             # new counter
                    ld   a, b             # current value
                    inc  hl               # cursor+=1 -> delta
                    jr   restart
      cursor_reset  pop  hl               # loop_at
                    push hl               # back at cursor
                    jr   get_cursor
    end

    # inp hl: tone chord control (moves hl past it)
    # a: current note offset
    ns :chord_progress do
                    ld   a, [hl]
                    anda a
                    jr   Z, adjust_exit
      proceed       dec  [hl]         # counter
                    jr   Z, cursor_next
                    inc  hl
                    ld   a, [hl]      # current_offs
                    dec  hl
      adjust_exit   ld   bc, 6
                    add  hl, bc
                    ret
      cursor_next   ld   sp, hl
                    pop  af           # move sp to cursor
                    pop  bc           # cursor
      restart       ld   a, [bc]      # delta<<5|note_offset
                    anda a
                    jr   Z, reset_cursor
                    inc  bc
                    push bc           # cursor
                    ld   c, a
                    anda 0x1F
                    ld   b, a         # b: note offset
                    xor  c            # counter
                    3.times { rlca }  # reposition counter
                    ld   c, a         # c: counter
                    push bc           # note offset|counter
                    ld   a, b         # a: note offset
                    ld   sp, ministack[-1]
                    jr   adjust_exit
      reset_cursor  pop  bc           # loop_at
                    push bc           # back at loop_at
                    jr   restart
    end

    # inp hl: envelope control (moves hl past it on CF:1)
    # CF:1 out bc: current tone period delta
    ns :vibrato_progress do
                    ld   a, [hl]
                    anda a
                    jr   Z, chord_progress.adjust_exit
                    inc  hl
                    ld   sp, hl
                    pop  bc                 # step
                    pop  hl                 # angle
                    add  hl, bc
                    push hl                 # angle
                    ld   a, h               # angle
                    sincos_from_angle(sincos, h, l)
                    ld   c, [hl]
                    inc  l
                    ld   b, [hl]
                    inc  sp                 # move sp past angle (next will pop hi angle byte and ampl)
                    pop  af                 # a: ampl, f: ignore angle hi, sp: VibratoControl[1]
                    mul8 b, c, a, tt:bc, clrhl:true, double:false
                    ld   c, h
                    sla  h
                    sbc  a
                    ld   b, a
                    ld   hl, 0
                    add  hl, sp
                    ld   sp, ministack[-1]
                    scf
                    ret
    end

    # inp de: envelope control (preserves hl)
    # out a: current mask value 0|-1
    ns :mask_progress do
                    ld   a, [hl]
                    anda a
                    jr   Z, adjust_exit
                    ld   sp, hl
                    pop  bc               # c: counter, b: current
                    dec  c                # c: counter -= 1
                    jr   Z, cursor_next
      restart       rlc  b                # rotate mask left
                    sbc  a, a             # 0 or -1
                    push bc               # put c: counter + b: current
                    ld   hl, +MaskControl + 1 # skip constant mask value
                    add  hl, sp           # hl: sp + sizeof MaskControl
                    ld   sp, ministack[-1]
                    ret

      adjust_exit   ld   bc, +MaskControl
                    add  hl, bc
                    ld   a, [hl]          # get const mask instead
                    inc  hl
                    ret

      cursor_next   pop  hl               # cursor
      get_cursor    ld   a, [hl]          # -> next counter
                    anda a
                    jr   Z, reset_cursor
                    ld   c, a             # c: counter
                    inc  hl               # cursor+=1
                    ld   b, [hl]          # -> next mask
                    inc  hl               # cursor+=1
                    push hl               # put back cursor
                    jr   restart
      reset_cursor  pop  hl               # loop_at
                    push hl               # back at cursor
                    jr   get_cursor
    end
  end # :play

  # notes durations table
  notes           label
end

if __FILE__ == $0
    # :stopdoc:
  require 'utils/zx7'
  require 'utils/ay_music/music_box'
  require 'zxlib/basic'

  class MusicTest
    include Z80
    include Z80::TAP
    include AYSound::EnvelopeControl
    extend ::MusicBox::Helpers
    SinCos      = AYMusic::SinCos
    SinCosTable = AYMusic::SinCosTable

    import            ZXSys, macros: true, code: false
    macro_import      AYSound
    macro_import      AYMusic
    macro_import      Z80MathInt
    macro_import      Z80SinCos

    sincos              addr 0xE000, SinCos
    ministack           addr sincos[0], 2
    # note to fine tones cursor table
    note_to_cursor      addr sincos + (+SinCosTable), 2 # max count 96
    # fine tones durations table
    fine_tones          addr note_to_cursor[96], 2 # count 256
    track_stack_end     addr fine_tones[256] + AYMusic::TRACK_STACK_TOTAL, AYMusic::TrackStackEntry
    empty_instrument    addr track_stack_end[-1]
    music_control       addr track_stack_end[0], AYMusic::MusicControl

    AY_MUSIC_OVERRIDE = { instrument_table: instrument_table, notes: notes, sincos: sincos,
                          note_to_cursor: note_to_cursor, fine_tones: fine_tones,
                          track_stack_end: track_stack_end,
                          empty_instrument: empty_instrument,
                          music_control: music_control, ministack: ministack }

    with_saved :demo, :exx, hl, ret: true, use: [:io128, :io] do
                        di
                        call mute
                        call make_sincos
      ns :extend_notes do
                        ay_extend_notes(music.notes, octaves:8, save_sp:true, disable_intr:false, enable_intr:false)
      end
      ns :tone_progress_table_factory do
                        ay_tone_progress_table_factory(fine_tones, hz: 440)
      end
      ns :note_to_fine_tone_cursor_table_factory do
                        ay_note_to_fine_tone_cursor_table_factory(note_to_cursor, play: music.play)
      end
                        call music.init
                        dw   track1, track2, track3
                        # ld   a, 15
                        # ld   [music.ay_registers.volume_a], a
                        # ld   [music.ay_registers.volume_b], a
                        # ld   [music.ay_registers.volume_c], a
                        # ld   a, 31
                        # ld   [music.ay_registers.noise_pitch], a
                        # ld   a, 0b00111000
                        # ld   [music.music_control.ay_registers.mixer], a
                        # ld   a, ALTERNATE|ATTACK|CONTINUE
                        # ld   [music.music_control.ay_registers.envelope_shape], a
                        # ld   hl, 2
                        # ld   [music.music_control.ay_registers.envelope_duration], hl
                        # ld   a, 255
                        # ld   [music.music_control.noise_envelope.current_value], a
                        # ld   hl, envelope0.counter
                        # ld   [music.music_control.noise_envelope], hl
                        # ld   hl, envelope1.counter
                        # ld   [music.music_control.chan_c.volume_envelope], hl

                        #   volume_envelope: envelope1.counter, current_volume: 255,
                        #   tone_progress: 0, chord_progress: 0, vibrato_control: 0, 
                        #   mask_env_ctrl: 0, mask_tone_ctrl: 0, mask_noise_ctrl: 0
      forever           ei
                        halt
                        di
                        push iy
                        xor  a
                        out  (io.ula), a
                        ay_preserve_io_ports_state(music.music_control, music.play, bc_const_loaded:false)
                        call music.play
                        ld   a, 6
                        out  (io.ula), a
                        pop  iy
                        key_pressed?
                        jp  Z, forever
                        call mute
                        ei
    end

    ns :mute, use: :io128 do
                        ay_init
                        ret
    end

    import            AYMusic, :music, override: AY_MUSIC_OVERRIDE
    music_end         label

    instrument_table  instruments(
                          instrument1,
                          instrument2,
                          instrument3,
                          instrument4,
                          instrument5,
                          chord_data1,
                          env_data1,
                          env_data2,
                          mask_data1,
                          mask_data2,
                          mask_data3,
                          mask_data4,
                          mask_data_on,
                          mask_data_off,
                          subtrack1,
                          subtrack2
                      )

    music_track :track1 do
      envd 15
      envs ALTERNATE|ATTACK|CONTINUE
      n 31
      i :instrument1
      l :wacek
      b 0; w 50
      a 1; w 50
      a 2; w 50
      l :placek
      a 3; w 50
      a 4; w 50
      lt :placek, 3
      a 5; w 50
      a 6; w 50
      s 200
      lt :wacek, 2
      v 0
    end

    music_track :track2 do
      v  15
      i  :instrument5
      np 1
      g 3; w 50
      np 10
      e 3; w 100
      f 3; w 50
      d 3; w 100
      c 3; w 25
      e 3; w 25
      g 3; w 15
      tp -24, 1000; g 1
      w 1000
      vs 1
      va 0.25
      w 20; vs 5; va 0.3
      w 20; vs 6; va 0.4
      w 20; vs 8; va 0.5
      w 20; vs 12; va 0.6
      w 20; vs 16; va 0.7
      w 20; vs 20; va 0.8
      w 20; vs 30; va 0.8
      w 20; vs 50; va 0.9
    end

    music_track :track3 do
      v 15
      l :forever
      sub_track :subtrack1
      a 4; w 50
      sub_track :subtrack1
      a 4; w 50
      i :instrument2
      lt :forever
    end

    music_track :subtrack1 do
      c 3; w 20
      f! 3; w 20
      sub :subtrack2
      g 3; w 20
      b_ 3; w 20
      sub :subtrack2
    end

    music_track :subtrack2 do
      g 3; w 20
      a_ 3; w 20
      e 3; w 20
      c! 3; w 20
    end

    music_track :instrument1 do
      me :mask_data3
      mn :mask_data1
      mt :mask_data2
      n 31
      v 15
      ne 4
      vg 0
      vs 10
      va 0.1
      w 15
      vs 20
      va 0.4
      w 15
      vs 30
      va 0.75
      w 10
      vo
    end

    music_track :instrument2 do
      v 15
      ve :env_data1
      ce :chord_data1
    end

    music_track :instrument3 do
      v 15
      n 31
      mn :mask_data_on
      mt :mask_data_off
      ne :env_data2
    end

    music_track :instrument4 do
      v 15
      mt :mask_data4
    end

    music_track :instrument5 do
      envd 15
      envs ALTERNATE|ATTACK|CONTINUE
      v 15
      n0;t1
      l :forever
      vv
      w 4
      fv
      w 10
      lt :forever
    end

    # current_channel db   0
    # 0boooccccc cccfffff
    # toneprogress0     data AYMusic::ToneProgressControl, {delta: 8, current: 0b0000000000000000, counter: 512*4-1}
    # toneprogress1     data AYMusic::ToneProgressControl, {delta: -32, current: 0b0011111111111111, counter: 511, target: 0xFBF}
    # vibrato0          data AYMusic::VibratoControl, {angle: 0, step: 50<<8, amplitude: 0x7DF-0x76E}
    # vibrato1          data AYMusic::VibratoControl, {angle: 128<<8, step: 25<<8, amplitude: 0x7DF-0x76E}
    # vibrato2          data AYMusic::VibratoControl, {angle: 0, step: 10<<8, amplitude: 30}
    # chord0            data AYMusic::ChordControl, {counter: 1, cursor: chord_data_end, loop_at: chord_data1}
    # envelope0         data AYMusic::EnvelopeControl, {counter: 1, cursor: env_data_end-1, loop_at: env_data}
    # envelope1         data AYMusic::EnvelopeControl, {counter: 1, cursor: env_data_end2-1, loop_at: env_data2}
    # mask0             data AYMusic::MaskControl, {counter: 1, current_mask: 0b00000000, cursor: mask_data_end, loop_at: mask_data}
    # mask1             data AYMusic::MaskControl, {counter: 1, current_mask: 0b00000000, cursor: mask_data_end2, loop_at: mask_data2}
    # mask2             data AYMusic::MaskControl, {counter: 1, current_mask: 0b00000000, cursor: mask_data_end3, loop_at: mask_data3}

    chord_data1       music_chord_data :all, [3, 0], [2, 4], [1, 7], [1, 10]
    env_data1         music_envelope_data :all, [255, -1]
    env_data2         music_envelope_data :all, [32, -1.0], [32, 1.0]
    mask_data1        music_mask_data :all, 32, 0b10101010, 32, 0b11001100, 16, 0b11110000, 8, 0b11111111, 8, 0b00000000
    mask_data2        music_mask_data -3, [32, 0b11110000], [32, 0b10101010], [32, 0b11001100]
    mask_data3        music_mask_data :all, 8, 0b11111111, 8, 0b00000000
    mask_data4        music_mask_data :all, 128, 0b01110111
    mask_data_off     music_mask_data :all, 255, 255
    mask_data_on      music_mask_data :all, 255, 0

    NOTES = ay_tone_periods(min_octave:0, max_octave:7)

    (1...NOTES.length).each do |i|
      puts "#{NOTES[i-1].to_s.rjust(4)}-#{NOTES[i].to_s.rjust(4)} #{NOTES[i-1]-NOTES[i]} #{(NOTES[i-1].to_f/NOTES[i])}"
    end

                      dw NOTES[11]*2
    notes             dw NOTES[0...12]
                      words 7*12

    make_sincos       create_sincos_from_sintable sincos, sintable:sintable

    sintable          bytes   neg_sintable256_pi_half_no_zero_lo
    sincos_end        label
  end

  music = MusicTest.new 0x8000
  puts music.debug
  puts "music size: #{music[:music_end] - music[:music]}"
  puts "TRACK_STACK_TOTAL: #{AYMusic::TRACK_STACK_TOTAL}"
  puts "TRACK_STACK_SIZE : #{AYMusic::TRACK_STACK_SIZE}"
  puts "TRACK_STACK_DEPTH: #{AYMusic::TRACK_STACK_DEPTH}"
  %w[
    +demo.extend_notes +demo.tone_progress_table_factory +demo.note_to_fine_tone_cursor_table_factory
    instrument_table
    notes
    ministack
    sincos
    note_to_cursor
    fine_tones
    track_stack_end empty_instrument
    music_control music.music_control
    music.init
    music.play music.play.play_note
    +music_control
    mute
    demo
    track1 track2 track3
  ].each do |label|
    puts "#{label.ljust(30)}: 0x#{'%04x'%music[label]} - #{music[label]}"
  end

  ZX7.compress music.code[(music[:music]-music.org),(music[:music_end] - music[:music])]
  ZX7.compress music.code[0, music[:make_sincos]-music.org]
  ZX7.compress music.code
  program = Basic.parse_source <<-EOC
    10 RANDOMIZE USR #{music[:demo]}
  9998 STOP: GO TO 10
  9999 CLEAR #{music.org-1}: LOAD ""CODE: RUN
  EOC
  puts program.to_source escape_keywords: true
  program.save_tap "music", line: 9999
  music.save_tap "music", append: true
  puts "TAP: music.tap:"
  Z80::TAP.parse_file('music.tap') do |hb|
      puts hb.to_s
  end
end