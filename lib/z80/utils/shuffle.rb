# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'

module Z80
    module Utils
        ##
        # =Z80::Utils::Shuffle
        #
        # A routine to efficiently shuffle bytes in Z80::Utils::Shuffle::Macros
        class Shuffle
            ##
            # =Z80::Utils::Shuffle Macros
            module Macros
                ##
                # Shuffles an array of bytes.
                #
                #   for i from 0 to length − 1 do
                #       j ← random integer such that 0 ≤ j ≤ i
                #       if j ≠ i
                #           target[i] ← target[j]
                #       target[j] ← source[i]
                #
                # After the shuffle is performed +hl+ points to the memory address immediately
                # following the shuffled table.
                #
                # Modifies: +af+, +bc+, +de+, +hl+.
                #
                # +next_rng+:: An address of a random number generator routine. The routine should return
                #              an 8bit random number in the +accumulator+. If +next_rng+ is +nil+
                #              the block of code with the RNG routine is excpected instead.
                #
                # Options:
                # * +target+:: An address of the target array as a label, a pointer or +hl+.
                # * +length+:: An 8bit length of an array in the range of 1..256 (0 is 256) as a label,
                #              pointer or a register.
                # * +source+:: A +source+ function. If +nil+ then identity is assumed: <tt>(i) => i</tt>,
                #              otherwise it should be an address of a source function routine which expects
                #              an argument +i+ in the register +c+ and <b>MUST PRESERVE</b> registers: +hl+ and +de+.
                #              Function is expected to return its result in the +accumulator+.
                def shuffle_bytes_source_max256(next_rng=nil, target:hl, length:a, source:nil, &next_rng_blk)
                    unless source.nil? or (address?(source) and !pointer?(source))
                        raise ArgumentError, "source should be nil or an address" 
                    end
                    unless next_rng.nil? or (address?(next_rng) and !pointer?(next_rng))
                        raise ArgumentError, "next_rng should be an address"
                    end
                    unless next_rng or block_given?
                        raise ArgumentError, "next_rng is not specified and there is no block given"
                    end
                    i = d
                    mask = e
                    j = b
                    t = c
                    isolate do
                                    ld   hl, target unless target == hl
                        unless length == a
                            if immediate?(length) && (length.to_i & 0xFF).zero?
                                    xor  a
                            else
                                    ld   a, length
                            end
                        end
                                    ld   i|mask, 0
                        loop0       push af             # save length
                                    ld   a, mask
                                    ora  i              # make mask from i
                                    ld   mask, a
                                    push hl

                        repeat_rand push i|mask
                        if next_rng
                                    call next_rng       # a = random
                        else
                                    ns(&next_rng_blk)
                        end
                                    pop  i|mask         # i|mask
                                    anda mask
                                    ld   j, a           # j = random & mask
                                    ld   a, i
                                    sub  j              # i - j
                                    jr   C, repeat_rand # j > i

                                    pop  hl             # restore target
                                    push i|mask         # save i|mask

                                    ld   t, i           # t = i

                                    ld16 de, hl         # de: hl
                                    jr   Z, skip_mov    # j == i
                                    ld   j, a           # if j ≠ i
                                    sub_from j, d, e    # de: hl - (i - j)
                                    ld   a, [de]        # target[j]
                                    ld   [hl], a        # target[i] = target[j]
                        skip_mov    label
                        if source.nil?
                                    ld   a, t
                        else
                                    call source
                        end
                                    ld   [de], a        # target[j] = source[i]
                                    pop  i|mask         # restore i|mask
                                    pop  af             # length
                                    inc  hl             # target++
                                    inc  i
                                    cp   i
                                    jr   NZ, loop0
                    end
                end
            end

            include Z80
        end
    end
end

# DEPRECATED
Z80Shuffle = Z80::Utils::Shuffle # :nodoc:

if __FILE__ == $0
    require 'zxlib/basic'
    require 'zxlib/sys'
    # :stopdoc:
    class TestShuffle # :nodoc: all
        include Z80
        include Z80::TAP

        macro_import Utils::Shuffle
        macro_import MathInt
        label_import ZXLib::Sys

        start       ld   hl, testarea
                    ld   a, 256
                    shuffle_bytes_source_max256 next_rng, target:hl, length:a, source:nil
                    ld   hl, mem.attrs
                    ld   a, 256
                    ld   ix, identity
                    call shuffle_it
                    ld16 bc, hl
                    ret

        identity    ld   a, c   # i = i
                    ret

        next_rng    ld   hl, [vars.seed]
                    rnd
                    ld   [vars.seed], hl
                    ld   a, l
                    ret

        forward_ix  jp   (ix)

        shuffle_it  label
                    shuffle_bytes_source_max256 target:hl, length:a, source:forward_ix do
                        call next_rng
                    end
        shuffle_end ret

        testarea    label
    end

    include ZXLib

    testshuffle = TestShuffle.new 0xe000
    testarea = testshuffle[:testarea]
    mem_attrs = Sys.new['mem.attrs']
    program = Basic.parse_source <<-END
      10 CLS: FOR i=0 TO 255: PRINT "`#6`";: POKE #{mem_attrs}+i, i: NEXT i
         PRINT #1;"press any key...";: PAUSE 0: INPUT ""
      20 RANDOMIZE : RANDOMIZE USR #{testshuffle.org}
      30 LET y=80
         FOR i=#{testarea} TO #{testarea+255}
         LET x=PEEK i: IF POINT (x,y) THEN PRINT "Error: duplicate at: ";i-#{testarea};" x=";x: GO TO 9998
         PLOT x,y: DRAW 0,7
         NEXT i
      40 PRINT "OK"
    9998 STOP
    9999 CLEAR #{testshuffle.org - 1}: LOAD ""CODE: RUN
    END
    puts testshuffle.debug
    puts program.to_source escape_keywords:true
    program.save_tap 'testshuffle.tap', line:9999
    puts "shuffle length: #{testshuffle[:shuffle_end] - testshuffle[:shuffle_it]}"
    testshuffle.save_tap 'testshuffle.tap', append:true
end
