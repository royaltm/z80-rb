# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'
require 'zxlib/sys'

class Z80Shuffle
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
        # Modifies: +af+, +bc+, +de+, +hl+
        #
        # +next_rng+:: Address of random number generator routine;
        #              it should return a 8bit next random number in a +l+ register.
        # +target+:: An address or a pointer to a target array; may be +hl+.
        # +length+:: A 8bit length of an array in the range of 1..256 (0 is 256); may be a register.
        # +source+:: If +nil+ then <tt>source(i) = i</tt> is assumed, otherwise an address of a source routine
        #            which should expect +i+ in register +c+ and <b>MUST PRESERVE</b> +hl+ and +de+ registers;
        #            source routine should return value in the +a+ register.
        def shuffle_bytes_source_max256(next_rng, target:hl, length:a, source:nil)
            unless source.nil? or (address?(source) and !pointer?(source))
                raise ArgumentError, "source should be nil or an address" 
            end
            unless address?(next_rng) and !pointer?(next_rng)
                raise ArgumentError, "next_rng should be an address"
            end
            i = d
            mask = e
            j = b
            t = c
            isolate do
                            ld   hl, target unless hl == target
                            ld   a, length unless a == length
                            ld   i|mask, 0
                loop0       push af             # save length
                            ld   a, mask
                            ora  i              # make mask from i
                            ld   mask, a
                            push hl
                repeat_rand push i|mask
                            call next_rng
                            ld   a, l           # j = random
                            pop  i|mask         # i|mask
                            anda mask
                            ld   j, a
                            ld   a, i
                            sub  j              # i - j
                            jr   C, repeat_rand # j > i
                            pop  hl             # restore target
                            push i|mask         # save i|mask
                            ld   t, i
                            ld16 de, hl
                            jr   Z, skip_mov    # j == i
                            ld   j, a           # if j ≠ i
                            sub_from j, d, e
                            ld   a, [de]        # target[j]
                            ld   [hl], a        # target[i] = target[j]
                skip_mov    label
                if source.nil?
                            ld   [de], t        # target[j] = source[i]
                else
                            call source
                            ld   [de], a
                end
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

if __FILE__ == $0
    require 'zxlib/basic'
    # :stopdoc:
    class TestShuffle # :nodoc: all
        include Z80
        include Z80::TAP

        macro_import Z80Shuffle
        macro_import Z80MathInt
        label_import ZXSys

        start       ld   hl, testarea
                    ld   a, 256
                    ld   ix, identity
                    call shuffle_it
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
                    ret

        forward_ix  jp   (ix)

        shuffle_it  shuffle_bytes_source_max256 next_rng, target:hl, length:a, source:forward_ix
        shuffle_end ret

        testarea    label
    end

    testshuffle = TestShuffle.new 0xe000
    testarea = testshuffle[:testarea]
    mem_attrs = ZXSys.new['mem.attrs']
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
