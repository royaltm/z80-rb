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
        # +next_rng+:: Address of random number generator routine;
        #              it should return a 8bit next random number in a +l+ register.
        # +target+:: An address or a pointer to a target array; may be +hl+.
        # +length+:: A 8bit length of an array in the range of 1..256 (0 is 256); may be a register.
        # +source+:: If +nil+ then <tt>source(i) = i</tt> is assumed, otherwise an address of a source routine
        #            which should expect +i+ in register +d+ and <b>MUST PRESERVE</b> +bc+ and +de+ registers;
        #            source routine should return value in register +a+.
        def shuffle_bytes_source_max256(next_rng, target:hl, length:a, source:nil)
            unless source.nil? or (address?(source) and !pointer?(source))
                raise ArgumentError, "source should be nil or an address" 
            end
            unless address?(next_rng) and !pointer?(next_rng)
                raise ArgumentError, "next_rng should be an address"
            end
            j = c
            i = d
            mask = e
            isolate do
                            ld   hl, target unless hl == target
                            ld   a, length unless a == length
                            ld   i|mask, 0
                loop0       push af
                            ld   a, i
                            ora  mask
                            ld   mask, a
                repeat_rand push i|mask
                            push hl
                            call next_rng
                            ld   a, l     # j = random
                            pop  hl       # target
                            pop  i|mask   # i|mask
                            anda mask
                            ld   j, a
                            ld   a, i
                            cp   j        # i - j
                            jr   C, repeat_rand # j > i
                            push hl
                            ld   a, j
                            ld16 b|j, hl
                            adda_to b, j  # target[j]
                            ld   a, i
                            adda_to h, l  # target[i]
                            ld   a, [b|j]
                            ld   [hl], a  # target[i] = target[j]
                if source.nil?
                            ld   [b|j], i # target[j] = source[i]
                else
                            call source
                            ld   [b|j], a
                end
                            pop  hl
                            pop  af       # n
                            inc  i
                            cp   i
                            jr   NZ, loop0
            end
        end
    end
    include Z80
end

if __FILE__ == $0
    # :stopdoc:
    class TestShuffle # :nodoc: all
        include Z80
        include Z80::TAP

        macro_import Z80Shuffle
        macro_import Z80MathInt
        label_import ZXSys

        start       ld   hl, mem.screen
                    ld   a, 256
                    ld   ix, identity
                    call shuffle_it
                    ld   hl, mem.attrs
                    ld   ix, to_attr
                    ld   a, 15
                    call shuffle_it
                    ret

        ns :to_attr do
                    ld   a, d
                    cp   8
                    jr   C, skip # i < 8 ? i
                    sub  7       # i - 7
                    ora  0x08    # | 0x08
            skip    label
                    3.times { rlca }
                    ret
        end

        identity    ld   a, d   # i = i
                    ret

        next_rng    ld   hl, [vars.seed]
                    rnd
                    ld   [vars.seed], hl
                    ret

        forward_ix  jp   (ix)

        shuffle_it  shuffle_bytes_source_max256 next_rng, target:hl, length:a, source:forward_ix
        shuffle_end ret

    end

    p = TestShuffle.new 0x8000
    puts p.debug
    puts "shuffle length: #{p[:shuffle_end] - p[:shuffle_it]}"
    p.save_tap 'testshuffle.tap'
end
