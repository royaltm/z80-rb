# -*- coding: BINARY -*-
require 'z80'

module Z80
    module Utils
        ##
        # =Z80::Utils::VecDeque.
        #
        # Routines for appending, removing and iterating byte elements from double ended queues.
        #
        # See Z80::Utils::VecDeque::Macros.
        #
        # ==Structs
        #
        # Z80::Utils::VecDeque::VecDequeState.
        class VecDeque
            include Z80

            ## A descriptor type for a double ended queue.
            class VecDequeState < Label
                head          word
                tail          word
                head_bf_tail  byte
            end
            ##
            # =Z80::Utils::VecDeque
            #
            # Macros producing routines for working with double ended queues.
            module Macros
                ##
                # Creates a routine that initializes or clears the queue.
                #
                # +vec_deque+:: A label of a type VecDequeState addressing the queue descriptor.
                #
                # Options:
                # * +vec_deque_bot+:: An address, a pointer to memory holding an address or +hl+ register holding 
                #                     an address of the first byte of memory holding the queue values.
                #
                # Modifies: +hl+.
                def vec_deque_clear(vec_deque, vec_deque_bot:)
                    raise ArgumentError unless address?(vec_deque) and !pointer?(vec_deque) and
                                                (vec_deque_bot == hl or address?(vec_deque_bot))
                    isolate do
                                    ld   hl, vec_deque_bot unless vec_deque_bot == hl
                                    ld   [vec_deque.head], hl
                                    ld   [vec_deque.tail], hl
                                    ld   hl, vec_deque.head_bf_tail
                                    ld   [hl], 0
                    end
                end
                ##
                # Creates a routine that checks if the queue is full.
                #
                # In case +branch_not_full+ is +nil+ the +Z+ flag, if set, indicates the queue is full.
                #
                # +vec_deque+:: A label of a type VecDequeState addressing the queue descriptor.
                #
                # Options:
                # * +branch_not_full+:: A target address or a label where to jump to when the queue is NOT full.
                # * +branch_relative+:: If true relative jumps are being used for branching.
                # * +tt+:: A temporary 16-bit register +bc+ or +de+.
                #
                # Modifies: +af+, +hl+, +tt+.
                def vec_deque_full?(vec_deque, branch_not_full: nil, branch_relative: true, tt: de)
                    raise ArgumentError unless address?(vec_deque) and !pointer?(vec_deque) and [bc, de].include?(tt) and
                                                (branch_not_full.nil? or (address?(branch_not_full) and !pointer?(branch_not_full)))
                    isolate do |eoc|
                                    ld   hl, [vec_deque.head]
                                    ld   tt, [vec_deque.tail]
                                    ora  a
                                    sbc  hl, tt
                        if branch_relative
                                    jr   NZ, branch_not_full || eoc
                        else
                                    jp   NZ, branch_not_full || eoc
                        end
                                    ld   a, [vec_deque.head_bf_tail]
                        if branch_not_full
                                    anda a
                            if branch_relative
                                    jr   Z, branch_not_full
                            else
                                    jp   Z, branch_not_full
                            end
                        else
                                    xor  -1
                        end
                    end
                end
                ##
                # Creates a routine that checks if the queue is empty.
                #
                # In case +branch_not_full+ is +nil+ the +Z+ flag, if set, indicates the queue is empty.
                #
                # +vec_deque+:: A label of a type VecDequeState addressing the queue descriptor.
                #
                # Options:
                # * +branch_not_empty+:: A target address or a label where to jump to when the queue is NOT empty.
                # * +branch_relative+:: If +true+ relative jumps are being used for branching.
                # * +tt+:: A temporary 16-bit register +bc+ or +de+.
                #
                # Modifies: +af+, +hl+, +tt+.
                def vec_deque_empty?(vec_deque, branch_not_empty: nil, branch_relative: true, tt: de)
                    raise ArgumentError unless address?(vec_deque) and !pointer?(vec_deque) and [bc, de].include?(tt) and
                                                (branch_not_empty.nil? or (address?(branch_not_empty) and !pointer?(branch_not_empty)))
                    isolate do |eoc|
                                    ld   hl, [vec_deque.head]
                                    ld   tt, [vec_deque.tail]
                                    ora  a
                                    sbc  hl, tt
                        if branch_relative
                                    jr   NZ, branch_not_empty || eoc
                        else
                                    jp   NZ, branch_not_empty || eoc
                        end
                                    ld   a, [vec_deque.head_bf_tail]
                                    anda a
                        if branch_not_empty
                            if branch_relative
                                    jr   NZ, branch_not_empty
                            else
                                    jp   NZ, branch_not_empty
                            end
                        end
                    end
                end
                ##
                # Creates a routine that calculates the current length of the queue.
                #
                # The length is made available as a 16-bit integer in the +hl+ register.
                #
                # +vec_deque+:: A label of a type VecDequeState addressing the queue descriptor.
                #
                # Options:
                # * +vec_deque_bot+:: An address or a pointer to memory holding an address of the first
                #                     byte of memory holding the queue values.
                # * +vec_deque_top+:: An address or a pointer to memory holding an address immediately
                #                     succeeding the last byte of memory holding the queue values.
                # * +tt+:: A temporary 16-bit register +bc+ or +de+.
                # * +subroutine+:: Pass +true+ for a subroutine.
                #
                # Modifies: +af+, +hl+, +tt+.
                def vec_deque_length(vec_deque, vec_deque_bot:, vec_deque_top:, tt: de, subroutine: false)
                    raise ArgumentError unless address?(vec_deque) and !pointer?(vec_deque) and [bc, de].include?(tt) and
                                                address?(vec_deque_bot) and address?(vec_deque_top)
                    isolate do |eoc|
                                            ld   hl, [vec_deque.head]
                                            ld   tt, [vec_deque.tail]
                                            ora  a
                                            sbc  hl, tt
                                            jr   C, head_before_tail
                        if subroutine
                                            ret  NZ
                        else
                                            jr   NZ, eoc
                        end
                                            ld   a, [vec_deque.head_bf_tail]
                                            anda a
                        if subroutine
                                            ret  Z
                        else
                                            jr   Z, eoc
                        end
                                            ld   hl, 0
                        head_before_tail    ld   tt, vec_deque_top
                                            add  hl, tt
                                            ld   tt, vec_deque_bot
                                            ora  a
                                            sbc  hl, tt
                                            ret if subroutine
                    end
                end
                ##
                # Creates a routine that appends a byte element to the back of the queue.
                #
                # +accumulator+:: Should hold a value to be pushed to the queue.
                # +vec_deque+:: A label of a type VecDequeState addressing the queue descriptor.
                #
                # Options:
                # * +vec_deque_bot+:: An address or a pointer to memory holding an address of the first
                #                     byte of memory holding the queue values.
                # * +vec_deque_top+:: An address or a pointer to memory holding an address immediately
                #                     succeeding the last byte of memory holding the queue values.
                # * +branch_on_full+:: A target address or a label where to jump to when the queue IS full.
                # * +branch_relative+:: If +true+ relative jumps are being used for branching.
                # * +tt+:: A temporary 16-bit register +bc+ or +de+.
                #
                # _NOTE_:: If +branch_on_full+ is +nil+ then the check is NOT being performed and the routine
                #          created assumes there is enough space for another element in the queue.
                #
                # Modifies: +f+, +hl+, +tt+.
                def vec_deque_push_back(vec_deque, vec_deque_bot:, vec_deque_top:, branch_on_full: nil, branch_relative: true, tt: de)
                    raise ArgumentError unless address?(vec_deque) and !pointer?(vec_deque) and [bc, de].include?(tt) and
                                                address?(vec_deque_bot) and address?(vec_deque_top)
                    unless branch_on_full.nil? or (address?(branch_on_full) and !pointer?(branch_on_full))
                        raise ArgumentError, "branch_on_full should be a direct address"
                    end
                    isolate do
                                        ld   tt, [vec_deque.head]
                        if branch_on_full
                                        ld   hl, [vec_deque.tail]
                                        ora  a
                                        sbc  hl, tt
                                        jp   NZ, push_value
                                        ld   hl, vec_deque.head_bf_tail
                                        bit  0, [hl]
                            if branch_relative
                                        jr   NZ, branch_on_full
                            else
                                        jp   NZ, branch_on_full
                            end
                        end
                        push_value      ld   [tt], a
                                        inc  tt
                                        ld   hl, vec_deque_top
                                        ora  a
                                        sbc  hl, tt
                                        jp   NZ, skip_wrap
                                        ld   hl, vec_deque.head_bf_tail
                                        ld   [hl], -1
                                        ld   tt, vec_deque_bot
                        skip_wrap       ld   [vec_deque.head], tt
                    end
                end
                ##
                # Creates a routine that removes a byte element from the back of the queue.
                #
                # The removed element is provided in +accumulator+.
                #
                # +vec_deque+:: A label of a type VecDequeState addressing the queue descriptor.
                #
                # Options:
                # * +vec_deque_bot+:: An address or a pointer to memory holding an address of the first
                #                     byte of memory holding the queue values.
                # * +vec_deque_top+:: An address or a pointer to memory holding an address immediately
                #                     succeeding the last byte of memory holding the queue values.
                # * +branch_on_empty+:: A target address or a label where to jump to when the queue IS empty.
                # * +branch_relative+:: If +true+ relative jumps are being used for branching.
                # * +tt+:: A temporary 16-bit register +bc+ or +de+.
                #
                # _NOTE_:: If +branch_on_empty+ is +nil+ then the check is NOT being performed and the routine
                #          created assumes there is at least one element available in the queue.
                #
                # Modifies: +af+, +hl+, +tt+.
                def vec_deque_pop_back(vec_deque, vec_deque_bot:, vec_deque_top:, branch_on_empty: nil, branch_relative: true, tt: de)
                    raise ArgumentError unless address?(vec_deque) and !pointer?(vec_deque) and [bc, de].include?(tt) and
                                                address?(vec_deque_bot) and address?(vec_deque_top)
                    unless branch_on_empty.nil? or (address?(branch_on_empty) and !pointer?(branch_on_empty))
                        raise ArgumentError, "branch_on_empty should be an address"
                    end
                    isolate do
                                    ld   tt, [vec_deque.head]
                        if branch_on_empty
                                    ld   hl, [vec_deque.tail]
                                    ora  a
                                    sbc  hl, tt
                                    jp   NZ, pop_value
                                    ld   hl, vec_deque.head_bf_tail
                                    bit  0, [hl]
                            if branch_relative
                                    jr   Z, branch_on_empty
                            else
                                    jp   Z, branch_on_empty
                            end
                        end
                        pop_value   ld   hl, vec_deque_bot
                                    ora  a
                                    sbc  hl, tt
                                    jp   NZ, skip_wrap
                                    ld   hl, vec_deque.head_bf_tail
                                    ld   [hl], 0
                                    ld   tt, vec_deque_top
                        skip_wrap   dec  tt
                                    ld   a, [tt]
                                    ld   [vec_deque.head], tt
                    end
                end
                ##
                # Creates a routine that appends a byte element to the front of the queue.
                #
                # +accumulator+:: Should hold a value to be pushed to the queue.
                # +vec_deque+:: A label of a type VecDequeState addressing the queue descriptor.
                #
                # Options:
                # * +vec_deque_bot+:: An address or a pointer to memory holding an address of the first
                #                     byte of memory holding the queue values.
                # * +vec_deque_top+:: An address or a pointer to memory holding an address immediately
                #                     succeeding the last byte of memory holding the queue values.
                # * +branch_on_full+:: A target address or a label where to jump to when the queue IS full.
                # * +branch_relative+:: If +true+ relative jumps are being used for branching.
                # * +tt+:: A temporary 16-bit register +bc+ or +de+.
                #
                # _NOTE_:: If +branch_on_full+ is +nil+ then the check is NOT being performed and the routine
                #          created assumes there is enough space for another element in the queue.
                #
                # Modifies: +f+, +hl+, +tt+.
                def vec_deque_push_front(vec_deque, vec_deque_bot:, vec_deque_top:, branch_on_full: nil, branch_relative: true, tt: de)
                    raise ArgumentError unless address?(vec_deque) and !pointer?(vec_deque) and [bc, de].include?(tt) and
                                                address?(vec_deque_bot) and address?(vec_deque_top)
                    unless branch_on_full.nil? or (address?(branch_on_full) and !pointer?(branch_on_full))
                        raise ArgumentError, "branch_on_full should be a direct address"
                    end
                    isolate do
                                    ld   tt, [vec_deque.tail]
                        if branch_on_full
                                    ld   hl, [vec_deque.head]
                                    ora  a
                                    sbc  hl, tt
                                    jp   NZ, push_value
                                    ld   hl, vec_deque.head_bf_tail
                                    bit  0, [hl]
                            if branch_relative
                                    jr   NZ, branch_on_full
                            else
                                    jp   NZ, branch_on_full
                            end
                        end
                        push_value  ld   hl, vec_deque_bot
                                    ora  a
                                    sbc  hl, tt
                                    jr   NZ, skip_wrap
                                    ld   hl, vec_deque.head_bf_tail
                                    ld   [hl], -1
                                    ld   tt, vec_deque_top
                        skip_wrap   dec  tt
                                    ld   [tt], a
                                    ld   [vec_deque.tail], tt
                    end
                end
                ##
                # Creates a routine that removes a byte element from the front of the queue.
                #
                # The removed element is provided in +accumulator+.
                #
                # +vec_deque+:: A label of a type VecDequeState addressing the queue descriptor.
                #
                # Options:
                # * +vec_deque_bot+:: An address or a pointer to memory holding an address of the first
                #                     byte of memory holding the queue values.
                # * +vec_deque_top+:: An address or a pointer to memory holding an address immediately
                #                     succeeding the last byte of memory holding the queue values.
                # * +branch_on_empty+:: A target address or a label where to jump to when the queue IS empty.
                # * +branch_relative+:: If +true+ relative jumps are being used for branching.
                # * +tt+:: A temporary 16-bit register +bc+ or +de+.
                #
                # _NOTE_:: If +branch_on_empty+ is +nil+ then the check is NOT being performed and the routine
                #          created assumes there is at least one element available in the queue.
                #
                # Modifies: +af+, +hl+, +tt+.
                def vec_deque_pop_front(vec_deque, vec_deque_bot:, vec_deque_top:, branch_on_empty: nil, branch_relative: true, tt: de)
                    raise ArgumentError unless address?(vec_deque) and !pointer?(vec_deque) and [bc, de].include?(tt) and
                                                address?(vec_deque_bot) and address?(vec_deque_top)
                    unless branch_on_empty.nil? or (address?(branch_on_empty) and !pointer?(branch_on_empty))
                        raise ArgumentError, "branch_on_empty should be an address"
                    end
                    isolate do
                                    ld   tt, [vec_deque.tail]
                        if branch_on_empty
                                    ld   hl, [vec_deque.head]
                                    ora  a
                                    sbc  hl, tt
                                    jp   NZ, pop_value
                                    ld   hl, vec_deque.head_bf_tail
                                    bit  0, [hl]
                            if branch_relative
                                    jr   Z, branch_on_empty
                            else
                                    jp   Z, branch_on_empty
                            end
                        end
                        pop_value   ld   a, [tt]
                                    inc  tt
                                    ld   hl, vec_deque_top
                                    ora  a
                                    sbc  hl, tt
                                    jr   NZ, skip_wrap
                                    ld   hl, vec_deque.head_bf_tail
                                    ld   [hl], 0
                                    ld   tt, vec_deque_bot
                        skip_wrap   ld   [vec_deque.tail], tt
                    end
                end
                ##
                # Creates a routine that reads a byte element from the front of the queue advancing the cursor forward.
                #
                # The removed element is provided in +accumulator+.
                #
                # +vec_deque+:: An optional label of a type VecDequeState addressing the queue descriptor.
                #               If provided the cursor is set to the front of the queue first.
                #               In this instance it's possible to skip this part by jumping to +get_next+
                #               sublabel address of the routine being created.
                #
                # Options:
                # * +vec_deque_bot+:: An address or a pointer to memory holding an address of the first
                #                     byte of memory holding the queue values.
                # * +vec_deque_top+:: An address or a pointer to memory holding an address immediately
                #                     succeeding the last byte of memory holding the queue values.
                # * +cursor+:: A 16-bit register +bc+ or +de+ holding the cursor position.
                # * +subroutine+:: Pass +true+ for a subroutine.
                #
                # Modifies: +af+, +hl+, +tt+.
                def vec_deque_next_front(vec_deque=nil, vec_deque_bot:, vec_deque_top:, cursor: de, subroutine: false)
                    raise ArgumentError unless (vec_deque.nil? or (address?(vec_deque) and !pointer?(vec_deque))) and
                                        address?(vec_deque_bot) and address?(vec_deque_top) and [bc, de].include?(cursor)
                    isolate do |eoc|
                                    ld   cursor, [vec_deque.tail] unless vec_deque.nil?
                        get_next    ld   a, [cursor]
                                    inc  cursor
                                    ld   hl, vec_deque_top
                                    ora  a
                                    sbc  hl, cursor
                            if subroutine
                                    ret  NZ
                            else
                                    jr   NZ, eoc
                            end
                                    ld   cursor, vec_deque_bot
                                    ret if subroutine
                    end
                end
                ##
                # Creates a routine that reads a byte element from the back of the queue advancing the cursor backwards.
                #
                # The removed element is provided in +accumulator+.
                #
                # +vec_deque+:: An optional label of a type VecDequeState addressing the queue descriptor.
                #               If provided the cursor is set to the back of the queue first.
                #               In this instance it's possible to skip this part by jumping to +get_next+
                #               sublabel address of the routine being created.
                #
                # Options:
                # * +vec_deque_bot+:: An address or a pointer to memory holding an address of the first
                #                     byte of memory holding the queue values.
                # * +vec_deque_top+:: An address or a pointer to memory holding an address immediately
                #                     succeeding the last byte of memory holding the queue values.
                # * +cursor+:: A 16-bit register +bc+ or +de+ holding the cursor position.
                # * +subroutine+:: Pass +true+ for a subroutine.
                #
                # Modifies: +af+, +hl+, +tt+.
                def vec_deque_next_back(vec_deque=nil, vec_deque_bot:, vec_deque_top:, cursor: de, subroutine: false)
                    raise ArgumentError unless (vec_deque.nil? or (address?(vec_deque) and !pointer?(vec_deque))) and
                                        address?(vec_deque_bot) and address?(vec_deque_top) and [bc, de].include?(cursor)
                    isolate do |eoc|
                                    ld   cursor, [vec_deque.head] unless vec_deque.nil?
                        get_next    ld   hl, vec_deque_bot
                                    ora  a
                                    sbc  hl, cursor
                                    jp   NZ, skip_wrap
                                    ld   cursor, vec_deque_top
                        skip_wrap   dec  cursor
                                    ld   a, [cursor]
                                    ret if subroutine
                    end
                end
            end
        end
    end
end

if __FILE__ == $0
    require 'z80/math_i'
    require 'z80/stdlib'
    require 'zxlib/sys'
    require 'zxlib/gfx'
    require 'zxlib/basic'
    # :stopdoc:
    class Snake # :nodoc: all
        include Z80
        include Z80::Utils
        include Z80::TAP

        macro_import MathInt
        macro_import Stdlib
        macro_import ZXLib::Gfx
        label_import ZXLib::Sys, macros: true
        macro_import VecDeque

        VecDequeState = VecDeque::VecDequeState
        Coords = ZXLib::Sys::Coords

        SnakeLength = 40

        ns :start do
                            call prepare_screen
                            ld   hl, [vars.udg]
                            ld   [snake_top], hl
                            ld   de, -SnakeLength*3
                            add  hl, de
                            ld   [snake_bot], hl
                            vec_deque_clear snake, vec_deque_bot: hl
                            call random_dir
            try_again       call randomize
                            ld   a, l
                            anda 31
                            ld   l, a
                            ld   a, h
                            anda 31
                            cp   24
                            jr   NC, try_again 
                            ld   h, a
                            ld   [cursor], hl
                            call release_key
        end

        ns :forward do
                            vec_deque_full? snake, branch_not_full: skip_pop
                            call snake_pop_front # x
                            ld   c, a
                            call snake_pop_front # y
                            ld   b, a
                            call snake_pop_front # color
                            call paint
            skip_pop        call random_turn
                            ld   a, d
                            ex   af, af
                            ld   bc, [cursor]
                            ld   a, c
                            call snake_push_back # x
                            ld   a, b
                            call snake_push_back # y
                            ex   af, af
                            call snake_push_back # color
                            call paint
                            call move
                            2.times { halt }
                            key_pressed?
                            jr   Z, forward
                            call release_key
                            call snake_next_front # x
                            ld   c, a
                            call snake_next_front.get_next # y
                            ld   b, a
                            ld   [cursor], bc
                            call random_dir
                            call move
        end

        ns :backward do
                            vec_deque_full? snake, branch_not_full: skip_pop
                            call snake_pop_back # color
                            ex   af, af
                            call snake_pop_back # y
                            ld   b, a
                            call snake_pop_back # x
                            ld   c, a
                            ex   af, af
                            call paint
            skip_pop        call random_turn
                            ld   a, d
                            call snake_push_front # color
                            ld   bc, [cursor]
                            call paint
                            ld   a, b
                            call snake_push_front # y
                            ld   a, c
                            call snake_push_front # x
                            call move
                            2.times { halt }
                            key_pressed?
                            jr   Z, backward
                            call release_key
                            call snake_next_back # color
                            call snake_next_back.get_next # y
                            ld   b, a
                            call snake_next_back.get_next # x
                            ld   c, a
                            ld   [cursor], bc
                            call random_dir
                            call move
                            jp   forward
        end

        ns :release_key do
                            halt
                            key_pressed?
                            ret  Z
                            call rom.break_key
                            jr   C, release_key
                            pop  af
                            call snake_length
                            ld16 bc, hl
                            ret
        end

        ns :snake_pop_front do
                            vec_deque_pop_front snake, vec_deque_bot: [snake_bot], vec_deque_top: [snake_top]
                            ret
        end

        ns :snake_pop_back do
                            vec_deque_pop_back snake, vec_deque_bot: [snake_bot], vec_deque_top: [snake_top]
                            ret
        end

        ns :snake_push_front do
                            vec_deque_push_front snake, vec_deque_bot: [snake_bot], vec_deque_top: [snake_top]
                            ret
        end

        ns :snake_push_back do
                            vec_deque_push_back snake, vec_deque_bot: [snake_bot], vec_deque_top: [snake_top]
                            ret
        end

        snake_next_front    vec_deque_next_front snake, vec_deque_bot: [snake_bot], vec_deque_top: [snake_top], subroutine: true
        snake_next_back     vec_deque_next_back snake, vec_deque_bot: [snake_bot], vec_deque_top: [snake_top], subroutine: true

        snake_length        vec_deque_length snake, vec_deque_bot: [snake_bot], vec_deque_top: [snake_top], subroutine: true

        ns :randomize do
                            ld   hl, [vars.seed]
                            rnd
                            ld   [vars.seed], hl
                            ret
        end

        ns :random_dir do
                            call randomize
                            ld   a, l
                            anda 3
                            ld   [cur_dir], a
                            ret
        end

        # > DE: random
        ns :random_turn do
                            call randomize
                            ex   de, hl
                            ld   a, e
                            anda 7
                            cp   3
                            ret  NC
                            dec  a
                            ld   hl, cur_dir
                            add  [hl]
                            anda 3
                            ld   [hl], a
                            ret
        end

        ns :move do
                            ld   a, [cur_dir]
                            add  a, a
                            ld   hl, dirs
                            adda_to h, l
                            ld   a, [cursor.x]
                            add  [hl]
                            anda 31
                            ld   [cursor.x], a
                            inc  hl
                            ld   a, [cursor.y]
                            add  [hl]
                            cp   -1
                            jr   NZ, check_max
                            ld   a, 23
            check_max       cp   24
                            jr   C, skip_wrap
                            xor  a
            skip_wrap       ld   [cursor.y], a
                            ret
        end

        # < A: color
        # < BC: cursor
        ns :paint do
                            ex   af, af
                            rctoattr b, c
                            ex   af, af
                            xor  [hl]
                            ld   [hl], a
                            ret
        end

        ns :prepare_screen do
                            clrmem mem.attrs, mem.attrlen, 0b00111111
                            ld   a, 0b01010101
                            ld   hl, mem.screen
                            ld   c, 24
            cloop           ld   b, 256
            clr_loop        ld   [hl], a
                            inc  l
                            djnz clr_loop
                            inc  h
                            cpl
                            dec  c
                            jr   NZ, cloop
                            ret
        end

        snake               data VecDequeState
        snake_top           dw  0
        snake_bot           dw  0
        cur_dir             db  0
        cursor              data Coords
        dirs                data Coords, [0, -1], [1, 0], [0, 1], [-1, 0]
    end

    snake = Snake.new 0xC000

    puts snake.debug

    %w[
        start
    ].each do |label|
        puts "#{label.ljust(20)}: 0x#{snake[label].to_s 16} - #{snake[label]}, size: #{snake.code.bytesize}"
    end

    program = ZXLib::Basic.parse_source <<-END
        10 BORDER 0: RANDOMIZE: PRINT USR #{snake[:start]}
        20 STOP
    9998 RUN
    9999 CLEAR #{snake.org-1}: LOAD "snake"CODE: RUN
    END
    program.start = 9999
    puts program.to_source

    program.save_tap 'snake'
    snake.save_tap 'snake', append: true

    Z80::TAP.parse_file('snake.tap') do |hb|
            puts hb.to_s
    end
end
