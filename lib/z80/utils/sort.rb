# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'

module Z80
    module Utils
        ##
        # =Z80::Utils::Sort
        #
        # Implementations of various sorting algorithms in Z80::Utils::Sort::Macros
        #
        # Example performance results (in T-states):
        #
        #   Array size: 5 elements             10 elements
        #                  SSort  ISort  QSort    SSort  ISort  QSort   Example SEQ
        #   best             733    215    846     2478    440   2326
        #   1-periodic       733    215   1380     2478    440   3665   (1,1,1,1,1)
        #   shuffled (avg)   770    614   1450     2601   1936   3583   (3,4,2,5,1)
        #   random   (avg)   770    607   1459     2598   1922   3593   (7,2,9,2,4)
        #   ordered          733    215   1076     2478    440   2326   (1,2,3,4,5)
        #   reversed         793    894   1248     2728   2989   3057   (5,4,3,2,1)
        #   worst            793    894   1770     2728   2989   4326
        #   
        #   Array size: 20 elements            30 elements
        #                  SSort  ISort  QSort    SSort  ISort  QSort   Example SEQ
        #   best            8818    890   5071    18958   1340   6984
        #   1-periodic      8818    890   8885    18958   1340  11065   (1,1,1,1,1)
        #   shuffled (avg)  9173   6298   8423    19593  12967  13712   (3,4,2,5,1)
        #   random   (avg)  9168   6295   8425    19589  12963  13641   (7,2,9,2,4)
        #   ordered         8818    890   5071    18958   1340   6984   (1,2,3,4,5)
        #   reversed        9818  10554   6207    21208  22619   8201   (5,4,3,2,1)
        #   worst           9818  10554  10055    21208  22619  16434
        #   
        #   Array size: 40 elements            50 elements
        #                  SSort  ISort  QSort    SSort  ISort  QSort   Example SEQ
        #   best           32898   1790  11076    50638   2240  14471
        #   1-periodic     32898   1790  20625    50638   2240  23845   (1,1,1,1,1)
        #   shuffled (avg) 33847  21817  19208    51938  33001  24895   (3,4,2,5,1)
        #   random   (avg) 33840  21909  19215    51911  32954  24930   (7,2,9,2,4)
        #   ordered        32898   1790  11076    50638   2240  14471   (1,2,3,4,5)
        #   reversed       36898  39184  12997    56888  60249  16469   (5,4,3,2,1)
        #   worst          36898  39184  22308    56888  60249  29829
        class Sort
            ##
            # =Z80::Utils::Sort macros
            #
            module Macros
                ##
                # Creates a subroutine that sorts an array of bytes using quicksort algorithm.
                #
                #   algorithm qsort(A, first, last) is
                #       pivot ← A[between first and last]
                #       i ← first
                #       j ← last
                #       loop forever
                #           while pivot > A[i]
                #               i ← i + 1
                #           while pivot < A[j]
                #               j ← j - 1
                #           if i ≤ j
                #               A[i] ⇄ A[j]
                #           else
                #               break loop
                #           i ← i + 1
                #           j ← j - 1
                #       if first < j
                #           qsort(A, first, j)
                #       if i < last
                #           qsort(A, i, last)
                #
                # Modifies: +af+, +bc+, +de+, +hl+.
                #
                # The created function is recursive and should be used for arrays larger than 30
                # elements for performance reasons.
                #
                # The routine expects addresses of the first and the last elements of the sorted array.
                # 
                # Depending on the +reverse+ option the registers to hold addresses are:
                #
                #   reverse  first  last  sorting order
                #     false  de     hl    ascending 
                #      true  hl     de    descending
                #
                # Unless +safe_args+ is +true+, if the address of the first item is greater than the address
                # of the last item the routine will resolve in <b>UNDEFINED BEHAVIOUR</b>. Thus the smallest
                # array that such a routine can sort safely is a single item array.
                #
                # * +select_pivot+:: Pivot selection method, see below.
                #
                # Options:
                # * +reverse+:: Should the sorting order be reversed (descending order).
                # * +safe_args+:: Inserts an additional argument check to ensure initial addresses are sane:
                #                 <tt>first ≤ last</tt>.
                #                 In case arguments are not valid (<tt>first > last</tt>), the subroutine
                #                 returns with the Carry Flag set (+C+).
                # * +pivot_reg+:: A register (+c+ or +b+) holding the current pivot value which must be
                #                 preserved when swapping items.
                # * +swap_same+:: Should the algorithm invoke the item swap function when <tt>i = j</tt>.
                #                 If +false+ the additional check is inserted before invoking item swap
                #                 procedure, which may impact the routine's performance.
                #
                # Optionally provide a block that creates a custom inline procedure for swapping items.
                # The procedure should expect items to be swapped at addresses pointed to by +hl+ and +de+
                # registers.
                # The Zero Flag will be set (+Z+) if both addresses are pointing to the same item (+hl+ = +de+).
                # The Carry Flag will be always reset (+NC+).
                #
                # The swap items procedure <b>MUST PRESERVE</b> values of +de+, +hl+ and +pivot_reg+ registers.
                # Additionally if +swap_same+ is +true+, the procedure <b>MUST ALSO PRESERVE</b> the content
                # of the Zero Flag.
                #
                # The +select_pivot+ argument may be one of:
                #
                # * +:half+:: a built-in method that picks the pivot value out of the array somewhere close to
                #             the middle of the sorted range. 
                # * +:first+:: a built-in method that picks the first element as the pivot value.
                # * +:last+:: a built-in method that picks the last element as the pivot value.
                # * +address+:: an address (as a label or an integer) of the routine that will be called to
                #               establish the pivot value.
                # * +proc+:: a procedure that creates the pivot selection algorithm inline.
                #
                # The pivot selection function should expect +hl+ and +de+ registers holding the current range
                # addresses according to the rules described above. The pivot value should be returned in
                # the +pivot_reg+ register. Content of both +hl+ and +de+ registers <b>MUST BE</b> preserved.
                #
                # The selected pivot value should be one of the sorted range elements.
                #
                # The built-in +:first+ and +:last+ strategies are quick but can end up with a poor algorithm
                # performance for sorted or nearly sorted ranges.
                #
                # The built-in +:half+ takes additional 39 to 43 T-states for each recursion but is a fair
                # strategy for many cases.
                #
                # Better pivot selection algorithms can be derived especially if values that are being sorted
                # are well known.
                def quicksort_bytes(select_pivot=:half,
                                    reverse: false, safe_args: true, pivot_reg: c, swap_same: true, &swap_items)
                    raise ArgumentError, "pivot_reg must be either c or b register" unless [c, b].include?(pivot_reg)
                    raise ArgumentError, "reverse should be a boolean" unless [true, false].include?(reverse)
                    temp = if pivot_reg == c then b else c end
                    first, last = if reverse then [hl, de] else [de, hl] end
                    ii, jj = first, last
                    unless block_given?
                        swap_items = proc do
                                    ld   temp, [hl]
                                    ld   a, [de]
                                    ld   [hl], a
                                    ld   a, temp
                                    ld   [de], a
                        end
                    end
                    select_pivot = case select_pivot
                    when :first, :last
                        case [first, last].send(select_pivot)
                        when hl
                            proc {  ld   pivot_reg, [hl]  }
                        when de
                            proc do
                                    ld   a, [de]
                                    ld   pivot_reg, a
                            end
                        end
                    when :half
                        proc do # pivot ← A[(first+last)/2]
                                    ld   temp, l     # save l
                                    ld   a, h        # save h
                                    add  hl, de      # CF|hl: first + last
                                    rr   h           # hl: (first + last) / 2
                                    rr   l           # (including overflow CF)
                                    ld   pivot_reg, [hl]
                                    ld   h, a        # restore h
                                    ld   l, temp     # restore l
                        end
                    else
                        if direct_address?(select_pivot)
                            selpiv_address = select_pivot
                            proc { call selpiv_address }
                        elsif select_pivot.respond_to?(:call)
                            select_pivot
                        else
                            raise ArgumentError,
                                  "select_pivot must be one of :half, :first, :last, a label or a proc"
                        end
                    end
                    find_i = if reverse
                        proc do |eoc| # while pivot < dat[i]: i ← i + 1
                                    ld   a, pivot_reg
                                    cp   [ii]      # pivot - [ii] (C: pivot < [ii], NC: pivot >= [ii])
                                    jr   NC, eoc   # NC: pivot >= dat[i]
                            find    inc  ii
                                    cp   [ii]      # pivot - [ii] (C: pivot < [ii], NC: pivot >= [ii])
                                    jr   C, find   # C: pivot < dat[++i]
                        end
                    else
                        proc do |eoc| # while pivot > dat[i]: i ← i + 1
                                    ld   a, [ii]
                                    cp   pivot_reg # [ii] - pivot (C: [ii] < pivot, NC: [ii] >= pivot)
                                    jr   NC, eoc   # NC: pivot <= dat[i]
                            find    inc  ii
                                    ld   a, [ii]
                                    cp   pivot_reg # [ii] - pivot (C: [ii] < pivot, NC: [ii] >= pivot)
                                    jr   C, find   # C: pivot > dat[++i]
                        end
                    end
                    find_j = if reverse
                        proc do |eoc| # while pivot > dat[j]: j ← j - 1
                                    ld   a, [jj]
                                    cp   pivot_reg # [jj] - pivot (C: [jj] < pivot, NC: [jj] >= pivot)
                                    jr   NC, eoc   # NC: pivot <= dat[j]
                            find    dec  jj
                                    ld   a, [jj]
                                    cp   pivot_reg # [jj] - pivot (C: [jj] < pivot, NC: [jj] >= pivot)
                                    jr   C, find   # C: pivot > dat[--j]
                        end
                    else
                        proc do |eoc| # while pivot < dat[j]: j ← j - 1
                                    ld   a, pivot_reg
                                    cp   [jj]      # pivot - [jj] (C: pivot < [jj], NC: pivot >= [jj])
                                    jr   NC, eoc   # NC: pivot >= dat[j]
                            find    dec  jj
                                    cp   [jj]      # pivot - [jj] (C: pivot < [jj], NC: pivot >= [jj])
                                    jr   C, find   # C: pivot < dat[--j]
                        end
                    end
                    isolate do
                        if safe_args
                                    cp16rr last, first # rigth - first
                                    ret  C             # rigth < first
                                    ret  Z             # rigth = first
                        end
                        qsort       label
                                    push de          # sp[]: first (or last if reversed)
                                    push hl          # sp[]: last (or first if reversed)
                        # pivot ← A[between first and last]
                                    ns(:select_pivot, &select_pivot)
                        # i ← first
                        # j ← last
                        ns :partition do |eoc|
                        #   while pivot > A[i]: i ← i + 1
                                    ns(:find_i, &find_i)
                        #   while pivot < A[j]: j ← j - 1
                                    ns(:find_j, &find_j)
                        #   if i > j
                                    cp16rr jj, ii     # jj - ii
                        #       break loop
                                    jr   C, eoc       # C: jj < ii (j < i)
                                                      # Z: jj = ii (j = i)
                                    jr   Z, skip_swap unless swap_same
                        #   else
                        #       A[i] ⇄ A[j]
                                    ns(:swap_items, &swap_items)
                                    inc  ii # i ← i + 1
                                    dec  jj # j ← j - 1
                        #   if i > j: break
                            if swap_same
                                    jp   NZ, partition # break if (i - 1 == j + 1 <=> i > j)
                            else
                                    jp   partition
                        skip_swap   label
                                    inc  ii
                                    dec  jj
                            end
                        end
                                    ex   [sp], hl      # hl: last or first sp[]: j or i if reverse
                        # if i < last
                                    cp16rr ii, last   # (i - last) or (first - j) if reverse
                        #     qsort[i, last]
                                    call  C, qsort     # C: (i < last) or (first < j) if reverse
                                    pop   hl           # hl: j or i if reverse
                                    pop   de           # de: first or last if reverse
                        # if first < j
                                    cp16rr first, jj    # (first - j) or (i - last) if reverse
                        #     qsort[first, j]
                                    jp    C, qsort     # C: (first < j) or (i < last) if reverse
                                    ret                # jp: tail call opt
                    end
                end
                ##
                # Creates a routine that sorts an array of bytes using insertion sort.
                #
                #   i ← 0
                #   while i < length(A) - 1
                #       i ← i + 1
                #       x ← A[i]
                #       j ← i - 1
                #       while j >= 0 and A[j] > x
                #           A[j+1] ← A[j]
                #           j ← j - 1
                #       end while
                #       A[j+1] ← x
                #   end while
                #
                # Modifies: +af+, +bc+, +de+, +hl+.
                #
                # This function outperforms quicksort for smaller arrrays or arrays that are nearly sorted.
                # The worst case scenario for this algorithm is the array sorted in reverse order.
                #
                # Depending on the +reverse+ option the +target+ should point to the first or the last
                # element of the sorted array:
                #
                #   reverse  target   sorting order
                #     false  first    ascending 
                #      true  last     descending
                #
                # Options:
                # * +reverse+:: Should the sorting order be reversed (descending order).
                # * +target+:: An address of the target element as a label, a pointer or +hl+.
                # * +length+:: An 8-bit length of an array in the range of 1..256 (0 is 256) as a label,
                #              pointer or a register.
                # * +subroutine+:: Whether to create a subroutine.
                #
                # Optionally provide a block that creates an inline procedure for side effects of each
                # insertion. The procedure should expect items already inserted at the address pointed
                # to by +de+ from the item pointed to by +hl+. Depending on the +reverse+ option:
                #
                #   reverse   false (ascending)          true (descending)
                #   before    B   C   D   A              A   D   C   B
                #            [de]        [hl]           [hl]        [de]
                #   after     A → B   C   D              D   C   B ← A
                #             ^                                      ^
                # The side effects procedure <b>MUST PRESERVE</b> the content of the +hl+ 16-bit register.
                def insertion_sort_bytes_max256(reverse:false, target:hl, length:b, subroutine:false, &side_effects)
                    next_hl = if reverse
                        proc { dec hl }
                    else
                        proc { inc hl }
                    end
                    prev_hl = if reverse
                        proc { inc hl }
                    else
                        proc { dec hl }
                    end
                    copy_back = if reverse
                        proc { ldi }
                    else
                        proc { ldd }
                    end
                    isolate do |eoc|
                                    ld    hl, target unless target == hl
                                    ld    b, length unless length == b
                                    dec   b
                        if subroutine
                                    ret   Z
                        else
                                    jr    Z, eoc
                        end
                        # i ← 0
                                    ld    c, 0
                        sort0       ld    e, [hl]
                        # while i < length(A) - 1
                        sort1       ns(&next_hl)
                        #     i ← i + 1
                                    inc   c
                        #     x ← A[i]
                                    ld    a, [hl]
                                    cp    e
                                    jr    C, swap0 # x >= A[i - 1]
                                    ld    e, a
                                    djnz  sort1
                        if subroutine
                                    ret
                        else
                                    jp    eoc
                        end
                        #     j ← i - 1
                        swap0       push  bc
                                    push  hl
                                    ld16  de, hl
                                    ns(&prev_hl)
                                    ld    b, 0
                        #     while j >= 0 and x < A[j]
                        #         A[j+1] ← A[j]
                        #         j ← j - 1
                        swap1       ns(&copy_back) # [de--] ← [hl--], bc--
                                    jp    PO, endswap
                                    cp    [hl]
                                    jr    C, swap1
                        #     A[j+1] ← x
                        endswap     ld    [de], a
                                    pop   hl
                        if block_given?
                                    ns(:inserted, &side_effects)
                        end
                                    pop   bc
                                    djnz  sort0
                                    ret if subroutine
                    end
                end
                ##
                # Creates a routine that sorts an array of bytes using selection sort.
                #
                #   i ← length(A) - 1
                #   while i > 0
                #       iMax ← i
                #       j ← i
                #       while j > 0
                #           j ← j - 1
                #           if A[iMax] < A[j]
                #               iMax = j
                #       A[i] ⇄ A[iMax]
                #       i ← i - 1
                #
                # Modifies: +af+, +bc+, +de+, +hl+.
                #
                # Produces the smallest code. Avoid using it when performance is important.
                # For smaller arrays prefer using inserting sort or quicksort for larger arrays.
                #
                # Depending on the +reverse+ option the +target+ should point to the first or the last
                # element of the sorted array:
                #
                #   reverse  target   sorting order
                #     false  last     ascending 
                #      true  first    descending
                #
                # Options:
                # * +reverse+:: Should the sorting order be reversed (descending order).
                # * +target+:: An address of the target element as a label, a pointer or +hl+.
                # * +length+:: An 8-bit length of an array in the range of 1..256 (0 is 256) as a label,
                #              pointer or a register.
                # * +subroutine+:: Whether to create a subroutine.
                #
                # Optionally provide a block that creates a custom inline procedure for swapping items.
                # The procedure should expect items to be swapped at addresses pointed to by +hl+ and +de+
                # registers. The +accumulator+ will already have the value loaded from <tt>[de]</tt>.
                #
                # The swap items procedure <b>MUST PRESERVE</b> values of +hl+ and +b+ registers.
                def selection_sort_bytes_max256(reverse:false, target:hl, length:b, subroutine:false, &swap_items)
                    next_hl = if reverse
                        proc { inc hl }
                    else
                        proc { dec hl }
                    end
                    unless block_given?
                        swap_items = proc do
                                    ld   c, [hl]
                                    ld   [hl], a
                                    ld   a, c
                                    ld   [de], a
                        end
                    end
                    isolate do |eoc|
                                    ld    hl, target unless target == hl
                                    ld    b, length unless length == b
                        # i ← length(A) - 1
                                    dec   b
                        if subroutine
                                    ret   Z
                        else
                                    jr    Z, eoc
                        end
                        # while i > 0
                        sort        ld    c, b         # save counter
                        #     x ← A[iMax]
                                    ld    a, [hl]
                        #     iMax ← i
                                    ld16  de, hl
                        #     j ← i
                                    push  hl
                        #     while j > 0
                        #         j ← j - 1
                        find_max1   ns(&next_hl)
                        #         if x < A[j]
                                    cp    [hl]
                                    jr    NC, find_max2
                        #             x = A[j]
                                    ld    a, [hl]
                        #             iMax = j
                                    ld16  de, hl
                        find_max2   djnz  find_max1
                                    ld    b, c         # restore counter
                                    pop   hl
                        #     A[iMax] ← A[i]
                        #     A[i] ← x
                                    ns(:swap_items, &swap_items)
                        #     i ← i - 1
                                    ns(&next_hl)
                                    djnz  sort
                                    ret if subroutine
                    end
                end
            end # Macros

            include Z80
        end
    end
end

if __FILE__ == $0
    require 'zxlib/basic'
    require 'zxlib/sys'
    require 'z80/utils/shuffle'
    # :stopdoc:
    class TestSort # :nodoc: all
        include Z80
        include Z80::TAP

        macro_import Stdlib
        macro_import Utils::Sort
        macro_import Utils::Shuffle
        macro_import MathInt
        label_import ZXLib::Sys

        ns :clr_low2 do
                    clrmem mem.screen + 32*64, 32*128
                    ret
        end

        ns :shuffle do
                    ld   hl, mem.attrs
                    ld   a, 256
                    call shuffle_it
                    ret
        end

        with_saved :randomize, :exx, hl, ret: true do
                    ld   hl, mem.attrs
                    ld   b, 256
            loop0   exx
                    call next_rng
                    exx
                    ld   [hl], a
                    inc  hl
                    djnz loop0
        end

        ns :qsort_ascending do
                    ld a, 3; out  (io.ula), a
                    ld   de, mem.attrs
                    ld   hl, mem.attrs + 255
                    call qsort_asc
                    ld a, 7; out  (io.ula), a
                    ret
        end

        ns :qsort_descending do
                    ld a, 3; out  (io.ula), a
                    ld   hl, mem.attrs
                    ld   de, mem.attrs + 255
                    call qsort_desc
                    ld a, 7; out  (io.ula), a
                    ret
        end

        ns :isort_ascending do
                    ld a, 3; out  (io.ula), a
                    ld   hl, mem.attrs
                    ld   b, 256
                    call isort_asc
                    ld a, 7; out  (io.ula), a
                    ret
        end

        ns :isort_descending do
                    ld a, 3; out  (io.ula), a
                    ld   hl, mem.attrs + 255
                    ld   b, 256
                    call isort_desc
                    ld a, 7; out  (io.ula), a
                    ret
        end

        ns :ssort_ascending do
                    ld a, 3; out  (io.ula), a
                    ld   hl, mem.attrs + 255
                    ld   b, 256
                    call ssort_asc
                    ld a, 7; out  (io.ula), a
                    ret
        end

        ns :ssort_descending do
                    ld a, 3; out  (io.ula), a
                    ld   hl, mem.attrs
                    ld   b, 256
                    call ssort_desc
                    ld a, 7; out  (io.ula), a
                    ret
        end

        next_rng    ld   hl, [vars.seed]
                    rnd
                    ld   [vars.seed], hl
                    ld   a, l
                    ret

        shuffle_it  shuffle_bytes_source_max256(next_rng, target:hl, length:a)
        shuffle_end ret

        qsort_asc   quicksort_bytes(:half, reverse: false, pivot_reg: c, swap_same: true,  safe_args: false)
        qsort_desc  quicksort_bytes(:half, reverse: true,  pivot_reg: b, swap_same: false, safe_args: false)
        qsort_end   label
        isort_asc   insertion_sort_bytes_max256(reverse: false, target:hl, length:b, subroutine:true)
        isort_desc  insertion_sort_bytes_max256(reverse: true , target:hl, length:b, subroutine:true)
        isort_end   label
        ssort_asc   selection_sort_bytes_max256(reverse: false, target:hl, length:b, subroutine:true)
        ssort_desc  selection_sort_bytes_max256(reverse: true , target:hl, length:b, subroutine:true)
        ssort_end   label
    end

    include ZXLib

    testsort = TestSort.new 0xe000
    mem_attrs = Sys.new['mem.attrs']
    program = Basic.parse_source <<-END
      10 CLS: FOR i=1 TO 8: PRINT "#{'`#6`'*32}";: NEXT i
         PRINT #1;"press any key...";: PAUSE 0: INPUT ""
         READ sorta,sortd,s$
      20 RANDOMIZE : RANDOMIZE USR #{testsort[:shuffle]}
         GO SUB 1000: PAUSE 0: GO SUB 2000
      30 RANDOMIZE USR sorta
         GO SUB 1000: PAUSE 0: GO SUB 2000
      40 RANDOMIZE USR sortd
         GO SUB 1000: PAUSE 0: GO SUB 2000
      50 RANDOMIZE : RANDOMIZE USR #{testsort[:shuffle]}
         GO SUB 1000: PAUSE 0: GO SUB 2000
      60 RANDOMIZE USR sortd
         GO SUB 1000: PAUSE 0: GO SUB 2000
      70 RANDOMIZE USR sorta
         GO SUB 1000: PAUSE 0: GO SUB 2000
     100 RANDOMIZE : RANDOMIZE USR #{testsort[:randomize]}
         LET s=0: GO SUB 1500: PAUSE 0: GO SUB 2000
     110 RANDOMIZE USR sorta
         LET s=1: GO SUB 1500: PAUSE 0: GO SUB 2000
     120 RANDOMIZE USR sortd
         LET s=-1: GO SUB 1500: PAUSE 0: GO SUB 2000
     130 RANDOMIZE : RANDOMIZE USR #{testsort[:randomize]}
         LET s=0: GO SUB 1500: PAUSE 0: GO SUB 2000
     140 RANDOMIZE USR sortd
         LET s=-1: GO SUB 1500: PAUSE 0: GO SUB 2000
     150 RANDOMIZE USR sorta
         LET s=1: GO SUB 1500: PAUSE 0: GO SUB 2000
     999 STOP: GO TO 10
    1000 INPUT "": LET y=80
         FOR i=#{mem_attrs} TO #{mem_attrs+255}
         LET x=PEEK i: IF POINT (x,y) THEN PRINT "Error: duplicate at: ";i-#{mem_attrs};" x=";x: GO TO 9998
         PLOT x,y: DRAW 0,7
         NEXT i
         PRINT #1;"OK ";INK 3;s$
         RETURN
    1500 INPUT "": LET y=0
         IF NOT s THEN LET x=-1: GO TO 1510
         IF s>0 THEN LET x=0: GO TO 1510
         IF s<0 THEN LET x=255: GO TO 1510
    1510 FOR i=#{mem_attrs} TO #{mem_attrs+255}
         LET x1=PEEK i
         IF (s<0 AND x1>x) OR (s>0 AND x1<x) THEN PRINT "Error: unsorted at: ";i-#{mem_attrs};" x1=";x1;" x=";x: GO TO 9998
         LET x=x1
         IF POINT (x,y) THEN LET y=y+1
         PLOT x,y
         NEXT i
         PRINT #1;"OK ";INK 3;s$
         RETURN
    2000 RANDOMIZE USR #{testsort[:clr_low2]}
         RETURN
    9000 DATA #{testsort[:ssort_ascending]},#{testsort[:ssort_descending]},"SSort"
         DATA #{testsort[:isort_ascending]},#{testsort[:isort_descending]},"ISort"
         DATA #{testsort[:qsort_ascending]},#{testsort[:qsort_descending]},"QSort"
    9998 STOP
    9999 CLEAR #{testsort.org - 1}: LOAD ""CODE: RUN
    END
    puts testsort.debug
    puts program.to_source escape_keywords:true
    program.save_tap 'testsort.tap', line:9999
    puts "qsort_asc  length: #{testsort[:qsort_desc] - testsort[:qsort_asc]}"
    puts "qsort_desc length: #{testsort[:qsort_end] - testsort[:qsort_desc]}"
    puts "isort_asc  length: #{testsort[:isort_desc] - testsort[:isort_asc]}"
    puts "isort_desc length: #{testsort[:isort_end] - testsort[:isort_desc]}"
    puts "ssort_asc  length: #{testsort[:ssort_desc] - testsort[:ssort_asc]}"
    puts "ssort_desc length: #{testsort[:ssort_end] - testsort[:ssort_desc]}"
    testsort.save_tap 'testsort.tap', append:true
end
