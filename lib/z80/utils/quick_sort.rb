# -*- coding: BINARY -*-
require 'z80'
require 'z80/math_i'

module Z80
    module Utils
        ##
        # =Z80::Utils::QuickSort
        #
        # Quicksort algorithm implementations in Z80::Utils::QuickSort::Macros
        class QuickSort
            ##
            # =Z80::Utils::QuickSort macros
            #
            #   qsort: [first, last] do
            #       pivot ← dat[between first and last]
            #       i ← first
            #       j ← last
            #       partition: loop do
            #           while pivot > dat[i]
            #               i ← i + 1
            #           while pivot < dat[j]
            #               j ← j - 1
            #           if i ≤ j
            #               dat[i] ⇄ dat[j]
            #           else
            #               break loop
            #           i ← i + 1
            #           j ← j - 1
            #       if first < j
            #           qsort[first, j]
            #       if i < last
            #           qsort[i, last]
            #
            # QuickSort macros require:
            #
            #    macro_import MathInt
            module Macros
                ##
                # Creates a subroutine that sorts an array of bytes.
                #
                # The created function is recursive.
                #
                # Modifies: +af+, +bc+, +de+, +hl+.
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
                # The built-in +:half+ takes additional 42 to 46 T-states for each element but is a fair
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
                        proc do # int pivot = dat[(first+last)/2];
                                    ld16 bc, hl      # bc: hl
                                    add  hl, de
                                    rr   h
                                    rr   l
                                    ld   a, [hl]     # a: pivot
                                    ld16 hl, bc      # hl: bc
                                    ld   pivot_reg, a
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
                        # pivot ← dat[between first and last]
                                    ns(:select_pivot, &select_pivot)
                        # i ← first
                        # j ← last
                        ns :partition do |eoc|
                        #   while pivot > dat[i]: i ← i + 1
                                    ns(:find_i, &find_i)
                        #   while pivot < dat[j]: j ← j - 1
                                    ns(:find_j, &find_j)
                        #   if i > j
                                    cp16rr jj, ii     # jj - ii
                        #       break loop
                                    jr   C, eoc       # C: jj < ii (j < i)
                                                      # Z: jj = ii (j = i)
                                    jr   Z, skip_swap unless swap_same
                        #   else
                        #       dat[i] ⇄ dat[j]
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
    class TestQSort # :nodoc: all
        include Z80
        include Z80::TAP

        macro_import Stdlib
        macro_import Utils::QuickSort
        macro_import Utils::Shuffle
        macro_import MathInt
        label_import ZXLib::Sys

        ns :clr_low2 do
                    clrmem mem.screen + 32*64, 32*128
                    ret
        end

        ns :shuffle do
                    ld   hl, testarea
                    ld   a, 256
                    call shuffle_it
                    ld   hl, mem.attrs
                    ld   a, 256
                    call shuffle_it
                    ret
        end

        with_saved :randomize, :exx, hl, ret: true do
                    ld   hl, testarea
                    ld   de, mem.attrs
                    ld   b, 256
            loop0   exx
                    call next_rng
                    exx
                    ld   [hl], a
                    inc  hl
                    ld   [de], a
                    inc  de
                    djnz loop0
        end

        ns :sort_ascending do
                    ld   de, testarea
                    ld   hl, testarea + 255
                    call qsort_asc
                    ld   de, mem.attrs
                    ld   hl, mem.attrs + 255
                    jp   qsort_asc
        end

        ns :sort_descending do
                    ld   hl, testarea
                    ld   de, testarea + 255
                    call qsort_desc
                    ld   hl, mem.attrs
                    ld   de, mem.attrs + 255
                    jp   qsort_desc
        end

        next_rng    ld   hl, [vars.seed]
                    rnd
                    ld   [vars.seed], hl
                    ld   a, l
                    ret

        shuffle_it  shuffle_bytes_source_max256(next_rng, target:hl, length:a)
        shuffle_end ret

        qsort_asc   quicksort_bytes(:half, reverse: false, pivot_reg: c, swap_same: true, safe_args: false)
        qsort_desc  quicksort_bytes(:half, reverse: true,  pivot_reg: b, swap_same: false, safe_args: false)
        qsort_end   label

        testarea    label
    end

    include ZXLib

    testquicksort = TestQSort.new 0xe000
    testarea = testquicksort[:testarea]
    mem_attrs = Sys.new['mem.attrs']
    program = Basic.parse_source <<-END
      10 CLS: FOR i=1 TO 8: PRINT "#{'`#6`'*32}";: NEXT i
         PRINT #1;"press any key...";: PAUSE 0: INPUT ""
      20 RANDOMIZE : RANDOMIZE USR #{testquicksort[:shuffle]}
         GO SUB 1000: PAUSE 0: GO SUB 2000
      30 RANDOMIZE USR #{testquicksort[:sort_ascending]}
         GO SUB 1000: PAUSE 0: GO SUB 2000
      40 RANDOMIZE USR #{testquicksort[:sort_descending]}
         GO SUB 1000: PAUSE 0: GO SUB 2000
      50 RANDOMIZE : RANDOMIZE USR #{testquicksort[:shuffle]}
         GO SUB 1000: PAUSE 0: GO SUB 2000
      60 RANDOMIZE USR #{testquicksort[:sort_descending]}
         GO SUB 1000: PAUSE 0: GO SUB 2000
      70 RANDOMIZE USR #{testquicksort[:sort_ascending]}
         GO SUB 1000: PAUSE 0: GO SUB 2000
     100 RANDOMIZE : RANDOMIZE USR #{testquicksort[:randomize]}
         LET s=0: GO SUB 1500: PAUSE 0: GO SUB 2000
     110 RANDOMIZE USR #{testquicksort[:sort_ascending]}
         LET s=1: GO SUB 1500: PAUSE 0: GO SUB 2000
     120 RANDOMIZE USR #{testquicksort[:sort_descending]}
         LET s=-1: GO SUB 1500: PAUSE 0: GO SUB 2000
     130 RANDOMIZE : RANDOMIZE USR #{testquicksort[:randomize]}
         LET s=0: GO SUB 1500: PAUSE 0: GO SUB 2000
     140 RANDOMIZE USR #{testquicksort[:sort_descending]}
         LET s=-1: GO SUB 1500: PAUSE 0: GO SUB 2000
     150 RANDOMIZE USR #{testquicksort[:sort_ascending]}
         LET s=1: GO SUB 1500: PAUSE 0: GO SUB 2000
     999 STOP
    1000 INPUT "": LET y=80
         FOR i=#{testarea} TO #{testarea+255}
         LET x=PEEK i: IF POINT (x,y) THEN PRINT "Error: duplicate at: ";i-#{testarea};" x=";x: GO TO 9998
         PLOT x,y: DRAW 0,7
         NEXT i
         PRINT #1;"OK"
         RETURN
    1500 INPUT "": LET y=0
         IF NOT s THEN LET x=-1: GO TO 1510
         IF s>0 THEN LET x=0: GO TO 1510
         IF s<0 THEN LET x=255: GO TO 1510
    1510 FOR i=#{testarea} TO #{testarea+255}
         LET x1=PEEK i
         IF (s<0 AND x1>x) OR (s>0 AND x1<x) THEN PRINT "Error: unsorted at: ";i-#{testarea};" x1=";x1;" x=";x: GO TO 9998
         LET x=x1
         IF POINT (x,y) THEN LET y=y+1
         PLOT x,y
         NEXT i
         PRINT #1;"OK"
         RETURN
    2000 RANDOMIZE USR #{testquicksort[:clr_low2]}
         RETURN
    9998 STOP
    9999 CLEAR #{testquicksort.org - 1}: LOAD ""CODE: RUN
    END
    puts testquicksort.debug
    puts program.to_source escape_keywords:true
    program.save_tap 'testquicksort.tap', line:9999
    puts "qsort_asc  length: #{testquicksort[:qsort_desc] - testquicksort[:qsort_asc]}"
    puts "qsort_desc length: #{testquicksort[:qsort_end] - testquicksort[:qsort_desc]}"
    testquicksort.save_tap 'testquicksort.tap', append:true
end
