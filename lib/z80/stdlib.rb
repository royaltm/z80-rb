module Z80
    ##
    # =Z80::Stdlib - Macros with commonly used memory routines.
    #
    #     require 'z80'
    #     
    #     class MyLib
    #         include Z80
    #         macro_import Stdlib
    #     
    #         export :auto
    #     
    #         ns :clear_memory do
    #                         pop  hl
    #                     [e, d, c, b].each do |r|
    #                         ld   r, [hl]
    #                         inc  hl
    #                     end
    #                         push hl
    #                         ex   de, hl
    #                         clrmem
    #                         ret
    #         end
    #     
    #         ns :copy_memory do
    #                         pop  hl
    #                     [e, d, c, b, a].each do |r|
    #                         ld   r, [hl]
    #                         inc  hl
    #                     end
    #                         inc  hl
    #                         push hl
    #                         dec  hl
    #                         ld   h, [hl]
    #                         ld   l, a
    #                         memcpy
    #                         ret
    #         end
    #     end
    #     
    #     
    #     class Program
    #         include Z80
    #         include Z80::TAP
    #     
    #         destination addr 0x5800
    #     
    #                     call mylib.copy_memory
    #                     dw   destination
    #                     dw   +source
    #                     dw   source
    #                     # ...
    #                     call mylib.clear_memory
    #                     dw   destination
    #                     dw   +source
    #                     # ...
    #                     ret
    #     
    #         source      data "Some data..."
    #         import      MyLib, :mylib
    #     end
    #     
    #     program = Program.new 0x8000
    #     puts program.debug
    #
    class Stdlib
        ## =Z80::Stdlib Macros.
        module Macros
            ##
            # Clears max 256 bytes of memory at +dest+.
            # Slower (does not use LDIR/LDDR) but involves less registers.
            #
            # T-states: ~ 26/cleared byte. (~ 45 when +rr+ is +ix+ or +iy+)
            #
            # Modifies: +a+, +b+, +rr+.
            #
            # +dest+::  An address of the memory area to be cleared as an integer, a label, a pointer
            #           or same as +rr+.
            # +size+::  A size of area to be cleared or one of the 8bit registers.
            # +value+:: A fill byte value or one of the 8bit registers except +b+.
            #
            # Options:
            # +rr+::    16bit address register: +de+, +hl+, +ix+, +iy+.
            def clrmem8(dest=hl, size=b, value=0, rr:hl)
                raise ArgumentError unless (address?(dest) or dest == rr) and
                                           (address?(size) or (register?(size) and size.bit8?)) and
                                           ((pointer?(size) and value != a) or (!pointer?(size))) and
                                           (address?(value) or (register?(value) and value != b and value.bit8?)) and
                                           [de, hl, ix, iy].include?(rr)
                isolate do
                    if pointer?(size)
                            ld  a, size
                            ld  b, a
                    elsif size != b
                            ld  b, size
                    end
                    if value == 0
                            xor a
                    elsif value != a
                            ld  a, value
                    end
                            ld  rr, dest unless dest == rr
                    loop1   ld  [rr], a
                            inc rr
                            djnz loop1
                end
            end
            ##
            # Clears memory at +dest+ using LDIR instruction.
            #
            # T-states: ~ 21/cleared byte.
            #
            # Modifies: +bc+, +de+, +hl+, optionally +a+ if +value+ is an indirect (pointer) address.
            #
            # +dest+:: An address of the memory area to be cleared as an integer, a label, a pointer or +hl+.
            # +size+:: 16bit size of area to be cleared as an integer, a label, a pointer or +bc+.
            # +value+:: A fill byte value as an integer, a label, a pointer or one of the registers: +a+, +b+, +c+, +d+, +e+.
            def clrmem(dest=hl, size=bc, value=0)
                raise ArgumentError unless (address?(dest) or dest == hl) and
                                           (address?(size) or size == bc) and
                                           (address?(value) or [a,b,c,d,e].include?(value))
                isolate do
                            ld   hl, dest unless dest == hl
                        if pointer?(value)
                            ld   a, value
                            ld   [hl], a
                        else
                            ld   [hl], value
                        end
                        if size == bc
                            dec  bc
                        elsif pointer?(size)
                            ld   bc, size
                            dec  bc
                        else
                            ld   bc, size - 1
                        end
                        if dest == hl or pointer?(dest)
                            ld16 de, hl
                            inc  de
                        else
                            ld  de, dest + 1
                        end
                            ldir
                end
            end
            ##
            # Clears memory at +dest+ in a faster way using unrolled instructions.
            #
            # T-states: ~ 13/cleared byte.
            #
            # Modifies: +a+, +rr+.
            #
            # +dest+::  An address of the memory area to be cleared as an integer, a label, a pointer or same as +rr+.
            # +size+::  A static size of area to be cleared. It should be a reasonably small positive integer number.
            #           There will be: +size+ * 2 bytes added to the code.
            # +value+:: A fill byte value as an integer, a label, a pointer or one of the 8-bit registers.
            #
            # Options:
            # +rr+::    16bit address register: +bc+, +de+, +hl+.
            def clrmem_quick(dest=hl, size=1, value=0, rr:hl)
                raise ArgumentError unless (address?(dest) or dest == rr) and
                                           (Integer === size) and size > 0 and
                                           (address?(value) or (register?(value) and value.bit8?)) and
                                           [bc, de, hl].include?(rr)
                isolate do
                        if value == 0
                            xor a
                        elsif value != a
                            ld  a, value
                        end
                            ld  rr, dest unless dest == rr
                        (size - 1).times do
                            ld  [rr], a
                            inc rr
                        end
                            ld  [rr], a
                end
            end
            ##
            # Clears a memory area using unrolled PUSH with a tight loop.
            #
            # _NOTE_:: Interrupts should be disabled during execution of this code.
            #
            # T-states: ~ 5,5/cleared byte in a chunk + 13 between chunks + 8 after the last chunk.
            #
            # +address+:: An address of the next byte _AFTER_ THE END of the memory area to be cleared.
            #             The address may be an integer, a label, a pointer or one of: +sp+, +hl+, +ix+ or +iy+.
            # +chunks_count+:: The number of the unrolled push chunks. It should be between 1 and 256.
            #                  It may be be an integer, a label, a pointer or an 8-bit register.
            # +chunk_size+:: The size in bytes of the chunk of memory being cleared by unrolled PUSHes.
            #                It must be an _EVEN_ integer number. Each 2 of +chunk_size+ adds a 1 byte to the code.
            # +value+:: A 16-bit (a word) fill value as an integer, a label, a pointer or +tt+.
            #
            # The total size being cleared equals to: +chunks_count+ * +chunk_size+.
            # 
            # Options:
            # * +tt+:: A 16-bit temporary register containing the filler value of the memory area to be cleared.
            #          One of: +hl+, +de+ and optionally +bc+ only if +chunks_count+ is 1.
            # * +disable_intr+:: A boolean flag indicating that the routine should disable interrupts. Provide +false+
            #                    only if you have already disabled the interrupts.
            # * +enable_intr+:: A boolean flag indicating that the routine should enable interrupts. Provide +false+
            #                   if you need to perform more uninterrupted actions.
            # * +save_sp+:: A boolean flag indicating that the +sp+ register should be saved and restored. Otherwise
            #               +sp+ will point to the beginning of the memory area being cleared.
            #
            # _NOTE_:: Restoring +sp+ register uses self-modifying code.
            #
            # Modifies: +tt+, +sp+, optionally +b+ if +chunks_count+ is not 1 and +a+ if +chunks_count+ is a pointer.
            def clrmem_fastest(address=hl, chunks_count=b, chunk_size=2, value=0, tt:hl, disable_intr:true, enable_intr:true, save_sp:true)
                raise ArgumentError unless (address?(address) or [sp,hl,ix,iy].include?(address)) and
                                           (address?(chunks_count) or (register?(chunks_count) and chunks_count.bit8?)) and
                                           (Integer === chunk_size) and
                                           (address?(value) or value == tt) and
                                           [bc,de,hl].include?(tt)
                raise ArgumentError, "chunk_size must be a positive and even integer number" unless chunk_size.even? and chunk_size > 0
                raise ArgumentError, "chunks_count must be between 1 and 256" if Integer === chunks_count and
                                                                                 !(1..256).include?(chunks_count)
                raise ArgumentError, "tt must not be bc when chunks_count is not 1" if tt == bc and chunks_count != 1
                isolate do
                                ld  [restore_sp + 1], sp if save_sp
                    unless chunks_count == 1
                        if pointer?(chunks_count)
                                ld  a, chunks_count
                                ld  b, a
                        else
                                ld  b, chunks_count unless chunks_count == b
                        end
                    end
                                di if disable_intr
                                ld  sp, address unless address == sp
                                ld  tt, value unless value == tt
                    loop1       label
                                (chunk_size/2).times { push tt }
                                djnz loop1 unless chunks_count == 1
                    restore_sp  ld  sp, 0 if save_sp
                                ei if enable_intr
                end
            end
            ##
            # Copies +size+ bytes from memory area +source+ to memory area +dest+.
            #
            # +dest+::  A destination address as an integer, a label, a pointer or +de+.
            # +source+:: A destination address as an integer, a label, a pointer or +hl+.
            # +size+::  A size of area to be copied as an integer, a label, a pointer or +bc+.
            #
            # Options:
            # +reverse+:: A flag if +true+ the routine uses LDDR, otherwise uses LDIR.
            # 
            # _NOTE_:: In case +reverse+ is +true+ the +dest+ and +source+ should address
            #          the last byte of the destinetion and source memory area to be copied.
            #
            # If +reverse+ is +nil+ and +dest+ and +source+ and +size+ are integers or labels (but not pointers)
            # detects if source and destination memory overlaps and applies LDIR or LDDR during code generation.
            #
            # Modifies: +bc+, +de+, +hl+.
            def memcpy(dest=de, source=hl, size=bc, reverse: nil)
                raise ArgumentError unless (address?(dest) or dest == de) and
                                           (address?(source) or source == hl) and
                                           (address?(size) or size == bc)
                isolate do
                    if reverse.nil? and address?(dest) and address?(source) and address?(size) and
                                         !pointer?(dest) and !pointer?(source) and !pointer?(size)
                            ld  bc, size
                        select(dest, source, size) {|dst,src,sz| dst > src && src + sz > dst }.then do |_|
                            ld  de, dest + size - 1
                            ld  hl, source + size - 1
                            lddr
                        end.else do
                            ld  de, dest
                            ld  hl, source
                            ldir
                        end
                    else
                            ld  bc, size unless size == bc
                            ld  de, dest unless dest == de
                            ld  hl, source unless source == hl
                        if reverse
                            lddr
                        else
                            ldir
                        end
                    end
                end
            end
            ##
            # Copies +size+ bytes from memory area +source+ to memory area +dest+ using unrolled LDI/LDD.
            #
            # +dest+::  A destination address as an integer, a label, a pointer or +de+.
            # +source+:: A destination address as an integer, a label, a pointer or +hl+.
            # +size+::   A static size of area to be copied. It should be a reasonably small positive integer number.
            #
            # Options:
            # +reverse+:: A flag if +true+ the routine uses LDD, otherwise uses LDI.
            # 
            # _NOTE_:: In case +reverse+ is +true+ the +dest+ and +source+ should address
            #          the last byte of the destinetion and source memory area to be copied.
            #
            # If +reverse+ is +nil+ and +dest+ and +source+ are integers or labels (but not pointers)
            # detects if source and destination memory overlaps and applies LDI or LDD during code generation.
            #
            # Modifies: +bc+, +de+, +hl+.
            def memcpy_quick(dest=de, source=hl, size=1, reverse: nil)
                raise ArgumentError unless (address?(dest) or dest == de) and
                                           (address?(source) or source == hl) and
                                           (Integer === size) and size > 0
                isolate do
                    if reverse.nil? and address?(dest) and address?(source) and
                                          !pointer?(dest) and !pointer?(source)
                        select(dest, source, size) {|dst,src,sz| dst > src && src + sz > dst }.then do |_|
                            ld  de, dest + size - 1
                            ld  hl, source + size - 1
                            size.times { ldd }
                        end.else do
                            ld  de, dest
                            ld  hl, source
                            size.times { ldi }
                        end
                    else
                            ld  de, dest if dest != de
                            ld  hl, source if source != hl
                        if reverse
                            size.times { ldd }
                        else
                            size.times { ldi }
                        end
                    end
                end
            end
        end

        include Z80
    end
end

# DEPRECATED
Z80Lib = ::Z80::Stdlib # :nodoc:

if __FILE__ == $0
    # :stopdoc:
    require 'test/unit/assertions'
    require 'digest/sha1'
    require 'zxlib/sys'

    include Test::Unit::Assertions

    class TestStdlib # :nodoc: all
        include Z80
        include Z80::TAP

        macro_import Stdlib
        label_import ZXLib::Sys

        ns :tests do
            memcpy_0            memcpy
            memcpy_1            memcpy  reverse:true
            memcpy_2            memcpy  mem.screen, mem.screen + 2048, 4096
            memcpy_3            memcpy  mem.screen + 4095, mem.screen + 2048 + 4095, 4096, reverse:true
            memcpy_4            memcpy  mem.screen + 2048, mem.screen, 4096
            memcpy_5            memcpy  mem.screen + 2048, mem.screen, 4096, reverse:false
            memcpy_6            memcpy  [destination], [source], [somelength]
            memcpy_7            memcpy  [destination], [source], [somelength], reverse:true

            memcpy_quick_0      memcpy_quick
            memcpy_quick_1      memcpy_quick  reverse:true
            memcpy_quick_2      memcpy_quick  mem.screen, mem.screen + 4, 6
            memcpy_quick_3      memcpy_quick  mem.screen + 5, mem.screen + 4 + 5, 6, reverse:true
            memcpy_quick_4      memcpy_quick  mem.screen + 4, mem.screen, 6
            memcpy_quick_5      memcpy_quick  mem.screen + 4, mem.screen, 6, reverse:false
            memcpy_quick_6      memcpy_quick  [destination], [source], 6
            memcpy_quick_7      memcpy_quick  [destination], [source], 6, reverse:true

            memcpy_quick1       memcpy_quick  mem.screen, mem.screen + 4, 6
            memcpy_quick2       memcpy_quick  mem.screen + 4, mem.screen, 6

            clrmem8_0           clrmem8
            clrmem8_1           clrmem8 mem.screen, 256, 0, rr:hl
            ns :clrmem8_2 do
                                ld  de, mem.screen
                                ld   c, 128
                                clrmem8 de, c, 42, rr:de
            end
            ns :clrmem8_3 do
                                ld  a, [somelength]
                                clrmem8 [destination], a, [somevalue], rr:ix
            end
            ns :clrmem8_4 do
                                ld  b, 42
                                ld  hl, mem.screen
                                xor a
                                clrmem8 hl, b, a, rr:hl
            end
            ns :clrmem8_5 do
                                ld  h, 42
                                ld  l, 0b01010101
                                clrmem8 mem.screen, h, l, rr:hl
            end
            clrmem8_6           clrmem8 [destination], [somelength], [somevalue], rr:hl
            clrmem8_7           clrmem8 [destination], [somelength], 0, rr:hl
            clrmem8_8           clrmem8 [destination], [somelength], 0b01010101, rr:iy
            ns :clrmem8_9 do
                                ld      iy, (0b01010101 << 8)|64
                                ld      ix, mem.screen
                                clrmem8 ix, iyl, iyh, rr:ix
            end
            ns :clrmem8_A do
                                ld      e, 0b01010101
                                clrmem8 [destination], [somelength], e, rr:de
            end
            clrmem_0            clrmem
            clrmem_1            clrmem  mem.screen, mem.scrlen, foo
            clrmem_2            clrmem  [destination], [somelength], [somevalue]
            clrmem_3            clrmem  hl, bc, a
            clrmem_4            clrmem  hl, bc
            clrmem_quick0       clrmem_quick
            clrmem_quick1       clrmem_quick mem.screen, 2, 0, rr: hl
            clrmem_quick2       clrmem_quick [destination], 2, 0b01010101, rr: hl
            ns :clrmem_quick3 do
                                ld  bc, mem.screen
                                ld  e, 0b01010101
                                clrmem_quick bc, 2, e, rr: bc
            end
            clrmem_fastest_0    clrmem_fastest
            clrmem_fastest_1    clrmem_fastest mem.screen, 192, 32, 0b0101010101010101, tt:de, disable_intr:true, enable_intr:false, save_sp:false
            ns :clrmem_fastest_2 do
                                ld  hl, 0b1010101010101010
                                clrmem_fastest [destination], [somecount], 32, hl, tt:hl, disable_intr:false, enable_intr:false, save_sp:false
            end
            ns :clrmem_fastest_3 do
                                ld  sp, mem.screen + 32
                                clrmem_fastest sp, 1, 32, [somevalue16], tt:bc, disable_intr:false, enable_intr:false, save_sp:false
            end
        end

        destination     dw     mem.screen
        source          dw     0x6000
        somelength      dw     mem.scrlen
        somecount       db     192
        somevalue       db     foo
        somevalue16     dw     0b1010101010101010
        foo             addr   42
    end

    program = TestStdlib.new 0x8000
    puts program.debug
    assert_equal Digest::SHA256.base64digest(program.code), 'Ol1CWDQzIGeXuJVkulajua3rkzpMWbVTzNUkmYUMrI8='
end
