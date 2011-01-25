class ZXGfx
  module Macros
    #  advances to next screen line byte address using ah al registers
    #  (optionally) returns from subroutine if address goes out of screen area
    #  uses: a, ah, al
    #  ah: register  input/output: address high byte
    #  al: register  input/output: address low byte
    #  bcheck: boundary check flag
    #    false = disable checking
    #    true  = return if out of screen (default)
    #    label = jump to label if out of screen
    def nextline(ah, al, bcheck = true) # 27:87.5% /49:1.56% (78:2/68:1 bckeck=jp) (73:2/63:1 bcheck=ret) /59:10.94% (62 bcheck)
        if ah == al or [ah, al].include?(a)
            raise ArgumentError, "xytoscr invalid arguments!"
        end
        ns do |eoc|
                inc  ah
                ld   a, ah
                anda 0x07
                jr   NZ,eoc
                ld   a, al
                add  0x20
                ld   al, a
          if bcheck
                jp   NC, restrh
                ld   a, ah
                cp   0x58
                jr   C, eoc
            if bcheck == true
                ret
            else
                jp   bcheck
            end
          else
                jr   C, eoc
          end
        restrh  ld   a, ah
                sub  0x08
                ld   ah, a
        end
    end
    #  converts x,y coordinates to screen byte address and bits shift
    #  uses: a, y, x, s, t
    #  y: register  input: vertical-coordinate   (may be a or same as h l s t)
    #  x: register  input: horizontal-coordinate (may be same as l)
    #  h: register  output: address high
    #  l: register  output: address low
    #  s: register  output: bits shift
    #  t: register  temporary
    #
    #  y< a1 a2 h3 h2 h1 l3 l2 l1  x< x5 x4 x3 x2 x1 s3 s2 s1
    #  y> 0  1  0  a1 a2 l3 l2 l1  x> h3 h2 h1 x5 x4 x3 x2 x1  s> 0  0  0  0  0  s3 s2 s1
    def xytoscr(y, x, h, l, s, t) # 101
        if y == x or [x,h,l,s,t].include?(a) or [h,l,s,t].uniq.size != 4 # adds a to rr, destroys a or [x,h,s,t].uniq.size != 4
            raise ArgumentError, "xytoscr invalid arguments!"
        end
        ns do
            if y == a 
              ld   h, a
            else
              ld   a, y
            end            # a= a a h h h l l l
            anda 0b00000111
            ld   s, a      # s= 0 0 0 0 0 l l l
            if y == a
              xor  h
            else
              xor  y       # a= a a h h h 0 0 0
            end
            rrca           # a= 0 a a h h h 0 0
            scf
            rra            # a= 1 0 a a h h h 0
            rrca
            ld   h, a      # h= 0 1 0 a a h h h
            anda 0b00000111
            ld   t, a      # t= 0 0 0 0 0 h h h
            xor  h         # a= 0 1 0 a a 0 0 0
            ora  s
            ld   h, a      # h= 0 1 0 a a l l l
            ld   a, x      # a= x x x x x s s s
            anda 0b00000111
            ld   s, a      # s= 0 0 0 0 0 s s s
            xor  x         # a= x x x x x 0 0 0
            ora  t         # a= x x x x x h h h
            3.times { rrca }
            ld   l, a      # l= h h h x x x x x
        end
    end
    #  converts 0,y coordinates to screen byte address
    #  uses: a, y, x, s, t
    #  y: register  input: vertical-coordinate (may be same as h, t or l or even a)
    #  h: register  output: address high
    #  l: register  output: address low
    #  t: register  temporary
    #
    #  y< a1 a2 h3 h2 h1 l3 l2 l1
    #  y> 0  1  0  a1 a2 l3 l2 l1  x> h3 h2 h1 0  0  0  0  0
    def ytoscr(y, h, l, t) # 73
      if [h,l,t].include?(a) or [h,l,t].uniq.size != 3
        raise ArgumentError, "ytoscr invalid arguments!"
      end
      ns do
            if y == a
              ld   l, a
            else
              ld   a, y
            end           # a= a a h h h l l l
            anda 0b00000111
            ld   t, a     # h= 0 0 0 0 0 l l l
            if y == a
              xor  l
            else
              xor  y      # a= a a h h h 0 0 0
            end
            rlca          # a= a h h h 0 0 0 a
            rlca          # a= h h h 0 0 0 a a
            ld   h, a     # b= h h h 0 0 0 a a
            anda 0b11100000
            ld   l, a     # l= h h h 0 0 0 0 0
            xor  h        # a= 0 0 0 0 0 0 a a
            3.times { rlca }
            ora  t        # a= 0 0 0 a a l l l
            ora  0x40     # a= 0 1 0 a a l l l
            ld   h, a     # h= 0 1 0 a a l l l
      end
    end
    #  converts row,col text coordinates to screen byte address
    #  uses: a, r, c, h, l
    #  r: register  input: text row     (may be same as l)
    #  c: register  input: text column  (may be same as l)
    #  h: register  output: address high
    #  l: register  output: address low (may be same as c, r)
    #  r< 0  0  0  5r 4r 3r 2r 1r  c< 0  0  0  5c 4c 3c 2c 1c
    #  h> 0  1  0  5r 4r 0  0  0   l< 3r 2r 1r 5c 4c 3c 2c 1c
    def rctoscr(r, c, h, l) # 47
      if r == h or [r, c, h, l].include?(a) or [r, c, h].uniq.size != 3 or h == l
        raise ArgumentError, "rctoscr invalid arguments!"
      end
      ns do
            ld   a, r
            anda 0b00011000
            ld   h, a       # h= 0  0  0  a  a  0  0  0
            xor  r          # a= 0  0  0  0  0  r  r  r
            3.times {rrca}  # a= r  r  r  0  0  0  0  0
            ora  c
            ld   l, a       # l= r  r  r  c  c  c  c  c
            set  6, h       # h= 0  1  0  a  a  0  0  0
      end
    end
    #  converts row,col text coordinates to attribute address
    #  uses: a, r, c, h, l
    #  r: register  input: text row     (may be same as l)
    #  c: register  input: text column  (may be same as l)
    #  h: register  output: address high
    #  l: register  output: address low (may be same as c, r)
    #  r< 0  0  0  5r 4r 3r 2r 1r  c< 0  0  0  5c 4c 3c 2c 1c
    #  h> 0  1  0  1  1  0  5r 4r  l< 3r 2r 1r 5c 4c 3c 2c 1c
    def rctoattr(r, c, h, l) # 57
      if r == h or [r, c, h, l].include?(a) or [r, c, h].uniq.size != 3 or h == l
        raise ArgumentError, "rctoscr invalid arguments!"
      end
      ns do
            ld   a, r
            3.times {rrca}
            ld   r, a       # h= r r r 0 0 0 a a
            anda 0b00000011
            ora  0b01011000
            ld   h, a       # h= 0 1 0 1 1 0 a a
            xor  0b01011000
            xor  r          # a= r r r 0 0 0 0 0
            ora  c
            ld   l, a       # l= r r r c c c c c
      end
    end
    #  converts hi byte screen address to attribute address
    #  uses: a, h
    #  h: register  input: hi byte screen address
    #  h: register  output:hi byte attr address
    #  h< 0  1  0  2a 1a l  l  l
    #  h> 0  1  0  1  1  0  2a 1a
    def scrtoattr(h) # 30
      ns do
            ld	 a, h unless h == a
            3.times {rrca}  # h= l l l 0 1 ? a a
            anda 0b00001111 # h= 0 0 0 0 1 ? a a
            ora	 0b01010000 # h= 0 1 0 1 1 ? a a
            ld	 h, a unless h == a
      end
    end
  end
  include Z80
end
