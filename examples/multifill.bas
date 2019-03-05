     DEF FN p$(x,y)=CHR$ x+CHR$ (175-y): DEF FN i()=CODE INKEY$#8+256*CODE INKEY$#8
   5 BORDER VAL "7": PAPER VAL "7": INK VAL "0": BRIGHT VAL "0": OVER VAL "0": INVERSE VAL "0": FLASH VAL "0": CLS
     RANDOMIZE FN o(8,"F")
     GO SUB 1000: STOP 
   9 REM #demo 1
  10 LET total=USR api: RANDOMIZE USR fill: LET stack=2*INT (total/18)
     BRIGHT 1: OVER 0: BORDER 7: PAPER 7: INK 4: CLS
  20 LET r=175/6: FOR i=0 TO 2: LET x=256*(i+.5)/3: FOR j=0 TO 2: LET y=175*(j+.5)/3: CIRCLE x,y,r: NEXT j: NEXT i
     PLOT 0,0: DRAW 255,0: 
  30 LET a$="": FOR i=0 TO 2: LET x=256*(i+.5)/3: FOR j=0 TO 2: LET y=175*(j+.5)/3: GO SUB 2000: NEXT j: NEXT i: GO SUB 3000
     RANDOMIZE FN w(8,n*4): GO SUB 4000
     LET a$="": LET stack=2*INT (total/6): GO SUB 3000: LET n=0
     LET d=r*2: FOR i=0 TO 2: LET x=256*(i+.5)/3: FOR j=0 TO 2: LET y=175*(j+.5)/3: PLOT x-r,y-r: DRAW d,0: DRAW 0,d: DRAW -d,0: DRAW 0,-d: NEXT j: PRINT #8;FN p$(i*256/3,87);: LET n=n+1: NEXT i
     PRINT #8;FN p$(250,87);#2;: LET n=n+1
     BORDER 4
     STOP
     INK 0: RANDOMIZE FN w(8,n*4): PRINT AT 0,0;INVERSE 1;"pixels in circles: ";pixels
     GO SUB 4000
     PRINT "pixels in columns: ";pixels
     GO SUB 1000: STOP 
  99 REM #demo 2
 100 LET total=USR api: RANDOMIZE USR fill: LET stack=2*INT (total/2)
     BORDER 7: PAPER 7: INK 1: BRIGHT 0: OVER 0: CLS 
     CIRCLE 128,87.5,88
     OVER 1: PRINT #8;FN p$(128,87);#2;: LET tid=FN m(fill,stack): PLOT 61,38: DRAW 115,115,10000
     PLOT 0,0: DRAW 255,0: PLOT 0,87: DRAW 255,0: OVER 0
 110 RANDOMIZE FN w(8,4): GO SUB 4000: PRINT AT 0,0;INK 0;INVERSE 1;"Pixels filled: ";pixels
     LET stack=2*INT (total/8)
     LET a$="": LET x=20: LET y=40: GO SUB 2000: LET y=127: GO SUB 2000: LET x=235: GO SUB 2000: LET y=40: GO SUB 2000: GO SUB 3000
     STOP
     INK 0: RANDOMIZE FN w(8,n*4): GO SUB 4000: PRINT "Pixels around: ";pixels
     GO SUB 1000: STOP 
 199 REM #demo 3 - single task
 200 RESTORE : RANDOMIZE USR api: BRIGHT 1: OVER 0: BORDER 7: PAPER 7: INK 0: CLS 
     LET pixels=0
     FOR i=1 TO 8: READ x,y: PLOT x,y: READ x,y: DRAW x,y: READ x,y: DRAW x,y: READ x,y: DRAW x,y: READ x,y: LET pixels=pixels+FN l(x,y): NEXT i
     PLOT 41,0: DRAW 0,175: PLOT 215,175: DRAW 0,-175: DRAW -174,0: FOR i=1 TO 4: READ x,y: LET pixels=pixels+FN l(x,y): NEXT i
     PRINT AT 0,6;OVER 1;"Pixels: ";pixels
 250 DATA 128,87,-30,50,60,0,-30,-50,128,119
     DATA 215,175,-57,-38,20,-20,37,58,188,147
     DATA 128,87,50,30,0,-60,-50,30,158,87
     DATA 215,0,-57,38,20,20,37,-58,188,27
     DATA 128,87,-30,-50,60,0,-30,50,128,55
     DATA 41,0,57,38,-20,20,-37,-58,68,27
     DATA 128,87,-50,30,0,-60,50,30,98,87
     DATA 41,175,57,-38,-20,-20,-37,58,68,147
     DATA 128,165,205,87,128,10,51,87
     GO SUB 1000: STOP 
 299 REM #demo 3 - multitask
 300 RESTORE : LET total=USR api: RANDOMIZE USR fill: OVER 0: BRIGHT 1: PAPER 7: BORDER 7: INK 2: CLS
     LET a$="": LET stack=2*INT (total/8): GO SUB 3000
     LET n=8: FOR i=1 TO n: READ x,y: PLOT x,y: READ x,y: DRAW x,y: READ x,y: DRAW x,y: READ x,y: DRAW x,y: READ x,y: PRINT #8;FN p$(x,y);: NEXT i
 320 PLOT 41,0: DRAW 0,175: PLOT 215,175: DRAW 0,-175: DRAW -174,0
     LET m=4: FOR i=1 TO m: READ x,y: PRINT #8;FN p$(x,y);#2;: NEXT i
     STOP
     RANDOMIZE FN w(8,(n+m)*4): GO SUB 4000: PRINT AT 0,6;OVER 1;"Pixels: ";pixels
     GO SUB 1000: STOP 
 399 REM #demo 4
 400 LET total=USR api: RANDOMIZE USR fill: BRIGHT 1: OVER 0: INK 2: PAPER 6: BORDER 6: CLS 
     LET a$="": LET stack=2*INT (total/6): GO SUB 3000
     LET x=128: LET y=87: CIRCLE x,y,35: CIRCLE x,y,60: PRINT #8;FN p$(x,y);FN p$(168,y);: CIRCLE x,y,y: PRINT #8;FN p$(58,y);#2;
     STOP
     INK 0: RANDOMIZE FN w(8,n*4): GO SUB 4000: PRINT AT 0,0;INVERSE 1;"Pixels: ";pixels
 999 GO SUB 1000: STOP : GO TO 10
1000 PRINT "System free:",FN z(),"Multitask free:",FN f()''"Enter`CONTINUE`to go on...": RETURN
2000 LET a$=a$+FN p$(x,y): RETURN 
3000 LET n=INT (total/stack)
3010 FOR i=1 TO n: LET tid=FN m(fill,stack): NEXT i
3020 PRINT #8;a$;#2;: RETURN 
4000 LET pixels=0
4010 LET len=FN w(8,0): IF len>=4 THEN RANDOMIZE FN t(FN i()): LET pixels=pixels+FN i(): GO TO 4010
     RETURN 