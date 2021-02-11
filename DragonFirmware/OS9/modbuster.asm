* Note: This is set up to run under level 1 as well. It was also written in C originally
* Edition 2 (LCB 09/29/2020):
*   - Added printing filenames to screen as they are (attempting to be) created.
*   - Error out if it is trying to overwrite a file with a File Already Exists error. This
*     is mainly for merged files that are named the same as one of the modules within - the
*     old version would overwrite the source file, thus wiping out all other modules past that
*     point (and you lost the original merged file altogether).
* This is not fully commented; I just commented bits that I needed to add my patches. But the
*  source is fixed to properly assemble when inserting new code now.
         nam   modbuster
         ttl   program module       

* Disassembled 2018/06/17 17:52:31 by Disasm v1.5 (C) 1988 by RML

       ifp1
         use   /dd/defs/deffile
       endc
         
tylg     set   Prgrm+Objct   
atrv     set   ReEnt+rev
rev      set   $01

L0000    mod   eom,name,tylg,atrv,start,size

u0000    rmb   1
u0001    rmb   1
u0002    rmb   2
u0004    rmb   2
u0006    rmb   2
u0008    rmb   2
u000A    rmb   1
u000B    rmb   2
u000D    rmb   39
u0034    rmb   157
u00D1    rmb   2
u00D3    rmb   58           Series of 16 bit pointers to each parameter string
u010D    rmb   1
u010E    rmb   3
u0111    rmb   28
u012D    rmb   34           Module name we are currently creating/writing out
u014F    rmb   $2380        Actual module itself we are currently creating/writing out
size     equ   .

name     fcs   /modbuster/
         fcb   $02            Edition #2

* Copy X bytes from Y to U
L0017    lda   ,y+
         sta   ,u+
         leax  -1,x
         bne   L0017
         rts

* First, copy pre-initilized data from table in program to data memory
start    pshs  y              Save ptr to end of parameter area
         pshs  u              Save ptr to start of data memory
         clra  
         clrb  
L0026    sta   ,u+            Clear direct page
         decb  
         bne   L0026
* LCB note: change next 3 lines to ldu ,s / leax >$014F,u
         ldx   ,s             Get ptr to start of data memory
         leau  ,x             Point U to it as well
         leax  >$014F,x       Point to end of internal buffer to initialize
         pshs  x              Save that
         leay  >L093D,pcr     Point to data initialization table
         ldx   ,y++           Get size of data block
         beq   L0041          If $0000, we are done copying current pre-initialized data; skip ahead
         bsr   L0017          If not, copy X # bytes from table to data area
         ldu   2,s            Point to start of data mem again
L0041    leau  >u0001,u       ? Bump up by 1
         ldx   ,y++           Get size of next data block
         beq   L004C          end of inited data table completely, skip ahead
         bsr   L0017          Copy next block
         clra                 And pad the rest with 0's
L004C    cmpu  ,s             After copy, have we hit the end of the initialized data "to" address?
         beq   L0055          Yes, we are done
         sta   ,u+            No, clear a byte until we finish the entire initialized data space
         bra   L004C

L0055    ldu   2,s            Get start of data mem ptr back
         ldd   ,y++           Get offset to data (from module start)
         beq   L0062          Flag that we are done, skip ahead
         leax  >L0000,pcr     Point to start of module itself
         lbsr  L0165          Go preinit randomly placed 2 byte integers
L0062    ldd   ,y++           Get final pre-init
         beq   L006B          None, skip ahead           
         leax  ,u             
         lbsr  L0165          Go preinit randomly placed 2 byte integers
L006B    leas  4,s            Eat temps
         puls  x              Get end of param area ptr back
         stx   >u0111,u       Save it
         sty   >u00D1,u       Save source ptr into data tables?
         ldd   #$0001
         std   >u010D,u
         leay  >u00D3,u       Point to individual parm ptr list  
         leax  ,s             Point X to start of parameter area
         lda   ,x+            Get byte from params
* Main param processing loop
L0087    ldb   >u010E,u       Get # of params (maybe?) already gotten? Or size of parameter?
         cmpb  #$1D           Max of 30 
         beq   L00E3          If 1D, skip ahead
L008F    cmpa  #C$CR          End of parms?
         beq   L00E3          Yes, done skip ahead
         cmpa  #C$SPAC        space?
         beq   L009B          Yes, eat it and get next parm byte
         cmpa  #',            Comma?
         bne   L009F          No, further parse
L009B    lda   ,x+            Get next parm byte & check it
         bra   L008F

* Check if parameter is quoted (ex. "parm")
L009F    cmpa  #'"          Double quote?
         beq   L00A7        Yes, skip ahead
         cmpa  #$27         Single quote?
         bne   L00C5        No, skip ahead
L00A7    stx   ,y++         If a quote of some kind, save ptr to byte after quote
         inc   >u010E,u     Inc either # of parms or size of parm
         pshs  a            Save quote char
L00AF    lda   ,x+          Get next parm byte
         cmpa  #C$CR        End of parms?
         beq   L00B9        Yes, skip ahead
         cmpa  ,s           Same as previous start quote char?
         bne   L00AF        No, keep checking bytes till we find close quote or CR
L00B9    puls  b            Yes, found end of quoted area, eat start quote char copy
         clr   -$01,x       NUL where the close quote was
         cmpa  #C$CR        Did we finish parms?
         beq   L00E3        Yes, go process parms
         lda   ,x+          No, get next parm byte and keep processing
         bra   L0087

L00C5    leax  -1,x         Not special char, found start of legit parameter, point to start of it
         stx   ,y++         Save ptr into table
         leax  1,x          Bump ptr back up to next char
         inc   >u010E,u     Inc size of parm?
L00CF    cmpa  #C$CR        Was last char end of all parms?
         beq   L00DF        Yes, replace CR with NUL and continue processing
         cmpa  #C$SPAC      No, was it a space?
         beq   L00DF        Yes, replace CR with NUL and continue processing
         cmpa  #',          No, was it a comma?
         beq   L00DF        Yes, replace with NUL and continue processing
         lda   ,x+          No, get next char
         bra   L00CF        and continue looking for parm separator or end of parms (CR)

L00DF    clr   -$01,x       Mark end of an individual parameter with NUL
         bra   L0087        And continue processing

* Parms all marked
L00E3    leax  >u00D1,u     Point to individual parm string ptr table
         pshs  x            Save it
         ldd   >u010D,u     Get either size of parm or # of parms (I think #)
         pshs  d            Save that
         leay  ,u           Point y to start of data mem
         bsr   L00FD
         lbsr  L017F
         clr   ,-s
         clr   ,-s
         lbsr  L0931
L00FD    leax  >$014F,y
         stx   >$011B,y
         sts   >$010F,y
         sts   >$011D,y
         ldd   #$FF82       -126 offset
L0112    leax  d,s          Bump S down by either 126 or -76
         cmpx  >$011D,y
         bhs   L0124
         cmpx  >$011B,y     Ran out of stack space?
         blo   L013E        Yes, return error
         stx   >$011D,y     No, save new check point
L0124    rts   

L0125    fcc   '**** STACK OVERFLOW ****'
         fcb   C$CR

L013E    leax  <L0125,pcr   Point to error message
         ldb   #$CF         Process memory full error
         pshs  b            Save on stack
         lda   #$02         Std Error path
         ldy   #100         Max size of message
L014B    os9   I$WritLn     Write 'stack overflow text' to stderr
         clr   ,-s
         lbsr  L0937
L0153    ldd   >$010F,y
         subd  >$011D,y
         rts   

L015C    ldd   >$011D,y
         subd  >$011B,y
L0164    rts   

* Copy pre-init data that isn't contiguous, but random bits within data mem and/or
*   the source code. I think.
* Entry: X=ptr within module Ptr to some spot in the module itself
*        D=Offset into module
*        Y=Ptr to pre-init table position
*        U=Ptr to data mem (I think)
L0165    pshs  x            Save ptr
         leax  d,y          Offset set dest ptr         
         leax  d,x          Offset source ptr
         pshs  x            Save updated ptr
L016D    ldd   ,y++         Get offset from pre-init table
         leax  d,u          Add to dest ptr
         ldd   ,x           Get another offset
         addd  2,s          Add to original ptr within module we came in with
         std   ,x           Save @ destination
         cmpy  ,s           Are we done pre-init table?
         bne   L016D        No, keep copying individual 2 byte chunks
         leas  4,s          Yes, eat temps & return
L017E    rts   

L017F    pshs  u
         ldd   #$FFB4       -76
         lbsr  L0112
         leas  -2,s
         ldd   6,s          Get # of parms (?)
         cmpd  #$0001       1 or less?
         ble   L01B6        Yes, print help message
         ldd   #$0001
         pshs  d
         ldx   $0A,s
         ldd   $02,x
         pshs  d
         lbsr  L0762
         leas  4,s
         std   >$0129,y
         cmpd  #$FFFF       -1?
         bne   L01C1
         ldd   >$011F,y
         pshs  d
         lbsr  L0931
         bra   L01BF

L01B6    leax  >L02DC,pcr   Point to help message
         pshs  x
         lbsr  L02F5
L01BF    leas  2,s
L01C1    ldd   #$07D0
         pshs  d
         lbsr  L08F6
         leas  $02,s
         std   >$0123,y
         cmpd  #$FFFF
         bne   L01DF
         ldd   #$0066
         pshs  d
         lbsr  L0931
         leas  $02,s
L01DF    ldd   #$07D0
         bra   L01EB

L01E4    ldd   >$0121,y     
         addd  #$0100
L01EB    std   >$0121,y
         ldd   #$0100
         pshs  d
         lbsr  L08F6
         leas  $02,s
         cmpd  #$FFFF
         bne   L01E4
         ldd   >$0123,y
         std   >$0125,y
         lbra  L02BA

L020A    ldx   >$0125,y
         ldd   $02,x
         cmpd  >$0121,y
         bls   L0221
         ldd   #$0066
         pshs  d
         lbsr  L0931
         leas  $02,s
L0221    ldx   >$0125,y
         ldd   $02,x
         addd  #$FFFC
         pshs  d
         ldd   >$0123,y
         addd  #$0004
         pshs  d
         ldd   >$0129,y
         pshs  d
         lbsr  L07DE
         leas  $06,s
         pshs  d
         ldx   >$0125,y
         ldd   $02,x
         addd  #$FFFC
         cmpd  ,s++
         beq   L025A
         ldd   #$0067
         pshs  d
         lbsr  L0931
         bra   L02B8

L025A    ldd   >$0123,y     Get ptr to actual module data?
         ldx   >$0125,y     And another (maybe for large modules?)
         addd  $04,x        Add offset to module name in module data itself
         pshs  d
         leax  >$012D,y     Point to start of module name
         pshs  x            Save that
         lbsr  L06A5        Copy module name to output filename buffer
         leas  4,s          Eat temps
         ldd   #$0002
         pshs  d
         leax  >$012D,y     Point to output filename buffer
         pshs  x            Save for sub
         lbsr  L0783        Create the file
         leas  $04,s
         std   >$012B,y
         cmpd  #$FFFF
         bne   L0296
         ldd   >$011F,y
         pshs  d
         lbsr  L0931
         leas  $02,s
L0296    ldx   >$0125,y
         ldd   $02,x
         pshs  d
         ldd   >$0123,y
         pshs  d
         ldd   >$012B,y
         pshs  d
         lbsr  L080F
         leas  $06,s
         ldd   >$012B,y
         pshs  d
         lbsr  L0771
L02B8    leas  $02,s
L02BA    ldd   #$0004
         pshs  d
         ldd   >$0123,y
         pshs  d
         ldd   >$0129,y
         pshs  d
         lbsr  L07DE
         leas  $06,s
         cmpd  #$0004
         lbeq  L020A
         leas  $02,s
         puls  pc,u

L02DC    fcc   'use:  modbuster pathname'
         fcb   $00
         
L02F5    pshs  u
         leax  >$000E,y
         pshs  x
         ldd   6,s
         pshs  d
         bsr   L0317
         leas  4,s
         leax  >$000E,y
         pshs  x
         ldd   #$000D
         pshs  d
         lbsr  L0337
         leas  $04,s
         puls  pc,u

L0317    pshs  u
         ldu   $04,s
         leas  -$01,s
         bra   L032D

L031F    ldd   $07,s
         pshs  d
         ldb   $02,s
         sex   
         pshs  d
         lbsr  L0337
         leas  $04,s
L032D    ldb   ,u+
         stb   ,s
         bne   L031F
         leas  $01,s
         puls  pc,u

L0337    pshs  u
         ldu   $06,s
         ldd   u0006,u
         anda  #$80
         andb  #$22
         cmpd  #$8002
         beq   L035B
         ldd   u0006,u
         clra  
         andb  #$22
         cmpd  #$0002
         lbne  L0473
         pshs  u
         lbsr  L0552
         leas  $02,s
L035B    ldd   u0006,u
         clra  
         andb  #$04
         beq   L0397
         ldd   #$0001
         pshs  d
         leax  $07,s
         pshs  x
         ldd   u0008,u
         pshs  d
         ldd   u0006,u
         clra  
         andb  #$40
         beq   L037C
         leax  >L0828,pcr
         bra   L0380

L037C    leax  >L080F,pcr
L0380    tfr   x,d          This is retarded. Should only need first transfer (and possibly neither?)
         tfr   d,x
         jsr   ,x
         leas  $06,s
         cmpd  #$FFFF
         bne   L03D8
         ldd   u0006,u
         orb   #$20
         std   u0006,u
         lbra  L0473

L0397    ldd   u0006,u
         anda  #$01
         clrb  
         std   -$02,s
         bne   L03A7
         pshs  u
         lbsr  L0490
         leas  $02,s
L03A7    ldd   ,u
         addd  #$0001
         std   ,u
         subd  #$0001
         tfr   d,x
         ldd   $04,s
         stb   ,x
         ldd   ,u
         cmpd  u0004,u
         bcc   L03CD
         ldd   u0006,u
         clra  
         andb  #$40
         beq   L03D8
         ldd   $04,s
         cmpd  #$000D
         bne   L03D8
L03CD    pshs  u
         lbsr  L0490
         std   ,s++
         lbne  L0473
L03D8    ldd   $04,s
         puls  pc,u

L03DC    pshs  u
         ldu   $04,s
         ldd   $06,s
         pshs  d
         pshs  u
         ldd   #$0008
         lbsr  L06D0
         pshs  d
         lbsr  L0337
         leas  $04,s
         ldd   $06,s
         pshs  d
         pshs  u
         lbsr  L0337
         lbra  L054A

L03FF    pshs  u,d
         leau  >$0001,y     Should be 5 bit offset
         clra  
         clrb  
         std   ,s
         bra   L0415

L040B    tfr   u,d
         leau  u000D,u
         pshs  d
         bsr   L0428
         leas  $02,s
L0415    ldd   ,s
         addd  #$0001
         std   ,s
         subd  #$0001
         cmpd  #$0010
         blt   L040B
         lbra  L048C

L0428    pshs  u
         ldu   4,s
         leas  -2,s
         cmpu  #$0000
         beq   L0438
         ldd   u0006,u
         bne   L043E
L0438    ldd   #$FFFF
         lbra  L048C

L043E    ldd   u0006,u
         clra  
         andb  #$02
         beq   L044D
         pshs  u
         bsr   L0462
         leas  $02,s
         bra   L044F

L044D    clra  
         clrb  
L044F    std   ,s
         ldd   u0008,u
         pshs  d
         lbsr  L0771
         leas  $02,s
         clra  
         clrb  
         std   u0006,u
         ldd   ,s
         bra   L048C

L0462    pshs  u
         ldu   $04,s
         beq   L0473
         ldd   u0006,u
         clra  
         andb  #$22
         cmpd  #$0002
         beq   L0478
L0473    ldd   #$FFFF
         puls  pc,u

L0478    ldd   u0006,u
         anda  #$80
         clrb  
         std   -$02,s
         bne   L0488
         pshs  u
         lbsr  L0552
         leas  $02,s
L0488    pshs  u
         bsr   L0490
L048C    leas  $02,s
         puls  pc,u

L0490    pshs  u
         ldu   $04,s
         leas  -$04,s
         ldd   u0006,u
         anda  #$01
         clrb  
         std   -$02,s
         bne   L04C2
         ldd   ,u
         cmpd  u0004,u
         beq   L04C2
         clra  
         clrb  
         pshs  d
         pshs  u
         lbsr  L054E
         leas  $02,s
         ldd   $02,x
         pshs  d
         ldd   ,x
         pshs  d
         ldd   u0008,u
         pshs  d
         lbsr  L0838
         leas  $08,s
L04C2    ldd   ,u
         subd  u0002,u
         std   $02,s
         lbeq  L053A
         ldd   u0006,u
         anda  #$01
         clrb  
         std   -$02,s
         lbeq  L053A
         ldd   u0006,u
         clra  
         andb  #$40
         beq   L0511
         ldd   u0002,u
         bra   L0509

L04E2    ldd   $02,s
         pshs  d
         ldd   ,u
         pshs  d
         ldd   u0008,u
         pshs  d
         lbsr  L0828
         leas  $06,s
         std   ,s
         cmpd  #$FFFF
         bne   L04FF
         leax  $04,s
         bra   L0529

L04FF    ldd   $02,s
         subd  ,s
         std   $02,s
         ldd   ,u
         addd  ,s
L0509    std   ,u
         ldd   $02,s
         bne   L04E2
         bra   L053A

L0511    ldd   $02,s
         pshs  d
         ldd   u0002,u
         pshs  d
         ldd   u0008,u
         pshs  d
         lbsr  L080F
         leas  $06,s
         cmpd  $02,s
         beq   L053A
         bra   L052B

L0529    leas  -$04,x
L052B    ldd   u0006,u
         orb   #$20
         std   u0006,u
         ldd   u0004,u
         std   ,u
         ldd   #$FFFF
         bra   L054A

L053A    ldd   u0006,u
         ora   #$01
         std   u0006,u
         ldd   u0002,u
         std   ,u
         addd  u000B,u
         std   u0004,u
         clra  
         clrb  
L054A    leas  $04,s
         puls  pc,u

L054E    pshs  u
         puls  pc,u

L0552    pshs  u
         ldu   $04,s
         ldd   u0006,u
         clra  
         andb  #$C0
         bne   L058A
         leas  <-$20,s
         leax  ,s
         pshs  x
         ldd   u0008,u
         pshs  d
         clra  
         clrb  
         pshs  d
         lbsr  L06F3
         leas  $06,s
         ldd   u0006,u
         pshs  d
         ldb   $02,s
         bne   L057E
         ldd   #$0040
         bra   L0581

L057E    ldd   #$0080
L0581    ora   ,s+
         orb   ,s+
         std   u0006,u
         leas  <$20,s
L058A    ldd   u0006,u
         ora   #$80
         std   u0006,u
         clra  
         andb  #$0C
         beq   L0597
         puls  pc,u

L0597    ldd   u000B,u
         bne   L05AC
         ldd   u0006,u
         clra  
         andb  #$40
         beq   L05A7
         ldd   #$0080
         bra   L05AA

L05A7    ldd   #$0100
L05AA    std   u000B,u
L05AC    ldd   u0002,u
         bne   L05C1
         ldd   u000B,u
         pshs  d
         lbsr  L08F6
         leas  $02,s
         std   u0002,u
         cmpd  #$FFFF
         beq   L05C9
L05C1    ldd   u0006,u
         orb   #$08
         std   u0006,u
         bra   L05D8

L05C9    ldd   u0006,u
         orb   #$04
         std   u0006,u
         leax  u000A,u
         stx   u0002,u
         ldd   #$0001
         std   u000B,u
L05D8    ldd   u0002,u
         addd  u000B,u
         std   u0004,u
         std   ,u
         puls  pc,u

L05E2    pshs  u
         ldu   $06,s
         leas  -$02,s
         ldd   $06,s
         std   ,s
L05EC    ldd   $0A,s
         addd  #$FFFF
         std   $0A,s
         subd  #$FFFF
         ble   L0610
         ldb   ,u+
         ldx   ,s
         leax  $01,x
         stx   ,s
         stb   -$01,x
         bne   L05EC
         bra   L0610

L0606    clra  
         clrb  
         ldx   ,s
         leax  $01,x
         stx   ,s
         stb   -$01,x
L0610    ldd   $0A,s
         addd  #$FFFF
         std   $0A,s
         subd  #$FFFF
         bgt   L0606
         lbra  L069F

L061F    pshs  u
         ldu   $04,s
         bra   L0635

L0625    ldx   $06,s
         leax  $01,x
         stx   $06,s
         ldb   -$01,x
         bne   L0633
         clra  
         clrb  
         puls  pc,u
L0633    leau  u0001,u
L0635    ldd   $08,s
         addd  #$FFFF
         std   $08,s
         subd  #$FFFF
         ble   L064F
         ldb   ,u
         sex   
         pshs  d
         ldb   [<$08,s]
         sex   
         cmpd  ,s++
         beq   L0625
L064F    ldd   $08,s
         bge   L0657
         clra  
         clrb  
         bra   L0662

L0657    ldb   [<$06,s]
         sex   
         pshs  d
         ldb   ,u
         sex   
         subd  ,s++
L0662    puls  pc,u

L0664    pshs  u
         ldu   $06,s
         leas  -$02,s
         ldd   $06,s
         std   ,s
L066E    ldx   ,s
         leax  $01,x
         stx   ,s
         ldb   -$01,x
         bne   L066E
         ldd   ,s
         addd  #$FFFF
         std   ,s
L067F    ldd   $0A,s
         addd  #$FFFF
         std   $0A,s
         subd  #$FFFF
         ble   L0697
         ldb   ,u+
         ldx   ,s
         leax  $01,x
         stx   ,s
         stb   -$01,x
         bne   L067F
L0697    ldd   $0A,s
         bge   L069F
         clra  
         clrb  
         stb   [,s]
L069F    ldd   $06,s
         leas  $02,s
         puls  pc,u

* Copy module name from module itself to (2,s) filename buffer to create
* Entry: 0-1,s = RTS address
*        2-3,s = Ptr to filename (module name) (where it is getting copied to)
*        4-5,s = Ptr to module name within module data itself
* Exit: U preserved
*       D = Ptr to filename
L06A5    pshs  u            Preserve U
         ldu   2+2,s        Get ptr to filename
L06A9    ldx   2+4,s        Get ptr to module name inside of module
         leax  1,x          Inc ptr
         stx   2+4,s        Save back
         ldb   -1,x         Get char from module name
         stb   ,u+          Save in output filename buffer
         bgt   L06A9        No high bit, keep copying filename
         lda   -1,u         Get last char of module name (hi bit set) again
         anda  #$7F         Mask off high bit
         ldb   #C$CR        And append CR
         std   -1,u         Save fixed version & CR
         ldd   2+2,s        Get ptr to filename back
         puls  pc,u         Restore U & return

L06C4    tstb  
         beq   L06DA
L06C7    asr   $02,s
         ror   $03,s
         decb  
         bne   L06C7
         bra   L06DA

L06D0    tstb  
         beq   L06DA
L06D3    lsr   $02,s
         ror   $03,s
         decb  
         bne   L06D3
L06DA    ldd   $02,s
         pshs  d
         ldd   $02,s
         std   $04,s
         ldd   ,s
         leas  $04,s
         rts   

L06E7    tstb  
         beq   L06DA
L06EA    lsl   $03,s
         rol   $02,s
         decb  
         bne   L06EA
         bra   L06DA

L06F3    lda   $05,s
         ldb   $03,s
         beq   L0726
         cmpb  #$01
         beq   L0728
         cmpb  #$06
         beq   L0728
         cmpb  #$02
         beq   L070E
         cmpb  #$05
         beq   L070E
         ldb   #$D0
         lbra  L0923

* Entry: A=path
*        B=GetStat code
*        other regs as needed
L070E    pshs  u
         os9   I$GetStt 
         bcc   L071A
         puls  u
         lbra  L0923

L071A    stx   [<$08,s]
         ldx   $08,s
         stu   $02,x
         puls  u
         clra  
         clrb  
         rts   

* Entry: A=path
*        B=GetStat code
*        other regs as needed
L0726    ldx   $06,s
L0728    os9   I$GetStt 
         lbra  L092C

L072E    lda   $05,s
         ldb   $03,s
         beq   L073D
         cmpb  #$02
         beq   L0745
         ldb   #$D0
         lbra  L0923

* Entry: A=path
*        B=SetStat code
*        other regs as needed
L073D    ldx   $06,s
         os9   I$SetStt 
         lbra  L092C

L0745    pshs  u
         ldx   $08,s
         ldu   $0A,s
         os9   I$SetStt 
         puls  u
         lbra  L092C

L0753    ldx   $02,s        Get ptr to path/filename
         lda   $05,s        Get access mode
         os9   I$Open       Open the file or path
         bcs   L075F        Doesn't exist, skip ahead
         os9   I$Close      Close the file/path
L075F    lbra  L092C

L0762    ldx   $02,s        Get ptr to path/filename
         lda   $05,s        Get access mode
         os9   I$Open       Open the file
         lbcs  L0923        If error, save error code
         tfr   a,b          No error, make D=path # & return
         clra  
         rts   

L0771    lda   $03,s        Get path #
         os9   I$Close      Close the file
         lbra  L092C

L0779    ldx   $02,s        Get ptr to Directory name we are creating
         ldb   $05,s        Get directory attributes
         os9   I$MakDir     Create the directory
         lbra  L092C        Return clean or with error

creattxt fcc   'Creating '
creatsz  equ   *-creattxt

L0783    ldx   $02,s        Get ptr to filename to create
* NEW CODE LCB
         pshs  y,x,d        Save regs
         leax  creattxt,pc  Point to prefix message
         ldy   #creatsz     Size of prefix
         lda   #1           Std out
         os9   I$Write      Send to screen (we are ignoring errors)
         lda   #1           Std out
         ldy   #29          Max size of a filename
         ldx   6+2,s        Get ptr to filename
         os9   I$WritLn     Write it out with CR
         puls  d,x,y        Restore regs
         lda   $05,s        Get access mode
         tfr   a,b          Dupe into B
         andb  #EXEC.+PEXEC.  %00100100 ($24) Keep only both EXEC attributes
         orb   #PREAD.+UPDAT. %00001011 ($0B) Add public read, and owner read/write
         os9   I$Create     Create the file
         bcs   L0796        If error, skip ahead
L0792    tfr   a,b          Successful create, return with D=path #
         clra  
         rts   

L0796    lbra  L0923        Return with error (including file exists)

L07C9    ldx   $02,s        Get ptr to filename to delete
         os9   I$Delete     Attempt to delete it
         lbra  L092C        Return cleanly or with error

L07D1    lda   $03,s        Get path #
         os9   I$Dup        Make duplicate path
         lbcs  L0923        Error, exit with it
         tfr   a,b          Otherwise, return with new path # in D
         clra  
         rts   

L07DE    pshs  y
         ldx   $06,s        Get ptr to buffer to read into
         lda   $05,s        Get path # to read from
         ldy   $08,s        Get # of bytes to read
         pshs  y            Save that
         os9   I$Read       Read the data
L07EC    bcc   L07FB        No error, return with size read in D
         cmpb  #E$EOF       Was error End of File?
         bne   L07F6        No, return with error
         clra               yes, return with no error
         clrb  
         puls  pc,y,x       

L07F6    puls  y,x
         lbra  L0923

L07FB    tfr   y,d
         puls  pc,y,x

L07FF    pshs  y
         lda   $05,s        Get path #
         ldx   $06,s        Get ptr to buffer to read into
         ldy   $08,s        Get # of bytes max to read for a line
         pshs  y
         os9   I$ReadLn     Read line in
         bra   L07EC        Handle good read or error

L080F    pshs  y
         ldy   $08,s
         beq   L0824
         lda   $05,s
         ldx   $06,s
         os9   I$Write  
L081D    bcc   L0824
         puls  y
         lbra  L0923

L0824    tfr   y,d
         puls  pc,y

L0828    pshs  y
         ldy   $08,s
         beq   L0824
         lda   $05,s
         ldx   $06,s
         os9   I$WritLn 
         bra   L081D

L0838    pshs  u
         ldd   $0A,s
         bne   L0846
         ldu   #$0000
         ldx   #$0000
         bra   L087A

L0846    cmpd  #$0001
         beq   L0871
         cmpd  #$0002
         beq   L0866
         ldb   #$F7
L0854    clra  
         std   >$011F,y
         ldd   #$FFFF
         leax  >$0113,y
         std   ,x
         std   $02,x
         puls  pc,u

L0866    lda   $05,s
         ldb   #$02
         os9   I$GetStt 
         bcs   L0854
         bra   L087A

L0871    lda   $05,s
         ldb   #$05
         os9   I$GetStt 
         bcs   L0854
L087A    tfr   u,d
         addd  $08,s
         std   >$0115,y
         tfr   d,u
         tfr   x,d
         adcb  $07,s
         adca  $06,s
         bmi   L0854
         tfr   d,x
         std   >$0113,y
         lda   $05,s
         os9   I$Seek   
         bcs   L0854
         leax  >$0113,y
         puls  pc,u

L089F    ldd   >$0111,y
         pshs  d
         ldd   $04,s
         cmpd  >$014D,y
         bcs   L08D3
         addd  >$0111,y
         pshs  y
         subd  ,s
         os9   F$Mem    
         tfr   y,d
         puls  y
         bcc   L08C5
         ldd   #$FFFF
         leas  $02,s
         rts   

L08C5    std   >$0111,y
         addd  >$014D,y
         subd  ,s
         std   >$014D,y
L08D3    leas  $02,s
         ldd   >$014D,y
         pshs  d
         subd  $04,s
         std   >$014D,y
         ldd   >$0111,y
         subd  ,s++
         pshs  d
         clra  
         ldx   ,s
L08EC    sta   ,x+
         cmpx  >$0111,y
         bcs   L08EC
         puls  pc,d

L08F6    ldd   $02,s
         addd  >$011B,y
         bcs   L091F
         cmpd  >$011D,y
         bcc   L091F
         pshs  d
         ldx   >$011B,y
         clra  
L090C    cmpx  ,s
         bcc   L0914
         sta   ,x+
         bra   L090C

L0914    ldd   >$011B,y
         puls  x
         stx   >$011B,y
         rts   

L091F    ldd   #$FFFF
         rts   

* Entry: B=error code
L0923    clra               Make error code 16 bit
         std   >$011F,y     Save it
         ldd   #$FFFF       Return with D=-1 to flag error
         rts   

L092C    bcs   L0923        If error, save error number & flag error on return (D=-1) 
         clra               No error, return with D=0
         clrb  
         rts   

L0931    lbsr  L093C
         lbsr  L03FF
L0937    ldd   $02,s
         os9   F$Exit 
L093C    rts

* Pre-initialized data table:
L093D    fdb   $0001        1 byte data to preinit
         fcb   $00          Data byte
         fdb   $00D0        208 bytes of data to pre-init
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   1
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   2
         fcb   0
         fcb   1
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   66
         fcb   0
         fcb   2
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fcb   0
         fdb   $0000        End of pre-init data flag
         fdb   $0000        ???
         
L0A16    fcs   'modbuste:T'
         emod
eom      equ   *
         end
