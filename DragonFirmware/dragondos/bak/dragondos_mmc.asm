;
; DragonDos 1.0, Copyright (C) 1982, Dragon Data Ltd.
;
; Disassembled 28/10/2004, P.Harvey-Smith.
;
; Ported to the Dragon Alpha/Professional hardware,
; 29/10/2004 P.Harvey-Smith.
;

;
; 2016-03-17, PHS.
; Updated to include all the bugfixes from DOS13.ROM
;

;
; The file RomDefs.asm contains a set of definitions for calling the ROM routines
; of either the Dragon or CoCo machines, these will be assembled conditionally
; depending on weather Dragon or Tandy is defined.
;

;		ifp1
;		use	cpudefs.asm
;		use	dgndefs.asm
;		use	dosdefs.asm
;		use	romdefs.asm
;		use	basictokens.asm
;		use 	basicdefs.asm
;		endc


		ifdef	Tandy
; Compiling to run on Tandy CoCo

NextResJump	EQU	BasStub3+StubResJumpOfs		; Jump to reserved word handler of user table
NextFuncsJump	EQU	BasStub3+StubFuncsJumpOfs	; Jump to functions handler of user table
		ELSE
; Compiling to run on Dragon 32/64/Alpha
NextResJump	EQU	BasStub2+StubResJumpOfs		; Jump to reserved word handler of user table
NextFuncsJump	EQU	BasStub2+StubFuncsJumpOfs	; Jump to functions handler of user table

		ENDC

;		ORG     $C000
; Disk controler ID, if a cartrage starts with the chars 'DK', then the basic rom routines
; will do a JMP to $C002 to init the cartrage.

;DC000   FCC     /DK/				

;LC002   BRA     DosInit

; Indirect jump table, these are probably the official DragonData entry
; points to be called with jsr [entry]

        FDB     DOSLowLevel		; Low Level disk IO routine
        FDB     DosHWByte		; Address of data table for low level command
        FDB     DOSValidFilename	; Validate filename & copy to disk block.
        FDB     DOSOpenFile		; Open A file.
        FDB     DOSCreateFile		; Create file (make backup)
        FDB     DOSGetFlen		; Get file length
        FDB     DOSCloseAll		; Close all open files
        FDB     DOSCloseFile		; Close file
        FDB     DOSFRead		; Read data from file
        FDB     DOSFWrite		; Write data to file
        FDB     DOSGetFree		; Get free space on a disk
        FDB     DOSDeleteFile		; Delete a file
        FDB     DOSProtect		; Protect/unprotect file
        FDB     DOSRename		; Rename a file 
        FDB     DOSGetDirEntry		; Get a directory entry
        FDB     DOSFindAndRead		; Find free buffer and read sector
        FDB     DOSSyncDir		; Copy updated sectors from track 20 to 16 (sync direcories)
        FDB     DOSReadAbsSector	; Read absolute sector
        FDB     DOSWriteAbsSector	; Write absolute sector (no verify)

HEADFLAG        equ     DosHWMaskFF40   ; Reuse for head flag as unused in DragonMMC
;
; Init for MMC
;
MMCInit	LBRA	RealMMCInit
;
; Init Dos
; 

DosInit orcc	#(FlagFIRQ+FlagIRQ)	; Make sure ints disabled
	LDX     #DosAreaStart		; Point to bottom of dos vars area	
        TFR     X,Y		
LC02F   CLR     ,X+			; Clear a byte, increment X
        LEAY    -1,Y			; decrement counter
        BNE     LC02F			; loop again if more to do

; X now points to the top of the dos variable area
	
        TFR     X,D
        TFR     A,B
        ADDB    #$18
        STB     <BasStartProg		; Setup new begining of basic
        JSR     >BasLocateScreen
        LDA     <GrDisplayStartAddr	; Adjust graphics ram pages
        ADDA    #$06
        STA     <GrLastDisplayAddr

;
; Init various low ram stuff, interrupt vectors, basic stub etc
; 
	
        LDX     #DDE1E			; Point to rom copy of data to copy
LC049   LDB     ,X+			; Get byte count byte
        BEQ     LC054			; zero= end of table, exit copy
        LDU     ,X++			; Get destination address
        JSR     >UtilCopyBXtoU		; do copy
        BRA     LC049			; do next


LC054   
	LDB     #$20			; Set hardware control mask
        STB     DosHWMaskFF48
       	COM     DosVerifyFlag		; function unknown 
       
	LDX     #DosNewUSRTable		; Adjust usr vector base
        STX     <BasUSRTableAddr	
        LDU     #BasFCError		; Setup the 10 usr vectors to point to BasFCError
        LDB     #$0A			; do 10

LC066   STU     ,X++			; setup vector
        DECB				; decrement count
        BNE     LC066			; loop again if more to do
	
        INC     DosDefDriveNo	
        BSR     DosReset
	
        LDX     #VectBase		; Point to ram hooks
        LDY     #RamHookTable		; Point to ram hook table in rom
        LDD     #$137E			; load A with number of vectors B with opcode for JMP
LC07A   STB     ,X+	; setup jump
        LDU     ,Y++			; setup vector
        STU     ,X++
        DECA				; decrement counter
        BNE     LC07A			; Loop again if more to do

;
; Attempt to chain rom at $E000 if one exists.
; 
       
;	LDX     #$4549			; Look for DK at $E000
;        CMPX    $E000
;        LBEQ    $E002			; Found it, call it's init routine
	
EIEND   
;	LDX     #ResetVector		; Setup new reset vector
;        STX     <IndVecReset
;        LDA     #$55
;        STA     <WarmStartFlag
	andcc	#~(FlagIRQ+FlagFIRQ)	; reenable inturrupts
	
;        LDX     #BasSignonMess		; Print staandard Basic signon message
;        JSR     >TextOutString

	LDX     #DosSignonMess-1	; Print dos signon message
        JSR     >TextOutString	

	jmp	>MMC_InitDone		; do post init stuff
        
;	JMP	>BasCmdMode		; Go to command mode

LC0A7	STA	DosSecTrkTblOfs,U	; save sectors / track for this drive
	TST	DosSecTrkTblOfs,U	; check what track we are currently on
	BNE	LC0B5			 
	
	JSR	>DosDoRestore
	
	BCC	LC0B5
	LEAS	2,S
LC0B5	RTS
	NOP
	NOP

DosReset   
;	LDA     #WDCmdForceInt		; Force WD2797 to interrupt & reset
;        STA     CMDREG
	
	LDX     #DosD0Online		; Clear drive online flags 
	
	CLRA
	CLRB
	STD	,X
	STD	2,X
;	CLR	<DosIOInProgress
	CLR	<$F7
	
	LDX     #Drv0Details+DrvDetUseCnt	; Clear drive in use counts
        CLR     ,X
        CLR     6,X
        CLR     12,X
        CLR     $12,X
	CLR     <DosIOInProgress 	; Flag to check for timeout
	
        LDX     #DosDirSecStatus 	; Clear Dirctory status, FCBs etc
LC0D9   CLR     ,X+
        CMPX    #DosFCBEnd
        BNE     LC0D9
	
        LDB     #$04			; Count of buffers to process
        LDX     #Buff1Details		; Setup disk buffer initial values
        LDU     #$0800			; addr of buffer
LC0E8   CLR     2,X
        STB     4,X
        STU     5,X
        LEAU    $0100,U			; Increment addr for next buffer
        LEAX    7,X			; do next buffer
        DECB				; done all ?
        BNE     LC0E8			; no : do next
        RTS

;
; The following code is quite clever, there are actually multiple code paths.
; This involves having a 2 byte instruction encoded as the oprand to a 3
; byte instruction eg :-
;
; L00FA	CMPX	#$C605
;
; CMPX is one byte long, so a jump to L00DB, will execute the oprand to the
; CMPX, which decodes as LDB #$05 this saves a few bytes by not having LDB #xx,
; BRA label.
;
; There are several examples of this in the Dos ROM !
;
	
; As several programs make calls to these entry points try and keep them in the same place 
; they are in normal DragonDOS.

	zmb		($C0F8-*)
	
DosDoReadAddr	
	LDB	#DosFnReadAddr		; Read address
	FCB	Skip2	

DosDoWriteTrack   
	LDB	#DosFnWriteTrack	; Write track
	FCB	Skip2	

DosDoWriteSec2   
	LDB	#DosFnWriteSec2 	; Write sector 2 
	FCB	Skip2		

DosDoWriteSec   
	LDB	#DosFnWriteSec		; Write sector
	FCB	Skip2		

DosDoReadSec   
	LDB	#DosFnReadSec		; Read sector

DosDoFuncinB   
	PSHS    A
        LEAS    -2,S			; Make room on stack
        CLR     $0600		
LC10D   LDA     #$03  			; retry count ?
        STD     ,S			; save on stack

LC111   BSR     DosDoSeek		; Try to seek to track
        BCS     LC11E			; Error ?
	
        LDB     1,S			; No error, get dos op code
        STB     <DosHWByte		; Save operation to perform
        JSR     >DOSLowLevel
;	JSR	>CON_DumpRegsWait
	BCC     LC15A			; No error
	
LC11E   CMPB    #DDErrWP		; Write protect ?, no point in retrying !
        BEQ     LC149
        
	DEC     ,S			; Dec retry count
        BEQ     LC13B			; Any retries left ?
        LDB     ,S			; get retry count
        LSRB				; gety lsbit
        BCC     LC133			; on even numbered retry, do recal
        INC     <DskTrackNo		; step out a track
        BSR     DosDoSeek
        DEC     <DskTrackNo		; step back in when retrying 
	BRA     LC111

LC133   LDA     <DskTrackNo		; Save Track no whilst doing restore
        BSR     DosDoRestore		; Restore & Recalibrate
        STA     <DskTrackNo		; Put track no back
        BRA     LC111			; Try again


; We come here when all reties exhausted
LC13B   CMPB    #DDErrNR		; Not ready ?
        BNE     LC157
        TST     DosAreaStart
        BNE     LC157
        COM     DosAreaStart
        BRA     LC153

; Something to do with dir track ? Make sure same op done to both copies of Dir
LC149   LDA     <DskTrackNo		; Get track number
        CMPA    #DirPrimary		; Track 20 ?
        BNE     LC157			; no : error
        LDA     #DirBackup		; set track to 16
        STA     <DskTrackNo
LC153   LDB     1,S			; Get Dos op code
        BRA     LC10D			; So same to track 16

LC157   ORCC    #FlagCarry		; Flag error
        FCB	Skip1			; opcocode for BRN
LC15A   CLRB
        LEAS    2,S			; Drop bytes from stack
        PULS    A,PC			; restore & return

;
; Another LDB, enbeded in CMPX sequence....
; 

DosDoReadSec2
	LDB     #DosFnReadSec2		; Read sector 2 (just first 2 chars)
	FCB	Skip2		

DosDoSeek   	
	LDB	#DosFnSeek
	FCB	Skip2			; Seek to a track

DosDoRestore   
        LDB	#DosFnRestore		; Restore to track 0
        STB     <DosHWByte		; save in hardware byte

;
; Disk Low level access.
;

DOSLowLevel   
	PSHS    CC,A,DP,X,Y,U
;        ORCC    #(FlagIRQ+FlagFIRQ)	; Disable interrupts	
        ROR     ,S
        CLR     <DosDiskError		; Flag no disk error
        LDA     <DosHWByte		; get HW byte
        CMPA    #$07			; Valid op
        BLS     LC179			; yes : do it !
        BRA     LC181			; No: error

LC179   
        JSR     >ResetAndStartDrive	
	bcc     LC18C			; No error : continue
        LDB     #$FD			; yes flag error

        FCB     Skip2			; Andothe CMPX saving.....
	
LC181   LDB     #$FC			; Set error code
        ORCC    #FlagCarry		; and carry bit
        ROL     ,S
        STB     <DosDiskError		; Flag disk error
        LBRA    LC230

; Disable IRQ


LC18C      
; DragonDOS copies the control registers of the PIAs onto the slack here.
; DragonMMC does not need to do this, however we manually move the stack pointer
; so that the stack is setup the same.
	leas	-4,s
	LDA     #$03			; Retry count?
        PSHS    A
   
LC18B        
        LDA     <DskSectorNo		; Get disk sector no
LC1B5   
        JSR     >MMC_SendHR             ; Send head ans sector no

LC1B8   LDY     <Misc16BitScratch
        LDX     <DiskBuffPtr		; Point to buffer
        LDB     <DosHWByte		; Get hardware byte (function code)
        ASLB				; Calculate offset in table
        
;        LDA     #$FF			; Set DP=$FF, to make IO quicker
;        TFR     A,DP
        LDU     #DosFunctionTable	; Point to function dispatch table
        JSR     [B,U]			; Jump to function handler
;	JSR	>CON_DumpRegsWait
        	
        STA     <DosDiskError		; Save error code
        BCC     LC1CF			; No timeout		
        BRA     LC1FD			; timeout, handle it

LC1CF   ANDA    DosErrorMask		; Mask out error bits we are not interested in
        STA     <DosDiskError		; save errors for later use
        BEQ     LC1E4
	
        LDA     <DosHWByte		; Get operation code
        CMPA    #DosFnReadSec2		; ReadSec2 command ?		
        BEQ     LC1E0

        DEC     ,S			; Dec retry count
        BNE     LC1B8			; retry
	
LC1E0   ORCC    #FlagCarry		; Flag error
        BRA     LC1FD

LC1E4   TST     DosErrorMask		; is this write sector ?
        BPL     LC1FD			; no : jump ahead
	
        LDA     <DskTrackNo		; Get track number
        CMPA    #DirPrimary		; Primary Dir track ?
        BEQ     LC1FA

        CMPA    #DirBackup		; Secondary dir track ?
        BEQ     LC1FA

        TST     DosVerifyFlag
        ANDCC   #~(FlagCarry)		; Flag no carry
        BPL     LC1FD
	
LC1FA   LBSR    DosDoReadSec2
LC1FD   LEAS    1,S
        ROL     4,S
	
        LDX     #DosD0Track-1		; Point to drive track table
        LDB     <LastActiveDrv		; Get last used drive
        ABX				; get pointer to track for current drive
;        LDA     trkreg			; Get current track number from WD
        LDA    	<DskTrackNo
	CMPA    <DskTrackNo		; Same as current track no ?
        BEQ     LC21F			; yes : update table
	
        LDB     DosErrorMask		; is this a seek ?
        CMPB    #$19
        BNE     LC21F

        LDB     #DDErrSFF		; Error code : no error
        STB     <DosDiskError
        ROR     4,S
        ORCC    #FlagCarry		; flag error 
        ROL     4,S
	
LC21F   
	STA     ,X			; Update current track for current drive

; DragonDOS copies the control registers of the PIAs onto the slack here.
; DragonMMC does not need to do this, however we manually move the stack pointer
; so that the stack is setup the same.
	leas	4,s
LC226   
        CLR     DosHWMaskFF40		; Clear hardware mask
LC230   CLRB				; Flag no error
        PULS    CC,A,DP,X,Y,U		; Restore regs
        BCS     LC236			; error : skip
LC235   RTS

LC236   LDB     <DosDiskError		; get last error code
	BEQ     LC235			; none : return
        BPL     LC25C
;
; Work out the dragon error code that coresponds to the hardware
; error reported by WD.
;        
	CMPB    #$FC
        BNE     LC244
 
        LDB     #DDErrPR		; parameter
        BRA     LC26B

LC244   CMPB    #$FD
        BNE     LC24C
        
	LDB     #BErrDN			; Device Number 
        BRA     LC26B

LC24C   CMPB    #$FE
        BNE     LC254
        
	LDB     #DDErrNR		; not ready
        BRA     LC26B

LC254   CMPB    #$FF
        BNE     LC269
        
	LDB     #DDErrSK		; seek
        BRA     LC26B

LC25C   TFR     B,A
        LDB     #$82
        ROLA
LC261   ADDB    #$02
        ROLA
        TSTA
        BCS     LC26B
        BNE     LC261
LC269   LDB     #DDErrUD		; undefined
LC26B   ORCC    #FlagCarry
        RTS
;
; Reset controler chip, and spin up drive.
;

ResetAndStartDrive   
;	LDA     #WDCmdForceInt		; Force interrupt
;        STA     cmdreg
        LDA     <DosLastDrive		; Get last active drive no
        BEQ     LC27C
	
        DECA				; Make hw drive no 0..3
        CMPA    #$03			; Drive valid ?
        BLS     StartDrive2		; yes : continue
 
LC27C   ORCC    #FlagCarry		; Flag error
DosHookRetDevParam   
	RTS

StartDrive2
;	ORA     #NMIEn+MotorOn		; Mask in nmi enable & Motor on bits
;        PSHS    A
;        LDA     DosHWMaskFF48		; Get HW byte mask

;	ANDA    #~DriveMask		; Mask out drive bits	
;	ORA     ,S+			; Mask in drive bits 
		
;        STA     DosHWMaskFF48		; Resave hardware mask
        LDX     #DosD0Track-1		; Point to current track table
        LDA     <LastActiveDrv		; Get active drive
        LDA     A,X			; Get drive current track
;        STA     trkreg			; Write to controler
        LDA     #$D2			; set timeout		
        STA     DosTimeout
	CLRB				; no error ?
        RTS

;
; Dos function 0 comes here
;

DosFunctionRestore   
	CLRA				; zero track no
        STA     >DskTrackNo		; Save Track number
        BRA     LC2B9

DosFunctionSeek
	LDA     >DskTrackNo		; Get current track no
;        CMPA    dptrkreg		; Are we over it ?
;        BRA     SeekTrackinA		; no : seek to it
;        CLRA
;        STA     DosErrorMask		; Turn off verify
;        TFR     A,DP			; Reset DP
;        RTS

;
; Seek to a track, routine will exit either with an error
; On entry A contains track to seek to.
;

SeekTrackinA
;	STA     <DPDataReg		; <$43 ;DATAREG  		
;        LDA     #WDCmdSeek		; Seek command
	
LC2B9   LDB     #$19
        STB     DosErrorMask
;        LDX     #DosD0StepRate-1	; Point to step rate table
;        LDB     >LastActiveDrv		; Get active drive
;        ORA     B,X			; Mask in that drive's step rate
;        STA     dpcmdreg		; save in command reg
	
	tfr	a,b			; move track to b
	lda	>LastActiveDrv		; Get active drive
	deca				; make zero based
	lbsr	MMC_SeekCheck		; Go check that it seeked ok
	rts

;LC2C8   MUL				; burn up CPU time waiting....
;        MUL				; NMI will exit this loop
;        LEAY    -1,Y			; decrement timeout counter
;        BNE     LC2C8			; count exhausted ? : no keep going
;        BRA     LC31B			; yes : error

;
; Dos function 6 : Read address mark (Dragon)
; not supported on MMC

DosFunctionReadAddr   
	clra
	rts

;
; Dos function 2 : Read sector (Dragon/Cumana)
; 
DosFunctionReadSec
	pshs	x,u
        LDB     #WDDefErrMask		; set error mask
        STB     DosErrorMask
	
	ldu	DosLBASec		; get LBA
	clrb				; msb always 0
	lda	<LastActiveDrv		; get drive
	deca				; Zero based
	lbsr	MMC_ReadDOSSec		; go read it
	
	puls	x,u,pc

;
; Dos function 7 read first two bytes of a sector, used by BOOT command.
;

DosFunctionReadSec2  
	pshs	x,u
	ldx	#$004F			; put data here
        LDA     #WDDefErrMask		; Set error mask
        STA     DosErrorMask

	ldu	DosLBASec		; get LBA
	clrb				; msb always 0
	lda	<LastActiveDrv		; get drive
	deca				; sddos drives zero based
	lbsr	MMC_ReadDOSSec2
	puls	x,u,pc
;
; Dos function 4
;

DosFunctionWriteSec2   
;
; Dos function 3
;
;	rts
	
DosFunctionWriteSec   
	pshs	x,y,u
	LDA     #WDErrMaskDF		; set error mask
        STA     DosErrorMask
	
	ldu	DosLBASec		; get LBA
	clrb				; msb always 0
	lda	<LastActiveDrv		; get drive
	deca				; sddos drives zero based
	lbsr	MMC_WriteDOSSec
	
	puls	x,y,u,pc

;
; Dos function 5 : Write (format) track
; not implemented for mmc (yet!)

DosFunctionWriteTrack 
	LDA     #WDErrMaskFormat	; set error mask	
        STA     DosErrorMask
	clra
	rts
	
;
; Dskinit dispatch routine
;
; Syntax :
;	DSKINIT				(default drive,sides,tracks)
;	DSKINIT drive			(specified drive, default sides,tracks) 
;	DSKINIT drive,sides		(specified drive,sides default tracks) 
;	DSKINIT drive,sides,tracks	(specified drive,sides,tracks)
;
		
CmdDskInit   
	BEQ     LC3BC			; No parameters : use defaults
        JSR     >GetDriveNoInB		; Get drive no
        STB     <DosLastDrive		; save it
        JSR     >GetCommaThen8Bit	; Get comma, and then no of sides
        BEQ     LC3C1			; Error, use default sides & tracks
	
        DECB				; Convert sides to zero base
        CMPB    #$01			; > 1 sides specified : error & exit
        BHI     LC3B9			; Error : use default tracks
	
	STB     <DosRecLenFlag		; Save sides
        JSR     >GetCommaThen8Bit	; Get comman, then tracks
        BEQ     LC3C3			; Error : use default tracks
	
 	CMPB    #$28			; 40 tracks ?
        BEQ     LC3C5			; Yes skip on
	
        NEG     <DosRecLenFlag
        CMPB    #$50			; 80 tracks ?
        BEQ     LC3C5			; yes, skip on
LC3B9   JMP     >DosPRError

;
; Set defaults for format : disk=1,sides=1,tracks=40
;
LC3BC   LDB     DosDefDriveNo		; use default drive
        STB     <LastActiveDrv		; save in last used
LC3C1   CLR     <DosRecLenFlag		
LC3C3   LDB     #FmtDefTracks		; no of tracks to format by default
LC3C5   STB     <DosDSKINITraks
        
;
; <DosDSKINITraks = tracks to format
; <DosRecLenFlag  = sides-1 so singlesided = 0, double sided = 1
;  
       
	JSR     >DosCloseAllFiles	; close all files error if can't
        LBNE    DosJmpToSysError
	
        LDX     #DosDiskBuffBase	; Point to the buffer base
        STX     <DiskBuffPtr
        JSR     >DosDoRestore		; Restore to track 0
        LBNE    DosJmpToSysError	; error : exit
	
        LDA     #$01			; start at sector 1
        STA     <DskSectorNo		
        JSR     >DosDoReadSec2		; try reading it
        CMPB    #$80			
        BEQ     DosJmpToSysError

        lda     <LastActiveDrv          ; Get drive id
        deca                            ; make zero based
        ldb     <DosDSKINITraks         ; get tracks
        tst     <DosRecLenFlag          ; double sided ?
        beq     DoMMCFormat             ; no skip to format
        orb     #$80                    ; flag double sided

DoMMCFormat	
        jsr     >MMC_FormatImage        ; Tell mmc to format virtual image

	lda	<DosLastDrive		; Get current drive
	deca				; zero based
	
	ldx	#DosD0SecTrack		; point to tracks / drive	
	ldb	#SectorsPerTrack	; Calculate sectors / cylinder
        tst     <DosRecLenFlag          ; double sided ?
        beq     SetSecTrack             ; no skip to format
	addb	#SectorsPerTrack	; make double sided.
	
SetSecTrack
	stb	a,x			; set logical sectors / track
	
        ifeq    1
        
LC3E5   CLR     <DosDSKINIHead		; do head 0
        CLR     <DskSectorNo		; start at sector 0
        
	JSR     >SetupTrackLayout	; setup track layout in ram
	JSR     >DosDoWriteTrack	; write the track
        BCS     DosJmpToSysError	; error : exit
        
	TST     <DosRecLenFlag		; is this a double sided disk ?	
        BEQ     LC404			; nope skip
        LDA     #$01			; do side 1
        STA     <DosDSKINIHead
        NEGA
        STA     <DskSectorNo

	JSR     >SetupTrackLayout	; setup track layout in ram
	JSR     >DosDoWriteTrack	; write the track
        BCS     DosJmpToSysError	; error : exit

LC404   INC     <DskTrackNo		; increment track 
        LDA     <DskTrackNo		
        CMPA    <DosDSKINITraks		; have we done all yet ?
        BCS     LC3E5			; nope do next track

        endc
	
        JSR     >DosDoRestore		; finished formatting, restore to track 0
        BCS     DosJmpToSysError

        ifeq    1
	
LC411   JSR     >DosDoSeek		; seek to track 
        BCS     DosJmpToSysError	; error : exit
	
        CLRA
        JSR     >CmdDskInitVerifyTrack	; verify current track
        INC     <DskTrackNo		; move to next track
        LDA     <DskTrackNo
        CMPA    <DosDSKINITraks		; done all ?	
        BCS     LC411			; nope : do next track
	
        endc
        
        LDX     <DiskBuffPtr		; point at disk buffer
        LDD     <Misc16BitScratch	; d=0
LC426   STD     ,X++			; fill buffer with zeros
        CMPX    #DskInitBuffer+(3*256)	; end of buffer?
        BNE     LC426			; nope keep filling
	
        LDA     #$01			; sector 0 
        STA     <DskSectorNo
        LDA     #DirPrimary		; Directory track
        BSR     BuildAndWriteBAM	; build and write BAM 

        DEC     <DskSectorNo		; Point back at first BAM block 
        DEC     <DiskBuffPtr
        LDA     #DirBackup		; Directory backup track
        BSR     WriteBAM		; Write BAM sectors
        BRA     MakeBlankDir

BuildAndWriteBAM   
	PSHS    A
        BSR     BuildBAM		; Build BAM and geometry info
        PULS    A

; Write first 2 sectors to dir track
WriteBAM
	STA     <DskTrackNo		; Write sector to track
        JSR     >DosDoWriteSec
        BCS     DosJmpToSysError	; error : exit
	
        INC     <DskSectorNo		; do sector 2
        INC     <DiskBuffPtr
        JSR     >DosDoWriteSec		; Write sector to track
        BCS     DosJmpToSysError	; error : exit
        RTS

;
; Exit with error, allow basic to handle it.
;

DosJmpToSysError   
	JMP     >DosHookSysError	; Jump to basic error handler

; Fill in a blank directory sector, setting default attributes.
; Bug : this seems to only fill in the backup track, as the call to Lc489 will never return !
MakeBlankDir   
	INC     <DiskBuffPtr		; Increment buff pointer by 1 page
        LDX     <DiskBuffPtr			
        LDD     #(AttrAtFormat*256)+DirEntPerSec	; Get default attribute & number of entries			
LC460   STA     ,X			; Fill in attributes
        LEAX    DirEntryLen,X		; move to next entry
        DECB				; any more : continue
        BNE     LC460			
	
        BSR     LC46E		
        
        LDA     #DirPrimary		; Do primary dir track
        STA     <DskTrackNo
	
LC46E   LDD     #$1003			; Process 16 sectors starting at sector 3
        STB     <DskSectorNo
LC473   JSR     >DosDoWriteSec		; go write the sector
        BCS     DosJmpToSysError	; error : exit
	
        INC     <DskSectorNo		; do next sector
        DECA				; decrement count
        BNE     LC473			; keep going if more
        JMP     >DosReset		; reset dos

;
; Build the block availability bitmap in sectors 1 and 2 of directory track
; this does 3 things.
; 1) Marks all physical sectors as available, adjusting for DS/SS & 40/80 tracks
; 2) Marks all directory track sectors as being in use, ajusting for DS/SS disks.
; 3) Fills in disk geometry bytes in the first BAM sector.
;
BuildBAM   
	STA     <DskTrackNo
        LDA     #SectorsPerTrack
        LDB     #BAMEntries40SS		; used to calculate number of needed BAM entries
        TST     <DosRecLenFlag		; Double sided ?
	BEQ     LC48C
        
        ASLB				; Double number of sectors as DS
        ASLA				; Double number of BAM entries as DS
LC48C   STA     DskInitBuffer+DirSecPerTrk	; save sectors / track
        COMA
        STA     DskInitBuffer+DirSecPerTrk1s	; save complement for error check

        LDA     <DosDSKINITraks		; Get number of tracks
        STA     DskInitBuffer+DirTracks	; Save no of tracks

        COMA				; Complement for error check
        STA     DskInitBuffer+DirTracks1s	; Save no of tracks check
        
	LDX     <DiskBuffPtr
        LDU     #DskInitBuffer+(256*1)	; 1 sector into buffer
        LDA     #$FF			; Mark a block of sectors free
LC4A3   STA     ,X+
        DECB				; Any more BAM groups ?
        BNE     LC4A3

        LDD     #(BAMOffDirBakSS*256)+BAMOffDirPriSS ; get offsets in BAM of dir sectors SS	
        TST     <DosRecLenFlag		; double sided ?
        BEQ     LC4BC
        BPL     LC4B9
	
        LDD     #$B4FF
LC4B4   STB     ,U+
        DECA
        BNE     LC4B4

LC4B9   LDD     #(BAMOffDirBakDS*256)+BAMOffDirPriDS ; get offsets in BAM of dir sectors DS 

LC4BC   LDU     <Misc16BitScratch	; U=0
        PSHS    A			; Mark track 20 dir in use
        BSR     LC4C4
        PULS    B			; mark track 16 dir in use

LC4C4   LDX     <DiskBuffPtr		; get pointer to BAM
        ABX				; calculate offset
        LDA     #$FC			; mask for last 2 sectors
        STU     ,X++			; mark dir sectors in use
        STA     ,X
        RTS

CmdDskInitVerifyTrack   
	CLR     <DskSectorNo		; Sector 0
        TST     <DosRecLenFlag		; is it DS ?
        BEQ     LC4D6			; nope just do side 0
        BSR     LC4D6			; do side 0 then 1
	
LC4D6   LDA     #SectorsPerTrack	; Sector counter
LC4D8   INC     <DskSectorNo		; next sector
        JSR     >DosDoReadSec2		; go read it
        LBCS    DosJmpToSysError	; error : exit
        DECA				; decrement count
        BNE     LC4D8			; loop again if more
LC4E4   RTS

;
; Setup format block for write track
;

SetupTrackLayout   
	LDU     <DiskBuffPtr		; Point to disk buffer
        LDX     #DDDF7
        LDY     #SectorIDTable		; Table of sector IDs with required interleave		
        LDB     #$0C			; Count
        BSR     DosUtilCopyBXtoU
	
LC4F2   LDX     #DDE03
        LDB     #$06
        BSR     DosUtilCopyBXtoU
        LDA     #$01
        LDB     <DskTrackNo
        STD     ,U++
        LDB     <DosDSKINIHead
        STB     ,U+
        LDB     ,Y+
        STD     ,U++
        STA     ,U+
        LDB     #$12
        BSR     DosUtilCopyBXtoU
        TST     ,Y
        BNE     LC4F2
        LDB     #$03

DosUtilCopyBXtoU   
	JMP     >UtilCopyBXtoU

;
; GetCommaThen8Bit, scan for comma, error if not found, then fetch 8 bit that follows (or error). 
;

GetCommaThen8Bit
	JSR     <BasChrGetCurr		; Get current basic char
        BEQ     LC4E4			; Any left no: return 
        JSR     >VarCKComma		; check for comma
        JMP     >Get8BitorError		; go get it
;
; Backup command dispatch routine
;
; Syntax :-
;	BACKUP SrcDrive TO Destdrive,heads,tracks
;
; Stack frame as follows :
;	16 bytes cleared on stack, U points to base of this area as with OS-9.
;
;	0,U	Drive number of source ?
;	1,U	$00 Source track
;	2,U	$01 Source sector
;	3,U	$DF5A ???
;	5,U	Source buffer addr
;	7,U	Drive number of dest ?
;	8,U	$00 Dest track
;	9,U	$01 Dest sector ????
;	10,U	$DF6D ???
;	12,U	dest buff addr
;	14,U	$12 Sector count per track to copy ?
;	15,U	Page number of top of usable RAM

CmdBackup   
	LEAS    -16,S			; Make tempory space on stack
        TFR     S,U			; Point U at base of tempory space (Like OS-9 !)
        TFR     U,D		
        SUBD    #$0040			; reserve room for working stack
        SUBD    <BasVarEnd		; Check that we have suficient memory available
        LBMI    BasOMError		; NO: report ?OM error
	
        CMPA    #$01			; At least 1 sector's worth of ram (256 bytes) available
        LBLT    BasOMError		; NO: report ?OM error
        STA     BupAvailPages,U		; Store memory page count of avaiable RAM
        LDA     #SectorsPerTrack	; Sectors per track, initially 18 for SS disk
        STA     BupSecTrk,U
        LDD     <BasVarEnd		; Get end of RAM in use by basic
        STD     BupSrcBuff,U		; save in buffer pointers for source and dest
        STD     BupDestBuff,U

        LDD     #MessInsertSource-1	; Insert source and destination message pointers
        STD     BupSrcMess,U
        LDD     #MessInsertDest-1
        STD     BupDestMess,U
        
        LDD     #$0001			; Set source and dest track and sector to 0 & 1
        STD     BupSrcTrk,U
        STD     BupDestTrk,U
        LDA     DosDefDriveNo		; Get default drive no
        STA     ,U			; save in source drive
        STA     BupDestDrive,U		; and dest
        LDY     #(SectorsPerTrack*40)	; sector count 720 sectors=ss40 disk

	JSR     <BasChrGetCurr
        BEQ     DoCmdBackup		; No params backup from default drive to default 
        JSR     >Get8BitorError
        CMPB    #MaxDriveNo		; greater than Max drive (4)?
        LBHI    DosDNError

        STB     ,U			; Save source drive
        STB     BupDestDrive,U		; and default dest to same drive

	JSR     <BasChrGetCurr		; Get current character from basic
        BEQ     DoCmdBackup		; end of line : yes do backup

 	CMPA    #DTokTO			; is this the "TO" token ?
        BNE     CmdBackupErrorExit

 	JSR     <BasChrGet		; Get next char, skip over "TO"
        JSR     >Get8BitorError		; Get dest drive in B
        CMPB    #MaxDriveNo		; greater than Max drive (4)?
        LBHI    DosDNError

        STB     BupDestDrive,U		; Save in Dest driveno

	BSR     GetCommaThen8Bit	; Skip comma, and get next param
        BEQ     DoCmdBackup		; nothing : do backup

        CMPB    #$02			; 2 sided disk specified ?
        BEQ     BackupDS		; yes backup double sided
        CMPB    #$01			; 1 sided disk specified ?
        BEQ     BackupSS		; yes backup single sided


CmdBackupErrorExit   
	JMP     >BasSNError		; error : exit

BackupDS   
	TFR     Y,D			; Double sector count if double sided
        LEAY    D,Y
        ASL     BupSecTrk,U		; Set sectors per track for DS disk

BackupSS   
	JSR     >GetCommaThen8Bit	; Get next param (if any)
        BEQ     DoCmdBackup		; none: continue
        CMPB    #$50			; Do 80 tracks ?
        BEQ     Backup80
        CMPB    #$28			; Do 40 tracks ?
        BEQ     DoCmdBackup
        BRA     CmdBackupErrorExit	; neither : error

Backup80
	TFR     Y,D			; Double sector count if 80 track
        LEAY    D,Y

DoCmdBackup   
	CLRA
BupReadFromSrc   
	LEAY    1,Y			; Get sector count
        LEAX    ,U			; point to source drive on stack frame
        BSR     BupCheckPrompt		; Check if drives are same & prompt if so
	
LC5B2   LEAY    -1,Y			; decrement sector count
        BNE     LC5BC			; if more sectors, do next
        BSR     BupWriteToDest
        LEAS    $10,U			; Clear stack frame
LC5BB   RTS

LC5BC   CMPA    BupAvailPages,U		; Filled all available RAM pages ?	
        BNE     LC5CE			; no : do next sector
        BSR     BupWriteToDest		; Yes : write to destination
        PSHS    D
        LDD     <BasVarEnd		; Get end of basic storage
        STD     BupDestBuff,U		; Save in source and dest buffer pointers
        STD     BupSrcBuff,U
        PULS    D
        BRA     BupReadFromSrc		; Do next sector

LC5CE   LDB     #DosFnReadSec		; Read the sectors
        BSR     LC5E4			; go do it
        INCA				
        BRA     LC5B2

BupWriteToDest   
	TSTA
        BEQ     LC5BB
        LEAX    BupDestDrive,U		; Point to dest drive vars
        BSR     BupCheckPrompt		; Check if drives are same & prompt if so
        LDB     #DosFnWriteSec		; write sectors to destination
LC5DE   BSR     LC5E4			; go do it
        DECA				; decrement sector count
        BNE     LC5DE			; if more go again
LC5E3   RTS

LC5E4   PSHS    D
        LDA     ,X			; Get source drive
        STA     <LastActiveDrv		; make source drive the current drive

        LDD     BupSrcBuff,X		; point to source buffer
        STD     <DiskBuffPtr
        LDD     BupSrcTrk,X		; get source track and sector
        STD     <DskTrackNo		; set them
        LDB     1,S			; get the function code read or write
        JSR     >DosDoFuncinB		; Ask dos to do it !
        BCC     LC60D			; no error, skip on
        STB     DosErrorCode		; save error code

        LDA     1,S			; Get function, read or write
        CMPA    #DosFnReadSec		; Read ?
	BNE     LC607
        PULS    D,X

	JSR     >BupWriteToDest
LC607   LDB     DosErrorCode		; Retrieve error code
        JMP     >DosHookSysError

LC60D   INC     BupSrcSec,X		; Move to next source sector
        LDA     BupSrcSec,X
        CMPA    BupSecTrk,U		; still sectors on this track to read ?
        BLS     LC61B			; yep : keep going
	
        LDA     #$01
        STA     BupSrcSec,X		; set source sec to 1
        INC     BupSrcTrk,X		; increment source track
LC61B   INC     BupSrcBuff,X		; move to next memorty page
        PULS    D,PC

;
; Check if source and dest drives are the same and if so prompt to swap disks.
;

BupCheckPrompt   
	LDB     ,U			; get source drive
        CMPB    7,U			; same as dest drive ?
        BNE     LC5E3			; no : continue
	
        PSHS    A,X,Y,U
        JSR     >TextCls		; clear screen
        LDX     1,S			; get message pointer
        LDX     3,X
        JSR     >TextOutString		; Print message (insert source/insert dest)
        LDX     #DMessPressAnyKey-1
        JSR     >TextOutString		; Print press any key
        JSR     >TextWaitKeyCurs2	; Wait for a kepress
        JSR     >TextCls
        PULS    A,X,Y,U,PC

;
; Get8BitorError, get non zero 8 bit value in B, or generate error
;
Get8BitorError
	PSHS    Y,U
        JSR     >VarGet8Bit		; Get 8 bit value into B
        TSTB				; B non zero ?
        BNE     LC64A
        JMP     >BasFCError		; No : error
	
LC64A   PULS    Y,U,PC			; Restore and return

DosCmdDispatch   
	CMPA    #$FF			; Invalid token ?
        LBEQ    BasSNError
        SUBA    #DDTokFirstC		; Make token number zero based
        BPL     LC659			; valid token : yep continue
	
LC656   JMP     >BasSNError		; nope SNError

LC659   CMPA    #DDTokCountC		; check token in range
        BCC     LC663			; Nope, continue to next jump table
        LDX     #CommandDispatchTable	; Point to command address table
        JMP     >BasDoDispatch		; go do it !
        
LC663   JMP     [>NextResJump]		; Jump to user reserved word handler >$0137

DosFuncDispatch   
	SUBB    #$44
        BPL     LC66D			; Check token in range, skip if ok
        BRA     LC656			; else ?SN Error

LC66D   CMPB    #(DDTokCountF*2)	; check token in range
        BCC     LC679			; nope : skip to next handler
        LDX     #FunctionDipatchTable	; point to function table
        JSR     [B,X]			; jump to function
        JMP     >VarGetExprCC		; return value to basic

LC679   JMP     [>NextFuncsJump]	; Jump to user function handler >$013C

; test and flush all buffers

TestAndFlushAll   
	LDX     #Buff1Details		; Point to first buffer
LC680   JSR     >TestAndFlushBuffer	; Flush if needed
        BNE     DosHookSysError		; error : exit
	
        CLR     BuffFlag,X		; mark buffer free
        LEAX    BuffDetailSize,X	; move to next buffer
        CMPX    #(Buff1Details+(BuffCount*BuffDetailSize))	
	
        BCS     LC680			; done all : no loop again
LC68E   RTS

;
; Get drive no in B, returns drive no (from command) in B,
; or causes error if (drive < 0 ) or (drive > 4)
;

GetDriveNoInB   
	JSR     >VarGet8Bit		; Get 8 bit var
        TSTB
        BEQ     DosDNError
        CMPB    #$04			; Valid < 4 ?
        BLS     LC68E			; yes : skip
	 
DosDNError   
	LDB     #BErrDN			; Device no error
	FCB	Skip2		

DosPRError
	LDB 	#DDErrPR		; Parameter error

DosHookSysError   
	STB     DosErrLast		; save last error code
        LDX     <BasCurrentLine		; Get current line no
        STX     DosErrLineNo		; save for ERR routine
        JSR     >BasResetStack		; reset basic stack
        CLR     <DosIOInProgress	; Flag no IO in progress
        JSR     >CasMotorOff		; turn off tape motor
        JSR     >SndDisable		; disable sound
        CLR     <TextDevN		; Set device no back to console
        JSR     >TextOutCRLF		; output EOL
        TST     DosErrGotoFlag		; Do we have an error handler ?
        BPL     LC6C1			; Yes, handle errors
        LDX     <BasCurrentLine		; Get current line no
        LEAX    1,X
        BNE     LC6D2

LC6C1   JSR     >TextOutQuestion 	; output '?'
        LDX     #BasErrorCodeTable	; Point to error code table $82A9
        LDB     DosErrLast		; Get last error code
	BPL     LC6CF
        
	LDX	#(DosErrorCodeTable-DDFirstError) 	; Get pointer to error table !
LC6CF   JMP     >SysErr2		; Jump to basic Error handler

LC6D2   LDX     #BasBRARun		; Go back to main interpreter loop $84DA
	PSHS    X
        LDD     DosErrDestLine
        STD     <BasTempLine
        JMP     >BasSkipLineNo

;
; New reset vector
;

DOSResetVector
	NOP				; Main ROM checks for reset->NOP
;        CLRA				; Reset DP=0
;        TFR     A,DP		
        JSR     >DosReset		; Reset WD, and various Dos vars.
        CLR     DosErrorMask		; reset various flags
        CLR     DosTimeout
        CLR     DosAutoFlag
;        LDA     #$35			; Re-enable NMI
;        STA     PIA0CRB
;        JMP     >WarmStart		; Jump back to Main ROM reset routine
	RTS
	
;
; NMI vector, called to break out of read & write loops between 6809 & WD
; This allows the IO routines to handle sectors of all lengths.
;

NMISrv   
	rti
;	LDA     dpcmdreg		; Read status register.
;        CLRB				; Reset DP=0
;        TFR     B,DP
;        LEAS    12,S			; Drop registers from stack
;        TSTA				; Setup CC
;LC6FF   RTS

;
; New IRQ vector, used to count down and shut off drives.
;

IRQSrv   
	BRA	LC724
	
LC702	BNE     LC71E			; Yes: don't time out
        LDA     DosTimeout		; Get timeout byte 
        BEQ     LC71E			; Already timed out : exit
 
        DECA				; Decrement timeout count
        STA     DosTimeout	

        BNE     LC71E			; not zero, don't timeout yet
        BSR     DOSSyncDir		; syncronsise directory
        BNE     LC721			; Error : report it
LC71E
	BRA	LC72D
	NOP
		
LC721   JMP     >DosHookSysError

LC724	PSHS	B
;	CLRA				; DP = 0
;	TFR	A,DP			
	TST	<DosIOInProgress 	; Doing IO ?
	BRA	LC702

LC72D	PULS	B
	JMP	BasIRQVec
	
	NOP
	NOP
;
; Copy updated track 20 to track 16
;

DOSSyncDir   
	JSR     >TestAndFlushAll	; Flush all buffers if needed
        LEAS    -8,S			; Make room on stack
        LEAU    ,S			; Point U at stack frame
        LEAY    SyncSectors,U		; point Y at sync sector table
	
        LDX     #DosDiskBuffBase	; Point at tempory buffer area
        STX     <DiskBuffPtr
        CLR     SyncBufferNo,U		; clear buffer no counter
        LDB     <LastActiveDrv		; Get last accessed drive
        STB     SyncDrive,U		; Save it
        LDB     #$01			; Drive counter shifted left to count
        STB     ,U
        CLR     <LastActiveDrv
        LDX     #DosDirSecStatus-1	; $06AA
	
LC751   LDB     #SectorsPerTrack	; Sector count
        STB     SyncSecNo,U
        INC     <LastActiveDrv

LC757   LDB     SyncSecNo,U		; get sector no
        LDA     B,X			; get it's status byte
        BITA    ,U			; test it
        BEQ     LC77D
	
        COMA
        ANDA    B,X
        STA     B,X
        INC     SyncBufferNo,U		; move to next buffer
        STB     <DskSectorNo
        STB     ,Y+
        LDB     #DirPrimary		; Track 20
        STB     <DskTrackNo		
        JSR     >DosDoReadSec		; Go read sector
        
	BNE     LC794			; Error !
	
        INC     <DiskBuffPtr		; use next disk buffer
        LDB     SyncBufferNo,U		; Check to see if we have filled all buffers
        CMPB    #$04			
        BCS     LC77D			; nope keep going
        BSR     LC797			; flush them
	
LC77D   DEC     SyncSecNo,U		; decrement sector no
        BNE     LC757			; keep going if more sectors
	
        TST     SyncBufferNo,U		; any buffers in use ?, so sectors still waiting to be flushed ?
        BEQ     LC787			; no  skip
        BSR     LC797			; yes : flush them
	
LC787   ASL     ,U			; move to next drive			
        LDA     ,U				
        CMPA    #$08			; done all drives ?	
        BLS     LC751			; nope do next
	        
        LDA     SyncDrive,U		; Restore last used drive
        STA     <LastActiveDrv

	CLRB				; Flag no error
LC794	LEAS    8,U			; Drop stack frame
	RTS

LC797   LDA     #DirBackup		; Backup track no
        STA     <DskTrackNo
LC79B   DEC     <DiskBuffPtr		; Move to previous buffer
        LEAY    -1,Y
        LDA     ,Y
        STA     <DskSectorNo		; Pickup sector no
        JSR     >DosDoWriteSec		; Go write it
        BEQ     LC7AB

	LEAS    8,U
        RTS

LC7AB   DEC     SyncBufferNo,U		; move to previous disk buffer
        BNE     LC79B
LC7AF   RTS

FIRQSrv   
;	TST     PIACRA			; Clear interrupt conditions 
;        TST     PIACRB
        RTI				; and return


LC7B7   BSR     DOSValidFilename	; check valid filename
        BNE     LC7AF			; nope return
        JMP     >DOSOpenFile		; otherwise open it

;
; Validate filename and copy to current drive block
;
;	  On entry:
;	    X points to filename e.g. '3:FILENAME.EXT'
;	    B length of filename e.g. 0x0e
;	    Y points to default extension to use if none is given e.g. 'DAT'.
;             Use '   ' for no default extension
;	  On Return:
;	    Filename appears at $0650-$065a
;	    CC.Z clear on error
;	    B contains error code
;	    U $065b always (SuperDosE6)
;

DOSValidFilename  
	JMP	>LDFF3
		
LC7C1	STA     DosCurDriveNo		; Set current drive number, default if non specified
        CLR     DosCurCount
        LDU     #DosCurDriveInfo	; Point at current drive info

        LDA     #$07			; Zero out first 8 bytes (filename)	
LC7CC   CLR     A,U
        DECA
        BPL     LC7CC
        
	LDA     2,Y			; Transfer extension into current details
        STA     DosCurExtension+2	; $065A
        LDA     1,Y
        STA     DosCurExtension+1	; $0659
        LDA     ,Y
        STA     DosCurExtension		; $0658

        CMPB    #MaxFilenameLen		; Filename too long ?
        BHI     LC829			; Yep : error
	
        CMPB    #$03			; Long enough to contain drive no ?
        BCS     LC811			; nope : skip on
	
; Because of the order of compare a drive letter at the END of the filename always
; takes presedence, this would only be siginificant if the filename where something like
; '1:2' which would access a file called '1' on drive 2, and NOT 2 on drive 1
	
        SUBB    #$02			; Look for drive no at end of filename
        LDA     B,X
        CMPA    #':			; Seperator present ? $3A
        BNE     LC7F6			; No skip on
        INCB
        LDA     B,X			; Get drive no
        INCB
        BRA     LC800

LC7F6  	ADDB    #$02			; Check for drive at begining of path
        LDA     1,X
        CMPA    #':			; Seperator present ? $3A
        BNE     LC811			; nope, use default drive
	
        LDA     ,X++			; Get ascii drive no
LC800   SUBA    #$30			; Work out drive number
	
	BLS	LC808			; invalid : exit
	CMPA    #MaxDriveNo		; Drive valid ?
	BLS	LC80C			; valid, skip on
	
LC808	LDB	#BErrDN			; Error device no
	RTS
	NOP
		
LC80C   STA     DosCurDriveNo		; Set current drive if specified
        SUBB    #$02

; Parse filename looking for extension seperator
	
LC811   LDA     ,X+			; Get next char
        DECB				; Decrement path count
        BMI     LC87A			; Reached end : yes skip
	
        CMPA    #'/			; Check for slash $2F
        BEQ     LC81E

        CMPA    #'.			; Check for period $2E
        BNE     LC83A

LC81E   CMPU    #DosCurDriveInfo	; $0650
        BEQ     LC829
        
        TST     DosCurCount		; First pass ?
        BEQ     LC82C			; yes : skip on

LC829   LDB     #DDErrFS		; Error : bad filespec 
        RTS

LC82C   INC     DosCurCount		; Mark second pass
        LDU     #DosCurExtension	; $0658
        CLR     ,U			; Zero out extension
        CLR     1,U
        CLR     2,U
        BRA     LC811

; Validate filename chars

LC83A   CMPA    #'A			; $41
        BCS     LC84E			; Below, check if still valid
	
        CMPA    #'Z			; $5A
        BLS     LC85A			; valid, skip on
	
        SUBA    #$20			; Convert to lower case if upper
        CMPA    #'A			; $41
        BCS     LC829			; Invalid, return error
	
        CMPA    #'Z			; $5A
        BLS     LC85A			; Valid: skip on
        BRA     LC829

LC84E   CMPA    #'-			; $2D
        BEQ     LC85A			; Valid skip on
	
        CMPA    #'0			; $30
        BCS     LC829			; Invalid : error
	
        CMPA    #'9			; $39
        BHI     LC829			; Invalid : error
	
LC85A   STA     ,U+			; Save char in path
        CMPU    #DosCurDriveNo		; Path full ?
        BNE     LC867			; nope : skip
        
	TSTB				; Done all path chars ?
        BNE     LC829			; nope : error !
        BRA     LC87A

LC867   CMPU    #DosCurExtension	; Reached extension ? $0658
        BNE     LC811
	
        LDA     ,X+			; Get next 
        DECB				; Dec count
        BMI     LC87A			; Done, return
	
        CMPA    #'.			; Check for seperator $2E
        BEQ     LC81E			; yep loop back
	
        CMPA    #'/			; Check for seperator $2F
        BEQ     LC81E			; Yep loop back
	
LC87A   CLRB
        RTS

;
; Open a file and copy dir entry into FCB.
;
;  On entry:
;	    Filename at $0650 ??
;	  Returns:
;	    CC.Z clear on error
;	    A FCB number (0-9)
;	    B contains error code
;

DOSOpenFile   
	LDX     #DosFCB0Addr		; Point to first FCB
        CLR     <DosCurrCtrlBlk
        LDD     DosCurDriveInfo		; Get first 2 bytes of current drive info
LC884  	CMPD    ,X			; Does this FCB point to it ?
        BNE     LC89F			; Nope : check next
	
; Found matching first 2 bytes of name in an FCB
	
        LDU     #DosCurDriveInfo+2	; Check bytes 2..end of filename
        LEAY    2,X			; Compare from byte 2 of FCB
        LDB     #$0A			; Do 10 bytes, rest of filename + ext
LC890   LDA     ,U+			; Get a byte from current
        CMPA    ,Y+			; compare to FCB
        BNE     LC89C			; Don't match : exit check
        DECB				; Decrement counter
        BNE     LC890			; Not at end : do next
        LBRA    LC947


; Move to check next FCB

LC89C   LDD     DosCurDriveInfo		; Re-get first 2 chars of current filaname
LC89F   LEAX    DosFCBLength,X		; Skip to next FCB
        INC     <DosCurrCtrlBlk		; Set current control block
        CMPX    #DosFCBEnd		; End of blocks ?
        BCS     LC884			; No, loop back and check this block
	
        CLR     <DosCurrCtrlBlk		; Set current block to zero
        LDX     #DosFCB0Addr		; Point at first FCB

LC8AE   TST     ,X			; FCB in use ?
        BEQ     LC8BF			; No : skip on
	
        LEAX    DosFCBLength,X		; Check next FCB
        INC     <DosCurrCtrlBlk		
        CMPX    #DosFCBEnd		; Done all FCBs
        BCS     LC8AE			; No : check next, yes error, can't open file, no free FCBS
        LDB     #DDErrTF		; error : too many files open
LC8BE   RTS

LC8BF  	LDB     #$0C			; Copy 12 characters of filename
        TFR     X,Y			; Point Y at selected FCB
        LDU     #DosCurDriveInfo	; Point at current info
LC8C6   LDA     ,U+			; Copy filename
        STA     ,Y+
        DECB				; Dec count
        BNE     LC8C6			; if not all done : do next
	
        STA     <LastActiveDrv		; Save current drive

; Note in disassembled superdos source, the following was LDU #$0616, which is part of the error line !
; This makes no sense, and is Drv0Details, in DragonDos source,	I think I just fixed a 20 year old
; bug !!!!!!

        LDU     #Drv0Details		; Get drive details	 
        LDB     #DrvDeatailLen		; 6 bytes/drive
        MUL
        LEAU    D,U			; Point at drive detail block
        INC     DrvDetUseCnt,U		; Increment usage/open file count       
	
	LDB     #$13			; Clear rest of FCB
LC8DB   CLR     ,Y+
        DECB				; Dec counter
        BNE     LC8DB			; Loop if more
	
        LDA     #AttrDeleted		; Flag file as deleted by default
        STA     FCBDirFlags,X
	
        CLR     DosTempFileNo		; start temp fileno at 0
        JSR     >DosGetDiskGeometry	; get disk geometry
	BNE     LC8BE			; error : exit
        
        LDY     ,X			; get LSN number from buffer			
        LEAY    2,Y			; add 2
        LDA     #DIRSecCount		; set number of sectors to scan
        STA     DosCurCount

LC8F6   STY     DosLSNCounter		; save LSN
        JSR     >DOSFindAndRead		; go read it
        BNE     LC8BE			; error
	
        LDX     BuffAddr,X		; get address of data buffer
        LEAU    DirLastByte,X		; point U at first byte after last entry
        STU     DosSaveBuffAddr		; save buff addr

LC908   LDA     ,X			; get first data byte (attribute)
        BITA    #(AttrDeleted+AttrIsCont) ; is this a deleted or continuation entry ?	
        BNE     LC928

        LDD     DosCurFilename		; get first 2 characters of filename
        CMPD    DirEntFilename,X	; compare to directory entry
        BNE     LC928			; not the same, skip to next

        LDU     #DosCurFilename+2	; check the rest of the name
        LEAY    DirEntFilename+2,X	; point to name[2] in entry		
        LDB     #$09			; check 9 characters, 6 left from name + 3 from extension
LC91D   LDA     ,U+			; get character from name
        CMPA    ,Y+			; compare to dir entry
        BNE     LC928			; not equal : give up
	
        DECB				; decrement character count
        BNE     LC91D			; keep going if more to compare
	
        BRA     LC954			; if we get here then we have a match

LC928   LDA     ,X			; get first data byte (attribute)
        BITA    #AttrEndOfDir		; end of directory ?
        BNE     LC944			; yep 
	
        INC     DosTempFileNo		; Move to next fileno
        LEAX    DirEntryLen,X		; move to next dir entry in sector
        CMPX    DosSaveBuffAddr		; beyond last entry ?
        BCS     LC908			; nope process next entry
	
        LDY     DosLSNCounter		; get current DIR LSN
        LEAY    1,Y			; increment it
        DEC     DosCurCount		; decrement directory sector count
        BNE     LC8F6			; if any left loop again
	
LC944   JSR     >DosFCBNoToAddr		; get FCB address
LC947   CLRB
        TST     FCBDirFlags,X		; check flags
        BPL     LC94E			; not a deleted file
	
        LDB     #DDErrNE		; flag file does not exist
LC94E   LEAX    FCBFilePointer,X	; point to file pointer !!!
        LDA     <DosCurrCtrlBlk
        TSTB
        RTS

LC954   PSHS    X			; save dir entry pointer
        JSR     >DosFCBNoToAddr		; get FCB address
        PULS    Y			; recover dir entry pointer
        LDA     DosTempFileNo		; get file no
	
; Fill in FCB

        STA     FCBDiskFileNo,X		; fill in fileno
        LDA     ,Y
        STA     FCBDirFlags,X		; attribute
        
	LDD     DirEntFnBlock1,Y
	STD     FCBLSNExtent1,X		; LSN of extent 1
        
	LDA     DirEntFnBlock1+2,Y
        STA     FCBSecExtent1,X		; sector count of extent 1
        STA     FCBFSNExtent2+1,X
        
	CLR     FCBFSNExtent2,X
        CLR     FCBFSNExtent1,X		; Clear FSN 
        CLR     FCBFSNExtent1+1,X
        
	LDD     DirEntFnBlock2,Y	; LSN of extent 2
        STD     FCBLSNExtent2,X
        
	LDA     DirEntFnBlock2+2,Y	; sector count of extent 2
        STA     FCBSecExtent2,X
        
	LDD     #$FFFF			; set file len to -1
        STD     FCBFileLen,X		
        STA     FCBFileLen+2,X
        BRA     LC947

;
; Read from a file.
;
; Entry :
;	A = FCB no
;	X = pointer to buffer to receive data
;	Y = no of bytes to read
;	U = MSW of file offset (sector no).
;	B = LSB of file offset (byte within sector).
;

DOSFRead   
	CLR     <DosIRQTimeFlag		; reset irq timeout
        STA     <DosCurrCtrlBlk		; set control block
        BRA     LC99E			; skip ahead

LC997   LDA     #$01
	FCB     CSkip2

LC99A   LDA     #$FF
        STA     <DosIRQTimeFlag
LC99E   STY     DosBytesRead		; save byte count
        LBEQ    DosFReadExit		; no bytes : exit
	
        STU     DosCurrSector		; save file pointer
        STB     DosSecOffset
        PSHS    X			; save buffer pointer
	
        JSR     >DosFCBNoToAddr		; conver FCB no to address in X
        LDA     FCBDrive,X		; get drive number
        STA     <LastActiveDrv
        TST     <DosIRQTimeFlag
        BNE     LC9C4
        LDD     DosBytesRead
        ADDD    13,X
        BCC     LC9C2
        INC     12,X
LC9C2   STD     13,X
LC9C4   PULS    X
        LDB     DosSecOffset
LC9C9   CLRA
        NEGB
        BNE     LC9CE
        INCA
LC9CE   CMPD    DosBytesRead
        BLS     LC9D7
        LDD     DosBytesRead
LC9D7   PSHS    D,X
        LDU     DosCurrSector
        JSR     >LCA8B
        BNE     LC9EF
        TFR     D,Y
        LDA     15,X
        BITA    #$02
        BEQ     LC9F2
        TST     <DosIRQTimeFlag
        BEQ     LC9F2
        LDB     #$98
LC9EF   LEAS    4,S
        RTS

LC9F2   LDX     2,S
        TST     1,S
        BNE     LCA22
        TST     <DosIRQTimeFlag
        BEQ     LCA03
        BPL     LCA08
        JSR     >LD303
        BRA     LCA0B

LCA03   JSR     >DOSReadAbsSector
        BRA     LCA0B

LCA08   JSR     >DOSWriteAbsSector
LCA0B   BNE     LC9EF
        INC     2,S
        LDX     DosCurrSector
        LEAX    1,X
        STX     DosCurrSector
        DEC     DosBytesRead
        LDD     DosBytesRead
        PULS    D,X
        BNE     LC9C9
        RTS

LCA22   TST     <DosIRQTimeFlag
        BMI     LCA6C
        JSR     >DOSFindAndRead
        BNE     LC9EF
        STX     $0667
        LDY     2,S
        LDB     DosSecOffset
        TST     <DosIRQTimeFlag
        BEQ     LCA60
        LDA     #$FF
        STA     2,X
        LDX     5,X
        ABX
        TFR     X,D
        DECB
        BCC     LCA44
LCA44   LDB     1,S
LCA46   LDA     ,Y+
        STA     ,X+
        DECB
        BNE     LCA46
        TFR     X,D
        TSTB
        BNE     LCA6C
        LDX     $0667
        PSHS    Y
        JSR     >TestAndFlushBuffer
        PULS    Y
        BNE     LC9EF
        BRA     LCA6C

LCA60   LDX     5,X
        ABX
        LDB     1,S
LCA65   LDA     ,X+
        STA     ,Y+
        DECB
        BNE     LCA65
LCA6C   LDX     DosCurrSector
        LEAX    1,X
        STX     DosCurrSector
        TFR     Y,X
        LDD     DosBytesRead
        SUBD    ,S++
        STD     DosBytesRead
        LEAS    2,S
        BEQ     DosFReadExit
        CLR     DosSecOffset
        CLRB
        JMP     >LC9C9

DosFReadExit   
	CLRB				; no error
        RTS

LCA8B   JSR     >DosFCBNoToAddr
LCA8E   TFR     U,D
        SUBD    $13,X
        BCS     LCAA2
        TSTA
        BNE     LCAA2
        CMPB    $17,X
        BCC     LCAA2
        ADDD    $15,X
        BRA     LCAB4

LCAA2   TFR     U,D
        SUBD    $18,X
        BCS     LCAB7
        TSTA
        BNE     LCAB7
        CMPB    $1C,X
        BCC     LCAB7
        ADDD    $1A,X
LCAB4   ORCC    #$04
        RTS

LCAB7   PSHS    U
        BSR     FindFSNinU
        BNE     LCAC8
        LDD     $066B
        CMPD    $066D
        BHI     LCACA
        LDB     #DDErrPE
LCAC8   PULS    U,PC

LCACA   SUBB    2,Y
        SBCA    #$00
        STD     $13,X
        LDA     2,Y
        STA     $17,X
        LDD     ,Y
        STD     $15,X
        TFR     Y,D
        PSHS    U
        SUBD    ,S++
        PULS    U
        CMPB    #$13
        BCC     LCAF7
        LDA     5,Y
        STA     $1C,X
        LDD     3,Y
        STD     $1A,X
        LDD     $066B
        STD     $18,X
LCAF7   BRA     LCA8E

;
; Entry : X=Address of a FCB
;	  U=FSN to find (File Sector Number)
;	  B=File number (on disk), also in $1d,X
;

FindFSNinU   
	PSHS    X			
        CLR     DosTotalSFound+1
        CLR     DosTotalSFound
        STU     DosFSNToFind		; set FSN to find 
        LDB     FCBDiskFileNo,X		; set current file to be FCB file
        STB     DosCurFileNo
        JSR     >DOSGetDirEntry		; Go get directory entry
        BNE     LCB56			; Error : exit
        
	TFR     X,U			; point u at dir entry
        PULS    X			; recover FCB pointer
	
        LEAY    DirEntFnBlock1,U	; point Y at first allocation block
        LDB     #$04			; entry counter
LCB17   LDA     ,U			; get attributes 
        ANDA    #AttrContinued		; extract continuation flag	
        BEQ     LCB20			; not a continuation block, skip
	
        LDA     DirEntFlag,U		; get dir entry flag

LCB20   PSHS    D
LCB22   LDD     DosTotalSFound		; get total sectors found so far
        ADDB    2,Y			; add sector count for this allocation block to total
        ADCA    #$00			; carry from b to a if neeeded
        STD     DosTotalSFound		; update total found
        CMPD    DosFSNToFind		; found the sector we need yet ?
        BHI     LCB55			; yep : exit
	
        LEAY    AllocEntrySize,Y	; move to next allocation entry			
        DEC     1,S			; decrement entry counter
        BNE     LCB22			; if more to do loop again

        LDB     ,S			; recover DirEntFlag or AttrContinued	
        BEQ     LCB53			; if it's 0
	
        LEAS    2,S			; drop stack frame
        STB     DosCurFileNo		

        PSHS    X			; save FCB pointer	
        JSR     >DOSGetDirEntry		; get next entry
        TFR     X,U			; make u point to dir entry
        PULS    X			; recover FCB pointer
	BEQ     LCB4D

        RTS

LCB4D   LEAY    DirEntCntBlock1,U	; point at first extension entry			
        LDB     #$07			; 7 entries in a continuation block
        BRA     LCB17

LCB53   LEAY    -3,Y
LCB55   CLRB				; flag no erroe
LCB56   LEAS    2,S			; Drop stack frame
        RTS

;
; Write data from memory to file, verify if verify on.
;

DOSFWrite   
	STA     <DosCurrCtrlBlk
        STX     $0671
        STU     $0673
        STY     $0675
        STB     $0677
        JSR     >DosFCBNoToAddr
        LDB     11,X
        STB     <LastActiveDrv
LCB6F   JSR     DOSGetFlen
        BEQ     LCB82
        CMPB    #$9C
        BEQ     LCB79
LCB78   RTS

LCB79   LDA     <DosCurrCtrlBlk
        JSR     >DOSCreateFile
        BNE     LCB78
        BRA     LCB6F

LCB82   CMPU    $0675
        BHI     LCB92
        BCS     LCB8F
        CMPA    $0677
        BCC     LCB92
LCB8F   LDB     #DDErrPE
LCB91   RTS

LCB92   PSHS    A
        LDD     $0673
        ADDB    $0677
        ADCA    $0676
        PSHS    B
        TFR     A,B
        LDA     $0675
        ADCA    #$00
        PSHS    U
        SUBD    ,S++
        TFR     B,A
        PULS    B
        BHI     LCBB8
        BCS     LCBC0
        SUBB    ,S
        BCC     LCBBC
        BRA     LCBC0

LCBB8   SUBB    ,S
        SBCA    #$00
LCBBC   BSR     LCBEC
        BNE     LCBE9
LCBC0   LEAS    1,S
        LDB     $0677
        LDX     $0671
        LDY     $0673
        LDU     $0675
        JSR     >LC997
        BNE     LCB91
        TST     DosVerifyFlag
        BEQ     LCB91
        LDB     $0677
        LDX     $0671
        LDY     $0673
        LDU     $0675
        LBRA    LC99A

LCBE9   LEAS    1,S
        RTS

LCBEC   LEAS    -12,S
        STD     ,S
        STD     10,S
        LDA     #$01
        STA     <DosIOInProgress
        JSR     >DosGetDiskGeometry
        LBNE    LCCE2
        STX     8,S
        JSR     >DosFCBNoToAddr
        LDB     $1E,X
        JSR     >DOSGetDirEntry
        LBNE    LCCE2
        STX     4,S
        LDU     DosCurDirBuff
        LDA     #$FF
        STA     2,U
        CLRA
        LDB     $18,X
        BNE     LCC1C
        INCA
LCC1C   ADDD    ,S
        TSTB
        BNE     LCC22
        DECA
LCC22   STD     ,S
        TSTA
        LBEQ    LCCCA
        LDB     #$04
        LEAY    12,X
        LDA     ,X
        BITA    #$01
        BEQ     LCC37
        LEAY    1,X
        LDB     #$07
LCC37   TST     2,Y
        BNE     LCC4F
        LDD     #$FFFF
        STD     6,S
        LEAY    -3,Y
        STY     2,S
        LDX     8,S
        TST     2,X
        LBNE    LCCE8
        BRA     LCC8D

LCC4F   TST     2,Y
        BEQ     LCC58
        LEAY    3,Y
        DECB
        BNE     LCC4F
LCC58   LEAY    -3,Y
        LDD     ,Y
        ADDB    2,Y
        ADCA    #$00
        STD     6,S
        STY     2,S
        LDX     8,S
        TST     2,X
        BEQ     LCC8D
        LDD     6,S
        SUBD    3,X
        BEQ     LCC9D
        JSR     >LCD31
        BEQ     LCC7C
        CMPB    #$94
        BNE     LCCE2
        BRA     LCCE8

LCC7C   LDX     8,S
        PSHS    A,U
        LDB     2,X
        LDX     3,X
        JSR     >LD012
        PULS    A,U
        BNE     LCCE2
        BRA     LCC92

LCC8D   JSR     >LCD31
        BNE     LCCE2
LCC92   LDX     8,S
        STU     3,X
        STA     2,X
        CMPU    6,S
        BNE     LCCE8
LCC9D   LDA     ,S
        CMPA    2,X
        BCS     LCCA5
        LDA     2,X
LCCA5   LDY     2,S
        PSHS    A
        ADDA    2,Y
        BCC     LCCB2
        PULS    A
        BRA     LCCE8

LCCB2   STA     2,Y
        LDA     2,X
        SUBA    ,S
        STA     2,X
        LDD     3,X
        ADDB    ,S
        ADCA    #$00
        STD     3,X
        LDA     1,S
        SUBA    ,S+
        STA     ,S
        BNE     LCC8D
LCCCA   LDX     4,S
        TFR     X,U
        JSR     >DosFCBNoToAddr
        LDD     10,S
        ADDD    $11,X
        BCC     LCCDB
        INC     $10,X
LCCDB   STD     $11,X
        STB     $18,U
        CLRB
LCCE2   LEAS    12,S
        CLR     <DosIOInProgress
        TSTB
        RTS

LCCE8   LDD     2,S
        TFR     D,Y
        SUBD    4,S
        CMPB    #$13
        BCC     LCCFD
        LEAY    3,Y
        STY     2,S
LCCF7   LDD     3,X
        STD     ,Y
        BRA     LCC9D

LCCFD   LDA     #$01
        JSR     >LD0FC
        BNE     LCCE2
        LDY     4,S
        STA     $18,Y
        LDB     ,Y
        ORB     #$20
        STB     ,Y
        LDB     #$FF
        STB     2,X
        LDB     #$01
        STB     ,U
        STU     4,S
        JSR     >DosFCBNoToAddr
        STA     $1E,X
        LEAY    $19,U
        LDB     #$18
LCD25   CLR     ,-Y
        DECB
        BNE     LCD25
        STY     2,S
        LDX     8,S
        BRA     LCCF7

LCD31   STS     $0601
        LEAS    -9,S
        CLR     4,S
        JSR     >DosGetDiskGeometry
        LBNE    LCE34
        LDY     ,X
        LDU     $11,S
        STU     5,S
        LEAX    1,U
        BEQ     LCD5B
        BSR     LCD8C
        BNE     LCD89
        CMPU    #$05A0
        BCS     LCD5B
        LDU     #$058E
        BRA     LCD7C

LCD5B   LEAU    ,Y
LCD5D   LDA     #$FF
        STA     ,S
LCD61   LEAU    -$12,U
        STU     -2,S
        BMI     LCD74
        BSR     LCD8C
        BEQ     LCD5D
        TST     ,S
        BPL     LCD89
        STU     ,S
        BRA     LCD61

LCD74   LDU     ,S
        STU     -2,S
        BPL     LCD89
        LEAU    ,Y
LCD7C   LEAU    $12,U
        CMPU    #$0B40
        BHI     LCD8F
        BSR     LCD8C
        BEQ     LCD7C
LCD89   LBRA    LCE0E

LCD8C   LBRA    LCE3A

LCD8F   LDA     #$FF
        STA     7,S
        LDA     4,S
LCD95   LDU     <Misc16BitScratch
        LSRA
        BCS     LCD9D
        LDU     #$05A0
LCD9D   BSR     LCD8C
        LDB     #$B4
LCDA1   LDA     ,X+
        BNE     LCDAC
        LEAU    8,U
        DECB
        BNE     LCDA1
        BRA     LCDED

LCDAC   CLRB
        STB     8,S
        PSHS    U
LCDB1   INCB
        LDA     ,X+
        INCA
        BEQ     LCDB1
        CMPB    10,S
        BLS     LCDBF
        STB     10,S
        STU     ,S
LCDBF   LEAX    -1,X
        LDA     #$08
        MUL
        LEAU    D,U
LCDC6   CLRB
        LDA     ,X+
        BNE     LCDB1
        LEAU    8,U
        TFR     X,D
        CMPB    #$B4
        BCS     LCDC6
        PULS    U
        BSR     LCE3A
        LEAU    8,U
        LDA     ,X
        LDB     8,S
        CMPB    #$01
        BHI     LCDE6
LCDE1   LEAU    -1,U
        ASLA
        BCC     LCDE1
LCDE6   ASLA
        BCC     LCDED
        LEAU    -1,U
        BRA     LCDE6

LCDED   CMPB    #$02
        BCC     LCE0E
        CMPB    7,S
        BHI     LCE0E
        LDA     7,S
        INCA
        BNE     LCE04
        STB     7,S
        STU     5,S
        LDA     4,S
        EORA    #$03
        BRA     LCD95

LCE04   LDU     5,S
        BSR     LCE3A
        BNE     LCE0E
        LDB     #DDErrDF
        BRA     LCE34

LCE0E   BSR     LCE3A
        CLRB
LCE11   INCB
        BEQ     LCE2B
        PSHS    A
        COMA
        ANDA    ,X
        STA     ,X
        PULS    A
        ASLA
        BCC     LCE24
        LDA     #$01
        LEAX    1,X
LCE24   BITA    ,X
        BNE     LCE11
        TFR     B,A
	
;LCE2B   EQU     *+1
;        CMPX    #$86FF

	FCB	Skip2
LCE2B	LDA	#$FF

        LDX     2,S
        LDB     #$FF
        STB     2,X
        CLRB
LCE34   LDS     $0601
        TSTB
        RTS

LCE3A   PSHS    Y,U
        LDA     #$01
        CMPU    #$05A0
        BCS     LCE4B
        LEAY    1,Y
        LEAU    $FA60,U
        ASLA
LCE4B   PSHS    U
        BITA    12,S
        BNE     LCE5E
        STA     12,S
        JSR     >DOSFindAndRead
        BNE     LCE34
        STX     10,S
        LDA     #$FF
        STA     2,X
LCE5E   LDX     10,S
        LDX     5,X
        PULS    D
        LSRA
        RORB
        LSRA
        RORB
        LSRA
        RORB
        LDA     3,S
        ANDA    #$07
        LDU     #$A672
        NEGA
        LDA     A,U
        ABX
        BITA    ,X
        PULS    Y,U,PC

;
; Get length of a file 
;
; Entry :
;	A = FCB no.
; 


DOSGetFlen
		STA     <DosCurrCtrlBlk		; save FCB no
        BSR     DosFCBNoToAddr		; get address of FCB
        TST     FCBDirFlags,X		; get dir flags from FCB
        BPL     LCE84			; ok skip
	
        LDB     #DDErrFF		; error : file not found
LCE83   RTS

LCE84   LDA     FCBFileLen+2,X		; number of extra bytes in last sector		
        LDU     FCBFileLen,X		; number of sectors
        LEAY    1,U			
        BNE     LCEAA
        
	JSR     >FindFSNinU		; Go find File sector no in U
        BNE     LCE83
        
	LDB     DosCurFileNo		; get file number
        STB     FCBDirNoLast,X		; save in FCB
	
LCE99   LDA     DirEntLastBytes,U	; get bytes in last sector
        STA     FCBFileLen+2,X		; add to file length
        LDU     DosTotalSFound
        TSTA				; 256 bytes in last sector ?
        BEQ     LCEA7			; yep skip ahead
	
        LEAU    -1,U			; decrement total sectors found
LCEA7   STU     FCBFileLen,X		; set file len
LCEAA   CLRB				; no error
LCEAB   RTS

;
; Converts current FCB number in DosCurrCtrlBlk to the FCB's address
; Exits with :
;	X=FCB address
; 

DosFCBNoToAddr   
	PSHS    D
        LDA     <DosCurrCtrlBlk		; get current FCB no
        LDB     #DosFCBLength		; work out offset into table
        MUL
        TFR     D,X			; get offset into X
        LEAX    DosFCB0Addr,X		; work out address of FCB
        PULS    D,PC

;
; Close All open files.
;

DOSCloseAll   
	LDA	#$09
	
        STA     <DosCurrCtrlBlk
LCEBF   BSR     DosFCBNoToAddr
        LDA     11,X
        CMPA    <LastActiveDrv
        BNE     LCECB
        BSR     DosCloseFile2
        BNE     LCEAB
LCECB   DEC     <DosCurrCtrlBlk
        BPL     LCEBF
LCECF   CLRB
        RTS

;
; Close a single file.
;

DOSCloseFile   
	STA     <DosCurrCtrlBlk
DosCloseFile2   
	BSR     DosFCBNoToAddr
        TST     ,X
        BEQ     LCECF
        LDA     <LastActiveDrv
        PSHS    A
        LDA     11,X
        STA     <LastActiveDrv
        CLR     ,X
        JSR     >DosGetDiskGeometry
        BNE     LCF0E
        TST     5,X
        BEQ     LCEF3
        DEC     5,X
        BEQ     LCEF3
        CLRB
        BRA     LCF0E

LCEF3   LDB     2,X
        BEQ     LCF04
        PSHS    X
        LDX     3,X
        JSR     >LD012
        PULS    X
        BNE     LCF0E
        CLR     2,X
LCF04   JSR     >DOSSyncDir
        LDU     #DosD0Online-1
        LDA     <LastActiveDrv
        CLR     A,U
LCF0E   PULS    A
        STA     <LastActiveDrv
        TSTB
LCF13   RTS

;
; Create a file, with backup.
;

DOSCreateFile   
	STA     $067D
        JSR     >DosFCBNoToAddr
        STX     $0678
        LDB     #$0C
        LDU     #DosCurDriveInfo
LCF22   LDA     ,X+
        STA     ,U+
        DECB
        BNE     LCF22
        LDD     -4,U
        STD     $067A
        LDA     -2,U
        STA     $067C
        LDD     #$4241
        STD     -4,U
        LDA     #$4B
        STA     -2,U
        
	TST	3,X
	
        BMI     LCF69
        JSR     >DOSOpenFile
        CMPB    #$A0
        
	BEQ	LCF51
	
        TSTB
        BNE     LCF13
        JSR     >DOSDeleteFile
        BNE     LCF13

	LEAX	12,X
LCF51	CLR	-12,X
	JSR	>LDFCA
	BEQ	LCF69
	
	JSR     >DOSRename
        BEQ     LCF61
        CMPB    #$9C
        BNE     LCF13
LCF61   LDA     $067D
        JSR     >DOSCloseFile
        BNE     LCF13
LCF69   LDD     $067A
        STD     $0658
        LDA     $067C
        STA     $065A
        JSR     >DOSOpenFile
        BEQ     LCF7E
        CMPB    #$A0
        BNE     LCF13
LCF7E   STA     <DosCurrCtrlBlk
        JSR     >DosFCBNoToAddr
        TST     15,X
        BMI     LCF8A
        LDB     #DDErrFE
LCF89   RTS

LCF8A   CLRA
        JSR     >LD0FC
        BNE     LCF89
        JSR     >DosFCBNoToAddr
        STA     $1D,X
        STA     $1E,X
        LDB     #$1C
LCF9B   CLR     B,X
        DECB
        CMPB    #$0C
        BCC     LCF9B
        LDB     #$18
LCFA4   CLR     B,U
        DECB
        BPL     LCFA4
        LEAU    1,U
        LDB     #$0B
LCFAD   LDA     ,X+
        STA     ,U+
        DECB
        BNE     LCFAD
        CLRB
LCFB5   RTS

;
; Delete a file from disk.
;

DOSDeleteFile   
	JSR     >LD0BC
        BNE     LCFB5
        PSHS    X
        LDB     FCBDiskFileNo,X
        JSR     >LD208
        BNE     LD00C
        TFR     X,U
        PULS    X
        LEAY    12,U
        LDB     #$04
LCFCD   LDA     ,U
        ANDA    #$20
        BEQ     LCFD6
        LDA     $18,U
LCFD6   PSHS    D
        LDB     #$81
        STB     ,U
        PSHS    X
LCFDE   LDX     ,Y++
        LDB     ,Y+
        BEQ     LCFEE
        PSHS    Y
        BSR     LD012
        PULS    Y
        LBNE    LC9EF
LCFEE   DEC     3,S
        BNE     LCFDE
        PULS    X
        LDB     ,S
        BEQ     LD00B
        LEAS    2,S
        PSHS    X
        JSR     >LD208
        TFR     X,U
        PULS    X
        BNE     LCFB5
        LEAY    1,U
        LDB     #$07
        BRA     LCFCD

LD00B   CLRB
LD00C   CLR     <DosIOInProgress
        LEAS    2,S
        TSTB
        RTS

LD012   CLRA
        PSHS    A
        PSHS    D,X
        JSR     >DosGetDiskGeometry
        BNE     LD09A
        LDY     ,X
        LDD     2,S
        SUBD    #$05A0
        BCS     LD031

        LEAY    1,Y
        STD     2,S
        ADDD    ,S
        SUBD    #$05A0
        BCC     LD098
LD031   LDD     2,S
        ADDD    ,S
        SUBD    #$05A0
        BCS     LD041
        STB     4,S
        NEGB
        ADDB    1,S
        STB     1,S
LD041   JSR     >DOSFindAndRead
        BNE     LD09A
        LDA     #$FF
        STA     2,X
        LDD     2,S
        LSRA
        RORB
        ROR     ,S
        LSRA
        RORB
        ROR     ,S
        LSRA
        RORB
        ROR     ,S
        LDX     5,X
        ABX
        LDB     #$01
        LDA     ,S
        BEQ     LD066
LD061   ASLB
        SUBA    #$20
        BNE     LD061
LD066   STB     ,S
        LDB     1,S
LD06A   LDA     ,S
        ORA     ,X
        STA     ,X
        DECB
        BEQ     LD08B
        ASL     ,S
        BCC     LD06A
        LDA     #$01
        STA     ,S
        LEAX    1,X
LD07D   CMPB    #$10
        BCS     LD06A
        LDA     #$FF
        STA     ,X+
        STA     ,X+
        SUBB    #$10
        BNE     LD07D
LD08B   LDX     #$05A0
        LEAS    4,S
        LDB     ,S+
        BNE     LD095
        RTS

LD095   LBRA    LD012

LD098   LDB     #$90
LD09A   LEAS    5,S
LD09C   RTS


;
; Protect/unprotect a file.
;

DOSProtect   
	STA     <DosCurrCtrlBlk
        JSR     >DosFCBNoToAddr
        LDA     15,X
        BMI     LD0CC
        TSTB
        BEQ     LD0AC
        ORA     #$02
	
;LD0AC   EQU     *+1
;        CMPX    #$84FD
	FCB	Skip2
LD0AC	ANDA	#$FD
	
        STA     15,X
        LDB     $1D,X
        JSR     >LD208
        BNE     LD09C
        STA     ,X
        CLRB
LD0BB   RTS

LD0BC   STA     <DosCurrCtrlBlk
        JSR     >DosFCBNoToAddr
        LDA     15,X
        BMI     LD0CC
        BITA    #$02
        BEQ     LD0BB
        LDB     #DDErrPT
        RTS

LD0CC   LDB     #DDErrFF
LD0CE   RTS


;
; Rename a file.
;

DOSRename
	JSR     >LD0BC
        BNE     LD0CE
        LDB     #$0B
        LDU     #DosCurDriveInfo
LD0D9   LDA     ,U+
        STA     ,X+
        DECB
        BNE     LD0D9
        LEAX    -11,X
        LDB     FCBDiskFileNo,X
        JSR     >LD208
        BNE     LD0CE
        LDU     #DosCurDriveInfo
        LDB     #$0B
        LEAX    1,X
LD0F1   LDA     ,U+
        STA     ,X+
        DECB
        BNE     LD0F1
        CLR     <DosIOInProgress
        CLRB
LD0FB   RTS

LD0FC   NEGA
        STA     $067E
        JSR     >DosGetDiskGeometry
        BNE     LD0FB
        LDX     ,X
        PSHS    X
        LEAX    2,X
LD10B   STX     $066F
        TFR     X,Y
        JSR     >DOSFindAndRead
        BNE     LD14A
        LDU     5,X
        LDB     #$0A
        TST     $067E
        BPL     LD123
        NEG     $067E
        BRA     LD12A

LD123   LDA     ,U
        BMI     LD142
        INC     $067E
LD12A   LEAU    $19,U
        DECB
        BNE     LD123
        LDX     $066F
        LEAX    1,X
        TFR     X,D
        SUBD    ,S
        CMPB    #$12
        BCS     LD10B
        LEAS    2,S
        LDB     #DDErrFD
        RTS

LD142   LDA     #$FF
        STA     2,X
        LDA     $067E
        CLRB
LD14A   LEAS    2,S
        RTS

;
; Get free space on a disk.
;

DOSGetFree   
		BSR     DosGetDiskGeometry
        BNE     LD161
        LDY     ,X
        LDX     <Misc16BitScratch
        BSR     LD162
        BNE     LD161
        LEAY    1,Y
        BSR     LD162
        BNE     LD161
        CLRB
LD161   RTS

LD162   PSHS    X
        JSR     >DOSFindAndRead
        BNE     LD14A
        LDU     5,X
        PULS    X
        LDB     #$B4
LD16F   LDA     ,U+
LD171   LSRA
        BCC     LD176
        LEAX    1,X
LD176   TSTA
        BNE     LD171
        DECB
        BNE     LD16F
        RTS
;
; Get geometry for a disk and set the apropreate low memory vars.
;
; Entry : DosLastDrive, set to last drive
;
; Exit  : Drive vars setup in low ram, to be same as disk in drive.
;	  X=Address of buffer detail entry for buffer to use

DosGetDiskGeometry
	LDX     #Drv0Details		; Point at drive details
        LDU     #DosD0Online-1		; Point at drive online table
        LDB     #DrvDeatailLen		; Get drive table entry len
        LDA     <DosLastDrive		; Get last used drive

        LEAU    A,U			; Point U at drive online flag
        DECA				; Make zero based
        MUL				; Calculate offset of drive we need
        LEAX    D,X
        TST     ,U			; Is drive online ?

        BNE     LD1CF			; Yes : exit
	
	LDY     #SectorsPerTrack*DirPrimary 	; First sector of DIR track ($0168)
        LDA     #SectorsPerTrack	; Set sectors per track for this drive
        
	JSR	LC0A7

	PSHS    X			; Save drive detail pointer
        JSR     >DOSFindAndRead		; Find free buffer and read sector
;	jsr	>CON_DumpRegsWait
        LBNE    LCB56			; Error : exit
	
; At this point X points to buffer details ???
	
        LDX     BuffAddr,X		; Get address of buffer data
        LDD     DirTracks1s,X		; Get complements of tracks/secs per track
        COMA				; Complemetn them for compare
        COMB
        CMPD    DirTracks,X		; compare them to validate the disk
        PULS    X			; restore drive detail pointer
        BNE     LD1D1			; Not the same, not valid disk.
	
        STB     DosSecTrkTblOfs,U	; Set Sectors/Track for this disk
        STA     DosTracksTblOfs,U	; Set tracks for this disk
        DEC     ,U			; Mark drive online
        CMPB    #SectorsPerTrack	; Disk single sided ?
        BEQ     LD1C0			; yes : skip on

        CLRB				; zero it
LD1C0   PSHS    B			; save it
        CLR     BuffFlag,X		; Clear buffer flag
        LDD     #SectorsPerTrack*DirPrimary 	; First sector of DIR track ($0168)
        TST     ,S+			; Do we need to double ? 
        BNE     LD1CD			; no : skip
	
        ASLB				; Multiply D by 2, as we have 2 sides
        ROLA
LD1CD   STD     ,X			; save it
LD1CF   
	CLRB				; No error
        RTS				; Return

LD1D1   LDB     #DDErrIV		; Flag error
LD1D3   RTS

;
; Get directory entry.
;
; Entry : B= File number(on disk) to get entry for???
;
; Exit  : X=Pointer to required Dir entry.
;

DOSGetDirEntry   
	LDA     #$FF			; Init sector counter
LD1D6   INCA				; increment sector counter
        SUBB    #DirEntPerSec		; Decrement file no, by a sectors worth of files
        BGE     LD1D6			; Done all ? no : continue looping
	
        ADDB    #DirEntPerSec		; Compensate for over loop
	
; At this point A contains the sector number within the directory that we are intereted in.
; and B contains the entry within that sector of the file's details.
	
        PSHS    D			; Save them
        BSR     DosGetDiskGeometry	; Setup disk geometry from disk in drive
        LBNE    LCB56			; Error : exit
	
        LDD     ,X			; Get LSN number from buffer
        ADDD    #$0002			; Advance past bitmap sectors
        ADDB    ,S+			; Add sector offset calculated above
        ADCA    #$00			; Deal with carry
        STD     DosCurLSN		; Save LSN
        TFR     D,Y			; Get LSN into Y
        JSR     >DOSFindAndRead		; Find free buffer and read sector
        PULS    A			; Retrieve entry number witin sector
        BNE     LD1D3			; Error : exit
	
        TFR     X,U			
        LDB     #$19			; Length of dir entry
        MUL				; Calculate offset
        STX     DosCurDirBuff		; Saave block def pointer
        LDX     BuffAddr,X		; Get pointer to block data
        LEAX    D,X			; Get offset of DIR entry into X
        CLRB				; Flag no error
        RTS

LD208   PSHS    D
        BSR     DOSGetDirEntry		; Get directory entry we are interested in
        LBNE    LCB56			; Error : exit
        LDY     DosCurDirBuff		; Get Buffer def block for this entry
        LDA     #$FE			; Set flag
        STA     BuffFlag,Y
        CLRB				; Flag no error
        PULS    D,PC			; Restore and return

;
; Find a free disk buffer.
;
; Entry	: Y=LSN
;
; Exits : U=pointer to detail entry for free buffer.
;	  B=Error code
;

FindFreeBuffer   
        LDX     #Buff1Details		; Point at disk buffer detail table
        LDU     <Misc16BitScratch	; Load U with 0 ?
        LDB     #BuffCount		; 4 Disk buffers
LD222   LDA     BuffFlag,X		; Get buffer flag in A
        BEQ     LD23D			; Zero ?
	
        CMPA    #BuffInUse		; Is buffer in use ?
        BEQ     LD23F
	
        CMPY    ,X
        BNE     LD239
	
        LDA     <LastActiveDrv		; Get last drive
        CMPA    BuffDrive,X		; Is this buffer using the same drive ?
        BNE     LD239			; nope, skip on
        BSR     MakeBuffYoungest	; Make this the youngest buffer
        CLRB				; Flag no error
        RTS

LD239   TST     BuffFlag,X		; Is buffer free ?
        BNE     LD23F			; nope, look at next
LD23D   TFR     X,U			; Select this buffer
LD23F   LEAX    BuffDetailSize,X	; move on to next buffer detail entry
        DECB				; Decrement counter
        BNE     LD222			; Any more to check ? : yes loop again
        LDB     #$FF			; Flag error
LD246   RTS

;
; Find a free buffer and read sector.
;
; Entry : 
;	Y = LSN to read
;	U = ???
; Exit :
;	X = pointer to buffer entry 
;	U preserved
;	B = error code 
;
DOSFindAndRead   
	PSHS    U
        JSR     >FindFreeBuffer		; Find free buffer, pointer to details returned in U
        LBEQ    LCB56
	
	LEAX    ,U			; Make X point to details
        PULS    U
        BNE     LD25A
	
        BSR     FindFreeDiskBuffer	; Find buffer to read data into
        BNE     LD246
	
LD25A   CLR     BuffFlag,X		; Make buffer free
        STY     ,X
        LDA     <LastActiveDrv		; Get last drive
        STA     BuffDrive,X		; Set this drive's buffer
        PSHS    X			; Save buffer detail pointer
        LDX     BuffAddr,X		; Get address of buffer
        JSR     >DOSReadAbsSector	; Read the sector
;	JSR	>CON_DumpRegsWait
        PULS    X			; Restore buff detail pointer
        BNE     LD246			; Error : exit
	
	LDA     #$01			; Set flag to 1
        STA     BuffFlag,X
        CLRB				; No error
        RTS

;
; Find least recently used disk buffer, if none, and there is 
; a dirty buffer, then flush it and use that one.
;
; Exit : X=pointer to buffer info block.
;

FindFreeDiskBuffer   
	PSHS    D,Y,U
LD276   LDX     #Buff1Details		; Point to disk buffer table
        LDB     #$04			; Check 4 buffers
LD27B   LDA     BuffAge,X		; Get buffer age
        CMPA    #$01			; Oldest ?
        BEQ     LD286			; Yes go process it
	
        LEAX    7,X			; Do next bufffer
        DECB				; Decrement buffer count
        BNE     LD27B			; More : do next

LD286   BSR     MakeBuffYoungest	; Adjust ages of all other buffers
        LDA     BuffFlag,X		; Get buffer flag byte 
        CMPA    #$55			; In use ???
        BEQ     LD276			; yes, select another buffer
	
        INCA				; Check for Flag=$FF
        BNE     LD295			; no : skip on
        
	DEC     BuffFlag,X		; yes, select another buffer
	BRA     LD276

LD295   BSR     TestAndFlushBuffer	; Check for buffer flush needed ?
        
	BEQ	LD29B
	STB	1,S
	
LD29B	PULS    D,Y,U,PC		; restore and return

MakeBuffYoungest   
	LDB     #BuffCount		; Process 4 buffers
        LDA     BuffAge,X		; Get current buffer Age
        LDU     #Buff1Details		; Point to disk buffer table
LD2A4   CMPA    BuffAge,U		; Compare to current buffer age
        BHI     LD2AA			; higher ? skip
        DEC     BuffAge,U		; Decrement Age byte (make older)
	
LD2AA  	LEAU    BuffDetailSize,U	; Do next buffer
        DECB				; Decrement count
        BNE     LD2A4			; More : do next
        LDA     #$04			; Mark this as youngest buffer
        STA     BuffAge,X
        RTS

TestAndFlushBuffer   
	TST     BuffFlag,X		; Buffer dirty ?
        BMI     FlushBuffer		; Yes, flush it !
        CLRB				; No error ?
        RTS

FlushBuffer
	LDA     <DosLastDrive		; Get last drive accessed
        PSHS    A			; save it on stack
        PSHS    X			; Save buffer pointer
        LDA     #$FF			; Flag Dos IO in progress
        STA     <DosIOInProgress
        CLR     BuffFlag,X		; Flag buffer no longer dirty
        LDA     BuffDrive,X		; Get drive this buffer refers to
        STA     <DosLastDrive		; Save in last accessed drive
        LDY     ,X			; get LSN ?
        LDX     BuffAddr,X		; Get buffer pointer
        JSR     >DOSWriteAbsSector	; Write it

        PULS    X			; Retrieve buffer pointer
        BNE     LD2F2			; no error : skip ahead

        LDA     <DskTrackNo		; Get current track no
        CMPA    #DirPrimary		; track 20 (directory) ?
        BNE     LD2ED			; no : skip ahead

;
; I do not have a clue why this code does this, it seems to take a byte from
; the basic rom do some stuff to it and update the Directory sector status table 
; with it !
; 
; Looking at $A673, the 8 bytes before it are $80,$40,$20,$10,$08,$04,$02,$01
; This is the 2 colour pixel mask table, but is a convenient table for mapping a bit
; number to the bit it represents.
;
	
	LDU     #PixMaskTable4Col	; This for some strange reason points U at basic rom !!!	
        LDA     <DosLastDrive		; get last drive
        NEGA
        LDA     A,U

        LDU     #DosDirSecStatus-1	; Point to directory status table
        LDB     <DskSectorNo		; get sector number
        ORA     B,U			; Put a byte in table
        STA     B,U

LD2ED   LDA     #$01			; Mark bufer as youngest
        STA     BuffFlag,X
        CLRB
	
LD2F2   PULS    A
        STA     <DosLastDrive		; Restore last drive
        CLR     <DosIOInProgress	; Mark no io in progress
        TSTB
        RTS

;
; Write absolute sector.
;
; Entry	:    X=Address to store data
;	     Y=LSN to read
;	 $00EA=Drive number
;

DOSWriteAbsSector   
	BSR     CalcTrackFromLSN	; Setup disk vars in low ram with trackno
        JSR     >DosDoWriteSec2

LD2FF   LDX     <DiskBuffPtr		; Restore buffer pointer 
        TSTB				; Test for Error
        RTS				; return to caller

LD303   BSR     CalcTrackFromLSN	; Setup disk vars in low ram with trackno
        JSR     >DosDoReadSec2
        BRA     LD2FF
;
; Read absolute sector.
;
; Entry	:    X=Address to store data
;	     Y=LSN to read
;	 $00EA=Drive number
;
DOSReadAbsSector   
	BSR     CalcTrackFromLSN
        JSR     >DosDoReadSec
        BRA     LD2FF

;
; Calculate track from Logical sector number.
;
; Entry : X=Buffer pointer
;	  Y=LSN to read/write
;
; Exit  : D=Disk track no, Low ram vars also set.
;

CalcTrackFromLSN  	
	STX     <DiskBuffPtr		; Save in buffer pointer
        LDX     #DosD0SecTrack-1	; Point to Sec/Track table
        LDB     <LastActiveDrv		; Get last drive
        LDB     B,X			; Get Sec/Track for that drive
        CLRA
        PSHS    D			; Save it
        CLR     ,-S			; Make room on stack
        TFR     Y,D			; Get LSN into D
	
; Calculate which track we need

LD321   INC     ,S			; Inc track counter
        SUBD    1,S			; Decrement sec/track from LSN
        BPL     LD321			; keep looping till it goes -ve
	
        ADDB    2,S			; Compensate for over-loop
        LDA     ,S			; Get track needed
        DECA				; Compensate track for over loop
        INCB
        LEAS    3,S			; Drop stack temps
        STD     <DskTrackNo		; Save track no
        RTS


;
; Copy command dispatch routine
;
; Syntax :
;	COPY filespec TO filespec	
;
; Stack setup as follows 
;
; offset	size 	purpose
; 0		1	Source FCB number
; 1		1	Destination FCB number
; 2		2	Buffer pointer ?
; 4		3	File pointer pos
;

CmdCopy 
	JSR     >DOSCloseAll		; Close all files & devices
        BNE     LD36B			; Error : exit
        LEAS    -7,S			; Make room on stack
	
;
; Make a buffer for copying the file.
;
        TFR     S,D			; move to D
        SUBD    #$0100			; Make room for 1 sector
        SUBD    <BasVarEnd		; Will we overwrite basic ?		
        LBMI    BasOMError		; yes : error, exit
	
        CLRB
        TSTA				; overwrite zero page ?
        LBEQ    BasOMError		; yes : error, exit
        STD     2,S			; IO buffer pointer
 
        JSR     >DosGetFilenameAndOpen	; Get source filename FCB number in A
        BNE     LD36B			; Error : exit
        STA     ,S			; save FCB no of source
        
		JSR		DOSGetFlen		; Get file length
        BNE     LD36B			; Error : exit
        JSR     <BasChrGetCurr		; scan current char from params
        CMPA    #$BC			; "TO" token
        LBNE    BasSNError		; no : Error
	
        JSR     <BasChrGet		; Get next character
        JSR     >DosGetFilenameAndOpen	; Get dest filename FCB number in A		

        BEQ     LD36E			; No error : continue
        CMPB    #$A0
        BEQ     LD36E			; No error : continue
	
LD36B   JMP     >DosHookSysError	; Error : exit

LD36E   STA     1,S			; Save destination FCB number
        JSR     >DOSCreateFile		; re-write destination file
        BNE     LD36B			; Error : exit
	
LD375   LDA     ,S			; Get source FCB no
        STA     <DosCurrCtrlBlk		; Save current FCB
        JSR     >DosFCBNoToAddr		; Get FCB address
	
; Compare file pointer position with file length for source file
	
        LDD     12,X
        CMPD    $10,X
        BCS     LD38B
        LDA     14,X
        CMPA    $12,X
        BEQ     LD3DB
LD38B   LDU     12,X
        LDA     14,X
        STA     6,S
        STU     4,S
        LDD     2,S
        ADDD    5,S
        STD     5,S
        BCC     LD39D
        INC     4,S
LD39D   LDA     4,S
        SUBA    $10,X
        BCS     LD3B2
        LDD     5,S
        SUBD    $11,X
        BLS     LD3B2
        LDD     $11,X
        SUBD    13,X
        STD     2,S
	
LD3B2   LDA     ,S			; Get source FCB no
        LDU     12,X			; Get number of bytes to read
        LDB     14,X			; Y:B=position to read from
        LDY     2,S			
        LDX     <BasVarEnd		; Point to end of basic memory
        JSR     >DOSFRead		; Read from source file
        BNE     LD36B			; error : exit
        
	LDA     1,S			; Get destination FCB No
        STA     <DosCurrCtrlBlk		; Save in current ctrl block no
        JSR     >DosFCBNoToAddr		; Get addres of Dest FCB
	
        LDY     $10,X
        LDB     $12,X
        LDU     2,S
        LDX     <BasVarEnd		; Point to end of basic memory
        JSR     >DOSFWrite		; Write to destination

        BNE     LD36B			; Error : exit			
        BRA     LD375			; continue copying

LD3DB   JSR     >DOSCloseAll		; Close all files, finished copy
        LBNE    LD36B			; Error !	
        LEAS    7,S			; drop stack frame
        RTS

;
; Merge command dispatch routine.
;
; Syntax :
;	MERGE filespec
;

CmdMerge   
	JSR     >DosValidateAndOpenBas
        BNE     LD433
        JSR     >ReadValidateHeader
        BNE     LD433
        CMPA    #$01
        BNE     ErrorFM
        LDU     <$1B
        LDY     <$19
        PSHS    Y,U
        LEAU    1,U
        STU     <$19
        JSR     >LD4E0
        PULS    X,U
        STU     <$1B
        STX     <$19
        LEAU    1,U
LD409   LDD     ,U++
        BEQ     LD42B
        LDD     ,U++
        STD     $02DA
        STD     <$2B
        CLRB
        LDX     #$02DC
LD418   INCB
        LDA     ,U+
        STA     ,X+
        BNE     LD418
        ADDB    #$04
        STB     <$03
        PSHS    U
        BSR     LD436
        PULS    U
        BRA     LD409

LD42B   CLR     DosRunLoadFlag
        LBRA    LD4BD

ErrorFM LDB     #DBErrFM
LD433   JMP     >DosHookSysError	; Error : exit

LD436   JSR     >$83FF
        BCS     LD44D
        LDD     <$47
        SUBD    ,X
        ADDD    <$1B
        STD     <$1B
        LDU     ,X
LD445   LDA     ,U+
        STA     ,X+
        CMPX    <$1B
        BNE     LD445
LD44D   LDD     <$1B
        STD     <$43
        ADDB    <$03
        ADCA    #$00
        STD     <$41
        JSR     >$831C
        LDU     #$02D8
LD45D   LDA     ,U+
        STA     ,X+
        CMPX    <$45
        BNE     LD45D
        LDX     <$41
        STX     <$1B
        JMP     >$83ED

;
; Read load header from disk and validate it, error if read error or invalid.
;

ReadValidateHeader   
	LDX     #DosCurDriveInfo		; Get current drive info
        LDY     #$0009
        LDU     <Misc16BitScratch		; U=0 ?
        CLRB
        LDA     <DosCurrCtrlBlk			; Get current FCB address
        JSR     >DOSFRead			; Go read the file
        BEQ     ValidateHeader				; No Error : continue
        RTS

;
; Validate load header checks for marker bytes 
;

ValidateHeader   
	LDA     #$55				; First marker byte
        LDX     #DosCurDriveInfo		; Get current drive info
        CMPA    ,X				; Marker found?
        BNE     ErrorFM				; nope : error 
	
        COMA					; Check for second marker $AA
        CMPA    8,X				; Marker found ?
        BNE     ErrorFM				; nope : error
        LDA     1,X				; get filetype
        CLRB					; no error
        RTS

DosHookRun   CLR     $0614
        CLR     $0619
        CLR     $0617
        CLR     $0618
        CMPA    #$22
        BEQ     LD4A2
        TSTA
        RTS

LD4A2   LEAS    2,S
        LDB     #$01
	
	FCB	$21
;
; Load command dispatch routine
;
; Syntax :
;	LOAD filespec
;	LOAD filespec,offset
;

CmdLoad   
	CLRB					; Set run/load flag to load
        STB     DosRunLoadFlag

        JSR     >DosValidateAndOpenBas		; Open supplied filename
        BNE     LD433				; Error : exit
	
        JSR     >ReadValidateHeader
        LBNE    LD433
	
        CMPA    #$01
        BNE     LD504
        BSR     LD4E0
LD4BD   CLR     <DosIOInProgress
        LDX     <$19
        JSR     >$85EE
        LDX     <$27
        STX     <$23
        LDX     <$1B
        STX     <$1D
        STX     <$1F
LD4CE   JSR     >$8514
        JSR     >$8434
        TST     DosRunLoadFlag
        BEQ     LD4DC
        JMP     >$849F

LD4DC   CLRA
        JMP     >$8371

LD4E0   LDD     4,X
        TFR     D,Y
        ADDD    <$19
        STD     <$1F
        LDB     #$40
        JSR     >$8331
LD4ED   LDA     <DosCurrCtrlBlk
        LDU     <Misc16BitScratch
        LDB     #$09
        LDX     <$19
        JSR     >DOSFRead
        LBNE    DosHookSysError
        JSR     >$83ED
        LEAX    2,X
        STX     <$1B
LD503   RTS

LD504   CMPA    #$02
DD507   EQU     *+1
        LBNE    ErrorFM
        LDU     6,X
        STU     <$9D
        JSR     <BasChrGetCurr
        BEQ     LD524
        PSHS    X
        BSR     LD581
        TFR     X,U
        PULS    X
        LDD     6,X
        SUBD    2,X
        STU     2,X
        ADDD    2,X
        STD     <$9D
LD524   LDY     4,X
        LDA     <DosCurrCtrlBlk
        LDB     #$09
        LDU     <Misc16BitScratch
        LDX     2,X
        JSR     >DOSFRead
        LBNE    DosHookSysError
        TST     DosRunLoadFlag
        BEQ     LD503
        JMP     [>$009D]

CmdSave   JSR     >VarGetStr
        JSR     >$8877
        JSR     <BasChrGetCurr
        BEQ     LD587
        LDY     #DosExtBin
        BSR     LD572
        BSR     LD581
        STX     $0652
        BSR     LD581
        TFR     X,D
        CMPX    $0652
        LBCS    DosPRError
        SUBD    $0652
        LBMI    BasFCError
        STD     $0654
        BSR     LD581
        STX     $0656
        LDB     #$02
        BRA     LD5A1

LD572   JSR     >LD6E2
        BEQ     LD57B
        CMPB    #$A0
        BNE     LD5BB
LD57B   JSR     >DOSCreateFile
        BNE     LD5BB
        RTS

LD581   JSR     >VarCKComma
        JMP     >$8E83

LD587   LDY     #DosExtBas
        BSR     LD572
        LDX     <$19
        STX     $0652
        LDD     <$1B
        SUBD    <$19
        STD     $0654
        LDX     #BasFCError
        STX     $0656
        LDB     #$01
LD5A1   LDX     #$0650
        LDA     #$55
        STA     ,X
        COMA
        STA     8,X
        STB     1,X
        LDA     <DosCurrCtrlBlk
        CLRB
        LDY     <Misc16BitScratch
        LDU     #$0009
        JSR     >DOSFWrite
        BEQ     LD5BE
LD5BB   JMP     >DosHookSysError

LD5BE   LDA     <DosCurrCtrlBlk
        LDB     #$09
        LDX     $0652
        LDU     $0654
        LDY     <Misc16BitScratch
        JSR     >DOSFWrite
        BNE     LD5BB
        CLR     <DosIOInProgress
        RTS

CmdChain   
	JSR     >DosValidateAndOpenBas
        LBNE    LD6F8
        JSR     >ReadValidateHeader
        LBNE    DosHookSysError
        CMPA    #$01
        LBNE    ErrorFM
        JSR     >LD669
        LDD     4,X
        STD     $0656
        ADDD    <$19
        SUBD    <$1B
        PSHS    D
        ADDD    <$1F
        STD     <$1F
        LDB     #$40
        JSR     >$8331
        LDD     ,S
        BPL     LD611
        LDX     <$1B
        LEAU    D,X
LD606   LDA     ,X+
        STA     ,U+
        CMPU    <$1F
        BLS     LD606
        BRA     LD61F

LD611   LDX     <$1F
        LEAX    1,X
        LEAU    D,X
LD617   LDA     ,-X
        STA     ,-U
        CMPX    <$1B
        BCC     LD617
LD61F   LDD     ,S
        ADDD    <$1B
        STD     <$1B
        PULS    D
        ADDD    <$1D
        STD     <$1D
        LDU     <$1F
        LDY     <$1D
        JSR     <BasChrGetCurr
        BEQ     LD652
        LDB     #$2C
        JSR     >$89AC
        JSR     >$869A
        JSR     >$8424
        STU     <$1F
        STY     <$1D
        LDY     $0656
        JSR     >LD4ED
        LDD     <$2B
        JSR     >$85E7
        BRA     LD661

LD652   JSR     >$841F
        STU     <$1F
        STY     <$1D
        LDY     $0656
        JSR     >LD4ED
LD661   LDB     #$01
        STB     DosRunLoadFlag
        JMP     >LD4CE

LD669   LDU     <$A6
        PSHS    X,U
        LDX     <$1B
        LEAX    2,X
LD671   CMPX    <$1D
        BCC     LD67F
        TST     -1,X
        BPL     LD67B
        BSR     LD6AD
LD67B   LEAX    7,X
        BRA     LD671

LD67F   LDU     <$1D
LD681   CMPU    <$1F
        BCC     LD6A5
        LDD     2,U
        LEAX    D,U
        PSHS    X
        TST     1,U
        BPL     LD6A1
        LDB     4,U
        CLRA
        ASLB
        LEAX    D,U
        LEAX    5,X
LD698   JSR     >LD6AD
        LEAX    5,X
        CMPX    ,S
        BCS     LD698
LD6A1   PULS    U
        BRA     LD681

LD6A5   JSR     >$8CD7
        PULS    X,U
        STU     <$A6
        RTS

LD6AD   PSHS    X,U
        CMPX    <$21
        BCC     LD6C6
        LDB     ,X
        BEQ     LD6C6
        JSR     >$8CB3
        TFR     X,U
        LDY     ,S
        LDX     2,Y
        STU     2,Y
        JSR     >$B7CC
LD6C6   PULS    X,U,PC

LD6C8   LDY     #DosExtDat		; get pointer to extension
        BRA     DosGetFilenameAndOpenExt


;
; Validate and open Basic program file supplied on command
;

DosValidateAndOpenBas   
	LDY     #DosExtBas
        BRA     DosGetFilenameAndOpenExt

;
; Get a filename from Dos and open it
; 	Takes a string supplied on command name, fetches it and
;	tries to open the file of that name.
; 
; If entered at DosGetFilenameAndOpenExt then extension must be pointed to by Y
;
; Exits with :-
;		A=FCB number for opened file
;		B= Error code ?
;		Y=ptr to extension
;

DosGetFilenameAndOpen   
	LDY     #DosExtNone		; Point to Blank extension

DosGetFilenameAndOpenExt   
	PSHS    Y			; save extension pointer
        JSR     >VarGetStr		; Get filename from command line
        JSR     >VarGetExpr
        PULS    Y
	
LD6E2   LDX     <BasVarAssign16		; get address of filename string
        PSHS    X
        LDB     ,X
        LDX     2,X
        JSR     >LC7B7			; validate and open file
        PULS    X
        PSHS    D
        JSR     >VarDelVar		; delete var
        PULS    D
        TSTB
        RTS

LD6F8   JMP     >DosHookSysError

DosCloseAllFiles   
	LDA     #$09			; set current control block to 9
        
	STA     <DosCurrCtrlBlk
	
LD6FF   JSR     >DosCloseFile2		; Try to close the file for this block
        BNE     LD71D			; error :
	
        DEC     <DosCurrCtrlBlk		; do next
        BPL     LD6FF
	
LD708   CLR     <DosIOInProgress	; Flag I/O not in progress
LD70A   RTS

LD70B   
	TSTB
	BLE	LD70A
	CLR	<DTextDevN		; reset device number
	LEAS    2,S			; drop return address
	TST	<$F7			
	BNE	LD720
	
	STB     <LastActiveDrv		; save last active drive
        
	JSR     >DOSCloseAll		; close all files
LD71B   BEQ     LD708			; exit
	
LD71D   JMP     >DosHookSysError

LD720
	JSR	DosCloseFile2		; Close a file
	BRA	LD71B
;
; Create command dispatch routine.
; 
; Syntax :
;	CREATE filespec,length	
;

CmdCreate   
	JSR     >LD6C8
        BEQ     LD732
        CMPB    #$9E
        BEQ     LD732
        CMPB    #$A0
        BNE     LD71D
LD732   LDA     <DosCurrCtrlBlk
        JSR     >DOSCreateFile
        BNE     LD71D
        JSR     <BasChrGetCurr
        BEQ     LD773
        JSR     >VarCKComma
        JSR     >VarGetStr
        JSR     >LD884
LD746   TST     <BasVarFPAcc1+2
        BEQ     LD75C
        LDD     #$FF00
        BSR     LD76E
        LDD     <BasVarAssign16
        SUBD    #$FF00
        STD     <BasVarAssign16
        BCC     LD746
        DEC     <BasVarFPAcc1+2
        BNE     LD746
LD75C   LDD     <BasVarAssign16
        BEQ     LD70A
        CMPD    #$FF00
        BLS     LD76E
        SUBD    #$FF00
        BSR     LD76E
        LDD     #$FF00
LD76E   JSR     >LCBEC
        BNE     LD71D
LD773   RTS


;
; Kill command dispatch routine
;
; Syntax :
;	KILL filespec
;

CmdKill   
	JSR     >DosGetFilenameAndOpen
        BNE     LD77E
        JSR     >DOSDeleteFile
	
LD77C	BEQ	LD720
	
LD77E   JMP     >DosHookSysError

;
; Protect command dispatch routine.
;
; Syntax :
;	PROTECT ON filespec
;	PROTECT OFF filespec
;

CmdProtect   
	TSTA
        BPL     LD78E
        CMPA    #$C2
        BEQ     LD7A0
        CMPA    #$88
        LBNE    BasSNError
LD78E   BSR     LD798
        LDB     #$01
LD792   JSR     >DOSProtect

	BRA	LD77C
	NOP
	
LD798   JSR     <BasChrGet
        JSR     >DosGetFilenameAndOpen
        
	BNE     LD7C4
        RTS

LD7A0   BSR     LD798
        CLRB
        BRA     LD792

;
; Rename command dispatch routine
;
; Syntax :
;	RENAME filespec TO filespec
;

CmdRename   
	JSR     >DosGetFilenameAndOpen
        BNE     LD7C4
        PSHS    A
        LDB     #$BC
        JSR     >$89AC
        JSR     >DosGetFilenameAndOpen
        BEQ     LD7C2
        CMPB    #$A0
        BNE     LD7C4
        PULS    A
        JSR     >DOSRename
        
	BRA	LD77C
	NOP
		
LD7C2   LDB     #DDErrFE
LD7C4   JMP     >DosHookSysError

;
; FLread command dispatch routine
;
; Syntax :
;	FLREAD filespec;string
;	FLREAD filespec, FROM first_byte,FOR count;string
;

CmdFLRead   
	BSR     LD7D6
        LDA     #$FF
        STA     DosFlFreadFlag
        JSR     >LD969
        BSR     LD803
        JMP     >BasLineInputEntry

LD7D6   JSR     >LD6C8
        BNE     LD82D
        JSR     >LD830
        JSR     >DosFCBNoToAddr
        TST     $067E
        BEQ     LD7F0
        LDU     $0664
        LDB     $0666
        STU     12,X
        STB     14,X
LD7F0   JSR     >LD9DC
        LDX     #$02DC
        CLR     ,X
        JSR     <BasChrGet
        RTS
;
; Fread command dispatch routine
;
; Syntax :
;	FREAD filespec;variable list
;	FREAD filespec,FROM startpos,FOR numbytes;variable list	
;

CmdFRead   
	BSR     LD7D6
        CLR     DosFlFreadFlag
        JSR     >CmdReadFromX
LD803   JSR     >DosFCBNoToAddr
        LDB     $0604
        BEQ     LD819
        CLR     DosErrorCode
        LDD     FCBFilePointer+1,X	; Get filepointer LSW
        SUBD    DosErrorCode
        STD     FCBFilePointer+1,X	; Resave filepointer LSW
        BCC     LD819
        DEC     FCBFilePointer,X	; Decrement filepointer MSB
LD819   TST     <DosRecLenFlag
        BEQ     LD82A
        LDB     <DosNoBytesMove
        BEQ     LD82A
        CLRA
        ADDD    FCBFilePointer+1,X
        STD     FCBFilePointer+1,X
        BCC     LD82A
        INC     FCBFilePointer,X
LD82A   CLR     <TextDevN		; Reset to basic console input
LD82C   RTS

LD82D   JMP     >DosHookSysError

LD830   JSR     >BasChkDirect
        LDA     <DosCurrCtrlBlk
        JSR     DOSGetFlen
        BNE     LD82D
	
        STU     $0664
        STA     $0666
        CLR     <DosRecLenFlag
        CLR     $067E
        LDA     #$01
        STA     <TextDevN
LD849   JSR     <BasChrGetCurr
        CMPA    #$3B
        BEQ     LD82C
        JSR     >VarCKComma
        CMPA    #$E5
        BNE     LD86A
        JSR     <BasChrGet
        JSR     >VarGetStr
        BSR     LD884
        STU     $0664
        STA     $0666
        LDA     #$FF
        STA     $067E
        BRA     LD849

LD86A   CMPA    #$80
        BNE     LD87B
        JSR     <BasChrGet
        JSR     >Get8BitorError
        STB     <DosNoBytesMove
        LDB     #$FF
        STB     <DosRecLenFlag
        BRA     LD849

LD87B   JMP     >BasSNError

LD87E   JMP     >BasFCError

LD881   JMP     >BasTMError

LD884   TST     <BasVarFPAcc1+5
        BMI     LD87E
        TST     <BasVarType
        BNE     LD881
        LDA     #$A0
        SUBA    <BasVarFPAcc1
        BEQ     LD89D
LD892   LSR     <BasVarFPAcc1+1
        ROR     <BasVarFPAcc1+2
        ROR     <BasVarAssign16
        ROR     <BasVarFPAcc1+4
        DECA
        BNE     LD892
LD89D   LDU     <BasVarFPAcc1+2
        LDA     <BasVarFPAcc1+4
        RTS

LD8A2   JMP     >DosHookSysError

;
; FWrite command dispatch routine.
;
; Syntax :
;	FWRITE filespec;variable list
;	FWRITE filespec,FROM startpos,FOR numbytes;variable list	
;

CmdFWrite   
	JSR     >LD6C8
        BEQ     LD8B3
        CMPB    #$A0
        BNE     LD8A2
        JSR     >DOSCreateFile
        BNE     LD8A2
LD8B3   JSR     >LD830
        JSR     >FindFreeDiskBuffer
        BNE     LD8A2
        STX     $060B
        LDA     #$55
        STA     2,X
        CLR     <DosBytesInDTA
        JSR     <BasChrGet
        JSR     >CmdPrint
        TST     <DosRecLenFlag
        BEQ     LD8D8
        LDB     <DosNoBytesMove
        BEQ     LD8D8
        LDA     #$20
LD8D3   BSR     LD91D
        DECB
        BNE     LD8D3
LD8D8   TST     <DosBytesInDTA
        BEQ     LD8F4
        LDX     $060B
        LDX     5,X
        CLRA
        LDB     <DosBytesInDTA
        TFR     D,U
        LDA     <DosCurrCtrlBlk
        LDY     $0664
        LDB     $0666
        JSR     >DOSFWrite
        BNE     LD8A2
LD8F4   LDX     $060B
        CLR     2,X
LD8F9   RTS

DosHookCheckIONum   
	BLE     LD8F9			; -ve?, tape, printer etc
        CMPB    #$04			; Devno >4
        BHI     LD914			; yep DN error
        PULS    X,PC			; return to callers, caller.....

DosHookOpenDev   
	LEAS    2,S			; drop return address
        JSR     >VarGetStr		
        JSR     >BasGetStrFirst
        PSHS    B
        JSR     >BasGetDevNo		; get device no
        TSTB				; valid?
        LBLE    CmdOpenEntry		; Yep, go open it, otherwise error
	
LD914   JMP     >$B851			; ?DNError

DosHookCharOut   
	TST     <TextDevN
        BLE     LD8F9
        LEAS    2,S
LD91D   PSHS    D,X,Y,U
        LDB     <DosRecLenFlag
        BEQ     LD927
        LDB     <DosNoBytesMove
        BEQ     LD95E
LD927   LDX     $060B
        LDX     5,X
        LDB     <DosBytesInDTA
        ABX
        STA     ,X
        DEC     <DosNoBytesMove
        INC     <DosBytesInDTA
        BNE     LD95E
        LDA     <DosCurrCtrlBlk
        LDX     $060B
        LDX     5,X
        LDU     #$0100
        LDY     $0664
        LDB     $0666
        JSR     >DOSFWrite
        BEQ     LD950
        JMP     >DosHookSysError

LD950   LDD     $0665
        ADDD    #$0100
        BCC     LD95B
        INC     $0664
LD95B   STD     $0665
LD95E   PULS    D,X,Y,U,PC

DosHookDiskItem   
	TST     <TextDevN
        BLE     LD8F9
        LDX     #$879A
        STX     ,S
LD969   LDA     <DosRecLenFlag
        BEQ     LD971
        LDA     <DosNoBytesMove
        BEQ     LD9BE
LD971   LDX     #$02DD
        LDB     <DosBytesInDTA
        STB     DosErrorCode
        ABX
        LDB     $0604
        STB     DosRunLoadFlag
        CLRB
        STB     -1,X
        STB     <BasBreakFlag
LD985   DEC     <DosNoBytesMove
        DEC     $0604
        INC     <DosBytesInDTA
        LDA     ,X+
        BEQ     LD9BE
        CMPA    #$0D
        BEQ     LD9BE
        TST     DosFlFreadFlag
        BNE     LD9A1
        CMPA    #$2C
        BEQ     LD9BE
        CMPA    #$3A
        BEQ     LD9BE
LD9A1   LDA     <DosRecLenFlag
        BEQ     LD9A9
        LDA     <DosNoBytesMove
        BEQ     LD9BE
LD9A9   INCB
        CMPB    #$FF
        BEQ     LD9BC
        LDA     $0604
        BNE     LD985
        LDB     <BasBreakFlag
        BNE     LD9BC
        BSR     LD9C8
        CLRB
        BRA     LD985

LD9BC   LEAX    1,X
LD9BE   LDB     DosErrorCode
        CLR     -1,X
        LDX     #$02DC
        ABX
        RTS

LD9C8   JSR     >DosFCBNoToAddr
        CLRA
        LDB     DosRunLoadFlag
        NOP
        PSHS    D
        LDD     13,X
        SUBD    ,S++
        STD     13,X
        BCC     LD9DC
        DEC     12,X
LD9DC   PSHS    Y,U
        LDU     #$00FF
        BSR     LDA01
        LDU     #$02DD
        CLR     D,U
        LDA     <DosCurrCtrlBlk
        LDB     14,X
        LDX     12,X
        EXG     X,U
        JSR     >DOSFRead
        BNE     LD9FE
        LDX     #$02DD
        CLR     -1,X
        PULS    Y,U,PC

DosErrorPE   
	LDB     #DDErrPE
LD9FE   JMP     >DosHookSysError

LDA01   PSHS    U
        LDD     13,X
        ADDD    ,S
        PSHS    D
        LDA     12,X
        ADCA    #$00
        SUBA    $10,X
        PULS    D
        BCS     LDA26
        BHI     DosErrorPE
        SUBD    $11,X
        BLS     LDA26
        LDD     $11,X
        SUBD    13,X
        BEQ     DosErrorPE
        STD     ,S
        COM     <BasBreakFlag
LDA26   LDD     ,S
        STB     $0604
        STB     DosRunLoadFlag
        CLR     DosErrorCode
        CLR     <DosBytesInDTA
        PULS    Y,PC

;
; Dir command dispatch routine
;
; Syntax :
;	DIR drivenum	
;

CmdDir  
	BEQ     LDA3C			; No drive specified, use default
        JSR     >GetDriveNoInB		; Else get from command 
        BRA     LDA3F

LDA3C   LDB     DosDefDriveNo		; Get default drive

LDA3F   STB     <LastActiveDrv		; Flag as last drive used
        CLR     ,-S			; make temp on stack (file number on disk counter)
	
	ldx	#DosD0Online-1		; point at online flags
	clr	b,x			; mark drive offline, forces directory re-read.
	
	JSR     >TextCls		; clear screen	
        CLRB
	
LDA44   JSR     >BasPollKeyboard	; poll keyboard
        JSR     >DOSGetDirEntry		; go get next entry
        LBNE    DosHookSysError		; error : exit
	
        LDA     ,X			; Get Attribute byte
        BITA    #AttrEndOfDir		; Check for end of directory $08
        BNE     CmdDirDoneAll		; Yes : stop processing
	
        BITA    #AttrDeleted+AttrIsCont ; Is entry a deleted file, or continuation entry ? $81				; and another thing
        BNE     CmdDirDoNextEntry	; yes : do next
	
        LEAX    1,X			; Point at filename
        LDB     #$08			; Up to 8 chars
        BSR     PrintBCharsFromX	; Print it
        LDA     #$2E			; '.'
	JSR     >TextOutChar		; output period
        LDB     #$03			; print extension
        BSR     PrintBCharsFromX
	
        LDA     -12,X			; Point at attributes
        BITA    #AttrWriteProt		; Is this a protected file ?
        BEQ     LDA70			; no skip on
        LDA     #'p			; Flag protected $70
        FCB	Skip2
LDA70	LDA	#$20			; space
        JSR     >TextOutChar		; output attribute byte
        JSR     >TextOutSpace		; And a space
        LDU     #$FFFF
        LDX     #DosFCB0Addr		; Point at first FCB
        LDB     ,S			; Get file number
        STB     FCBDiskFileNo,X		; Save in FCB

        JSR     >FindFSNinU		; find last File sector
        LBNE    DosHookSysError		; error : exit

        JSR     >LCE99			; add last bytes to length
        BSR     LDAB7			; Print filesize

	JSR	TextOutCRLF		; Output EOL
	
        LDD     <TextVDUCursAddr	; Check for near end of screen
        CMPD    #$05A0				
        BLS     CmdDirDoNextEntry	; not near end, skip on
        
	PSHS    D,X,Y,U			; save regs
        JSR	>CON_PromptMore		; Prompt for more
	JSR     >TextCls		; clear screen
        PULS    D,X,Y,U			; Restore regs
	
CmdDirDoNextEntry 
	INC     ,S			; do next
        LDB     ,S			; Get disk file number counter
        CMPB    #$B4			; More than max files on disk ?
        BCS     LDA44			; Less, loop again.	

;
; We come here either when we have processed $A0 entries, which is the maximum,
; or we have reached an entry with the AttrEndOfDir bit set which signals the end
; of the directory.
;

CmdDirDoneAll   
	PULS    A
        JSR     >DOSGetFree		; Get free bytes on drive
        CLRA
        TFR     X,U
        BSR     LDAB7			; Display free bytes
        LDX     #BytesFreeMess-1	; Point to message
        JMP     >TextOutString		; print it, and return

; Print B chars pointed to by X, if char is $00, then output a space.
; Used to print filenames.

PrintBCharsFromX   
	LDA     ,X+			; Fetch a char
        BNE     LDAB0			; Is it zero ? no : skip
        LDA     #$20			; Replace it with a space
LDAB0   JSR     >TextOutChar		; Output it
        DECB				; Decrement count
        BNE     PrintBCharsFromX	; any left yes : loop again
        RTS

LDAB7   
	JSR	>LDD95
	JMP	TextOutNumFPA0
	
DosHookReadInputBugfix
	PSHS	U,X,D
	LDX	#$837D
	CMPX	8,S
	BNE	LDACB
	JSR	DosCloseAllFiles
	BRA	CheckAndDoAuto2
LDACB	PULS	D,X,U,PC
	NOP
	
BytesFreeMess
        FCC     / FREE BYTES/
        FCB     $0D
        FCB     $0D
        FCB     $00

;
; Auto command dispatch routine
;
; Syntax :
;
;	AUTO startline,increment
;

CmdAuto 
	LDD     <BasCurrentLine		; Get current line number
        CMPD    #$FFFF			; Direct mode ?
        BEQ     LDAE7			; yep start numbering
        JMP     >DBasFCError		; nope : error

LDAE7   LDD     #AutoStartLine		; set default start line
        PSHS    D
        LDB     #AutoIncrement		; set default increment
        PSHS    D
        JSR     <BasChrGetCurr		; get current character
        BEQ     CmdAutoDoAuto		; nothing, used defaults		
	
        JSR     >VarGet16Bit		; get startline
        LDD     <BasVarAssign16
        STD     2,S			; update value on stack
        JSR     <BasChrGetCurr		; any more parameters ?
        BEQ     CmdAutoDoAuto		; nope use default increment
	
        JSR     >VarCKComma		; get comma, error if not
        JSR     >DVarGet16Bit		; get increment
        LDD     <BasVarAssign16
        BEQ     CmdAutoErrorExit			; increment is zero : error
        STD     ,S			; update increment value
        JSR     <BasChrGetCurr		; get current char
        BEQ     CmdAutoDoAuto		; no more : do it !
	
CmdAutoErrorExit   
	JMP     >BasSNError		; More chars left, SN? error

CmdAutoDoAuto   
	ORCC    #(FlagIRQ+FlagFIRQ)	; Disable interrupts	
        LDD     ,S++			; Get Increment off stack		
        STD     DosAutoInc
        LDD     ,S++			; Get start line off stack
        SUBD    DosAutoInc		; Subtrack increment
        STD     DosAutoCurrent		; Save in current
        LDA     #AutoFlag		; Flag in AUTO mode
        STA     DosAutoFlag
        RTS

CheckAndDoAuto   
	PSHS    D,X,U
CheckAndDoAuto2
        TST     DosAutoFlag		; is auto enabled ?
        BNE     LDB30			; yes : handle it
LDB2E   PULS    D,X,U,PC

LDB30   LDD     DosAutoCurrent		; get current line no
        ADDD    DosAutoInc		; add increment
        CMPD    #BasMaxLineNo		; greater than max line no ?
        BHI     LDB2E			; yep : exit
	
        STD     ,S			; save new line on stack
        JSR     >TextOutNum16		; output line no
        LDA     #$20			; output a space
        JSR     TextOutChar		
	
        LDU     #BasBuffer+3		; point at basic buffer
        LDD     ,S			; get current line no
        STD     DosAutoCurrent		; save it
        LDX     #BasLinInpBuff+1	; point to line input buffer
        LDB     #$00
LDB53   LDA     ,U+			; get byte from basic buffer
        BEQ     LDB5C			; zero ?
	
        STA     ,X+			; nope put in buffer
        INCB
        BRA     LDB53

LDB5C   LDA     #$20			; store space
        STA     ,X+
        INCB			
LDB61   JSR     TextWaitKeyCurs2	; Wait with cursor
        CMPA    #$0D			; return ?
        BEQ     LDB6C			
	
        CMPA    #$03			; break ?
        BNE     LDB82
	
LDB6C   CLR     DosAutoFlag		; clear auto flag
        LDA     #$0D			; output a return
        JSR     TextOutChar
	
        LEAS    8,S			; drop stack frame
        CLR     <CasLastSine
        LDX     #DBasLinInpBuff+1	; point at input buffer
        LDA     #$0D
        LDB     #$01
        JMP     >BasInBuffFromX		; input it

LDB82   CMPA    #$20			; space ?
        BCS     LDB61			; yep keep going

        CMPA    #$7B			; max ascii char ?
        BCC     LDB61			; yep keep going

        LEAS    8,S			; drop stack frame
        CLR     <CasLastSine
        JMP     >BasInBuffFromX		

DoBeep  CLRA				; A=0	
        LDX     #$0008			; Repeat 8 times
LDB95   JSR     >CasByteOut		; Output A to cassette
        LEAX    -1,X			; Decrement count
        BNE     LDB95			; Loop again if all not done
LDB9C   RTS

;
; Beep command dispatch routine
;
; Syntax :
;	BEEP nobeeps
;

CmdBeep   
	BEQ     LDBA3			; if no params, default to 1 beep
        JSR     >Get8BitorError		; get beep count 

	FCB	Skip2
LDBA3	LDB	#$01			; Default beep count

        PSHS    B			; save count on stack
        CLRB
        JSR     >SndDTOAOn		; switch sound to D to A
LDBAB   JSR     DoBeep
        JSR     >BasPollKeyboard	; check for key
        DEC     ,S			; decrement beep count
        BEQ     LDBBF			; done all : restore and exit
        LDY     #$6000			; wait a short while
LDBB9   LEAY    -1,Y
        BEQ     LDBAB			; loop back and do next beep
        BRA     LDBB9

LDBBF   PULS    B,PC

;
; Wait command dispatch routine.
;
; Syntax :
;	WAIT miliseconds
;

CmdWait   
	BEQ     LDB9C			; no params : exit
        JSR     >VarGet16Bit		; get no of ms to wait
        LDX     <BasVarAssign16
LDBC8   JSR     >BasPollKeyboard	; scan keyboard
        LDB     #$A0			; loop to delay
LDBCD   DECB
        BNE     LDBCD			; keep going
	
        LEAX    -1,X			; decrement ms counter
        BNE     LDBC8			; keep going if not zero
        RTS

;
; Swap command dispatch routine.
;
; Syntax :
;	SWAP var1,var2
;

CmdSwap   
	JSR     >VarGetVar		; Get var from basic
        LDX     <BasVarPtrLast		; get variable pointer and type
        LDA     <BasVarType
        PSHS    A,X			; save them
        JSR     >VarCKComma		; check for comma
        JSR     >VarGetVar		; get second var
        PULS    A,Y			; recover pointer and type of first
        CMPA    <BasVarType		; var types the same ?
        LBNE    BasTMError		; nope : error
	
        LDU     <BasVarPtrLast		; get pointer to second var
        LDX     #$0005			; 5 bytes for var discriptor
LDBF1   LDA     ,U			; swap a pair of bytes
        LDB     ,Y
        STB     ,U+			; increment pointers
        STA     ,Y+
        LEAX    -1,X			; decrement count
        BNE     LDBF1			; keep going if more bytes
        RTS

LDBFE   LDB     #DDErrBT		; flag boot error
LDC00   JMP     >DosHookSysError

;		
; Boot command dispatch routine (DragonDos only).
;
; Syntax :
;	BOOT drivenum	
;

CmdBoot   
	BEQ     LDC0A			; No drive supplied, use default
        JSR     >GetDriveNoInB		; Get drive number 
        BRA     LDC0D

LDC0A   LDB     DosDefDriveNo		; use default drive no
LDC0D   LDA     #BootFirstSector 	; Read boot sector	
        STA     <DskSectorNo
        STB     <LastActiveDrv		; Set drive number
        LBSR    DosDoRestore		; Restore to track 0
        BCS     LDC00			; Error : exit
        
	LBSR    DosDoReadSec2		; Read first 2 chars of boot sec
        BCS     LDC00			; error : exit
	
        LDD     #BootSignature		; Boot signature found ?
        CMPD    <BasVarFPAcc1
        BNE     LDBFE			; no : error
	
        LDD     #BootLoadAddr		; start at boot load address
        STD     <DiskBuffPtr		; Set disk buffer to point there
	
LDC2A   JSR     >DosDoReadSec		; Read a sector
        BCS     LDC00			; Error : exit
	
        INC     <DiskBuffPtr		; increment page of buffer pointer	
        INC     <DskSectorNo		; increment sector number
        LDA     <DskSectorNo		; get sector number
        CMPA    #BootLastSector		; Read the whole boot area ?
        BLS     LDC2A			; no : read next sector
        
	JMP     >BootEntryAddr		; jump to loaded boot code
		
;
;Drive command dispatch routine.
;
; Syntax :
;	DRIVE drivenum	
;

CmdDrive
	BEQ     LDC46			; If no parameters : error
        JSR     >GetDriveNoInB		; Get drive number	
        STB     DosDefDriveNo		; set it
        rts
;        JMP     <BasChrGet		; back to interpreter loop

LDC46   JMP     >BasSNError

;
; Error command dispatch routine.
;
; Syntax :
;	ERROR GOTO lineno
;

CmdError   

	ifne	Dragon
	CMPA    #DTokGO			; Check next token is GO
	else
	CMPA	#CTokGO
	endc

        BNE     LDC46			; Error if not
        
	JSR     <BasChrGet		; get next char from basic
        
	ifne	Dragon
	CMPA    #DTokTO			; check next token is TO
	else
	CMPA    #CTokTO
	endc
	
        BNE     LDC46			; Error if not TO
	
	JSR     <BasChrGet		; skip to token
        JSR     >BasGetLineNo		; get line number for error handler
        LDX     <BasTempLine		
        CMPX    #BasMaxLineNo		; if bigger than max line no : FCer
        LBHI    BasFCError		
	
        STX     DosErrDestLine		; save in error handler line 
        BEQ     LDC6C			; if Zero : clear error handler flag
        LDA     #ErrGotoEnabled		; flag goto enabled
        STA     DosErrGotoFlag
        RTS

LDC6C   CLR     DosErrGotoFlag		; Turn off error goto
        RTS

;
; Gets address of a string  supplied on command line into X
;

LDC70   JSR     >VarCKComma		; Check for a comma
        JSR     >VarGetVar		; get next variable
        JMP     >VarGetExpr		; and evaluate it

;
;Sread command dispatch routine.
;
; Syntax :
;	SREAD driveno,trackno,secno,part1$,part2$
;

CmdSread   
	LBSR    GetSreadWriteParams	; Get drive,track,secno
        BSR     LDC70			; Get address of first 128 bytes to read
        PSHS    X			; save on stack
        BSR     LDC70			; Get address of second 128 bytes to read
        
	PSHS    X			; save on stack
        LDB     #$FF
        STB     <DosIOInProgress	; Flag Dos IO in progress
        JSR     >FindFreeDiskBuffer	; Find a buffer to read sector into
	BNE     LDCB1			; Error : exit
        
        CLR     BuffFlag,X		; Clear buffer flag
        LDX     BuffAddr,X		; Get buffer address
        STX     <DiskBuffPtr		; Save as pointer to do read
        JSR     >DosDoReadSec		; Read the sector
	
        STB     DosErrorCode		; Save error code in temp storage
        LDU     <DiskBuffPtr		; Get pointer to read data
        LEAU    $0080,U			; Point to second half of sector
        PULS    X			; Get address of second string
        BSR     LDCB4			; Copy bytes to string
        
	LDU     <DiskBuffPtr		; Point at begining of disk buffer
        PULS    X			; Get address of first string
        BSR     LDCB4			; Copy bytes to string
        CLR     <DosIOInProgress	; Flag Dos IO not in progress
        LDB     $0603			; Retrieve error code from read
        BNE     LDCB1			; Error : go to error handler
        RTS				; return to caller

LDCB1   JMP     >DosHookSysError	; Jump to error hook

LDCB4   PSHS    X,U			; save regs		
        LDB     #$80			; Make room for 128 bytes
        JSR     >BasResStr		; resize the string
        LEAU    ,X
        PULS    X
        STB     ,X
        STU     2,X
        PULS    X
        JMP     >UtilCopyBXtoU		; copy the bytes, return to caller

;
; Swrite command dispatch routine.
;
; Syntax :
;	SWRITE driveno,side,sector,part1$,part2$
;

CmdSwrite   
	BSR     GetSreadWriteParams	; get parameters
        BSR     LDD14			; get first half of sector
        JSR     >VarGetExpr
        LDX     <BasVarAssign16		; get address of string
        PSHS    X
        BSR     LDD14			; get second half of sector
        JSR     >BasGetStrLenAddr	; get length & addr
        PSHS    B,X
	
        LDB     #IOInProgress		; flag that IO is in progress
        STB     <DosIOInProgress
        JSR     FindFreeDiskBuffer	; Go find a disk buffer to use
        BNE     LDCB1			; error : exit
	
        CLR     BuffFlag,X		; make buffer free
        LDX     BuffAddr,X		; get buffer address
        STX     <DiskBuffPtr		; use this for disk io
        CLRB
LDCEA   CLR     ,X+			; Clear buffer
        DECB
        BNE     LDCEA
	
        PULS    B,X			; get saved string pointers for second half of sec
        LDU     <DiskBuffPtr		; point to disk buffer
        LEAU    $0080,U			; start halfway through
        TSTB				; any bytes to transfer ?
        BEQ     LDCFD			; nope skip on
        JSR     UtilCopyBXtoU		; copy bytes

LDCFD   PULS    X			; recover pointer to fist half of sector
        JSR     >VarDelVar		; delete var
        LDU     <DiskBuffPtr		; recover disk buffer ptr
        TSTB				; any bytes to transfer ?
        BEQ     LDD0A			; nope skip
	
        JSR     >UtilCopyBXtoU		; copy first half of sector
	
LDD0A   JSR     DosDoWriteSec		; go write it
        LBCS    DosHookSysError		; error : exit
        CLR     <DosIOInProgress	; flag no io in progresss	
        RTS

LDD14   JSR     VarCKComma		; check for comma
        JMP     >VarGetStr		; get string

GetSreadWriteParams   
	BNE     LDD1F			; params, read them
        JMP     >BasSNError		; none : SN Error

;
; Get params for Sread/Swrite
;

LDD1F   JSR     GetDriveNoInB		; Drive number
        STB     <DosLastDrive
        JSR     >VarGetComma8		; Track number
        CMPB    #MaxTrack		; greater than track 80 ?
	BHI     LDD33			; Yes : error
	
	STB     <DskTrackNo		; Save track number
        JSR     >VarGetComma8		; Get sector number
        STB     <DskSectorNo		; save it
LDD32   RTS

LDD33   JMP     >BasFCError		; Error!

;
; Verify command dispatch routine.
;
; Syntax :
;	VERIFY ON
;	VERIFY OFF
;

CmdVerify   
	BEQ     LDD46			; end of command : error
	ifdef	Tandy
        CMPA    #DTokOFF		; is next token 'OFF'
	else
	CMPA    #DTokOFF		; is next token 'OFF'
	endc
        BNE     LDD3F			; no : check for 'ON'
        CLRB				; set verify off
        BRA     LDD48

LDD3F   CMPA    #DTokON			; is next token 'ON' ?
        BEQ     LDD46			; yes : skip
        JMP     >BasSNError		; return error

LDD46   LDB     #$FF			; set verify flag
LDD48   STB     DosVerifyFlag
        JMP     <BasChrGet		; go get char from basic

DosHookEOF   
	TST     <BasVarType		; check var type
        BEQ     LDD32			; if numeric exit
	
        CLR     <BasVarType		; set var type numeric !
        LEAS    2,S			; drop return addresss
        LDY     #DosExtDat
        JSR     LD6E2			; validate and open file
        BNE     LDD77			; Error : exit
	
        JSR     DOSGetFlen		; get file length
        BNE     LDD77			; Error : exit
	
	JSR     DosFCBNoToAddr		; get address of FCB
        CMPU    FCBFilePointer,X	; check to see if file pointer is same as length, so EOF
        BHI     LDD6F			; nope
        CMPA    FCBFilePointer+2,X	
        BLS     LDD71			
	
LDD6F   CLRA				; return false
	FCB	Skip2
LDD71	LDA	#$01			; return true
        LDU     <Misc16BitScratch
        BRA     ReturnFP

LDD77   JMP     >DosHookSysError	; Deal with errors

;
; LOC "filename" get file pointer
; 
FuncLoc
	JSR     LD6C8			; Validate and open file
        BNE     LDD77			; error : exit
        
	JSR     DosFCBNoToAddr		; get address of current FCB
        LDU     FCBFilePointer,X	; get filepointer from FCB
        LDA     FCBFilePointer+2,X
        BRA     ReturnFP		; return as float in fpa0

;
; Function LOF "filename" get length of file.
; bug: file is left open.
; 
FuncLof 
	JSR     LD6C8			; Validate and open file
        BNE     LDD77			; error : exit
        
	JSR     DOSGetFlen		; get file length
        BNE     LDD77			; error : exit

ReturnFP   
	CLR     <BasVarType		; numeric var type
	
	NOP
LDD95	CLRB
	STU     <BasVarFPAcc1+2
        
	STD	<BasVarFPAcc1+4
        CLR     <$63
        LDA     #$A0
        STD     <DBasVarFPAcc1
	
        JMP     >VarNormFPA0		; normalize FPA0

FuncFree   
	JSR     <BasChrGetCurr		; get current bas char
        BEQ     LDDAC			; none, use default drive no
        JSR     GetDriveNoInB		; else get drive from command line
        BRA     LDDAF			; skip on

LDDAC   LDB     DosDefDriveNo		; get default drive
LDDAF   STB     <LastActiveDrv		; set it as current
        JSR     DOSGetFree		; go get free space
        BNE     LDD77			; error : exit
        TFR     X,U			
        CLRA
        BRA     ReturnFP		; return as float in fpa0

FuncErl   
	LDD     DosErrLineNo		; get last error line
LDDBE   JMP     >VarAssign16Bit2	; return value to basic

FuncErr   
	CLRA				; msb=0
        LDB     DosErrLast		; get last error no
        BRA     LDDBE			; return it to basic

FuncHimem   
	LDD     <AddrFWareRamTop	; get hardware ramtop
        BRA     LDDBE			; return it to basic

FuncFres   
	JSR     >VarGarbageCollect	; collect garbage...defrag string space !
        LDD     <BasVarStrTop		; work out size of string space
        SUBD    <BasVarStringBase
        BRA     LDDBE			; return it to basic

;
; The actual core disk IO routine, accepts a function code $00..$07
; these are dispatched through this table
;

DosFunctionTable   
	FDB     DosFunctionRestore
        FDB     DosFunctionSeek
        FDB     DosFunctionReadSec
        FDB     DosFunctionWriteSec
        FDB     DosFunctionWriteSec2
        FDB     DosFunctionWriteTrack
        FDB     DosFunctionReadAddr
        FDB     DosFunctionReadSec2
	
;
; Data table for Format ?
;

SectorIDTable   
	FCB     $01,$0A		; Sector layout table for format ?
        FCB     $02,$0B
        FCB     $03,$0C
        FCB     $04,$0D
        FCB     $05,$0E
        FCB     $06,$0F
        FCB     $07,$10
        FCB     $08,$11
        FCB     $09,$12
	FCB     $00

DDDF7	FCB	$35,$4E
	FCB	$4E,$08
	FCB	$00,$00
	FCB	$03,$F6
	FCB	$FC,$1F
	FCB	$4E,$4E

DDE03	FCB	$07,$00
	FCB	$00,$03
	FCB	$F5,$FE
	FCB	$01,$F7
	FCB	$4E,$14
	FCB	$4E,$4E
	FCB	$0B,$00
	FCB	$00,$03
	FCB	$F5,$FB
	FCB	$00,$E5
	FCB	$F7,$17
	FCB	$4E,$4E
	FCB	$00,$4E
	FCB	$4E

;
; Data copied into low ram at init, interrupt vectors etc
;

        ifeq    1
DDE1E   FCB     (ENDINTVEC-NMIJMP)	; No bytes
        FDB     SecVecNMI		; address to copy
; interrupt vectors
NMIJMP  JMP     >NMISrv
IRQJMP  JMP     >IRQSrv
FIRQJMP JMP     >FIRQSrv
ENDINTVEC
        else
; The MMC version only uses IRQ.
DDE1E   FCB     (ENDINTVEC-IRQJMP)	; No bytes
        FDB     SecVecIRQ		; address to copy
IRQJMP  JMP     >IRQSrv
ENDINTVEC       
        endc
        
; Dos vars, step rates for drives
;
;        FCB     (EndStep-BeginStep)	; No bytes
;        FDB     DosD0StepRate		; address to copy
;
;BeginStep
;        FCB     SeepRateDefault
;        FCB     SeepRateDefault
;        FCB     SeepRateDefault
;        FCB     SeepRateDefault
;EndStep

        FCB     $00			; No bytes : terminate

RamHookTable   
	FDB     DosHookOpenDev		; open dev/file
DDE97   FDB     DosHookCheckIONum	; check io num
DDE99   FDB     DosHookRetDevParam	; return dev parameters
DDE9B   FDB     DosHookCharOut		; char output	
        FDB     DosHookRetDevParam	; char input
        FDB     DosHookRetDevParam	; check dev open for input
        FDB     DosHookRetDevParam	; check dev open for output
	FDB     DosCloseAllFiles	; close all devs and files
DDEA5   FDB     LD70B			; close single dev/file
DDEA7   FDB     DosHookRetDevParam	; first char of new statement
DDEA9   FDB     DosHookDiskItem		; Disk file item scanner
DDEAB   FDB     DosHookRetDevParam	; poll for break
	FDB	DosHookReadInputBugfix	; read line of input
	FDB	DosHookRetDevParam	; finish loading ASCII program
	FDB	DosHookEOF		; EOF function
	FDB	DosHookRetDevParam	; Eval expression
	FDB	DosHookRetDevParam	; User error trap
	FDB	DosHookSysError		; System error trap
	FDB	DosHookRun		; run statement
		        	
;
; Some mesagaes
;
MessInsertSource
        FCC     /INSERT SOURCE/
        FCB     $0D
        FCB     $00
MessInsertDest
        FCC     /INSERT DESTINATION/
        FCB     $0D
	FCB     $00
	
DMessPressAnyKey   
        FCC     /PRESS ANY KEY/
        FCB     $0D
        FCB     $00

DosExtBas   
	FCC     /BAS/
DosExtDat	
	FCC	/DAT/
DosExtBin	
	FCC	/BIN/
DosExtNone	
	FCC	/   /

DosErrorCodeTable
	FCC	/NR/		; $80 : not ready
	FCC	/SK/		; $82 : seek
	FCC	/WP/		; $84 : write protect
	FCC	/RT/		; $86 : record type
	FCC	/RF/		; $88 : record not found
	FCC	/CC/		; $8A : CRC
	FCC	/LD/		; $8C : Lost data
	FCC	/BT/		; $8E : boot
	FCC	/IV/		; $90 : invalid volume / directory
	FCC	/FD/		; $92 : Full directory
	FCC	/DF/		; $94 : Disk full
	FCC	/FS/		; $96 : File spec
	FCC	/PT/		; $98 : Protection
	FCC	/PE/		; $9A : (read) Past end 
	FCC	/FF/		; $9C : File not Found 
	FCC	/FE/		; $9E : File exists
	FCC	/NE/		; $A0 : (file does) Not Exist
	FCC	/TF/		; $A2 : Too many Files open
	FCC	/PR/		; $A4 : Parameter
	FCC	/??/		; $A6 : Undefined

DosSignonMess	
	FCC	/DRAGONDOS /
	FCC	/1.3MMC/
        FCB     $0D
	FCB	$00
	
LDFCA	LDA	$067D
	TST	[$0678]
	RTS
	
; Set PIAs, called with X=FF01 or X=FF21
; Called with EXG as that way we can return ans still leave things on the stack.

SetPIA  LDA     ,X
        PSHS    A
        STB     ,X++
        LDA     ,X
        PSHS    A
        STB     ,X
        LDX     #PIA1CRA
        TFR     U,PC

ResetPIA   
	LDX     #$FF25
ResetPIA2   
	PULS    A
        STA     ,--X
        PULS    A
        STA     ,--X
        LDX     #$FF05
        TFR     U,PC

LDFF3   TSTB
	BNE	LDFF9
	LDB	#$96
	RTS
LDFF9	LDA	DosDefDriveNo
	JMP	LC7C1
	FCB	$00
	
;DE000   END
