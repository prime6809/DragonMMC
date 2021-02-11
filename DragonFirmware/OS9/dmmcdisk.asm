********************************************************************
* DMMCDisk - DragonMMC disk driver
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   -      ????/??/??
*
* 2019-04-04, P.Harvey-Smith.
*       Ported by using ddisk as a skeleton and adding DragonMMC
*		functions from ROM source.
* 
* 2021-02-05, P.Harvey-Smith.
*		Removed CMD_NOP calls to AVR.
*

			nam   	DMMCDisk
			ttl   	DragonMMC disk driver

			use   	defsfile

tylg		set   	Drivr+Objct   
atrv    	set   	ReEnt+rev
rev     	set  	$00
edition 	set   	1

MaxDrv   	set   	4

			mod   	eom,name,tylg,atrv,start,size
		
			org		DRVBEG
DrvTab		RMB   	MaxDrv*DrvMem	; Drive tables, 1 per drive 
CDrvTab	  	rmb   	2		; Pointer to current drive table entry above
DrivSel   	rmb   	1		; Saved drive mask
Settle	 	rmb	1		; Head settle time
SavePIA0CRB	rmb 	1		; Saved copy of PIA0 control reg b
SaveACIACmd	rmb	1		; Saved copy of ACIA command reg
BuffPtr	 	rmb	2		; Buffer pointer
SideSel	 	rmb	1		; Side select.
Density		rmb	1		; Density 0=double, %00001000=single D64, %00100000=single Alpha

DskError	rmb	1		; hardware disk error	

size     	equ  	.

		fcb   	$FF 
name    equ   	*
        fcs   	/MDisk/
        fcb   	edition

start   lbra  	Init			; Initialise Driver
        lbra  	Read			; Read sector
        lbra  	Write			; Write sector
        lbra 	GetStat			; Get status
        lbra  	SetStat			; Set status
        lbra  	Term			; Terminate device

IRQPkt   fcb   	$00 			; Normal bits (flip byte)
         fcb   	$01			; Bit 1 is interrupt request flag (Mask byte)
         fcb   	10			; Priority byte

*
* Init
*
* Entry:
*    Y  = address of device descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
Init   	clra
	sta	>D.DskTmr		; Zero motor timer
 
        ldb   	#MaxDrv
        leax  	DrvBeg,u
	 
InitDriveData    
	sta   	DD.Tot,x		; Set total sectors = $FF
        sta   	<V.Trak,x		; Set current track = 0
        leax  	<DrvMem,x		; Skip to next drive
        decb  
        bne   	InitDriveData
	 
        ldd   	#$0100			; Request a page of system ram
        pshs  	u			; used to verify writes
        os9   	F$SRqMem 
        tfr   	u,x
        puls  	u
        bcs   	Return

        stx   	>BuffPtr,u		; Save verify page pointer

        clrb  
Return  rts   


* GetStat
*
* Entry:
*    A  = function code
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
GetStat

* Term
*
* Entry:
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
Term    clrb  
        rts   

* Read
*
* Entry:
*    B  = MSB of the disk's LSN
*    X  = LSB of the disk's LSN
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
Read   
        cmpx  	#$0000			; LSN ?
        bne   	ReadData		; No : Just do read,
        bsr   	ReadData		; Yes : do read and copy disk params
        bcs   	ReadDataExit

        ldx   	PD.Buf,y
        pshs  	y,x
        ldy   	>CDrvTab,u
        ldb   	#DD.Siz-1 
L0082   lda   	b,x
        sta   	b,y
        decb  
        bpl   	L0082
        clrb  
        puls  	pc,y,x

ReadDataExit    
	rts   

ReadData	
	pshs  	y,x,b,a
	bsr	DoReadData
	puls	y,x,b,a,pc

DoReadData    
	lbsr	SelectDrive   		; select drive
	bcs   	ReadDataExit		; error selecting bomb
		
	lbsr	MMC_SendLSN		; send LSN to mmc

	ldx   	PD.Buf,y		; Target address for data

	lda	#CMD_READ_IMG_SEC	; Read sector from AVR
	lbsr	MMC_SendCmd		; send command
	bmi	DecodeReadWriteError	; if error handle it
		
	lda	#CMD_INIT_READ		; read the sector
	lbsr	MMC_SendCmdRaw		; send command
		
	pshs	x
		
	clrb				; init count

MMC_ReadDOSSecLoop		
	lbsr	MMC_WaitGetWritten	; wait for byte
	sta	,x+			; save in buffer
	incb				; inc count
	bne	MMC_ReadDOSSecLoop	; not done, loop again
		
	puls	x
		
	clrb				; flag no error

MMC_ReadDOSSecExit
	rts

;
; Convert DragonMMC / FATFS errors to OS9.
;

DecodeReadWriteError	
	anda	#$3F			; extract error code
	cmpa	#ERROR_INVALID_DRIVE	; invalid drive
	lbeq	RetErrorBadUnit		; Bad unit
	
	cmpa	#FR_WRITE_PROTECTED	; Write protected
	beq	RetErrorWP

	cmpa	#ERROR_READ_ONLY	; Read only
	beq	RetErrorWP

	bra	RetErrorNotReady	; Else not ready.
;		 
; Return error code
;

RetErrorNotReady    
	comb  
        ldb   	#E$NotRdy
        rts   
RetErrorWP    
	comb  
        ldb   	#E$WP
        rts   
RetWriteError    	
	comb  
        ldb   	#E$Write
        rts   
RetErrorSeek    
	comb  
        ldb   	#E$Seek
        rts   
RetErrorCRC    
	comb  
        ldb   	#E$CRC
         rts   
RetReadError
	comb  
        ldb   	#E$Read
        rts   

* Write
*
* Entry:
*    B  = MSB of the disk's LSN
*    X  = LSB of the disk's LSN
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
Write   
L0124   pshs  	x,b,a
        bsr   	DoWrite			; Attempt to do write
        puls  	x,b,a

        bcs   	RetWriteError		; return error

        tst   	<PD.Vfy,y		; Written, should we verify ?
        bne   	WriteDone		; no : return
        lbsr  	WriteVerify		; yes : verify write
        bcs   	RetWriteError		; return error
		
WriteDone    
	clrb  				; Return status ok
WriteDataExit        
	rts   
 
DoWrite 
	lbsr	SelectDrive  		; select drive
	bcs   	WriteDataExit		; error selecting bomb
		
	lbsr	MMC_SendLSN		; send LSN to mmc

        ldx   	PD.Buf,y		; Get data buffer in X

	lda	#CMD_INIT_WRITE		; Prepare to send the data
	lbsr	MMC_SendCmd		; send command

	clrb				; 256 bytes

DoWriteLoop
	lda	,x+			; get a byte

	lbsr	MMC_WaitPutRead		; send byte, wait for AVR to read it
	decb				; decrement count
	bne	DoWriteLoop		; keep going till done
	lda	#CMD_WRITE_IMG_SEC	; write the sector
	lbsr	MMC_SendCmd

	bmi	DecodeReadWriteError	; error : decode it
	rts

	 
; Verify a written sector.
WriteVerify    
	pshs  	x,b,a				
        ldx   	PD.Buf,y		; Swap buffer pointers
        pshs  	x
        ldx   	>BuffPtr,u	
        stx   	PD.Buf,y
        ldx   	4,s
        lbsr  	DoReadData		; Read data back in
        puls  	x
        stx   	PD.Buf,y		; Swab buffer pointers back
        bcs   	VerifyEnd
        lda   	#$20
        pshs  	u,y,a
        ldy   	>BuffPtr,u
        tfr   	x,u

VerifyLoop    
	ldx   	,u			; Compare every 4th word
        cmpx  	,y
        bne   	VerifyErrorEnd
        leau  	8,u
        leay  	8,y			; Increment pointers
        dec   	,s
        bne   	VerifyLoop
        bra   	VerifyEndOk		; Verify succeeded.

VerifyErrorEnd    
	orcc  	#Carry			; Flag error

VerifyEndOk    
	puls  	u,y,a

VerifyEnd    
	puls  	pc,x,b,a

;
; Select drive: Select the correct drive
;	On return 
;		if cc.c set, error code in b.
;		if cc.c clear then no error, and b unchanged.
;		selected drive returned in a
; 

SelectDrive    
	lda   	<PD.Drv,y		; Check it's a valid drive
        cmpa  	#MaxDrv
        bcs   	SelectDriveValid	; yes : continue
 
RetErrorBadUnit
        comb  				; Return bad unit error
        ldb   	#E$Unit
        rts   

SelectDriveValid    
	pshs  	x,d			; Unit valid so slect it
        sta   	>DrivSel,u		; Save selected drive
        leax  	DrvBeg,u		; Calculate offset into table
        ldb   	#DrvMem
        mul   
        leax  	d,x
        stx   	>CDrvTab,u		; Save selected drive
      
	puls	x,d
	andcc	#^CARRY			; clear carry, no error

SelectDriveEnd    
	rts				; restore and return


* SetStat
*
* Entry:
*    A  = function code
*    Y  = address of path descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
SetStat 
	ldx   	PD.Rgs,y		; Retrieve request
        ldb   	R$B,x
 
        cmpb  	#SS.Reset		; Restore to track 0.
	beq	DoReset			; Reset disk
;        beq   	SetStatNoErr		; no error : ignore
        
	cmpb  	#SS.Wtrk		; Write (format) a track
        beq   	DoWriteTrack		; not yet implemented.
        
	comb  
        ldb   	#E$UnkSvc

SetStatEnd    
	rts   

SetStatNoErr
	clrb
	rts

;
; Seek to track 0, because DragonMMC disk files don't have any heads to seek
; We set the LSN for the drive to 0, this has the same effect.
;
DoReset
        lda   	<PD.Drv,y		; Get drive from pd
	cmpa  	#MaxDrv			; drive valid?
	bhi	RetUnitError		; nope: unit error

	clrb				; LSN0
	ldx	#$0000
	lbsr	MMC_SendLSN		; send LSN to mmc

	clrb
	rts

;
; Write (format) a track
;
; Since the CMD_CREATE_IMG command formats the whole image, we only actually
; do anyting if formatting track 0, otherwise we just return.
;
; CMD_CREATE_IMG needs to be passed :
; ofs	size
; 0	1	driveid
; 1	2	total cylinders to format
; 3	1	head count
; 4	1	sectors / track
;
DoWriteTrack  
	lda	R$U+1,x			; get track
	bne	SetStatNoErr		; only format if track=0
	
	lda	R$Y+1,x			; get side and denity
	anda	#$01			; mask out side
	bne	SetStatNoErr		; only format if head=0
	
        lda   	<PD.Drv,y		; Get drive from pd
	cmpa  	#MaxDrv			; drive valid?
	bhi	RetUnitError		; nope: unit error
	
	bsr	MMC_InitSendBBytes	; start sending bytes
	bsr	MMC_WaitPutRead		; send drive id
	
	lda	<PD.Cyl+1,y		; Get cylinders from path low byte first
	bsr	MMC_WaitPutRead		; send low byte
	lda	<PD.Cyl,y		
	bsr	MMC_WaitPutRead		; send high byte
	
	lda	<PD.Sid,y		; Get sides from path 
	bsr	MMC_WaitPutRead		; send it

; Note OS-9 uses a word for sectors per track DragonMMC firmware only uses a byte.
; This should work for floppy disks but may need modifying for a device with > 255
; sectors per track
	
	lda	<PD.Sct+1,y		; Get sectors/track from path 
	bsr	MMC_WaitPutRead		; send it
	
	lda	#CMD_CREATE_IMG		; create image command
	bsr	MMC_SendCmd		; go do it
	
	clrb
	rts

RetUnitError
	comb  
        ldb   	#E$Unit
        rts   

;
; Send Command
;
; Entry a = command to send
;
; exit a = status code.
;

MMC_SendCmd
	bsr	MMC_SendCmdRaw		; send the command

	lda	#STATUS_FLAG_WRITTEN	; Written flag
	anda	D_STATUS_REG		; is it set ?
	beq	MMC_SendCmdNoResult	; no clear a and return
	lda	D_CMD_REG		; Get status (if any)
	tsta
	rts				; return
	
MMC_SendCmdNoResult
	clra				; flag no error
	rts
	
;
; Send a command but don't read a result.
;

MMC_SendCmdRaw
	sta	D_CMD_REG		; send the command
	bsr	MMC_WaitRead		; Wait for AVR to read it
	bsr	MMC_WaitNotBusy		; wait command completion
	rts
	
;
; Init buffer write
;
MMC_InitSendBBytes
	pshs	a
	lda	#CMD_INIT_WRITE		; Write bytes
	bsr	MMC_SendCmd		; send the command
	puls	a,pc
	
;
; MMC_WaitNotBusy, waits for the AVR busy flag to be reset.
;
MMC_WaitNotBusy
        
MMC_WaitNotBusyLoop        
	lda	D_STATUS_REG		; Busy flag
	anda	#STATUS_FLAG_BUSY	; is it set ?
	bne	MMC_WaitNotBusyLoop	; yes : keep waiting
	rts
;
; MMC_WaitPutRead Send a byte in a to the AVR DATA_REG and wait for it to read it
;

MMC_WaitPutRead 
	sta	D_WRITE_DATA_REG	; send it, and fall through.....
;
; MMC_WaitRead : waits for the AVR to read byte
;
MMC_WaitRead

MMC_WaitReadLoop
	lda	D_STATUS_REG
	anda	#STATUS_FLAG_READ	; Has it been read yet ?
	bne	MMC_WaitReadLoop	; no : keep waiting
	rts

;
; MMC_WaitWritten, waits for the AVR to write a byte
;
MMC_WaitWritten
        
MMC_WaitWrittenLoop
	lda	D_STATUS_REG
	anda	#STATUS_FLAG_WRITTEN	; Written flag is it set ?
	beq	MMC_WaitWrittenLoop	; no : keep waiting
	rts

;
; MMC_WaitGetWritten, wait for the AVR to write a byte, return it in a
;
MMC_WaitGetWritten
	bsr	MMC_WaitWritten		; Wait for byte
	lda	D_READ_DATA_REG		; get byte
	rts

;
; Send LSN to AVR :
; a = drive id
; b = LSN 23..16
; x = LSN 15..0
;
MMC_SendLSN
	pshs	x,d
	lbsr	MMC_InitSendBBytes	; Begin sending bytes
	
	lbsr	MMC_WaitPutRead		; send drive id 
	
	exg	x,d			; get LSW of LBA
	exg	a,b			; move lsb into a
	
	lbsr	MMC_WaitPutRead		; send LSB of LSN
	
	exg	a,b			; get next byte of LBA
	
	lbsr	MMC_WaitPutRead		; send it
	
	exg	d,x			; retrieve MSB
	
	exg	a,b			; move lsb into a
	lbsr	MMC_WaitPutRead		; send it

	clra				; msb of LSN always 0
	lbsr	MMC_WaitPutRead		; send it

	lda	#CMD_LOAD_LBA		; Tell AVR
	lbsr	MMC_SendCmd		; send command
	
	puls	x,d,pc			; restore / return
        
;
; MMC_ReadDOSSec,  Read an emulated dos sector
;
; Entry :
;	X	= buffer
;
; Before calling :
; Drive and LSN should have been set with a call to MMC_SendLSN
;
;

MMC_ReadDOSSec


        emod
eom     equ   *
        end
