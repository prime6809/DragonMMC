;
; MMC functions.
;
; 2011-06-13 Phill Harvey-Smith.
;
; Define MINIMUM to exclude code not needed by FLASH
;

;
; Note some routines make calls / returns into the Dragon and CoCo roms.
; Please remember to define them so they assemble correctly on *BOTH* 
; machines.
; 

;
; **** NOTE ****
;
; Send CMD_INIT_READ with MMC_sendCmdRaw *NOT* MMC_SendCmd
;

__mmc_func

;
; Send Command
;
; Entry a = command to send
;
; exit a = status code.
;

MMC_SendCmd
		bsr		MMC_SendCmdRaw			; send the command

		lda		#STATUS_FLAG_WRITTEN	; Written flag
		anda	D_STATUS_REG			; is it set ?
		beq		MMC_SendCmdNoResult		; no clear a and return
		lda		D_CMD_REG				; Get status (if any)
		tsta
		rts								; return
		
MMC_SendCmdNoResult
		clra							; flag no error
		rts
		
;
; Send a command but don't read a result.
;

MMC_SendCmdRaw
		sta		D_CMD_REG				; send the command
		bsr		MMC_WaitRead			; Wait for AVR to read it
		bsr		MMC_WaitNotBusy			; wait command completion
		rts
		
;
; Init buffer write
;
MMC_InitSendBBytes
		pshs	a
		lda		#CMD_INIT_WRITE			; Write bytes
		bsr		MMC_SendCmd				; send the command
		puls	a,pc

;
; Init write buffer and send B bytes from X to MMC 
;

MMC_StartSendBBytes
		bsr		MMC_InitSendBBytes
;
; Send B bytes pointed to by X to the WRITE_DATA_REG
;
MMC_SendBBytes
		pshs	d,x
        tstb                            ; Is b zero ?
        beq     MMC_SendBBytesExit      ; yep : exit
		
MMC_SendBBytes2
        lda		,x+						; get a byte

		bsr		MMC_WaitPutRead			; send byte, wait for AVR to read it

		decb							; decrement count
		bne		MMC_SendBBytes2			; more : loop again

MMC_SendBBytesExit
		puls	d,x,pc					; restore and return
	
;
; Send U bytes pointed to by X to the WRITE_DATA_REG
; 	
        
MMC_SendUBytes
		pshs	u,d,x
        
MMC_SendUBytesLoop        
        cmpu    #$0000                  ; U already zero?
        beq     MMC_SendUBytesExit      ; yep : exit
        
		lda		,x+						; get a byte

		bsr		MMC_WaitPutRead			; send byte, wait for AVR to read it

		leau    -1,u					; decrement count
		bra		MMC_SendUBytesLoop		; more : loop again

MMC_SendUBytesExit
        puls    u,d,x,pc

;
; Init buffer and write B bytes from X, and terminate with 0 byte 
;	
; MMC_Sendname2 differs from MMC_Sendname, in that it assumes that a write 
; bytes command has already been written, for example if parameters have been 
; written before the filename
;
MMC_SendBBytesZero
MMC_SendName
		bsr		MMC_InitSendBBytes		; start sending bytes

MMC_SendName2
		pshs	a
		bsr		MMC_SendBBytes			; Send the bytes
		
		lda		#0						; Send terminator
		bsr		MMC_WaitPutRead			; send byte, wait for AVR to read it
		puls	a,pc					; restore and return
        
;
; Init buffer and write bytes from X, till a zero terminator reached.
; does *NOT* send the zero terminator
;

MMC_SendTillZero
		bsr		MMC_InitSendBBytes		; start sending bytes

MMC_SendTillZeroLoop
        lda     ,x+                     ; get a byte
        tsta                            ; Zero?
        beq     MMC_SendTillZeroExit    ; yep : stop sending
        
        bsr     MMC_WaitPutRead			; send byte, wait for AVR to read it
        bra     MMC_SendTillZeroLoop

MMC_SendTillZeroExit
        rts
        
;
; MMC_WaitNotBusy, waits for the AVR busy flag to be reset.
;
MMC_WaitNotBusy
        ifndef  EMULATE
        
MMC_WaitNotBusyLoop        
		lda		D_STATUS_REG			; Busy flag
		anda	#STATUS_FLAG_BUSY		; is it set ?
		bne		MMC_WaitNotBusyLoop		; yes : keep waiting
        endc
		rts

;
; MMC_WaitPutLatchRead Send a byte in a to the AVR LATCH_REG and wait for it to read it
;
MMC_WaitPutLatchRead
		sta		D_LATCH_REG				; write the data
		bra		MMC_WaitRead			; and wait for it to be read
;
; MMC_WaitPutRead Send a byte in a to the AVR DATA_REG and wait for it to read it
;

MMC_WaitPutRead 
		sta		D_WRITE_DATA_REG		; send it, and fall through.....

;
; MMC_WaitRead : waits for the AVR to read byte
;
MMC_WaitRead
        ifndef  EMULATE

MMC_WaitReadLoop
		lda		D_STATUS_REG
		anda	#STATUS_FLAG_READ		; Has it been read yet ?
		bne		MMC_WaitReadLoop		; no : keep waiting
        endc
		rts

;
; MMC_WaitWritten, waits for the AVR to write a byte
;
MMC_WaitWritten
        ifndef  EMULATE
        
MMC_WaitWrittenLoop
		lda		D_STATUS_REG
		anda	#STATUS_FLAG_WRITTEN	; Written flag is it set ?
		beq		MMC_WaitWrittenLoop			; no : keep waiting
        endc
		rts

;
; MMC_WaitGetWritten, wait for the AVR to write a byte, return it in a
;
MMC_WaitGetWritten
		bsr		MMC_WaitWritten			; Wait for byte
		lda		D_READ_DATA_REG			; get byte
		rts

;
; MMC_CloseFile : close current file
;		
MMC_CloseFile
		lda		#CMD_FILE_CLOSE			; Close the file
		lbra	MMC_SendCmd				; send the command
	
;
; MMC_ReadDOSSec,  Read an emulated dos sector
;
; Entry :
;	A	= drive id
;	X	= buffer
;
; Before calling :
; Drive track should have been set with a call to MMC_SeekCheck
; Head and Sector number should have been set with a call to MMC_SendHR
;
; Note for whatever path this code takes it should always pickup the
; simulated WD error at the end
;

MMC_ReadDOSSec
;        jsr     >CON_DumpRegs
		bsr		MMC_ReadDOSSecInit	; init the read
		bmi		MMC_ReadDOSSecEExit	; Error : exit
		
		clrb						; init count
	
MMC_ReadDOSSecLoopB
;		lbsr	ConWriteHexX
MMC_ReadDOSSecLoop		
		lbsr	MMC_WaitGetWritten	; wait for byte
		sta		,x+					; save in buffer
		incb						; inc count
		bne		MMC_ReadDOSSecLoop	; not done, loop again
		
MMC_ReadDOSSecEExit
		bra		MMC_GetWDError

;
; Read the next dos sector, must be preceeded by a call to 
; MMC_ReadDOSSec, that will correctly set the starting LSN and drive ID.
;
; Entry :
;	X	= buffer
;

MMC_ReadNextDOSSec
		bsr		MMC_ReadDOSSecInitNext	; Init the read
		clrb						; init count
		bra		MMC_ReadDOSSecLoopB	; Go read the data		

MMC_ReadDOSSecInitNext
		lda		#CMD_READ_NEXT_IMG_SEC	; read next sector
		bra		MMC_ReadDOSSecInit2

MMC_ReadDOSSecInit
		
		lda		#CMD_READ_IMG_SEC	; Read sector from AVR
MMC_ReadDOSSecInit2		
		lbsr	MMC_SendCmd			; send command
		bmi		MMC_ReadDOSSecExit	; if error handle it
		
		lda		#CMD_INIT_READ		; read the sector
		lbsr	MMC_SendCmdRaw		; send command

MMC_ReadDOSSecExit
		rts

;
; MMC_WriteDOSSec,  Read an emulated dos sector
;
; Entry :
;	A	= drive id
;	X	= buffer
;
; Before calling :
; Drive track should have been set with a call to MMC_SeekCheck
; Head and Sector number should have been set with a call to MMC_SendHR
;
; Note for whatever path this code takes it should always pickup the
; simulated WD error at the end
;

MMC_WriteDOSSec
		bsr		MMC_WriteDOSSecInit	; init the write
		bcs		MMC_ReadDOSSecExit	; Error : exit
		
        pshs    u
		ldu     #$100				; send 256 bytes
		lbsr	MMC_SendUBytes		; send 256 bytes from x
		puls    u
        
		lda		#CMD_WRITE_IMG_SEC	; write the sector
		lbsr	MMC_SendCmd

		bra		MMC_GetWDError		; Get simulated WD error	
		

MMC_WriteDOSSecInit
;		bsr		MMC_SendLBA			; send LBA to AVR
		
		lda		#CMD_INIT_WRITE		; Prepare to send the data
		lbra	MMC_SendCmd			; send command

;
; Get the simulated WD17xx/WD27xx error from the AVR
; simulated WD error returned in b 
;
MMC_GetWDError
		pshs	a
		lda		#CMD_GET_FDC_STATUS	; Get status
		lbsr	MMC_SendCmd			; send command
		exg		a,b					; get error into b for Flex
		tstb						; set flags
;		pshs	d,cc
;		pshs	cc
;		lbsr	CON_WriteHexByte
;		puls	a
;		lbsr	CON_WriteHexByteEOL
;		puls	d,cc
		puls	a,pc

;
; Check to see if image is valid and we can seek to track.
; track to seek to in a
;
; Entry :
;	A 	= Drive id 0..3
;	B	= Track 0..79
; Exit :
;	B 	= Simulated WD error.
;

MMC_SeekCheck
		pshs	b
		lbsr	MMC_InitSendBBytes	; Send drive id and track to AVR
		
		lbsr	MMC_WaitPutRead		; send drive id to AVR
		puls	a					; recover track
		lbsr	MMC_WaitPutRead		; send track AVR
		
		lda		#CMD_IMG_SEEK		; Try the seek
		lbsr	MMC_SendCmd
		
		bra		MMC_GetWDError		; Get simulated WD error
		
;
; Send first and last drive ids to MMC for list drives command
;
; Entry :
;	A 	= First drive id 0..3
;	B	= Second drive id 0..3
; Exit :
        
MMC_SendDriveIDs
        lbsr	MMC_InitSendBBytes  ; Init writing bytes
        
        lbsr    MMC_WaitPutRead     ; send first drive
        exg     b,a                 ; get second drive
        lbra    MMC_WaitPutRead     ; send second drive

;
; Send Head and record (sector) number.
; Logical sector no in a, 1..36 for Dragondos 1..18 for RSDOS
; DragonDos version needs to deal with both head and sector
; 

MMC_SendHR            
        pshs    d
        jsr	    >MMC_InitSendBBytes ; Init writing bytes
        
        ldb     FlexLastActiveDrv      	; Get drive no
;        decb
        exg     a,b                 ; swap it into a
        
        jsr     >MMC_WaitPutRead    ; send drive id
        exg     a,b                 ; recover sector no

        clrb                        ; Head no 0
        cmpa    #FlexSecTrack    	; > Sectors / Track, if so on side 2
        bls     MMC_SendHR_side0    ; lower, on side 0
        
        suba    #FlexSecTrack    	; get correct sector no
        incb                        ; on side 1
        
MMC_SendHR_side0
        exg     a,b                 ; send head first
        jsr     >MMC_WaitPutRead    ; send head       
        exg     a,b                 ; get sec no into a
        deca
        jsr     >MMC_WaitPutRead    ; send it
        
        lda     #CMD_LOAD_HR        ; set sector and head
        jsr     >MMC_SendCmd
        
        puls    d,pc                ; restore and return.
        
		
__mmc_func_end
