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

		ifdef	Dragon
CasRDCksum		EQU		$B972				
CasWRCksum		EQU		$B9C9
		else
CasRDCksum		EQU		$A73B
CasWRCksum		EQU		$A824
		endc

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
; Init buffer write
;
MMC_InitSendBytes
		pshs	a
		lda		#CMD_INIT_WRITE			; Write bytes
		bsr		MMC_SendCmd				; send the command
		puls	a,pc
		
		ifne	0
;
; Init write buffer and send B bytes from X to MMC 
;

MMC_StartSendBBytes
		bsr		MMC_InitSendBytes
		endc
		
;
; Send B bytes pointed to by X to the WRITE_DATA_REG
;
MMC_SendBBytes
		pshs	d,x
        tstb                            ; Is b zero ?
        beq     MMC_SendBBytesExit      ; yep : exit
		
MMC_SendBBytesLoop
        lda		,x+						; get a byte

		bsr		MMC_WaitPutRead			; send byte, wait for AVR to read it

		decb							; decrement count
		bne		MMC_SendBBytesLoop		; more : loop again

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
MMC_SendName
		bsr		MMC_InitSendBytes		; start sending bytes

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
		bsr		MMC_InitSendBytes		; start sending bytes

MMC_SendTillZeroLoop
        lda     ,x+                     ; get a byte
        tsta                            ; Zero?
        beq     MMC_SendTillZeroExit    ; yep : stop sending
        
        bsr     MMC_WaitPutRead			; send byte, wait for AVR to read it
        bra     MMC_SendTillZeroLoop

MMC_SendTillZeroExit
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

		ifndef	MINIMUM
		ifne	0
;
; MMC_WaitGetWrittenReturn, wait for the AVR to write a byte, return it to basic
;

MMC_WaitGetWrittenReturn
		bsr		MMC_WaitWritten			; Wait for byte
		ldb		D_READ_DATA_REG			; get byte
		jmp		VarAssign8Bit			; return it to basic

		endc
		endc
;
; MMC_CloseFile : close current file
;		
MMC_CloseFile
		lda		#CAS_FILE				; file id of cas file
		bsr		MMC_WaitPutLatchRead	; send it to latch register
	
		lda		#CMD_FILE_CLOSE			; Close the file
		lbsr	MMC_SendCmd				; send the command
		rts

		
;
; MMC_ReadFByte : Read a byte from a file and return it in a
;
MMC_ReadFByte
		lda		#1						; One byte
		bsr		MMC_ReadFCommon
		bra		MMC_WaitGetWritten		; get the byte, returned in a

MMC_ReadFCommon
		pshs	a						; save byte count
		lda		#CAS_FILE				; file id of cas file
		bsr		MMC_WaitPutLatchRead	; send it to latch register
		puls	a						; restore byte count
		
		bsr		MMC_WaitPutLatchRead	; send it to latch register
		
		lda 	#CMD_READ_BYTES			; Read bytes from file
		lbsr	MMC_SendCmd				; send the command
		
		lda		#CMD_INIT_READ			; Read the byte
		lbsr	MMC_SendCmdRaw			
		rts
		
;
; Read a block from open cas file, length in 
; Entry :
;	CasBlockLen 	= size of block in bytes $00..$FF
;	X				= Pointer to buffer to recieve bytes
;
; Exit :
;	X				= Updated to point to byte after last byte read.
;	a				= error code, $00=none, $01=Csum error, $02=invalid dest addr
;	CasCkSum		= Checksum of block
;	
;

		ifndef	MINIMUM

MMC_ReadFBlock
		bsr		MMC_ReadFileBlockCas	; read the block
		jmp		CasRDCksum				; and back to the rom for the checksum

		endc
;		
; Read a block pointed to by X from MMC 
;
; Entry :
;	b				= size of block in bytes $00..$FF
;	X				= Pointer to buffer to recieve bytes
;
; Exit :
;	X				= Updated to point to byte after last byte written.
;

MMC_ReadFileBlock
		stb		<CasBlockLen		; Save block length
		stb		<CasIOErrorCode		; running count
		
MMC_ReadFileBlockCas
		lda		<CasBlockLen		; get length of block
		bsr		MMC_ReadFCommon		; Set the bytes
		
MMC_ReadFBlockLoop
		lbsr	MMC_WaitGetWritten	; get the byte, returned in a
		sta		,x					; save in buffer
		cmpa	,x+					; Did it save ok ? 
		bne		MMC_ReadFBlockError	; Yep eat rest of read and return error
		adda    <CasCkSum			; add read byte to checksum
        sta    	<CasCkSum			
        dec     <CasIOErrorCode		; decrement byte count
		bne		MMC_ReadFBlockLoop	; Keep going if more bytes

		rts
	
		suba	<CasCkSum			; check against calculated checksum
		bne		MMC_ReadFBlockExit
	
		lda		#$01				; Flag error
	
MMC_ReadFBlockExit
		sta		<CasIOErrorCode		; Save error code
		rts	

MMC_ReadFBlockError
		lda		#$02				; Flag error	
		bra		MMC_ReadFBlockExit	
		

		ifndef	MINIMUM
;		
; Read a block pointed to by X from MMC, does not use tape vars like MMC_ReadFileBlock 
;
; Entry :
;	b				= size of block in bytes $00..$FF
;	X				= Pointer to buffer to recieve bytes
;
; Exit :
;	X				= Updated to point to byte after last byte written.
;

MMC_ReadFileBlockRaw
        pshs    b                   ; save byte count                       
        exg     b,a                 ; get byte count
        bsr     MMC_ReadFCommon     ; Tell AVR to read bytes & begin reading

        puls    b                   ; restore byte count
MMC_ReadFileBlockRawLoop
		lbsr	MMC_WaitGetWritten	; get the byte, returned in a
		sta		,x+ 				; save in buffer
        decb                        ; decrement byte count
		bne		MMC_ReadFileBlockRawLoop ; Keep going if more bytes

		rts
;
; Write a byte to open cas file, from a
;
	
MMC_WriteFByte
		pshs	a
		lda		#CMD_INIT_WRITE		; Initiialise write
		lbsr	MMC_SendCmd			; send command
		lda		,s					; peek a back off stack 
		
		lbsr	MMC_WaitPutRead 	; send byte to AVR
		
		lda		#CAS_FILE				; file id of cas file
		lbsr	MMC_WaitPutLatchRead	; send it to latch register
		
		lda		#1					; Write 1 byte
		lbsr	MMC_WaitPutLatchRead	; send it to latch register
		
		lda		#CMD_WRITE_BYTES	; Write bytes
		lbsr	MMC_SendCmd			; send command
		
		puls	a,pc				; restore and return

;
; Read 2 bytes from file and into D
;
MMC_ReadDFile        
        lda     #2                  ; 2 bytes
        bsr     MMC_ReadFCommon     ; go read them 
;
; Fetch two bytes into D register, assumes CMD_INIT_READ already reading bytes
; 
		
MMC_ReadD
        lbsr    MMC_WaitGetWritten  ; get msb
        exg     a,b                 ; swap them
        lbsr    MMC_WaitGetWritten  ; get lsb
        exg     a,b                 ; swap them back
        rts

;
; Send D register, assumes CMD_INIT_WRITE already sending bytes
; 
		
MMC_WriteD
        pshs    d
        lbsr    MMC_WaitPutRead     ; send msb
        exg     a,b                 ; swap them
        lbsr    MMC_WaitPutRead     ; send lsb
        puls    d,pc
        
        
;
; Write a block pointed to by X to MMC (tape emulation)
;
; Entry :
;	CasBlockLen 	= size of block in bytes $00..$FF
;	CasIOErrorCode	= CasBlockLen
;	X				= Pointer to buffer to recieve bytes
;
; Exit :
;	X				= Updated to point to byte after last byte written.
;

MMC_WriteFBlock
		bsr		MMC_WriteFileBlockCas	; Write it
		jmp		CasWRCksum				; Jump back to rom

;		
; Write a block pointed to by X to MMC 
;
; Entry :
;	b				= size of block in bytes $00..$FF
;	X				= Pointer to buffer to recieve bytes
;
; Exit :
;	X				= Updated to point to byte after last byte written.
;

MMC_WriteFileBlock
		stb		<CasBlockLen		; Initialise block len / count
		stb		<CasIOErrorCode
		
MMC_WriteFileBlockCas
		lda		#CMD_INIT_WRITE		; Initiialise write
		lbsr	MMC_SendCmd			; send command
MMC_WriteFBlockLoop
;		com		$401
		lda		,x+					; get byte to write
		lbsr	MMC_WaitPutRead 	; send byte to AVR
	
		dec     <CasIOErrorCode		; Decrement count
		bne		MMC_WriteFBlockLoop	; More ? yep : keep going
		
		lda		<CasBlockLen		; get length of block
MMC_WriteAFromAVRBuf
		pshs	a						; save byte count
		lda		#CAS_FILE				; file id of cas file
		lbsr	MMC_WaitPutLatchRead	; send it to latch register
		puls	a						; restore byte count
		
		lbsr	MMC_WaitPutLatchRead	; send it to latch register
				
		lda		#CMD_WRITE_BYTES	; Write it to the file
		lbsr	MMC_SendCmd			; send it
		rts	

;
; MMC_SaveFile, save to the open file 
; 
; Entry :
;	u				= Pointer to the file header for the file to save.
;

MMC_SaveFile
		ldb		#FileHeadLen		; Header length
		leax	,u					; point to header
		bsr		MMC_WriteFileBlock	; Write the block

		ldx		HdrLoad,u			; Get load address

MMC_SaveFileLoop	
		ldd		HdrLen,u			; Get file length
		cmpd	#$100				; > 256 bytes ?
		blo		MMC_SaveTail		; no : last block

		subd	#$100				; decrement length
		std		HdrLen,u			; update
		
		clrb						; flag 256 bytes
		bsr		MMC_WriteFileBlock	; Write the block
		bra		MMC_SaveFileLoop	; Do next block	
		
MMC_SaveTail
		tstb						; any more bytes ?
		beq		MMC_SaveFileExit	; nope : exit
		bsr		MMC_WriteFileBlock	; Write the block
				
MMC_SaveFileExit
		clra	
		rts
		
;
; MMC_LoadFile, load from the open file
;
; Entry :
;	u	= Pointer to the file header for the file to load.
; Exit :
;	x	= pointer just beyond end of loaded buffer.
;


MMC_LoadFile
		ldx		HdrLen,u			; Get length
		pshs	x					; save on stack
		ldx		HdrLoad,u			; get load address
		bra		MMC_LoadFileLoop	; go do it

;
; MMC_LoadFile2, load from the open file
;
; Entry :
;   x	= Pointer to buffer to load file into
;	u	= Bytes to read from the file.
; Exit :
;	x	= pointer just beyond end of loaded buffer.
;

MMC_LoadFile2
		pshs	u					; save length	
		
MMC_LoadFileLoop
		ldd		,s					; Get length remaining
		cmpd	#$100				; > 256 bytes ?
		blo		MMC_LoadTail		; no : last block
		
		subd	#$100				; decrement remaining length
		std		,s					; update
		
		clrb						; flag 256 bytes
		lbsr	MMC_ReadFileBlock	; read next block
		bra		MMC_LoadFileLoop	; do next block
		
MMC_LoadTail
		tstb						; any more bytes ?
		beq		MMC_LoadFileExit	; nope : exit
		lbsr	MMC_ReadFileBlock	; load last block
		
MMC_LoadFileExit
		leas	2,s					; drop saved count
		clra
		rts

        ifeq    1
;
; Send LBA to AVR
;
MMC_SendLBA
		pshs	u,d
		lbsr	MMC_InitSendBytes	; Begin sending bytes
		
		lbsr	MMC_WaitPutRead		; send drive id it
		
		exg		u,d					; get LSW of LBA
		exg		a,b					; move lsb into a
		
		lbsr	MMC_WaitPutRead		; send LSB of LSN
		
		exg		a,b					; get next byte of LBA
		
		lbsr	MMC_WaitPutRead		; send it
		
		exg		d,u					; retrieve MSB
		
		exg		a,b					; move lsb into a
		lbsr	MMC_WaitPutRead		; send it

		clra						; msb of LSN always 0
		lbsr	MMC_WaitPutRead		; send it

		lda		#CMD_LOAD_PARAM		; Tell AVR
		lbsr	MMC_SendCmd			; send command
		
		puls	u,d,pc				; restore / return
        
        endc
		
;
; MMC_ReadDOSSec,  Read an emulated dos sector
; MMC_ReadDOSSec2  Read just the first two bytes, used by 'BOOT'
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
		bra		MMC_ReadDOSSecLoopB	; Go read the data

MMC_ReadDOSSec2
		bsr		MMC_ReadDOSSecInit	; init the read
		bmi		MMC_ReadDOSSecEExit	; Error : exit
		ldb		#$FE				; init count, reads 2 bytes
	
MMC_ReadDOSSecLoopB
;		lbsr	ConWriteHexX
MMC_ReadDOSSecLoop		
		lbsr	MMC_WaitGetWritten	; wait for byte
		sta		,x+					; save in buffer
		incb						; inc count
		bne		MMC_ReadDOSSecLoop	; not done, loop again
		
MMC_ReadDOSSecEExit
		bsr		MMC_GetWDError
		rts

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

		bsr		MMC_GetWDError		; Get simulated WD error	
		rts


MMC_WriteDOSSecInit
;		bsr		MMC_SendLBA			; send LBA to AVR
		
		lda		#CMD_INIT_WRITE		; Prepare to send the data
		lbsr	MMC_SendCmd			; send command
		rts

;
; Get the simulated WD17xx/WD27xx error from the AVR
; simulated WD error returned in a
;
MMC_GetWDError
		lda		#CMD_GET_FDC_STATUS	; Get status
		lbsr	MMC_SendCmd			; send command
		tsta						; set flags
		rts

;
; Check to see if image is valid and we can seek to track.
; track to seek to in a
;
; Entry :
;	A 	= Drive id 0..3
;	B	= Track 0..79
; Exit :
;	A 	= Simulated WD error.
;

MMC_SeekCheck
		pshs	b
		lbsr	MMC_InitSendBytes	; Send drive id and track to AVR
		
		lbsr	MMC_WaitPutRead		; send drive id to AVR
		puls	a					; recover track
		lbsr	MMC_WaitPutRead		; send track AVR
		
		lda		#CMD_IMG_SEEK		; Try the seek
		lbsr	MMC_SendCmd
		
		bsr		MMC_GetWDError		; Get simulated WD error
		rts
		
;
; Send first and last drive ids to MMC for list drives command
;
; Entry :
;	A 	= First drive id 0..3
;	B	= Second drive id 0..3
; Exit :
        
MMC_SendDriveIDs
        lbsr	MMC_InitSendBytes  ; Init writing bytes
        
        lbsr    MMC_WaitPutRead     ; send first drive
        exg     b,a                 ; get second drive
        lbsr    MMC_WaitPutRead     ; send second drive
        rts
  
;
; issue a format image command.
; Entry :
;   A = Drive id 0..3
;   B = Tracks / sides, b7=0 single sided,  b7=1 double sided.
;
MMC_FormatImage
        jsr	    >MMC_InitSendBytes ; Init writing bytes
        
        jsr     >MMC_WaitPutRead    ; send drive id

        tfr     b,a                 ; get tracks LSB
        anda    #$7F                ; mask out sides
        jsr     >MMC_WaitPutRead    ; tracks LSB
		
        clra                        ; Send MSB of tracks = 0
        jsr     >MMC_WaitPutRead    ; tracks MSB
             
        lda     #$01                ; Assume single sided
        tstb                        ; check for double sided
        bpl     MMC_FormatImageSS   ; no : skip
        inca
        
MMC_FormatImageSS
        jsr     >MMC_WaitPutRead    ; heads
        lda     #SectorsPerTrack    ; 18 sectors / track
        jsr     >MMC_WaitPutRead    
        
        lda     #CMD_CREATE_IMG     ; Send command
        
        lbsr	MMC_SendCmd
        rts

;
; Send Head and record (sector) number.
; Logical sector no in a, 1..36 for Dragondos 1..18 for RSDOS
; DragonDos version needs to deal with both head and sector
; 

        ifdef    Dragon
MMC_SendHR       
        
        pshs    a
        jsr	    >MMC_InitSendBytes ; Init writing bytes
        
        ldb     <DosDriveNo      	; Get drive no
        decb
        exg     a,b                 ; swap it into a
        
        jsr     >MMC_WaitPutRead    ; send drive id
        exg     a,b                 ; recover sector no

        clrb                        ; Head no -1
        cmpa    #SectorsPerTrack    ; > Sectors / Track, if so on side 2
        bls     MMC_SendHR_side0    ; lower, on side 0
        
        suba    #SectorsPerTrack    ; get correct sector no
        incb                        ; on side 2
        
MMC_SendHR_side0
        exg     a,b                 ; send head first
        jsr     >MMC_WaitPutRead    ; send head       
        exg     a,b                 ; get sec no into a
        deca
        jsr     >MMC_WaitPutRead    ; send it
        
        lda     #CMD_LOAD_HR        ; set sector and head
        jsr     >MMC_SendCmd
        
        puls    a,pc                ; restore and return.
        
        else

;
; RSDOS version, much simpler as head is always 0
;

MMC_SendHR       

        pshs    a
        jsr	    >MMC_InitSendBytes ; Init writing bytes
        
        ldb     dcdrv               ; Get drive no
        exg     a,b                 ; swap it into a
        
        jsr     >MMC_WaitPutRead    ; send drive id
 
        clra    
        jsr     >MMC_WaitPutRead    ; send head, always 0 for RSDOS
       
        exg     a,b                 ; get sec no into a
        deca
        jsr     >MMC_WaitPutRead    ; send it
        
        lda     #CMD_LOAD_HR        ; set sector and head
        jsr     >MMC_SendCmd
        
        puls    a,pc                ; restore and return.
        
        endc
        
        
;
; Get disk geometry of drive in DosLastDrive
;
; Entry : DosLastDrive contains drive to interigate
; Exit	: DosDxTracks and DosDxSecTrack set for drive
;		  Also returned in A=Tracks, B=Sec/Trk
;
; If an error occours, or a geometry block is not present
; defaults to standard single sided, 40 track.
;

		ifne	0
MMC_GetGeom
		pshs	x
		lda		<DosLastDrive		; Pickup drive id
		deca						; make zero based

		ldx		#DosD0Online		; Point to tables for this drive
		leax	a,x

		lbsr	MMC_WaitPutLatchRead	; send the drive id		
		
		lda		#CMD_IMG_GET_GEOM	; get geometry
		lbsr	MMC_SendCmd			; send command
		
		lda		#CMD_INIT_READ		; Read the results
		lbsr	MMC_SendCmdRaw		; send command
		
		lbsr	MMC_WaitGetWritten	; get sec/track
		
		sta		DosSecTrkTblOfs,x	; save sec/trk it
		exg		a,b					; get sec / trk in b
	
		lbsr	MMC_WaitGetWritten	; get tracks
		sta		DosTracksTblOfs,x	; save tracks it
		
		puls	x,pc				; restore / return
		endc 
		
		endc
		
__mmc_func_end
