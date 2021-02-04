;
; DragonMMC functions for OS-9.
;
; 2021-02-03 Phill Harvey-Smith.
;

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
        
MMC_WaitNotBusyLoop        
		lda		D_STATUS_REG			; Busy flag
		anda	#STATUS_FLAG_BUSY		; is it set ?
		bne		MMC_WaitNotBusyLoop		; yes : keep waiting
		rts
		
;
; Init buffer write
;
MMC_InitSendBytes
		pshs	a
		lda		#CMD_INIT_WRITE			; Write bytes
		bsr		MMC_SendCmd				; send the command
		puls	a,pc
				
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

MMC_WaitReadLoop
		lda		D_STATUS_REG
		anda	#STATUS_FLAG_READ		; Has it been read yet ?
		bne		MMC_WaitReadLoop		; no : keep waiting
		rts

;
; MMC_WaitWritten, waits for the AVR to write a byte
;
MMC_WaitWritten
        
MMC_WaitWrittenLoop
		lda		D_STATUS_REG
		anda	#STATUS_FLAG_WRITTEN	; Written flag is it set ?
		beq		MMC_WaitWrittenLoop			; no : keep waiting
		rts

;
; MMC_WaitGetWritten, wait for the AVR to write a byte, return it in a
;
MMC_WaitGetWritten
		bsr		MMC_WaitWritten			; Wait for byte
		lda		D_READ_DATA_REG			; get byte
		rts

