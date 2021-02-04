********************************************************************
* Boot_d64, bootfile for DragonMMC interface.
* Provides HWInit, HWTerm, HWRead which are called by code in
* "use"d boot_common.asm
*
* 2019-04-03, P.Harvey-Smith.
*       Ported by using d64 booter as a skeleton and adding DragonMMC
*		functions from ROM source.
*

                nam   Boot
                ttl   DragonMMC Boot module

                use     defsfile

* Default Boot is from drive 0
BootDr          set   0

tylg            set   Systm+Objct
atrv            set   ReEnt+rev
rev             set   $01

                mod   eom,name,tylg,atrv,start,size

* NOTE: these are U-stack offsets, not DP
seglist         rmb   2         pointer to segment list
blockloc        rmb   2         pointer to memory requested
blockimg        rmb   2         duplicate of the above
bootsize        rmb   2         size in bytes
LSN0Ptr         rmb   2         In memory LSN0 pointer
drvsel          rmb   1
CurrentTrack    rmb   1         Current track number
* Note, for optimization purposes, the following two variables
* should be adjacent!!
ddtks           rmb   1         no. of sectors per track
ddfmt           rmb   1
side            rmb   1         side 2 flag

size            equ   .

name            equ   *
                fcs   /Boot/
                fcb   $00

* Common booter-required defines
LSN24BIT        equ   	1
FLOPPY          equ   	1

HWInit  

HWTerm  clrb
        rts

        use     boot_common.asm

* HWRead - Read a 256 byte sector from the device
*   Entry: Y = hardware address
*          B = bits 23-16 of LSN
*          X = bits 15-0  of LSN
*                  blockloc,u = ptr to 256 byte sector
*   Exit:  X = ptr to data (i.e. ptr in blockloc,u)
*          Carry Clear = OK, Set = Error

ReadSec
HWRead
;		pshs	y
;		ldy		#$2112
;		puls	y
		
		lda		#BootDr					; get boot drive
		bsr		MMC_SendLSN				; send LSN to mmc

		ldx		blockloc,u				; get ptr to data
		bsr		MMC_ReadDOSSec			; go read it

		tsta	
		bpl		ReadSecNoErr			; No error : exit

		comb							; flag error	
		rts

ReadSecNoErr
		clrb
		rts
		
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
; MMC_WaitNotBusy, waits for the AVR busy flag to be reset.
;
MMC_WaitNotBusy
        
MMC_WaitNotBusyLoop        
		lda		D_STATUS_REG			; Busy flag
		anda	#STATUS_FLAG_BUSY		; is it set ?
		bne		MMC_WaitNotBusyLoop		; yes : keep waiting
		rts
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

;
; Send LSN to AVR :
; a	= drive id
; b	= LSN 17..23
; x = LSN 0..15
;
MMC_SendLSN
		pshs	x,d
		lbsr	MMC_InitSendBBytes	; Begin sending bytes
		
		lbsr	MMC_WaitPutRead		; send drive id 
		
		exg		x,d					; get LSW of LBA
		exg		a,b					; move lsb into a
		
		lbsr	MMC_WaitPutRead		; send LSB of LSN
		
		exg		a,b					; get next byte of LBA
		
		lbsr	MMC_WaitPutRead		; send it
		
		exg		d,x					; retrieve MSB
		
		exg		a,b					; move lsb into a
		lbsr	MMC_WaitPutRead		; send it

		clra						; msb of LSN always 0
		lbsr	MMC_WaitPutRead		; send it

		lda		#CMD_LOAD_LBA		; Tell AVR
		lbsr	MMC_SendCmd			; send command
		
		puls	x,d,pc				; restore / return
        
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
		lda		#CMD_READ_IMG_SEC	; Read sector from AVR
		bsr		MMC_SendCmd			; send command
		bmi		MMC_ReadDOSSecExit	; if error handle it
		
		lda		#CMD_INIT_READ		; read the sector
		bsr		MMC_SendCmdRaw		; send command
		
		pshs	y,x
;		ldy		#$8000
		
		clrb						; init count
MMC_ReadDOSSecLoop		
		bsr		MMC_WaitGetWritten	; wait for byte
;		sta		,y+
		sta		,x+					; save in buffer
		incb						; inc count
		bne		MMC_ReadDOSSecLoop	; not done, loop again
		
		puls	y,x
		
		clra						; flag no error
MMC_ReadDOSSecExit
;		bsr		MMC_GetWDError
		rts


Address  fdb   D_MMC_BASE
        emod
eom     equ   *
        end
