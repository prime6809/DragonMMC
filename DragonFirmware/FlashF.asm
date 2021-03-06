;
; Flash the DragonMMC rom.
;
; 2016-05-26, Now platform independent.
;
; 2017-06-06, Ported to work with 39sf010 (or compatible flash rom).
;             Should also work with 39sf020 and 39sf040.
;
; 2017-08-23, V1.32f Initialize platform 0 config to 0.
;
; 2017-09-04, V1.33f, Mask out platform 0 flash / debug enable, leave others alone.
;

        use	cpudefs.asm
        use	dgndefs.asm
        use WDdefs.asm
        use	romdefs.asm
        use	mmc2def.asm
        use	DragonMMCdef.asm
        use	basictokens.asm
        use	basicdefs.asm
	
; Local Defs

FlashPageSize   EQU     64                  ; EEPROM pages are 64 bytes
        
BASE    equ     $0c00

        ifdef   Header
        org     BASE-FileHeadLen            ; Size of header
start
        fcb     MarkerHeadStart             ; Begin header
        fcb     FTypeBin                    ; binary file
        fdb     BASE                        ; load address
        fdb     LENGTH                      ; length of file
        fdb     BASE                        ; exec address
        fcb     MarkerHeadEnd               ; End header
        else
        org	BASE
        endc
        

codestart
        lbsr	MachineDetect2			    ; Detect Dragon or CoCo
        bne	    ClearCoCo
        jsr	    DTextCls			        ; clear screen
        bra	    signon
	
ClearCoCo
        jsr	    CTextCls			        ; clear screen
	
signon
        leax	mess-1,pcr
        lbsr	LTextOutString			    ; signon

		if 0
        ldd		CartBase					; get 2 characters at $C000
		cmpd	#"DK"						; check for Dos cart flag
		bne		ErrNotDMMC					; nope, not our cart
		
		ldd		CartBase+$2000				; get 2 characters at $E000
		cmpd	#"DK"						; check for Dos cart flag
		bne		ErrNotDMMC					; nope, not our cart
		endc 
		
        lda	    <CFGByte			        ; Get config byte

        bita	#CFG_ENABLE_DOS			    ; is dos emulation enabled ?
        bne	    ErrDosEnabled			    ; dos emulation enabled : Error bail
	
        lda     D_RAM_CTRL                  ; Save current value of ram control register
        pshs    a,cc                        ; on stack
        
        orcc    #FlagFIRQ+FlagIRQ           ; disable interrupts, just to be safe
        
        ora     #D_ROM_WE                   ; Enable writing to EEPROM
        anda    #~(D_ROM_A14+D_RAM_ENABLE)  ; turn off ram enable, select lower 16K of ROM
        sta     D_RAM_CTRL

;        lbsr    UnlockEE                    ; Unlock EEPROM if locked
        
        bsr     OpenRomFile                 ; open rom file
        lbmi    ErrOpen                     ; print error
        
        bsr     FlashRom                    ; Flash it
        lbmi    ErrFlash                    ; flash it
	
		if 0
		lda		#CAS_FILE					; close file 0
		lbsr	MMC_WaitPutLatchRead		; send file id to latch register

        lda     #CMD_FILE_CLOSE             ; Close flash file
        lbsr    MMC_SendCmd
        endc
		
		lbsr	MMC_CloseFile				; close the file
		
        leax	FlashOK-1,pcr			    ; point to compledted message
        lbsr	LTextOutString			
	
FlashDone
		clra                                ; byte set to 0
        lbsr	GetCFGinA					; get config byte for platform 0
		tfr		a,b							; platform byte to b
		andb	#CFG_HIDE_MAC				; leave these bits alone, but reset CFG_ENABLE_BOOTLOAD and CFG_DEBUG_LOG
		clra
		lbsr    SetCFGinAB                  ; go set it.

        puls    a,cc                        ; Restore ram control regiser & int status
        anda    #~(D_RAM_ENABLE)            ; Switch back to ROM
        sta     D_RAM_CTRL

        lbsr    CON_WaitKeyL                ; Wait for a key.....
        
        clr     <WarmStartFlag
        jmp     [HWVecReset]
 
        rts

ErrNotDMMC
		leax	NotDMMCErr-1,pcr			; Error not DMMC message
        lbsr	LTextOutString			
        rts	

ErrDosEnabled
        leax	DosErr-1,pcr			    ; Print error
        lbsr	LTextOutString			
        rts
        
ErrOpen leax    OpenErr-1,pcr               ; print error
        lbsr	LTextOutString			
        bra     FlashDone                   ; exit
        
ErrFlash
        leax    FlashErr-1,pcr              ; print error
        lbsr	LTextOutString			
        bra     FlashDone                   ; exit

;
; Open the flash file for input
;

OpenRomFile
        leax    FlashFileName,pcr           ; point at filename
        ldb     #FlashNameLen               ; Get length
        lbsr    MMC_SendName                ; send it to AVR
        
		lda		#CAS_FILE					; open default file
		lbsr	MMC_WaitPutLatchRead		; send file id to latch register
		
		lda     #CMD_FILE_OPEN_READ
        lbsr	MMC_SendCmd			        ; Open file 
        
        tsta                                ; Set flags
        rts
        
;
; Flash the ROM
;        

FlashRom
        lbsr    EraseFlash                  ; Erase the flash

; Make sure bank 0 selected
        lda     D_RAM_CTRL                      
        ora     #D_ROM_WE                   ; Enable writing to EEPROM
        anda    #~(D_ROM_A14+D_RAM_ENABLE)  ; turn off ram enable, select lower 16K of ROM
        sta     D_RAM_CTRL
        
        ldx	    TextVDUCursAddr			    ; get current cursor address
        stx	    OutPos1,pcr			        ; save it
        clr	    BlockNo,pcr			        ; block no 0
	
        bsr	    ShowBlock
        bsr     FlashBlock                  ; Flash lower 16K
        bmi     EndFlashRom                 ; error : exit
        
        lda     D_RAM_CTRL                  ; Change to upper block
        ora     #D_ROM_A14
        sta     D_RAM_CTRL

        bsr	    ShowBlock
        bsr     FlashBlock                  ; Flash upper 16K
        bmi     EndFlashRom                 ; error : exit
        
        lbsr	CON_EOL	
        clra    
EndFlashRom
        rts
        
FlashBlock
        leax	AddrMess-1,pcr			    ; Print writing mess
        lbsr	LTextOutString
	
        ldx	    TextVDUCursAddr			    ; get current cursor address
        stx	    OutPos2,pcr			        ; save it
	
        ldu     #$C000                      ; Base of ROM

FlashBlockLoop
        stu     >FalshPageBase              ; save base of block being flashed
        leax    ReadBuff,pcr                ; point at buffer
        ldb     #FlashPageSize              ; page size
        lbsr    MMC_ReadFileBlock           ; read the block

        bsr	    ShowAddr			        ; show flash address
        
        cmpu    #$FF00                      ; In IO area?
        bhs     SkipFlash                   ; yes, don't flash it
        
        leax    ReadBuff,pcr                ; point at buffer
        ldb     #FlashPageSize              ; page size

FlashNextByte
        lda     ,x+                         ; Get byte from buffer
        lbsr	FlashAByte					; write to flash
		leau	1,u							; point to next byte.
		decb                                ; decrement count
        bne     FlashNextByte               ; loop if more in page
        
FlashWait      
        leax    ReadBuff,pcr                ; point at buffer
        ldb     #FlashPageSize              ; page size
        ldu     >FalshPageBase              ; recover beginning of flashed page

FlashVerifyLoop        
        lda     ,x+                         ; byte from buffer
        cmpa    ,u+                         ; compare to rom
        bne     VerifyFail                  ; not equal, flash failed!
        
        decb                                ; decrememnt count
        bne     FlashVerifyLoop             ; Verify next
        
        bra     NextPage                    ; Do next page
        
SkipFlash
        leau    FlashPageSize,u             ; move to next page
NextPage
        cmpu    #$c000                      ; looped round to addr 0?
        bhs     FlashBlockLoop              ; no : go again
        
        clra
        rts
        
ShowBlock
        ldx	    OutPos1,pcr			        ; Get saved output address
        stx	    TextVDUCursAddr			    ; set output address
	
        leax	BlockMess-1,pcr			    ; point at message
        lbsr	LTextOutString			    ; print it
	
        lda	    BlockNo,pcr			        ; get block no
        lbsr	CONWriteHexByteEOL		    ; write it
	
        inc	    BlockNo,pcr			        ; increment block no
        rts

ShowAddr
        pshs	x,u
        ldx	    OutPos2,pcr			        ; Get saved output address
        stx	    TextVDUCursAddr			    ; set output address
	
        tfr	    u,x				            ; get write pointer
        lbsr	CONWriteHexX			    ; show address
	
        puls	x,u,pc

VerifyFail
        leau    -1,u                        ; point to fail address
        pshs    u                           ; save fail address
        leax	VerifyFailMess-1,pcr		; point at message
        lbsr    LTextOutString			    ; print it
	
        puls    x                           ; recover failed address
        lbsr	CONWriteHexX			    ; show address
        lbsr    CON_EOL
        lda     #-1                         ; flag error 
        tsta
        rts                                     

;
; Erase the entire flash rom.
;
; For details see Microchip 39sf010 datasheet
;

EraseFlash
        leax    EraseMess-1,pcr             ; print message
        lbsr    LTextOutString			    ; print it
	
		bsr		FlashCmdMode				; command mode
		
        lda     #$80                        ; Flash $5555 = $80
        bsr     WriteAt5555

        lda     #$AA                        ; Flash $5555 = $AA
        bsr     WriteAt5555

        lda     #$55                        ; Flash $2AAA = $55
        bsr     WriteAt2AAA

        lda     #$10                        ; Flash $5555 = $10
        bsr     WriteAt5555

        bsr     StartWaitForFlash           ; Wait for flash to do the erase 

        rts

FlashCmdMode  
        lda     #$AA                        ; Flash $5555 = $AA
        bsr     WriteAt5555

        lda     #$55                        ; Flash $2AAA = $55
        bsr     WriteAt2AAA
		rts
    
WriteAt2AAA
        ldu     #$2AAA                      ; Flash address $2AAA
        bra     FlashAbsWrite

WriteAt5555  
        ldu     #$5555                      ; Flash address $5555        
        bra     FlashAbsWrite
        
        
FlashAbsWrite
        ldb     D_RAM_CTRL					; get current RAM_CTRL reg
        
		cmpu    #$4000                      ; top half of ROM?
        bhs     FlashAbsTop
        
        andb    #~D_ROM_A14                 ; rom A14 = 0
        stb     D_RAM_CTRL
		leau	$c000,u						; Get real address into u
        bra     FlashAbsW
        
FlashAbsTop
        orb     #D_ROM_A14                  ; rom A14 = 1
        stb     D_RAM_CTRL
        leau    $8000,u                     ; Get real address into u
        
FlashAbsW
        sta     ,u                     		; write byte
        rts

;
; Write a byte in a to the flash at offset u.
; On entry u contains the address to write $c000-$FEFF, a contains byte to write.
;

WriteWaitForFlash
		sta		,u							; write byte to flash
StartWaitForFlash
		lda     ,u                          ; get toggle bit
        anda    #$40                        ; Mask out toggle bit
ContinueWait
        sta     >Toggle
        nop
		nop
		nop
        lda     ,u                          ; get byte
        anda    #$40                        ; Mask out toggle bit
        cmpa	>Toggle						; stopped toggling ?
		bne		ContinueWait
		rts

;
; Write a byte to flash, sends write command + address & byte to write.
; on entry D_RAM_CTRL is set, u contains the address to write, a the byte to write
;

FlashAByte
        pshs    b
        ldb     D_RAM_CTRL					; save D_RAM_CTRL
		pshs	d,u							; save offset & byte
		
		bsr		FlashCmdMode				; go into command mode
		
		lda		#$A0						; Write a byte to flash command
		bsr		WriteAt5555
		
		puls	d,u							; restore D_RAM_CTRL, offset & byte
		stb		D_RAM_CTRL
        puls    b
		bra		WriteWaitForFlash			; write it!
	
MachineDetect2
        clr	    >MachineType			    ; assume dragon
        ldx     #$B4BC                  	; Dragon ROM contains the word DRAGON at $B4BC.
        ldd		,x++			            ; looks for 'DRAGON' string 
        addd	,x++
        addd	,x++
        cmpd 	#$D4E7				        ; Sum of the letters of 'DR' + 'AG' + 'ON'
        beq	    MachineDetect2End		    ; Found, leave as dragon
        com	    >MachineType			    ; mark as CoCo
MachineDetect2End
        rts    

LTextOutString
        tst	    >MachineType			    ; CoCo or Dragon?
        bne	    COTextOutString			
        jmp	    >DTextOutString
COTextOutString
        jmp	    >CTextOutString
	
mess	
        ifdef	Dragon
        fcc	    "DRAGON SD/MMC"
        else
        fcc	    "COCO   SD/MMC"
        endc
        fcc 	" FLASH UPDATER"
		fcb		$0d
		fcc		"V1.34F"
        fcb	    $0d,0
;		         12345678901234567890123456789012	
DosErr
        fcc	    "ERROR DOS EMULATION ENABLED."
        fcb	    $0d
        fcc	    "PLEASE DISABLE AND POWER CYCLE"
        fcb	    $0d
        fcc	    "BEFORE RUNNING UPDATE"
        fcb	    $0d
        fcb	    $00

NotDMMCErr
		fcc		"ERROR, DRAGONMMC ROM NOT FOUND"
		fcb	    $0d
		fcc		"ABORTING"
		fcb	    $0d
        fcb	    $00
	

OpenErr fcc     "ERROR OPENING FLASH FILE :"
        fcb	    $0d
        fcc     "DGNMMC.ROM"
        fcb     $0d,$00
        
FlashErr
        fcc     "ERROR WRITING FLASH FILE :"
        fcb	    $0d
        fcc     "AT ADDR: "
        fcb     $00
        fcb     $0d,$00

FlashOK
        fcc     "FINISHED WRITING FLASH FILE."
        fcb     $0d
        fcc     "PRESS A KEY TO RESTART."
        fcb     $0d,$00

EraseMess
        fcc     "ERASING 39SF010......"
        fcb     $0d,$00
            	
BlockMess
        fcc	    "WRITING PAGE: "
        fcb	    0
	
AddrMess
        fcc	    "WRITING BLOCK AT: "
        fcb	    0
			
VerifyFailMess
        fcb     $0d,$0d
        fcc     "VERIFY FAILED AT: "
        fcb     0
        
FlashFileName
        fcc     "DGNMMC.ROM"
FlashNameLen    equ     (*-FlashFileName)
                        
FailAddr
        fdb     0                               ; Byte where flash failed


;
; Routines copied from Console.asm, as we need them to be platform independent.
;

CONWriteHexX
        pshs	d
        tfr	    x,d
        bsr	    ConWriteHexWordS		        ; Write x
        puls	d,pc
;
; Write hex word in D.
;

CONWriteHexWord
        pshs	d				                ; save d
        bsr	    CONWriteHexByte			        ; Write MSB
        exg	    a,b				                ; get LSB
        bsr	    CONWriteHexByte			        ; Write LSB
        puls	d,pc				            ; restore / return
		
ConWriteHexWordS
        bsr	    CONWriteHexWord			        ; Write word
        bra	    CONWriteSpace			        ; write space
		
;
; Write byte in a as hex followed by space.
;
		
CONWriteHexByte
        pshs	a
	
        lsra					                ; Move msn to lsn
        lsra	
        lsra	
        lsra	

        bsr	    CONGetHexNibbleA		        ; send msn
        bsr	    LBasicScreenOut	
	
        lda	    ,s				                ; retrive digit
        bsr	    CONGetHexNibbleA		        ; send lsn
        bsr	    LBasicScreenOut	
        puls	a,pc

CONWriteHexByteS
        bsr		CONWriteHexByte	
		
CONWriteSpace
        pshs	a
        lda	    #$20				            ; send space
        bsr	    LBasicScreenOut			        ; send it
        puls	a,pc
	
		
CONHexDigits
        fcc		/0123456789ABCDEF/
	
CONGetHexNibbleA
        pshs	x
        anda	#$0F				            ; mask out bytes
	
        leax	>CONHexDigits,pcr		        ; Point to digits
        leax	a,x				                ; point to needed digit
        lda	    ,x				                ; **GET** hex digit !
        puls	x,pc

		
;
; CON_EOL : Write end of line sequence
;

CON_EOL
        lda	    #$0d				            ; EOL
        bra	    LBasicScreenOut			        ; send it
		

CONWriteHexByteEOL
        pshs	a
        bsr	    CONWriteHexByte
        bsr	    CON_EOL
        puls	a
        rts

LBasicScreenOut
        tst	    >MachineType			        ; CoCo or Dragon?
        bne	    COBasicScreenOut	
        jmp	    >DBasicScreenOut
COBasicScreenOut
        jmp	    >CBasicScreenOut
  

CON_WaitKeyL
		pshs	a
CON_WaitKeyLoopL
		tst     >MachineType			        ; CoCo or Dragon?
        bne     COKeyscan
        jsr		>DBasicKbdIn
        bra     CON_TestKey
COKeyscan
        jsr		>CBasicKbdIn
CON_TestKey
		beq		CON_WaitKeyLoopL
		puls	a
		rts     
        
        use	mmc_func.asm
		use config_func.asm

        ifdef    Header
LENGTH  equ     (*-start)	
        endc
        
;
; Note anything below here is **NOT** saved in the binary, use for uninitialized data **ONLY**
;
        
ReadBuff        
        rmb     FlashPageSize               ; Buffer to read ROM into 
	
BlockNo
        rmb	    1
OutPos1
        rmb	    2
OutPos2	
        rmb	    2
FalshPageBase
        rmb     2
MachineType
        rmb	    1				            ; Machine type, 0=Dragon, nonzero=coco
Toggle
        rmb     1                           ; Saved toggle bit
	