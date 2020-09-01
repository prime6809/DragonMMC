;
; CmdsRam	: Ram copy related commands.
;

;
; Note some routines make calls / returns into the Dragon and CoCo roms.
; Please remember to define them so they assemble correctly on *BOTH* 
; machines.
; 

__cmd_ram

;
; Copy Dragon ROM from $8000-$FEFF to ram.
;
CmdRamBoot
		pshs	x						; save x

		ifndef	EMULATE
        bsr     GoROMMode               ; make sure we are in ROM mode, RAM write enabled
		ldx		#$8000					; point at base of system ROM
RamBootLoop
		ldd		,X						; get a word
		std		,x++					; put it in ram, writes always go to ram
		cmpx	#$FF00					; done all
		blo		RamBootLoop				; nope, do next
		
        bsr     GoRAMMode               ; Switch to RAM mode
		
		else
		
		ldx		#RamBoot				; Copy to tape buffer
		ldu		#DCasFNameLen			
RamBootCopyLoop
		lda		,x+						; get byte
		sta		,u+						; save it
		cmpx	#GoROMMode				; done all?
		bne		RamBootCopyLoop
		jmp		>DCasFNameLen			; call copied code
		
RamBoot		
		ldx		#$8000					; point at base of system ROM
RamBootLoop
		clr		SAMCTY					; ROM mode
		ldd		,X						; get a word
		clr		SAMSTY					; RAM mode
		std		,x++					; put it in ram, writes always go to ram
		cmpx	#$FF00					; done all
		blo		RamBootLoop				; nope, do next
		jmp		>RamBootDone

RamBootDone		
		endc
		
		clra							; no error
		puls	x,pc					; restore and return

GoROMMode
        pshs    a                       ; save a
        lda     D_RAM_CTRL              ; make sure we are in ROM mode, ram write enabled
		anda	#~(D_RAM_ENABLE+D_RAM_WP)  
		sta     D_RAM_CTRL              
        puls    a,pc                    ; restore and return
        

GoRAMMode
        pshs    a                       ; save a
        lda     D_RAM_CTRL              ; enable RAM
		ora	    #D_RAM_ENABLE          
        sta     D_RAM_CTRL              
        puls    a,pc                    ; restore and return

;
; Copy Dragon rom to ram and patch for cassette emulation on mmc
;

CmdMCas
		tsta
		beq		CmdMCasNormal
		JSR     <BasChrGet				; skip the charcter
		
		lda		#D_RAM_ENABLE			; Leave it write enabled
		bra		CmdMcasGo
		
CmdMCasNormal
		lda		#D_RAM_ENABLE+D_RAM_WP	; put us in ram mode, write protected.
CmdMcasGo	
		ldx		#CasPatch				; point to patches
CmdMcasGo2
		pshs	a
	
		bsr		CmdRamBoot				; Switch to all ram mode
CmdMCasNext
		ldy		,x++					; Get address to patch
		beq		CmdMCasPatchDone 		; address=0, end
	
		ldb		,x+						; Byte count
CmdMCasPLoop
		lda		,x+						; Get a byte
		sta		,y+						; Put in ram
		decb							; Decrement count
		bne		CmdMCasPLoop
		bra		CmdMCasNext				; Do next patch
	
CmdMCasPatchDone
		lda		D_RAM_CTRL              ; Get ram control register
        ora	    ,s+						; mask in flags
		sta		D_RAM_CTRL              ; Put back.
		
		rts

			ifdef Dragon
CP1Addr		EQU		$BDED
CP3Addr		EQU		$B945
CP4Addr		EQU		$B961
CP6Addr		EQU		$b9c1
CloadExit	EQU		$B733
			else
CP1Addr		EQU		$A782
CP3Addr		EQU		$A712
CP4Addr		EQU		$A72B
CP6Addr		EQU		$A81C
CloadExit	EQU		$A4F5	
			endc

;******************************
; Patches to rom for CmdMcas. *
;******************************

CasPatch
		ifndef	EMULATE
		
		FDB	CP1Addr				; Address
		FCB	CasP1E-CasP1		; length

; Patch wait for leader to just return
CasP1
		rts						; Bytes to patch
CasP1E


		FDB	CasByteIn			; Address
		FCB	CasP2E-CasP2		; length

; Patch byte in to read a byte from the MMC
CasP2
		JMP	>MMC_ReadFByte	
CasP2E	


		FDB	CP3Addr				; Address
		FCB	CasP3E-CasP3		; length

; Patch byte in to read a byte from MMC
CasP3
		JSR		>MMC_ReadFByte	
		ifdef	Dragon	
		NOP
		endc
CasP3E

		FDB	CP4Addr				; Address
		FCB	CasP4E-CasP4		; length

; Patch byte in to read a block from the MMC
CasP4
		nop
		nop
		jmp		>MMC_ReadFBlock	;Read block
CasP4E


		FDB	CasByteOut			; Address
		FCB	CasP5E-CasP5		; length

; Patch write a byte to the MMC
CasP5
		JMP		>MMC_WriteFByte	
CasP5E


;
; Patch the write byte loop in the CasBlockOut to use a multi-byte write file.
;

		fdb	CP6Addr
		FCB	CasP6E-CasP6		; length

; Patch write a byte to the MMC
CasP6
		JMP		>MMC_WriteFBlock
		nop
CasP6E

;
; Patch the gap checking routine to not toggle the cassette relay.
;

;		fdb	$b906
;		fcb	CasP7E-CasP7		; length
;
;CasP7
;		fcb	$21					; code for BRN
;CasP7E

;
; Patch motor on to just return, without turning on motor.
;

;		fdb	CasMotorOn
;		fcb	CasP8E-CasP8		; length
;
;CasP8
;		rts
;CasP8E

		endc
;
; Patch Cload, to run loaded basic program (tokenized)
;
		fdb	CloadExit
		fcb	CasP9E-CasP9		; length

CasP9
		JMP	CLoadRun			; Go run it!
CasP9E

;
; Patch Cload, to run loaded basic program (ASCII)
;
		fdb	VectCloseFileCmd
		fcb	CasP10E-CasP10		; length

CasP10
		JMP	CLoadRunASCII		; Go run it!
CasP10E
        
;*****************************************************************
;******************[ End of Patches ]*****************************
;*****************************************************************
		FDB	$0000			; end of patches

__cmd_ram_end
