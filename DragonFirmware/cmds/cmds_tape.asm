;
; cmds_tape.asm : Cassette emulation commands.
;

__cmd_tape

;
; Note some routines make calls / returns into the Dragon and CoCo roms.
; Please remember to define them so they assemble correctly on *BOTH* 
; machines.
; 
		ifdef	Dragon
CloadMEntry		EQU		$A095
CloadEntry		EQU		$B6E1
ClearEntry		EQU		$8424
CloseFilesEntry	EQU		$B663
BasicLoop		EQU		$849F
		else
CloadMEntry		EQU		$A502
CloadEntry		EQU		$A4A3
ClearEntry		EQU		$AE6F
CloseFilesEntry	EQU		$A42D
BasicLoop		EQU		$AD9E
		endc
        
ScannerExit     EQU     $00A8    


FTBasic	equ		0
FTCode	equ		2


CmdMMount
		tfr		a,b						; keep current next char
		jsr		<BasChrGet				; skip first char 	
	
		cmpb	#'R'					; MMountR ?
		beq		CmdMRMount				; Yep insert, read only
	
		cmpb	#'W'					; MMountW ?
		beq		CmdMWMount				; Yep insert, write only
	
		cmpb	#'C'					; MMountC ?
		beq		CmdMUNMount				; Yep, unmount
	
		jmp		>BasSNError				; no : ?SN Error
		
;
; Mount a cas file for input.
;
; syntax :
;	mrmount "filename"
;

CmdMRMount
CmdMRMountEntry	
		lda		#CAS_FILE				; open default file
		lbsr	MMC_WaitPutLatchRead	; send file id to latch register

		lda		#CMD_FILE_OPEN_READ		; Open file
		
CmdMMountCommon
		lbra	CmdSendFilenameCmd

;
; Mount a cas file for output.
;
; syntax :
;	mwmount "filename"
;

CmdMWMount
		lda		#CAS_FILE				; open default file
		lbsr	MMC_WaitPutLatchRead	; send file id to latch register

		lda		#CMD_FILE_OPEN_WRITE	; Open file
		bra		CmdMMountCommon			; rest same as rmount

;
; Unmount a cas file, closing it and flushing any output.
;

CmdMUNMount
;		jsr		<BasChrGet				; skip first char 		
		lbsr	MMC_CloseFile			; Close tape file
		lbsr	CheckError				; Check for error
		clra							
		rts

;
; Get the filetype of the next file on (emulated) tape
; 
; Syntax :
; 	X=CFTYPE


FuncCFType
        cmpa    #'E'                    ; Exec: get exec address
        beq     GetCFExecD
        
        cmpa    #'L'                    ; Load, get load address
        beq     GetCFLoadD
        
		bsr		GetCFTypeA				; get cas filetype in A

		tfr		a,b						; move to b	
		jmp		VarAssign8Bit			; return it to basic


GetCFTypeA
		lda		#CMD_CAS_FTYPE			; Get file type
		lbsr	MMC_SendCmd				
		lbsr	CheckError				; Bomb out on error
		
		lda		#CMD_INIT_READ			; init read
		lbsr	MMC_SendCmdRaw
		lbsr	MMC_WaitGetWritten		; Wait for byte, and get it in a
		rts

;CFTypeE
;        bsr     GetCFExecD                  ; Get exec address
;        jmp     VarAssign16Bit2             ; assign it and return
;        
;        
;CFTypeL      
;        bsr     GetCFLoadD                  ; get load address
        
;
; Get exec address for next cassette file in D
;
GetCFExecD      
        ldb     #14                     ; Exec address offset
        bra     GetFC16
;
; Get load address for next cassette file in D
;

GetCFLoadD      
        ldb     #16                     ; Load address is 15 bytes in

GetFC16
        jsr     <BasChrGet				; skip first char, B un affected 	
        bsr     GetCFTypeA              ; Get filetype, header block in AVR buffer

GetCFLoadDLoop        
        lbsr    MMC_WaitGetWritten      ; get a byte
        decb                            ; decrement count
        bne     GetCFLoadDLoop          ; Keep going until at right place
        
        tfr     a,b                     ; get msb
        lbsr    MMC_WaitGetWritten      ; get second byte of Load address
        
        exg     a,b        

;
; Assign an unsigned 16 bit value from the D register
; call as you would VarAssign16Bit or VarAssign16Bit2
;

DO_Assign16
		CLR     <BasVarType
        STD     <BasVarAssign16
        JMP     >VarAssign16BitB
;
; Enable tape emulatiom, open, and load a tape using cload/cloadm as needed.
;

CmdTapeRun
		lbsr	CmdMCasNormal			; boot into RAM mode
		lbsr	CmdMRMountEntry			; Open the file

		jsr		>DO_ResetPoweron		; reset memory map.

        ldx     #(TextScreenBase+TextLineLen) ; point to screen, beginning of line 2
        stx     <TextVDUCursAddr        ; Set cursor address 
		
        bsr		GetCFTypeA				; get cas filetype in A

		cmpa	#FTBasic				; Basic program ?
		beq		CmdTapeRunLBas			; yes load it
		
		cmpa	#FTCode					; Machine code ?
		beq		CmdTapeRunCode			; yes load it
		
		jmp		BasFCError				; nope : error 
		
CmdTapeRunLBas		
		CLR     <CasStatus
		leas	2,s
		
		jmp		CloadEntry				; cload the file, this never returns!

; Emulated CLOAD command patched to come back to here if loading ASCII Basic!

CLoadRunASCII
		leas	-2,s					; drop return address
		jsr		CloseFilesEntry			; Close all files
		bra		CLoadRunRun				; Go run it
		
; Emulated CLOAD command patched to come back to here !
CLoadRun
		jsr		>BasVect1				; Reset basic memory
		jsr		>BasVect2				; Reset basic memory
        
CLoadRunRun
		clra
		jmp		BasRun
		
CmdTapeRunCode
        bsr     GetCFLoadD              ; get load address in d
        pshs    d                       ; save it
                
		CLR     <CasStatus
		jsr		CloadMEntry				; cloadm the file
        
        puls    d                       ; recover load address

;
; Check to see if the IRQ vector has been changed to point to the loaded area?
; Some software does this to auto-run, notably "Facemaker" by DragonData.
;
        
        cmpd    SecVecIRQ+1             ; load > SecIRQ ?           
        bhi     CmdTapeRunCodeCheckExec ; lower, check exec address                  
        
        ldx     SecVecIRQ+1             ; get IRQ vector
        
        cmpx    <CasIOBuffAddr          ; Vector below end load address ?
        blo     RunViaIRQ               ; yep it's in loaded area, just exit.
 
        
;
; Check to see if EXEC address is within the loaded area, if so call it to run the code.
;
        
CmdTapeRunCodeCheckExec
        cmpd    <BasExecAddr            ; Load address below Exec address?
        bhi     CmdTapeRunCodeExit      ; Exec < Load, invalid exit
        
        ldx     <BasExecAddr            ; get exec address
        cmpx    <CasIOBuffAddr          ; Exec address below end load address ?
        bhs     CmdTapeRunCodeExit      ; Exec > Last loaded, invalid exit
          
        jmp     ,x                      ; call it

;
; Some auto-runners catch the line scanner, call it to try and catch them.
; Notably "FlagonBird".
;
CmdTapeRunCodeExit        
        jsr     <ScannerExit            ; Jump to last line of scanner, catch some auto-runs

RunViaIRQ
		jmp		BasicLoop				; jump to interpreter.
;		rts

;
; Rewind the emulated tape
;
CmdRewind
		lda		#CMD_REWIND				; Rewind tape file back to 0

		lbsr	MMC_SendCmd				; Send Command
		lbsr	CheckError				; Bomb out on error

		clra
		rts

;
; Mirror incoming tape to MMC.
;		


CmdMMirror
        cmpa    #'A'                    ; Auto flag?
        beq     CmdMMirrorA             ; yep deal with it
        
        clra    
        pshs    a                       ; save auto flag
		bra     CmdMMirrorCommon        ; Flag not auto
CmdMMirrorA
        pshs    a                       ; save auto flag
		jsr		<BasChrGet				; skip first char 	
CmdMMirrorCommon
        jsr		>CmdRamBoot				; Copy ROM into RAM.

		lda		#D_RAM_ENABLE+D_RAM_WP	; put us in ram mode, write protected.
		ldx		#MirrorPatches			; Point to our patches.
		jsr		>CmdMcasGo2				; go copy rom and patch it.

		lda		#CAS_FILE				; open default file
		lbsr	MMC_WaitPutLatchRead	; send file id to latch register

		lda		#CMD_FILE_OPEN_STREAMW	; Open file for streaming to
		jsr		>CmdSendFilenameCmd		; Send it and init streaming
        puls    a                       ; recover auto flag
        cmpa    #'A'                    ; Auto?
        beq     CmdMMirrorAuto          ; yep : do it.
        
		clra
		rts

;
; Turn on motor and audio.
; Go into an endless loop that does block read to a constant buffer.
; this will of course then be mirrored to the cas file.
;        
CmdMMirrorAuto
        jsr     >CasMotorOn             ; Turn on motor
		clrb							; B must be zero !
        jsr     >CasAudioOn             ; turn on audio

CmdMMirrorAutoLoop        
        ldx     #$600                   ; use graphics screen
        stx     CasIOBuffAddr

        jsr     >CasBlockIn             ; Read block, ignore any error
        bra     CmdMMirrorAutoLoop      ; loop again!
        
;
; This replaces the byte in routine in the Dragon / CoCo roms, it's exactly the same code
; with the addition of a save at the end to send the read byte to the MMC
;		
NewByteIn
		LDA     #$08					; Read 8 bits
        STA     <CasBitCount
NewByteInLoop							
		JSR     >CasBitIn 				; read a bit
        RORA							; rotate it into A
        DEC     <CasBitCount			; decrement count
        BNE     NewByteInLoop			; keep going if not all done.
		sta		D_WRITE_DATA_REG		; send it to the AVR, don't wait
        RTS

;
; Make sure beggining of block mark is copied to the output
;
NewFoundBlkMark
		pshs    a
        lda     #'U
        sta		D_WRITE_DATA_REG   	    ; send it to the AVR, don't wait
		puls    a
        sta		D_WRITE_DATA_REG   	    ; send it to the AVR, don't wait
		
		ifdef	Dragon		
		bra		NewByteIn				; get next byte
		else
		bsr		NewByteIn				; get next byte
		sta		<CCasBlockType			; save it
		rts
		endc
		
			ifdef 	Dragon
CMP2Addr	EQU		$B94D
			else
CMP2Addr	EQU		$A719
			endc
		
		
MirrorPatches

		FDB		CasByteIn				; Byte in routine
		FCB		CasMP1E-CasMP1			; length
CasMP1
		JMP		>NewByteIn				; Jump to our copy of byte in
CasMP1E

		ifdef	Dragon
		FDB		CMP2Addr
		FCB		CasMP2E-CasMP2			; length
CasMP2
		JSR		>NewFoundBlkMark		; Jump to our copy of byte in
CasMP2E
		
		else
		FDB		CMP2Addr
		FCB		CasMP2E-CasMP2			; length
CasMP2
		JSR		>NewFoundBlkMark		; Jump to our copy of byte in
		NOP
CasMP2E
		
		endc

;*****************************************************************
;******************[ End of Patches ]*****************************
;*****************************************************************
		FDB	$0000			; end of patches
		
__cmd_tape_end

