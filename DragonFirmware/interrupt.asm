;
; New NMI handler, for 'Snap' button on DragonMMC.
;

;
; Initialize the interrupt vector area at the top of memory
;

__interrupt

MenuLoc	        equ		TextScreenBase+(14*TextLineLen)			; Address of NMI menu

FileNameLen     equ     9               ; Filename length including terminating 0
TempS           equ     $500            ; Tempory S whilst loading image 

LoadBlock1      equ     $0000           ; beginning of first image load block 
LoadBlock1a     equ     $0600           ; Top of text screen
LoadBlock2      equ     $4000           ; beginning of second image load block 
LoadEnd         equ     $8000           ; End of load area (beginning of ROM)

InterruptInit
		pshs	cc						; save ccr
		orcc	#FlagIRQ+FlagFIRQ		; disable interrupts
		
        leax    NewNMI,pcr              ; get new vector
        stx     SecVecNMI+1             ; set it up
        lda     #$7E                    ; JMP opcode
        sta     SecVecNMI               ; fill in jump
        
;		ldx     #MonStart               ; Set SWI->mon
;        stx		SecVecSWI+1				; Temp point SWI at it so we can test in MESS!
;		sta		SecVecSWI
		
        ldb     D_RAM_CTRL              ; Get ram control       
		andb	#~(D_NMI_ENABLE+D_RAM_VEC+D_RAM_VEC_WP) ; Disable NMI + Select rom vectors, remove write prot on RAM vectors
		stb		D_RAM_CTRL
;
; Copy hardware interrupt vectors from ROM to RAM, as writes always go to RAM, no need to swap.
;
		lda		#HWVecCount				; No of vectors
		ldx		#HWVecBase				; point to base
IntCopyLoop
		ldy		,x						; get vector
		sty		,x++					; put it in RAM 
		deca 							; decrement counter
		bne		IntCopyLoop
	
        ldx     #NewNMI                 ; set newnmi
        stx     HWVecNMI                
    
;        ldx     #MonStart               ; Set SWI->mon
;        stx     HWVecSWI
    
        orb		#D_NMI_ENABLE+D_RAM_VEC+D_RAM_VEC_WP	; Enable NMI routing + RAM vectors, write protect RAM vectors.
		stb		D_RAM_CTRL
		       
		puls	cc,pc					; restore CC (and int flags if enabled) + return
        
;
; We are in the NMI handler, and therefore the registers of the 
; currently running program are saved on the stack, so we are free 
; to use them as long as S is the same at exit. 
;
NewNMI
        lda     D_RAM_CTRL              ; Disable NMI so we don't get called recursively
        anda    #~D_NMI_ENABLE       	; Mask out NMI enable
        sta     D_RAM_CTRL
        
		jsr		>Util_Select			; Select correct ROM version
        leau    NMI_SavePIA,pcr         ; save PIA register contents on stack
        exg     pc,u                    ; call subroutine without using stack!

		ldd     D_SAMBITS_MSB           ; get sambits into D
		pshs	D						; save on stack

		ldd		<TextVDUCursAddr		; Save cursor address
		pshs	d
		
        clra                            ; DP=0, as ROM routines assume this.
        tfr     a,dp
        
        jsr     >TextResetVDU           ; Go to text mode
        
		ldx		#MenuLoc				; Beginning of line #14
		stx		<TextVDUCursAddr		; set cursor address
		
SaveTextLoop
		ldd		,x++					; Get bytes from screen
		pshs	d						; save on stack
		cmpx	#$600					; end of screen?
		blo		SaveTextLoop			; keep going
		
		leax	NMIMenu1,pcr			; point to menu text
		leay	NMIMenuTable1,pcr		; Keycodes & jump table
		bsr		NMIMenu
     
NMI_Exit

		ldx		#$600					; end of screen
RestoreTextLoop
		puls	d						; get bytes
		std		,--x					; save on screen
		cmpx	#MenuLoc				; done all?
		bhi		RestoreTextLoop			; nope keep going

		puls	d						; restore cursor address
		std		<TextVDUCursAddr
		
		puls	D						; restore sambits
		lbsr	SetSambits				; set them
		
        leau    NMI_RestorePIA,pcr      ; save PIA register contents on stack
        exg     pc,u                    ; call subroutine without using stack!

        bsr     NMI_Enable
        rti                             ; return.

NMI_Enable
        lda     D_RAM_CTRL              ; Reenable NMI
        ora     #D_NMI_ENABLE           ; Enable NMI routing
        sta     D_RAM_CTRL              ; update it.
        rts

NMIMenu
		ldb     #15                     ; clear bottom two lines
        jsr     >CON_ClearLineB         ; line 15
        decb    
        jsr     >CON_ClearLineB         ; line 14
        
        pshs	x,y						; save pointer to text & table
        jsr     CON_WriteString
WaitKey
		jsr		BasicKbdIn				; Read keyboard
		cmpa	#0						; Key pressed?
		beq		WaitKey					; nope loop again
		
check_next
		cmpa	,y						; Check key from table?
		bne		try_next				; no try next
		
		pshs	a,y						; save on stack
		jsr		[1,y]					; Jump to handler

		puls	a,y						; restore
		
try_next
		leay	3,y						; next entry in table
		tst		,y						; last entry?
		bne		check_next				; nope loop again
		
		ldy		1,y						; recover pointer to table beginning
		bra		WaitKey					; invalid key keep going!

NMIMenuExit
		leas	5,s						; drop return address & saved a,y
		puls	x,y,pc					; restore and return

     
		
NMICold
		clr		WarmStartFlag			; clear warm start flag
        lda     D_RAM_CTRL              ; Get RAM control reg
        anda    #D_ROM_A14              ; Reset all bits *EXECPT* rom select           
        sta     D_RAM_CTRL              

; Clear the RAM on cold boot.
        
        clra                            ; D=0, X=0
        clrb
        
        tfr     d,x
NMIClearLoop
        std     ,x++                    ; save it
        cmpx    #$8000                  ; end of ram
        blo     NMIClearLoop
        
        bra     NMIDoReset
NMIWarm
        lda     #$55                    ; Make sure it's a warm start
        sta     WarmStartFlag			
NMIDoReset
		bsr     NMI_Enable              ; We have to re-enable here for warm start
        JMP		[HWVecReset]			; Jump to reset vector
		
;
; Save a dump of memory to card, prompts for filename and dumps memory to that file.
; not yet error checked, and will overwrite exiting files.
;
        
NMISave
        bsr     NMIGetSendFileName      ; Get filename, send to AVR
        
		lda		#CAS_FILE				; file id of cas file
		lbsr	MMC_WaitPutLatchRead	; send it to latch register

		lda		#CMD_FILE_OPEN_SNAPW	; Open snapshot file for write file
		lbsr	MMC_SendCmd		        ; Open file

        tfr     s,d                     ; get copy of stack pointer
        jsr     >MMC_InitSendBytes     	; send S to file
        jsr     >MMC_WriteD             ; Write S to file

        lda     #2                      ; 2 bytes
        jsr     >MMC_WriteAFromAVRBuf   ; Write to file.

        ldx     #LoadBlock1             ; start of RAM
        
NMISaveLoop
        clrb                            ; 256 bytes
		jsr     >MMC_WriteFileBlock     ; write to MMC
        
        cmpx    #LoadEnd                ; End of RAM?
        blo     NMISaveLoop             ; no do next block
        
        lbsr	MMC_CloseFile		    ; close the file

        ldb     #15                     ; Clear last line
        jsr     >CON_ClearLineB         
        
		rts

NMIGetSendFileName
        ldb     #15                     ; Line 15
        leax    FilePrompt,pcr          ; Point to prompt
        jsr     >CON_ClearBPrintX       ; print it
        
        leas    -FileNameLen,s          ; Make room on stack
        
        leax    ,s                      ; get pointer to buffer
        ldb     #FileNameLen            ; filename length
        jsr     >CON_InputBufXB         ; go read it
        
        jsr     >MMC_SendTillZero       ; send filename to AVR
        
        leas    FileNameLen,s           ; discard buffer
        
        leax    SnapExt,pcr             ; point to extension
        ldb     #4                      ; 4 bytes in extension
        jmp     >MMC_SendName2          ; send the name

;
; Load a snapshot file from card, will need to load the file in parts
; so as to avoid over-writing stack.
; Proposed method is : 
;   check / move** stack to top 16K of RAM, then load in the first 16K.
;   move stack pointer to the screen area at $500, and load the top 16K.
;   read the real stack pointer loaction from the file (first 2 bytes) and set it.
;   re-read the screen ram at $400.
;   return from the load routine which will return into the snap menu. 


NMILoad
        bsr     NMIGetSendFileName      ; Get filename, send to AVR
        
		lda		#CAS_FILE				; file id of cas file
		lbsr	MMC_WaitPutLatchRead	; send it to latch register

		lda		#CMD_FILE_OPEN_SNAPR    ; Open snapshot for read 
		lbsr	MMC_SendCmd		        ; Open file
        bmi     NMILoadError            ; error opening file
NMILoadEntry               
        cmps    #LoadBlock2             ; Is stack in top of RAM?
        bhs     NMIStackHigh            ; yes : do nothing
        lds     #LoadEnd-1              ; no : move to top of ram
        
NMIStackHigh        
        lbsr    MMC_ReadDFile           ; Read saved S (skip past it)
        pshs    d                       ; Save stapshot's S
        
; Load the first 16K of the image        
        ldx     #LoadBlock1             ; begin loading at bottom of RAM
NMILoadLoop1
        clrb                            ; flag 256 bytes
        jsr     >MMC_ReadFileBlockRaw   ; read a block
        cmpx    #LoadBlock2             ; top of first 16K
        bne     NMILoadLoop1            ; no : keep loading

; We have loaded the first 16K of the image, move stack pointer to $500        
        puls    d                       ; recover saved snapshot S
        cmpd    #LoadBlock2             ; in already laded RAM ?
        bhs     NMITempS                ; no : use temp stack in screen ram
        tfr     d,s                     ; yes : make it active
        bra     NMIDoBlock2             ; Go load upper ram
        
NMITempS        
        lds     #TempS                  ; setup temp stack
NMIDoBlock2        
        ldx     #LoadBlock2             ; begin loading at bottom of RAM
NMILoadLoop2
        clrb                            ; flag 256 bytes
        jsr     >MMC_ReadFileBlockRaw   ; read a block
        cmpx    #LoadEnd                ; top of first 16K
        bne     NMILoadLoop2            ; no : keep loading
        
; we now have loaded the whole image, except the text screen ram which we used as
; a stack. We need to position the stack pointer at the saved stack pointer and
; re-load the screen ram from $400-$500        
		lda		#CAS_FILE				; file id of cas file
		lbsr	MMC_WaitPutLatchRead	; send it to latch register
		
        lda     #CMD_REWIND             ; rewind file to beginning
        jsr     >MMC_SendCmd            
        
        lbsr    MMC_ReadDFile           ; Read saved S        
        tfr     d,s                     ; setup loaded stack pointer

        cmpd    #LoadBlock2             ; in bottom of RAM?
        blo     NMINoBlock3             ; yes no need to re-read bottom of RAM

        ldx     #LoadBlock1             ; begin loading at bottom of RAM
NMILoadLoop3
        clrb                            ; flag 256 bytes
        jsr     >MMC_ReadFileBlockRaw   ; read a block
        cmpx    #LoadBlock1a            ; top of text screen
        bne     NMILoadLoop3            ; no : keep loading

NMINoBlock3 

        lbsr	MMC_CloseFile		    ; close the file
        lbsr    InterruptInit           ; Reinit our vectors as they may be different in image
     
        leas    11,s                    ; Drop bytes saved by menu etc
        jmp     NMI_Exit                ; exit NMI back to loaded program
        
NMILoadError
        pshs    a                       ; save error code
        ldb     #15                     ; Line 15
        leax    ErrorMess1,pcr          ; Point to message
        jsr     >CON_ClearBPrintX       ; print it
        
        puls    a                       ; retrieve error code
        jsr     >CON_WriteHexByte        ; write it.
        
        leax    ErrorMess1,pcr          ; Point to message
        jsr     CON_WriteString         ; print it
        
        rts
        
NMIUtil
		rts
			
;
; On entry D contains SAM bits to set.
;	
; We only set the graphics mode and offset bits, we should not need to touch
; the memory type & CPU speed as they should not have been changed.
;
SetSambits
		ldx		#SAMBase				; point at base of SAM registers

SetSambitsLoop		
		lsra							; bottom bit into carry
		rorb							; carry-> top bit, bottom bit into carry
		bcs		SetSambit				; yes set it
		clr		,x						; Clear sam bit
		bra		NextBit
SetSambit
		clr		1,x						; set sambit
NextBit
		leax	2,x						; next bit
		cmpx	#SAMCP1					; done all?
		blo		SetSambitsLoop			; no : go again
		rts
		
;
; Save PIA registers on stack :
;
; CRA           <- highest address
; DDRA
; PA
; CRB
; DDRB
; DB            <- lowest address
;
; This is repeated for PIA2
;
NMI_SavePIA
        ldx     #PIA0DA                 ; point to PIA0

NMI_SavePIALoop
        lda     1,x                     ; get control register A side
        pshs    a                       ; save it
        
        anda    #$FF-PIACRDDR           ; select DDR
        sta     1,x                     
        ldb     ,x                      ; Get DDR
        pshs    b
        
        ora     #PIACRDDR               ; select data register
        sta     1,x
        ldb     ,x
        pshs    b
        
        lda     3,x                     ; get control register A side
        pshs    a                       ; save it
        
        anda    #$FF-PIACRDDR           ; select DDR
        sta     3,x                     
        ldb     2,x                     ; Get DDR
        pshs    b
        
        ora     #PIACRDDR               ; select data register
        sta     3,x
        ldb     2,x
        pshs    b
        
        leax    $20,x                   ; do next PIA
        cmpx    #$FF40                  ; Done both ?
        blo     NMI_SavePIALoop         ; no go again.
        
        exg     pc,u                    ; return to caller

        
NMI_RestorePIA
        ldx     #PIA1DA                 ; point to PIA1

NMI_RestorePIALoop
        lda     3,x                     ; get control register A side

; Restore B side
        ora     #PIACRDDR               ; select Data reg
        sta     3,x                     
        puls    b                       ; restore it
        stb     2,x
        
        anda    #$FF-PIACRDDR           ; select DDR register
        sta     3,x
        puls    b                       ; restore it
        stb     2,x
        
        puls    a                       ; Restore CRB
        sta     3,x

;Restore A side
        lda     1,x                     ; get control register A side

        ora     #PIACRDDR               ; select Data reg
        sta     1,x                     
        puls    b                       ; restore it
        stb     ,x
        
        anda    #$FF-PIACRDDR           ; select DDR register
        sta     1,x
        puls    b                       ; restore it
        stb     ,x
        
        puls    a                       ; Restore CRA
        sta     1,x
   
        leax    -$20,x                  ; do next PIA
        cmpx    #$FF00                  ; Done both ?
        bhs     NMI_RestorePIALoop      ; no go again.
        
        exg     pc,u                    ; return to caller

;
; Data, menu tables messages etc.
;
    
NMIMenu1
;				 01234567890123456789012345678901
		FCC		'wARM cOLD sAVE lOAD uTIL ExIT'
		FCB		$0d,$00

FilePrompt
        FCC     'FILENAME >'
        FCB     $00

;
; Use different extensions for Dragon and CoCo so we don't inadvertently load a snapshot
; on the wrong platform.
;
SnapExt
        ifdef   Dragon
        FCC     '.SSD'
        else
        FCC     '.SSC'
        endc
        
ErrorMess1
        FCC     'ERROR: '
        FCB     $00

		ifne	0
ErrorMess2
        FCC     'OPENING FILE'
        FCB     $00
		endc
		
NMIMenuTable1
		FCB		'W'						; Warm start
		FDB		NMIWarm
		
		FCB		'C'						; Cold start
		FDB		NMICold
		
		FCB		'S'						; Save snapshot
		FDB		NMISave
		
        FCB		'L'						; Load snapshot
		FDB		NMILoad
		        
		FCB		'U'						; Utils
		FDB		NMIUtil
		
		FCB		'X'						; Exit
		FDB		NMIMenuExit
		
		FCB		0						; end of table
		FDB		NMIMenuTable1			; pointer back to beginnig of table

__interrupt_end
