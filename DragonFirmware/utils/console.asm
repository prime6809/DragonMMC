;
; Console.asm : write things to Dragon screen.
;
; Define MINIMUM to exclude code not needed by FLASH
;
; 2017-03-22, removed all calls to TextOutString, **DO NOT** put any back in
; as they mess with the system area too much.
; use CON_WriteString instead.
;
__console

		ifne	0
CON_WriteHexX
		pshs	d
		tfr		x,d
		bsr		CON_WriteHexWordS	        ; Write x
		puls	d,pc
		
;
; Write hex word in D.
;

CON_WriteHexWord
		pshs	d				            ; save d
		bsr		CON_WriteHexByte	        ; Write MSB
		exg		a,b				            ; get LSB
		bsr		CON_WriteHexByte	        ; Write LSB
		puls	d,pc			            ; restore / return

CON_WriteHexWordS
		bsr		CON_WriteHexWord	        ; Write word
		bra		CON_WriteSpace	            ; write space
		endc
		
;
; Write byte in a as hex followed by space.
;
		
CON_WriteHexByte
		pshs	a
	
		lsra					            ; Move msn to lsn
		lsra	
		lsra	
		lsra	

		bsr		CON_GetHexNibbleA	        ; send msn
		jsr		BasicScreenOut	
	
		lda		,s				            ; retrive digit
		bsr		CON_GetHexNibbleA	        ; send lsn
		jsr		BasicScreenOut	
		puls	a,pc

		ifne	0
CON_WriteHexByteS
		bsr		CON_WriteHexByte	
		
CON_WriteSpace
		pshs	a
		lda		#$20			            ; send space
		jsr		BasicScreenOut	            ; send it
		puls	a,pc
		endc
		
CON_HexDigits
		fcc		/0123456789ABCDEF/
	
CON_GetHexNibbleA
		pshs	x
		anda	#$0F			            ; mask out bytes
	
		leax	>CON_HexDigits,pcr	        ; Point to digits
		leax	a,x				            ; point to needed digit
		lda		,x				            ; **GET** hex digit !
		puls	x,pc

		ifne	0
ASCII_AToHexA
        pshs    x,b
        clrb
        leax    CON_HexDigits,pcr           ; Point to hex digits
ASCII_AToHexALoop
        cmpa    b,x                         ; same digit?
        beq     ValidHex
        incb                                ; next digit
        cmpb    #16                         ; tried all?
        blo     ASCII_AToHexALoop           ; no : loop again
        orcc    #FlagCarry                  ; flag error
        puls    b,x,pc                      ; return it
        
ValidHex
        tfr     b,a                         ; get position into a
        puls    b,x,pc
		
ASCII_XToHexByte
        lda     ,x                          ; Get byte
        bsr     ASCII_AToHexA               ; convert it
        asla                                ; convert to MSN
        asla
        asla
        asla                                
        pshs    a                           ; save it
        lda     1,x                         ; Get byte
        bsr     ASCII_AToHexA               ; convert it
        ora     ,s+                         ; combine with stacked MSN
        rts
        
		endc
		
;
; CON_EOL : Write end of line sequence
;

CON_EOL
		lda		#$0d			            ; EOL
		jmp		BasicScreenOut	            ; send it
		

		ifne	0

CON_WriteHexByteEOL
		pshs	a
		bsr		CON_WriteHexByte
		bsr		CON_EOL
		puls	a,pc
		endc
		
;
; Write a zero terminated string to the screen, does not screw with 
; as many of system vars as rom routine at $90E5
; 
; Entry : X address of ASCIIZ string to print.
;
        
CON_WriteString
        pshs    x,a
CON_WriteStringLoop        
        lda     ,x+                         ; Get character to print
        tsta                                ; Zero : end of string
        beq     CON_WriteStringExit         ; exit
        jsr     BasicScreenOut              ; output it
        bra     CON_WriteStringLoop         ; do next
CON_WriteStringExit        
        puls    x,a,pc
        
        
        
        
		ifndef	MINIMUM
		ifne	0
CON_WaitKey
		pshs	a
CON_WaitKeyLoop
		jsr		BasicKbdIn		
		beq		Con_WaitKeyLoop
		puls	a,pc
		
		endc
;
; Write a version number in a as msn.lsn followed by space.
;

CON_NdotN
		pshs	a
	
		lsra					            ; Move msn to lsn
		lsra	
		lsra	
		lsra	

		bsr		CON_GetHexNibbleA           ; send msn
		jsr		BasicScreenOut	

		lda		#'.				            ; send dot
		jsr		BasicScreenOut	

		lda		,s				            ; retrive digit
		bsr		CON_GetHexNibbleA           ; send lsn
		jsr		BasicScreenOut	
	
		lda		#$20			            ; send space
		jsr		BasicScreenOut	            ; send it
	
		puls	a,pc
		
;
; CON_PromptMore : display a prompt and wait for a key
;				   Key pressed returned in a
; 

CON_PromptMore
		leax	MoreMess,PCR				; Prompt for more
		JSR     >CON_WriteString
CON_PromptMoreWait		
		jsr		>BasicKbdIn					; Poll keyboard
		beq		CON_PromptMoreWait			; No key pressed loop again

		rts
		
		ifne	0
;
; CON_DumpRegs : Dump the registers.
;
; After the push (and set of wait flag) the stack will be....
;
; PCL	from BSR/JSR
; PCH	S+11
; UL	From pshs
; UH	S+9
; YL
; YH	S+7
; XL
; XH	S+5
; DP	S+4
; B		S+3
; A		S+2
; CC   	S+1
; WFLAG S+0

CON_DumpRegs
		pshs	u,y,x,dp,b,a,cc				; pushed in this order
		clra								; flag don't wait for keypress

CON_DumpRegsCommon
		pshs	a							; save wait flag

		leax	RegNames,pcr				; print reg names
		JSR     >CON_WriteString
		
		ldd		11,s						; Get PC
		lbsr	CON_WriteHexWordS			; display it

		lda		2,s							; get A
		lbsr	CON_WriteHexByteS			; display it
		
		lda		3,s							; get B
		lbsr	CON_WriteHexByteS			; display it
		
		ldd		5,s  						; Get X
		lbsr	CON_WriteHexWordS			; display it
		
		ldd		7,s							; Get Y
		lbsr	CON_WriteHexWordS			; display it
			
		ldd		9,s							; Get U
		lbsr	CON_WriteHexWordS			; display it
	
		lda		4,s							; get DP
		lbsr	CON_WriteHexByteS			; display it
	
		lda		1,s							; get CC
		lbsr	CON_WriteHexByteS			; display it
	
		puls	a							; get wait flag
		beq		CON_DumpRegsExit			; Don't wait exit

CON_DumpRegsLoop	
		bsr		CON_WaitKey					; wait for a keypress
CON_DumpRegsExit	
		puls	u,y,x,dp,b,a,cc
		rts

CON_DumpRegsWait		
		pshs	u,y,x,dp,b,a,cc				; pushed in this order
		lda		#$FF						; flag wait for keypress
		bra		CON_DumpRegsCommon

		endc
;
; Get maximum of B bytes of input into buffer at X
;

CON_InputBufXB
        decb
        pshs    b
        clrb
        
CON_InputBufXBLoop
        pshs    x,b
        jsr     >BasicCursorB               ; Blink cursor
        jsr		>BasicKbdIn				    ; Read keyboard
        puls    x,b
        
        cmpa    #0
        beq     CON_InputBufXBLoop          ; keep looping until keypressed
        
        cmpa    #$0d                        ; Enter pressed?
        beq     CON_InputBufXBEnd
        
        cmpa    #$08                        ; Backspace?
        beq     CON_InputBufXBBack          ; yep : act on it
        
        cmpb    ,s                          ; Past end of buffer?
        bhs     CON_InputBufXBLoop          ; yep : do nothing
        
        sta     b,x                         ; save in buffer
        incb                                ; increment count
        
CON_InputBufXBOut
        jsr     >BasicScreenOut             ; output character
        bra     CON_InputBufXBLoop          ; go again
        
        
CON_InputBufXBBack
        cmpb    #0                          ; at beginning of buffer?
        beq     CON_InputBufXBLoop          ; Yep do nothing
        
        decb                                ; decrement pointer
        pshs    x                           ; save x
        ldx     <TextVDUCursAddr            ; get cursor pos
        leax    -1,x                        ; previous char
		stx     <TextVDUCursAddr            ; update it
        lda		#' '
        jsr     >BasicScreenOut             ; output character
        stx     <TextVDUCursAddr            ; reset pointer
        puls    x
        
        bra     CON_InputBufXBOut           
        
CON_InputBufXBEnd
        clr     b,x                         ; Terminate string
        puls    b,pc
        
;
; Clear text screen line specified in B
;
CON_ClearLineB
        pshs    d,x
        
        lda     #TextLineLen                ; line length
        mul                                 ; calculate offset
        ldx     #TextScreenBase             ; point to screen
        leax    d,x                         ; add offset
        stx     <TextVDUCursAddr            ; Set cursor address to beginning of line
        
        lda     #$60                        ; VDG space :)
        clrb
CON_ClearLineBLoop
        sta     b,x                         ; blank it
        incb                                ; next
        cmpb    #TextLineLen                ; done a line ?
        blo     CON_ClearLineBLoop          ; nope keep going
        
        puls    d,x,pc
        
;
; Clear line B and print string at X
;
CON_ClearBPrintX
        bsr     CON_ClearLineB              ; clear line
        lbsr    CON_WriteString             ; print it
        rts

       
;
; Acorn style inline print where string to be printed follows jsr/bsr
;
        ifne    DEBUG
        ifne	0
CON_InlinePrint
        pshs    x,d                         ; PC now at s+4
        ldx     4,s                         ; Get PC of return address
        
CON_InlinePrintLoop
        lda     ,x+                         ; get a byte from string
        beq     CON_InlinePrintEnd          ; zero end of sting : exit
        jsr     >BasicScreenOut
        bra     CON_InlinePrintLoop         ; loop again!
CON_InlinePrintEnd
        
        stx     4,s                         ; Update return address
        puls    d,x,pc
        endc
        endc
        
		endc
		
__console_end
