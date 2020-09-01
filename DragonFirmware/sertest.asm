;
; Serial test program, for Dragon 64 compatible serial port.
;
; 2019-03-19, P.Harvey-Smith.
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
        jsr	    TextCls			        	; clear screen
	
signon
        leax	mess-1,pcr
        lbsr	TextOutString			    ; signon

		bsr		SerInit
MainLoop:		
		jsr		BasicKbdIn					; scan keyboard
		beq		ScanSer						; check serial
		
		bsr		SerSend
		
ScanSer	bsr		SerIn
		beq		MainLoop
		
		jsr		BasicScreenOut				; send to screen
		bra		MainLoop
		
SerInit
        LDD     #$0A9E			; init ACIA, 9600,8,n,1
        STD     AciaCmd			; 
        LDA     AciaData

		rts
		
SerSend	PSHS    CC,B			; Save regs
        LDB     #$10			; transmit register empty?
LBE9C   BITB    AciaStat		; check it
        BEQ     LBE9C			; keep waiting until transmit finishes
        STA     AciaData		; send next byte
        PULS    CC,B,PC			; restore and return

SerIn	pshs	b
		CLRA
		ldb		#$08						; Char ready?
		BITB    AciaStat					; character received yet?
		LDA		AciaData
		TSTA								; set flag for caller
		puls	b,pc	
	
mess	
        fcc	    "DRAGON SERIAL TESTER"
		fcc		"V1.00"
        fcb	    $0d,0
;		         12345678901234567890123456789012	

CON_WaitKeyL
		pshs	a
CON_WaitKeyLoopL
        jsr		>DBasicKbdIn
CON_TestKey
		beq		CON_WaitKeyLoopL
		puls	a
		rts     
        
LENGTH         equ       (*-start)