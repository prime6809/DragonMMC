;
; Inverse a portion of the screen.
;
; 2017-10-02, Phill Harvey-Smith.
;
; Use : EXEC invaddr,pat_pos,count
;

        use	cpudefs.asm
        use	dgndefs.asm
        use WDdefs.asm
        use	romdefs.asm
        use	mmc2def.asm
        use	DragonMMCdef.asm
        use	basictokens.asm
        use	basicdefs.asm

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

Inverse
		jsr		VarCKComma				; Check for comma, SN error if not
		jsr		VarGet16Bit				; Get print pos
		cmpx	#0						; beforer first?
		blo		InvError
		cmpx	#$1FF					; beyond end of screen?
		bhi		InvError				; yep 
		
		leax	$400,x					; turn into screen offset
		
		pshs	x						; save on stack
		
		jsr		VarCKComma				; Check for comma, SN error if not
		jsr		VarGet8Bit				; Get char count (in b)
		
		puls	x
		
InvLoop
		lda		,x						; get char from screen
		cmpa	#$80					; >$80, semigraphic, leave unchanged.
		bhs		NoInv
		eora	#$40					; flip bit
NoInv
		sta		,x+						; resave it
		decb							; decrement count
		bne		InvLoop					; not zero loop again
	
		rts

InvError
		jmp		BasFCError
		
        ifdef   Header
LENGTH  equ     (*-start)	
        endc
        
;
; Note anything below here is **NOT** saved in the binary, use for uninitialized data **ONLY**
;
