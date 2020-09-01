;
; cmds_config.asm Get/Set config.
;

;
; If one byte supplied then set config for this platform, else if two 
; bytes supplied, then set for the supplied platform plat,value
;	
; SetCfg configbyte,<platform>
;	

__cmd_config

CmdSetCFG
		cmpa	#'S						; Setbit ?
		beq		CmdSetCFGSetBits
		cmpa	#'C						; Clearbit ?
		beq		CmdSetCFGClearBits

; Come here if setting directly		
		bsr		CmdSetCFGGetParm		; Get params
CmdSetCFGSetByte		
		bsr		SetCFGinAB				; set config byte
		clra
		rts

CmdSetCFGSetBits
		JSR     <BasChrGet				; skip	
		
		bsr		CmdSetCFGGetParm		; Get params in 
		bra		SetCFGBitsAB			; Go set them
		
CmdSetCFGClearBits		
		JSR     <BasChrGet				; skip	
		
		bsr		CmdSetCFGGetParm		; Get params in 
		bra		ClearCFGBitsAB			; Go set them
		
CmdSetCFGGetParm		
		jsr		>VarGet8Bit				; Get first byte from basic
		jsr		>CkComma				; is there another param?
		bne		CmdSetCFGDefault		; no assume current platform
		
		pshs	b						; save value
		jsr		>MGetCommaThen8Bit		; yes: go get platform

		puls	a						; recover value
        exg		a,b						; get platform in a, value in b
		bra		CmdSetCFGGetParmEnd		; Go set it

CmdSetCFGDefault
		bsr		GetPlatform				; go get platform
CmdSetCFGGetParmEnd
		rts
		
        
SetCfgDefaultB
        bsr     CmdSetCFGDefault        ; go set it
        bra		CmdSetCFGSetByte
	
;
; GetCfg (<platform>)
;	
		
FUNCGetCFG
		jsr		>CkOpenBrac				; Any parameters?
		bne		FUNCGetCFGDef
		
		jsr		>VarCKOpBrac			; Skip bracket
		jsr		>VarGet8Bit				; Get platform byte from basic
		pshs    b
        jsr		>VarCKClBrac			; Close bracket, error if not
		puls    a
        
        cmpa    #$FF                    ; flag to get ID rather than cfg byte
        bne     FUNCGetCFGID
        bsr     GetPlatform             ; get platform
        bra		FUNCGetCFGAssign		; and assign 
        
FUNCGetCFGID
		bsr		GetCFGinA				; go get it
		bra		FUNCGetCFGAssign		; and assign 
FUNCGetCFGDef
		bsr		GetDefCFGinA			; get config byte
FUNCGetCFGAssign
		tfr		a,b
		jmp		>VarAssign8Bit			; return it to basic
				
__cmd_config_end

