;
; Data and time functions
;

__cmd_datetime

CmdSetDT
    lda     #CMD_SET_DATETIME           ; Set data and time
    jmp     >CmdSendStringCmd           ; Go send it!
        
FuncGetDT
    lda		#CMD_GET_DATETIME			; Get date and time
	jsr	    >MMC_SendCmd
	jsr 	>CheckError					; check for errors, returns to basic on error

    jmp     >FuncRetMMCString           ; Return it to basic.



ShowDateTime
    ldx     #RTCMess-1                  ; get address of messgae
    jsr     >TextOutString

    lda		#CMD_GET_DATETIME			; Get date and time
	jsr	    >MMC_SendCmd

    jsr     >GetStrLenA                 ; Get length in A

    pshs    a                           ; save length
    
	lda		#CMD_INIT_READ				; Get date string
	jsr	    >MMC_SendCmdRaw
   
    puls    b                           ; retrieve length

ShowDateNext
	jsr	    >MMC_WaitGetWritten			; Wait for byte, and get it in a
    jsr     >BasicScreenOut             ; send to screen
    decb                                ; decrement count
    bne     ShowDateNext
    
    jsr     >CON_EOL                    ; print eol.

    rts

__cmd_datetime_end
