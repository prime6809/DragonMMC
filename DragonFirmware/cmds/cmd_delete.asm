;
; cmd_delete.asm : delete the specified file.
;

__cmd_delete

MFileFlagCopy	equ	'C'
MFileFlagRename	equ	'R'
MFileFlagSync	equ	'S'

CmdMDelete
		lbsr	GetSendFileName		; Get filename send to MMC
		leax	DeleteMess-1,PCR	; Prompt the user
		jsr     >TextOutString	
		
                
CmdMDeleteLoop
		jsr		>BasicKbdIn			; Poll keyboard
		beq		CmdMDeleteLoop		; No key pressed loop again
		
		cmpa	#'N'					; No key pressed ?
		beq		CmdMDeleteExit		; yes : exit
		
		cmpa	#'Y'					; Yes key pressed ?
		bne		CmdMDeleteLoop		; no : keep waiting
		
		lda		#CMD_FILE_DELETE	; Delete the file
		lbsr	MMC_SendCmd			; send it
		lbsr	CheckError			; and check for error

CmdMDeleteExit
		clra
		rts
;
; MFile, copy or rename file.
;		
CmdMFile
		cmpa	#MFileFlagCopy		; is it a copy?
		beq		CmdMFileValid		; yep
		cmpa	#MFileFlagRename	; is it a copy?
		beq		CmdMFileValid		; yep
		cmpa	#MFileFlagSync		; is it a Sync?
		beq		CmdMFileSync		; do a sync
		jmp		>BasSNError			; none, error
		
CmdMFileValid
		pshs	a					; save copy / rename flag
		
		jsr     <BasChrGet			; skip op flag.
        jsr		>GetSendFileName	; get first filename, send it
		jsr		>VarCKComma			; check for comma, error if not
		jsr		>GetSendFileName2	; Send the second name
		
		puls	a					; retrieve copy / rename flag
		
		cmpa	#MFileFlagCopy		; Copy ?
		bne		CmdMFileDoRename	; nope skip
		lda		#CMD_FILE_COPY		; do a copy
		bra		CmdMfileDoCmd
		
CmdMFileDoRename
		lda		#CMD_FILE_RENAME	; do a rename

CmdMfileDoCmd
		lbsr	MMC_SendCmd			; send it
		lbsr	CheckError			; and check for error

		clra
		rts

CmdMFileSync
		jsr     <BasChrGet			; skip op flag.
		lda		#CMD_SYNC			; Sync command
		bra		CmdMfileDoCmd		; go do it!
		
__cmd_delete_end
