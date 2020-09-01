;
;
; CmdCat.asm
;
; Catalog the MMC.
;

__cmd_cat

	ifdef		Dragon
VarReturnStr	EQU	$8DE1
	else
VarReturnStr	EQU	$B69B
	endc

LinesPerPage	equ	15					; line per page

CmdCat
	leas	-2,s						; make room on stack
	sta		1,s							; save paged flag
	cmpa	#'P							; Paged ?
	bne		CmdCatNonPaged				; nope skip
	JSR     <BasChrGet					; skip the P

	lda		#LinesPerPage+1				; set lines to next prompt
	sta		,s
	bra		CmdCatGo
	
CmdCatNonPaged
	clr		,s							; flag not paged
	clr		1,s

CmdCatGo
	bsr		InitDirRead					; Shared by cat and findfirst
	
CmdCatLoop
	lda		#CMD_DIR_READ				; get next entry
	lbsr	MMC_SendCmd
	lbsr	CheckError					; check for errors, returns to basic on error
	
	cmpa	#STATUS_COMPLETE+STATUS_LAST	; Listed all entries ?
	bne		CmdCatPrintNext				; nope : print next entry
CmdCatEnd
	leas	2,s							; drop stack bytes
	clra	
	rts		

CmdCatPrintNext
	tst		1,s							; Paged mode ?
	beq		CmdCatPrintNext1			; nope, just print
	
	dec		,s							; decrement counter
	bne		CmdCatPrintNext1			; If not done LinesPerPage lines keep printing 
	
	lda		#LinesPerPage				; re-initialize counter
	sta		,s
	
	lbsr	CON_PromptMore				; Display prompt and wait for key
	cmpa	#'Q							; Quit?
	beq		CmdCatEnd					; Yep : exit
	
	lbsr	CON_EOL						; next line
	
CmdCatPrintNext1
	lda		#CMD_INIT_READ				; Start reading filename 
	lbsr	MMC_SendCmdRaw
	
CmdCatPrintLoop
	lbsr	MMC_WaitGetWritten			; Wait for byte, and get it in a
	cmpa	#0							; end of filename ?
	beq		CmdCatEOL					; yes : process next name
	
	jsr		BasicScreenOut				; nope print it
	bra		CmdCatPrintLoop

CmdCatEOL
	lbsr	CON_EOL						; send eol
	bra		CmdCatLoop					; do next entry


InitDirRead
	JSR     <BasChrGetCurr				; get current character from command
	beq		InitNullDir

	lbsr	GetSendFileName				; Get path / wildcard 
	bra		InitDirReadOpen				; open the directory
	
; No filename supplied, set a null filename, which also reset wildcard to *
InitNullDir
	lda 	#CMD_INIT_WRITE				; init write, clear dir name
	lbsr	MMC_SendCmd
	lbsr	CheckError					; check for errors, returns to basic on error
	
	lda		#0
	sta		D_WRITE_DATA_REG			; Send null dirname for now
	
InitDirReadOpen
	lda		#CMD_DIR_OPEN				; open dir
	lbsr	MMC_SendCmd
	lbsr	CheckError					; check for errors, returns to basic on error
	rts
	
;
; CmdCWD, change current working directory, change current snapshot directory.
;

CmdCWD
	cmpa	#'S							; CWDS ? (change snapshot dir?)
	bne		CmdDoCWD					; nope do CWD
	JSR     <BasChrGet					; skip the S

	lda		#CMD_DIR_SET_SNAPPATH		; change the snapshot path
    bra     CmdWD
CmdDoCWD
	lda		#CMD_DIR_CWD				; change working directory

CmdWD
	lbsr	CmdSendFilenameCmd			; Send directory name and command
	clra	
	rts

;
; CmdMkdir Make a directory.
;
CmdMKDir
	lda		#CMD_DIR_MAKE				; Make directory
	lbsr	CmdSendFilenameCmd			; Send directory name and command
	lbsr	CheckError					; check for errors, returns to basic on error
	
	clra
	rts

;
; FuncGetSnapDir, like CWD bugt get snapshot directory.
;
FuncGetSnapDir
	lda		#CMD_DIR_GET_SNAPPATH		; get snapshot path
	bra		FuncCWDC					; Rest same as WD$
	
;
; FuncCWD, return the current working directory as a string.
;

FuncCWD
	lda		#CMD_DIR_GETCWD				; Get current working directory
FuncCWDC
	jsr 	>MMC_SendCmd
	jsr 	>CheckError					; check for errors, returns to basic on error

    bra     FuncRetMMCString            ; Return it

GetStrLenA
	lda		#CMD_GET_STRLEN				; get length of returned path
	jsr     >MMC_SendCmdRaw
	jsr     >MMC_WaitGetWritten			; Wait for byte, and get it in a
    rts
    
FuncRetMMCString
    bsr     GetStrLenA
    
FuncRetMMCStringLenA
	tfr		a,b							; move length to b
	pshs	a
	JSR     >BasResStr					; reserve string space, exits with OS error if no space
										; X points to string
	puls	b							; recover length
	tstb								; check for zero length string
	beq		FuncRetMMCStringExit		; yes don't copy anything
	
	lda		#CMD_INIT_READ				; Start reading filename 
	jsr	    >MMC_SendCmdRaw

FuncCWDLoop
	jsr	    >MMC_WaitGetWritten			; Wait for byte, and get it in a
	sta		,x+
	decb								; decrement count
	bne		FuncCWDLoop					; more : keep going
	
FuncRetMMCStringExit
	JMP		>VarReturnStr				; return string to basic.
	
;
; Find first file
;

FuncFindFirst
	bsr		InitDirRead					; Shared by cat and findfirst

;
; Find next file.
;

FuncFindNext
	lda		#CMD_DIR_READ				; get next entry
	lbsr	MMC_SendCmd
	lbsr	CheckError					; check for errors, returns to basic on error
	
	cmpa	#STATUS_COMPLETE+STATUS_LAST		; Listed all entries ?
	beq		FindEnd
	bra		FuncRetMMCString			; return filename to basic
	
FindEnd
	clra								; return no bytes
	bra		FuncRetMMCStringLenA
	
__cmd_cat_end

