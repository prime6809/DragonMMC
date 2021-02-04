********************************************************************
* mcat 	: catalog the specified directory on the DragonMMC sd card.
*
* $Id$
*
* Ed.    Comments                                       Who YY/MM/DD

         nam   LoadAt
         ttl   Load a file at a fixed location

         use    defsfile
	 use	os9.d

tylg     set   Prgrm+Objct   
atrv     set   ReEnt+rev
rev      set   $01
edition  set   8

         mod   eom,name,tylg,atrv,start,size

;
; working storage, first 256 bytes will be in the DP.
;
BUFFSIZE	equ	80


         org   0
DataPtr    	rmb  	2		; pointer to the data area will be at location 0 in DP
InPtr		rmb	2		; input pointer....
LineBuf		rmb	BUFFSIZE	; Line buffer
size     equ   .

name     	fcs   /mcat/
		fcb   edition

UsageMsg 	fcb   C$LF 
		fcc   "Use: mcat <sd_directory_path>"
		fcb   C$CR
		
slash	equ	'/'

* Here's how registers are set when this process is forked:
*
*   +-----------------+  <--  Y          (highest address)
*   !   Parameter     !
*   !     Area        !
*   +-----------------+  <-- X, SP
*   !   Data Area     !
*   +-----------------+
*   !   Direct Page   !
*   +-----------------+  <-- U, DP       (lowest address)
*
*   D = parameter area size
*  PC = module entry point abs. address
*  CC = F=0, I=0, others undefined
 
start   equ   *
        stu   	<DataPtr			; save data area pointer

	stx	<InPtr				; save input pointer
	bsr	NextNonSpace			; find first non space character

	ldb	#C$CR				; carrage return
	bsr	CountStrTerm			; count string till terminator
	
	lbsr	MMC_SendName			; send path name + null terminator

	lda	#CMD_DIR_OPEN			; open dir
	lbsr	MMC_SendCmd
	

CmdCatLoop
	lda	#CMD_DIR_READ			; get next entry
	lbsr	MMC_SendCmd
	
	cmpa	#STATUS_COMPLETE+STATUS_LAST	; Listed all entries ?
	beq	CmdCatLast			; nope : print next entry

	lda	#CMD_INIT_READ			; Start reading filename 
	lbsr	MMC_SendCmdRaw
	
	leax	<LineBuf,u			; point to buffer
CmdCatPrintLoop
	lbsr	MMC_WaitGetWritten		; Wait for byte, and get it in a
	cmpa	#0				; end of filename ?
	beq	CmdCatEOL			; yes : print it
	
	sta	,x+				; else put in buffer
	bra	CmdCatPrintLoop			; get next character to buffer
		
CmdCatEOL
	lda	#C$CR				; terminate with a CR
	sta	,x+				; put in buffer
		
	leax	<LineBuf,u			; point to buffer
	lda 	#1				; stdout
	ldy	#BUFFSIZE			; size of buffer
	os9   	I$WritLn 			; go write it
        bra	CmdCatLoop			; go get the next
	
CmdCatLast
	bra	ExitOK

;
; Utility routines
;	
NextNonSpace
	lda	,x+				; get a character
	cmpa	#' '				; space?
	beq	NextNonSpace			; yes keep going
	leax	-1,x				; back up to char
	rts
	
ShowUsage  
	leax   	>UsageMsg,pcr			; point to usage message

PrintAndExit
	clrb
PrintError
	bsr	PrintIt				; print it
	bra	ErrorExit			; exit returning code

ExitOK	clrb					; flag no error
	
ErrorExit	
	os9   	F$Exit   			; exit back to os

;
; Print message to stderr pointed to by x.
;
PrintIt
	pshs  	b				; save return code
        lda   	#$02				; stderr
        ldy   	#$0100				; up to 255 bytes
        os9   	I$WritLn 			; go write it
        puls  	b				; restore return code
	rts
;
; Count bytes in a string at X, terminated by B return count in b
;
CountStrTerm
	pshs	x,b				; save string start and terminator on stack
	clrb					; clear counter

CountStrLoop	
	lda	,x				; get a byte from string
	cmpa	,s				; is it the terminator?
	beq	FoundEnd			; yes, we have the end of the string
	
	leax	1,x				; point to next 
	incb					; increment counter
	bra	CountStrLoop			; loop again
FoundEnd	
	leas	1,s				; drop saved terminator
	puls	x,pc				; restore and return
;
; Code to talk to mmc.....
;	
	
InitDirReadOpen
	lda	#CMD_DIR_OPEN			; open dir
	lbsr	MMC_SendCmd
	rts	
	
	use	mmc_func.asm
	
	emod
eom      equ   *
