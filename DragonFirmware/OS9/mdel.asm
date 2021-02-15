********************************************************************
* mdsk	: Delete a file on DragonMMC sd card.
*
* $Id$
*
* Ed.    Comments                                       Who YY/MM/DD

         nam   MDel
         ttl   Delete a fie on the DragonMMC card

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
FnLength	rmb	1		; length of supplied filename
LineBuf		rmb	BUFFSIZE	; Line buffer
size     equ   .

name     	fcs   /mdsk/
		fcb   edition

;                        12345678901234567890123456789012
UsageMsg 	fcb   	C$LF 
		fcc   	"Use: "
		fcb   	C$LF
		fcc   	"mdel mmcfilename"
		fcb   	C$CR

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
        stu   	<DataPtr		; save data area pointer

	stx	<InPtr			; save input pointer
	lbsr	NextNonSpace		; find first non space character

	cmpa	#C$CR			; end of line, no parameters
	lbeq	HelpMe

	lbsr	GetMMCFilePtr		; get MMC filename pointer, and length
	stx	<InPtr			; save pointer to filename
	stb	<FnLength		; save filename length
	
	lbsr	MMC_SendName2		; get filename and send to AVR
	
	lda	#CMD_FILE_DELETE	; Delete the file
	lbsr	MMC_SendCmd		; Do it !
	
DoInsertExit
	bra	ExitOK			; exit

PrintAndExit
	bsr	PrintIt			; print message at x

ExitOK	clrb				; flag no error
	
ErrorExit	
	os9   	F$Exit   		; exit back to os

;
; Utility routines
;	
NextNonSpace
	lda	,x+			; get a character
	cmpa	#' '			; space?
	beq	NextNonSpace		; yes keep going
	leax	-1,x			; back up to char
	rts

HelpMe
	leax	UsageMsg,pcr		; point to message
	bsr	PrintIt			; print it
	bra	ExitOK			; exit
	
;
; Print message to stderr pointed to by x.
;
PrintIt
	pshs  	b			; save return code
        lda   	#$02			; stderr
        ldy   	#$0100			; up to 255 bytes
        os9   	I$WritLn 		; go write it
        puls  	b			; restore return code
	rts
;
; Count bytes in a string at X, terminated by B return count in b
;
CountStrTerm
	pshs	x,b			; save string start and terminator on stack
	clrb				; clear counter

CountStrLoop	
	lda	,x			; get a byte from string
	cmpa	,s			; is it the terminator?
	beq	FoundEnd		; yes, we have the end of the string
	
	leax	1,x			; point to next 
	incb				; increment counter
	bra	CountStrLoop		; loop again
FoundEnd	
	leas	1,s			; drop saved terminator
	puls	x,pc			; restore and return

	use	mmc_func.asm
	
;
; Get pointer to MMC filename, and filename length
;	
GetMMCFilePtr
	lbsr	NextNonSpace		; Get next non space character
	
	ldb	#C$CR			; end of line terminator
	lbsr	CountStrTerm		; count length of string
	rts
	
	emod
eom      equ   *
