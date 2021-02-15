********************************************************************
* mdsk	: Manipulate virtual disk images on DragonMMC sd card.
*
* $Id$
*
* Ed.    Comments                                       Who YY/MM/DD

         nam   MDsk
         ttl   Manipulate virtual disk images on DragonMMC sd card.

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
DriveNo		rmb	1		; Drive number to insert/eject/create
FnLength	rmb	1		; length of supplied filename
InsCreCmd	rmb	1		; insert / create command
LineBuf		rmb	BUFFSIZE	; Line buffer
size     equ   .

name     	fcs   /mdsk/
		fcb   edition

;                        12345678901234567890123456789012
UsageMsg 	fcb   	C$LF 
		fcc   	"Use: "
		fcb   	C$LF
		fcc   	"mdsk l"
		fcb   	C$LF
		fcc	"mdsk i n sdimagepath"
		fcb   	C$LF
		fcc	"mdsk o n" 
		fcb   	C$LF
		fcc	"mdsk c n sdimagepath"
		fcb   	C$LF
		fcc	"l=List, i=insert, o=out, c=create"
		fcb   	C$LF
		fcc	"n=drive 0..3"
		fcb   	C$CR
		
NoFileName	fcb   	C$LF 
		fcc	"Error: must supply filename"
		fcb   	C$CR
 		
InvalidDrive	fcb   	C$LF 
		fcc	"Invalid drive no"
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

	anda	#$DF			; convert command to upper case
	ldb	1,x			; get character after command

	cmpb	#C$CR			; EOL?
	beq	CheckCmd		; yes could still be valid check it
	
	cmpb	#' '			; is it a space?
	lbne	HelpMe			; no, display help and exit

CheckCmd
	leax	1,x			; skip past command byte
	cmpa	#'L'			; list command?
	beq	DoList
	
	cmpa	#'I'			; insert image command?
	beq	DoInsert
	
	cmpa	#'O'			; remove image command?
	beq	DoRemove			
	
	cmpa	#'C'			; create image command?
	beq	DoCreate
	
	lbra	HelpMe			; Invalid command display help


DoList
	clra                            ; send first and last disk IDs required
        ldb     #3                      ; 0..3

        lbsr	MMC_InitSendBytes  	; Init writing bytes
        lbsr    MMC_WaitPutRead     	; send first drive
        exg     b,a                 	; get second drive
        lbsr    MMC_WaitPutRead     	; send second drive
       
	lda	#CMD_GET_IMG_NAME	; Get names of images
	lbsr	MMC_SendCmd		; do it

; If we get here, then we have the images names in the AVR data buffer
; we will read them back and display them

	lda	#CMD_INIT_READ		; start reading
	lbsr	MMC_SendCmdRaw		; do it

	clrb				; name counter
CmdMDiskLNext
	leax	LineBuf,u		; point at output buffer
	pshs	b			; save count
	addb	#'0'			; convert to ascii

	stb	,x+			; save driveno in buffer
	lda	#':'			; save colon in buffer
	sta	,x+

CmdMDiskLNextChar
	lbsr	MMC_WaitGetWritten	; wait for next char
	
	cmpa	#0			; end of imagename?
	beq	CmdMDiskLEOL		; yes print line
	sta	,x+			; put in buffer
	bra	CmdMDiskLNextChar	; loop again for next char

CmdMDiskLEOL
	lda	#C$CR			; terminate the line
	sta	,x+

	lda	#1			; stdout
	leax	LineBuf,u		; point at output buffer
	ldy	#BUFFSIZE		; max xhars to send
	os9	I$WritLn		; write it
	
	puls	b			; recover count
	incb				; increment it
	cmpb	#4			; done all drives?
	bne	CmdMDiskLNext		; nope, do next
	
	
	bra	ExitOK			; exit

DoCreate
	lda	#CMD_FILE_OPENCRE_IMG	; create image command
	bra	DoInsertCreate		; go do it

DoInsert
	lda	#CMD_FILE_OPEN_IMG	; Open image

DoInsertCreate
	sta	<InsCreCmd		; save Insert / Create flag
	
	lbsr	GetDriveNo		; get drive number
	sta	<DriveNo		; save it
	bcs	InvalidDriveNo		; error and exit

	lda	,x			; is next char space?
	cmpa	#' '			
	bne	NoImage			; invalid image specifiction, exit

	lbsr	GetImagePtr		; get image pointer, and length
	stx	<InPtr			; save pointer to filename
	stb	<FnLength		; save filename length
	
	bsr	SendDriveNo		; send drive number to AVR
	
	ldx	<InPtr			; Get pointer to filename
	ldb	<FnLength		; and length
	
	lbsr	MMC_SendName2		; get filename and send to AVR
	
	lda	<InsCreCmd		; get insert or create command
	
	lbsr	MMC_SendCmd		; Do it !
	
DoInsertExit
	bra	ExitOK			; exit

DoRemove
	lbsr	GetDriveNo		; get drive number
	sta	<DriveNo		; save it
	bcs	InvalidDriveNo		; error and exit

	bsr	SendDriveNo		; send drive number to AVR

	lda	#CMD_IMG_UNMOUNT	; Unmount the image
	lbsr	MMC_SendCmd		; do it

	bra	ExitOK			; exit

SendDriveNo
	lbsr	MMC_InitSendBytes	; Begin sending bytes to the AVR
	lda	<DriveNo		; get drive number
	lbsr	MMC_WaitPutRead		; send the drive number
	rts

	
;	ldb	#C$CR			; carrage return
;	bsr	CountStrTerm		; count string till terminator
;	
;	lbsr	MMC_SendName		; send path name + null terminator
;
;	bra	ExitOK

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

InvalidDriveNo
	leax	InvalidDrive,pcr	; point at message
	bsr	PrintAndExit		; print it
	
NoImage
	leax	NoFileName,pcr		; point at message
	bsr	PrintAndExit		; print it
	
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
; Get / validate drive number from command line
;

GetDriveNo
	lbsr	NextNonSpace		; Get next non space character
	
	cmpa	#'0'			; lowest valid driveno
	blo	GetDriveInvalid		; invalid error
	cmpa	#'3'			; highest valid drive no
	bhi	GetDriveInvalid		; invalid error

	leax	1,x			; move passt drive no
	suba	#'0'			; convert to binary
	rts
GetDriveInvalid
	coma
	rts
	
;
; Get pointer to image filename, and filename length
;	
GetImagePtr
	lbsr	NextNonSpace		; Get next non space character
	
	ldb	#C$CR			; end of line terminator
	lbsr	CountStrTerm		; count length of string
	rts
	
	emod
eom      equ   *
