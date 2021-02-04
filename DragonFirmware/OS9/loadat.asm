********************************************************************
* loadat - load a file (or part of) at a specified location
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
         org   0
DataPtr    	rmb  	2		; pointer to the data area will be at location 0 in DP
InPtr		rmb	2		; input pointer....
Location	rmb	2		; Location to load the file at
LoadSize	rmb	2		; number of bytes to load
FnPtr		rmb	2		; pointer to filename
InFileId	rmb	1		; the file ID of the file to load whilst reading
size     equ   .

name     	fcs   /LaodAt/
		fcb   edition

UsageMsg 	fcb   C$LF 
		fcc   "Use: LoadAt HexAddr Count filename"
		fcb   C$LF 
		fcc   "HexAddr is the memory address in hex to load at"
		fcb   C$LF 
		fcc   "Count is the number of bytes to load in hex"
		fcb   C$LF 
		fcc   "filename is the filename to load"
		fcb   C$CR

InvalidAddr    	fcb   C$LF
		fcc   "Error: Invalid load address."
		fcb   C$CR 

InvalidCount   	fcb   C$LF 
		fcc   "Error: Invalid byte count"
		fcb   C$CR 

FileError    	fcb   C$LF
		fcc   "Error: reading load file"
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
	bsr	GetHexWord			; get load address
	bcs	InvalidLoad			; error, print and exit		
	
	std	<Location			; save load address
	
	bsr	NextNonSpace			; find next non space character
	bsr	GetHexWord			; get load size
	bcs	InvalidSize			; error, print and exit		
	
	std	<LoadSize			; save size

	bsr	NextNonSpace			; find next non space character
	stx	<FnPtr				; save filename pointer
	
	lda	#READ.				; access mode read
	os9	I$Open				; open the file
	bcs	InvalidLoad			; error, exit
	
	sta	<InFileId			; save file id of input file
	
	ldx	<Location			; buffer to read to
	ldy	<LoadSize			; no of bytes to read
	lda	<InFileId			; File ID of input file
	os9	I$Read				; Go read it
	bcs	InvalidLoad			; error, exit
	
	lda	<InFileId			; get file id of input file
	os9	I$Close				; close input file

	bra	ExitOK
	
;
; Get a hex word number from the command line, terminates after 4 digits, or a non hex digit
; cc.c set on error.
;
GetHexWord	
	bsr	GetHexByte			; Get MSB
	bcs	GetHexWordExit			; error: exit
	pshs	a
	bsr	GetHexByte			; Get LSB
	puls	b				; flags unchanged
	exg	a,b				; swap them so MSB in a
	
GetHexWordExit
	rts
	
	
GetHexByte
	ldb	,x+				; get a byte
	bsr	HexConv				; convert to hex
	bcs	GetHexWordExit			; error : exit
	tfr	b,a				; get it in a
	
	asla					; shift to upper nibble
	asla
	asla
	asla
	ldb	,x+				; get a byte
	bsr	HexConv				; convert to hex
	bcs	GetHexWordExit			; error : exit

	pshs	b				; save LSB
	ora	,s+				; combine with msb

	andcc	#$FE				; clear carry
	rts

GetHexErr
	orcc	#$01				; set carry
	rts

HexConv
	cmpb	#'0'				; Less than zero?
	blo	GetHexErr			; yep error
	
	cmpb	#'A'				; is it a letter?
	blo	NoCase				; nope do nothing
	andb	#$DF				; convet to upper case (if character)
NoCase	
	subb	#'0'				; make zero based
	
	cmpb	#$9				; is it a number?
	ble	HexOK				; yes : no error
	
	subb	#('A'-'9')-1			; Convert A..F
	cmpb	#$F				; valid letter?
	bhi	GetHexErr			; nope!
	cmpb	#$A				; valid letter?
	blo	GetHexErr			; nope!
HexOK
	andcc	#$FE				; clear carry
	rts
	
	
NextNonSpace
	lda	,x+				; get a character
	cmpa	#' '				; space?
	beq	NextNonSpace			; yes keep going
	leax	-1,x				; back up to char
	rts
	
InvalidSize
	leax	>InvalidCount,pcr		; point at message
	bra	PrintAndExit

InvalidLoad
	leax	>InvalidAddr,pcr		; point at message
	bra	PrintError
	
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
	
	emod
eom      equ   *
