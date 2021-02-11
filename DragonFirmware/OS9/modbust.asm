********************************************************************
* modbust	: Take a combined OS9 module file e.g. OS9Boot and break it into 
*		  individual modulefiles.	
* $Id$
*
* Ed.    Comments                                       Who YY/MM/DD

         nam   ModBust
         ttl   Bust modules apart

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
BUFFSIZE	equ	16*1024		; Module buffer size, 16K should be enough.....
PATHBUFSIZE	equ	255		; size of path buffer

         org   0
DataPtr    	rmb  	2		; pointer to the data area will be at location 0 in DP
FnPtr		rmb	2		; Input filename pointer
InFileID	rmb	1		; file ID of input file
OutFileID	rmb	1		; file ID of output file
PathEndPtr	rmb	2		; pointer to the end of the path, where filename should be appended
PathBuf		rmb	PATHBUFSIZE	; Path buffer for output file
ModBuf		rmb	BUFFSIZE	; Line buffer
size     equ   .

name     	fcs   /modbust/
		fcb   edition

;                        12345678901234567890123456789012
UsageMsg 	fcb   	C$LF 
		fcc   	"Use: "
		fcb   	C$LF
		fcc   	"modbust modpath <outpath>"
		fcb   	C$LF
		fcc	"Note: modules are extracted to"
		fcb   	C$LF
		fcc	"CWD if outpath not specified"
		fcb   	C$CR
		
NoFileName	fcb   	C$LF 
		fcc	"Error: must supply filename"
		fcb   	C$CR

FileError	fcb	C$LF
		fcc	"Error accessing input file"
		fcb   	C$CR

OutFileError	fcb	C$LF
		fcc	"Error writing output file"
		fcb   	C$CR

ModuleMsg	fcb	C$LF
		fcc	"Module: "
;		fcb   	C$CR
ModuleMsgLen	equ	*-ModuleMsg

ExtractedMsg	fcc	" extracted"
;		fcb   	C$CR
ExtractedMsgLen	equ	*-ExtractedMsg
	
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

	lbsr	NextNonSpace		; find first non space character

	cmpa	#C$CR			; end of line, no parameters
	lbeq	HelpMe

	stx	<FnPtr			; save pointer to input filename
	
	lbsr	NextSpaceorEOL		; Have we got an output path?
	cmpa	#C$CR			; end of line, no more parameters
	beq	UseCWD			; yep, open input, no output path
	
	lbsr	NextNonSpace		; find next non space character
	cmpa	#C$CR			; end of line, no parameters
	beq	UseCWD			; yep, open input, no output path
	
CopyPath
	leay	PathBuf,u		; point to path buffer

CopyPathNext
	lda	,x+			; get a byte from the command line
	
	cmpa	#C$SPAC			; space?
	beq	EndPath			; yep end of path
	cmpa	#C$CR			; end of command line
	beq	EndPath			; yep end of path
	
	sta	,y+			; save in pathbuff
	bra	CopyPathNext		; copy next char
	
EndPath
	lda	#Slash			; terminate the path
	sta	,y+			; put it in path
	
	bra	OpenInput

UseCWD
	leay	PathBuf,u		; point to path buffer
	
OpenInput	
	sty	PathEndPtr,u		; save path end pointer
	ldx	<FnPtr			; get pointer to filename
	lda	#READ.			; open file for read
	os9	I$Open			; open input file
	lbcs	PFileError		; error : exit
	
	sta	<InFileID		; save file id
	
; Read the module header into memory, from this get the size of the module
; and then load the rest of the module into memory after the header	
ReadModHead
	leax	ModBuf,u		; Point at module buffer
	lda	<InFileID		; get file id
	ldy	#M$IDSize		; Size of module header
	os9	I$Read			; read module header
	lbcs	PFileError		; error : exit
	
	cmpy	#M$IDSize		; check we have enough bytes 
	blo	InputEnd		; else assume end of input file.
	
	leax	ModBuf,u		; Point at module buffer
	ldd	M$Size,x		; get size of module
	subd	#M$IDSize		; subtract size of header already read
	tfr	d,y			; put it in y for read
	
	leax	M$IDSize,x		; point past already loaded header
	lda	<InFileID		; get file id
	os9	I$Read			; read rest of module
	lbcs	PFileError		; error : exit
	
; at this point we have the module in memory in the module buffer	
	bsr	PrintModName		; print module name
	bsr	SaveModule		; save this module 
	
	bra	ReadModHead		; go process next module
	
InputEnd
	lda	<InFileID		; get file id
	os9	I$Close			; close input file
	lbra	ExitOK			; and exit

; print the module name.	
PrintModName
	leax	ModuleMsg,pcr		; point at message	
	ldy	#ModuleMsgLen		; length
	lda	#1			; stdout
	os9	I$WritLn		; Print it
	
	leax	ModBuf,u		; point at loaded module
	ldd	M$Name,x		; get offset to name
	leax	d,x			; point at name
	
	ldy	PathEndPtr,u		; point at path buffer
CopyNameLoop	
	lda	,x+			; get a byte from module name
	tsta				; check for end of name
	bmi	CopyNameEnd		; yep, go print it
	
	sta	,y+			; save unmodified byte
	bra	CopyNameLoop		; nope, process next
	
CopyNameEnd
	anda	#$7F			; reset bit 7
	sta	,y+			; save in buffer
	
TerminateName
	lda	#C$CR			; put a cr at the end
	sta	,y+
	
PrintName
	tfr	y,d			; work out count.....
	subd	PathEndPtr,u		; subtract start address
	subd	#1			; don't include terminator
	
	ldx	PathEndPtr,u		; point at name buffer
	clra				; zero msb
	tfr	d,y			; get byte count
	lda	#1			; stdout
	os9	I$WritLn		; Print it
	
	leax	ExtractedMsg,pcr	; point at message	
	ldy	#ExtractedMsgLen	; length
	lda	#1			; stdout
	os9	I$WritLn		; Print it
	rts
	
; save the module to it's own file in the CWD.
SaveModule
	leax	PathBuf,u		; point at path name buffer
	os9	I$Delete		; delete output file if it already exists
	bcc	Deleted			; delete was OK
	
	cmpb	#E$PNNF			; Was the file there to delete?
	beq	Deleted			; no, this is not an error then
	bra	POutFileError		; error, print error and exit

Deleted
	leax	PathBuf,u		; point at path name buffer
	lda	#WRITE.			; open for write mode
	ldb	#$03			; read and write attributes
	os9 	I$Create		; try creating the file
	bcs	POutFileError		; error, print error and exit
		
CreateOK
	sta	<OutFileID		; save file ID
	
	leax	ModBuf,u		; point at module in memory
	ldy	M$Size,x		; get size of module	
	lda	<OutFileID		; get file ID
	os9	I$Write			; write the module out
	bcs	POutFileError		; error, print error and exit

	lda	<OutFileID		; get file ID
	os9	I$Close			; close output file	
	rts


; Messages and exit

POutFileError
	leax	OutFileError,pcr	; point to message
	bra	PrintAndExit		; print it
	
PFileError
	cmpb	#E$EOF			; end of file?
	lbeq	InputEnd		; not an error!
	
	leax	FileError,pcr		; point at message
	bra	PrintAndExit		; print it
	
NoFile
	leax	NoFileName,pcr		; point at message
	bra	PrintAndExit		; print it

HelpMe
	leax	UsageMsg,pcr		; point to message

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

; Fine the next space or CR. Exit with X pointing to it	
NextSpaceorEOL
	lda	,x+			; get a character
	cmpa	#' '			; space?
	beq	FoundTerm		; yep, terminate
	cmpa	#C$CR			; CR?
	bne	NextSpaceorEOL		; nope, loop again
	
FoundTerm
	leax	-1,x			; back up to char
	rts
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
	
	emod
eom      equ   *
