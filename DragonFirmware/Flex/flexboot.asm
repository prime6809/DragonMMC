        use	cpudefs.asm
        use	dgndefs.asm
		use dosdefs.asm
        use WDdefs.asm
        use	romdefs.asm
        use	mmc2def.asm
        use	DragonMMCdef.asm
        use	basictokens.asm
        use	basicdefs.asm
		use samdefs.asm
		use	loader.asm


BASE    equ     $2500

        ifdef   Header
        org     BASE-FileHeadLen            ; Size of header
start
        fcb     MarkerHeadStart             ; Begin header
        fcb     FTypeBin                    ; binary file
        fdb     BASE                        ; load address
        fdb     LENGTH                      ; length of file
        fdb     Entry                       ; exec address
        fcb     MarkerHeadEnd               ; End header
        else
		org	BASE
        endc
		
L8000   	EQU $8000
LC104   	EQU $C104						; low level call into DOS rom.

FlexSOR		equ	$02							; Flex start of record flag
FlexTAR		equ	$16							; Flex transfer address record

;
; The original Flex boot routine set this to $DFFF as the normal DragonDos rom is only 8K
; However for DragonMMC this needs to be $FEFF, so all of the DragonMMC rom is copied.
;
TopCopy		EQU	$FEFF

LoaderAddr	equ	BASE

		org		LoaderAddr

		BRA     L252C					; jump to loader code

CopyAddr        
		FCB    $00,$00,$00
LinkAddr   	
		FCB    $01,$01					; Link address
TransferAddr   		
		FDB    $C100
LoadAddr
		FDB    $0000					; Load address where bootfile should be moved to
LastAddr
		FDB    $0000					; Last address loaded from bootfile + 1	

CopyOfs	EQU	(CopyAddr-LoaderAddr)

MNotLinked
        FCC    "DISK NOT LINKED"
        FCB    $00

L251D   LEAX    <MNotLinked,PCR			; point to message
L2520   LDA     ,X+						; get a byte
        BEQ     L2529					; end of string, yes exit
        JSR     >BasicScreenOut			; go print it
        BRA     L2520					; loop again

L2529	JMP     >ErrToBasic

L252C   LDS     #$2400					; Setup stack to be below where we are loaded.
        LDD     <LinkAddr,PCR			; Get Link address (on disk).			
        BEQ     L251D					; Zero : not linked exit

        STD     >LoadAt					; save in $2700
        LDX     #LoadAt					; Poiint at buffer to load

L253B   
		BSR    	ReadSec					; Go read a sector
		;FCB		$8d,$79
        LDD     ,X						; get track / head of next sector
        BEQ     L2547					; if Zero then end of file

        LEAX    256,X					; increment buffer pointer
        BRA     L253B					; loop again

L2547   ORCC    #FlagIRQ				; disable IRQ (on CPU)
        LDA     #$3C					; and on PIA
        STA     >PIA0CRB
		
        LEAX    256,X					; increment pointer
        STX     <LastAddr,PCR			; save it
        LDX     #L8000					; point at upper RAM
L2558
        CLR     ,X						; Clear a byte
        TST     ,X+						; test it is zero
        BNE     L2529					; nope, bad ram or not there, exit

        CMPX    #$FEFF					; end of RAM?
        BNE     L2558					; no keep testing	

        LDY     #LoadAt					; Get address of loaded OS file

;
; Here we decode the loaded data in flex binary file format.
; And re-located it to the upper memory.		
;
		
L2567  	BSR     GetNextDataByte

        CMPA    #FlexSOR				; Start of record ?
        BEQ     L257D					; yep : deal with it

        CMPA    #FlexTAR				; Transfer address record?
        BNE     L2567					; nope : loop again

        BSR     GetNextDataByte			; Get transfer (exec) address 
        STA     <TransferAddr,PCR

        BSR     GetNextDataByte
        STA     <TransferAddr+1,PCR

        BRA     L2567

L257D   BSR     GetNextDataByte			; Get next byte
        STA     <LoadAddr,PCR			; Get two bytes of load address			

        BSR     GetNextDataByte
        STA     <LoadAddr+1,PCR

        BSR     GetNextDataByte			; Get bytes in this block

        TFR     A,B						; transfer to b
        BEQ     L2567					; Zero bytes : do nothing, loop again

        LDX     LoadAddr,PCR			; get load address

L2591   BSR     GetNextDataByte			; get a byte from loaded data
        STA     ,X+						; save it in destination
        DECB							; decrement count
        BNE     L2591					; loop if more

        BRA     L2567					; loop for next byte

;
; Get next data byte from the loaded sectors, skiping over 
; the link to next sector and the record number.
;

GetNextDataByte   
		PSHS    B						; save b
        TFR     Y,D						; Y->A:B
        TSTB							; Test low byte of address
        BNE     L25A3					; not on page boundry

        LEAY    4,Y						; increment past link and record number
L25A3   CMPY    >LastAddr,PC				; at end of loaded data?
        BHI     L25AE					; yep go run it

        LDA     ,Y+						; get next byte from loaded data in a
        PULS    PC,B                    ; Pull of PC, effective RTS

; We will need to insert patching code here........
L25AE
		ifdef	Patch
		bsr		DoPatch					; Patch in our disk code, already loaded into memory
		endc
		
		LDS     #$C07F
        JMP     [TransferAddr,PCR]

ReadSec STA     DskTrackNo				; Save track and sector for DOS
        STB     DskSectorNo
L25BA
        STX     DiskBuffPtr				; And load address
        PSHS    U,Y,X,DP				; save regs
        JSR     >LC104					; Call DragonDos to load the sector
        PULS    U,Y,X,DP				; restore regs
		
        TST     DosDiskError			; Error : try again?
        BNE     L25BA
        RTS								

		ifdef	Patch
DoPatch
		ldx		#LoadDskAt				; point at loaded code header
		ldu		,x++					; get dest address
		ldy		,x++					; get dest length
		
DoPatchLoop
		lda		,x+						; get a byte from source
		sta		,u+						; put in dest
		leay	-1,y					; decrement count
		bne		DoPatchLoop				; loop if more
		rts
		endc

		zmb		($2600-*)
		
; 
; Stored on DragonDos boot track 0 sector 3
;
        FCC    "OS"						; boot marker
Entry
        BRA     L2662					; jump to actual code

        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00,$00,$00,$00

        FCC    "FLEXEDAS   "			; volumen label

        FDB    $0000

        FCB    $0F,$02

        FCB    $0E,$09

        FDB    $01D5

        FCB    $0B,$10,$54,$27,$12,$00,$00,$00
        FCB    $00,$00,$00,$00,$00,$00,$00,$00
        FCB    $00

L2634   FCB    $7F,$33

MessFlexCR
        FCB    $20
        FCC    "FLEX COPYRIGHT COMPUSENSE LTD"
        FCB    $0D,$00

MessLoadErr
        FCB    $0D
        FCC    "LOAD ERROR"
        FCB    $00

L2662
        LDA     #$BF					; fill screen with $BF
        LDX     #$0400					; base of screen
L2667
        STA     ,X+						; save it
        CMPX    #$0600					; End of screen ?
        BNE     L2667					; nope : loop again

        LDD     #$0500					; Print messsage in middle of screen
        STD     TextVDUCursAddr			; save in output address
        LEAX    <MessFlexCR,PCR			; point to message
        BSR     PrintStr				; go display it

        STS     <L2634,PCR				; save Stack pointer
        LDA     #$3C					; disable ints from PIA0B					
        STA     PIA0CRB
		
        ORCC    #FlagIRQ				; Disable IRQ
        STA     SAMSTY					; Switch to map 1, all ram
        CLRA							; Zero D
        CLRB
        STD     L8000					; try writing to upper 32K
        LDD     L8000					; read it back, is it the same?
        BNE     ErrToBasic				; nope : No ram or bad.

        LDX     #L8000					; copy Roms into RAM	
L2693
        STA     SAMCTY					; set map type 0
        LDA     ,X						; pick up a byte
        STA     SAMSTY					; set map type 1
        STA     ,X+						; put it in RAM
        CMPX    #TopCopy				; reached end ?
        BNE     L2693					; nope : do next

		ifndef	Patch

        LDD     #$0001					; track 0 sector 1
        LDX     #LoaderAddr				; Load at this address
        STA     >DskTrackNo				; setup for DragonDos call
        STB     >DskSectorNo
        STX     >DiskBuffPtr
        JSR     >LC104					; Call DragonDos 'read sector'

		else
		
		lda		#1						; boot from drive 1
		sta		>DosDriveNo				; set drive no (was DosLastDrive)
		
        LDD     #$0001					; track 0 sector 1
        LDX     #LoadAt					; Load at this address, temp buffer
        STA     DskTrackNo				; setup for DragonDos call
        STB     DskSectorNo
        STX     DiskBuffPtr
        JSR     >LC104					; Call DragonDos 'read sector'

;
; Copy values from loaded sector to our code.
;		
		ldx		#CopyAddr				; point to copy dest
		ldu		#(LoadAt+CopyOfs)		; point to loaded data
CopyParamLoop
		lda		,u+						; copy a byte
		sta		,x+
		cmpx	#MNotLinked				; end of copy data
		bne		CopyParamLoop			; nope keep going
		
		endc 
		
        JMP     >LoaderAddr				; jump to loaded code

ErrToBasic
        LEAX    <MessLoadErr,PCR		; Point at load error string
        BSR     PrintStr				; print it

        LDS     L2634,PCR				; Restore S
		
L26C1
        STA     SAMCTY					; Set Map type 0
        LDA     #$3F					; setup PIA0 side B
        STA     PIA0CRB
L26C9
        RTS
		
;
; Display pointed to string on screen until zero byte
; 
		
PrintStr
        LDA     ,X+						; Get a byte from string
        BEQ     L26C1					; =0, yes end of string : exit

        PSHS    X						; save X
        JSR     >BasicScreenOut			; call ROM to output it
        PULS    X						; restore X
        BRA     PrintStr				; do next 

        RTS

        ifdef   Header
LENGTH  equ     (*-start)	
        endc
