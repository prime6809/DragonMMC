;
; cmds_diskimg.asm
;
; Disk image manipulation.
;

__cmd_diskimg

        ifdef   Dragon
TokOn   equ     DTokON
TokOff  equ     DTokOFF
        else
TokOn   equ     CTokON
TokOff  equ     CTokOFF
        endc

CmdMDisk
	tfr	a,b
	jsr	<BasChrGet		; skip first char 		

	cmpb	#'I			; MDiskI ?
	beq	CmdMDiskIC		; Yep insert
	
	cmpb	#'C			; MDiskC ?
	beq	CmdMDiskIC		; Yep insert
	
	cmpb	#'O			; MDiskO ?
	beq	CmdMdiskO		; Yep, eject
	
	cmpb	#'L			; MDiskL ?
	beq	CmdMDiskL		; Yep, list
	
        cmpb    #TokOn                  ; MDisk ON ?
        lbeq    CmdMdiskOn
        
        cmpb    #TokOff                 ; MDisk OFF ?
        lbeq    CmdMDiskOff
	
	cmpb	#'B			; MdiskB ?
	lbeq	CmdMdiskB		; Yep, boot
    
	jmp	>BasSNError		; no : ?SN Error

;
; Insert a disk image into a virtual drive.
;

CmdMDiskIC
	pshs	b			; Save 'I' or 'C'
	bsr	CmdDiskIOCommon		; get drive number and send it
	jsr     >VarCKComma		; Check for comma, error if not
	
	jsr	>GetSendFileName2	; get filename and send to AVR
	
	lda	#CMD_FILE_OPEN_IMG	; Open disk image
	puls	b			; revover version code
	cmpb	#'C			; Open/Create?
	bne	CmdMDiskICSend
	lda	#CMD_FILE_OPENCRE_IMG	; Open / create disk image
	
CmdMDiskICSend	
	jsr	>MMC_SendCmd		; Do it !
	jsr	>CheckError		; Check for error 
	
	clra
	rts

CmdDiskIOCommon
	bsr     CmdDiskIOGetID          ; get disk id in a
	
	jsr	>MMC_InitSendBytes	; Begin sending bytes to the AVR
	jmp	>MMC_WaitPutRead	; send the drive number
	
CmdDiskFC
	jmp	>BasFCError		; Generate FC error

;
; Get an 8 bit number and validate as drive id
;
CmdDiskIOGetID
	jsr	>VarGet8Bit		; Get drive no into b
	
	tfr	b,a			; get into a

; Dragon DOS drives are 1..4, RS-DOS are 0..3
	ifdef	Dragon
	cmpa	#1			; less than 1 ?
	blo	CmdDiskFC		; yep : error
	cmpa	#5			; greater than 4 ?
	bhs	CmdDiskFC		; yep : error
	deca				; make zero based
	
	else
	
	cmpa	#0			; less than 0 ?
	blo	CmdDiskFC		; yep : error
	cmpa	#4			; greater than 3 ?
	bhs	CmdDiskFC		; yep : error
	endc
        rts
        
;
; Eject a disk image from a virtual drive.
;
	
CmdMdiskO
	bsr	CmdDiskIOCommon		; get drive number and send it

	lda	#CMD_IMG_UNMOUNT	; Unmount the image
	jsr	>MMC_SendCmd		; do it

	clra				; no error
	rts

;
; List mounted disks
;
	
CmdMDiskL
	clra                            ; send first and last disk IDs required
        ldb     #3                      ; 0..3
        lbsr    MMC_SendDriveIDs
        
	lda	#CMD_GET_IMG_NAME	; Get names of images
	jsr	>MMC_SendCmd		; do it
	jsr	>CheckError		; Check for error 

; If we get here, then we have the images names in the AVR data buffer
; we will read them back and display them

	lda	#CMD_INIT_READ		; start reading
	jsr	>MMC_SendCmdRaw		; do it

	clrb				; name counter
	
CmdMDiskLNext
	pshs	b			; save it
	
	ifdef	Dragon
	addb	#'1
	else
	addb	#'0			; convert to ASCII
	endc
	tfr	b,a
	jsr	>BasicScreenOut		; print it
	
	lda	#':			; print :
	jsr	>BasicScreenOut		; print it
	
CmdMDiskLNextChar	
	jsr	>MMC_WaitGetWritten	; wait for a byte
	
	cmpa	#0			; end of name ?
	beq	CmdMDiskLEOL
	jsr	>BasicScreenOut		; print it
	bra	CmdMDiskLNextChar

CmdMDiskLEOL
	lbsr	CON_EOL			; send eol
	puls	b			; recover drive no
	
	incb				; increment
	cmpb	#4			; all done ?
	blo	CmdMDiskLNext		; nope do next 
	
	clra
	rts

;
; Turn on disk emulation and reboot
;
CmdMdiskOn
        ldb     <CFGByte                ; Get config byte
        orb     #CFG_ENABLE_DOS         ; Flag Dos enabled
MDiskOnOff
        jsr     >SetCfgDefaultB 
        
        clr     WarmStartFlag           ; flag cold start
        jmp     [$FFFE]                 ; reset machine, never returns
        

CmdMDiskOff
        ldb     <CFGByte                ; Get config byte
        andb    #~CFG_ENABLE_DOS         ; Flag Dos enabled
        bra     MDiskOnOff
        
; 
; Get the name of a specified image
; returns an empty string if no image mounted.
;
FuncGetIName
        bsr     CmdDiskIOGetID          ; get drive id, and adjust as needed, FCerror if invalid
        tfr     a,b                     ; copy it into, so we only get one filename
        
        lbsr    MMC_SendDriveIDs        ; send them
        
	lda	#CMD_GET_IMG_NAME	; Get names of images
	jsr	>MMC_SendCmd		; do it
	jsr	>CheckError		; Check for error 

        jmp     >FuncRetMMCString       ; Return it to basic

;
; Boot disk inserted into the first drive. 
; This command can be used to boot a disk *WITHOUT* Basic disk emulation
; being enabled, for example to boot OS-9, NitrOS9, Flex etc.
;

CmdMdiskB
	ifdef	Dragon
	ldu	#2			; track 0, sector 2
	endc
	
	ifdef	Tandy
	ldu	#(34*18)		; track 34, sector 0
	endc

	clrb				; MSB of LSN always 0
	clra				; drive 0
	ldx	#$2600			; boot address
	jsr	>MMC_ReadDOSSec		; go read first sector
	bne 	CmdMdiskBErr
	
	ldd	#$2600			; get first 2 bytes
	cmpd	#$4f53			; is it 'OS' ?
	bne	CmdMdiskBErr		; nope : error
	
	ifdef	Dragon
	ldb	#15			; 15 more sectors
	endc
	
	ifdef	Tandy
	ldb	#17			; 17 more sectors
	endc

CmdMdiskBLoop	
	pshs	b			; save count
	jsr	>MMC_ReadNextDOSSec	; go read next sector
	bne	CmdMdiskBErr
	puls	b			; recover count
	
	decb				; decrement count
	bne	CmdMdiskBLoop		; do next.
	
	jmp	$2602			; jump to loaded code
	rts
     
CmdMdiskBErr
	jmp	>BasFCError
     
__cmd_diskimg_end
