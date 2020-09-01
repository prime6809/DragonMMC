;
; cmds_direct.asm : commands for loading and saving direct to the disk.
;
; 2019-04-01, added MLOADR / MSAVER for loading / saving raw files 
; (without a header) to / from  memory.
; 

__cmd_direct

DebugTest	equ	0

MSaveFlagMCode	equ		'M'
MLoadFlagAuto	equ		'A'
MloadFlagClear	equ		'C'
MloadFlagSnap	equ		'S'
MloadFlagRaw	equ		'R'


AutoWaitTime    equ     (1*50)      	; Wait time in IRQ periods 50Hz for PAL, 60Hz for NTSC.

;
; MSave, same format as DragonDos save, files format compatible.
;
; MSave  "filename"
; MSaveM "Filename",start,end,entry
;

CmdMSave
		cmpa	#MSaveFlagMCode			; check for 'M'
		pshs	a						; save next letter
		bne		CmdMSaveFname			; nope, skip
		JSR     <BasChrGet				; skip the M

CmdMSaveFname
		lbsr	GetSendFileName			; Get filename send to MMC
		puls	a

		pshs	u						; Make temp stack frame
		leas	-FileHeadLen,s


        cmpa	#MSaveFlagMCode			; is it MSAVEM  
		bne		CmdSaveBasic			; nope : it's basic save as such
		
		leau	,s						; U=S
		
		bsr		GetCommaThen16			; Get start addr
		stx		HdrLoad,u
		
		bsr		GetCommaThen16			; Get end addr
		tfr		x,d
		cmpx	HdrLoad,u		
		lbcs	BasFCError
		
		subd	HdrLoad,u				; Convert to length
		lbmi	BasFCError				; ?FC error if -ve
		addd	#1						; fixup 
		std		HdrLen,u				; Save length
		
		bsr		GetCommaThen16			; Get entry address
		stx		HdrExec,u
		
		lda		#FTypeBin				; Set as binary
		
		bra		CmdSaveCommon			; Go save it
		
CmdSaveBasic
		leau	,s						; U=S
		ldx		<BasStartProg			; get start of program
		stx		HdrLoad,u
		
		ldd		<BasVarSimpleAddr		; get end of program
		subd	<BasStartProg			; calculate length
		std		HdrLen,u
		
		LDX     #BasFCError				; Exec address = FC Error
		stx		HdrExec,u

		lda		#FTypeBas				; Set as basic
		
CmdSaveCommon
		sta		HdrType,u		

		lda		#$55					; Save ID bytes
		sta		HdrID55,u
		coma
		sta		HdrIDAA,u
		
		lda		#CAS_FILE				; open default file
		lbsr	MMC_WaitPutLatchRead	; send file id to latch register

		lda		#CMD_FILE_OPEN_WRITE	; Open file
		lbsr	MMC_SendCmd				; Open file
		lbsr	CheckError				; Bomb out on error
		
		lbsr	MMC_SaveFile			; Save the file
		
		lbsr	MMC_CloseFile			; close the file
		
		clra
		leas	FileHeadLen,s			; cleanup stack frame
		puls	u,pc					; restore / return
	
GetCommaThen16
		pshs	u						; save u as often clobbered !
		jsr     >VarCKComma				; Check for comma
        jsr     >VarGet16Bit			; returned in X, FCError if invalid
		puls	u,pc					; restore / return
		
;
; MLoad, load a DragonDos format compatible file.
;
; Syntax :
;	MLOAD filespec
;	MLOAD filespec,new_start_addr
;
; Auto-start:
;   MLOADA filespec
; Snapshot:
;   MLOADS filespec
; Clear back to machine power on state, then autorun 
; 	MLOADC filespec
; Load a raw file :
;	MLOADR filespec,load_addr,length
;

CmdMLoad
		bsr		DoCmdMLoad				; attempt to load
		tsta							; Error ?
		beq		CmdMLoadOK				; nope : just return
		jmp		BasFCError				; Yep : error 
		
CmdMLoadOK
		rts
		
DoCmdMLoad
		pshs	a,u						; save next character
		cmpa	#MLoadFlagAuto			; is it LOADA ?
		beq		CmdMLoadAutoBoot		; yep : do it

        cmpa    #MloadFlagSnap      	; is is MLOADS ?
        lbeq    CmdMLoadS           	; yep : do it

		cmpa	#MloadFlagClear			; is it MLOADC
		lbeq    CmdMLoadAutoBoot    	; yep : do it

		cmpa	#MloadFlagRaw			; Is it MLOADR 
		lbeq	CmdMloadRaw				; yep : do it

        bra     CmdMLoadNoSkip      	; don't skip first
        
CmdMLoadAutoBoot
		JSR     <BasChrGet				; skip the A
CmdMLoadNoSkip
		JSR		CmdMLoadSendFName		; Get filename send to MMC, open file
		cmpa	#STATUS_ERROR  	    	; Error ?
		lbhs	CmdMLoadError2			; Yes : flag it

		lda		,s						; Recover flag
		cmpa	#MloadFlagClear			; MLOADC?
		bne		CmdMloadAutoexec		; nope : skip
		
		puls	a,u						; get saved regs off stack as we will move it
		
		jsr		>DO_ResetPoweron		; reset memory map.

		lda		#MLoadFlagAuto			; change to auto-run flag
		pshs	a,u						; re-save them

CmdMloadAutoexec
		leas	-FileHeadLen,s			; Make temp stack frame
		leau	,s						; U=S

		leax	,u						; Get address for file header
		ldb		#FileHeadLen			; Read file header bytes
		lbsr	MMC_ReadFileBlock		; go read it
		
		lda		#$55					; Check marker bytes
		cmpa	HdrID55,u		
		lbne	CmdMLoadError		
		
		coma
		cmpa	HdrIDAA,u
		lbne	CmdMLoadError		
		
		lda		HdrType,u				; Get filetype
		cmpa	#FTypeBas				; Basic file ?
		beq		CmdMLoadBas				; yep : load it
		
		cmpa	#FTypeBin				; Binary file ?
		lbne	CmdMLoadError			; nope, error

		ldx		HdrExec,u				; Get exec address
		stx		BasExecAddr				; set exec address
	
		lbsr	CkComma					; Comma ?
		bne		CmdMLoadBinDef			; no : use default addresses
		
		bsr		GetCommaThen16			; get new load address
		leau	,s						; U=S
		
		ldd		HdrExec,u				; get exec address
		subd	HdrLoad,u				; subtract start address, calculate exec offset from start
		stx		HdrLoad,u				; save new load address in header
		addd	HdrLoad,u				; calculate new exec address
		std		BasExecAddr				; set exec address

CmdMLoadBinDef		
		lbsr	MMC_LoadFile			; load the file
		lbsr	MMC_CloseFile			; close the file

		leas	FileHeadLen,s			; cleanup stack frame
		puls	a,u						; recover auto flag
		cmpa	#MLoadFlagAuto			; auto-run ?
		bne		CmdMLoadBinExit			; nope : return

		jmp		[BasExecAddr]			; Run the loaded code

CmdMLoadBinExit		
		clra
		rts								; restore / return

CmdMloadRaw
		JSR		CmdMLoadSkipSendFName	; skicp character, send filename
		
		cmpa	#STATUS_ERROR  	    	; Error ?
		lbhs	CmdMLoadError2			; Yes : flag it

		lbsr	CkComma					; Is there a comma and load address?
		bne		CmdMLoadError2
		lbsr	GetCommaThen16			; get new load address
		
		pshs	x						; save load address
		
		lbsr	CkComma					; Is there a comma and byte count?
		bne		CmdMLoadError2
		lbsr	GetCommaThen16			; get byte count
		
		tfr		x,u						; byte count to u
		puls	x						; recover load address
		
		lbsr	MMC_LoadFile2			; Go load into memory
		lbsr	MMC_CloseFile			; close the file

		puls	a,u						; clean up stack
		clra
		rts								; restore / return	

CmdMLoadBas
		bsr		LoadBasic				; load basic prog into memory
		        
		ldx		<BasStartProg			; get base of loaded prog
		jsr		>BasSetProgPtrX			; setup
		
        ldx		<AddrFWareRamTop		; get ramtop
		stx		<BasVarStrTop			; set top of string area
		ldx		<BasVarSimpleAddr		; get simple vars addr
		stx		<BasVarArrayAddr		; set array vars to be the same
		stx		<BasVarEnd				; set end of basic vars
		
		jsr		>CmdRestore				; do a restore
        
		leas	FileHeadLen,s			; cleanup stack frame
		puls	a,u						; recover auto flag (a)

		jsr		>BasResetStack			; reset the stack a preserved
		
		cmpa	#MLoadFlagAuto			; Auto run ?
		bne		CmdMLoadBasExit			; nope : return to command mode
		
		clra	
		jmp		BasRun					; run it

CmdMLoadBasExit
		CLRA
        JMP     >BasCmdMode

CmdMLoadError
		leas	FileHeadLen,s			; cleanup stack frame
CmdMLoadError2
		jsr		>MMC_CloseFile			; close the file

		puls	a,u						; recover auto flag (a)

		clra							; flag error
		coma
		rts
		
LoadBasic
		ldd		HdrLen,u				; get file length
		addd	<BasStartProg			; add start of basic text
		std		<BasVarEnd				; set end of storage
		
		ldb		#$40					; check to see there's enough memory
		jsr		>BasChkB2Free
	
		ldx		<BasStartProg			; get load address
		stx		HdrLoad,u				; save in header
	
		lbsr	MMC_LoadFile			; load the file
		jsr		>BasVect2				; setup basic program 
	
		leax	2,x						; setup vars
		stx		<BasVarSimpleAddr
		lbsr	MMC_CloseFile			; close the file
		rts

;
; Skip first character send filename....
;

CmdMLoadSkipSendFName
		JSR     <BasChrGet			    ; skip the fist char
CmdMLoadSendFName
		JSR	    >GetSendFileName		; Get filename send to MMC

		lda		#CAS_FILE				; open default file
		lbsr	MMC_WaitPutLatchRead	; send file id to latch register
        
		lda		#CMD_FILE_OPEN_READ	    ; Open file
		lbsr	MMC_SendCmd		        ; Open file
        
        rts
        
CmdMLoadS
        jsr     CmdMLoadSkipSendFName   ; get send filename
        bmi     CmdMLoadSError          ; error :exit
		
        orcc	#(FlagFIRQ+FlagIRQ)		; Make sure ints disabled
        
        lbra    NMILoadEntry        	; file open, jump into snapshot load.
        
CmdMLoadSError
        leas    3,s                 	; drop saved registers
        rts
		
;
; TestAutoBoot, test if auto-booting is enabled, and try to boot if it is.
;

TestAutoBoot
		lda		<CFGByte				; Get config byte
		
		anda	#CFG_ENABLE_AUTOBOOT 	; auto-boot enabled ?
		beq		NoAutoBoot				; nope return

		leax	AttemptBootMess,pcr		; point at message
		jsr		>CON_WriteString		; print it

        bsr     AutoScanKbd         	; scan keyboard
   		cmpa	#$20					; space ?
		beq		AutoBootAbort			; yep skip auto boot

		jsr		>CON_EOL
		
		ifdef	Dragon
        lda     #CMD_FILE_OPENAUTOD 	; Change to root & Open AUTOEXEC, Dragon
		else
		lda		#CMD_FILE_OPENAUTOC 	; Change to root & Open AUTOEXEC, CoCo
		endc
		
		lbsr    MMC_SendCmd
        
		lda		#MLoadFlagAuto			; Auto boot 'flag'
		pshs	u,a						; stack them
		
		lbra	CmdMloadAutoexec    	; Attempt to open / load

AutoBootAbort
		leax	AbortBootMess,pcr		; point at message
		jsr		>CON_WriteString		; print it
		
NoAutoBoot
		clra	
		rts

AutoScanKbd
		ldx		SysTimeVal				; Get current timer
		leax	AutoWaitTime,x			; Time to wait
        
PollAgain
        sync
		jsr		>BasicKbdIn				; check keyboard
		bne		AutoScanKbdExit			; key pressed, exit loop	
        cmpx	SysTimeVal				; time reached value yet?
		bhs		PollAgain				; nope keep going	

AutoScanKbdExit
        tsta
        rts
        
AttemptBootMess	
		FCC		"AUTOBOOT...."
		FCB		$00

AbortBootMess
		FCC		"ABORTED."
		FCB		$0d,$00

__cmd_direct_end
