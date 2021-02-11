;
; DragonMMC system ROM,
;
; 2011-06-13, Phill Harvey-Smith.
; 2011-06-13, Phill Harvey-Smith.
;
; Fixed for assembly with "mamou" cross-assembler,
; from NitrOS9 cocotools.
;
; 2012-10-07,   Began porting to CoCo.
;
; 2012-10-20,   Began merging with SuperDos for dos disk image emulation.
;
; 2016-03-21,   Abandoned use of Superdos, and reverted to a bugfixed 
;               Dragondos (1.3) for Dragon disk emulation.
;
; 2016-04-05,   Implemented RSDOS disk emulation.
;
; 2017-03-04,   Began implementing NMI handler for snapshot button.
;
; 2017-03-07,   Fixed a bug that caused MTAPERUN to crash if called from a program,
;               caused by stack being over-written. Fixed by effectively doing a
;				clear 200,ramtop, to restore the machine to it's default power on 
;				state.
;
; 2017-03-10,	Fixed a further bug with MTAPERUN caused by the fix to the stack 
;				overwrite, where FlagonBird would crash on loading due to the 
;				return address from MTAPERUN being destroyed by the clear above.
;				we now save and restore the return address so that on exit MTAPERUN
;				returns to the BASIC loop where it left off!
;				MTAPERUN now also tries to validate that the EXEC address is within
;				the area of memory loaded and only calls it if this is true.
;				Implemented CFTypeE and CFTypeL to retrieve the EXEC and Load addresses
;				of the next tape file.
; 
; 2017-03-10,	Note to self, calling a routine that does hardware IO with the wrong
;				register contents leads to a crash!
;
; 2017-03-14,	Patched so that Basic programs will auto run when loaded, the CLOAD
;				command never returns and jumps directly into the basic main interpreter
;				loop. So we patch it in two ways, for tokenized programs we patch the RAM
;				copy of the ROM to call a routine in the MMC rom to tidy up and run the 
;				program. For ASCII programs we hook the "finish loading ASCII program" vector
;				and cause it to run the program.
;
; 2017-03-20,   Moved SAM bits saving into CPLD as AVR could not give quick enough response
;               time and was therefore dropping bits.
;               Implemented NMI handler with menu that handles cold / warm re-booting and
;               Memory snapshots.
; 
; 2017-04-03,	Fixed BASIC program auto load / run bug that was borking PImania, caused by
;				the wrong basic vector being patched leading to it being called in a recursive
;				loop and overwriting the RAM by blowing the stack up :(
;				Implemented conditianally assembled code that allows MCAS and auto-running basic 
;				to be emulated in MESS, to help fix above.
;
; 2017-07-29	Implemented MMIRROR and MMIRRORA commands for capturing tape input to a cas file
;				stored on the card.
;
; 2017-08-02	Removed array address function as will not be needed now that the AVR sorts files.
;				Removal of dead code, streamlining prior to first full release.
;
; 2017-09-08    Forked to V1.00 for release.
;
; 2017-09-11	Began work on V1.10.
;				Implemented MSETCFGS and MSETCFGC to set and clear the bits specified in the 
;				config byte. So something like MSETCFGS $80,0 will set bit 7 of the system config 
;				but leave bits 6..0 unchanged. Basically 'S' is OR cfgbyte and 'C' is AND NOT cfgbyte.
;
;
; 2017-10-08    Beta test release of 1.10.
;               Changes :
;                   
;                   Added MSETCFGs MSETCFGc to set and clear individual bits in config. 
;                   Added MLOADc to clear back to power on state and then do an MLOADa
;
;                   AVR firmware 2.70 now makes a bak file if an attempt to create an
;                   already existing file is made, instead of generating an error.
;
;                   Partially Re-written Dragon / RS dos emulation code to pass head and
;                   sector to AVR code, this allows us to create disk images.
;
;                   Before loading AUTOEXEC.DGN / .CCO the firmware now prints an autoboot 
;                   message and waits for 1 second before booting. Pressing space during the
;                   wait will abort the auto boot.
;

; These are defined by the makefile
;
;DragonDos		EQU		1	; Define this if compiling for Standard DragonDos Cart.
;DragonAlpha	EQU		1	; Define this if compiling for Dragon Alpha.
;RSDos			EQU		1	; Define this if compiling for the RS-DOS cart.

; Set to > 0 to enable debugging 
DEBUG		EQU	    0

#Set to 1 for development version, mainly just affects signon message :)
DEVEL		EQU		0

			use		cpudefs.asm
			use		dgndefs.asm
			use		dosdefs.asm
            use     WDdefs.asm
			use		romdefs.asm
			use		mmc2def.asm
			use		DragonMMCdef.asm
			use		basictokens.asm
			use 	basicdefs.asm
			use		samdefs.asm
					
			ORG     $C000


; Disk controler ID, if a cartrage starts with the chars 'DK', then the basic rom routines
; will do a JMP to $C002 to init the cartrage.

__init
DC000   FCC     /DK/				

; Jump table.
LC002   BRA    MMCInit
__init_end

		ifdef	BuildDos
 		ifdef	Dragon
		use		dragondos_mmc.asm
		endc
		
		ifdef	Tandy
 		use		rsdos_mmc.asm
		endc

__dosspare	
		zmb		($e000-*)
__dosspare_end
		
__main
		FCC     /DK/
		BRA    	RealMMCInit	
		endc

;Util_SafeCopy
;		JMP		>DO_SafeCopy		; Jump to safe copy routine	
;Util_RamCopy
;		JMP		>DO_RamCopy
Util_Select	
		JMP		>DO_Select
;Util_AssignU16D
;		JMP		>DO_Assign16

		jmp		>Inverse
		
Signature:
		FDB		SignatureWord	; So NMI chan check to see if we are paged in.
;
; Init Dos
; 
		ifeq	BuildDos
MMCInit
		else
RealMMCInit
        endc

		bra		DoInit
        
;
; First try and detect the machine type
;
DO_Select
        ldb     D_RAM_CTRL              ; Get ram control reg
        andb    #D_SEL_DRAGON           ; Default to dragon
        
        pshs	b
		jsr     >MachineDetect          ; Detect the machine
		puls	b						; note doesn't change flags
        beq     InitDragon
        
        orb     #D_SEL_COCO             ; Select CoCo version
        
InitDragon
        stb     D_RAM_CTRL              ; Select ROM.
		rts

		ifne	0
DO_SWI
		swi								; So we can test in MESS
		rts
		endc 
		
;
; Init various low ram stuff, interrupt vectors, basic stub etc
; 
	
DoInit    
		jsr		>Util_Select			; Select correct ROM version
		jsr		>InterruptInit			; Initialize interrupt vectors

        LDX     #RamVarPointer			; Point to rom copy of data to copy
MMCLC049   
		LDB     ,X+						; Get byte count byte
        BEQ     MMCLC054				; zero= end of table, exit copy
        LDU     ,X++					; Get destination address
        JSR     >UtilCopyBXtoU			; do copy
        BRA     MMCLC049				; do next

MMCLC054
		JSR		>SetPlatform			; Tell the AVR what our platform is
		JSR		>GetConfig				; get config byte

		ifdef	BuildDos
		lda		<CFGByte				; Get config byte
		
		ifdef	EMULATE
        lda     #$50
        sta     <CFGByte
		endc
	
		bita	#CFG_ENABLE_DOS			; is dos emulation enabled ?
		bne		MMCNoMoveUSR			; dos emulation enabled don't move USR vectors
		
		endc
		LDX     #NewUsrVec				; Adjust usr vector base
        STX     <BasUSRTableAddr	
        LDU     #BasFCError				; Setup the 10 usr vectors to point to BasFCError
        LDB     #$0A					; do 10
MMCLC064   
		STU     ,X++					; setup vector
        DECB							; decrement count
        BNE     MMCLC064				; loop again if more to do

MMCNoMoveUSR		
        LDX     #ResetVector			; Setup new reset vector
        STX     <IndVecReset
		lda		#$55					; Mark reset vector valid
		sta		<WarmStartFlag
			
        ANDCC   #~(FlagIRQ+FlagFIRQ)	; reenable inturrupts
	    
        LDX     #BasSignonMess 			; Print staandard Basic signon message
        JSR     >TextOutString
        
        LDX     #SignonMess-1
        JSR     >TextOutString
        
        lda		<CFGByte				; Get config byte (as DosInit may have trashed it!)
		bita    #CFG_SHOW_COMPILE       ; show compile date?
        beq     CheckDateTime           ; nope
        
        LDX     #CompileDate-1          ; show compile date
        JSR     >TextOutString      
        
CheckDateTime
        lda		<CFGByte				; Get config byte (as DosInit may have trashed it!)
		bita    #CFG_SHOW_DATETIME      ; show date and time?
        beq     NoDateTime
        
        jsr     >ShowDateTime            ; Display date and time from RTC
NoDateTime
		
		ifdef	BuildDos
		lda		<CFGByte				; Get config byte

		bita	#CFG_ENABLE_DOS			; is dos emulation enabled ?
		BEQ		MMCNoDosInit			; nope : don't init
		JMP		DosInit					; init dos if linked and enabled, exits to command mode

MMCNoDosInit
		endc

MMC_InitDone
		jsr	    >TestAutoBoot			; test for auto-boot, does not return if boots

        JMP     >BasCmdMode				; Jump to normal basic command mode
	
;
; Dispatch routines for new functions/commands
;

CmdDispatch   
		CMPA    #$FF					; Token in range ?
        BEQ     CmdDispatchSN			; nope : error
        SUBA    #DOSTokFirstC			; make our tokens zero based
        BPL     CmdCheckDOS				
CmdDispatchSN
		JMP     >BasSNError				; Basic : ?SNerror

CmdCheckDOS
		pshs	a
		lda		<CFGByte				; Get config byte
		bita	#CFG_ENABLE_DOS			; is dos emulation enabled ?
		puls	a						; doesn't affect flags !
		bne		CmdDOSOk				; dos emulation enabled accept all tokens

		cmpa	#DOSTokCountC			; Is it a dos token with DOS disabled ?
		blo		CmdDispatchSN			; yes : ?SNError	

CmdDOSOk
		CMPA    #NoCmds					; Is this one of our commands ?
        BCC     JumpToNextStubCmd		; nope : try next stub (if any)
        LDX     #CommandDispatchTable	; point to table of commands
		JMP		BasDoDispatch			; go execute it !
		
JumpToNextStubCmd
		ifdef	Dragon
		JMP     [>DBasStub2+DStubResJumpOfs]
		else
		jmp		[>CBasStub3+DStubResJumpOfs]
		endc

;
; On entering this routine, the function code has been left shifted 1 bit.
; This has the effect of discarding the top bit and converting the remaining
; bits to an offset into the jump table.
;
FuncDispatch   
		SUBB    #((DOSTokFirstF*2)&$FF) 	; Make function offset zero based
        BPL     LC691					; is this a basic function ?
        BRA     CmdDispatchSN			; yep : error

LC691   CMPB    #((NoFunc+1)*2)			; is this function handled by us ?
        BCC     JumpToNextStubFn		; nope : try next stub
		
		pshs	a
		lda		<CFGByte				; Get config byte
		bita	#CFG_ENABLE_DOS			; is dos emulation enabled ?
		puls	a						; doesn't affect flags !
		bne		FuncDOSOk				; dos emulation enabled accept all tokens

		cmpb	#(DOSTokCountF*2)		; Is it a dos token with DOS disabled ?
		blo		CmdDispatchSN			; yes : ?SNError	
	
FuncDOSOk		
        LDX     #FunctionDipatchTable
        JSR     [B,X]
		JMP		VarGetExprCC

JumpToNextStubFn

		ifdef	Dragon
		JMP     [>DBasStub2+DStubFuncsJumpOfs]
		else
		JMP     [>CBasStub3+DStubFuncsJumpOfs]
		endc

;
; New reset vector
;

ResetVector   
		NOP								; Main ROM checks for reset->NOP
        CLRA							; Reset DP=0
        TFR     A,DP		
		
		ifdef	BuildDos
		jsr		DOSResetVector			; Reset dos
		endc
		
;        LDA     #$35					; Re-enable NMI
;        STA     PIA0CRB
        
		JMP		WarmStart				; Jump back to Main ROM reset routine

RamVarPointer   
; New basic dispatch stub
;
; The Dragon in an unexpanded state has both Colour basic an Extended colour basic
; in the same rom, and so only has one command table, so we put our commainds in
; the second stub.
; The CoCo has CB & ECB in seperate roms, and so already has two command tables
; therefore our commands must go in the third table.
;
        FCB     StubLen					; No bytes

		ifdef	Dragon
; Dragon
        FDB     DBasStub1				; address to copy
		else
;CoCo ECB only !		
        FDB     CBasStub2				; address to copy
		endc
;
; Stub for our new commands
;
; note NoCmds & No Funcs defined below....
NewStub
        FCB     NoCmds					; No of commands
        FDB     CmdNames				; command list
        FDB     CmdDispatch				; command dispatch routine
        FCB     NoFunc					; No of functions
        FDB     FuncNames				; function list
        FDB     FuncDispatch			; function dispatch table

;
; New null terminating stub
;
        FCB     $00						; No of commands
        FDB     $0000					; Null list
        FDB     BasSNError				; command dispatch 
        FCB     $00						; no of functions
        FDB     $0000					; function dispatch
        FDB     BasSNError
StubLen	EQU		(*-NewStub)
       
        FCB     $00						; No bytes : terminate copy table
        FCB     $00

;
; Command routine pointers
; 

CommandDispatchTable
		use		dos_cmd_addrs.asm
		FDB		CmdMCas	
		FDB		CmdMMirror
		FDB		CmdMMount
		FDB		CmdHelp
		FDB		CmdRamBoot
		FDB		CmdCat
		FDB		CmdMDelete
		FDB		CmdMSave
		FDB		CmdMLoad
		FDB		CmdCartLoad
		FDB		CmdTapeRun
		FDB		CmdCWD
		FDB		CmdSetCFG
		FDB		CmdReTok
		FDB		CmdMDisk
		FDB		CmdRewind
        FDB     CmdSetDT
		FDB		CmdMKDir
		FDB		CmdMFile

; Note no spaces must appear in the EQU below!
CMDTableLen		EQU	(*-CommandDispatchTable)/2


FunctionDipatchTable   
		use		dos_func_addrs.asm
		FDB     BasFCError
        FDB		FuncCWD
		FDB		FuncFindFirst
		FDB		FuncFindNext
		FDB		FuncCFType
		FDB		FUNCGetCFG
        FDB     FuncGetDT
        FDB     FuncGetIName
        FDB		FuncGetSnapDir
		
; Note no spaces must appear in the EQU below!
FuncTableLen	EQU	(*-FunctionDipatchTable)/2
	
CmdNames   
		use		dos_cmd_names.asm

		FCS		/MCAS/
		FCS		/MMIRROR/
		FCS		/MMOUNT/
		FCS		/HELP/
		FCS		/RAMBOOT/
		FCS		/CAT/
		FCS		/MDELETE/
		FCS		/MSAVE/
		FCS		/MLOAD/
		FCS		/MCARTLOAD/
		FCS		/MTAPERUN/
		FCS		/CWD/
		FCS		/MSETCFG/
		FCS		/RETOK/
		FCS		/MDISK/
		FCS		/REWIND/
        FCS     /MSETDT/
		FCS		/MKDIR/
		FCS		/MFILE/
		
FuncNames   
		use		dos_func_names.asm
		
		FCS		/VER/
		FCS		/WD$/
		FCS		/FINDFIRST$/
		FCS		/FINDNEXT$/
		FCS		/CFTYPE/
		FCS		/MGETCFG/
        FCS     /MGETDT$/
        FCS     /MGETINAME$/
        FCS		/SD$/

NoCmds	EQU		CMDTableLen
NoFunc	EQU 	FuncTableLen

;
; Get string from command into temp var.
; Exits :
;	x = pointer to string data
;	b = length of string
;

GetCmdStrXB
        JSR     >VarGetStr				; get string into temp variable
		JSR		>BasGetStrLenAddr		; Get pointer in x len in b
		rts

;
; Get a filename and send to MMC
;

GetSendFileName
		bsr		GetCmdStrXB				; Get string from basic

		lbsr	MMC_SendName			; Send name to MMC
		rts

GetSendFileName2
		bsr		GetCmdStrXB				; Get string from basic

		lbsr	MMC_SendName2			; Send name to MMC
		rts
		
;
; Get a filename and send to MMC folowed by command in a
;
		
CmdSendFilenameCmd
CmdSendStringCmd
		pshs	a
		bsr		GetSendFileName			; get filename and send it
		
		puls	a						; Restore read or write
		lbsr	MMC_SendCmd				; Open file
		lbsr	CheckError				; Bomb out on error
		
		clra
		rts

CkComma
		pshs	b						; save b
		ldb		#Comma					; comma character
		cmpb	[BasAddrSigByte]		
		puls	b,pc					; flags untouched

CkOpenBrac
		pshs	b						; save b
		ldb		#OpenBrac				; comma character
		cmpb	[BasAddrSigByte]		
		puls	b,pc					; flags untouched
		
CheckError
		tsta							; Test for error, if bit 7 set
		bmi		ReportError				; Yes : error
		rts

ReportError	
		bne		FlagError				; Yes : flag it
		rts
	
FlagError
		lbsr	CON_WriteHexByte		; Write to screen as hex
		lbsr	CON_EOL
		jmp		BasFCError
	

CmdHelp 
        cmpa    #'X'                    ; helpx ?
        bne     CmdHelpNoX              ; no skip
        
        jsr		<BasChrGet				; skip first char 	
        lda     #$FF                    ; flag extended.
        bra     CmdHelpMain             ; skip forward
        
CmdHelpNoX
        clra                            ; Flag normal
CmdHelpMain
        pshs    a
        
        ldx     #SignonMess-1		    ; Print signon message including compile date
        JSR     >TextOutString
        
        LDX     #CompileDate-1          ; show compile date
        JSR     >TextOutString      
		
		ldx	    #IFVerMess-1			; print IF ver message
		JSR     >TextOutString
		
		lda		#CMD_GET_FW_VER			; Get firmware version
		jsr	    >MMC_SendCmd				
		jsr	    >CON_NdotN				; Write msn.lsn
		
        ldx	    #BLVerMess-1			; print IF ver message
		JSR     >TextOutString
		
		lda		#CMD_GET_BL_VER			; Get bootloader version
		jsr	    >MMC_SendCmd				
		jsr     >CON_NdotN				; Write msn.lsn
        
        jsr	    >CON_EOL
		
        puls    a                       ; retrieve flag
        tsta                            ; Check it
        beq     CmdHelpLoopExit         ; Not extended : exit
        
; HelpX, also display AVR firmware & Bootloader compile dates.
        
        lda		#CMD_GET_FW_VER			; Get firmware version
		bsr     CmdGetSend64            ; send command display string
                
        lda		#CMD_GET_BL_VER			; Get bootloader version
		bsr     CmdGetSend64            ; send command display string
        		
CmdHelpLoopExit
		clra
		rts

CmdGetSend64
        jsr	    >MMC_SendCmd			; send the command

		lda 	#CMD_INIT_READ			; Read compile message
		jsr	    >MMC_SendCmdRaw			
        
		ldb		#64						; 64 chars max.

GetSendBBytes
		jsr	    >MMC_WaitGetWritten		; Wait for byte
		
		tsta							; test a
		beq		GetSendBBytesExit		; Zero : exit loop
		
		jsr		>BasicScreenOut			; output character
		
		decb							; decrement character count
		bne		GetSendBBytes			; loop if more
		
GetSendBBytesExit
		jmp		>CON_EOL				; newline & return

;
; Detect Dragon or CoCo, returns cc.z = 1 if Dragon 
;
		
MachineDetect
		ldx     #$B4BC                  ; Dragon ROM contains the word DRAGON at $B4BC.
        ldd		,x++					; looks for 'DRAGON' string 
		addd	,x++
		addd	,x++
		cmpd 	#$D4E7					; Sum of the letters of 'DR' + 'AG' + 'ON'
		rts

__main_end

; Messages
		use		message.asm
        
; Command includes
		use		cmds_tape.asm
		use		cmds_ram.asm
		use		cmd_cat.asm
		use		cmd_delete.asm
		use		cmds_direct.asm
		use		cmds_cart.asm
		use		cmds_retok.asm
		use		cmds_diskimg.asm
        use     cmds_datetime.asm
		use		cmds_config.asm

; Driver function includes
		use		config_func.asm
		use		mmc_func.asm
		use		console.asm
		use		util.asm
		use 	interrupt.asm
;        use     mon.asm


Inverse
		jsr		VarCKComma				; Check for comma, SN error if not
		jsr		VarGet16Bit				; Get print pos
		cmpx	#0						; beforer first?
		blo		InvError
		cmpx	#$1FF					; beyond end of screen?
		bhi		InvError				; yep 
		
		leax	$400,x					; turn into screen offset
		
		pshs	x						; save on stack
		
		jsr		VarCKComma				; Check for comma, SN error if not
		jsr		VarGet8Bit				; Get char count (in b)
		
		puls	x
		
InvLoop
		lda		,x						; get char from screen
		cmpa	#$80					; >$80, semigraphic, leave unchanged.
		bhs		NoInv
		eora	#$40					; flip bit
NoInv
		sta		,x+						; resave it
		decb							; decrement count
		bne		InvLoop					; not zero loop again
	
		rts
		
InvError
		jmp		BasFCError


;********************************
;** PLACE NO CODE BELOW HERE ! **
;********************************

		zmb		$fffd-*
        
;		org		$ffed
		FCC		/PHS/
;DE000   END
        