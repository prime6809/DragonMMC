;
; rsdos_mmc_dummy.asm
; Dummy file that allows DragonMMC to be compiled for the CoCo, but without
; disk image emulation (currently).
; The file merely contains all the entry points re-directed to FCError!
;

;
; Init for MMC
;
MMCInit		lbra	RealMMCInit

DosInit:	
			jmp	>MMC_InitDone		; do post init stuff

rts

;
; New reset vector
;

DOSResetVector   
	NOP				; Main ROM checks for reset->NOP
	rts

CmdAuto:
			jmp	BasFCError
			
CmdBackup:
			jmp	BasFCError
			
CmdBeep:
			jmp	BasFCError
			
CmdBoot:
			jmp	BasFCError
			
CmdChain:
			jmp	BasFCError
			
CmdCopy:
			jmp	BasFCError
			
CmdCreate:
			jmp	BasFCError
			
CmdDir:
			jmp	BasFCError
			
CmdDrive:
			jmp	BasFCError
			
CmdDskInit:
			jmp	BasFCError
			
CmdFRead:
			jmp	BasFCError
			
CmdFWrite:
			jmp	BasFCError
			
CmdError2:
			jmp	BasFCError
			
CmdKill:
			jmp	BasFCError
			
CmdLoad:
			jmp	BasFCError
			
CmdMerge:
			jmp	BasFCError
			
CmdProtect:
			jmp	BasFCError
			
CmdWait:
			jmp	BasFCError
			
CmdRename:
			jmp	BasFCError
			
CmdSave:
			jmp	BasFCError
			
CmdSread:
			jmp	BasFCError
			
CmdSwrite:
			jmp	BasFCError
			
CmdVerify:
			jmp	BasFCError
			
CmdFLRead:
			jmp	BasFCError
			
CmdSwap:
			jmp	BasFCError
			

FuncLof:
			jmp	BasFCError
			
FuncFree:
			jmp	BasFCError
			
FuncErl:
			jmp	BasFCError
			
FuncErr:
			jmp	BasFCError
			
FuncHimem:
			jmp	BasFCError
			
FuncLoc:
			jmp	BasFCError
			
FuncFres:
			jmp	BasFCError
			









