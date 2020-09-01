DosCommandDispatchTable   
		FDB     CmdAuto
        FDB     CmdBackup
        FDB     CmdBeep
        FDB     CmdBoot		; Else, if dragondos, boot command 
        FDB     CmdChain
        FDB     CmdCopy
        FDB     CmdCreate
        FDB     CmdDir
        FDB     CmdDrive
        FDB     CmdDskInit
        FDB     CmdFRead
        FDB     CmdFWrite
        FDB     CmdError
        FDB     CmdKill
        FDB     CmdLoad
        FDB     CmdMerge
        FDB     CmdProtect
        FDB     CmdWait
        FDB     CmdRename
        FDB     CmdSave
        FDB     CmdSread
        FDB     CmdSwrite
        FDB     CmdVerify
        FDB     BasSNError
        FDB     CmdFLRead
        FDB     CmdSwap

		
; Note no spaces must appear in the EQU below!
DosCMDTableLen		EQU	(*-DosCommandDispatchTable)/2