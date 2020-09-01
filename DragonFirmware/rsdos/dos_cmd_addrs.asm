DosCommandDispatchTable   
        fdb     CmdDir                   ; dir / ce                      
        fdb     CmdDrive                 ; drive / cf                     
        fdb     CmdField                 ; field / d0                     
        fdb     CmdFiles                 ; files / d1                     
        fdb     CmdKill                  ; kill / d2                   
        fdb     CmdLoad                  ; load / d3                   
        fdb     CmdLset                  ; lset / d4                   
        fdb     CmdMerge                 ; merge / d5                     
        fdb     CmdRename                ; rename / d6                       
        fdb     CmdRset                  ; rset / d7                   
        fdb     CmdSave                  ; save / d8                   
        fdb     CmdWrite                 ; write / d9                     
        fdb     CmdVerify                ; verify / da                       
        fdb     CmdUnload                ; unload / db                       
        fdb     CmdDskini                ; CmdDskini /dc                      
        fdb     CmdBackup                ; backup / dd                       
        fdb     CmdCopy                  ; copy / de                   
        fdb     CmdDski                  ; dski$ / df                    
        fdb     CmdDsko                  ; dsko$ / e0                    
        fdb     CmdDos                   ; dos / e1                 
		
; Note no spaces must appear in the EQU below!
DosCMDTableLen		EQU	(*-DosCommandDispatchTable)/2