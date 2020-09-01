DosFunctionDipatchTable
        fdb     FuncCvn                 ; cvn / a2                      
        fdb     FuncFree                ; free / a3                   
        fdb     FuncLoc                 ; loc / a4                 
        fdb     FuncLof                 ; lof / a5                 
        fdb     FuncMkn                 ; mkn$ / a6                   
        fdb     BasSNError              ; as / a7               
		
; Note no spaces must appear in the EQU below!
DosFuncTableLen		EQU	(*-DosFunctionDipatchTable)/2
