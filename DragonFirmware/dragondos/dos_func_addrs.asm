DosFunctionDipatchTable   
		FDB     FuncLof
        FDB     FuncFree
        FDB     FuncErl
        FDB     FuncErr
        FDB     FuncHimem
        FDB     FuncLoc
		FDB     FuncFres

		
; Note no spaces must appear in the EQU below!
DosFuncTableLen		EQU	(*-DosFunctionDipatchTable)/2