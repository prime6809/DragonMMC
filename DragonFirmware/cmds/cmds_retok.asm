;
; Retokenize basic between Dragon and CoCo
;
; Majority of code borrowed from MultiCartBoot.
;

__cmd_retok

        use     tokens.asm

CmdReTok:
		tfr		a,b					; save next char
		jsr		<BasChrGet			; skip the D or C
		
		cmpb	#'D					; RetokD CoCo->Dragon ?
		beq		CmdRetokToDragon	; yep, do it
		cmpb	#'C					; RetokC Dragon->CoCo ?
		beq		CmdRetokToCoCo		; yep, do it
		clra						; no error
		rts

; retokenize CoCo to Dragon.
CmdRetokToDragon:
		leax	TokCoCoToDragon,pcr	; point at token tables
		bra		DoRetok				; Retokenize

; retokenize Dragon to CoCo.
CmdRetokToCoCo:
		leax	TokDragonToCoCo,pcr	; point at token tables
		bra		DoRetok				; Retokenize

DoRetok
		pshs	x
		ldu		BasStartProg		; Get start of basic 
	
DoRetokNewLine
		leau	4,u					; skip link & lineno
RetokLoop
		lda		,u					; get a byte
	
		tsta						; Test a and set flags
		beq		DoRetokEol			; end of line, check for end of code
	
		bmi		LookupToken			; Token, yes process it

RetokNextByte
		leau	1,u					; increment pointer
		bra		RetokLoop			; do next
	
LookupToken
		cmpa	#$FF				; Check extended token flag
		beq		RetokNextByte		; yes : do next
	
		anda	#$7f				; Make token zero based

        ldx		,s					; Pointer to pointer table

		ldb		-1,u				; Get previous byte
		cmpb	#$FF				; Extended token flag ?
		beq		DoExtended			; Yes do it
   
		cmpa	2,x                 ; Token not one of ours?
		bhs		RetokNextByte		; if so ignore it
        
		ldx		,x					; get normal function pointer
		bra		DoTranslate			; skip

DoExtended
		leax	3,x					; Point at extended token table
		cmpa	2,x                 ; Token not one of ours?
		bhs		RetokNextByte		; if so ignore it
        ldx     ,x                  ; Get function pointer 
        
DoTranslate
		ldb		a,x					; get new token
		stb		,u					; replace it
		bra		RetokNextByte		; do next
	
DoRetokEol
		leau	1,u					; increment pointer
		tst		,u					; check for a second zero, marking end of program
		bne		DoRetokNewLine		; no : do next line
		clra						; no error
		puls	x,pc


;***********************************************
; Token translation tables for basic programs. *
;***********************************************

TokDragonToCoCo
	FDB	DragonCoCoCmd
    FCB NoCommandsDgn
	FDB	DragonCoCoFunc
    FCB NoFuncsDgn
	
TokCoCoToDragon
	FDB	CoCoDragonCmd
	FCB NoCommandsCoCo
	FDB	CoCoDragonFunc
    FCB NoFuncsCoCo

;*****************
; Dragon to CoCo *
;*****************

DragonCoCoCmd
	FCB	CoFOR
	FCB	CoGO
	FCB	CoREM
	FCB	CoREM2
	FCB	CoELSE
	FCB	CoIF
	FCB	CoDATA
	FCB	CoPRINT
	FCB	CoON
	FCB	CoINPUT
	FCB	CoEND
	FCB	CoNEXT
	FCB	CoDIM
	FCB	CoREAD
	FCB	CoLET
	FCB	CoRUN
	FCB	CoRESTORE
	FCB	CoRETURN
	FCB	CoSTOP
	FCB	CoPOKE
	FCB	CoCONT
	FCB	CoLIST
	FCB	CoCLEAR
	FCB	CoNEW
	FCB	CoDEF
	FCB	CoCLOAD
	FCB	CoCSAVE
	FCB	CoOPEN
	FCB	CoCLOSE
	FCB	CoLLIST
	FCB	CoSET
	FCB	CoRESET
	FCB	CoCLS
	FCB	CoMOTOR
	FCB	CoSOUND
	FCB	CoAUDIO
	FCB	CoEXEC
	FCB	CoSKIPF
	FCB	CoDEL
	FCB	CoEDIT
	FCB	CoTRON
	FCB	CoTROFF
	FCB	CoLINE
	FCB	CoPCLS
	FCB	CoPSET
	FCB	CoPRESET
	FCB	CoSCREEN
	FCB	CoPCLEAR
	FCB	CoCOLOR
	FCB	CoCIRCLE
	FCB	CoPAINT
	FCB	CoGET
	FCB	CoPUT
	FCB	CoDRAW
	FCB	CoPCOPY
	FCB	CoPMODE
	FCB	CoPLAY
	FCB	CoDLOAD
	FCB	CoRENUM
	FCB	CoTAB
	FCB	CoTO
	FCB	CoSUB
	FCB	CoFN
	FCB	CoTHEN
	FCB	CoNOT
	FCB	CoSTEP
	FCB	CoOFF
	FCB	CoPlus
	FCB	CoMinus
	FCB	CoTimes
	FCB	CoDivide
	FCB	CoPower
	FCB	CoAND
	FCB	CoOR
	FCB	CoGt
	FCB	CoEq
	FCB	CoLt
	FCB	CoUSING

; DOS
	
	FCB	CoREM
	FCB	CoBACKUP
	FCB	CoREM
	FCB	CoDOS
	FCB	CoREM
	FCB	CoCOPY
	FCB	CoREM
	FCB	CoDIR
	FCB	CoDRIVE
	FCB	CoDSKINI
	FCB	CoREM
	FCB	CoREM
	FCB	CoREM
	FCB	CoKILL
	FCB	CoLOAD
	FCB	CoMERGE
	FCB	CoREM
	FCB	CoREM
	FCB	CoRENAME
	FCB	CoSAVE
	FCB	CoDSKIS	
	FCB	CoDSKOS 	
	FCB	CoVERIFY
	FCB	CoREM 
	FCB	CoREM 
	FCB	CoREM 
	
; MMC
	
	FCB	CoMCAS
	FCB	CoMMIRROR
	FCB	CoMMOUNT
	FCB	CoHELP	
	FCB	CoRAMBOOT
	FCB	CoCAT	
	FCB	CoMDELETE
	FCB	CoMSAVE	
	FCB	CoMLOAD	
	FCB	CoMCARTLOAD
	FCB	CoMTAPERUN
	FCB	CoCWD	
	FCB	CoMSETCFG
	FCB	CoRETOK	
	FCB	CoMDISK	
	FCB	CoREWIND
	FCB	CoMSETDT
	FCB	CoMKDIR
DragonCoCoCmdEnd

DragonCoCoFunc
	FCB	CoSGN
	FCB	CoINT
	FCB	CoABS
	FCB	CoPOS
	FCB	CoRND
	FCB	CoSQR
	FCB	CoLOG
	FCB	CoEXP
	FCB	CoSIN
	FCB	CoCOS
	FCB	CoTAN
	FCB	CoATN
	FCB	CoPEEK
	FCB	CoLEN
	FCB	CoSTRS
	FCB	CoVAL
	FCB	CoASC
	FCB	CoCHRS
	FCB	CoEOF
	FCB	CoJOYSTK
	FCB	CoFIX
	FCB	CoHEXS
	FCB	CoLEFTS
	FCB	CoRIGHTS
	FCB	CoMIDS
	FCB	CoPOINT
	FCB	CoINKEYS
	FCB	CoMEM
	FCB	CoVARPTR
	FCB	CoINSTR
	FCB	CoTIMER
	FCB	CoPPOINT
	FCB	CoSTRINGS
	FCB	CoUSR

; DOS

	FCB	CoLOF			
	FCB	CoFREE			
	FCB	CoREM		; DgnERL			
	FCB	CoREM 		; DgnERR			
	FCB	CoREM		; DgnHIMEM		
	FCB	CoLOC 			
	FCB	CoREM 		; DgnFRES			

; MMC

	FCB	CoVER		
	FCB	CoWDS		
	FCB	CoFINDFIRSTS	
	FCB	CoFINDNEXTS	
	FCB	CoCFTYPE	
	FCB	CoMGETCFG	
	FCB	CoMGETDTS	
	FCB	CoMGETINAMES	
	FCB	CoSDS		
DragonCoCoFuncEnd

NoCommandsDgn	EQU		DragonCoCoCmdEnd-DragonCoCoCmd
NoFuncsDgn	    EQU		DragonCoCoFuncEnd-DragonCoCoFunc

NoCommands      EQU     NoCommandsDgn
NoFuncs         EQU     NoFuncsDgn

;*****************
; CoCo to Dragon *
;*****************

CoCoDragonCmd
	FCB	DgnFOR
	FCB	DgnGO
	FCB	DgnREM
	FCB	DgnREM2
	FCB	DgnELSE
	FCB	DgnIF
	FCB	DgnDATA
	FCB	DgnPRINT
	FCB	DgnON
	FCB	DgnINPUT
	FCB	DgnEND
	FCB	DgnNEXT
	FCB	DgnDIM
	FCB	DgnREAD
	FCB	DgnRUN
	FCB	DgnRESTORE
	FCB	DgnRETURN
	FCB	DgnSTOP
	FCB	DgnPOKE
	FCB	DgnCONT
	FCB	DgnLIST
	FCB	DgnCLEAR
	FCB	DgnNEW
	FCB	DgnCLOAD
	FCB	DgnCSAVE
	FCB	DgnOPEN
	FCB	DgnCLOSE
	FCB	DgnLLIST
	FCB	DgnSET
	FCB	DgnRESET
	FCB	DgnCLS
	FCB	DgnMOTOR
	FCB	DgnSOUND
	FCB	DgnAUDIO
	FCB	DgnEXEC
	FCB	DgnSKIPF
	FCB	DgnTAB
	FCB	DgnTO
	FCB	DgnSUB
	FCB	DgnTHEN
	FCB	DgnNOT
	FCB	DgnSTEP
	FCB	DgnOFF
	FCB	DgnPlus
	FCB	DgnMinus
	FCB	DgnTimes
	FCB	DgnDivide
	FCB	DgnPower
	FCB	DgnAND
	FCB	DgnOR
	FCB	DgnGt
	FCB	DgnEq
	FCB	DgnLt
	FCB	DgnDEL
	FCB	DgnEDIT
	FCB	DgnTRON
	FCB	DgnTROFF
	FCB	DgnDEF
	FCB	DgnLET
	FCB	DgnLINE
	FCB	DgnPCLS
	FCB	DgnPSET
	FCB	DgnPRESET
	FCB	DgnSCREEN
	FCB	DgnPCLEAR
	FCB	DgnCOLOR
	FCB	DgnCIRCLE
	FCB	DgnPAINT
	FCB	DgnGET
	FCB	DgnPUT
	FCB	DgnDRAW
	FCB	DgnPCOPY
	FCB	DgnPMODE
	FCB	DgnPLAY
	FCB	DgnDLOAD
	FCB	DgnRENUM
	FCB	DgnFN
	FCB	DgnUSING

;DOS
	FCB	DgnDIR
	FCB	DgnDRIVE
	FCB	DgnREM		; CoFIELD
	FCB	DgnREM 		; CoFILES
	FCB	DgnKILL
	FCB	DgnLOAD
	FCB	DgnREM		; CoLSET
	FCB	DgnMERGE
	FCB	DgnRENAME
	FCB	DgnREM		; CoRSET
	FCB	DgnSAVE
	FCB	DgnREM 		; CoWRITE
	FCB	DgnVERIFY
	FCB	DgnREM 		; CoUNLOAD
	FCB	DgnDSKINIT
	FCB	DgnBACKUP
	FCB	DgnCOPY
	FCB	DgnSREAD 	; CoDSKIS
	FCB	DgnSWRITE	; CoDSKOS
	FCB DgnBOOT     ; CoDOS
    
; MMC
	FCB	DgnMCAS		
	FCB	DgnMMIRROR	
	FCB	DgnMMOUNT	
	FCB	DgnHELP		
	FCB	DgnRAMBOOT	
	FCB	DgnCAT		
	FCB	DgnMDELETE	
	FCB	DgnMSAVE	
	FCB	DgnMLOAD	
	FCB	DgnMCARTLOAD	
	FCB	DgnMTAPERUN	
	FCB	DgnCWD		
	FCB	DgnMSETCFG	
	FCB	DgnRETOK	
	FCB	DgnMDISK	
	FCB	DgnREWIND	
	FCB	DgnMSETDT	
	FCB	DgnMKDIR	
CoCoDragonCmdEnd

;	FILL	DgnREM,(NoCommandsDgn-NoCommandsCoCo)

	
CoCoDragonFunc
	FCB	DgnSGN
	FCB	DgnINT
	FCB	DgnABS
	FCB	DgnUSR
	FCB	DgnRND
	FCB	DgnSIN
	FCB	DgnPEEK
	FCB	DgnLEN
	FCB	DgnSTRS
	FCB	DgnVAL
	FCB	DgnASC
	FCB	DgnCHRS
	FCB	DgnEOF
	FCB	DgnJOYSTK
	FCB	DgnLEFTS
	FCB	DgnRIGHTS
	FCB	DgnMIDS
	FCB	DgnPOINT
	FCB	DgnINKEYS
	FCB	DgnMEM
	FCB	DgnATN
	FCB	DgnCOS
	FCB	DgnTAN
	FCB	DgnEXP
	FCB	DgnFIX
	FCB	DgnLOG
	FCB	DgnPOS
	FCB	DgnSQR
	FCB	DgnHEXS
	FCB	DgnVARPTR
	FCB	DgnINSTR
	FCB	DgnTIMER
	FCB	DgnPPOINT
	FCB	DgnSTRINGS

; DOS
	FCB	DgnREM		; CoCVN
	FCB	DgnFREE
	FCB	DgnLOC
	FCB	DgnLOF
	FCB	DgnREM 		; CoMKNS
	FCB	DgnREM		; CoAS

; MMC
	FCB	DgnVER		
	FCB	DgnWDS		
	FCB	DgnFINDFIRSTS	
	FCB	DgnFINDNEXTS	
	FCB	DgnCFTYPE	
	FCB	DgnMGETCFG	
	FCB	DgnMGETDTS	
	FCB	DgnMGETINAMES	
	FCB	DgnSDS		
	
CoCoDragonFuncEnd
	
;    FILL	DgnREM,(NoFuncsDgn-NoFuncsCoCo)

NoCommandsCoCo	EQU	CoCoDragonCmdEnd-CoCoDragonCmd
NoFuncsCoCo	    EQU	CoCoDragonFuncEnd-CoCoDragonFunc


    ifne    0
DragonCoCoCmd
	FCB	$80
	FCB	$81
	FCB	$82
	FCB	$83
	FCB	$84
	FCB	$85
	FCB	$86
	FCB	$87
	FCB	$88
	FCB	$89
	FCB	$8a
	FCB	$8b
	FCB	$8c
	FCB	$8d
	FCB	$ba
	FCB	$8e
	FCB	$8f
	FCB	$90
	FCB	$91
	FCB	$92
	FCB	$93
	FCB	$94
	FCB	$95
	FCB	$96
	FCB	$b9
	FCB	$97
	FCB	$98
	FCB	$99
	FCB	$9a
	FCB	$9b
	FCB	$9c
	FCB	$9d
	FCB	$9e
	FCB	$9f
	FCB	$a0
	FCB	$a1
	FCB	$a2
	FCB	$a3
	FCB	$b5
	FCB	$b6
	FCB	$b7
	FCB	$b8
	FCB	$bb
	FCB	$bc
	FCB	$bd
	FCB	$be
	FCB	$bf
	FCB	$c0
	FCB	$c1
	FCB	$c2
	FCB	$c3
	FCB	$c4
	FCB	$c5
	FCB	$c6
	FCB	$c7
	FCB	$c8
	FCB	$c9
	FCB	$ca
	FCB	$cb
	FCB	$a4
	FCB	$a5
	FCB	$a6
	FCB	$cc
	FCB	$a7
	FCB	$a8
	FCB	$a9
	FCB	$aa
	FCB	$ab
	FCB	$ac
	FCB	$ad
	FCB	$ae
	FCB	$af
	FCB	$b0
	FCB	$b1
	FCB	$b2
	FCB	$b3
	FCB	$b4
	FCB	$cd
DragonCoCoCmdEnd

DragonCoCoFunc
	FCB	$80
	FCB	$81
	FCB	$82
	FCB	$9a
	FCB	$84
	FCB	$9b
	FCB	$99
	FCB	$97
	FCB	$85
	FCB	$95
	FCB	$96
	FCB	$94
	FCB	$86
	FCB	$87
	FCB	$88
	FCB	$89
	FCB	$8a
	FCB	$8b
	FCB	$8c
	FCB	$8d
	FCB	$98
	FCB	$9c
	FCB	$8e
	FCB	$8f
	FCB	$90
	FCB	$91
	FCB	$92
	FCB	$93
	FCB	$9d
	FCB	$9e
	FCB	$9f
	FCB	$a0
	FCB	$a1
	FCB	$83
DragonCoCoFuncEnd

NoCommands	EQU		DragonCoCoCmdEnd-DragonCoCoCmd
NoFuncs		EQU		DragonCoCoFuncEnd-DragonCoCoFunc

;*****************
; CoCo to Dragon *
;*****************

CoCoDragonCmd
	FCB	$80
	FCB	$81
	FCB	$82
	FCB	$83
	FCB	$84
	FCB	$85
	FCB	$86
	FCB	$87
	FCB	$88
	FCB	$89
	FCB	$8a
	FCB	$8b
	FCB	$8c
	FCB	$8d
	FCB	$8f
	FCB	$90
	FCB	$91
	FCB	$92
	FCB	$93
	FCB	$94
	FCB	$95
	FCB	$96
	FCB	$97
	FCB	$99
	FCB	$9a
	FCB	$9b
	FCB	$9c
	FCB	$9d
	FCB	$9e
	FCB	$9f
	FCB	$a0
	FCB	$a1
	FCB	$a2
	FCB	$a3
	FCB	$a4
	FCB	$a5
	FCB	$bb
	FCB	$bc
	FCB	$bd
	FCB	$bf
	FCB	$c0
	FCB	$c1
	FCB	$c2
	FCB	$c3
	FCB	$c4
	FCB	$c5
	FCB	$c6
	FCB	$c7
	FCB	$c8
	FCB	$c9
	FCB	$ca
	FCB	$cb
	FCB	$cc
	FCB	$a6
	FCB	$a7
	FCB	$a8
	FCB	$a9
	FCB	$98
	FCB	$8e
	FCB	$aa
	FCB	$ab
	FCB	$ac
	FCB	$ad
	FCB	$ae
	FCB	$af
	FCB	$b0
	FCB	$b1
	FCB	$b2
	FCB	$b3
	FCB	$b4
	FCB	$b5
	FCB	$b6
	FCB	$b7
	FCB	$b8
	FCB	$b9
	FCB	$ba
	FCB	$be
	FCB	$cd

CoCoDragonFunc
	FCB	$80
	FCB	$81
	FCB	$82
	FCB	$a1
	FCB	$84
	FCB	$88
	FCB	$8c
	FCB	$8d
	FCB	$8e
	FCB	$8f
	FCB	$90
	FCB	$91
	FCB	$92
	FCB	$93
	FCB	$96
	FCB	$97
	FCB	$98
	FCB	$99
	FCB	$9a
	FCB	$9b
	FCB	$8b
	FCB	$89
	FCB	$8a
	FCB	$87
	FCB	$94
	FCB	$86
	FCB	$83
	FCB	$85
	FCB	$95
	FCB	$9c
	FCB	$9d
	FCB	$9e
	FCB	$9f
	FCB	$a0

    endc
	
__cmd_retok_end

