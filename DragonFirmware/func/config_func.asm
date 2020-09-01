;
; config_func.asm Get/Set config functions
;
; Seperated from cmds_config.asm 2017-09-04, PHS.
;
; Define MINIMUM to exclude code not needed by FLASH
; 
__config_func
;
; Set the config byte from platform in A, byte in B
;
; Writes the config byte to the global data area, using CMD_SEND_BYTES.
; Then writes the platform byte to the latch register and give a
; CMD_SET_CFG.
;
SetCFGinAB
;		lbsr	MMC_InitSendBytes		; send config byte
		lbsr	MMC_WaitPutLatchRead	; send platform byte to AVR	
		exg		a,b	
		lbsr	MMC_WaitPutLatchRead	; send config byte to AVR	
		lda		#CMD_SET_CFG_BYTE		; set config byte
		lbra	MMC_SendCmd				; Exec send command
		
		ifndef	MINIMUM

SetCFGBitsAB
		pshs	a						; save platform
		bsr		GetCFGinA				; get it's config byte
		pshs	b						; save mask
		ora		,s						; mask it in
SetClearCFGBitsAB
		exg		a,b						; platform in a value in b
		leas	1,s						; drop mask
		
		puls	a						; recover platform
		bra		SetCFGinAB				; go set it
		

ClearCFGBitsAB
		pshs	a						; save platform
		bsr		GetCFGinA				; get it's config byte
		comb							; and with inverse mask
		pshs	b						; save mask
		anda	,s						; mask it
		
		bra		SetClearCFGBitsAB		; go set it

;
; Get default (our) platform's config in a	
;


GetDefCFGinA
		bsr		GetPlatform				; get platform, 1=Dragon, 2=CoCo

		endc
		
GetCFGinA
		lbsr	MMC_WaitPutLatchRead	; send platform byte to AVR
		
		lda		#CMD_GET_CFG_BYTE		; Get the config byte
		lbsr	MMC_SendCmdRaw
		lbsr	MMC_WaitGetWritten		; wait for reply return it        
		rts
			
		ifndef	MINIMUM

;
; Tell the AVR what platform it is running on, we do this here, so
; that if the 6809 changes platform (for example in a DraCo) the AVR
; is aware of the fact.
;

SetPlatform
		bsr		GetPlatform				; get the platform by reading D_RAM_CTRL
		lbsr	MMC_WaitPutLatchRead	; send platform byte to AVR
		
		lda		#CMD_SET_PLATFORM		; set the platform
		lbra	MMC_SendCmd				; send the cnd and get the result
;
; Get our config and set system var.
;
GetConfig
        bsr     GetDefCFGinA            ; get default config
		sta		<CFGByte				; save it in ram
        rts

; Check if we are running on a Dragon or CoCo by looking at the ROM select bit
; returns 1 if Dragon, 2 if CoCo
GetPlatform
		lda		D_RAM_CTRL              ; Get ram control reg
		anda	#D_ROM_A14				; check rom sel bit
		bne		GetPlatformCoCo			; CoCo : exit
		lda		#PLAT_DRAGON			; Flag Dragon
		rts
			
GetPlatformCoCo
		lda		#PLAT_COCO				; Flag CoCo
		rts

		endc
		
__config_func_end

