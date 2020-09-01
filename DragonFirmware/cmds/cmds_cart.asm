;
; cmds_cart, commands for loading carts.
; 

__cmd_cart

CmdCartLoad:
		cmpa	#'A						; is this a McartloadA ?
		bne		CmdCartLoadNoAuto
		pshs	a						; save auto flag for later
		JSR     <BasChrGet				; skip the A
		
		bra		CmdCartLoadOpen			; open the file
CmdCartLoadNoAuto
		clra							; flag not auto
		pshs	a						; save for later

CmdCartLoadOpen
		lbsr	CmdMRMountEntry			; Get filename and open for reading
		lbsr	CmdRamBoot				; Copy Basic / Cart rom to ram
		
        lbsr    GoROMMode               ; make sure we are in ROM, ram write enabled
		
		ldx		#$c000					; point at cart rom base

CmdCartLoadNext:
		clra							; read 256 bytes
		
		lbsr	MMC_ReadFCommon			; Tell AVR we want to read bytes
		clrb
					
CmdCartLoadBlkLoop:
		lbsr	MMC_WaitGetWritten		; get the byte, returned in a
		sta		,x+						; save in buffer
		incb							; inc counter
		bne		CmdCartLoadBlkLoop		; Get next byte
		
		cmpx	#$ff00					; end of cart area ?
		bne		CmdCartLoadNext			; nope do next block
			
		clr		WarmStartFlag			; make sure it's a cold boot
		
		orcc	#FlagIRQ+FlagFIRQ		; disable ints
		
		ldu		#CasIOBuff				; Copy code to cas buffer
		leax	CmdCartRAMCode,pcr		; point to ram code
		ldb		#CmdCartRAMCodeLen		; bytes to copy
		jsr		UtilCopyBXtoU			; copy them
		jmp		CasIOBuff				; jump to called code
		
		
; This code has to be executed from outside the cart space as it pages
; the rom out.

CmdCartRAMCode:
		lda     D_RAM_CTRL              ; get current control register
        anda    #~(D_RAM_VEC+D_NMI_ENABLE)  ; Turn off NMI button as almost certainly will be wrong!
		ora		#D_RAM_ENABLE+D_RAM_WP	; put us in ram mode, write protected.
        tst		,s						; test auto flag
		beq		CmdCartRAMNoAuto
		ora		#D_FIRQ_ENABLE			; turn on FIRQ enable for auto-boot
CmdCartRAMNoAuto:
		sta		D_RAM_CTRL				; set flags
		jmp		[HWVecReset]			; Reset machine !
CmdCartRAMCodeEnd:

CmdCartRAMCodeLen	equ		CmdCartRAMCodeEnd-CmdCartRAMCode

__cmd_cart_end
