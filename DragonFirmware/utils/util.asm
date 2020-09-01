;
; Utility routines
;

;
; wrapper for UtilCopyBXtoU, that disables interrupts whilst doing it's work
; 
__utils
		ifne	0
DO_SafeCopy
		pshs	cc						; save condition codes
		orcc	#FlagIRQ+FlagFIRQ		; disable ints
		jsr		UtilCopyBXtoU			; do the copy
		puls	cc,pc					; restore and return

;
; Wrapper for UtilCopyBXtoU which enables extra ram before copying, and disables afterwards
; this assumes that the roms have already been mirrored to ram, and is to be used
; for copying data to ram e.g. whilst snapshotting.
;
		
DO_RamCopy
		pshs	a,cc					; save condition codes
		orcc	#FlagIRQ+FlagFIRQ		; disable ints

		lda     D_RAM_CTRL				; Get RAM control reg
        ora		#D_RAM_ENABLE			; put us in RAM mode
		sta		D_RAM_CTRL				; set flags
        
		jsr		UtilCopyBXtoU			; do the copy
		
		lda     D_RAM_CTRL				; Get RAM control reg
        anda	#~D_RAM_ENABLE			; put us in ROM mode
		sta		D_RAM_CTRL				; set flags

		puls	a,cc,pc					; restore and return

		endc

;
; MGetCommaThen8Bit, scan for comma, error if not found, then fetch 8 bit that follows (or error). 
;

MGetCommaThen8Bit
		JSR     <BasChrGetCurr			; Get current basic char
        BEQ     MGetCommaThen8BitExit	; Any left no: return 
        JSR     >VarCKComma				; check for comma
;        bra     MGet8Bit				; go get it
MGet8Bit
		PSHS    Y,U
        JSR     >VarGet8Bit				; Get 8 bit value into B
;MGet8BitorErrorExit	
		PULS    Y,U						; Restore and return

MGetCommaThen8BitExit
		rts
;
; Reset the machine to power on memory map, used by tape and direct loading routines.
; Note : corrupts Y and moves stack.
;

DO_ResetPoweron
		puls	y						; get our return address

; Perform the equivelent of a clear 200,MaxRam so the machine has it's ramtop
; reset as if it had just been turned on, as a lot of commercial tape software 
; assumes this condition, and will bomb on loading due to stack being over-written!

		ldx		<AddrRamTop				; Get top of RAM
		stx		<AddrFWareRamTop		; Firmware ramtop, last basic used address
		leax	-200,x					; 200 bytes string space
		stx		<AddrStack				; Stack address here
		jsr		ClearEntry				; perform a clear, moves stack!
		jsr		>BasNew					; Do a 'new'
		pshs 	y						; restore our return address!
		rts

__utils_end
