;
; DragonMMC flex driver.
;

			use		cpudefs.asm
			use		dgndefs.asm
            use     WDdefs.asm
			use		mmc2def.asm
			use		DragonMMCdef.asm
			use		samdefs.asm
			use		loader.asm


DiskBaseAddr	equ	$DE00
FlexSecTrack	equ	18

;
; Note the driver is assembled at DiskBaseAddr ($DE00) but the header loads it at 
; LoadDskAt ($5000). This is because flexboot then copies the driver to it's final
; location (DiskBaseAddr), once it has loaded the rest of the OS from the boot 
; track on the disk.
;
; To use these two files to boot Flex, insert the flex boot disk in drive 0 with :
; mdiski 1,"flex.dsk"
;
; Then load this and flexboot :-
; mload "flexboot.dgn"
; mload "diskmmc.dgn"
; exec
;
; Note you can use mloada for the second mload, and then it will auto run without the 
; exec, but the first *MUST* be mload, *NOT* mloada !
;


        ifdef   Header
hstart  fcb     MarkerHeadStart             ; Begin header
        fcb     FTypeBin                    ; binary file
        fdb     LoadDskAt                 	; load address
        fdb     (LENGTH+hlen)               ; length of file
        fdb     $2602                      	; exec address
        fcb     MarkerHeadEnd               ; End header
hlen	equ		(*-hstart)		
        endc

;
; DISK DRIVER ROUTINE JUMP TABLE
; DE00 ORG $DE00
;
			ifdef	Loader
			org		DiskBaseAddr-4
			
			FDB		DiskBaseAddr		; Load address for our loader
			FDB		LENGTH
			else
			org		DiskBaseAddr-4
			endc 
			
ReadSec 	JMP 	>DoReadSec 			; Read a single sector
WriteSec	JMP 	>DoWriteSec			; Write a single sector
VerifySec	JMP 	>DoVerify			; Verify last sector written
Restore		JMP 	>DoRestore			; Restore head to track #0
SelDrive	JMP 	>DoSelDrive			; Select the specified drive
CheckRdy 	JMP 	>DoCheckRdy			; Check for drive ready
QCheckRdy	JMP		>DoQCheckRdy		; Quick check for drive ready
ColdStart	JMP 	>DoColdStart		; Driver initialize (cold start)
WarmStart	JMP 	>DoWarmStart		; Driver initialize (warm start)
SeekTrk		JMP 	>DoSeekTrk			; Seek to specified track

;
; So that we only need one set of MMC communication routines, we will include
; jumps to them here, that way any Flex utilities to natively access the MMC
; can call them here.
;



;
; Disk driver state veriables :
;

FlexLastActiveDrv	FCB	0					; last active drive, updated by SelDrive

; A full description of each of the above mentioned routines follows.
; Each lists the necessary entry parameters and what exit conditions must
; exist. Note that "(Z)" represents the Zero condition code bit and "(C)"
; represents the Carry condition code bit. All other letters in
; parentheses represent CPU registers. In most cases the B register is
; reserved for "Error Conditions" upon return. If there is no error, the
; B register may be.destroyed. The "Error Condition" referred to is the
; status returned by a Western Digital 1771 or 1791 floppy disk controller
; chip. Those statuses are briefly described here. An error is indicated
; by a "1" in the indicated bit position.
;
;	BIT 	READ 			WRITE 			OTHER
;	7 		not ready 		not ready 		not ready
;	6 		0 				write protect 	write protect
;	5 		0 				0 				0
;	4 		not found 		not found 		seek error
;	3 		CRC error 		CRC error 		CRC error
;	2 		lost data 		lost data 		0
;	1 		0 				0 				0
;	0 		0 				0 				0
;
; If the Western Digital chip is not used, these statuses must be
; simulated by the user s routines.
;
; Each description lists any necessary entry parameters and the proper
; state of certain registers on exit. Unless stated otherwise, the 'Y',
; 'U', and 'S' registers must NOT be altered by any of the routines.
;

; READ This routine reads the specified sector into memory at the
; specified address. This routine should perform a seek
; operation if necessary. A sector is 256 bytes in length.
; ENTRY - (X) = Address in memory where sector is to be placed.
; (A) = Track Number
; (B) = Sector Number
; EXIT - (X) May be destroyed
; (A) May be destroyed
; (B) = Error condition
; (Z) = 1 if no error
; = 0 if an error

; s->A,B,XH,XL

DoReadSec
		pshs	d,x						; save parameters
		ldb		0,s						; get saved track no
		lda		FlexLastActiveDrv		; get drive id
		lbsr	MMC_SeekCheck			; go seek and check
		
		anda	#WDErrSeek				; Mask all but seek error
		bne		DoReadSecExit			; error : exit
		
		lda		1,s						; get sector no
		lbsr	MMC_SendHR				; send it
		
		lda		0,s						; get drive ID
		ldx		2,s						; and buffer address
		lbsr	MMC_ReadDOSSec			; go read it
		
DoReadSecExit
		leas	4,s						; drop saved parameters
		rts
		
; WRITE This routine writes the information from the specifed memory
; buffer area to the disk sector specified. This routine should
; perform a seek operation if necessary. A sector is 256 bytes
; in length.
; ENTRY - (X) = Address of 256 memory buffer containing data
; to be written to disk
; (A) = Track Number
; (B) = Sector Number
; EXIT - (X) May be destroyed
; (A) May be destroyed
; (B) = Error condition
; (Z) = 1 if no error
; = 0 if an error

DoWriteSec		
		pshs	d,x						; save parameters
		ldb		0,s						; get saved track no
		lda		FlexLastActiveDrv		; get drive id
		lbsr	MMC_SeekCheck			; go seek and check
		
		anda	#WDErrSeek				; Mask all but seek error
		bne		DoReadSecExit			; error : exit
		
		lda		1,s						; get sector no
		lbsr	MMC_SendHR				; send it
		
		lda		0,s						; get drive ID
		ldx		2,s						; and buffer address
		lbsr	MMC_WriteDOSSec			; go read it
		
DoWriteSecExit
		leas	4,s						; drop saved parameters
		rts


; VERIFY The sector just written to the disk is to be verified to
; determine if there are CRC errors. No seek is required as
; this routine will only be called immediately after a write
; single sector operation.
; ENTRY - No entry parameters
; EXIT - (X) May be destroyed
; (A) May be destroyed
; (B) = Error condition
; Page 10 - Section 4
; 6809 FLEX Adaptation Guide
; (Z) = 1 if no error
; = 0 if an error

DoVerify
		clrb
		rts

; RESTORE A restore operation (also known as a "seek to track 00") is to
; be performed on the specified drive. The drive is specified
; in the FCB pointed to by the contents of the X register. Note
; that the drive number is the 4th byte of the FCB. This
; routine should select the drive before executing the restore
; operation.
; ENTRY - (X) = FCB address (3,X contains drive number)
; EXIT - (X) May be destroyed
; (A) May be destroyed
; (B) = Error condition
; (Z) = 1 if no error
; = 0 if an error

DoRestore
		clrb
		rts
		
; DRIVE The specified drive is to be selected. The drive is specified
; in the FCB pointed to by the contents of the X register. Note
; that the drive number is the 4th byte of the FCB.
; ENTRY - (X) = FCB address (3,X contains drive number)
; EXIT - (X) May be destroyed
; (A) May be destroyed
; (B) = $0F if non-existent drive
; = Error condition otherwise
; (Z) =1 if no error
; =0 if an error
; (C) =0 if no error
; =1 if an error

DoSelDrive
		lda		3,x						; Get drive ID
		sta		FlexLastActiveDrv		; save it
		clrb							; no error.
		rts

; CHKRDY Check for a drive ready condition. The drive number is found
; in the specified FCB (at 3,X). If the user's controller turns
; the drive motors off after some time delay, this routine
; should first check for a drive ready condition and if it is
; not ready, should delay long enough for the motors to come up
; to speed, then check again. This delay should be done ONLY if
; not ready on the first try and ONLY if necessary for the
; particular drives and controller! If the hardware always
; leaves the drive motors on, this routine should perform a
; single check for drive ready and immediately return the
; resulting status. Systems which do not have the ability to
; check for a drive ready condition should simply always return
; a ready status if the drive number is valid.
; ENTRY - (X) = FCB address (3,X contains drive number)
; EXIT - (X) May be destroyed
; (A) May be destroyed
; (B) = Error condition
; (Z) = 1 if drive ready
; = 0 if not ready
; (C) = 0 if drive ready
; = 1 if not ready

DoCheckRdy
		clrb
		rts

; QUICK This routine performs a "quick" drive ready check. Its
; function is exactly like the CHKRDY routine above except that
; no delay should be done. If the drive does not give a ready
; condition on the first check, a not ready condition is
; immediately returned. Entry and exit are as above.

DoQCheckRdy
		clrb
		rts

; INIT This routine performs any necessary initialization of the
; drivers during cold start (at boot time). Actually, any
; operation which must be done when the system is first booted
; can be done here.
; ENTRY - No parameters
; EXIT - A, B, X, Y, and U may be destroyed

DoColdStart
		clrb
		stb			FlexLastActiveDrv		; Reset drive
		rts
		
; WARM Performs any necessary functions during FLEX warmstart. FLEX
; calls this routine each time it goes thru the warm start
; procedure (after every command). As an example, some
; controllers use PIA's for communication with the processor.
; If FLEX is exited with a CPU reset, these PIA's may also be
; reset such that the controller would not function properly
; upon a jump to the FLEX warm start entry point. This routine
; could re-initialize the PIA when the warm start was executed.
; ENTRY - No parameters
; EXIT - A, B, X, Y, and U may be destroyed

DoWarmStart
		clrb
		rts

; SEEK Seeks to the track specified in the 'A' accumulator. In
; double-sided systems, this routine should also select the
; correct side depending on the sector number supplied in 'B'.
; ENTRY - (A) = Track Number
; (B) = Sector Number
; EXIT - (X) May be destroyed (See text)
; (A) May be destroyed (See text)
; (B) = Error condition
; (Z) = 1 if no error
; = 0 if an error

DoSeekTrk
		clrb
		rts
		
		use		mmc_func_flex.asm
		
		ifdef	Loader
LENGTH	equ		(*-DiskBaseAddr)
		endc
		
