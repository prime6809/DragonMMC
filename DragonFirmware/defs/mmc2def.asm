; /*
; mmc2def.h Symbolic defines for DragonMMC

; 2011-05-25, Phill Harvey-Smith.

; // Register definitions, these are offsets from 0xFF50 on the Dragon side.
                        
CMD_REG                 equ $00
LATCH_REG               equ $01
READ_DATA_REG           equ $02
WRITE_DATA_REG          equ $03

; // DIR_CMD_REG commands
CMD_DIR_OPEN            equ $00
CMD_DIR_READ            equ $01
CMD_DIR_CWD             equ $02
CMD_DIR_GETCWD			equ $03
CMD_DIR_MAKE			equ	$04
CMD_DIR_REMOVE			equ	$05
CMD_DIR_SET_SNAPPATH	equ $06
CMD_DIR_GET_SNAPPATH	equ $07

; // CMD_REG_COMMANDS
CMD_FILE_CLOSE          equ $10
CMD_FILE_OPEN_READ      equ $11
CMD_FILE_OPEN_IMG       equ $12
CMD_FILE_OPEN_WRITE     equ $13
CMD_FILE_DELETE         equ $14
CMD_FILE_GETINFO        equ $15
CMD_FILE_OPENAUTOD      equ $16
CMD_FILE_OPENAUTOC      equ $17
CMD_FILE_OPEN_OVERWRITE equ $18
CMD_FILE_OPEN_SNAPR		equ $19
CMD_FILE_OPEN_SNAPW		equ $1A
CMD_FILE_OPEN_STREAMR	equ	$1B
CMD_FILE_OPEN_STREAMW	equ	$1C
CMD_FILE_COPY			equ $1D
CMD_FILE_RENAME			equ	$1E
CMD_FILE_OPENCRE_IMG	equ	$1F

CMD_INIT_READ           equ $20
CMD_INIT_WRITE          equ $21
CMD_READ_BYTES          equ $22
CMD_WRITE_BYTES         equ $23
CMD_REWIND			    equ $24
CMD_SEEK			    equ $25
CMD_TELL			    equ $26
CMD_GET_FID			    equ $27

; // READ_DATA_REG "commands"

; // Utility commands
CMD_GET_STRLEN			equ $30

; // EXEC_PACKET_REG "commands"
CMD_EXEC_PACKET         equ $3F

; // SDOS_LBA_REG commands
CMD_LOAD_LBA          	equ $40
CMD_GET_IMG_STATUS      equ $41
CMD_GET_IMG_NAME        equ $42
CMD_READ_IMG_SEC        equ $43
CMD_WRITE_IMG_SEC       equ $44
CMD_SER_IMG_INFO        equ $45
CMD_VALID_IMG_NAMES     equ $46
CMD_IMG_UNMOUNT         equ $47
CMD_IMG_SEEK			equ $48
CMD_CREATE_IMG			equ $49
CMD_GET_FDC_STATUS		equ $4A
CMD_READ_NEXT_IMG_SEC   equ $4B
CMD_LOAD_HR             equ $4C

; // Cassette file commands

CMD_CAS_FTYPE			equ $50
CMD_CAS_EMULATE     	equ $5F

; // UTIL_CMD_REG commands
CMD_GET_CARD_TYPE       equ $80
CMD_SET_BUSY			equ $90
CMD_NOP				    equ $91
CMD_SYNC				equ $92

CMD_GET_PORT_DDR        equ $A0
CMD_SET_PORT_DDR        equ $A1
CMD_READ_PORT           equ $A2
CMD_WRITE_PORT          equ $A3

CMD_GET_DATETIME        equ $C0
CMD_SET_DATETIME        equ $C1

CMD_GET_FW_VER          equ $E0
CMD_GET_BL_VER          equ $E1

CMD_GET_CFG_BYTE        equ $F0
CMD_SET_CFG_BYTE        equ $F1
CMD_SET_PLATFORM		equ	$F2
CMD_READ_AUX            equ $FD
CMD_GET_HEARTBEAT       equ $FE


;// Status codes
;// Unlike AtoMMC, DragonMMC should return with STATUS_COMPLETE set when 
;// a command completes.
;// If an error condition occours, then STATUS_ERROR should be set, this 
;// way we can use the 6809 BITA / BITB instructions to test for error.
;// this also allows us to BMI on error.
;//
;// Multi-phase commands like getting directory entries return status 
;// complete after each entry. On the last entry this is or'd with 
;// STATUS_LAST.
;//
STATUS_ERROR			equ $80
STATUS_COMPLETE			equ $40

ERROR_MASK			    equ $7F

;// To be or'd with STATUS_COMPLETE
STATUS_LAST			    equ $01

; FATFS Errors 
FR_OK 				    equ $00	; /* (0) Succeeded */
FR_DISK_ERR			    equ $01 ; /* (1) A hard error occured in the low level disk I/O layer */
FR_INT_ERR			    equ $02 ; /* (2) Assertion failed */
FR_NOT_READY			equ $03 ; /* (3) The physical drive cannot work */
FR_NO_FILE			    equ $04 ; /* (4) Could not find the file */
FR_NO_PATH			    equ $05 ; /* (5) Could not find the path */
FR_INVALID_NAME			equ $06 ; /* (6) The path name format is invalid */
FR_DENIED			    equ $07 ; /* (7) Acces denied due to prohibited access or directory full */
FR_EXIST			    equ $08 ; /* (8) Acces denied due to prohibited access */
FR_INVALID_OBJECT		equ $09 ; /* (9) The file/directory object is invalid */
FR_WRITE_PROTECTED		equ $0A ; /* (10) The physical drive is write protected */
FR_INVALID_DRIVE		equ $0B ; /* (11) The logical drive number is invalid */
FR_NOT_ENABLED			equ $0C ; /* (12) The volume has no work area */
FR_NO_FILESYSTEM		equ $0D ; /* (13) There is no valid FAT volume */
FR_MKFS_ABORTED			equ $0E ; /* (14) The f_mkfs() aborted due to any parameter error */
FR_TIMEOUT			    equ $0F ; /* (15) Could not get a grant to access the volume within defined period */
FR_LOCKED			    equ $10 ; /* (16) The operation is rejected according to the file shareing policy */
FR_NOT_ENOUGH_CORE		equ $11 ; /* (17) LFN working buffer could not be allocated */
FR_TOO_MANY_OPEN_FILES	equ $12 ; /* (18) Number of open files > _FS_SHARE */
FR_INVALID_PARAMETER	equ $13 ; /* (19) Given parameter is invalid */
	
; // To be or'd with STATUS_ERROR
ERROR_INVALID_CMD		equ $20
ERROR_INVALID_IMAGE		equ $21
ERROR_NO_DATA           equ $22
ERROR_INVALID_DRIVE     equ $23
ERROR_READ_ONLY         equ $24
ERROR_ALREADY_MOUNT     equ $25
ERROR_INVALID_TIME      equ $26
ERROR_INVALID_FID		equ $27
ERROR_INVALID_PARAM		equ $28
ERROR_NO_FREE_FID		equ $29

; // Config bit flags (system)
CFG_ENABLE_BOOTLOAD		equ $80 
CFG_BACKUP_FILE			equ	$04
CFG_HIDE_MAC            equ $02
CFG_DEBUG_LOG		    equ $01

; // Config bit flags (Dragon, CoCo)

CFG_ENABLE_AUTOBOOT		equ $40
CFG_ENABLE_DOS			equ $20
CFG_SHOW_DATETIME       equ $10
CFG_SHOW_COMPILE        equ $08

; File ids....
NO_FILES				equ 6
CAS_FILE				equ	0
