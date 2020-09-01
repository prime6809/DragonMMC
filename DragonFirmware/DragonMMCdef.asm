IO_BASE				EQU		$ff00					; I/O Page
DP_REG_BASE		    EQU	    $50			        	; MMC reg base (DP offset)

DP_D_CMD_REG		EQU	    DP_REG_BASE+CMD_REG	    	; command register
DP_D_LATCH_REG		EQU	    DP_REG_BASE+LATCH_REG	    ; Latch register for command params
DP_D_READ_DATA_REG	EQU 	DP_REG_BASE+READ_DATA_REG	; Read data register for AVR->Dragon
DP_D_WRITE_DATA_REG	EQU	    DP_REG_BASE+WRITE_DATA_REG 	; Write data reg for Dragon->AVR

D_CMD_REG			EQU	    IO_BASE+DP_D_CMD_REG	    ; command register
D_LATCH_REG			EQU	    IO_BASE+DP_D_LATCH_REG	    ; Latch register for command params
D_READ_DATA_REG		EQU 	IO_BASE+DP_D_READ_DATA_REG	; Read data register for AVR->Dragon
D_WRITE_DATA_REG	EQU	    IO_BASE+DP_D_WRITE_DATA_REG ; Write data reg for Dragon->AVR

DP_D_STATUS_REG		EQU	    $54			        		; Status register.
D_STATUS_REG		EQU	    IO_BASE+DP_D_STATUS_REG		; Status register.

STATUS_FLAG_BUSY	EQU	    $01			            ; AVR Busy 
STATUS_FLAG_READ	EQU	    $02			            ; AVR has read our byte
STATUS_FLAG_WRITTEN	EQU	    $04			            ; AVR has written a byte

;
; The DragonMMC CPLD contains logic that allows reads from the PIA A side data register
; at $FF20 to be substituted with data comeing from the AVR. 
; This allows the AVR to generate an FSK'd signal that the default ROM will read
; as if it where coming from a real tape. This allows us to bootload the ROM flash program
; **BEFORE** the rom contains any valid code.
;

DP_D_PIA_MAP        EQU     $55                   	; PIA MAP
D_PIA_MAP           EQU     IO_BASE+DP_D_PIA_MAP	; PIA MAP
D_PIA_MAP_ENABLE    equ     $01                     ; Enable PIA map

;
; The DragonMMC board includes a 32K static RAM chip that can be mapped into the
; $8000-$FFEF area, which is normally occupied by the system and cartrige ROMS.
; This is done so that the roms can be copied accross and patched for MMC use without
; having to have an overlay rom, containing a patched copy of the Dragon's ROM, 
; this way I don't end up having to distribute Microsoft owned code.
;

DP_D_RAM_CTRL		EQU	    $56			        	; RAM control reg
D_RAM_CTRL		    EQU	    IO_BASE+DP_D_RAM_CTRL   ; RAM control reg

; D_RAM_CTRL bit encodings

D_RAM_ENABLE		EQU 	$01			            ; set to enable overlay ram, clear to disable
D_RAM_WP		    EQU	    $02			            ; set to write protect ram, clear to write enable
D_FIRQ_ENABLE		EQU	    $04			            ; Enable routing of clock to FIRQ for cart auto-start
D_ROM_WE            EQU     $08                     ; Enable writes to the ROM
D_ROM_A14           EQU     $10                     ; A14 line to the rom, used to select Dragon (0) or CoCo (1)
D_RAM_VEC		    EQU	    $20			            ; Read interrupt vectors from ROM (0) or RAM (1)
D_NMI_ENABLE        EQU     $40                     ; Enable generation of NMI on snap button press.
D_RAM_VEC_WP	    EQU	    $80			            ; Write protect area of RAM containing int vectors.

D_SEL_DRAGON        EQU     $FF-D_ROM_A14           ; Select dragon
D_SEL_COCO		    EQU	    D_ROM_A14           	; Select coco

;D_RAM_COLDBOOT      EQU     

;
; The DragonMMC CPLD contains logic that allows it to record writes to the SAM's write only
; configuration bits. This allows these to be saved and restored when a snapshot is created.
; Since there are 16 SAM control bits the register accupies 2 loacations MSB first, so may
; be loaded directly into a 16 bit register : D, X, Y, U, S.
;


DP_D_SAMBITS_MSB    EQU     $58                   	; SAM bits TY, M1, M0, R1, R0, P1, F6, F5
DP_D_SAMBITS_LSB    EQU     $59                   	; SAM bits F4, F3, F2, F1, F0, V2, V1, V0

D_SAMBITS_MSB		EQU		IO_BASE+DP_D_SAMBITS_MSB
D_SAMBITS_LSB		EQU		IO_BASE+DP_D_SAMBITS_LSB

PLAT_BOOTLOAD       EQU     $00                     ; Bootloader flags
PLAT_DRAGON 	    EQU	    $01		                ; Dragon platform
PLAT_COCO		    EQU	    $02		                ; CoCo platform
PLAT_INVALID        EQU     $FF                     ; Invalid platform flag

PLAT_LOW            EQU     PLAT_BOOTLOAD           ; Lowest valid platform
PLAT_HIGH           EQU     PLAT_COCO               ; Highest valid platform

VarsBase		    EQU	    $600			        ; base of vars

;NewUsrVec		    EQU	    VarsBase+$00		    ; new USR vectors
NewUsrVec		    EQU	    $EC			            ; Unused if dos not present.

Dollar			    EQU	    '$'
BlkBegin		    EQU	    '<'			            ; Cassette block begin byte
;SyncByte		    EQU	    'U'			            ; Cassette sync byte
OpenBrac		    EQU	    '('			            ; Open bracket
CloseBrac		    EQU	    ')'			            ; Open bracket
Comma			    EQU	    ','			            ; Comma

; Offsets within file header, compatible with DragonDos/SuperDos
HdrID55			    EQU	    $00			            ; magic marker byte #1
HdrType			    EQU	    $01			            ; file type see filetypes below
HdrLoad			    EQU	    $02			            ; Load address of file
HdrLen			    EQU	    $04			            ; length of file
HdrExec			    EQU	    $06			            ; entry address of file
HdrIDAA			    EQU	    $08			            ; magic marker byte #2

FileHeadLen		    EQU	    $09

; File types

FTypeBas		    EQU	    $01			            ; Basic files
FTypeBin		    EQU	    $02			            ; Binary file types

; Marker bytes
MarkerHeadStart     EQU     $55
MarkerHeadEnd       EQU     $AA

; Tempory vars

CFGByte			    EQU	    BasUnused1		        ; put it in unused byte at $76

SignatureWord		EQU	    $2112
