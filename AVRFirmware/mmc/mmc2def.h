/*
	mmc2def.h Symbolic defines for DragonMMC
	
	2011-05-25, Phill Harvey-Smith.
*/

// Register definitions, these are offsets from 0xFF50 on the Dragon side.

#define CMD_REG				    0x00
#define LATCH_REG			    0x01
#define READ_DATA_REG		    0x02
#define WRITE_DATA_REG		    0x03

// DIR_CMD_REG commands
#define CMD_DIR_OPEN		    0x00
#define CMD_DIR_READ		    0x01
#define CMD_DIR_CWD			    0x02
#define CMD_DIR_GETCWD		    0x03
#define CMD_DIR_MAKE			0x04
#define CMD_DIR_REMOVE			0x05
#define CMD_DIR_SET_SNAPPATH	0x06
#define CMD_DIR_GET_SNAPPATH	0x07

// CMD_REG_COMMANDS
#define CMD_FILE_CLOSE		    0x10
#define CMD_FILE_OPEN_READ	    0x11
#define CMD_FILE_OPEN_IMG	    0x12
#define CMD_FILE_OPEN_WRITE	    0x13
#define CMD_FILE_DELETE		    0x14
#define CMD_FILE_GETINFO	    0x15
#define CMD_FILE_OPENAUTOD      0x16
#define CMD_FILE_OPENAUTOC      0x17
#define CMD_FILE_OPEN_OVERWRITE 0x18
#define CMD_FILE_OPEN_SNAPR		0x19
#define CMD_FILE_OPEN_SNAPW		0x1A
#define CMD_FILE_OPEN_STREAMR	0x1B
#define CMD_FILE_OPEN_STREAMW	0x1C
#define CMD_FILE_COPY			0x1D
#define CMD_FILE_RENAME			0x1E
#define CMD_FILE_OPENCRE_IMG	0x1F

#define CMD_INIT_READ		    0x20
#define CMD_INIT_WRITE		    0x21
#define CMD_READ_BYTES		    0x22
#define CMD_WRITE_BYTES		    0x23
#define CMD_REWIND			    0x24
#define CMD_SEEK			    0x25
#define CMD_TELL			    0x26
#define CMD_GET_FID			    0x27


// READ_DATA_REG "commands"

// Utility commands
#define CMD_GET_STRLEN		    0x30

// EXEC_PACKET_REG "commands"
#define CMD_EXEC_PACKET		    0x3F

// SDOS_LBA_REG commands
#define CMD_LOAD_LBA		    0x40
#define CMD_GET_IMG_STATUS	    0x41
#define CMD_GET_IMG_NAME	    0x42
#define CMD_READ_IMG_SEC	    0x43
#define CMD_WRITE_IMG_SEC	    0x44
#define CMD_SER_IMG_INFO	    0x45
#define CMD_VALID_IMG_NAMES	    0x46
#define CMD_IMG_UNMOUNT		    0x47
#define CMD_IMG_SEEK		    0x48
#define CMD_CREATE_IMG		    0x49
#define CMD_GET_FDC_STATUS	    0x4A
#define CMD_READ_NEXT_IMG_SEC   0x4B
#define CMD_LOAD_HR             0x4C

// Cassette file commands
// CAS_EMULATE for loading DragonMMC flasher
// CAS_EMULATE2 for loading Diag flasher
#define CMD_CAS_FTYPE		    0x50
#define CMD_CAS_EMULATE2        0x5E
#define CMD_CAS_EMULATE         0x5F

// UTIL_CMD_REG commands
#define CMD_GET_CARD_TYPE	    0x80
#define CMD_SET_BUSY		    0x90
#define CMD_NOP				    0x91
#define CMD_SYNC				0x92

#define CMD_GET_PORT_DDR	    0xA0
#define CMD_SET_PORT_DDR	    0xA1
#define CMD_READ_PORT		    0xA2
#define CMD_WRITE_PORT		    0xA3

#define CMD_GET_DATETIME        0xC0
#define CMD_SET_DATETIME        0xC1

#define CMD_GET_FW_VER		    0xE0
#define CMD_GET_BL_VER		    0xE1

#define CMD_GET_CFG_BYTE	    0xF0
#define CMD_SET_CFG_BYTE	    0xF1
#define CMD_SET_PLATFORM		0xF2
#define CMD_READ_AUX		    0xFD
#define CMD_GET_HEARTBEAT	    0xFE


// Status codes
// Unlike AtoMMC, DragonMMC should return with STATUS_COMPLETE set when 
// a command completes.
// If an error condition occours, then STATUS_ERROR should be set, this 
// way we can use the 6809 BITA / BITB instructions to test for error.
// this also allows us to BMI on error.
//
// Multi-phase commands like getting directory entries return status 
// complete after each entry. On the last entry this is or'd with 
// STATUS_LAST.
//
#define STATUS_ERROR		    0x80
#define STATUS_COMPLETE		    0x40

#define ERROR_MASK			    0x7F

// To be or'd with STATUS_COMPLETE
#define STATUS_LAST			    0x01

// To be or'd with STATUS_ERROR
// since STATUS_ERROR can also return a fatFS error code
// we must make sure these are in a non overlapping range.
#define ERROR_INVALID_CMD	    0x20
#define ERROR_INVALID_IMAGE	    0x21

#define ERROR_NO_DATA		    0x22
#define ERROR_INVALID_DRIVE	    0x23
#define ERROR_READ_ONLY		    0x24
#define ERROR_ALREADY_MOUNT	    0x25
#define ERROR_INVALID_TIME      0x26
#define ERROR_INVALID_FID		0x27
#define ERROR_INVALID_PARAM		0x28
#define ERROR_NO_FREE_FID		0x29
#define ERROR_INVALID_PLATFORM	0x2A

#define IS_ERROR(estat)		((estat & STATUS_ERROR) == STATUS_ERROR)

// Config bit flags (system)
#define CFG_ENABLE_BOOTLOAD	    0x80
#define CFG_DEBUG_LOG_REPLY		0x08
#define CFG_BACKUP_FILE			0x04
#define CFG_HIDE_MAC            0x02
#define CFG_DEBUG_LOG		    0x01

// Config bit flags (Dragon, CoCo)
#define CFG_ENABLE_AUTOBOOT	    0x40
#define CFG_ENABLE_DOS		    0x20
#define CFG_SHOW_DATETIME	    0x10
#define CFG_SHOW_COMPILE        0x08

#define DEBUG_ENABLED		(configByte & CFG_DEBUG_LOG)
#define HIDE_MAC            (configByte & CFG_HIDE_MAC)
#define DO_BACKUP			(configByte & CFG_BACKUP_FILE)
#define DEBUG_REPLY			(configByte & CFG_DEBUG_LOG_REPLY)

// WD command types
#define WD0					    0x00
#define WD1					    0x10
#define WD2					    0x20
#define WD3					    0x30
#define WD4					    0x40

#define WDMASK				    0xF0
#define WDPMASK				    0x0F	

// Cas file to be used for flashing rom.
// TapeFile = DragonMMC, TapeFile2 = Diag board ROM, in DMMC
#define TapeFile			    "FLASH.DGN"
#define TapeFile2				"DIAGFSH.DGN"

// Autoexecute files for Dragon & CoCo.
#define AutoFileDragon          "AUTOEXEC.DGN"
#define AutoFileCoCo            "AUTOEXEC.CCO"

#define PLAT_BOOTLOAD           0x00   
#define PLAT_SYSTEM             PLAT_BOOTLOAD                  
#define PLAT_DRAGON 	        0x01		                
#define PLAT_COCO		        0x02	
#define PLAT_INVALID            0xFF	                

#define PLAT_LOW                PLAT_BOOTLOAD           
#define PLAT_HIGH               PLAT_COCO               

#define PlatformValid(plat)		((PLAT_LOW <= plat) && (PLAT_HIGH >= plat))

#define SHORT_NAME_LEN			12
#define MAX_PATH_LEN			128	
#define SNAP_PATH_LEN			MAX_PATH_LEN
#define DEFAULT_SNAPPATH		"/"

#define SETTINGS_FILE_DRAGON    "/SETTINGS.DGN"
#define SETTINGS_FILE_COCO      "/SETTINGS.CCO"


// Cache of directory entries to allow sorting of directory.
#define DIR_CACHE_SIZE		256
#define CACHE_PTR_INVALID	-1

#ifndef BOOT_ADDR
typedef struct
{
	FILINFO	FileInfo[DIR_CACHE_SIZE];			// Cache of fileinfo structures
	int		FileInfoPtrs[DIR_CACHE_SIZE];		// Pointers to structures for sorting.
	int		IsCached;							// Cached flag 1 if cached 0 if not
	int		Entries;							// No of entries in DirCache / DirCachePtrs
	int		CachePtr;							// Current pointer to entry being operated on.
	int		Head;								// First pointer in chain.
	int		LastDir;							// Last entry that is a directory
} DirCache_t;
#endif

// Mask for logical sectors for CMD_LOAD_PARAM (and indirectly load_lsn).
#define LSN_MASK		(DWORD)0x00FFFFFF

// File IDs
// **NOTE** if you change NO_FILES, you may also need to change FF_FS_LOCK
// in ffconf.h. 
// As supplied the value of FF_FS_LOCK is 10, which is enough for 6 files 
// here and 4 files for disk emulation.
#define NO_FILES		6
#define CAS_FILE		0
