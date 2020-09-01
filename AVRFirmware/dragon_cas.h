/*
	dragon_cas.h 	Dragon / CoCo cassette file definitions.
*/

#define CASSyncByte			0x55 	// Sync byte in cassete files
#define	CASBlockBegin		0x3C	// Begining of block marker

// Block Types
#define CASBtFileName		0x00	// File name block
#define CASBtData			0x01	// Data block
#define CASBtEOF			0xFF	// End of file block

#define CASFNameBlockLen    15   	// 15 bytes in header block

#define CASDefBlockSize     250  	// Default block size

// File Types, as stored in filename block
#define CASFtBasic		 	0x00	// Basic program
#define CASFtDataFile	 	0x01	// Data file
#define CASFtMachineCode	0x02	// Machine code program
#define CASFtBinary	 		0x03	// Binary file
#define CASFtHeaderless     0xFF  	// Invalid / Headerless

// Ascii/Binary flag from filename block
#define CASAsAscii			0xFF	// ASCII file
#define CASAsBinary	 		0x00	// Binary file (tokenised basic)

// Gap Flag from filename block
#define CASGfUngapped	 	0x00	// No gaps
#define CASGfGapped	 		0xFF	// Gaps between blocks

#define CASFilenameLen      0x08    // Filenames 8 bytes long

typedef char			CHAR;

typedef struct __attribute__ ((__packed__))
{
	BYTE	BlkType;		// Block type 
	BYTE	BlkLength;		// Block length
	CHAR	Filename[CASFilenameLen];	// Filename
	BYTE	FileType;		// File type
	BYTE	AsciiFlag;		// ASCII flag
	BYTE	GapFlag;		// Gapped file flag
	WORD	ExecAddress;	// Execution address (Motorola format)
	WORD	LoadAddress;	// Load Address (Motorola format)
} CASFilenameBlock;