#ifndef __MMC2_H
#define __MMC2_H

#ifdef INCLUDE_SDDOS
#include "dragon_disk.h"
#endif 

extern const __flash char CompileTime[];

#define VSN_MAJ 3
#define VSN_MIN 3

#define SECBUFFSIZE 	512
#define GLOBUFFSIZE 	512
#define STREAMBUFSIZE	256

#define BYTESLATCHSIZE	16

// Invalid file ID.
#define INVALID_FILE	0xFF

// Structure to manage the latched bytes array
typedef struct
{
	BYTE	Bytes[BYTESLATCHSIZE];
	BYTE	Index;
} BytesLatch_t;

#define LatchHasRoom(latch)			(latch.Index < BYTESLATCHSIZE)
#define ResetLatch(latch)			(latch.Index = 0)
#define InsertLatch(latch,value)	do {if (LatchHasRoom(latch)) \
										  { latch.Bytes[latch.Index]=value; \
										    latch.Index++; } \
										} while (0)	

//#define INCLUDE_SDDOS

#define EE_SYSFLAGS 0x40
#define EE_PORTBVALU 0x3f
#define EE_PORTBTRIS 0x3e

extern unsigned char configByte;
extern unsigned char CardType;
extern unsigned char blVersion;

extern volatile unsigned char globalData[];
extern volatile unsigned char globalFlag;
extern volatile BYTE globalStreaming;
extern volatile BytesLatch_t BytesLatch;

extern volatile WORD globalIndex;
extern WORD globalAmount;
extern BYTE globalDataPresent;
extern char SnapPath[];

#ifndef BOOT_ADDR
extern FIL Files[NO_FILES];
#endif

extern void at_initprocessor(void);
extern void at_process(void);
extern void InitNewCard(void);
extern void DismountCard(void);

#define MODEREAD 1
#define MODEWRITE 2


typedef void (*WORKERFN)(void);

#define WFUNC(x)  extern void wfn##x(void); const WORKERFN WFN_##x = wfn##x;



#ifdef INCLUDE_SDDOS

typedef struct
{
   char 			filename[MAX_PATH_LEN+1];
   unsigned char 	attribs;
   WORD				HeaderSize;
   DISK_GEOM		geom;
   WORD             CurrentCyl;
   BYTE             CurrentHead;
   BYTE             CurrentSecNo;
   DWORD			CurrentLBA;
   BYTE				LBAMode;
}
imgInfo;

#define SDDOS_NO_DRIVES		4
#define SDDOS_SECTOR_SIZE	256
#define INVALID_DRIVE       0xFF

#define DRIVE_VALID(dvalidno)			(driveInfo[dvalidno].attribs != INVALID_DRIVE)
#define INVALIDATE_DRIVE(dvalidno)		{memset(&driveInfo[dvalidno], 0x00, sizeof(imgInfo)); driveInfo[dvalidno].attribs = INVALID_DRIVE;}

#define SET_LBAMODE(drvno)				(driveInfo[drvno].LBAMode = 1)
#define SET_CHRNMODE(drvno)				(driveInfo[drvno].LBAMode = 0)
#endif

void SaveSettings(void);
void LoadSettings(void);

#endif // __MMC2_H
