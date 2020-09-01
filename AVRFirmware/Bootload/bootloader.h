/*
   bootloader.h
*/

#ifndef _BOOTLOADER
#define _BOOTLOADER

//#include "integer.h"

//						"0123456789A"
#define BootFileName	"DGNMMC.BIN"

typedef BYTE DSTATUS;

typedef struct
{
	BYTE	blmajor;
	BYTE    blminor;
	char    bl_compile[];
} bootdata_t;

extern const __flash1 bootdata_t *BLD;
#endif