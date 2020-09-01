#ifndef __WFN_H
#define __WFN_H

#include "ff.h"

#ifdef _INCLUDE_WFUNC_

WFUNC(DirectoryOpen)
WFUNC(DirectoryRead)
WFUNC(SetCWDirectory)
WFUNC(GetCWDirectory)
WFUNC(MakeDirectory)
WFUNC(RemoveDirectory)
WFUNC(SetSnapPath)
WFUNC(FileOpenRead)
WFUNC(FileOpenWrite)
WFUNC(FileGetInfo)
WFUNC(FileOpenAuto)
WFUNC(FileOpenOverwrite)
WFUNC(FileOpenSnapR)
WFUNC(FileOpenSnapW)
WFUNC(FileOpenStreamW)
WFUNC(FileCopyRename)
WFUNC(FileRead)
WFUNC(FileWrite)
WFUNC(FileClose)
WFUNC(FileDelete)
WFUNC(FileRewind)
WFUNC(FileSeek)
WFUNC(FileTell)
WFUNC(GetFreeFID)
WFUNC(SyncDisk)
WFUNC(ExecuteArbitrary)

#ifdef INCLUDE_SDDOS
WFUNC(OpenSDDOSImg)
WFUNC(OpenSDDOSImgCreate)
WFUNC(ReadSDDOSSect)
WFUNC(WriteSDDOSSect)
WFUNC(ReadSDDOSNextSec)
WFUNC(ValidateSDDOSDrives)
WFUNC(SerialiseSDDOSDrives)
WFUNC(UnmountSDDOSImg)
WFUNC(GetSDDOSImgNames)
WFUNC(ImgSeek)
//WFUNC(GetImgGeometry)
WFUNC(CreateImg)
#endif
WFUNC(GetCasFiletype)
WFUNC(GetDateTime)
WFUNC(SetDateTime)
WFUNC(TapeUpdate)
#endif

// These are exported for CPLD updater
BYTE fileOpen(BYTE mode, 
			  BYTE FileNo);
BYTE fileClose(BYTE FileNo);
unsigned char fileRead(void);

// Borrowed from FATFS, as static in there !
WORD load_word (const BYTE* ptr);		/*	 Load a 2-byte little-endian word */
DWORD load_dword (const BYTE* ptr);		/* Load a 4-byte little-endian word */
DWORD load_lsn (const BYTE* ptr);		/* Load a 3-byte little-endian word, OS-9 logical sector no */

#endif // __WFN_H
