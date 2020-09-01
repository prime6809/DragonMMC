#include "ff.h"

#include "dragonio.h"
#include "dragon_disk.h"
#include "mmc2def.h"
#include "mmc2.h"

#include <string.h>
#include "mmc2io.h"
#include "diskio.h"
#include "status.h"
#include "dragon_cas.h"
#include "wildcard.h"
#include "pcf2129rtc.h"
#include "TapeEmulate.h"
#include <util/delay.h>
#include "xilinx/ports.h"

BYTE res;

volatile WORD globalIndex;
WORD globalAmount;
BYTE globalDataPresent;
BYTE FDCStatus;
BYTE LastCommand;

volatile BytesLatch_t BytesLatch;

DIR     dir;
//FIL     fil;
FILINFO filinfo;
FATFS   fatfs;

FIL		Files[NO_FILES];

char	SnapPath[SNAP_PATH_LEN+1];

#define WILD_LEN	16

char	WildPattern[WILD_LEN+1];

// Directory cache for sorting.
DirCache_t	DirCache;

#ifdef INCLUDE_SDDOS

imgInfo	driveInfo[SDDOS_NO_DRIVES];
FIL		driveFILE[SDDOS_NO_DRIVES];

BYTE globalCurDrive;
DWORD globalLBAOffset;

#endif

// Flash strings.....
const __flash	char BackupExt[] 		= ".BAK";
const __flash	char SettingsDragon[]	= SETTINGS_FILE_DRAGON;
const __flash	char SettingsCoCo[]		= SETTINGS_FILE_COCO;


// use only immediately after open
extern void get_fileinfo_special(FILINFO *);

BYTE fileClose(BYTE FileNo);

FRESULT	ResolvePath(char  ToResolve[],
                    int   IsFile);

// Get the file id, parno is the latch parameter number, usually 0
BYTE GetFileID(BYTE	parno,
			   BYTE *id)
{
	BYTE result = ERROR_INVALID_FID;		// assume fail.....
	BYTE fid	= INVALID_FILE;
	
	if (parno < BytesLatch.Index) 
	{
		fid = BytesLatch.Bytes[parno];
		
		if((0 <= BytesLatch.Bytes[parno]) && (NO_FILES > BytesLatch.Bytes[parno]))
		{
			fid = BytesLatch.Bytes[parno];
			result = FR_OK;
		}
	}
	
	*id=fid;
	
	return result;
}

WORD load_word (const BYTE* ptr)	/*	 Load a 2-byte little-endian word */
{
	WORD rv;

	rv = ptr[1];
	rv = rv << 8 | ptr[0];
	return rv;
}

DWORD load_dword (const BYTE* ptr)	/* Load a 4-byte little-endian word */
{
	DWORD rv;

	rv = ptr[3];
	rv = rv << 8 | ptr[2];
	rv = rv << 8 | ptr[1];
	rv = rv << 8 | ptr[0];
	return rv;
}

DWORD load_lsn (const BYTE* ptr)	/* Load a little-endian word logical sector no */
{
	DWORD rv;
	rv = load_dword(ptr) & LSN_MASK;
	return rv;	
}

void at_initprocessor(void)
{
	BYTE	DriveID;

    globalFlag=PLAT_INVALID;    // Invalidate platform until set
	globalStreaming=0;			// Flag not streaming input
	
#ifdef INCLUDE_SDDOS
	for(DriveID = 0; DriveID < SDDOS_NO_DRIVES; DriveID++)
	{
		INVALIDATE_DRIVE(DriveID);
	}	
#endif

	InitNewCard();
}

void InitNewCard(void)
{
#ifdef _FFCONF
    log0("f_mount()=%X\n",f_mount(0,&fatfs));
#else 
    log0("f_mount()=%X\n",f_mount(&fatfs,"0:",1));
#endif
    if(PLAT_INVALID != globalFlag)
        LoadSettings();
}


void DismountCard(void)
{
	BYTE DriveID;
	BYTE FileNo;

#ifdef _FFCONF
	f_mount(0,NULL);
#else	
	f_mount(0,NULL,1);
#endif
	for(FileNo = 0; FileNo < NO_FILES; FileNo++)
	  f_close(&Files[FileNo]);

	// Force drives invalid on card eject.
#ifdef INCLUDE_SDDOS	
	for (DriveID = 0; DriveID < SDDOS_NO_DRIVES; ++DriveID)
	{
		f_close(&driveFILE[DriveID]);
		INVALIDATE_DRIVE(DriveID);
	}
#endif
}

void GetWildcard(void)
{
	int	Idx			= 0;
	int	WildPos		= -1;
	int	LastSlash	= -1;
	
	while ((Idx<strlen((const char*)globalData)) && (WildPos<0)) 
	{
		// Check for wildcard character
		if((globalData[Idx]=='?') || (globalData[Idx]=='*')) 
			WildPos=Idx;

		// Check for path seperator
		if((globalData[Idx]=='\\') || (globalData[Idx]=='/'))
			LastSlash=Idx;
			
		Idx++;
	}
	
	if(WildPos>-1)
	{
		if(LastSlash>-1)
		{
			// Path followed by wildcard
			// Terminate dir filename at last slash and copy wildcard
			globalData[LastSlash]=0x00;
			strncpy(WildPattern,(const char*)&globalData[LastSlash+1],WILD_LEN);
		}
		else
		{
			// Wildcard on it's own
			// Copy wildcard, then set path to null
			strncpy(WildPattern,(const char*)globalData,WILD_LEN);
			globalData[0]=0x00;
		}
	}
	else
	{
		// No wildcard, show all files
		strncpy_P(WildPattern,PSTR("*"),WILD_LEN);
	}
}

//
// Since FatFS uses the standard character ~ for shortening long filnames
// and this character is not typeable on a Dragon / CoCo keyboard (inverse
// up arrow), we must translate something that is typeable.
// So in this case we use up arrow and translate this to '~'.
// Note this means we can't have pathnames containing up arrow :(
//
void FixupShortPathStr(int offset,
					   char *Path)
{
	int	Idx			= 0;
		
    for(Idx = offset; Idx < strlen(Path); Idx++)
    {
        if(Path[Idx]=='^')
            Path[Idx]='~';
    }
}

#define FixupShortPath(offs)	FixupShortPathStr(offs,(char *)globalData)

// The Directory cache consists of an array of records containing the actual 
// directory entries. There is also an array of sequence pointers that contain 
// the sequence of the directory entries as a linked list.
// InsertCache works out the correct position to insert each entry in the list.

int InsertCache(int	Entry,
				int	First)
				 
{
    int Current = First;
    int Last    = CACHE_PTR_INVALID;
    int NewHead = First;
    
	// If the list is empty, then add us as the head.
    
	if(CACHE_PTR_INVALID == First) 
	{
        DirCache.FileInfoPtrs[Entry]=CACHE_PTR_INVALID;
		NewHead=Entry;
        //log0c(DEBUG_ENABLED,"Inserted file entry as head : %s\n",DirCache.FileInfo[Entry].fname);
	}
	else
    { 
        // Search list for the correct position to insert us
        // Exists either at end of chain, or when Entry < Current
        while ((DirCache.FileInfoPtrs[Current] != CACHE_PTR_INVALID) && (strcmp(DirCache.FileInfo[Entry].fname,DirCache.FileInfo[Current].fname) >= 0))
        {
            Last=Current;
            Current=DirCache.FileInfoPtrs[Current];
        }
        
        // If we are at the end of the chain, check to si if the entry should come after the last entry
        if ((CACHE_PTR_INVALID == DirCache.FileInfoPtrs[Current]) && (strcmp(DirCache.FileInfo[Entry].fname,DirCache.FileInfo[Current].fname) >= 0))
        {
            // Insert after last
            DirCache.FileInfoPtrs[Entry]=DirCache.FileInfoPtrs[Current];
            DirCache.FileInfoPtrs[Current]=Entry;
        }
        else
        {
            // Insert before current entry, check to see if current is first entry, if so make it the new head
            if(CACHE_PTR_INVALID != Last)
            {
                DirCache.FileInfoPtrs[Entry]=DirCache.FileInfoPtrs[Last];
                DirCache.FileInfoPtrs[Last]=Entry;
            }
            else
            {
                NewHead=Entry;
                DirCache.FileInfoPtrs[Entry]=First;
            }
        }     
        
        //log0c(DEBUG_ENABLED,"Inserted file entry [%02d] : %s\n",Stage,DirCache.FileInfo[Entry].fname)
    }
    
    return NewHead;
}

// Try to cache the current directory by reading directory entries into the 
// directory cache. If more entries are present than there are slots in the 
// cache, the cache is invalidated and the system drops back to the un-cached,
// un-sorted list. Currently with 22 byte directory entries, and 256 cache 
// entries + 256 integer sequence pointers the cache takes between 5K and 6K. 
int TryCache(void)
{
	int	fres;
	int	Entry;
    int FileHead        = CACHE_PTR_INVALID;
	
	DirCache.IsCached	= 1;		            // Assume we're gonna be ok.
	DirCache.CachePtr	= 0;		            // Zero pointer.
	DirCache.Entries	= 0;		            // Start empty.
	DirCache.Head		= CACHE_PTR_INVALID;	// start uninilialized
	DirCache.LastDir	= CACHE_PTR_INVALID;
    
    // Scan the directory, stop if there is an error, when there are no more entries (fname[0]=0)
    // or when we run out of slots in the cache.
    // also takes care of filtering on wildcard so  that only filered entries are added to the cache.
    // This means that even if the directory has > DIR_CACHE_SIZE entries, if the number of filtered
    // entries is smaller than DIR_CACHE_SIZE, the directory will still be sorted.
    
	fres=f_readdir(&dir, &DirCache.FileInfo[DirCache.CachePtr]);
	while((FR_OK==fres) && (DirCache.FileInfo[DirCache.CachePtr].fname[0]!=0) && (DirCache.Entries < DIR_CACHE_SIZE))
	{
	log0("%s\n",DirCache.FileInfo[DirCache.CachePtr].fname);
		if ((wildcmp(WildPattern,DirCache.FileInfo[DirCache.CachePtr].fname)) &&
            ((DirCache.FileInfo[DirCache.CachePtr].fname[0] != '_') || !HIDE_MAC))
		{
            //log0c(DEBUG_ENABLED,"Inserted cache entry %d, name=%s\n",DirCache.CachePtr,DirCache.FileInfo[DirCache.CachePtr].fname);
    
            DirCache.FileInfoPtrs[DirCache.CachePtr]=CACHE_PTR_INVALID;
			DirCache.CachePtr++;
			DirCache.Entries++;
        }
		fres=f_readdir(&dir, &DirCache.FileInfo[DirCache.CachePtr]);
	}
	
    log0c(DEBUG_ENABLED,"DirCache contains %d entries\n",DirCache.Entries);
    
	if(DirCache.Entries < DIR_CACHE_SIZE)
	{
        log0c(DEBUG_ENABLED,"Sorting <DIR> entries.....\n");
		// sort cache, directories first
		for(Entry=0; Entry < DirCache.Entries; Entry++)
		{
			if(AM_DIR == (DirCache.FileInfo[Entry].fattrib & AM_DIR))
			{
				DirCache.Head=InsertCache(Entry,DirCache.Head);
				if(CACHE_PTR_INVALID == DirCache.FileInfoPtrs[Entry]) 
					DirCache.LastDir=Entry;
			}
		}
		
        log0c(DEBUG_ENABLED,"Sorting file entries.....\n");
		// now do regular files
		for(Entry=0; Entry < DirCache.Entries; Entry++)
		{
			if(AM_DIR != (DirCache.FileInfo[Entry].fattrib & AM_DIR))
			{
                FileHead=InsertCache(Entry,FileHead);
			}
		}
        
        // Join the chain of directories and chain of files.
        // with files after directories
        if (CACHE_PTR_INVALID != DirCache.LastDir)
            DirCache.FileInfoPtrs[DirCache.LastDir]=FileHead;
        else
            DirCache.Head=FileHead;
		
        // Point to the first entry
        DirCache.CachePtr=DirCache.Head;
#if 0        
        log0c(DEBUG_ENABLED,"Cache dump: head=%d\n",DirCache.Head);
		for(Entry=0; Entry < DirCache.Entries; Entry++)
        {
            log0c(DEBUG_ENABLED,"Entry: %3.3d, Next: %3.3d, Name=%s\n",Entry,DirCache.FileInfoPtrs[Entry],DirCache.FileInfo[Entry].fname);
        }
#endif
	}
	else
	{
		// too many entries, so not cached.
		DirCache.IsCached=0;
	}

    return DirCache.IsCached;
}

void wfnDirectoryOpen(void)
{
    int openres;
	
    log0c(DEBUG_ENABLED,"wfnDirectoryOpen(%s)\n",(const char*)globalData);
   
    // Translate ^ to ~ for convenience!
    FixupShortPath(0);
    
    // Separate wildcard and path 
	GetWildcard();
   
    log0c(DEBUG_ENABLED,"wfnDirectoryOpen(%s)\n",(const char*)globalData);
   
    openres = f_opendir(&dir, (const char*)globalData);
	if (FR_OK == openres)
    {
        // If caching fails, try re-opening dir so we can read uncached.
        if(!TryCache())
            openres = f_opendir(&dir, (const char*)globalData);
    }
    
    if (FR_OK != openres)
		WriteStatus(STATUS_ERROR | openres);
    else
        WriteStatus(STATUS_COMPLETE);
	
    ClearBusy();
}

void OutputEntry(FILINFO *Entry)
{
	int len;
	int Offset = 0;

    len = (char)strlen(Entry->fname);

	if (Entry->fattrib & AM_DIR)	
	{
		Offset = 1;
		globalData[0] = '<';
	}

    strcpy((char *)&globalData[Offset], (const char*)Entry->fname);

	if (Entry->fattrib & AM_DIR)
	{
		globalData[len+1] = '>';
		globalData[len+2] = 0;
		len += 2; // brackets
	}

	// just for giggles put the attribute & filesize in the buffer
	//
	globalData[len+1] = Entry->fattrib;
	memcpy((void *)&globalData[len+2], (void*)(&Entry->fsize), sizeof(DWORD));

    WriteStatus(STATUS_COMPLETE);
	ClearBusy();
}

void wfnDirectoryRead(void)
{
	int Entry;
    
	if(DirCache.IsCached)
	{
			if(CACHE_PTR_INVALID == DirCache.CachePtr)
			{
				WriteStatus(STATUS_COMPLETE+STATUS_LAST);
				ClearBusy();
				return;
			}
			
			Entry=DirCache.CachePtr;
			DirCache.CachePtr=DirCache.FileInfoPtrs[Entry];
			
			OutputEntry(&DirCache.FileInfo[Entry]);
	}
	else
	{
		while (1)
		{
			res = f_readdir(&dir, &filinfo);
			if (res != FR_OK || !filinfo.fname[0])
			{
				// done
				WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE+STATUS_LAST);
				ClearBusy();
				return;
			}

			// Check to see if filename matches current wildcard
			//
			if(wildcmp(WildPattern,filinfo.fname))
			{
				OutputEntry(&filinfo);
				return;
			}
		}
	}
}

void wfnSetCWDirectory(void)
{
	// Translate ^ to ~ for convenience!
    FixupShortPath(0);

	res = f_chdir((const TCHAR*)globalData);
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);  
	ClearBusy();
}

void wfnGetCWDirectory(void)
{
	// Get current working directory and return it as a count + string
	res = f_getcwd((TCHAR*)globalData,GLOBUFFSIZE-2);

    log0c(DEBUG_ENABLED,"res=%2.2X, data=%s\n",(const char*)globalData);

    WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);
    ClearBusy();
}

void wfnMakeDirectory(void)
{
	// Translate ^ to ~ for convenience!
    FixupShortPath(0);

	res = f_mkdir((const TCHAR*)globalData);
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);  
	ClearBusy();
}

void wfnRemoveDirectory(void)
{
	// Translate ^ to ~ for convenience!
    FixupShortPath(0);

	res = f_unlink((const TCHAR*)globalData);
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);  
	ClearBusy();
}

FRESULT	ResolvePath(char  ToResolve[],
                    int   IsFile)
                  
{
	FRESULT	cdresult;
    char    SavePath[MAX_PATH_LEN+1];
    char    Path[MAX_PATH_LEN+1];
    char    *NamePtr = NULL;
   
    log0c(DEBUG_ENABLED,"ResolvePath(%s,%d)\n",ToResolve,IsFile);
    
    cdresult=f_getcwd(SavePath,MAX_PATH_LEN);
    log0c(DEBUG_ENABLED,"f_getcwd:cdresult=%02X\n",cdresult);
	
    strcpy(Path,ToResolve);
    log0c(DEBUG_ENABLED,"strcpy ok\n");
	
    if(IsFile)
    {
        NamePtr=(char *)strrchr_P((const char *)Path,'/');
		log0c(DEBUG_ENABLED,"NamePtr=%s\n",NamePtr);
		
        if(NULL != NamePtr)
        {
            NamePtr[0]=0x00;
        }
    }

	log0c(DEBUG_ENABLED,"Path=%s\n",Path);
    cdresult=f_chdir(Path);
    log0c(DEBUG_ENABLED,"f_chdir:cdresult=%02X\n",cdresult);
	
    log0c(DEBUG_ENABLED,"Path=%s, result=%2.2X\n",Path,cdresult);
   
    if(FR_OK == cdresult)
    {
        cdresult = f_getcwd(ToResolve,MAX_PATH_LEN);

        log0c(DEBUG_ENABLED,"ToResolve=%s, result=%2.2X\n",ToResolve,cdresult);
   
        if(IsFile)
        {
            strcat_P(ToResolve,PSTR("/"));
            strcat(ToResolve,NamePtr);
        }
    }
    log0c(DEBUG_ENABLED,"Result:ToResolve=%s\n",ToResolve);
   
    // Restore wd
    f_chdir(SavePath);
    
    return cdresult;
}

FRESULT ChangeSnapPath(char *NewPath)
{
	char	SavePath[MAX_PATH_LEN+1];
	FRESULT	cdresult;
    FRESULT change_res;
	
	change_res=FR_OK;
	
	// Get and save the current working directory
	cdresult=f_getcwd(SavePath,MAX_PATH_LEN);
	
	// if snappath empty, set snappath to default, else specified path
	if (strlen(NewPath)==0) 
		strcpy_P(NewPath,PSTR(DEFAULT_SNAPPATH));

	// Change dir to current Snap path incase supplied path is relative.
	cdresult=f_chdir(SnapPath);
	
	// Check that CD worrked, and that path is valid.
	if(FR_OK==cdresult)
	{
		// Attempt to change to supplied directory.
		cdresult=f_chdir(NewPath);
		
		// If successfull, change the snap path.
		if (FR_OK==cdresult)
			cdresult=f_getcwd(SnapPath,MAX_PATH_LEN);
		else
        {   
            strcpy_P(SnapPath,PSTR(DEFAULT_SNAPPATH));
			change_res=cdresult;
        }
	}
	else
		change_res=cdresult;
 
	// Restore working directory
	cdresult=f_chdir(SavePath);
       
    return change_res;
}

void wfnSetSnapPath(void)
{	
    res=ChangeSnapPath((char *)globalData);
    
    // Save settings
    SaveSettings();
    
	WriteStatusCond((res!=FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);
    ClearBusy();
}

void BackupFile(void)
{
	char	backup_name[MAX_PATH_LEN+1];	// backup filename :)
	char	*ptrFileName;					// pointer to filename
	char	*ptrExt;						// pointer to extension
	
	strncpy(backup_name, (const char*)globalData, MAX_PATH_LEN);
	
	ptrFileName=strrchr(backup_name, '/');	// Find last path seperator
	
	if (NULL != ptrFileName)
		ptrFileName = ptrFileName +1;		// There is a path, just consider filename
	else
		ptrFileName	= backup_name;			// no path, just a filename
		
	ptrExt = strrchr(ptrFileName, '.');		// look for dot
	
	if (NULL != ptrExt)
		strcpy_P(ptrExt, BackupExt);		// found, replace with .BAK
	else
		strcat_P(ptrExt, BackupExt);		// not found, append .BAK
		
	log0c(DEBUG_ENABLED,"backing up %s to %s\n",(const char*)globalData ,backup_name);

	f_unlink(backup_name);					// Delete backup file
	f_rename((const char*)globalData ,backup_name);					// Rename old file to .bak
}

BYTE fileOpen(BYTE mode,
			  BYTE FileNo)
{
	BYTE result;
	
    if(INVALID_FILE != FileNo)
	{
		// Fixup path from Dragon
		FixupShortPath(0);

		if ((mode & FA_WRITE) && DO_BACKUP) 
			BackupFile();
 
		log0("Opening file no %d, name : %s, mode : %02X\n",FileNo,(const char*)globalData, mode);
		result = f_open(&Files[FileNo], (const char*)globalData, mode);
		log0("result:%02X\n",result);
	}
	else
		result = ERROR_INVALID_FID;
		
	return result;
}

BYTE ValidFIDOpenFile(BYTE mode)
{
	BYTE fid;
	BYTE result;
	
	//HexDumpHead(&BytesLatch,sizeof(BytesLatch),0);
	
	result = GetFileID(0,&fid);
	
	log0("ValidFIDOpenFile:fid=%d,res=%d\n",fid,res);
	
	if (FR_OK == result)
	{
		fileClose(fid);
		result = fileOpen(mode,fid);
	}
		
	return result;	
}

void wfnFileOpenRead(void)
{
	res = ValidFIDOpenFile(FA_OPEN_EXISTING|FA_READ);
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);	
	ClearBusy();
}

void wfnFileOpenWrite(void)
{
	res = ValidFIDOpenFile(FA_CREATE_NEW|FA_WRITE);
    
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);		
	ClearBusy();
}


void FileGetInfo(BYTE FileNo)
{
	union
	{
		DWORD dword;
		char byte[4];
	}
	dwb;

	if (INVALID_FILE != FileNo)
	{
		dwb.dword = f_size(&Files[FileNo]);		//.fsize;
		globalData[0] = dwb.byte[0];
		globalData[1] = dwb.byte[1];
		globalData[2] = dwb.byte[2];
		globalData[3] = dwb.byte[3];

#ifdef _FFCONF
		dwb.dword = (DWORD)(Files[FileNo].sclust-2) * fatfs.csize + fatfs.database;
#else
		dwb.dword = (DWORD)((&Files[FileNo])->obj.sclust-2) * fatfs.csize + fatfs.database;
#endif
		globalData[4] = dwb.byte[0];
		globalData[5] = dwb.byte[1];
		globalData[6] = dwb.byte[2];
		globalData[7] = dwb.byte[3];

		dwb.dword = Files[FileNo].fptr;
		globalData[8] = dwb.byte[0];
		globalData[9] = dwb.byte[1];
		globalData[10] = dwb.byte[2];
		globalData[11] = dwb.byte[3];

		globalData[12] = filinfo.fattrib & 0x3f;
	}
}

void wfnFileGetInfo(void)
{
	BYTE fid;

	res = GetFileID(0,&fid);
	
	if (FR_OK == res)
		FileGetInfo(fid);

	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);		
	ClearBusy();
}

// Change back to root directory and open autoexec file
void wfnFileOpenAuto(void)
{
    // Change back to root
    strcpy_P((char*)&globalData[0],PSTR("/"));
    f_chdir((const TCHAR*)globalData);           
    
    log0c(DEBUG_ENABLED,"Platform=%2.2X\n",globalFlag);
    
    // Put filename in buffer
	if(PLAT_DRAGON == globalFlag)
		strcpy_P((char*)&globalData[0],PSTR(AutoFileDragon));
	else
		strcpy_P((char*)&globalData[0],PSTR(AutoFileCoCo));

    // Open file and return
	res = fileOpen(FA_OPEN_EXISTING|FA_READ,CAS_FILE);

	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);	
	ClearBusy();
}

void wfnFileOpenOverwrite(void)
{
    res = ValidFIDOpenFile(FA_CREATE_ALWAYS|FA_WRITE);
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);		
	ClearBusy();
}

void MergeSnapPath(void)
{
	char	FileName[SHORT_NAME_LEN+1];
	
	if((strlen((char *)globalData)<=SHORT_NAME_LEN) &&
	   (strstr_P((char *)globalData,PSTR("/"))==NULL))
	{
		strcpy(FileName,(char *)globalData);
		snprintf_P((char *)globalData,GLOBUFFSIZE-1,PSTR("%s/%s"),SnapPath,FileName);
	}
    log0c(DEBUG_ENABLED,"Merged snapshot path:%s\n",globalData);
}

void wfnFileOpenSnapR(void)
{
    MergeSnapPath();

    res = ValidFIDOpenFile(FA_OPEN_EXISTING|FA_READ);
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);		
	ClearBusy();
}

void wfnFileOpenSnapW(void)
{
    MergeSnapPath();

    res = ValidFIDOpenFile(FA_CREATE_ALWAYS|FA_WRITE);
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);		
	ClearBusy();
}

void wfnFileOpenStreamW(void)
{
    fileClose(CAS_FILE);
    
	res = fileOpen(FA_CREATE_NEW|FA_WRITE,CAS_FILE);

	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);	
	
	// If the file opened OK, set flags and set data port to input, so we can service it in
	// an ISR.
	if(FR_OK == res)
    {
        memset((void *)globalData,0,GLOBUFFSIZE);
        globalIndex=0;
		globalStreaming = 1;
		SelectData(); 
		SetIORead();
		EnableInt();
    }

	ClearBusy();
}

#define COPY_BUFF_SIZE	512

BYTE CopyFile(char	*Source,
			  char 	*Dest)
{
	char	Buff[COPY_BUFF_SIZE];
	FIL		SourceFile;
	FIL		DestFile;
	UINT	BytesRead;
	UINT	BytesWritten;
	
    log0c(DEBUG_ENABLED,"Copying from %s to %s\n",Source,Dest);
    
	res = f_open(&SourceFile,Source,FA_READ | FA_OPEN_EXISTING);
	if (FR_OK == res)
	{
		res = f_open(&DestFile,Dest,FA_WRITE | FA_CREATE_ALWAYS);
		if (FR_OK == res)
		{
			while((!f_eof(&SourceFile) && (FR_OK == res))) 
			{
				res = f_read(&SourceFile,&Buff[0],COPY_BUFF_SIZE,&BytesRead);
				if(FR_OK == res)
					res = f_write(&DestFile,&Buff[0],BytesRead,&BytesWritten);
                    
                log0c(DEBUG_ENABLED,"buffsize=%d, %05d bytes read, %05d bytes written\n",COPY_BUFF_SIZE,BytesRead,BytesWritten);
			}
			f_close(&DestFile);
		}
		f_close(&SourceFile);
	}
	
	return res;
}

BYTE DoCopyRename(BYTE	OpCode)
{

	char	*Source;
	char	*Dest;
	
	Source=(char *)&globalData[0];
	Dest=(char *)&globalData[strlen(Source)+1];
	
	FixupShortPathStr(0,Source);
	FixupShortPathStr(0,Dest);

	if ((strlen(Source) > MAX_PATH_LEN) ||  (strlen(Source) == 0) ||
	    (strlen(Dest) > MAX_PATH_LEN) ||  (strlen(Dest) == 0))
		res = FR_INVALID_NAME;
	else
	{
		switch (OpCode) 
		{
			case CMD_FILE_COPY: 	res = CopyFile(Source,Dest); break;
			case CMD_FILE_RENAME: 	res = f_rename(Source, Dest); break;
			default:				res = FR_OK;
		}
	}
	
	return res;
}

void wfnFileCopyRename(void)
{
	res=DoCopyRename(LastCommand);
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);	
	ClearBusy();
}


// Read 1 byte and return to caller, used by Xilinx programming code.
unsigned char fileRead(void)
{
	UINT read;
	
	f_read(&Files[CAS_FILE],(void *)globalData, 1, &read);
	
	return globalData[0];
}

void DoReadWriteFile(BYTE	IsWrite)
{
	UINT transferred;
	BYTE fid;

	if (1 < BytesLatch.Index)
	{
		globalAmount = BytesLatch.Bytes[1];
		if (globalAmount == 0)
			globalAmount = 256;

		res = GetFileID(0,&fid);
	
		if (FR_OK == res)
		{
			if (IsWrite)
				res = f_write(&Files[fid], (void*)globalData, globalAmount, &transferred);
			else
				res = f_read(&Files[fid],(void *)globalData, globalAmount, &transferred);
		}
	}
	else
		res = ERROR_INVALID_PARAM;

	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);		
	
	ClearBusy();
}

void wfnFileRead(void)
{
	DoReadWriteFile(0);
}

void wfnFileWrite(void)
{
	DoReadWriteFile(1);
}

void wfnFileRewind(void)
{
	BYTE fid;

	res = GetFileID(0,&fid);
	
	if (FR_OK == res)
		res = f_lseek(&Files[fid], 0);		// Seek file to position 0
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);	
	ClearBusy();
}

void wfnFileSeek(void)
{
	DWORD	SeekTo;
	BYTE fid;

	res = GetFileID(0,&fid);
	
	if (FR_OK == res)
	{
		// Get seek pos MSB first.
		SeekTo = ((DWORD)globalData[0] << 24) + ((DWORD)globalData[1] << 16) + ((DWORD)globalData[2] << 8UL) + (DWORD)globalData[3];
		res = f_lseek(&Files[fid], SeekTo);
	}
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);	
	ClearBusy();
}

void wfnFileTell(void)
{
	DWORD 	FPointer;
	BYTE fid;

	res = GetFileID(0,&fid);
	
	if (FR_OK == res)
	{
		FPointer=f_tell(&Files[fid]);
	
		globalData[0] = ((DWORD)(FPointer && 0xFF000000) >> 24);
		globalData[1] = ((DWORD)(FPointer && 0x00FF0000) >> 16);
		globalData[2] = ((DWORD)(FPointer && 0x0000FF00) >> 8);
		globalData[3] = ((DWORD)(FPointer && 0x000000FF)); 
	}
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);	
	ClearBusy();
}

void wfnGetFreeFID(void)
{
	BYTE Idx;
	BYTE fid = INVALID_FILE; 	// assume none found
	
	// We start from one so we never return fid 0, as the firmware exclusively uses this. 
	for(Idx = 1; Idx < NO_FILES; Idx++)
		if((INVALID_FILE == fid) && (0 == Files[Idx].obj.fs))
			fid = Idx;

	// return the found file ID to the caller.
	globalData[0] = fid;
	
	res = (INVALID_FILE == fid) ? ERROR_NO_FREE_FID : FR_OK;
	
log0("wfnGetFreeFID:fid=%d,res=%2.2X\n",fid,res);	
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);	
	ClearBusy();
}

BYTE fileClose(BYTE FileNo)
{
	f_sync(&Files[FileNo]);
	return f_close(&Files[FileNo]);
}

void wfnFileClose(void)
{
	BYTE fid;
	
	res = GetFileID(0,&fid);
	
	if (FR_OK == res)
		res = fileClose(fid);
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);	
	ClearBusy();
}

void wfnFileDelete(void)
{
	// Translate ^ to ~ for convenience!
    FixupShortPath(0);

	res = f_unlink((const TCHAR*)&globalData[0]);
	
	WriteStatusCond((res != FR_OK),(STATUS_ERROR | res),STATUS_COMPLETE);	
	ClearBusy();
}


#ifdef INCLUDE_SDDOS

// Convert a FATFS / DragonMMC to its WD17xx / WD27xx equivilent
// See WD Data sheets for details.
void SetFDCStatus(BYTE CmdType,
			      BYTE ErrorCode)
{
	BYTE WDCmdType = CmdType & WDMASK;
	
	// Type 1 commands, Restore, Seek, Step 
	if(WDCmdType == WD1)
	{
		if(ErrorCode != FR_OK)
			FDCStatus = 0x28;	// Head loaded + Seek error
		else
			FDCStatus = 0x20;	// Head loaded
			
		if((CmdType & WDPMASK) == 0)
			FDCStatus |= 0x04;	// Flag on track 0
	}
	// Type 2 commands Read sector, Write sector
	else if (WDCmdType == WD2)
	{
		FDCStatus = 0x00;
		if(IS_ERROR(ErrorCode))
		{
			switch (ErrorCode & ERROR_MASK)
			{
				case ERROR_INVALID_DRIVE	: FDCStatus = 0x10;	break;	// RNF
				case FR_WRITE_PROTECTED		: 
				case ERROR_READ_ONLY		: FDCStatus = 0x40; break; 	// Write protect
				default: 					  FDCStatus = 0x10; break;	// RNF
			}
		}
	}
	// Type 3 commands Read Address, Read track, Write track
	else if (WDCmdType == WD3)
	{
		FDCStatus = 0x00;
	}
	// Type 4 command Force interrupt
	else if (WDCmdType == WD4)
	{
		FDCStatus = 0x00;
	}
	// Invalid / no WD error
	else
	{
		FDCStatus = 0x00;
	}
	//log0c(DEBUG_ENABLED,"SetFDCStatus(%d,%02X), FDCStatus: %02X\n",CmdType,ErrorCode,FDCStatus);
}

// Tryto find a Dragondos disk info block at offsets for a DS disk, then an SS

#define MIN_DDOS_SIZESS	((DWORD)DDOS_TRKSIZE_SS * 20UL) + (DWORD)DD_GEOM_OFFSET + (DWORD)sizeof(DISK_GEOM)
#define MIN_DDOS_SIZEDS	((DWORD)DDOS_TRKSIZE_DS * 20UL) + (DWORD)DD_GEOM_OFFSET + (DWORD)sizeof(DISK_GEOM)

BYTE TryDragonDos(BYTE 		ImageNo)

{
	DISK_GEOM	GeomSS;	    // Geometry from track 20 (if single sided)
    DISK_GEOM	GeomDS;	    // Geometry from track 20 (if double sided)
	UINT		BytesRead;
    BYTE        Result = 0; // Assume we will fail!
	
	SET_GEOM_INVALID(&driveInfo[ImageNo].geom);
	SET_GEOM_INVALID(&GeomSS);
	SET_GEOM_INVALID(&GeomDS);

    // Try to read geometry from single sided position
	if(f_size(&driveFILE[ImageNo]) > MIN_DDOS_SIZESS)
	{
		if(FR_OK == f_lseek(&driveFILE[ImageNo],(((DWORD)DDOS_TRKSIZE_SS * 20UL) + (DWORD)DD_GEOM_OFFSET)))
			f_read(&driveFILE[ImageNo],&GeomSS,sizeof(GeomSS),&BytesRead);
	}
	
    // Try to read geometry from double sided position 
	if(f_size(&driveFILE[ImageNo]) > MIN_DDOS_SIZEDS)
	{
		if(FR_OK == f_lseek(&driveFILE[ImageNo],(((DWORD)DDOS_TRKSIZE_DS * 20UL) + (DWORD)DD_GEOM_OFFSET)))
			f_read(&driveFILE[ImageNo],&GeomDS,sizeof(GeomDS),&BytesRead);
    }
	
    if (GEOM_VALID(&GeomDS) && GEOM_VALID_DD(&GeomDS)) 
    {
        memcpy(&driveInfo[ImageNo].geom,&GeomDS,sizeof(GeomDS));
        Result = 1;
    }
    else if (GEOM_VALID(&GeomSS) && GEOM_VALID_DD(&GeomSS))
    {
        memcpy(&driveInfo[ImageNo].geom,&GeomSS,sizeof(GeomDS));
        Result = 1;
    }
    
    return Result;
}

BYTE tryOpenImage(BYTE 		ImageNo,
				  uint8_t	CanCreate)
{
	BYTE 		DriveNo;
	VDK_HEADER	Head;
	UINT		read;
	FILINFO		Info;
    
	// Note file will be created if it does not exist, this allows us to create images
	// First check to see if image already mounted, if so return error and exit.
	for (DriveNo = 0; DriveNo < SDDOS_NO_DRIVES; ++DriveNo)
	{
		if ((ImageNo != DriveNo) && (memcmp((void*)&driveInfo[ImageNo], (void*)&driveInfo[DriveNo], sizeof(imgInfo)) == 0))
		{
			// warning - already mounted
			return STATUS_ERROR + ERROR_ALREADY_MOUNT; // 0x4a;
		}
	}
	
	// Close image file if another file already open for this drive
	f_close(&driveFILE[ImageNo]);

    // Try to open the image, if CanCreate is not true the image must already exist, 
	// if it is true the image will be created if it does not exist.
	if (CanCreate)
		res = f_open(&driveFILE[ImageNo], (const TCHAR*)&driveInfo[ImageNo].filename,FA_READ | FA_WRITE | FA_OPEN_ALWAYS);
	else
		res = f_open(&driveFILE[ImageNo], (const TCHAR*)&driveInfo[ImageNo].filename,FA_READ | FA_WRITE | FA_OPEN_EXISTING);

	log0c(DEBUG_ENABLED,"tryOpenImage(%d,%d): res=%02X\n",ImageNo,CanCreate,res);
	
	// Oops error opening image file bomb out
	if (FR_OK != res) 
	{
		return STATUS_ERROR | res;
	}

	res = f_stat((const TCHAR*)&driveInfo[ImageNo].filename,&Info);
	if (FR_OK != res) 
		return STATUS_ERROR | res;

    // Get image attributes
    driveInfo[ImageNo].attribs=Info.fattrib;
    
	// assume no header
	driveInfo[ImageNo].HeaderSize=0;
	
	// Try to read VDK header, in future if no VDK header found will try
	// interpreting first sector as OS-9 parameter block.
	if(FR_OK == f_read(&driveFILE[ImageNo],&Head,sizeof(Head),&read))
	{
		if(VALID_VDK(&Head))
		{
			log0c(DEBUG_ENABLED,"Found valid VDK header, size=%d\n",Head.Headersize);
			// Valid VDK header get geometry from it
			SET_GEOM(&driveInfo[ImageNo].geom,Head.NoTracks,(Head.NoSides*DDOS_SECTORS_SS));
			driveInfo[ImageNo].HeaderSize=Head.Headersize;
			//HexDumpHeadc(DEBUG_ENABLED,(uint8_t *)&Head,sizeof(Head),0);
			//HexDumpc(DEBUG_ENABLED,(uint8_t *)&driveInfo[ImageNo],sizeof(driveInfo[ImageNo]),0);
		}
        else if (TryDragonDos(ImageNo))
        {
            log0c(DEBUG_ENABLED,"Found headerless DragonDOS image of %d tracks &d heads\n",driveInfo[ImageNo].geom.Tracks,driveInfo[ImageNo].geom.SecPerTrack);
        }
		else if ((Info.fsize % DDOS_TRKSIZE_SS) == 0)
		{
			log0c(DEBUG_ENABLED,"Found headerless image of %d tracks\n",(Info.fsize / DDOS_TRKSIZE_SS));
			
			SET_GEOM(&driveInfo[ImageNo].geom,(Info.fsize / DDOS_TRKSIZE_SS),DDOS_SECTORS_SS);
			driveInfo[ImageNo].HeaderSize=0;
		}
		else
		{	
			log0c(DEBUG_ENABLED,"Failed to find valid disk image\n");
            log0c(DEBUG_ENABLED,"Image size = %ul\n",Info.fsize);
            
			// No VDK header assume default disk size 
			SET_GEOM(&driveInfo[ImageNo].geom,DDOS_DEF_TRACKS,DDOS_SECTORS_SS);
			driveInfo[ImageNo].HeaderSize=0;
			
			// Invalid image, for the time being insist on vdk
			return STATUS_ERROR | ERROR_INVALID_IMAGE;
		}
	}

	return STATUS_COMPLETE;
}

void OpenSDDOSImgCommon(uint8_t	CanCreate)
{
	// globalData[0] = drive number 0..3
	// globalData[1]... image filename
	
	BYTE    error;
	BYTE    id = globalData[0] & 3;
	char    ImageFileName[MAX_PATH_LEN+1];

	// Translate ^ to ~ for convenience!
    FixupShortPath(1);

    // Get working directory
    res = f_getcwd(ImageFileName,MAX_PATH_LEN);
    
    if((strlen(ImageFileName)>0) && (ImageFileName[strlen(ImageFileName)-1]!='/'))
    {
        strcat_P(ImageFileName,PSTR("/"));
    }
    strncat(ImageFileName,(char *)&globalData[1],MAX_PATH_LEN);
    ResolvePath(ImageFileName,1);
    
	// Zero out driveinfo and copy filename to it
	memset(&driveInfo[id], 0, sizeof(imgInfo));
	strncpy((char*)&driveInfo[id].filename, (const char*)ImageFileName, MAX_PATH_LEN);

	log0c(DEBUG_ENABLED,"wfnOpenSDDOSImg():%s\n",driveInfo[id].filename);

	error = tryOpenImage(id,CanCreate);
	if (error & STATUS_ERROR)
	{
		// fatal error range
		INVALIDATE_DRIVE(id);
	}

	// always save - even if there was an error
	// we may have nullified a previously valid slot.
	SaveSettings();

	WriteStatus(error);
	ClearBusy();
}

// Open a DOS image, file must already exist.
void wfnOpenSDDOSImg(void)
{
	OpenSDDOSImgCommon(0);
}

// Open a DOS image, file created if it does not already exist.
void wfnOpenSDDOSImgCreate(void)
{
	OpenSDDOSImgCommon(1);
}

// Return an error if drive not mounted!
BYTE SDDOS_seek_old(BYTE	drive,		// Drive ID
					DWORD	LBA,		// LBA of sector to access
					BYTE	offset)		// Offset within sector, usually 0
{
	DWORD	fpos;
	BYTE	result = STATUS_ERROR | ERROR_INVALID_DRIVE;
	
	log0c(DEBUG_ENABLED,"SDDOS_seek(%d,%08X,%02X) head=%u\n",drive,LBA,offset,driveInfo[drive].HeaderSize);
	if(DRIVE_VALID(drive))
	{
		fpos=(LBA * DDOS_SECTOR_SIZE)+offset+driveInfo[drive].HeaderSize;

		result = f_lseek(&driveFILE[drive],fpos);
	}

	return result;
}

BYTE SDDOS_fileseek(BYTE	drive)
{
    DWORD   fpos;
	BYTE	result = STATUS_ERROR | ERROR_INVALID_DRIVE;

	if(DRIVE_VALID(drive))
	{
		if (driveInfo[drive].LBAMode)
		{
			log0c(DEBUG_ENABLED,"SDDOS_fileseek(%d,%lu) HeaderSize=%lu\n",
				drive,driveInfo[drive].CurrentLBA,
				(DWORD)driveInfo[drive].HeaderSize);
				
			fpos = 	((DWORD)driveInfo[drive].CurrentLBA * (DWORD)DDOS_SECTOR_SIZE) + (DWORD)driveInfo[drive].HeaderSize;
		}
		else
		{
			log0c(DEBUG_ENABLED,"SDDOS_fileseek(%d,c=%d,h=%d,r=%d) HeaderSize=%u ",drive,
				driveInfo[drive].CurrentCyl, driveInfo[drive].CurrentHead, driveInfo[drive].CurrentSecNo,driveInfo[drive].HeaderSize);
			//log0c(DEBUG_ENABLED,"Geom:Tracks=%d, SpT=%d\n",  driveInfo[drive].geom.Tracks , driveInfo[drive].geom.SecPerTrack );

			fpos =  ((DWORD)driveInfo[drive].CurrentCyl * (DWORD)driveInfo[drive].geom.SecPerTrack * (DWORD)DDOS_SECTOR_SIZE) +
					((DWORD)driveInfo[drive].CurrentHead * (DWORD)(driveInfo[drive].geom.SecPerTrack / 2) * (DWORD)DDOS_SECTOR_SIZE) +
					((DWORD)(driveInfo[drive].CurrentSecNo) * (DWORD)DDOS_SECTOR_SIZE) + 
					(DWORD)driveInfo[drive].HeaderSize;
		}
	
        if (fpos < f_size(&driveFILE[drive])) 
            result = f_lseek(&driveFILE[drive],fpos);         
        else
            result = FR_INVALID_PARAMETER; 
            
        log0c(DEBUG_ENABLED,"fpos=%lu\n",fpos);
    }
    return result;
}


// Places drive file at beginning of track
BYTE SDDOS_seek_track(BYTE	drive,		// Drive ID
                      BYTE  track)
{                      
	BYTE	result = STATUS_ERROR | ERROR_INVALID_DRIVE;
	
	log0c(DEBUG_ENABLED,"SDDOS_seek_track(%d,%d)\n",drive,track);
	if(DRIVE_VALID(drive))
	{
		SET_CHRNMODE(drive);
        driveInfo[drive].CurrentCyl 	= track;
        driveInfo[drive].CurrentHead	= 0;
		driveInfo[drive].CurrentSecNo	= 0;
		
        if((track <  driveInfo[drive].geom.Tracks) || (0 == track))
            result = FR_OK;
        else
            result = FR_INVALID_PARAMETER;  // Seek beyond end of image
	}

	return result;
}

void wfnReadSDDOSSect(void)
{
	BYTE 	result = 0x00;
	UINT	bytes_read;
	
	log0c(DEBUG_ENABLED,"wfnReadSDDOSSect(%d)\n",globalCurDrive);
/*	if (driveInfo[globalCurDrive].LBAMode)
	{
		log0c(DEBUG_ENABLED,"wfnReadSDDOSSect() Drive=%d, LBA=%ud\n",
			globalCurDrive,
			driveInfo[globalCurDrive].CurrentLBA) 
	}
	else
	{
		log0c(DEBUG_ENABLED,"wfnReadSDDOSSect() Drive=%d, C=%d, H=%d, S=%d, offset=%u\n",
			globalCurDrive,
			driveInfo[globalCurDrive].CurrentCyl, 
			driveInfo[globalCurDrive].CurrentHead, 
			driveInfo[globalCurDrive].CurrentSecNo,
			driveInfo[globalCurDrive].HeaderSize);
	}
*/  //log0c(DEBUG_ENABLED,"attrib=%02X\n",driveInfo[globalCurDrive].attribs);
	
	if (DRIVE_VALID(globalCurDrive))
	{
		result = SDDOS_fileseek(globalCurDrive);
		if(FR_OK==result) 
		{
			result = f_read(&driveFILE[globalCurDrive], (void *)&globalData[0], SDDOS_SECTOR_SIZE, &bytes_read);
		}

		if (RES_OK == result)
			result = STATUS_COMPLETE;
		else
			result |= STATUS_ERROR;
	}
	else
		result = STATUS_ERROR | ERROR_INVALID_DRIVE;

	//HexDumpHead(&globalData[0], SDDOS_SECTOR_SIZE,0);
	
	SetFDCStatus(WD2,result);
	WriteStatus(result);
	ClearBusy();
}

void wfnWriteSDDOSSect(void)
{
	BYTE result = FR_OK;
	UINT bytes_written;

	log0c(DEBUG_ENABLED,"wfnWriteSDDOSSect(%d)\n",globalCurDrive);
	
	/*log0c(DEBUG_ENABLED,"wfnWriteSDDOSSect() Drive=%d, LBA=%d, Track=%u Sector=%u,offset=%u\n",globalCurDrive,globalLBAOffset, \
	(globalLBAOffset /  driveInfo[globalCurDrive].geom.Tracks), (globalLBAOffset % driveInfo[globalCurDrive].geom.Tracks), \
	driveInfo[globalCurDrive].HeaderSize);
	*/
	
	if (DRIVE_VALID(globalCurDrive))
	{
		// Check for read only
		if (driveInfo[globalCurDrive].attribs & FA_READ)
		{
			// read-only
			result = (STATUS_ERROR | ERROR_READ_ONLY);
		}

		if (FR_OK == result) 
			result = SDDOS_fileseek(globalCurDrive);

		if(FR_OK==result)
		{
			result = f_write(&driveFILE[globalCurDrive], (void *)&globalData[0], SDDOS_SECTOR_SIZE, &bytes_written);
		}

		// invalidate the drive on error
		if(FR_OK != result)
		{
			//INVALIDATE_DRIVE(globalCurDrive);
			result |= STATUS_ERROR;
		}
	}
	else
		result = STATUS_ERROR | ERROR_INVALID_DRIVE;

	SetFDCStatus(WD3,result);
	WriteStatus(result);
	ClearBusy();
}

void wfnReadSDDOSNextSec(void)
{
	globalLBAOffset++;
	wfnReadSDDOSSect();
}

void wfnValidateSDDOSDrives(void)
{
	BYTE DriveID;
	for (DriveID = 0; DriveID < SDDOS_NO_DRIVES; ++DriveID)
	{
		if (!DRIVE_VALID(DriveID) || (tryOpenImage(DriveID,0) & 0x40))
		{
//			INVALIDATE_DRIVE(DriveID);
		}
	}

	SaveSettings();

	WriteStatus(STATUS_COMPLETE);
	ClearBusy();
}


void wfnSerialiseSDDOSDrives(void)
{
   SaveSettings();
   WriteStatus(STATUS_COMPLETE);
   ClearBusy();
}

void wfnUnmountSDDOSImg(void)
{
   BYTE	DriveID = globalData[0] & 3;
   
   log0c(DEBUG_ENABLED,"wfnUnmountSDDOSImg: %d\n",DriveID);
   
   f_close(&driveFILE[DriveID]);
   
   INVALIDATE_DRIVE(DriveID);
   
   SaveSettings();
   WriteStatus(STATUS_COMPLETE);
   ClearBusy();
}


void wfnGetSDDOSImgNames(void)
{
	BYTE DriveID;
	BYTE FnIdx, GlobalIdx = 0;
    
    BYTE FirstID    = globalData[0] & 3;
    BYTE LastID     = globalData[1] & 3;
   
    // Swap them if wrong way round!
    if(LastID < FirstID)
    {
        DriveID = LastID;
        LastID = FirstID;
        FirstID = DriveID;
    }
        
	for (DriveID = FirstID; DriveID < LastID+1; ++DriveID)
	{
		if (DRIVE_VALID(DriveID))
		{
			FnIdx = 0;

//			while(driveInfo[DriveID].filename[FnIdx] && FnIdx < 12)
			while(FnIdx < strlen(driveInfo[DriveID].filename))
			{
				globalData[GlobalIdx] = driveInfo[DriveID].filename[FnIdx];
				++FnIdx;
				++GlobalIdx;
			}
		}
		globalData[GlobalIdx] = 0;
		++GlobalIdx;
	}

	WriteStatus(STATUS_COMPLETE);
	ClearBusy();
}

void wfnImgSeek(void)
{
	BYTE Drive = globalData[0] & 3;
	BYTE Track = globalData[1];

	log0c(DEBUG_ENABLED,"wfnImgSeek Drive:%d, Track:%d\n",Drive,Track);
	
	SetFDCStatus(WD1 | (Track & WDPMASK),SDDOS_seek_track(Drive,Track));
	
	WriteStatus(STATUS_COMPLETE);
	ClearBusy();
}

void wfnCreateImg(void)
{
	BYTE Drive      = globalData[0] & 3;                    // Byte 0 drive
    WORD Tracks     = load_word((BYTE *)&globalData[1]);    // Byte 1,2 track count.
    BYTE Heads      = globalData[3];                        // Byte 3 head count
    BYTE Sectors    = globalData[4];                        // Byte 4 sectors / track
    DWORD MaxLSN; 
    DWORD LSN;
    UINT bytes_written;
    
    // Calculate the maximum logical sector no
    MaxLSN = Tracks * Heads * Sectors;
    
    log0c(DEBUG_ENABLED,"wfnCreateImg: Tracks=%d, Heads=%d, Sides=%d, MaxLSN=%d\n",Tracks,Heads,Heads,MaxLSN);
    
    //HexDumpHeadc(DEBUG_ENABLED,(uint8_t *)globalData,16,0);
    
    // Modify the driveInfo for the drive
    SET_GEOM(&driveInfo[Drive].geom,Tracks,(Sectors*Heads));
    
    // Seek to that sector, this will extend the file if needed.
	//SDDOS_seek(Drive,MaxLSN,0);
    driveInfo[Drive].CurrentSecNo=0;
    driveInfo[Drive].CurrentHead=0;
    driveInfo[Drive].CurrentCyl=Tracks+1;
    SDDOS_fileseek(Drive);

    // Clear a sectors worth of 0xFF, as RSDOS assumes this is written, DragonDos doesn't care!
    memset((void *)globalData,0xFF,SDDOS_SECTOR_SIZE);
    
    // Fill all sectors
    for(LSN=0; LSN < MaxLSN; LSN++)
    {
        f_lseek(&driveFILE[Drive],(LSN * DDOS_SECTOR_SIZE)+driveInfo[Drive].HeaderSize);
        f_write(&driveFILE[Drive], (void *)&globalData[0], SDDOS_SECTOR_SIZE, &bytes_written);
    }
    
    // Sync the disk and save settings
    f_sync(&driveFILE[Drive]);
   	SaveSettings();

	WriteStatus(STATUS_COMPLETE);
	ClearBusy();
}

#endif

void SaveSettings(void)
{
    BYTE    DriveID;
    UINT    temp;
    FIL     Settings;
    
    // Put filename in buffer
	if(PLAT_DRAGON == globalFlag)
		strcpy_P((char*)&globalData[0],SettingsDragon);
	else
		strcpy_P((char*)&globalData[0],SettingsCoCo);

	log0("Writing %s\n",globalData);

	res = f_open(&Settings, (const TCHAR*)globalData, FA_OPEN_ALWAYS|FA_WRITE);
	if (FR_OK == res)
	{     
        f_write(&Settings, (const void*)SnapPath, sizeof(SnapPath), &temp);
#ifdef INCLUDE_SDDOS
        for (DriveID = 0; DriveID < SDDOS_NO_DRIVES; ++DriveID)
        {
            f_write(&Settings, (const void*)&driveInfo[DriveID].filename, sizeof(driveInfo[DriveID].filename), &temp);
            log0c(DEBUG_ENABLED,"Image %d, name=%s\n",DriveID,driveInfo[DriveID].filename);
        }
#endif
		f_close(&Settings);
	}
}

void LoadSettings()
{
    BYTE    DriveID;
    UINT    temp;
    FIL     Settings;
    
    // Put filename in buffer
	if(PLAT_DRAGON == globalFlag)
		strcpy_P((char*)&globalData[0],SettingsDragon);
	else
		strcpy_P((char*)&globalData[0],SettingsCoCo);

	log0("Reading %s\n",globalData); 

	res = f_open(&Settings, (const TCHAR*)globalData, FA_READ|FA_OPEN_EXISTING);
	if (FR_OK == res)
	{
        f_read(&Settings, (void *)SnapPath, sizeof(SnapPath), &temp);
        log0("Snappath : %s\n",SnapPath);
#ifdef INCLUDE_SDDOS
        for (DriveID = 0; DriveID < SDDOS_NO_DRIVES; ++DriveID)
        {
            f_read(&Settings, (void *)&driveInfo[DriveID].filename, sizeof(driveInfo[DriveID].filename), &temp);
            log0c(DEBUG_ENABLED,"Image %d, name=%s\n",DriveID,driveInfo[DriveID].filename);
            tryOpenImage(DriveID,0);
        }
#endif
		f_close(&Settings);
        
        // Check that loaded path exists, if not set default.
        if(FR_OK != ResolvePath(SnapPath,0)) 
            strcpy_P(SnapPath,PSTR(DEFAULT_SNAPPATH));
            
    }
    else    // Settings failed to open / read 
    {
        strcpy_P(SnapPath,PSTR(DEFAULT_SNAPPATH));
        for (DriveID = 0; DriveID < SDDOS_NO_DRIVES; ++DriveID)
        {
            INVALIDATE_DRIVE(DriveID);
        }
        SaveSettings();
    }
}

void wfnGetCasFiletype(void)
{
	DWORD				old_fp;
	CASFilenameBlock	FNBlock;
	BYTE				last_byte;
	UINT 				read;
	
	// Assume that the test will fail !
	globalData[0]=0xFF;

#ifdef _FFCONF
	if(!(Files[CAS_FILE].fs==0))
#else	 
	if(!(Files[CAS_FILE].obj.fs==0))
#endif
	{
		// Get current filepointer, save it till later
		old_fp=f_tell(&Files[CAS_FILE]);			
		
		// Keep reading bytes until we hit a block beggining byte
		do
		{
			f_read(&Files[CAS_FILE],&last_byte,1,&read);
		}
		while(!f_eof(&Files[CAS_FILE]) && (CASBlockBegin != last_byte));
		
		// If we are not at the end of file, read a filename block and
		// copy filetype to globalData[0]
		if (!f_eof(&Files[CAS_FILE]))
		{
			f_read(&Files[CAS_FILE],&FNBlock,sizeof(FNBlock),&read);
            globalData[0]=FNBlock.FileType;
            memcpy((void *)&globalData[1],&FNBlock,sizeof(FNBlock));
            
            log0("Filetype=%d, Load=%4X, Entry=%4x\n",FNBlock.FileType,FNBlock.LoadAddress,FNBlock.ExecAddress);
            //HexDump((const uint8_t *)&globalData[1],sizeof(FNBlock),0);
		}
		// Restore old filepointer.
		f_lseek(&Files[CAS_FILE],old_fp);
	}

	WriteStatus(STATUS_COMPLETE);
	ClearBusy();	
}

// Force sync any file buffers annd then  disk
void wfnSyncDisk(void)
{
	BYTE	DiskNo;
	BYTE	FileNo;
	
	// General file
	for(FileNo = 0; FileNo < NO_FILES; FileNo++)
		f_sync(&Files[FileNo]);
	
	// Disk files
	for(DiskNo=0; DiskNo < SDDOS_NO_DRIVES; DiskNo++)
		f_sync(&driveFILE[DiskNo]);
	
	WriteStatus(STATUS_COMPLETE);
	ClearBusy();	
}

// YYYY-MM-DD hh:mm:ss
void wfnGetDateTime(void)
{
    rtcUpdate();
    
    snprintf_P((char *)&globalData[0],32,PSTR("%s"),rtcDateTimeASCII());
    
	WriteStatus(STATUS_COMPLETE);
    ClearBusy();
}

void wfnSetDateTime(void)
{
    if(UpdateRTCFromASCII((char *)&globalData[0]))
    {
        WriteStatus(STATUS_COMPLETE);
	}
    else
    {
        WriteStatus(STATUS_ERROR | ERROR_INVALID_TIME);
    }
    
	ClearBusy();	
}

#define MK_WORD(x,y) ((WORD)(x)<<8|(y))

// Read Eeprom
#define COM_RE MK_WORD('R','E')

// Write Eeprom
#define COM_WE MK_WORD('W','E')


void wfnExecuteArbitrary(void)
{
   if (globalAmount == 0 && globalDataPresent == 0)
   {
      WriteStatus(STATUS_ERROR | ERROR_NO_DATA);
	  ClearBusy();
      return;
   }

   switch (load_word((const BYTE *)&globalData[0]))
   {
   case COM_RE: // read eeprom
      {
         // globalData[2] = start offset, [3] = count

         WORD start = (WORD)globalData[2];
         WORD end = start + (WORD)globalData[3];

         WORD i, n = 0;
         for (i = start; i < end; ++i, ++n)
         {
            globalData[n] = ReadEEPROM(i);
         }

         WriteStatus(STATUS_COMPLETE);
  	     ClearBusy();
      }
      break;

   case COM_WE: // write eeprom
      {
         // globalData[2] = start offset, [3] = count

         WORD start = (WORD)globalData[2];
         WORD end = start + (WORD)globalData[3];

         WORD i, n = 4;
         for (i = start; i < end; ++i, ++n)
         {
            WriteEEPROM(i,globalData[n]);
         }

         WriteStatus(STATUS_COMPLETE);
 	     ClearBusy();
      }
      break;
   }
}

void wfnTapeUpdate(void)
{
    CPLDUpdateAsOutput();
    
	log0("emulating tape output\n");
//	log0("wait.....\n");
//    _delay_ms(1000);
//    log0("go....\n");
	
	if (LastCommand == CMD_CAS_EMULATE) 
		strcpy_P((char *)globalData,PSTR(TapeFile));
	else
		strcpy_P((char *)globalData,PSTR(TapeFile2));
		
    TapeOut();

    WriteStatus(STATUS_COMPLETE);
    ClearBusy();
}
