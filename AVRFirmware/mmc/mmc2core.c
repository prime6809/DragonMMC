#include "ff.h"

#include "dragon_disk.h"
#include "mmc2def.h"
#include "mmc2.h"
#include "mmc2io.h"
#include "status.h"

#include "bootload/bootloader.h"

#define _INCLUDE_WFUNC_
#include "mmc2wfn.h"

#include <string.h>

#define LOG_CMD		1
#define LOG_DATAIN	0

///extern unsigned char configByte;
//extern unsigned char blVersion;

//extern WORD globalIndex;
//extern WORD globalAmount;
//extern BYTE globalDataPresent;
//extern char SnapPath[];
//extern FIL fil;

static BYTE LatchedData;

static BYTE LatchedAddress;
static BYTE LatchedAddressLast;

#ifdef INCLUDE_SDDOS

extern BYTE globalCurDrive;
extern DWORD globalLBAOffset;
extern WORD globalCyl;         // Cylinder
extern BYTE globalHead;        // Head
extern BYTE globalSector;      // Sector no
extern BYTE FDCStatus;
extern BYTE LastCommand;

extern void unmountImg(BYTE drive);

extern imgInfo driveInfo[];

#endif

extern unsigned char CardType;
extern unsigned char disk_initialize (BYTE);


// cache of the value written to port 0x0e
//BYTE byteValueLatch;

#ifdef LOG_CMD
void log_cmd(BYTE	cmd)
{
	switch(cmd)
	{
		case	CMD_DIR_OPEN: 
			log0("[%02X] CMD_DIR_OPEN\n",cmd); break;
		case	CMD_DIR_READ: 
			log0("[%02X] CMD_DIR_READ\n",cmd); break;
		case	CMD_DIR_CWD: 
			log0("[%02X] CMD_DIR_CWD\n",cmd); break;
		case	CMD_DIR_GETCWD: 
			log0("[%02X] CMD_DIR_GETCWD\n",cmd); break;
		case	CMD_DIR_MAKE: 
			log0("[%02X] CMD_DIR_MAKE\n",cmd); break;
		case	CMD_DIR_REMOVE: 
			log0("[%02X] CMD_DIR_REMOVE\n",cmd); break;
			
		case	CMD_DIR_SET_SNAPPATH: 
			log0("[%02X] CMD_DIR_SET_SNAPPATH\n",cmd); break;
		case	CMD_DIR_GET_SNAPPATH: 
			log0("[%02X] CMD_DIR_GET_SNAPPATH\n",cmd); break;

		case	CMD_FILE_CLOSE: 
			log0("[%02X] CMD_FILE_CLOSE\n",cmd); break;
		case	CMD_FILE_OPEN_READ: 
			log0("[%02X] CMD_FILE_OPEN_READ\n",cmd); break;
		case	CMD_FILE_OPEN_IMG: 
			log0("[%02X] CMD_FILE_OPEN_IMG\n",cmd); break;
		case	CMD_FILE_OPEN_WRITE: 
			log0("[%02X] CMD_FILE_OPEN_WRITE\n",cmd); break;
		case	CMD_FILE_DELETE: 
			log0("[%02X] CMD_FILE_DELETE\n",cmd); break;
		case	CMD_FILE_GETINFO: 
			log0("[%02X] CMD_FILE_GETINFO\n",cmd); break;
		case	CMD_FILE_OPENAUTOD: 
			log0("[%02X] CMD_FILE_OPENAUTOD\n",cmd); break;
		case	CMD_FILE_OPENAUTOC: 
			log0("[%02X] CMD_FILE_OPENAUTOC\n",cmd); break;		
		case	CMD_FILE_OPEN_OVERWRITE: 
			log0("[%02X] CMD_FILE_OPEN_OVERWRITE\n",cmd); break;
		case	CMD_FILE_OPEN_SNAPR: 
			log0("[%02X] CMD_FILE_OPEN_SNAPR\n",cmd); break;
		case	CMD_FILE_OPEN_SNAPW: 
			log0("[%02X] CMD_FILE_OPEN_SNAPW\n",cmd); break;
		case	CMD_FILE_OPEN_STREAMR: 
			log0("[%02X] CMD_FILE_OPEN_STREAMR\n",cmd); break;
		case	CMD_FILE_OPEN_STREAMW: 
			log0("[%02X] CMD_FILE_OPEN_STREAMW\n",cmd); break;
		case	CMD_FILE_COPY: 
			log0("[%02X] CMD_FILE_COPY\n",cmd); break;
		case	CMD_FILE_RENAME: 
			log0("[%02X] CMD_FILE_RENAME\n",cmd); break;
		case 	CMD_FILE_OPENCRE_IMG:
			log0("[%02X] CMD_FILE_OPENCRE_IMG\n",cmd); break;
		
		case	CMD_INIT_READ: 
			log0("[%02X] CMD_INIT_READ\n",cmd); break;
		case	CMD_INIT_WRITE: 
			log0("[%02X] CMD_INIT_WRITE\n",cmd); break;

		case	CMD_READ_BYTES: 
			log0("[%02X] CMD_READ_BYTES\n",cmd); break;
		case	CMD_WRITE_BYTES: 
			log0("[%02X] CMD_WRITE_BYTES\n",cmd); break;
		case	CMD_REWIND: 
			log0("[%02X] CMD_REWIND\n",cmd); break;
		case	CMD_SEEK: 
			log0("[%02X] CMD_SEEK\n",cmd); break;
		case	CMD_TELL: 
			log0("[%02X] CMD_TELL\n",cmd); break;
		case	CMD_GET_FID: 
			log0("[%02X] CMD_GET_FID\n",cmd); break;


		case	CMD_GET_STRLEN: 
			log0("[%02X] CMD_GET_STRLEN\n",cmd); break;
		case	CMD_EXEC_PACKET: 
			log0("[%02X] CMD_EXEC_PACKET\n",cmd); break;
		
		case	CMD_LOAD_LBA: 
			log0("[%02X] CMD_LOAD_LBA\n",cmd); break;
		case	CMD_GET_IMG_STATUS: 
			log0("[%02X] CMD_GET_IMG_STATUS\n",cmd); break;
		case	CMD_GET_IMG_NAME: 
			log0("[%02X] CMD_GET_IMG_NAME\n",cmd); break;
		case	CMD_READ_IMG_SEC: 
			log0("[%02X] CMD_READ_IMG_SEC\n",cmd); break;
		case	CMD_WRITE_IMG_SEC: 
			log0("[%02X] CMD_WRITE_IMG_SEC\n",cmd); break;
		case	CMD_SER_IMG_INFO: 
			log0("[%02X] CMD_SER_IMG_INFO\n",cmd); break;
		case	CMD_VALID_IMG_NAMES: 
			log0("[%02X] CMD_VALID_IMG_NAMES\n",cmd); break;
		case	CMD_IMG_UNMOUNT: 
			log0("[%02X] CMD_IMG_UNMOUNT\n",cmd); break;
		case	CMD_IMG_SEEK: 
			log0("[%02X] CMD_IMG_SEEK\n",cmd); break;
		case	CMD_CREATE_IMG: 
			log0("[%02X] CMD_CREATE_IMG\n",cmd); break;
	    case	CMD_GET_FDC_STATUS: 
			log0("[%02X] CMD_GET_FDC_STATUS\n",cmd); break;
	    case	CMD_READ_NEXT_IMG_SEC: 
			log0("[%02X] CMD_READ_NEXT_IMG_SEC\n",cmd); break;
	    case	CMD_LOAD_HR: 
			log0("[%02X] CMD_LOAD_HR\n",cmd); break;

		case	CMD_CAS_FTYPE: 
			log0("[%02X] CMD_CAS_FTYPE\n",cmd); break;
        case    CMD_CAS_EMULATE:
        	log0("[%02X] CMD_CAS_EMULATE\n",cmd); break;
        case    CMD_CAS_EMULATE2:
        	log0("[%02X] CMD_CAS_EMULATE2\n",cmd); break;
        
		case	CMD_GET_CARD_TYPE: 
			log0("[%02X] CMD_GET_CARD_TYPE\n",cmd); break;
		case	CMD_SET_BUSY: 
			log0("[%02X] CMD_SET_BUSY\n",cmd); break;
		case	CMD_NOP: 
			log0("[%02X] CMD_NOP\n",cmd); break;
		case	CMD_SYNC: 
			log0("[%02X] CMD_SYNC\n",cmd); break;


		case	CMD_GET_DATETIME: 
			log0("[%02X] CMD_GET_DATETIME\n",cmd); break;  
		case	CMD_SET_DATETIME: 
			log0("[%02X] CMD_SET_DATETIME\n",cmd); break;

		case	CMD_GET_FW_VER: 
			log0("[%02X] CMD_GET_FW_VER\n",cmd); break;
		case	CMD_GET_BL_VER: 
			log0("[%02X] CMD_GET_BL_VER\n",cmd); break;

		case	CMD_GET_CFG_BYTE: 
			log0("[%02X] CMD_GET_CFG_BYTE\n",cmd); break;
		case	CMD_SET_CFG_BYTE: 
			log0("[%02X] CMD_SET_CFG_BYTE\n",cmd); break;
		case	CMD_SET_PLATFORM: 
			log0("[%02X] CMD_SET_PLATFORM\n",cmd); break;
		case	CMD_READ_AUX: 
			log0("[%02X] CMD_READ_AUX\n",cmd); break;
		case	CMD_GET_HEARTBEAT: 
			log0("[%02X] CMD_GET_HEARTBEAT\n",cmd); break;
			
		default : 
			log0("[%02X] Unknown\n",cmd); 
	}
}
#endif

void mmc_process(void)
{
	unsigned char received;
    void (*worker)(void) = NULL;
    
    static unsigned char heartbeat = 0x55;
    unsigned char EEData;

    BYTE EEAddress;
    
	switch (LatchedAddress & ADDR_LINE_MASK)
	{
		case CMD_REG:
		{
			if (WASWRITE)
			{
				SetBusy();
				ReadDataPort();
				received = LatchedData;
				LastCommand = LatchedData;
#if LOG_CMD
				if(DEBUG_ENABLED)
					log_cmd(received);
#endif
				// Directory group, moved here 2011-05-29 PHS.
				//
				if (received == CMD_DIR_OPEN)
				{
					// reset the directory reader
					worker = WFN_DirectoryOpen;
				}
				else if (received == CMD_DIR_READ)
				{
					// get next directory entry
					worker = WFN_DirectoryRead;
				}
				else if (received == CMD_DIR_CWD)
				{
					// set CWD
					worker = WFN_SetCWDirectory;
				}
				else if (received == CMD_DIR_GETCWD)
				{
					// Get CWD
					worker = WFN_GetCWDirectory;
				}	
				else if (received == CMD_DIR_MAKE)
				{
					worker = WFN_MakeDirectory;
				}
				else if (received == CMD_DIR_REMOVE)
				{
					worker = WFN_RemoveDirectory;
				}
			
				else if (received == CMD_DIR_SET_SNAPPATH)
				{
					worker = WFN_SetSnapPath;
				}
				else if (received == CMD_DIR_GET_SNAPPATH)
				{
					// copy snap path to global data and return it
					strncpy((char *)globalData,SnapPath,SNAP_PATH_LEN);
					WriteStatus(STATUS_COMPLETE);
					ClearBusy();			
				}
            
				// File group.
				//
				else if (received == CMD_FILE_CLOSE)
				{
					// close the open file, flushing any unwritten data
					//
					worker = WFN_FileClose;
				}
				else if (received == CMD_FILE_OPEN_READ)
				{
					// open the file with name in global data buffer
					//
					worker = WFN_FileOpenRead;
				}
#ifdef INCLUDE_SDDOS
				else if (received == CMD_FILE_OPEN_IMG)
				{
					// open the file as backing image for virtual floppy drive
					// drive number in globaldata[0] followed by asciiz name.
					//
					worker = WFN_OpenSDDOSImg;
				}
#endif
				else if (received == CMD_FILE_OPEN_WRITE)
				{
					// open the file with name in global data buffer for write
					//
					worker = WFN_FileOpenWrite;
				}
				else if (received == CMD_FILE_DELETE)
				{
					// delete the file or directory with name in global data buffer
					//
					worker = WFN_FileDelete;
				}
				else if (received == CMD_FILE_GETINFO)
				{
					// return file's status byte
					//
					worker = WFN_FileGetInfo;
				}
				else if (received == CMD_FILE_OPENAUTOD)
				{
					worker = WFN_FileOpenAuto;
				}  
				else if (received == CMD_FILE_OPENAUTOC)
				{
					worker = WFN_FileOpenAuto;
				}  
				else if (received == CMD_FILE_OPEN_OVERWRITE)
				{
					// open a file for overwrite
					//
					worker = WFN_FileOpenOverwrite;
				}     
				else if (received == CMD_FILE_OPEN_SNAPR)
				{
					// return file's status byte
					//
					worker = WFN_FileOpenSnapR;
				}     
				else if (received == CMD_FILE_OPEN_SNAPW)
				{
					// return file's status byte
					//
					worker = WFN_FileOpenSnapW;
				}  
				else if (received == CMD_FILE_OPEN_STREAMR)
				{
					// Open a file for streaming from, currently a NOP :)
					//
					ClearBusy();
				}     
				else if (received == CMD_FILE_OPEN_STREAMW)
				{
					// Open a file for streaming to
					//
					worker = WFN_FileOpenStreamW;
				}  
				else if (received == CMD_FILE_COPY)
				{
					// Copy a file to another file
					//
					worker = WFN_FileCopyRename;
				}  
				else if (received == CMD_FILE_RENAME)
				{
					// Rename a file
					//
					worker = WFN_FileCopyRename;
				}  
			
#ifdef INCLUDE_SDDOS
				else if (received == CMD_FILE_OPENCRE_IMG)
				{
					// open the file as backing image for virtual floppy drive
					// drive number in globaldata[0] followed by asciiz name.
					// This will create a new image if it does not exist
					worker = WFN_OpenSDDOSImgCreate;
				}
#endif

				else if (received == CMD_INIT_READ)
				{
					// All data read requests must send CMD_INIT_READ before beggining reading
					// data from READ_DATA_PORT. After execution of this command the first byte 
					// of data may be read from the READ_DATA_PORT.
					//
					WriteDataPort(globalData[0]);
					globalIndex = 1;
					LatchedAddress=READ_DATA_REG;
					ClearBusy();
				}
				else if (received == CMD_INIT_WRITE)
				{
					// all data write requests must send CMD_INIT_WRITE here before poking data at 
					// WRITE_DATA_REG	
					// globalDataPresent is a flag to indicate whether data is present in the bfr.
					//
					globalIndex = 0;
					globalDataPresent = 0;
					WriteStatus(STATUS_COMPLETE);
					ClearBusy();
				}
				else if (received == CMD_READ_BYTES)
				{	
					worker = WFN_FileRead;
				}
				else if (received == CMD_WRITE_BYTES)
				{
					worker = WFN_FileWrite;
				}
				else if (received == CMD_REWIND)
				{
					worker = WFN_FileRewind;
				}
				else if (received == CMD_SEEK)
				{
					worker = WFN_FileSeek;
				}
				else if (received == CMD_TELL)
				{
					worker = WFN_FileTell;
				}
				else if (received == CMD_GET_FID)
				{
					worker = WFN_GetFreeFID;
				}
				
			
				// 
				// Utility functions
				else if(received == CMD_GET_STRLEN)
				{
					log0c(DEBUG_ENABLED,"strlen=%d\n",strlen((const char*)globalData));
					WriteDataPort(strlen((const char*)globalData));
					ClearBusy();
				}

				// 
				// Exec a packet in the data buffer.
				else if (received == CMD_EXEC_PACKET)
				{
					worker = WFN_ExecuteArbitrary;
				}

#ifdef INCLUDE_SDDOS
				// SDDOS/LBA operations
				//
				else if (received == CMD_LOAD_LBA)
				{
					// load sddos parameters for read/write
					// globalData[0] = img num
					// globalData[1..4 incl] = 256 byte sector number
					globalCurDrive = globalData[0] & 3;
					SET_LBAMODE(globalCurDrive);
				
					driveInfo[globalCurDrive].CurrentLBA = load_lsn((BYTE *)&globalData[1]);
					log0c(DEBUG_ENABLED,"driveInfo[%d].CurrentLBA=%lu\n",globalCurDrive,driveInfo[globalCurDrive].CurrentLBA);
					ClearBusy();
				}
				else if (received == CMD_GET_IMG_STATUS)
				{
					// retrieve sddos image status
					// globalData[0] = img num
					WriteDataPort(driveInfo[BytesLatch.Bytes[0] & 3].attribs);
					ClearBusy();
				}
				else if (received == CMD_GET_IMG_NAME)
				{
					// retrieve sddos image names
					//
					worker = WFN_GetSDDOSImgNames;
				}
				else if (received == CMD_READ_IMG_SEC)
				{
					// read image sector
					//
					worker = WFN_ReadSDDOSSect;
				}
				else if (received == CMD_WRITE_IMG_SEC)
				{
					// write image sector
					//
					worker = WFN_WriteSDDOSSect;
				}
				else if (received == CMD_SER_IMG_INFO)
				{
					// serialise the current image information
					//
					worker = WFN_SerialiseSDDOSDrives;
				}
				else if (received == CMD_VALID_IMG_NAMES)
				{
					// validate the current sddos image names
					//
					worker = WFN_ValidateSDDOSDrives;
				}
				else if (received == CMD_IMG_UNMOUNT)
				{
					// unmount the selected drive
					//
					worker = WFN_UnmountSDDOSImg;
				}
				else if (received == CMD_IMG_SEEK)
				{
					// Try to seek the image
					//
					worker = WFN_ImgSeek;
				}
				else if (received == CMD_CREATE_IMG)
				{
					// Create a new image or re-initialize an existing one
					// drive and mas LSN must be supplied by calling 
					// CMD_LOAD_PARAM first
					//
					worker = WFN_CreateImg;
				}
				else if (received == CMD_GET_FDC_STATUS)
				{
					WriteStatus(FDCStatus);
					ClearBusy();
				}	
				else if (received == CMD_READ_NEXT_IMG_SEC)
				{
					// read image sector
					//
					worker = WFN_ReadSDDOSNextSec;
				}
				else if (received == CMD_LOAD_HR)
				{
					// globalData[] :
					// 0    : Image number 0..3 (currently)
					// 1    : Head no
					// 2    : Sector no (or sectors / track for format)
					globalCurDrive                          = globalData[0] & 3;
					driveInfo[globalCurDrive].CurrentHead   = globalData[1];
					driveInfo[globalCurDrive].CurrentSecNo  = globalData[2];
					SET_CHRNMODE(globalCurDrive);
					log0c(DEBUG_ENABLED,"CMD_LOAD_HR: Drive=%d, Head=%d, Sector=%d\n",
							globalCurDrive, driveInfo[globalCurDrive].CurrentHead,  driveInfo[globalCurDrive].CurrentSecNo);
				
					ClearBusy();
				}
            
#endif
				// 
				// Cassette file handling commands
				//
				else if (received == CMD_CAS_FTYPE)
				{
					worker = WFN_GetCasFiletype;
				}
				else if ((received == CMD_CAS_EMULATE) || (received == CMD_CAS_EMULATE2))
				{
					worker = WFN_TapeUpdate;
				}

				//
				// Utility commands.
				// Moved here 2011-05-29 PHS
				else if (received == CMD_GET_CARD_TYPE)
				{
					// get card type - it's a slowcmd despite appearance
					disk_initialize(0);
					WriteDataPort(CardType);
					ClearBusy();
				}
				else if (received == CMD_SET_BUSY)
				{
					// Just returns with busy set for testing.
				}
				else if (received == CMD_NOP)
				{
					ClearBusy();
				}
				else if (received == CMD_SYNC)
				{
					worker = WFN_SyncDisk;
				}
				else if (received == CMD_GET_DATETIME)
				{
					worker = WFN_GetDateTime;
				}
				else if (received == CMD_SET_DATETIME)
				{
					worker = WFN_SetDateTime;
				}
				else if (received == CMD_GET_FW_VER) // read firmware version
				{
					WriteDataPort(VSN_MAJ<<4|VSN_MIN);
					strcpy_P((char *)&globalData[0],CompileTime);
					ClearBusy();
				}
				else if (received == CMD_GET_BL_VER) // read bootloader version
				{
					WriteDataPort(blVersion);
#if !defined(RAMPZ)               
					strcpy_P((char *)&globalData[0],BLD->bl_compile);
#else
					strcpy_PF((char *)&globalData[0],(uint_farptr_t)BLD->bl_compile);
#endif
					ClearBusy();
				}
				else if (received == CMD_GET_CFG_BYTE) // read config byte
				{
					EEAddress=BytesLatch.Bytes[0];
				
					// The first time the config byte is got after power on / reset, get read the settings.
//					if(PLAT_INVALID == globalFlag)
//					{
//						//globalFlag=byteValueLatch;
//						globalFlag=BytesLatch.Bytes[0];
//						LoadSettings();
//					}
                
					EEData=ReadEEPROM(EE_SYSFLAGS+EEAddress);
					WriteDataPort(EEData);
					log0c(DEBUG_ENABLED,"Get Config:Platform=%02X, Config=%02X\n",EEAddress,EEData);
					ClearBusy();
				}
				else if (received == CMD_SET_CFG_BYTE) // write config byte
				{
					if (1 < BytesLatch.Index)
					{
						EEAddress=BytesLatch.Bytes[0];
						EEData=BytesLatch.Bytes[1];
				
						log0c(DEBUG_ENABLED,"set config:platform=%02X, Config=%02X\n",EEAddress,EEData);
						WriteEEPROM(EE_SYSFLAGS+EEAddress,EEData);
						if ((PLAT_SYSTEM == EEAddress))           
							configByte=EEData;
                    }
					
					WriteStatusCond((1 < BytesLatch.Index), STATUS_COMPLETE,(STATUS_ERROR | ERROR_INVALID_PARAM));
					ClearBusy();
				}
				else if (received == CMD_SET_PLATFORM)
				{
					if (0 < BytesLatch.Index)
					{
						if (PlatformValid(BytesLatch.Bytes[0]))
							globalFlag = BytesLatch.Bytes[0];
						else
							globalFlag = PLAT_INVALID;
					}
					log0c(DEBUG_ENABLED,"Platform set to %02X\n",globalFlag);
					
					// Load the settings here, as the rom always sets the platform when booting, that way
					// we don't have to eject and insert the card to pick these up.
					LoadSettings();
					
					WriteStatusCond(PlatformValid(globalFlag),STATUS_COMPLETE,(STATUS_ERROR | ERROR_INVALID_PLATFORM));  
					ClearBusy();
				}
				else if (received == CMD_READ_AUX) // read porta - latch & aux pin on dongle
				{
					WriteDataPort(LatchedAddress);
					ClearBusy();
				}
				else if (received == CMD_GET_HEARTBEAT)
				{
					// get heartbeat - this may be important as we try and fire
					// an irq very early to get the OS hooked before its first
					// osrdch call. the psp may not be enabled by that point,
					// so we have to wait till it is.
					//
					WriteDataPort(heartbeat);
					heartbeat ^= 0xff;
					ClearBusy();
				}
				else	// Invalid command 
				{
					WriteStatus(STATUS_ERROR | ERROR_INVALID_CMD);
					ClearBusy();	
				}
			}
//		 	HexDumpHeadc(DEBUG_ENABLED,(uint8_t *)&BytesLatch,sizeof(BytesLatch),0);
//		 	ResetLatch(BytesLatch);
		}
		break;

		case READ_DATA_REG:
		{
			if(!WASWRITE)
			{
				WriteDataPort((globalData[(int)globalIndex]));
				if (globalIndex < GLOBUFFSIZE)
					++globalIndex;
			}
		}
		break;

		case WRITE_DATA_REG:
		{
			// write data port.
			// must have poked 255 at port 3 before starting to poke data here.
			//
			if (WASWRITE)
			{
				SetBusy();
				ReadDataPort();
				received = LatchedData;

				globalData[globalIndex] = received;
				if (globalIndex < GLOBUFFSIZE)
					++globalIndex;

				globalDataPresent = 1;
#if LOG_DATAIN
				log0("%c",received);
#endif
				ClearBusy();
			}
		}
		break;

		case LATCH_REG:
		{
			// latch the written value, and put it into the latched bytes array
			if (WASWRITE)
			{
				SetBusy();
				ReadDataPort();
				received = LatchedData;
				InsertLatch(BytesLatch,received);
				
				ClearBusy();
			}
		}
		break;
	}

	if (worker)
	{
		worker();
	}
   
	// We have to do this here, so that the worker call gets a chance to
	// see the latched bytes before clearing.
	if(CMD_REG == (LatchedAddress & ADDR_LINE_MASK))
	{
		// Don't reset latch if writing data.
		if (CMD_INIT_WRITE != LastCommand)
			ResetLatch(BytesLatch);
	}
}

void at_process(void)
{    	
	// port a holds the latched contents of the address bus a0-a3
	//
	LatchAddressIn();
	if (WASWRITE)
		LatchedAddress=LatchedAddressLast;
		
	mmc_process();
}
