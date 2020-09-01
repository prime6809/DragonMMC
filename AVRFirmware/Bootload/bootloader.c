/*
	Mega644/1284 SD/MMC bootloader for DragonMMC project.
	
	2011-10-09, P.Harvey-Smith, based on AtoMMC bootloader 
	2009	  , C.Robson.
    
    2017-03,    P.Harvey-Smith, modified to use the petit fatfs
                this removes the dependency on the bootfile being 
                non-fragmented, and it's directory entry being < 8.
                
    2017-03-16, P.Harvey-Smith, added compile time and version in flash.
    
*/

#include <inttypes.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <avr/boot.h>
#include <avr/pgmspace.h>
#include <util/delay.h>
#include <string.h>
#include <ctype.h>
#include "status.h"
#include "bootloader.h"
//#include "integer.h"
#include "pff.h"
#include "../mmc/mmc2.h"
#include "../mmc/mmcio.h"
#include "../mmc/mmc2io.h"
#include "../mmc/mmc2def.h"
#include "../DragonIO.h"
#include "diskio.h"

#define NOW "AVR-Boot: compiled at " __TIME__ " on " __DATE__ 

// 0xD8, 0xDE
FUSES = 
{
// Mega644p / Mega1284p
//    .low = (FUSE_SUT1 & FUSE_CKSEL0) ,                                  // External 8Mhz+ clock, + start up time.
// Mega1284, Atmel, what are identical fuses not named the same for identical Archetectures!!!!
    .low = (FUSE_SUT_CKSEL5 & FUSE_SUT_CKSEL0),
    
    .high = (FUSE_BOOTSZ0 & FUSE_BOOTSZ1 & FUSE_SPIEN & FUSE_BOOTRST),  // 8K bootload, SPI enabled, Bootload on reset
    .extended = EFUSE_DEFAULT,                                          // No brown out detector
};

void (*JumpToApp)(void) = 0x0000;
void RunApplication(void);

#define BL_MAJOR	2
#define BL_MINOR	3

__attribute__((used, section(".bldata"))) const bootdata_t BootData = {BL_MAJOR, BL_MINOR, NOW};

#if !defined(RAMPZ)
const __flash bootdata_t *BLD = (const __flash bootdata_t *)BOOTLOADER_DATA;
#else
const __flash1 bootdata_t *BLD = (const __flash1 bootdata_t *)BOOTLOADER_DATA;
#endif

void RunApplication(void)
{
	log0("Bootloader done\n");
    log0("Running application !\n");
	//_delay_ms(5000);
	boot_rww_enable_safe();
	JumpToApp();
};

// jumped to if there's no firmware flashed

BYTE CardType;

FATFS Fatfs;				/* Petit-FatFs work area */
BYTE Buff[SPM_PAGESIZE];	/* Page data buffer */

#define PAGES_PER_SECTOR	(SECBUFFSIZE / SPM_PAGESIZE)

void WriteDataToMemory(
DWORD startaddr,
BYTE *Data,
BYTE PagesToWrite
)
{
	BYTE		PageNo		= 0;
	DWORD		BuffOffset	= 0;
	DWORD		PageAddress;
	uint16_t	Count;
	uint16_t	ProgWord;
	uint16_t	ProgWordIdx;
	
	while(PageNo<PagesToWrite)
	{
		PageAddress=startaddr+(PageNo*SPM_PAGESIZE);
		log0("Programing address : %05X\n",(unsigned int)PageAddress);
		
		boot_page_erase(PageAddress);		// Erase the page
		boot_spm_busy_wait();				

		// Move data to tempory programming buffer
		ProgWordIdx=PageAddress;
		for(Count=0; Count<SPM_PAGESIZE; Count+=2, BuffOffset+=2 )
		{
			ProgWord=Data[BuffOffset];
			ProgWord|=(Data[BuffOffset+1] << 8);	
			boot_page_fill(ProgWordIdx,ProgWord);
			ProgWordIdx+=2;
		}
		PageNo++;
		
		boot_page_write_safe(PageAddress);	// program the page
		boot_spm_busy_wait();
	}
}

#if 0
unsigned short updateCRC(unsigned char data, unsigned short crc)
{
    crc  = (unsigned char)(crc >> 8) | (crc << 8);
    crc ^= data;
    crc ^= (unsigned char)(crc & 0xff) >> 4;
    crc ^= (crc << 8) << 4;
    crc ^= ((crc & 0xff) << 4) << 1;

    return crc;
}
#endif

void send(char  *str, int max)
{
    int count = 0;
    
    while ((str[count]!=0x00) && (count < max))
    {
        Serial_TxByte0(str[count]);
        count++;
    }
}

int main(void)
{
    BYTE configFlags;
    DWORD address;
	UINT br;	    /* Bytes read */
	FRESULT res;
   
	InitIO();
	Serial_Init(115200,115200);
    
//    RAMPZ=1;
	log0("NewBootloader entered\n");
    log0i(BLD->bl_compile); log0("\n");
	
    // lights out
    REDLEDOFF();
    GREENLEDOFF();
   
	// Read flags from EEPROM
	configFlags=ReadEEPROM(EE_SYSFLAGS);
	log0("Flags(%02X)=%02X\n",EE_SYSFLAGS,configFlags);
    
    // enter the bootloader only if config byte has bit 7 set.

    // now it might be possible to lock things up if we get here without any firmware loaded
    // and the bootloader flag is clear. This would be bad. It is extremely unlikely however
    // as long as any bootloader-updater ensures the eeprom is correctly configured (reset)...

    if (configFlags & CFG_ENABLE_BOOTLOAD)
    {
        CardType = disk_initialize();
		log0("cardtype=%d\n",CardType);
		
        if (CardType == 0)
        {
//            RunApplication();
        }

     	/* Initialize file system */
		res=pf_mount(&Fatfs);
        if(res != FR_OK)
        {
			log0("Error %02X mounting\n",res);
            RunApplication();
        }
		log0("Mounted\n");

        /* Attempt to open bootfile */
        if(pf_open(BootFileName) == FR_OK)
        {
			log0("Found bootfile %s\n",BootFileName);
            REDLEDON();
            for (address = 0x0000; address < BOOT_ADDR; address += SPM_PAGESIZE)
            {
                GREENLEDON();
                pf_read(Buff, SPM_PAGESIZE, &br);
                GREENLEDOFF();

                if (br) WriteDataToMemory(address, Buff, 1);
            }
            REDLEDOFF();
        

            // If we get here then we have programmed the flash
            // Reset bootloader enabled flag after flash
            configFlags &= ~CFG_ENABLE_BOOTLOAD;
            log0("Writeflags: %02X\n",configFlags);
            WriteEEPROM(EE_SYSFLAGS,configFlags);
        }
        RunApplication();
    }

	RunApplication();
	
	return 0;
}
