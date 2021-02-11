	/*
	DragonMMC	: MMC interface for the Dragon.

	2011-06-16, P.Harvey-Smith, MMC work based on AtoMMC by Charlie Robson.
	
	2011-06-28
		Prototype board completed.
	
	2011-07-02
		MMC Card directory command now working !
	
	2011-07-03
		Cassette emulation now working for cload[m] / csave[m]
		Delete command implemented.
		
	2011-09-xx
		Added ability to load and run cartridge images in the $C000-$FEFF 
		area by loading them into the DragonMMC ram area.
		
	2011-11-12
		Added directory and wildcard support to CAT and FINDFIRST/FINDNEXT
		
	2011-12-06
		Fixed initialisation of the JTAG CPLD programming lines.
		JTAG lines are now left as inputs with pullups enabled unless 
		the CPLD is being programmed. This prevents noise on the lines 
		causing the CPLD to spuriously enter programming mode.
		
	2012-10-20
		Modified SDDOS read and write sector routines to use files rather
		than base sector + displacement, which assumes that the image file
		is contiguous.
		
	2015-11-02
		Began update for Mk 2.00 hardware.
		
	2016-03-14
		Tidyed up some of the code in mmc2wfn.c specifically fixed the broken
		if..else if chain in the command byte processing, and made the code
		always ClearBusy() and return an error if an invalid command byte is 
		sent.
		Began tring to debug DOS handling code.

    2016-05-19
        Config byte handling code re-written so that it supports multiple config bytes
        offset from EE_SYSFLAGS in the EEPROM.
        Currently supported are :
        offset      function
        0           System, 0x80 = bootlaoder will flash firmware, 0x01 = debug output to serial
        1           Dragon, 0x40 = autoexec enabled, 0x20 = Dos emulation enabled
        1           CoCo, flags as Dragon above.

    2016-05-21
        Forked to version 2.10
        Began adding code to handle PCF2129AT RTC in SPI mode, this will allow timestamping of created 
        files, and getting and setting the date / time from the Dragon.
        
    2016-05-24
        Get/Set data time working with hardware, file dates being set correctly.
        
    2017-03-05
        Following a bug report from Richard Harding, fixed a protocol error in the 6809 code that
        was causing WD$, FINDFIRST$ and FINDNEXT$ to fail.
        
    2017-03-07
        Fixed a bug in the unmount disk routine that caused the wrong disk to be unmounted, by using
        byteValueLatch as the source of the drive ID rather than globalData[0].
    
    2017-03-07
        Changed get drive wfnGetSDDOSImgNames, to expect to be passed the fist and last drive names
        to fetch. This means that we can fetch individual drive filnames, to allow us to have a 
        function on the dragon side to return the filename mounted on a certain drive.

    2017-03-20
        Moved SAM bits handling code into CPLD as AVR could not respond to writes quickly enough and
        was dropping writes.
  
	2017-05-12
		Forked to version 2.30.
        
    2017-07-20
        Added call to make a directory.
        Added cached read directory, so that the AVR can read the directory entries and sort them
        before outputting to the client. This is based on a define configurable static array of entries 
        if there are more directory entries than array entries, the cache falls back to the old 
        un-cached method.
        The Array is currently set to 256 entries which seems reasonable, with the current ATMega1284's
        16K of RAM available, this could probably be doubled without problems (current memory use is 
        between 5K and 6K).
		
    2017-07-29
        Forked to version 2.40.
        Implemented Tape streaming code to allow MMIRROR to work, this disables the normal polled I/O
        and implements an interrupt handler who's sole function is fetching bytes from the Dragon and 
        placing them in a buffer. When the buffer is full, the ISR flags the main loop to write the 
        buffer to disk, whilst filling a second buffer, once this if full it is also written and the 
        first is re-used.
        
        Pressing the snapshot button breaks out of this and reverts to normal operation, flushing the 
        buffer and closing the output file first.
		
	2017-08-03
		Forked to version 2.50 ready for code clenup and pre-release testing.
        
    2017-09-08.
        Forked to V2.60 for release.
		
	2017-09-11.
		Began work on first post release version.
        
    2017-10-08.
        Releasead to beta testers.
        
        New features implemented :
            Attempting to create an already existing file will rename old file to filename.bak
            before saving new file. If .bak file already exists it is deleted.
            Controlled by bit 2 of the system cfg.
            
            Changed the way that floppy disk emulation works so that it is now based on Cylinder, Head, 
            Record rather than logical sector no. This was required to enable us to implement disk 
            image creation, implemented CMD_CREATE_IMG and CMD_LOAD_HR to support this, also changed 
            how CMD_IMG_SEEK works.
			
	2019-04-03.
		Re-factored disk emulation so that it can support both CHRN and LBA type operations.
		The imgInfo structure now stores both CHRN and LBA info, plus a flag that specifies which
		to use when reading and writing data. This flag (LBAMode) is set by the functions that load the 
		data into the structure (from the client) so LBAMode is SET when a CMD_LOAD_LBA is sent and reset
		if a CMD_IMG_SEEK or CMD_LOAD_HR is sent. This is done to make the NitrOS9 driver easier as
		internally it uses logical blocks.
		
	2019-09-04.
		Firmware release 1.20. 
		New in this version :
			Fixed bug where DragonDos disk image checks where corrupting CoCo 35 track disks.
			Fixed a bug with snapshot facility caused by code refactor.
			Implemented MDISKC, which acts like MDISKI *EXCEPT* that it will create a blank
			image if it does not already exist. MDISKI reverted to v1.00 behavior where 
			the supplied disk image file must already exist.
			Under the hood adjustments to allow Flex / NitOS9 to boot.
			
	2020-04-24.
		Firmware release 1.25. 
		New in this version :
			Implemented function $5E, to load flasher for Diag board which uses the same 
			flash rom.
			Function 		File loaded.
			$5F				FLASH.DGN
			$5E				DIAGFSH.DGN
			
	2020-05-17.
		Firmware release 1.30 alpha.
		New in this version :
			Updated version of FatFS from V0.09 to V0.14.
			
	2021-02-11.
		Firmware release 1.35.
		New in this version :
			Squashed a bug (in 6809 firmware) where attempting to load a file from mmc
			that was an exact multiple of 256 bytes would crash the machine (endless loop).
			
			Settings.dgn or Settings.cco are read from disk when a CMD_SET_PLATFORM command
			is received. This means that the card does not have to be unplugged and
			re-inserted at power on to get the correct settings, as the 6809 firmware 
			always issues this command as part of it's boot sequence.
*/

#include <avr/interrupt.h>
#include <inttypes.h>
#include <util/delay.h>
#include "shared/status.h"
#include "dragonio.h"
#include "ff.h"
#include "dragon_disk.h"
#include "mmc/mmc2def.h"
#include "mmc/mmc2.h"
#include "mmc/mmc2io.h"
#include "mmc/mmcio.h"
#include "mmc/diskio.h"
#include "mmc/pcf2129rtc.h"
#include "dragon_cas.h"
#include "bootload/bootloader.h"

#if DEVEL
#define NOW "AVR-devel: compiled at " __TIME__ " on " __DATE__ 
#else
#define NOW "AVR: compiled at " __TIME__ " on " __DATE__ 
#endif

// ***NOTE*** BOOTLOADER_DATA *MUST* be the same as the value set in the bootloader's Makefile.
// Mega644
//#define BOOTLOADER_DATA 0xFF80

// Mega1284
#define BOOTLOADER_DATA 0x1FF80

// Global data for MMC

volatile unsigned char 	globalData[GLOBUFFSIZE];		// Global buffer used for data exchange with Dragon
volatile unsigned char	globalFlag;						// Global platform byte.
volatile BYTE			globalStreaming;				// Global sstreaming, if true we are streaming rather 
														// than using command response driven protocol.
volatile BYTE	        globalWriteFlag;				// Used whilst streaming to flag we need to write data to disk.
volatile WORD	        globalWriteBase;				// Base offset of streamed data to be written.
volatile BYTE	        dataValue;						// data read whilst streaming.
BYTE 					configByte;						// System configuration byte
BYTE 					blVersion;						// boot loder version
BYTE					CardInserted;					// Flag to indicate card is inserted
BYTE					CardInsertedNow;				// last value of above flag.
const __flash 			char CompileTime[]  = NOW;		// Time firmware compiled.

extern FIL fil;


// Get pointer to bootloader version, and compile time.
#if !defined(RAMPZ)
const __flash bootdata_t *BLD = (const __flash bootdata_t *)BOOTLOADER_DATA;
#else
const __flash1 bootdata_t *BLD = (const __flash1 bootdata_t *)BOOTLOADER_DATA;
#endif


// Initialise variables 
void InitVars(void)
{
	_delay_ms(1000);
    blVersion=((BLD->blmajor & 0x0F) << 4) | (BLD->blminor & 0x0F);
}

// Toggle the leds so that the user knows the system is ready.
void flag_init(void)
{
	GREENLEDON();
	_delay_ms(100);
	REDLEDON();
	_delay_ms(100);
	
	GREENLEDOFF();
	_delay_ms(100);
	REDLEDOFF();
}

// We use an interrupt vector when we are streaming from Dragon / CoCo Tape, so that 
// we can ensure that incomming bytes are not missed if they happen whilst the main
// loop is off writing the buffer to the disk.
// We use 2 consecutive buffers that are used in a round robbin way so that whilst
// one is being written to the card, the other can still be receiving bytes from the 
// Dragon / CoCo.
// This will keep going until the snapshot button is pressed where the loop will be 
// broken, any unsaved data saved, and the interrupt handler disabled.
// Note this relies on the global buffer being **AT LEAST** twice as big as 
// STREAMBUFFERSIZE
//
ISR(INTIN_vect)
{
    // Read value from Dragon, put it in the buffer and increment the buffer pointer
	ReadDataPortFast(dataValue);
    
	globalData[globalIndex] = dataValue;
	++globalIndex;
	
    // If either buffer is full, then flag that the main loop should write the buffer,
    // and set the base of the buffer to write.
	if((STREAMBUFSIZE == globalIndex) || ((STREAMBUFSIZE*2) == globalIndex))
	{
		globalWriteBase=(STREAMBUFSIZE == globalIndex) ? 0 : STREAMBUFSIZE;
		globalWriteFlag=1;
        
        // If we reach the end of the second buffer re-set to the beginning of the first.
        // toggle the LED to let the user know we are still working.
        if(globalIndex >= GLOBUFFSIZE)
        {
            globalIndex=0;
            REDLEDTOGGLE();
        }
	}
}

int main(void)
{
	UINT written;

	InitVars();
	Serial_Init(115200,115200);
#if DEVEL	
	log0("Dragon SD/MMC Interface V2.8 development release\n");
#else
	log0("Dragon SD/MMC Interface V2.8 release\n");
#endif
	log0("2021-02-11 Ramoth Software.\n");
	log0(NOW); log0("\n");
    log0i(BLD->bl_compile); log0("\n");
    log0("Bootload maj=%d, min=%d, vers=%02X\n",BLD->blmajor,BLD->blminor,blVersion);
	log0("\nRam free=%d bytes\n",FreeRam());
    
	log0("I/O Init\n");
	InitIO();
	configByte = ReadEEPROM(EE_SYSFLAGS);
	log0("SysConfigByte=%02X\n",configByte);
	
	CardInserted=CARD_INSERTED();
	log0("Card inserted=%02X\n",CardInserted);
	
	log0("Init SPI & RTC\n");
    INIT_SPI();
    rtcInit();

    log0("RTC Date/Time: %s\n",rtcDateTimeASCII());
    
	log0("MMC Init\n");
	at_initprocessor();
	
	log0("init done!\n");
	sei();

	log0("Sizeof(FIL)=%d\n",sizeof(FIL));
	log0("Sizeof(Files)=%d\n",sizeof(Files));

	flag_init();

	// Before entering the main program loop, check to see if the CPLD update jumper
	// is set, and if so update the onboard CPLD from it's firmware file on the SDCARD.
	if(CPLDUpdateFlag())
	{
		REDLEDON();
		UpdateCPLD();
		REDLEDOFF();
	}

	// Check to see if we have a card in, if so load settings etc.
	if(CardInserted)
		InitNewCard();
		
	while(1)
	{
		// Check for incomming activity from Dragon,
		// Don't trigger if streaming as we'll handle this with an ISR
		if(!globalStreaming) 
		{
			// Not streaming so handle command / data and respond
			if(LatchInt())
			{
				REDLEDON();
				ClearLatchInt();
				at_process();
				REDLEDOFF();
			}
		}
		else
		{
			// Streaming, check for a full buffer and write if so.
			if(globalWriteFlag)
			{
				f_write(&Files[CAS_FILE], (void*)&globalData[globalWriteBase], STREAMBUFSIZE, &written);
				globalWriteFlag=0;
			}
		}
		
		// Check for snapshot button pressed
		if(SnapPressed())
		{
			// Debounce snapshot button.
			_delay_ms(2);
			if(SnapPressed())
			{
				log0("Snapshot pressed!\n");
				WaitSnapsRelease();
				log0("Snapshot released!\n");
			}
			
			// If we are streaming, flush then close the stream file and turn off streaming.
			if(globalStreaming)
			{
                globalWriteBase=(globalIndex >= STREAMBUFSIZE) ? STREAMBUFSIZE : 0;
                f_write(&Files[CAS_FILE], (void*)&globalData[globalWriteBase], (globalIndex-globalWriteBase), &written);
                
				f_close(&Files[CAS_FILE]);
				DisableInt();
                ClearLatchInt();
				globalStreaming=0;
                REDLEDOFF();
			}
			
			// Assert NMI to the Dragon / CoCo to activate it's menu.
			AssertNMI();
			_delay_us(10);
			ClearNMI();
		}
		
		// Check for new card insertion / removal
		if(CARD_INSERTED() != CardInserted)
		{
			_delay_ms(1000);
			CardInsertedNow=CARD_INSERTED();
			if(CardInsertedNow)
			{
				log0("Card inserted!\n");
				if (!disk_initialize(0))
					InitNewCard();
			}
			else
			{	
				DismountCard();
				log0("Card removed!\n");
			}
			
			CardInserted=CardInsertedNow;
		}
	}
	
	return 0;
}
