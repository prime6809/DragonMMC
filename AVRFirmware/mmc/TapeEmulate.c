/*

	TapeEmulate.
	
	Emulates the Dragon / CoCo tape input stream by taking an opened CAS file
	reading it and generating a digital FSK representation of the signal.
	This is written to the CPLD which is programmed to return this value 
	instead of the one from the PIA port at $FF20.
	
	First version: 2017-02-22.

*/

#include <avr/interrupt.h>
#include <inttypes.h>
#include <util/delay.h>
#include <string.h>
#include "ff.h"
#include "mmc2wfn.h"
#include "status.h"
#include "dragon_cas.h"
#include "dragon_file.h"
#include "TapeEmulate.h"
#include "mmc2def.h"
#include "mmc2.h"
#include "mmc2io.h"

#define CMATCH4800	0x08
#define CMATCH2400	0x11

// Filename block and first block of file use SYNCHEAD
// all others use SYNCDATA
#define SYNCHEAD    128
#define SYNCDATA    16

BYTE Output;

void InitTimer(void)
{
	// init CTC to 4800Hz.
	// Values correct for an 11.05MHz CPU clock.
	TCCR0A	= (1 << WGM01);					// No pins connected, WGM = CTC.
	TCCR0B	= (1 << CS02);	                // Clock scale of /256
	TIMSK0	= 0x00;							// Ints disabled
	
	OCR0A	= 0x08;							// Output compare at count
	TCNT0	= 0x00;							// Reset timer
    
    Output  = 0x01;
    WriteDataPort(Output);                  // Init data port
}

void StopTimer(void)
{
    TCCR0A  = 0;                            // Stop the timer.
}

// Two macros to wait for next CTC match and to reset the compare match flag.
#define WaitCTC() 	{ while (!(TIFR0 & (1 << OCF0A))) { asm("nop"); };  }
#define ResetCTC() 	TIFR0 |= (1 << OCF0A);

// Output a frequency shift keyed square wave to the tape emulation port
// works by counting cycles of the base 4800KHz timer frequency.
// The Dragon and CoCo use one cycle of 2400Hz to represent a 1 and 
// one cycle of 1200Hz to represent a 0.
// This means for a 1 we count 2 cycles of 4800Hz, and 4 for a 0.
void /*inline*/ FSKBit(BYTE	Bit)
{
	BYTE	Count  = 0x00;	        // Count of timer periods
	BYTE	Top;			        // Timer count to stop;
	BYTE	Invert;			        // Timer count to invert output.
	
    // This selects the count point where we need to invert the output.
	if(Bit)
		Invert = 1;			        // 2 cycles of 4800 = 2400
	else
		Invert = 2;			        // 4 cycles of 4800 = 1200
	
    // This selects the point we return to caller, one count *BEFORE*
    // the end of the cycle to give the caller some time to get the
    // next bit / byte before the beginning of the next cycle.
	Top = (Invert*2)-1;		        // Top at twice invert -1
        
    // However the above means that we have to wait for the beginning of the 
    // cycle and flip the output before counting this cycle.
	WaitCTC();				        // Wait for count
	ResetCTC();				        // Reset compare match flag

	Output ^= 0x01;			        // Flip output bit;
	WriteDataPortFast(Output);	    // init the cycle, always start high.
	
    // Count cycles of 4800Hz, flipping output as needed.
    while (Count < Top)
	{
		WaitCTC();					// Wait for count
		ResetCTC();					// Reset compare match flag
		Count++;
        
		if(Count==Invert)
		{
			Output ^= 0x01;			// Flip output bit;
			WriteDataPortFast(Output);	// Write it.
		}
	}
    // Exits one count early to give client code a chance to execute before next cycle.
}


// Output a byte to the tape, LSb first.
void inline TapeByteOut(BYTE    OutputByte)
{
	int		BitNo;

    // Generate FSK least significant bit first.
	for(BitNo=0; BitNo < 8; BitNo++)
	{
        FSKBit(OutputByte & 0x01);			// Extract LSBit and send it
		OutputByte = OutputByte >> 1;		// Shift right
	}
}

// Output a block to the tape, of specified type, length and header length.
// The block checksum is automatically calculated and output.
// Format is :
//  BYTE 0x55 * n   Sync bytes
//  BYTE 0x3C       Block begin marker
//  BYTE 0xXX       Block type.
//  BYTE n *        Block data
//  BYTE n          Checksum byte
//  BYTE 0x55       Final sync.
   
void TapeBlockOut(
    BYTE    *Block,
    BYTE    BlockType,
    BYTE    BlockLen,
    BYTE    LeaderLen)
{
    int     Count;
    BYTE    Checksum = BlockType+BlockLen;

    // Output leader
    for(Count=0; Count<LeaderLen; Count++)
        TapeByteOut(CASSyncByte);
        
    // Block begin marker
    TapeByteOut(CASBlockBegin);
   	
	// Output block type and length.
	TapeByteOut(BlockType);
	TapeByteOut(BlockLen); 
	
    // Block data
    for(Count=0; Count<BlockLen; Count++)
    {
        TapeByteOut(Block[Count]);
        Checksum += Block[Count];
    }

    TapeByteOut(Checksum);
    TapeByteOut(CASSyncByte);
}

// Output the file who's name is in the global data block to the dragon
// as if input from tape. The CPLD will remap our output so that it looks
// as if it comes from the PIA at 0xFF20.
void TapeOut(void)
{
	UINT	BytesRead;
    BYTE    Buffer[CASDefBlockSize];
    BYTE    SyncSize;
	BYTE 	LatchedAddressLast;
	
	int	result;
    CASFilenameBlock    FBlock;
    DGNHead             FHead;

	result = fileOpen(FA_OPEN_EXISTING|FA_READ,CAS_FILE);
	if(result)
	{
		log0("ERROR 0x%02X opening Tape emulation file %s\n",result,globalData);
	}
	else
	{   
		log0("Waiting for motor on\n");
		
		do 
		{
			LatchAddressIn();		// Read the motoron bit from the CPLD
			log0("%2.2X\n",LatchedAddressLast);
		} while (MotorIsOff);		// Loop while it's off.....
		
		log0("Motor on, wait.....\n");
		_delay_ms(1000);
	    log0("go....\n");
		
        // Initialize timer used to time tape pulses.
        InitTimer();
        
        // Read header from DGN file.
        f_read(&Files[CAS_FILE], &FHead, sizeof(FHead), &BytesRead);
        
        // Setup filename block, copied from input file.
        memset(&FBlock.Filename[0],0x20,CASFilenameLen);        // fill in filename
        strncpy(&FBlock.Filename[0],(CHAR *)globalData,CASFilenameLen);
        FBlock.FileType=CASFtMachineCode;                       // machine code file
        FBlock.AsciiFlag=CASAsBinary;                           // Binary
        FBlock.GapFlag=CASGfUngapped;                           // Ungapped
        FBlock.ExecAddress=FHead.HdrExec;                       // Exec address
        FBlock.LoadAddress=FHead.HdrLoad;                       // Load address

        // Filename block and first block of file use SYNCHEAD
        // all others use SYNCDATA
        SyncSize=SYNCHEAD;   
        
        // Output filename block
        TapeBlockOut((BYTE *)&FBlock.Filename,CASBtFileName,CASFNameBlockLen,SyncSize);      
        _delay_ms(90);
        
        // Keep outputting data whilst available from file
        f_read(&Files[CAS_FILE], Buffer, CASDefBlockSize, &BytesRead);
        while(0 != BytesRead)
        {
            TapeBlockOut(Buffer,CASBtData,BytesRead, SyncSize);
            _delay_ms(90);
            SyncSize=SYNCDATA;
            f_read(&Files[CAS_FILE], Buffer, CASDefBlockSize, &BytesRead);
        }
        
        // Output EOF block
        TapeBlockOut(Buffer,CASBtEOF,0,16); 
        
        // Close input file, stop timer
        fileClose(CAS_FILE);
        StopTimer();
		log0("Tape emulation done\n");
	}
}