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
#include "mmc2wfn.h"
#include "ff.h"
#include "status.h"
#include "dragonio.h"
#include "dragon_cas.h"
#include "dragon_file.h"
#include "TapeEmulate.h"
#include "mmc2.h"
#include "xilinx/ports.h"

extern FIL fil;

#define TEST1
#define CMATCH4800	0x08
#define CMATCH2400	0x11

BYTE Output;

void InitCTC(void)
{
	// init CTC to 4800Hz.
	// Values correct for an 11.05MHz CPU clock.
	TCCR0A	= (1 << WGM01);					// No pins connected, WGM = CTC.
	TCCR0B	= (1 << CS02);	                // Clock scale of /256
	TIMSK0	= 0x00;							// Ints disabled
	
	OCR0A	= 0x08;							// Output compare at count
	TCNT0	= 0x00;							// Reset timer
    
    Output  = 0x01;
    WriteDataPort(Output);
}

#define WaitCTC() 	{ while (!(TIFR0 & (1 << OCF0A))) { asm("nop"); };  }
#define ResetCTC() 	TIFR0 |= (1 << OCF0A);

#ifdef TEST1
// Works by counting fixed 4800Hz timer periods, does not change the timer,
// more code but I have more confidence in it working!
void /*inline*/ FSKBit(BYTE	Bit)
{
	BYTE	Count  = 0x00;	        // Count of timer periods
	BYTE	Top;			        // Timer count to stop;
	BYTE	Invert;			        // Timer count to invert output.
	
	if(Bit)
		Invert = 1;			        // 2 cycles of 4800 = 2400
	else
		Invert = 2;			        // 4 cycles of 4800 = 1200
	
	Top = (Invert*2)-1;		        // Top at twice invert -1
        
	WaitCTC();				        // Wait for count
	ResetCTC();				        // Reset compare match flag

	Output ^= 0x01;			        // Flip output bit;
	WriteDataPortFast(Output);	    // init the cycle, always start high.
	
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

#else
// Works by adjusting the compare match value to actually use the timer 
// to time single 2400 or 1200 cycles (either 2 cycles of 4800 or 
// 2 cycles of 2400).
// Advantage of less code & variables, but less confidence without testing!
void inline FSKBit(BYTE	Bit)
{
	BYTE	Output = 0x01;	// Start with output bit set.
	BYTE	Count;			// Count of timer periods
	
	if(Bit)
		OCR0A	= CMATCH4800; // 2 cycles of 4800 = 2400
	else
		OCR0A	= CMATCH2400; // 2 cycles of 4800 = 1200
	
	Count=0;
	WriteDataPort(Output);	// init the cycle, always start high.

	for(Count=0; Count < 2; Count++)
	{
		WaitCTC();					// Wait for count
		ResetCTC();					// Reset compare match flag
		Output ^= 0x01;				// Flip output bit;
		WriteDataPortFast(Output);	// Write it.
        CPLDUpdateOut(Output);
	}
}
#endif


ISR(TIMER1_COMPA_vect)
{
    DATAPORT ^= 0x01;
    AssertLE();
    ClearLE();
    
}

#define BuffSZ  1500

void TapeEmulate(void)
{
#if 0
	UINT	BytesRead;
	UINT	ByteNo;
	BYTE 	Current;
	int		BitNo;
	char    Buff[BuffSZ];
    
	// Initialize counter.
	InitCTC();
	
//	f_read(&fil, globalData, GLOBUFFSIZE, &BytesRead);
	f_read(&fil, Buff, BuffSZ, &BytesRead);
	while (0 != BytesRead)
	{
		for(ByteNo=0; ByteNo < BytesRead; ByteNo++)
		{
//			Current=globalData[ByteNo];
			Current=Buff[ByteNo];
				
			// Generate FSK least significant bit first.
			for(BitNo=0; BitNo < 8; BitNo++)
			{
#if 0
                if(SnapPressed())
                    FSKBit(0x1);
                else
                    FSKBit(0x0);
#endif                
				FSKBit(Current & 0x01);			// Extract LSBit and send it
				Current = Current >> 1;				// Shift right
			}
		}
		
		// Read next chunk and loop again.
		f_read(&fil, globalData, BuffSZ, &BytesRead);
//        log0("%d bytes read\n",BytesRead);
	}
#endif
}

void inline TapeByteOut(BYTE    OutputByte)
{
	int		BitNo;

//    log0("%02X ",Output);
		
    // Generate FSK least significant bit first.
	for(BitNo=0; BitNo < 8; BitNo++)
	{
        FSKBit(OutputByte & 0x01);			// Extract LSBit and send it
		OutputByte = OutputByte >> 1;		// Shift right
	}
}

void TapeBlockOut(
    BYTE    *Block,
    BYTE    BlockType,
    BYTE    BlockLen,
    BYTE    LeaderLen)
{
    int     Count;
    BYTE    Checksum = BlockType+BlockLen;

    //log0("TapeBlockOut(%d,%d,%d)\n",BlockType,BlockLen,LeaderLen);
    //HexDump(Block,BlockLen,0);
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
    
    // Idle low.
//    WriteDataPortFast(0);
}

// Output the file who's name is in the global data block to the dragon
// as if input from tape. The CPLD will remap our output so that it looks
// as if it comes from the PIA at 0xFF20.
void TapeOut(void)
{
	UINT	BytesRead;
    BYTE    Buffer[CASDefBlockSize];
    BYTE    SyncSize;

	int	result;
    CASFilenameBlock    FBlock;
    DGNHead             FHead;

    // Initialize counter.
    InitCTC();

	result = fileOpen(FA_OPEN_EXISTING|FA_READ);
	if(result)
	{
		log0("ERROR 0x%02X opening Tape emulation file %s\n",result,globalData);
	}
	else
	{    	
        f_read(&fil, &FHead, sizeof(FHead), &BytesRead);
        
        // Setup filename block
        memset(&FBlock.Filename[0],0x20,CASFilenameLen);        // fill in filename
        strncpy(&FBlock.Filename[0],(CHAR *)globalData,CASFilenameLen);
        FBlock.FileType=CASFtMachineCode;                       // machine code file
        FBlock.AsciiFlag=CASAsBinary;                           // Binary
        FBlock.GapFlag=CASGfUngapped;                           // Ungapped
        FBlock.ExecAddress=FHead.HdrExec;                       // Exec address
        FBlock.LoadAddress=FHead.HdrLoad;                       // Load address

        SyncSize=128;
        
        // Output header
        TapeBlockOut((BYTE *)&FBlock.Filename,CASBtFileName,CASFNameBlockLen,SyncSize);      
        _delay_ms(90);
        
        f_read(&fil, Buffer, CASDefBlockSize, &BytesRead);
        while(0 != BytesRead)
        {
            TapeBlockOut(Buffer,CASBtData,BytesRead, SyncSize);
            _delay_ms(90);
            SyncSize=16;
            f_read(&fil, Buffer, CASDefBlockSize, &BytesRead);
        }
        
        TapeBlockOut(Buffer,CASBtEOF,0,16); 
        
        fileClose();
		log0("Tape emulation done\n");
	}
}