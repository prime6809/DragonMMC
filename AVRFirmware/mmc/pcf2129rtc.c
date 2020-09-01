/*
  pcf2129rtc.c
  
  NXP PCF2129AT / PCF2129T driver in SPI mode.
*/

#include <avr/io.h>
#include <string.h>
#include "mmcio.h"

#include "mmc2io.h"
#include "diskio.h"
#include "status.h"
#include "pcf2129rtc.h"

uint8_t rtc_time_regs[NO_TIME_REGS];

void rtcInit(void)
{
    uint8_t CTRL[3];        // Temp store for control regs.
    
    // Init the RTC chip select line, this assumes that INIT_SPI() 
    // is called elsewhere
    RTCDDR |= RTCCS_MASK;   // Setup the pin
    ClearRTCCS();           // Deselect it

    // Set control registers, see datasheet for details.
    CTRL[0]=0x00;           // normal operation, no interrupts, no timestamp, 24 hour clock
    CTRL[1]=0x00;           // no interupts or alarms
    CTRL[2]=0x00;           // Normal operation with battery switchover, battery low detection enabled.
    
    // Set control registers
    rtcSendRegs(PCF2129_CTRL1,3,&CTRL[0]);
    
    // Set CLKOUT & Watchdog regs, re-use CTRL var.....
    CTRL[0]=0x07;           // Temp compensation every 4 mins, no clock out.
    CTRL[1]=0x00;           // Watchdog disabled
    
    // Set CLKOUT register
    rtcSendRegs(PCF2129_CLKOUT,2,&CTRL[0]); 
    
    // Refresh time registers
    rtcUpdate();
}

void rtcUpdate(void)
{
    // Refresh time registers
    rtcGetRegs(PCF2129_SECONDS,7,&rtc_time_regs[0]);
}

/*
  Send a pointed to group of RTC registers to the RTC over SPI
*/
void rtcSendRegs(uint8_t    FirstReg,
                 uint8_t    Count,
                 uint8_t    *Regs)
                 
{
    uint8_t CmdByte;
    uint8_t RegNo;
    
    // Generate command, and combine with first register  
    CmdByte = CMD_WRITE | (FirstReg & CMD_REG_MASK);    
    
    // Select RTC
    AssertRTCCS();
 
    // Send command byte to RTC
    XFER_SPI(CmdByte);  
    
    // Send register bytes to RTC
    for(RegNo=0; RegNo<Count; RegNo++)
        XFER_SPI(Regs[RegNo]);
        
    // Deselect RTC
    ClearRTCCS();
}

void rtcGetRegs(uint8_t     FirstReg,
                uint8_t     Count,
                uint8_t     *Regs)
{
    uint8_t CmdByte;
    uint8_t RegNo;
    
    // Generate command, and combine with first register  
    CmdByte = CMD_READ | (FirstReg & CMD_REG_MASK);    
    
    // Select RTC
    AssertRTCCS();
 
    // Send command byte to RTC
    XFER_SPI(CmdByte);  
    
    // Receive register bytes to RTC
    for(RegNo=0; RegNo<Count; RegNo++)
        Regs[RegNo]=XFER_SPI(0xFF);
        
    // Deselect RTC
    ClearRTCCS();
}

/*
    Convert BCD byte to two ASCII characters.
    Note, does no error checking!
    Note, returns pointer to static buffer!
*/
char *BCDtoASCII(uint8_t	BCDByte,
				 char		*Dest)
{
	uint8_t	nibble;
	
	// Deal with MSN
	nibble=(BCDByte & 0xFF)>>4;
	*Dest++=nibble+'0';
	
	// now do LSN
	nibble=(BCDByte & 0x0F);
	*Dest++=nibble+'0';

	return Dest;
}

char * rtcDateTimeASCII(void)
{
    static char TimeBuff[24];
    char        *Ptr;

	// Initialise pointer at begining of buffer
	Ptr=TimeBuff;
	
	// MS digits of century, I very much doubt this code will live 
	// beyond 2099 !
	*Ptr++='2';
	*Ptr++='0';

    // Convert years
    Ptr=BCDtoASCII(rtc_time_regs[RTC_YEARS] & YEARS_MASK,Ptr);
    *Ptr++='-';
    
    // Convert months
    Ptr=BCDtoASCII(rtc_time_regs[RTC_MONTHS] & MONTHS_MASK,Ptr);
    *Ptr++='-';
    
    // Convert month day
    Ptr=BCDtoASCII(rtc_time_regs[RTC_DAYS] & DAYS_MASK,Ptr);
    *Ptr++=' ';
    
    // Convert hours
    Ptr=BCDtoASCII(rtc_time_regs[RTC_HOURS] & HOURS_MASK24,Ptr);
    *Ptr++=':';
    
    // Convert minutes
    Ptr=BCDtoASCII(rtc_time_regs[RTC_MINUTES] & MINUTES_MASK,Ptr);
    *Ptr++=':';
    
    // Convert seconds
    Ptr=BCDtoASCII(rtc_time_regs[RTC_SECONDS] & SECONDS_MASK,Ptr);

    // Null terminator
	*Ptr++=0x00;

    return TimeBuff;
}
/*
    Convert two ASCII digits to one BCD byte
*/

uint8_t ASCIItoBCD(char     *ASCII,
                   uint8_t  Digits)
{
    uint8_t BCD;
    
    BCD=(ASCII[0]-'0');
    
    if(Digits>1)
        BCD=(BCD << 4)+(ASCII[1]-'0');

    return BCD;
}

/*
    Validate that ascii time is in the format YYYY-MM-DD hh:mm:ss
*/

// Table of characters valid for each position in the date
const uint8_t ValidChars[20][2] PROGMEM = 
{
    {'2','2'},      // Century must be 20xx
    {'0','0'},
    {'0','9'},      // Year 00..99
    {'0','9'},
    {'-','-'},
    {'0','2'},      // Month 01..12
    {'0','9'},
    {'-','-'},
    {'0','3'},      // Month 01..31
    {'0','9'},
    {' ',' '},
    {'0','2'},      // Hour 00..23
    {'0','9'},
    {':',':'},
    {'0','5'},      // Minute 00..59
    {'0','9'},
    {':',':'},
    {'0','5'},      // Second 00..59
    {'0','9'}
};

uint8_t ValidASCIIDateTime(char *DateTime)
{
    uint8_t CharNo;
    uint8_t Valid = 1;
    uint8_t Min;
    uint8_t Max;
    
    for(CharNo=0; CharNo<20; CharNo++)
    {
        Min=pgm_read_byte(&(ValidChars[CharNo][0]));
        Max=pgm_read_byte(&(ValidChars[CharNo][1]));
        
        if((DateTime[CharNo] < Min) || (DateTime[CharNo] > Max))
            Valid = 0;
    }
        
    return Valid;
}

/*
    Update regs *IF* ASCII time is valid.
    0000000000111111111 
    0123456789012345678
    YYYY-MM-DD hh:mm:ss
*/

uint8_t ASCIIDateTimetoRegs(char    *DateTime,
                            uint8_t *Regs)
{
    uint8_t Valid;
    uint8_t TempRegs[NO_TIME_REGS];
    uint8_t BCDVal;
    uint8_t BINVal;

    // Preserve weekday
    TempRegs[RTC_WEEKDAYS]=Regs[RTC_WEEKDAYS];
    
    Valid = ValidASCIIDateTime(DateTime);
    if(Valid)
    {
        // Years can be 00..99 so always valid
        BCDVal=ASCIItoBCD(&DateTime[2],2);
        TempRegs[RTC_YEARS]=BCDVal & YEARS_MASK;
 
        // Month must be 01..12
        BCDVal=ASCIItoBCD(&DateTime[5],2);
        BINVal=BCDtoBIN(BCDVal);
        
        TempRegs[RTC_MONTHS]=BCDVal & MONTHS_MASK;
        if((BINVal < 1) || (BINVal > 12))
            Valid = 0;
        
        // Day must be 01..31
        BCDVal=ASCIItoBCD(&DateTime[8],2);
        BINVal=BCDtoBIN(BCDVal);
            
        TempRegs[RTC_DAYS]=BCDVal & DAYS_MASK;
        if((BINVal < 1) || (BINVal > 31))
            Valid = 0;
        
        // Hours must be 00..23
        BCDVal=ASCIItoBCD(&DateTime[11],2);
        BINVal=BCDtoBIN(BCDVal);
        
        TempRegs[RTC_HOURS]=BCDVal & HOURS_MASK24;
        if(BINVal > 23)
            Valid = 0;
     
        // Minutes must be 00..59
        BCDVal=ASCIItoBCD(&DateTime[14],2);
        BINVal=BCDtoBIN(BCDVal);
        
        TempRegs[RTC_MINUTES]=BCDVal & MINUTES_MASK;
        if(BINVal > 59)
            Valid = 0;

        // Seconds must be 00..59
        BCDVal=ASCIItoBCD(&DateTime[17],2);
        BINVal=BCDtoBIN(BCDVal);
        
        TempRegs[RTC_SECONDS]=BCDVal & SECONDS_MASK;
        if(BINVal > 59)
            Valid = 0;   
  
        // If all valid update master copy of regs
        if(Valid)
            memcpy(Regs,&TempRegs[0],NO_TIME_REGS);
    }
    
    return Valid;
}

/*
    Update the rtc from a supplied date and tiome in ASCII.
*/

uint8_t UpdateRTCFromASCII(char    *DateTime)
{
    uint8_t Valid;
    
    Valid=ASCIIDateTimetoRegs(DateTime,&rtc_time_regs[0]);
    
    if(Valid)
    {
        rtcSendRegs(PCF2129_SECONDS,NO_TIME_REGS,&rtc_time_regs[0]);
    }
    
    return Valid;
}

/*
    Return data and time in DOS format for fatfs.
    Dos format is a 32 bit word of the format
     3         2         1         0
    10987654321098765432109876543210
    yyyyyyymmmmdddddhhhhhmmmmmmsssss
*/
DWORD rtcDOS(void)
{
    rtcUpdate();

    return ((DWORD)(BCDtoBIN(rtc_time_regs[RTC_YEARS] & YEARS_MASK) + 20) << 25)  +
           ((DWORD)BCDtoBIN(rtc_time_regs[RTC_MONTHS] & MONTHS_MASK) << 21)       +
           ((DWORD)BCDtoBIN(rtc_time_regs[RTC_DAYS] & DAYS_MASK) << 16)           +
           ((DWORD)BCDtoBIN(rtc_time_regs[RTC_HOURS] & HOURS_MASK24) << 11)       +
           ((DWORD)BCDtoBIN(rtc_time_regs[RTC_MINUTES] & MINUTES_MASK) << 5)      +
           ((DWORD)BCDtoBIN(rtc_time_regs[RTC_SECONDS] & SECONDS_MASK) >> 1);
}
