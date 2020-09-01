/*
  pcf2129rtc.h
  
  NXP PCF2129AT / PCF2129T driver in SPI mode.
  
  Taken from NXP datasheet

  All Time / Alarm registers are in BCD format.
*/

#ifndef __pcf2129rtc__
#define __pcf2129rtc__

// Register offsets
// Control registers
#define PCF2129_CTRL1       0x00
#define PCF2129_CTRL2       0x01
#define PCF2129_CTRL3       0x02

// Time registers
#define PCF2129_SECONDS     0x03
#define PCF2129_MINUTES     0x04
#define PCF2129_HOURS       0x05
#define PCF2129_DAYS        0x06
#define PCF2129_WEEKDAYS    0x07
#define PCF2129_MONTHS      0x08
#define PCF2129_YEARS       0x09

// Alarm registers
#define PCF2129_SECONDSA    0x0A
#define PCF2129_MINUTESA    0x0B
#define PCF2129_HOURSA      0x0C
#define PCF2129_DAYSA       0x0D
#define PCF2129_WEEKDAYSA   0x0E

// Clkout register
#define PCF2129_CLKOUT      0x0F

// Watchdog registers
#define PCF2129_WDCTRL      0x10
#define PCF2129_WDVAL       0x11

// Timestamp registers
#define PCF2129_TSCTRL      0x12
#define PCF2129_TSSECOND    0x13
#define PCF2129_TSMINUTE    0x14
#define PCF2129_TSHOUR      0x15
#define PCF2129_TSDAY       0x16
#define PCF2129_TSMONTH     0x17
#define PCF2129_TSYEAR      0x18

// Aging register
#define PCF2129_AGING       0x19

// Internal registers
#define PCF2129_INTERNAL1   0x1A
#define PCF2129_INTERNAL2   0x1B

// Bit masks for CTRL1
// undefined bits must always be written as 0
#define C1_EXT_TST          0x80
#define C1_STOP             0x20
#define C1_TSF1             0x10
#define C1_POR_OVRD         0x08
#define C1_12_24            0x04
#define C1_MI               0x02
#define C1_SI               0x01

// Bit masks for CTRL2
// undefined bits must always be written as 0
#define C2_MSF              0x80
#define C2_WDTF             0x40
#define C2_TSF2             0x20
#define C2_AF               0x10
#define C2_TSIE             0x04
#define C2_AIE              0x02

// Bit masks for CTRL3
#define C3_PWMNG2           0x80
#define C3_PWMNG1           0x40
#define C3_PWMNG0           0x20
#define C3_BTSE             0x10
#define C3_BF               0x08
#define C3_BLF              0x04
#define C3_BIE              0x02
#define C3_BLIE             0x01

// Bitmasks for time / alarm registers

#define SECONDS_OSF         0x80
#define SECONDS_MASK        0x7F
#define MINUTES_MASK        0x7F
#define HOURS_MASKAMPM      0x20
#define HOURS_MASK12        0x1F
#define HOURS_MASK24        0x3F
#define DAYS_MASK           0x3F
#define WEEKDAYS_MASK       0x07
#define MONTHS_MASK         0x1F
#define YEARS_MASK          0xFF

// Bitmasks for alarm registers. 
// Alarm time masks are the same as for the time registers

#define ALARM_AE_S          0X80
#define ALARM_AE_M          0X80
#define ALARM_AE_H          0X80
#define ALARM_AE_D          0X80
#define ALARM_AE_W          0X80

// Bitmasks for CLKOUT register
#define CLKOUT_TCR1         0x80
#define CLKOUT_TCR0         0x40
#define CLKOUT_OTPR         0x20
#define CLKOUT_COF2         0x04
#define CLKOUT_COF1         0x02
#define CLKOUT_COF0         0x01

// Bitmasks for watchdog register
// Undefined bits should always be 0
#define WATCH_WD_CD         0x80
#define WATCH_TI_TP         0x20
#define WATCH_TF1           0x02
#define WATCH_TF0           0x01
#define WATCH_VALUE         0xFF

// Timestamp time bitmasks as for time / alarm

// Bitmasks for aging offset
#define AGING_AO3           0x08
#define AGING_AO2           0x04
#define AGING_AO1           0x02
#define AGING_AO0           0x01

// Commands to read and write registers

#define CMD_READ            0xA0
#define CMD_WRITE           0x20
#define CMD_REG_MASK        0x1F

#define NO_TIME_REGS        7

extern uint8_t rtc_time_regs[NO_TIME_REGS];

// Offsets in rtc_time_regs
#define RTC_SECONDS         (PCF2129_SECONDS-PCF2129_SECONDS)
#define RTC_MINUTES         (PCF2129_MINUTES-PCF2129_SECONDS)
#define RTC_HOURS           (PCF2129_HOURS-PCF2129_SECONDS)
#define RTC_DAYS            (PCF2129_DAYS-PCF2129_SECONDS)
#define RTC_WEEKDAYS        (PCF2129_WEEKDAYS-PCF2129_SECONDS)
#define RTC_MONTHS          (PCF2129_MONTHS-PCF2129_SECONDS)
#define RTC_YEARS           (PCF2129_YEARS-PCF2129_SECONDS)

#define BCDtoBIN(BCD)       ((BCD & 0x0F) + (((BCD & 0xF0)>>4)*10))

void rtcInit(void);
void rtcUpdate(void);
void rtcSendRegs(uint8_t    FirstReg,
                 uint8_t    Count,
                 uint8_t    *Regs);

void rtcGetRegs(uint8_t     FirstReg,
                uint8_t     Count,
                uint8_t     *Regs);
                 
char *BCDtoASCII(uint8_t	BCDByte,
				 char		*Dest);

char * rtcDateTimeASCII(void);

uint8_t ASCIItoBCD(char     *ASCII,
                   uint8_t  Digits);
                   
uint8_t ValidASCIIDateTime(char *DateTime);

uint8_t ASCIIDateTimetoRegs(char    *DateTime,
                            uint8_t *Regs);
                            
uint8_t UpdateRTCFromASCII(char    *DateTime);

DWORD rtcDOS(void);

#endif


