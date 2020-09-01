/*******************************************************/
/* file: ports.h                                       */
/* abstract:  This file contains extern declarations   */
/*            for providing stimulus to the JTAG ports.*/
/*******************************************************/

#ifndef ports_dot_h
#define ports_dot_h

/* these constants are used to send the appropriate ports to setPort */
/* they should be enumerated types, but some of the microcontroller  */
/* compilers don't like enumerated types */
#define TCK (short) 0
#define TMS (short) 1
#define TDI (short) 2

#define JTAG_PORT	PORTD
#define JTAG_PIN	PIND
#define JTAG_DDR	DDRD
#define TCK_LINE	3
#define TMS_LINE	6
#define TDI_LINE	5
#define TDO_LINE	4
#define CPLD_UPDATE	7

#define TCK_MASK			(1<<TCK_LINE)
#define TMS_MASK			(1<<TMS_LINE)
#define TDI_MASK			(1<<TDI_LINE)
#define TDO_MASK			(1<<TDO_LINE)
#define CPLD_UPDATE_MASK	(1<<CPLD_UPDATE)

#define CPLDUpdateFlag()	(~JTAG_PIN & CPLD_UPDATE_MASK)
#define CPLDUpdateAsOutput()  JTAG_DDR |= CPLD_UPDATE_MASK
#define CPLDUpdateOut(bit)  { if (bit) JTAG_PORT |= CPLD_UPDATE_MASK else JTAG_PORT &= ~CPLD_UPDATE_MASK; }
#define FirmwareFile		"DGNMMC.SVF"


/* set the port "p" (TCK, TMS, or TDI) to val (0 or 1) */
extern void setPort(short p, short val);

/* read the TDO bit and store it in val */
extern unsigned char readTDOBit(void);

/* make clock go down->up->down*/
extern void pulseClock(void);

/* read the next byte of data from the xsvf file */
extern void readByte(unsigned char *data);

extern void waitTime(long microsec);

#endif
