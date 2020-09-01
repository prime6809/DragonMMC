#ifndef _IO

#include <avr/io.h>
#include <avr/pgmspace.h>
#include <avr/eeprom.h>

#define LOG_RESULT

#define REDLED		1
#define GREENLED	0

#define REDMASK		(1<<REDLED)
#define	GREENMASK	(1<<GREENLED)

#define LEDDDR		DDRB
#define LEDPORT		PORTB

#define LEDPINSOUT() 	LEDDDR |= (REDMASK | GREENMASK)

#define REDLEDON()		    LEDPORT &= ~REDMASK
#define REDLEDOFF()		    LEDPORT |= REDMASK
#define REDLEDTOGGLE()      LEDPORT ^= REDMASK
#define GREENLEDON()	    LEDPORT &= ~GREENMASK
#define GREENLEDOFF()	    LEDPORT |= GREENMASK
#define GREENLEDTOGGLE()	LEDPORT ^= GREENMASK

#define CARD_DETECT	2
#define WRITE_PROT	3

#define CARD_DETECT_MASK	(1<<CARD_DETECT)
#define WRITE_PROT_MASK		(1<<WRITE_PROT)

#define CARDIO_DDR	DDRB
#define CARDIO_PORT	PORTB
#define CARDIO_PIN	PINB

#define CARD_INSERTED()		(~CARDIO_PIN & CARD_DETECT_MASK)
#define CARD_WRITABLE()		(CARDIO_PIN & WRITE_PROT_MASK)

#define ASSERTIRQ()
#define RELEASEIRQ()

#define NOPDelay()		{ asm("nop; nop;"); }

/* Dataport for communication with host processor */
#define DATAPORT	PORTC
#define DATAPIN		PINC
#define	DATADDR		DDRC

#define SetIOWrite()	{ DATADDR=0xFF; };
#define SetIORead() 	{ DATADDR=0x00; };

/* OE line from input latch */
#define	OEPORT	PORTA
#define	OE		4		
#define OEMASK	(1 << OE)
#define OEDDR	DDRA

#define AssertOE()	{ OEPORT &= ~OEMASK; NOPDelay(); };
#define ClearOE()	{ OEPORT |= OEMASK; };

/* LE line for output latch */
#define	LEPORT		PORTA
#define	LE			5		
#define LEMASK		(1 << LE)
#define LEDDR		DDRA

#define AssertLE()	{ LEPORT &= ~LEMASK; NOPDelay(); };
#define ClearLE()	{ LEPORT |= LEMASK; };

/* Address pin, selects either latched address or data reg */
#define ADDRPORT	PORTA
#define ADDR		0
#define ADDRMASK	(1 << ADDR)
#define ADDRDDR		DDRA

#define SelectData()	ADDRPORT &= ~ADDRMASK
#define SelectAddr()	ADDRPORT |= ADDRMASK

/* Status lines so that AVR can signal status to Dragon */
#define ST_PORT		PORTA
#define ST_DDR		DDRA
#define ST_BUSY		6
#define ST_BUSYMASK	(1<<ST_BUSY)

#define SetBusy()	ST_PORT |= ST_BUSYMASK
#define ClearBusy()	ST_PORT &= ~ST_BUSYMASK

/* When reading status from Dragon */
#define RWLine			6
#define RWMask			(1 << RWLine)
#define ADDR_LINE_MASK	0x0F

#define MotorLine		7
#define MOTOR_MASK		(1 << MotorLine)

#define LatchAddressIn()				{ SelectAddr(); SetIORead(); AssertOE(); LatchedAddressLast=DATAPIN; ClearOE(); }
#define ReadDataPort()					{ SelectData(); SetIORead(); AssertOE(); LatchedData=DATAPIN; ClearOE(); }
#define ReadDataPortFast(outvar)		{ AssertOE(); outvar=DATAPIN; ClearOE(); }
#define WriteDataPort(value)			do { SelectData(); SetIOWrite(); DATAPORT=value; AssertLE(); ClearLE(); } while (0)
#define WriteDataPortFast(value)		do { DATAPORT=value; AssertLE(); ClearLE(); } while (0)

#define AddressPORT	
#ifdef LOG_RESULT
#define WriteStatus(status)					do { WriteDataPort(status); log0c(DEBUG_REPLY,"R:%02X\n",status); } while (0)
#else
#define WriteStatus(status)					do { WriteDataPort(status); } while (0)
#endif
#define WriteStatusCond(cond,tcond,fcond)	{ if (cond) WriteStatus(tcond); else WriteStatus(fcond); }

#define WASWRITE				((LatchedAddressLast & RWMask)==0) 
#define MotorIsOff				((LatchedAddressLast & MOTOR_MASK)==0)

#define ReadEEPROM(addr)		eeprom_read_byte ((const uint8_t *)(addr))	
#define WriteEEPROM(addr, val)	eeprom_write_byte ((uint8_t *)(addr), (uint8_t)(val))

#define _IO
#endif
