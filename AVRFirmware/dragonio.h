/*
	DragonIO	: IO routines for the Dragon 32/64/Alpha.

	2011-06-16, Phill Harvey-Smith.
*/

#ifndef _DRAGON_IO_H_
#define _DRAGON_IO_H_

#include "mmc/mmc2io.h"
#include "xilinx/ports.h"
#include "xilinx/micro.h"

/* Interrupt trigered by host writing to input latch */
#define INTIN_vect	INT0_vect
#define INTIN		(1<<INT0)
#define INTINMASK	((1<<ISC00) | (1<<ISC01))
#define	INTINDIR	(1<<ISC01)	// Negedge triggered
#define INTIMSK		(1<<INT0)

#define LatchInt()	((EIFR & INTIN)!=0)
#define ClearLatchInt()	{ EIFR |= INTIN; }

#define EnableInt()		(EIMSK |= INTIMSK)
#define DisableInt()	(EIMSK &= ~INTIMSK)

#define INTIN_PORT PORTD
#define INTIN_DDR	DDRD
#define INTIN_PIN	PIND
#define INTIN_PPIN	2
#define INTIN_PMASK	(1<<INTIN_PPIN)

// Snapshot switch
#define SNAP_DDR	DDRA
#define SNAP_PIN	PINA
#define SNAP_PORT	PORTA

#define SNAP_SWITCH	7
#define SNAP_MASK	(1<<SNAP_SWITCH)

#define SnapPressed()		(~SNAP_PIN & SNAP_MASK)
#define WaitSnapsRelease()	do {} while (SnapPressed())  

// NMI line to 6809 (through CPLD)
#define NMI_DDR		DDRA
#define NMI_PIN		PINA
#define NMI_PORT	PORTA

#define NMI_LINE	2
#define NMI_MASK	(1<<NMI_LINE)

#define AssertNMI()	NMI_PORT &= ~NMI_MASK
#define ClearNMI()	NMI_PORT |= NMI_MASK

void InitSPI(void);
void InitIO(void);
void ReleaseJTAG(void);
void InitJTAG(void);
uint8_t ReadIO(void);
void WriteIO(uint8_t ToWrite);
void InitMMC(void);
char SPI(uint8_t ToSend);
uint8_t MMCCommand(char befF, uint16_t AdrH, uint16_t AdrL, char befH, char DoDisable);
#endif