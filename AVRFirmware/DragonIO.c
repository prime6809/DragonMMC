/*
	DragonIO	: IO routines for the Dragon 32/64/Alpha and Tandy CoCo.

	2011-07-02, P.Harvey-Smith.
*/

#include <avr/interrupt.h>
#include <inttypes.h>
#include <util/delay.h>
#include "dragonio.h"
#include "status.h"
#include "mmc/mmc2io.h"
#include "xilinx/ports.h"

void InitIO(void)
{
	// IO reg as input 
	SetIORead();
	
	// Setup input interrupt, negedge
	EICRA &= ~INTINMASK;
	EICRA |= INTINDIR;
	
	// Enable interrupt
	EIMSK &= ~INTINMASK;
	//EIMSK |= INTIN;
	
	// Set intin pin as an input with pullups
	INTIN_DDR 	&= ~INTIN_PMASK;
	INTIN_PORT	|= INTIN_PMASK;
	
	// Setup Input latch OE
	OEDDR 	|= OEMASK;
	ClearOE();

	// Setup output latch 
	LEDDR	|= LEMASK;
	ClearLE();
	
	// Setup address line 
	ADDRDDR	|= ADDRMASK;
	SelectData();		
	
	// Setup busy line
	ST_DDR |= ST_BUSYMASK;
	ClearBusy();
	
	// Setup card inputs for card detect and write protect
	// enable pullups
	CARDIO_DDR	&= ~(CARD_DETECT_MASK | WRITE_PROT_MASK);
	CARDIO_PORT	|= CARD_DETECT_MASK | WRITE_PROT_MASK;
	
	//Init LEDS
	LEDPINSOUT();
	REDLEDOFF();
	GREENLEDOFF();

	ReleaseJTAG();
	
	//Setup snapshot button, input + pullup
	SNAP_DDR &= ~SNAP_MASK;
	SNAP_PORT |= SNAP_MASK;
	
	//Setup NMI line as output and clear NMI
	ClearNMI();
	NMI_DDR |= NMI_MASK;
}

void ReleaseJTAG(void)
{
	//Init CPLD jumper input bit, with pullup enabled
	JTAG_DDR &= ~(CPLD_UPDATE_MASK);
	JTAG_PORT |= CPLD_UPDATE_MASK;
	
	//Init CPLD JTAG bits as inputs, pullups enabled
	//We init the pins to the correct I/O in InitJTAG() below
	//just before using it.

	// Set TCK, TMS, TDO and TDI as inputs, pullups enabled.
	JTAG_DDR &= ~(TCK_MASK | TMS_MASK | TDI_MASK | TDO_MASK);
	JTAG_PORT |= (TCK_MASK | TMS_MASK | TDI_MASK | TDO_MASK);
}

/* Init bitbanged JTAG for programming XILINx CPLD for */
/* firmware upgrades */
void InitJTAG(void)
{
	// Set TCK, TMS and TDI as outputs
	JTAG_DDR |= (TCK_MASK | TMS_MASK | TDI_MASK);
	
	// Set TDO as an input 
	JTAG_DDR &= ~(TDO_MASK | CPLD_UPDATE_MASK);
}
