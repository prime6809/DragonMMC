/*

	TapeEmulate.
	
	Emulates the Dragon / CoCo tape input stream by taking an opened CAS file
	reading it and generating a digital FSK representation of the signal.
	This is written to the CPLD which is programmed to return this value 
	instead of the one from the PIA port at $FF20.
	
	First version: 2017-02-22.

*/

#ifndef _TAPE_EMULATE
#define _TAPE_EMULATE
void TapeEmulate(void);
void TapeOut(void);

#endif