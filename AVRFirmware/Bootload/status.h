/*
	Status.h
	
	Functions for logging program status to the serial port, to
	be used for debugging pruposes etc.
	
	2008-03-21, P.Harvey-Smith.

	Some functions and macros borrowed from Dean Camera's LURFA 
	USB libraries.
	
*/

#include <avr/io.h>
#include <avr/pgmspace.h>
#include <stdbool.h>
#include <stdio.h>


#ifndef __STATUS_DEFINES__
#define __STATUS_DEFINES__

#ifdef SERIAL_STATUS
#if defined(RAMPZ) && defined(BOOT_ADDR)

#define RAMPZMASK   ((unsigned long)FLASHEND >> 16)
#define STATIC_BUF_SIZE 128
extern char format_buff[];

#define log0(format,...) {strncpy_PF(format_buff,(uint_farptr_t)PSTR(format),STATIC_BUF_SIZE); fprintf(&ser0stream,format_buff,##__VA_ARGS__); }
#define log1(format,...) {strncpy_PF(format_buff,PSTR(format),STATIC_BUF_SIZE); fprintf(&ser1stream,format_buff,##__VA_ARGS__); }
#define log0c(cond,format,...) {if (cond) {strncpy_PF(format_buff,PSTR(format),STATIC_BUF_SIZE); fprintf(&ser0stream,format_buff,##__VA_ARGS__);}; } 
#define log1c(cond,format,...) {if (cond) {strncpy_PF(format_buff,PSTR(format),STATIC_BUF_SIZE); fprintf(&ser0stream,format_buff,##__VA_ARGS__);}; }

#define log0i(format,...) {strncpy_PF(format_buff,format,STATIC_BUF_SIZE); fprintf(&ser0stream,format_buff,##__VA_ARGS__); }
#else
#define log0(format,...) fprintf_P(&ser0stream,PSTR(format),##__VA_ARGS__) 
#define log1(format,...) fprintf_P(&ser1stream,PSTR(format),##__VA_ARGS__)
#define log0c(cond,format,...) {if (cond) fprintf_P(&ser0stream,PSTR(format),##__VA_ARGS__);} 
#define log1c(cond,format,...) {if (cond) fprintf_P(&ser1stream,PSTR(format),##__VA_ARGS__);}

#define log0i(format,...) fprintf_P(&ser0stream,format,##__VA_ARGS__) 
#endif 

#else
#define log0(format,...) 
#define log1(format,...) 
#define log0(cond,format,...) 
#define log1(cond,format,...) 
#define log0i(format,...) 
#endif

//
// For stdio
//

extern FILE ser0stream;
extern FILE ser1stream;

/* Default baud rate if 0 passed to Serial_Init */
  
#define DefaultBaudRate	9600

/** Indicates whether a character has been received through the USART - boolean false if no character
 *  has been received, or non-zero if a character is waiting to be read from the reception buffer.
 */
#define Serial_IsCharReceived() ((UCSR1A & (1 << RXC1)) ? true : false)

/** Macro for calculating the baud value from a given baud rate when the U2X (double speed) bit is
 *  not set.
 */
#define SERIAL_UBBRVAL(baud)    (((F_CPU / 16) / baud) - 1)

/** Macro for calculating the baud value from a given baud rate when the U2X (double speed) bit is
 *  set.
 */
#define SERIAL_2X_UBBRVAL(baud) (((F_CPU / 8) / baud) - 1)

#define SerEOL0()	{ Serial_TxByte0('\r'); Serial_TxByte0('\n'); } 

#ifdef NOUSART1
#undef UCSR1A
#endif

void USART_Init0(const uint32_t BaudRate);
void Serial_TxByte0(const char DataByte);
char Serial_RxByte0(void);
uint8_t Serial_ByteRecieved0(void);

void USART_Init1(const uint32_t BaudRate);
void Serial_TxByte1(const char DataByte);
char Serial_RxByte1(void);
uint8_t Serial_ByteRecieved1(void);

void Serial_Init(const uint32_t BaudRate0,
				 const uint32_t BaudRate1);

void cls(uint8_t	Port);
#endif
