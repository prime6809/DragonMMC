#ifndef _MMC

#include "ff.h"

/* SPI stuff : PHS 2010-06-08 */
#define SPIPORT			PORTB
#define SPIPIN			PINB
#define	SPIDDR			DDRB
#define SPI_SS			4
#define SPI_MOSI		5
#define SPI_MISO		6
#define	SPI_SCK			7

#define SPI_MASK		((1<<SPI_SS) | (1<<SPI_MOSI) | (1<<SPI_MISO) | (1<<SPI_SCK))
#define SPI_SS_MASK		(1<<SPI_SS)

#define AssertSDCS()	{ SPIPORT &= ~SPI_SS_MASK; };
#define ClearSDCS()		{ SPIPORT |= SPI_SS_MASK; };
#define WaitSPI()		{ while(!(SPSR & (1<<SPIF))); }; 

#define MMC_SEL()       (SPI_CS_PIN & SPI_SS_MASK)==0

// For PCF2129AT RTC, in SPI mode, the RTC shares MOSI, MISO and SCK with the
// SD/MMC, so we only need to define a chip select for the RTC

#define RTCPORT			PORTA
#define RTCDDR          DDRA
#define RTCPIN          PINA
#define RTCCS           3

#define RTCCS_MASK      (1<<RTCCS)

#define AssertRTCCS()	{ RTCPORT &= ~RTCCS_MASK; };
#define ClearRTCCS()	{ RTCPORT |= RTCCS_MASK; };

/* Card type flags (CardType) */
#define CT_MMC 			0x01 				/* MMC ver 3 */
#define CT_SD1 			0x02 				/* SD ver 1 */
#define CT_SD2 			0x04 				/* SD ver 2 */
#define CT_SDC 			(CT_SD1|CT_SD2) 	/* SD */
#define CT_BLOCK 		0x08 				/* Block addressing */

void INIT_SPI(void);
BYTE XFER_SPI(BYTE output);


#define _MMC
#endif

