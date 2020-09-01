/*
  diskio routines for bootloader.
*/

#include "bootloader.h"
#include "integer.h"
#include "diskio.h"
#include "../mmc/mmc2.h"
#include "../mmc/mmcio.h"
#include "../mmc/mmc2io.h"
#include "../mmc/mmc2def.h"
#include "../DragonIO.h"

/* Definitions for MMC/SDC command */

#define CMD0  	(0x40+0)    /* GO_IDLE_STATE */
#define CMD1  	(0x40+1)    /* SEND_OP_COND (MMC) */
#define ACMD41 	(0xC0+41)   /* SEND_OP_COND (SDC) */
#define CMD8  	(0x40+8)    /* SEND_IF_COND */
#define CMD16 	(0x40+16)   /* SET_BLOCKLEN */
#define CMD17 	(0x40+17)   /* READ_SINGLE_BLOCK */
#define CMD24 	(0x40+24)   /* WRITE_BLOCK */
#define CMD55 	(0x40+55)   /* APP_CMD */
#define CMD58 	(0x40+58)   /* READ_OCR */

void INIT_SPI(void)
{
	// SCK, MOSI and SS as outputs, MISO as output
	SPIDDR |= ((1<<SPI_SCK) | (1<<SPI_MOSI) | (1<<SPI_SS));
	SPIDDR &= ~(1<<SPI_MISO);
	
	// initialize SPI interface
	// master mode
	SPCR |= (1<<MSTR);
	
	// select clock phase positive-going in middle of data
	// Data order MSB first
	// switch to f/4 2X = f/2 bitrate
    // SPCR &= ~((1<<CPOL) | (1<<DORD) | (1<<SPR0) | (1<<SPR1));
	
	SPCR &= ~((1<<SPR0) | (1<<SPR1));
	SPSR |= (1<<SPI2X);
	SPCR |= (1<<SPE);
	
	ClearSDCS();
	SPIPORT |= (1<<SPI_SCK);	// set SCK hi
}

BYTE XFER_SPI(BYTE output)
{
	SPDR = output;						// send to SPI
	loop_until_bit_is_set(SPSR,SPIF);	// wait for it to be sent
	return SPDR;						// return received byte
}

/*-----------------------------------------------------------------------*/
/* Deselect the card and release SPI bus                                 */
/*-----------------------------------------------------------------------*/

void release_spi(void)
{
   ClearSDCS();
   XFER_SPI(0xff);
}

void SinkXFER(void)
{
    XFER_SPI(0xff);
}

/*-----------------------------------------------------------------------*/
/* Send a command packet to MMC                                          */
/*-----------------------------------------------------------------------*/

static BYTE send_cmd (BYTE cmd, DWORD arg)
{
   BYTE n, res;

	//log0("send_cmd(%02X, %08X)",cmd,arg);

   union
   {
      BYTE b[4];
      DWORD d;
   }
   argbroke;
   argbroke.d = arg;

    if (cmd & 0x80)
    {
        /* ACMD<n> is the command sequense of CMD55-CMD<n> */
        cmd &= 0x7F;

        res = send_cmd(CMD55, 0);
        if (res > 1)
        {
            return res;
        }
   }

   /* Select the card */

   ClearSDCS();
   XFER_SPI(0xff);
   AssertSDCS();
   XFER_SPI(0xff);

   /* Send a command packet */

   XFER_SPI(cmd);                /* Start + Command index */
   XFER_SPI(argbroke.b[3]);         /* Argument[31..24] */
   XFER_SPI(argbroke.b[2]);         /* Argument[23..16] */
   XFER_SPI(argbroke.b[1]);         /* Argument[15..8] */
   XFER_SPI(argbroke.b[0]);         /* Argument[7..0] */
   n = 0x01;                     /* Dummy CRC + Stop */
   if (cmd == CMD0) n = 0x95;       /* Valid CRC for CMD0(0) */
   if (cmd == CMD8) n = 0x87;       /* Valid CRC for CMD8(0x1AA) */
   XFER_SPI(n);

    /* Wait for a valid response in timeout of 10 attempts */

   n = 10;
   do
    {
      res = XFER_SPI(0xff);
   }
   while ((res & 0x80) && --n);

	//log0("res=%02X, n=%02X\n",res,n);

   return res;
}

#if 0
DRESULT disk_read_sector(DWORD lba)
{
   DRESULT res;
   BYTE rc;
   WORD bc;

   GREENLEDON();

//log0("disk_read_sector(%08X)\n",lba);

   if (!(CardType & CT_BLOCK))
   {
      /* Convert to byte address if needed */
        //lba *= 512;
        lba <<= 9;
   }

   res = RES_ERROR;

    /* READ_SINGLE_BLOCK */
   if (send_cmd(CMD17, lba) == 0)
    {
      /* Wait for data packet with timeout of 100s of ms */
      bc = 30000;
      do
      {
         rc = XFER_SPI(0xff);
      }
      while (rc == 0xFF && --bc);

        if (rc == 0xFE)
        {
            BYTE* data = &globalData[0];

            /* A data packet arrived */
            for(bc = 0; bc <512; ++bc)
            {
                data[bc] = XFER_SPI(0xff);
            }

            /* skip CRC */
         SinkXFER(); // less code than 'XFER_SPI(0xff);'
         SinkXFER();

         res = RES_OK;
      }
   }

   release_spi();

   GREENLEDOFF();

   return res;
}
#endif


BYTE disk_initialize(void)
{
    WORD tmr;
    BYTE n, cmd, ty, ocr[4];

    GREENLEDON();
    INIT_SPI();
 
    /* send dummy clocks */
    /* at least 80 - enough to fluch through any unfinished cmd sequence */
   for (n = 0; n < 10; ++n)
   {
        SinkXFER();
   }

    /* initialise card & determine its capabilities */
   ty = 0;

    /* Enter Idle state */
    if (send_cmd(CMD0, 0) == 1)
    {
      if (send_cmd(CMD8, 0x1AA) == 1)
        {
            /* it's an SDv2 */
            /* Get trailing return value of R7 resp */
          SinkXFER();
          SinkXFER();
          ocr[2] = XFER_SPI(0xff);
          ocr[3] = XFER_SPI(0xff);

            if (ocr[2] == 0x01 && ocr[3] == 0xAA)
            {
                /* The card can work at vdd range of 2.7-3.6V */
                /* Wait for leaving idle state (ACMD41 with HCS bit) */
                for (tmr = 0x3000; tmr && send_cmd(ACMD41, 1UL << 30); --tmr);

                if (tmr && send_cmd(CMD58, 0) == 0)
                {
                    /* Check CCS bit in the OCR */
                ocr[0] = XFER_SPI(0xff);
                SinkXFER(); // code size reduction
                SinkXFER();
                SinkXFER();

                    /* SDv2 (HC or SC) */
               ty = (ocr[0] & 0x40) ? CT_SD2 | CT_BLOCK : CT_SD2;
            }
         }
      }
      else
        {
            /* SDv1 or MMCv3 */
            if (send_cmd(ACMD41, 0) <= 1)
            {
                /* SDv1 */
                ty = CT_SD1;
                cmd = ACMD41;
            }
         else
         {
            /* MMCv3 */
            ty = CT_MMC;
            cmd = CMD1;
         }

         /* Wait for leaving idle state */
         for (tmr = 0x7000; tmr && send_cmd(cmd, 0); tmr--);

            /* Set R/W block length to 512 */
            if (!tmr || send_cmd(CMD16, 512) != 0)
            {
            ty = 0;
            }
      }
   }

   release_spi();

   GREENLEDOFF();

   return ty;
}

/*-----------------------------------------------------------------------*/
/* Read partial sector                                                   */
/*-----------------------------------------------------------------------*/

DRESULT disk_readp (
	BYTE *buff,		/* Pointer to the read buffer (NULL:Read bytes are forwarded to the stream) */
	DWORD sector,	/* Sector number (LBA) */
	UINT offset,	/* Byte offset to read from (0..511) */
	UINT count		/* Number of bytes to read (ofs + cnt mus be <= 512) */
)
{
	DRESULT res;
	BYTE rc;
	UINT bc;

    GREENLEDON();

	if (!(CardType & CT_BLOCK)) sector *= 512;	/* Convert to byte address if needed */

	res = RES_ERROR;
	if (send_cmd(CMD17, sector) == 0) {		/* READ_SINGLE_BLOCK */

		bc = 40000;
		do {							/* Wait for data packet */
			rc = XFER_SPI(0xFF);
		} while (rc == 0xFF && --bc);

		if (rc == 0xFE) {				/* A data packet arrived */
			bc = 514 - offset - count;

			/* Skip leading bytes */
			if (offset) {
				do XFER_SPI(0xFF); while (--offset);
			}

			/* Receive a part of the sector */
			do {
				*buff++ = XFER_SPI(0xFF);
			} while (--count);

			/* Skip trailing bytes and CRC */
			do XFER_SPI(0xFF); while (--bc);

			res = RES_OK;
		}
	}

    release_spi();

    GREENLEDOFF();

	return res;
}
