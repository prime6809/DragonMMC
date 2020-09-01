/*
  DGNn file header
*/

#ifndef _DGN_FILE_
#define _DGN_FILE_

typedef struct __attribute__ ((__packed__))
{
    BYTE    Hdr55;      //  magic marker byte #1 always 0x55
    BYTE    HdrType;	// file type see filetypes below
    WORD    HdrLoad;	// Load address of file
    WORD    HdrLen;		// length of file
    WORD    HdrExec;	// entry address of file
    BYTE    HdrIDAA;	// magic marker byte #2
} DGNHead;
#endif