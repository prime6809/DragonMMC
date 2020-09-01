//
// Dragon disk defs.
//

#ifndef __DDOS_DISK__
#define __DDOS_DISK__

#define DDOS_SECTORS_SS		18
#define DDOS_SECTORS_DS		(DDOS_SECTORS_SS*2)
#define DDOS_SECTOR_SIZE	256
#define DDOS_TRKSIZE_SS		(DDOS_SECTOR_SIZE * DDOS_SECTORS_SS)
#define DDOS_TRKSIZE_DS		(DDOS_SECTOR_SIZE * DDOS_SECTORS_DS)


#define DDOS_DEF_TRACKS		40
#define DDOS_DEF_SECTORS	DDSS_SECTORS

typedef struct
{
	BYTE	Tracks;				// Number of tracks on disk
	BYTE	SecPerTrack;		// Number of sectors per track Heads = this / DDOS_SECTORS_SS
	BYTE	NotTracks;			// Tracks ^ 0xFF
	BYTE	NotSecPerTrack;		// SecPerTrack ^ 0xFF
} DISK_GEOM;

// Offset of geometry within BAM sector on dir track
#define DD_GEOM_OFFSET	0xFC	

// Track no of DIR track, and dir backup.
#define DD_DIR_TRACK	20
#define DD_DIR_BACKUP	16

#define LBA_DIR_SS		(DD_DIR_TRACK * DDSS_SECTORS)
#define LBA_DIR_BAK_SS	(DD_DIR_BACKUP * DDSS_SECTORS)
#define LBA_DIR_DS		(DD_DIR_TRACK * DDDS_SECTORS)
#define LBA_DIR_BAK_DS	(DD_DIR_BACKUP * DDDS_SECTORS)

#define GEOM_VALID(Geom)		(((Geom)->Tracks == ((Geom)->NotTracks ^ 0xff)) && ((Geom)->SecPerTrack == ((Geom)->NotSecPerTrack ^ 0xff)))
#define GEOM_VALID_DD(Geom)     ((40 == (Geom)->Tracks) || (80 == (Geom)->Tracks)) && ((18 == (Geom)->SecPerTrack) || (36 == (Geom)->SecPerTrack)) && GEOM_VALID(Geom)
#define SET_GEOM(Geom,Trk,Sec)	{ (Geom)->Tracks = Trk; (Geom)->NotTracks=Trk ^ 0xff; (Geom)->SecPerTrack=Sec; (Geom)->NotSecPerTrack=Sec ^ 0xff; }
#define SET_GEOM_INVALID(Geom)	{ (Geom)->Tracks = 0; (Geom)->NotTracks=0; (Geom)->SecPerTrack=0; (Geom)->NotSecPerTrack=0; }

typedef struct
{
	char	ID[2];			// ID bytes 'd','k'
	WORD	Headersize;		// Header size little endian
	BYTE	Version;		// Version of VDK format
	BYTE	Back;			// Backwards compatibility version
	BYTE	FileSrc;		// Identity of file source
	BYTE	FileVer;		// Version of file source
	BYTE	NoTracks;		// Number of tracks
	BYTE	NoSides;		// Number of sides
	BYTE	Flags;			// Flags
	BYTE	CompLen;		// Compression flags and name length 
} VDK_HEADER;

#define VDK_WP			0x01		
#define VDK_ALOCK		0x02
#define VDK_FLOCK		0x04
#define VDK_DISK_SET	0x08

#define VALID_VDK(vdk)	((((vdk)->ID[0]=='d') || ((vdk)->ID[0]=='D')) && (((vdk)->ID[1]=='k') || ((vdk)->ID[1]=='K')))

#endif