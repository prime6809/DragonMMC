#
# Makefile for DragonMMC Flex drivers / utils.
#
# 2019-03-30, P.Harvey-Smith.
#

AS=lwasm
ASFLAGS=-I../defs -I.. -I../dragondos -r 
ZIP=pkzip
RM=rm
DISTFILES=DISKMMC.DGN FLEXBOOT.DGN readme_flex.txt

all: flex_disk flex_disk_head flexboot

flex_disk: DiskDMMC.asm
		$(AS) $(ASFLAGS) -D Dragon -D Loader -o DiskMMC.bin -llist/DiskMMC.lst DiskDMMC.asm

flex_disk_head: DiskDMMC.asm
		$(AS) $(ASFLAGS) -D Header -D Dragon -D Loader -o DISKMMC.DGN -llist/DiskMMCh.lst DiskDMMC.asm 
		
flexboot:
		$(AS) $(ASFLAGS) -D Dragon -D MINIMUM -D Header -D Patch -o FLEXBOOT.DGN -llist/flexboot.lst flexboot.asm 
		$(AS) $(ASFLAGS) -D Dragon -D MINIMUM -o FLEXBOOT.BIN -llist/flexbootb.lst flexboot.asm 


dist:
		zip flex_util.zip $(DISTFILES)
clean:
		$(RM) -f *.bin
		$(RM) -f *.zip
		$(RM) -f *.o
		$(RM) -f *.cas
		$(RM) -f *.rom
		$(RM) -f *.DGN
		$(RM) -f *.CCO
		$(RM) -f list/*.lst
		
