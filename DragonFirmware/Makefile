#
# Makefile for DragonMMC rom.
#
# 2010-08-09, P.Harvey-Smith.
#

SHELL=/bin/bash
AS=lwasm
#ASFLAGS=-idefs -icmds -iutils -ifunc 
ASFLAGS=-9 -r -I defs -I cmds -I utils -I func 
ZIP=pkzip
RM=rm
ASMFILES=DragonMMC.asm
ASMTIME=asmdatetime
BINDIR=binaries
LISTDIR=list

all: dragonmmc cocommc dragonmmcemu cocommcemu romfile flashf diagflash inverse sertest

dragonmmc: $(ASMFILES)
		$(ASMTIME) -6809 > datetime.asm	
		$(AS) $(ASFLAGS) -I dragondos -D Dragon -D DragonDos -D BuildDos -o $(BINDIR)/DragonMMC.rom -l$(LISTDIR)/DragonMMC.lst $(ASMFILES)

dragonmmcemu: $(ASMFILES)
		$(ASMTIME) -6809 > datetime.asm	
		$(AS) $(ASFLAGS) -I dragondos -D Dragon -D DragonDos -D BuildDos -D EMULATE -o $(BINDIR)/DragonMMC_emulate.rom -l$(LISTDIR)/DragonMMC_emulate.lst $(ASMFILES)

cocommc: $(ASMFILES)
		$(ASMTIME) -6809 > datetime.asm	
		$(AS) $(ASFLAGS) -I rsdos -D Tandy -D BuildDos -o $(BINDIR)/CoCoMMC.rom -l$(LISTDIR)/CoCoMMC.lst $(ASMFILES) 2>&1 |tee  $(LISTDIR)/rsdos.err

cocommcemu: $(ASMFILES)
		$(ASMTIME) -6809 > datetime.asm	
		$(AS) $(ASFLAGS) -I rsdos -D Tandy -D BuildDos -D EMULATE -o $(BINDIR)/CoCoMMC_emulate.rom -l$(LISTDIR)/CoCoMMC_emulate.lst $(ASMFILES)
		
clean:
		$(RM) -f $(BINDIR)/*.bin $(BINDIR)/*.BIN
		$(RM) -f *.zip
		$(RM) -f *.o
		$(RM) -f *.cas
		$(RM) -f $(BINDIR)/*.rom $(BINDIR)/*.ROM
		$(RM) -f $(BINDIR)/*.DGN
		$(RM) -f $(BINDIR)/*.CCO
		$(RM) -f $(LISTDIR)/*.lst $(LISTDIR)/*.err
		
		
romfile: dragonmmc cocommc
		cat $(BINDIR)/DragonMMC.rom $(BINDIR)/CoCoMMC.rom > $(BINDIR)/DGNMMC.ROM
	
#ROM flasher for 28C256 EEPROM
flashe: romfile
		$(AS) $(ASFLAGS) -D Dragon -D MINIMUM=1 -D Header -o $(BINDIR)/FLASHE.DGN -l$(LISTDIR)/flashe.lst flashe.asm
		$(AS) $(ASFLAGS) -D Dragon -D MINIMUM -D $(BINDIR)/FLASHE.BIN -l$(LISTDIR)/flashbe.lst flashe.asm 

#ROM flasher for 39SF010 FLASH rom.
flashf: romfile
		$(AS) $(ASFLAGS) -D Dragon -D MINIMUM -D Header -o $(BINDIR)/FLASH.DGN -l$(LISTDIR)/flashf.lst flashf.asm 
		$(AS) $(ASFLAGS) -D Dragon -D MINIMUM -D EMULATE -o $(BINDIR)/FLASH.BIN -l$(LISTDIR)/flashbf.lst flashf.asm 
        
#		$(AS) flash.asm $(ASFLAGS) -mr -aDragon=1 -oflashd.bin 

diagflash: DiagFlash.asm
		$(AS) $(ASFLAGS) -D Dragon -D MINIMUM -D Header -o $(BINDIR)/DIAGFSH.DGN -l$(LISTDIR)/DiagFlash.lst DiagFlash.asm 

		
inverse:
		$(AS) $(ASFLAGS) -D Dragon -D Header -o $(BINDIR)/INVERSE.DGN -l$(LISTDIR)/inversed.lst inverse.asm
		$(AS) $(ASFLAGS) -D Tandy -D Header -o $(BINDIR)/INVERSE.CCO -l$(LISTDIR)/inversec.lst inverse.asm 
		
sertest:
		$(AS) $(ASFLAGS) -D Dragon -D Header -o $(BINDIR)/SERTEST.DGN -l$(LISTDIR)/sertest.lst sertest.asm 
