Flex disk booter for DragonMMC FW 1.2 and above.

The Flex disk booter programs allow the booting of a standard Dragon Flex disk on 
DragonMMC, when running firmware 1.2 or later.

You will need to copy DISKMMC.DGN and FLEXBOOT.DGN onto your SD/MMC card along
with the flex disk image you want to boot e.g. flexdisk.dsk

To use these programs what you will need to do is the following :

1) Use MDISKI 1,"flexdisk.dsk" to mount the disk image you want to boot.
2) MLOAD "DISKMMC.DGN"
3) MLOADA "FLEXBOOT.DGN" (or MLOAD, then EXEC).

Note at present there are no native Flex utilities for manipulating disk images
so any images you wish to use must also be mounted on the other virtual drives
before booting. I hope to be able to later write flex code to allow disks to
at least be mounted / unmounted & listed e.g. an equivilent to the MDISKx 
commands.
