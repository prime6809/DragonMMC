SET CHIP=atmega1284
SET PROG=avrispmkII

avrdude -p %CHIP% -P usb -c %PROG% -tuF -b 115200 < spi_slow.txt
avrdude -p %CHIP% -P usb -c %PROG% -U efuse:w:0xFF:m -U hfuse:w:0xD8:m -U lfuse:w:0xDE:m
avrdude -p %CHIP% -P usb -c %PROG% -tuF -b 115200 < spi_fast.txt
avrdude -p %CHIP% -P usb -c %PROG% -U flash:w:bootloader.hex 