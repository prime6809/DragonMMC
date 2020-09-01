SET CHIP=atmega1284
SET PROG=avrispv2
SET PORT=com5

rem avrdude -p %CHIP% -P usb -c %PROG% -tuF -b 115200 < spi_slow.txt
avrdude -B 8.0 -p %CHIP% -P %PORT% -c %PROG% -U efuse:w:0xFF:m -U hfuse:w:0xD8:m -U lfuse:w:0xDE:m
rem avrdude -p %CHIP% -P usb -c %PROG% -tuF -b 115200 < spi_fast.txt
avrdude -B 0.5 -p %CHIP% -P %PORT% -c %PROG% -U flash:w:bootloader.hex 