@echo off
REM Generate an XSVF file from a jed file.
REM 2020-05-04 PHS.
REM
REM For DragonMMC we have to then rename this to the correct filename that the
REM AVR, CPLD updater expects because impact won't output an xsvf file directly 
REM to a file with the SVF extension!

REM You may need to change these as needed, expecially the path to Impact.

set IMPACT=d:\Xilinx\14.7\ISE_DS\ISE\bin\nt64\impact.exe 
set BATCHCMD=make-xsvf.cmd
set JEDFILE=DragonMMC.jed
set XSVFFILE=dgnmmc.xsvf
set TARGET=DGNMMC.SVF

REM generate batch command for impact
echo setmode -bscan > %BATCHCMD%
echo setcable -p xsvf -file %XSVFFILE% >> %BATCHCMD%
echo addDevice -p 1 -file %JEDFILE% >> %BATCHCMD%
echo program -e -v -p 1 >> %BATCHCMD%
echo quit >> %BATCHCMD%

REM run impact
%IMPACT% -batch %BATCHCMD%

REM generate output file.
if ERRORLEVEL 1 (
  echo Error : %ERRORLEVEL% running impact
) ELSE (
  if EXIST %XSVFFILE% (
    del %TARGET%
    ren %XSVFFILE% %TARGET%
    echo Now copy %TARGET% to your SD card, set CPLD update jumper and reset AVR.
    echo CPLD should update, both Red and Green LEDS should go out when finished.
  ) ELSE (
    echo ERROR! something went wrong!
  )
)