Building Xilinx firmware

Open up project in Xilinx Webpack.

Select DragonMMC.v in the hierachy view.
Expand "Implement design" in the process view, and then double click on 
"generate programming file".
Once the programming file is complete double click on "configure target device".
Click OK to open impact (in a new window).

Within Impact :
Double click on "boundry scan" within "impact flows", this will open up a big 
blank pane with "Right click to add a device to the JTAG chain" in it.
Right click within this pane and from the popup menu select :
	Output file type->XSVF file->Create XSVF file.
	Enter an outout file name e.g. DragonMMC.xsvf
	You will then be prompted for the JED file, select the DragonMMC.jed
	
The JTAG pane will nwo display a graphic representing the xc95144xl, right click 
on this and select program and then 'OK' in the popup, the 'device' then be 
programmed with output going to the XSVF file.

Once this is done copy the DragonMMC.xsvf to your SD card as DGNMMC.SVF 
**NOTE** case and filename! Then program as usual.

