;
; Message.asm : text messages.
;

__message

	ifne	0
MessPressAnyKey   
		FCB     $00
        FCC     /PRESS ANY KEY     /
        FCB     $0D
        FCB     $00
	endc

SignonMess	
	IFdef	Dragon
	FCC	/DRAGON/
	ELSE
	FCC	/COCO/
	ENDC
	
	FCC	/ MMC V1.35/
	
	IFdef	DEVEL
	FCC	/-devel/
	ENDC
	
	FCB	$0D
        FCB     $00
CompileDate        
	FCC	/COMPILED: /
	use	datetime.asm
        
	FCB     $0D
        FCB     $00

RTCMess	
        FCC     /RTC: /
        FCB     $00
	
;		 01234567890123456789012345678901
;		'1234 12 12 1234 1234 1234 12 12
IFVerMess
	FCC	'AVR FW V' 
	FCB	$00

BLVerMess       
	FCC     'BLD FW V'
	FCB	$00

DeleteMess
	FCC	'ARE YOU SURE (Y/N)'
	FCB	$00
		
MoreMess
	FCC	'MORE:'
	FCB	$00
;		 01234567890123456789012345678901
;		'1234 12 12 1234 1234 1234 12 12
RegNames
	FCC	' PC   A  B  X    Y    U   DP CC'
	FCB	$0D,$00

	ifne	0
CROKMess
        FCB     $0D
OKMess
        FCC     'OK'
	FCB	$0D,$00
        endc
		
		
__message_end
        
        