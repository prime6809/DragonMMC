rompak         equ     $c000                                      

bs             equ     $08                      ; backspace                  
cr             equ     $0d                      ; enter key                   
esc            equ     $1b                      ; escape code                       
lf             equ     $0a                      ; line feed                   
formf          equ     $0c                      ; form feed                      
space          equ     $20                      ; space (blank)                           

stkbuf         equ     58                       ; stack buffer room                               
debdel         equ     $45e                     ; debounce delay                              
lbufmx         equ     250                      ; max number of chars in a basic line                                                  
maxlin         equ     $fa                      ; maximum ms byte of line number                                             

dosbuf         equ     $2600                    ; ram load location for the dos command                                                      
;fatcon         equ     6                       ; number of control bytes before fat                                               
fcbcon         equ     25                       ; number of control bytes before fcb                                                
dirlen         equ     32                       ; number of bytes in directory entry                                                
seclen         equ     256                      ; length of sector in bytes                                        
secmax         equ     18                       ; maximum number of sectors per track                                                 
trklen         equ     secmax*seclen            ; length of track in bytes                                                   
trkmax         equ     35                       ; max number of tracks                                  
fatlen         equ     ((trkmax-1)*2)+6         ; file allocation table length                                                     
granmx         equ     (trkmax-1)*2             ; maximum number of granules                                                     
fcblen         equ     seclen+25                ; file control block length                                              
inpfil         equ     $10                      ; input file type                              
outfil         equ     $20                      ; output file type                               
ranfil         equ     $40                      ; random/direct file type                                      

ressgn          equ     $62                     ; sign of result of floating point operation
cinbfl          equ     $0070                   ;pv console in buffer flag: 00=not empty, $ff=empty                                                              


; extended basic scratch pad variables
vcb             equ     $00CB
vcd             equ     $00CD
vcf             equ     $00CF
vd1             equ     $00D1
vd3             equ     $00D3
vd4             equ     $00D4
vd5             equ     $00D5
vd6             equ     $00D6
vd7             equ     $00D7
vd8             equ     $00D8
vd9             equ     $00D9
vda             equ     $00DA

; dskcon variables
dcopc           equ     $00EA                   ;pv dskcon operation code 0-3                                       
dcdrv           equ     $00EB                   ;pv dskcon drive number 0-3                                     
dctrk           equ     $00EC                   ;pv dskcon track number 0-34                                      
dsec            equ     $00ED                   ;pv dskcon sector number 1-18                                      
dcbpt           equ     $00EE                   ;pv dskcon data pointer                                 
dcsta           equ     $00F0                   ;pv dskcon status byte                                

fcbtmp          equ     $00F1                   ; temporary fcb pointer                                  

;start of additional ram variable storage (disk basic only)
dbuf0           equ     $0600                   ; i/o buffer #0                              
dbuf1           equ     $0700                   ; i/o buffer #1                              
fatbl0          equ     $0800                   ; file allocation table - drive 0                                                 
fatbl1          equ     $084A                   ; file allocation table - drive 1                                                 
fatbl2          equ     $0894                   ; file allocation table - drive 2                                                 
fatbl3          equ     $08DE                   ; file allocation table - drive 3                                                 
fcbv1           equ     $0928                   ; file buffer vectors (15 user, 1 system)                                                                 
rnbfad          equ     $0948                   ; start of free random file buffer area                                                  
fcbadr          equ     $094A                   ; start of file control blocks                                         
dnambf          equ     $094C                   ; disk file name buffer                                  
dextbf          equ     $0954                   ; disk file extension name buffer                                            

dfltyp          equ     $0957                   ;dv; disk file type: 0=basic, 1=data, 2=machine                                                          
                                                ; language, 3=text editor source file
dascfl          equ     $0958                   ;dv; ascii flag: 0=crunched or binary, $ff=ascii                                                           
drunfl          equ     $0959                   ; run flag: (if bit 1=1 then run, if bit 0=1, then close                                                                   
                                                ; all files before running)

defdrv          equ     $095A                   ; default drive number                                 
fcbact          equ     $095B                   ; number of fcbs active                                  
dresfl          equ     $095C                   ; reset flag: <>0 will cause a 'new' & shut down all fcbs                                                                    

dusrvc          equ     $095F                   ; disk basic usr command vectors                                            
wfatvl          equ     $097A                   ; write fat value: number of free granules which must be taken                                                                         
                                                ; from the fat to trigger a write fat to disk sequence
dfflen          equ     $097C                   ; direct access file record length                                             
dr0trk          equ     $097E                   ; current track number, drives 0,1,2,3                                                 

dloadfl         equ     $095D                   ; load flag: cause a 'new' following a load error                                                             
dmrgfl          equ     $095E                   ; merge flag: 0=no merge, $ff=merge                                              

v973            equ     $0973                   ; sector number                        
v974            equ     $0974                   ; ram directory image address                                      
v976            equ     $0976                   ; first granule number                               
                                                ; unused file
v977            equ     $0977                   ; sector number                        
v978            equ     $0978                   ; ram directory image address                                      

nmiflg          equ     $0982                   ; nmi flag: 0=don't vector <>0=yector out                                                    
dnmivc          equ     $0983                   ; nmi vector: where to jump following an nmi                                                       

dverfl          equ     $0987                   ; verify flag: 0=off, $ff=on                                       
attctr          equ     $0988                   ; read/write attempt counter: number of times the                                                            


rdytmr          equ     $0985                   ; motor turn off timer                                 
drgram          equ     $0986                   ; ram image of dskreg ($ff40)                                        

dflbuf          equ     $0989                   ; initialized to seclen by diskbas    

l8168           equ     $8168                   ; if not pos, go to exbas sec
l813c           equ     $813c
l8311           equ     $8311                   ; jump to exbas' cload
l8316           equ     $8316                   ; go check for csavem
l836c           equ     $836c                   ; evaluate expression, put ii
l8748           equ     $8748                   ; evaluate a string expression
l8955           equ     $8955                   ; jump to extended basic's irq handler
l8c1b           equ     $8c1b                   ; jump to exeas' dload 

l962e           equ     $962e                   ; jump to exeas' pmode comman
l9650           equ     $9650                   ; branch if comma
l96cb           equ     $96cb                   ; readjust line numbers, etc
l975f           equ     $975f                   ; coming from exbas' get/put?
l9fb5           equ     $9fb5                   ; mult (unsigned)

la0e2           equ     $a0e2                   ; back into ecb reset routine.
bawmst          equ     $a0e8
la176           equ     $a176                   ; get a char from input buffe
la35f           equ     $a35f                   ; set print parameters 
la37c           equ     $a37c                   ; save the print parameters
la3ed           equ     $a3ed                   ; verify that the file type
la3fb           equ     $a3fb                   ; 'file not open' error
la406           equ     $a406                   ; test device number 
la426           equ     $a426                   ; close all files 
la42d           equ     $a42d                   ; close file 
la549           equ     $a549                   ; go do a break check
la5a5           equ     $a5a5                   ; evaluate an expression (device number)
la5ae           equ     $a5ae                   ; strip device number off of input line
la5c7           equ     $a5c7                   ; error if any further charac
la5da           equ     $a5da                   ; branch back to  basic's eof if not disk file
la5e4           equ     $a5e4                   ; link back to basic's eof statement
la61c           equ     $a61c                   ; 'file already open' error
la61f           equ     $a61f                   ; 'device number' error
la7d1           equ     $a7d1                   ; wait a while 
la7e9           equ     $a7e9                   ; turn off the cassette motor
lac7c           equ     $ac7c                   ; go to basic's main loop,
ladc6           equ     $adc6                   ; loop through basic's main interpretation loop
lae15           equ     $ae15                   ; go 'stop' the system
lac37           equ     $ac37                   ; see of enough room in ram
laf9a           equ     $af9a
lafa4           equ     $afa4                   ; branch back to basic's 'let
lafb1           equ     $afb1                   ; branch if string stored 

lb00c           equ     $b00c                   ; return unless coming from basic 'input' statement
lb01e           equ     $b01e                   ; get modified reentry point
lb069           equ     $b069
lb148           equ     $b148                   ; do a 'tm' check
lb166           equ     $b166                   ; coming from the 'let' comma
lb244           equ     $b244                   ; strip prompt string from basic and put it on the string stack
lb262           equ     $b262                   ; basic check for (
lb2ce           equ     $b2ce                   ; jump to basic's secondary ?
lb3e6           equ     $b3e6                   ; evaluate expression
givabf          equ     $b4f4
lb516           equ     $b516                   ; put on temporary string sta
lb657           equ     $b657                   ; purge the string put on the string stack
lbc5f           equ     $bc5f                   ; save number of bytes in ful
lb69b           equ     $b69b                   ; save string descriptor on 
lb70e           equ     $b70e                   ;evaluate numeric expression 
lb95c           equ     $b95c                   ; send a cr to the screen 
lb99f           equ     $b99f                   ; print string to console out
lb9a2           equ     $b9a2                   ; send filename to console ou
lbc14           equ     $bc14                   ; copy a packed fp number fro
lbc33           equ     $bc33                   ; if numeric variable, pack 
lbc35           equ     $bc35                   ; pack fpa0 and store it in s
lbdd9           equ     $bdd9                   ; convert fp number to ascii 

pia0           equ                                 $ff00              peripheral interface adapter one                                                       
pia1           equ                                 $ff20               peripheral interface adapter two                                                        

dskreg         equ      $ff40                   ; disk control register                                  

; bit0 drive select 0
; bit1 drive select 1
; bit2 drive select 2
; bit3 drive motor enable 0 = motors off 1 = motors on
; bit4 write precompensation 0 = no precomp 1 = precomp
; bit5 density flag 0 = single 1 = double
; bit6 drive select 3
; bit7 halt flag 0 = disabled 1 = enabled

; floppy disk controller internal registers

; commands      type    command         code
;               i       restore         $03
;               i       seek            $17
;               i       step            $23
;               i       step in         $43
;               i       step out        $53
;               ii      read sector     $80
;               ii      write sector    $a0
;               iii     read address    $c0
;               iii     read track      $e4
;               iii     write track     $f4
;               iv      force interrupt $d0

; status bit    type i          read address/sector/track write sector/track
; s0            busy            busy                            busy
; s1            index           drq                             drq
; s2            track 0         lost data                       lost data
; s3            crc error       crc error (except track)        crc error (except track)
; s4            seek error      rnf (except track)              rnf (except track)
; s5            head loaded     record type (sector only)       write fault
; s6            write protect   write protect
; s7            not ready       not ready                       not ready

fdcreg         equ     $ff48                    ; status/command register                                    
fdctrack       equ     $ff49                    ; track register                     
fdcsect        equ     $ff4a                    ; sector register                      
fdcdata        equ     $ff4b                    ; data register                    
                                              
