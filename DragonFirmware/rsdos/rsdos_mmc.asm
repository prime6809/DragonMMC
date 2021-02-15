__rsdos
                use     coco-defs.asm
                
dhitok         equ     $e1                      ; highest 1.1 disk token                                     
cyear          equ     '2'                                      


;wd27           equ     $08                     ; uncomment this if using wd2797 on dragon/rs contoler
;wd27            equ     $00                     ; uncomment this if using standard rs-dos or wd2793

;WDRestore       equ     WDCmdRestore+StepRate30ms
;WDSeek          equ     WDCmdSeek+WDFlagVerify+StepRate30ms
;WDStepIn        equ     WDCmdStepIn+WDFlagUpdate+StepRate30ms
;WDStepOut       equ     WDCmdStepOut+WDFlagUpdate+StepRate30ms
;WDReadSector    equ     WDCmdReadSec+wd27       ; read sector command
;WDWriteSector   equ     WDCmdWriteSec+wd27      ; write sector command
;WDWriteTrack    equ     WDCmdWriteTrack

;
;
;
; file allocation table format
;
;
; the file allocation table (fat) contains the status of the granules on a diskette.
; the fat contains 6 control bytes followed by 68 data bytes (one per granule). only the
; first two of the six control bytes are used. a value of $ff is saved in unallocated
; granules. if bits 6 & 7 of the data byte are set, the granule is the last granule
; in a file and bits 0-5 are the number of used sectors in that granule. if bits 6 & 7
; are not set, the data byte contains the number of the next granule in the file.

; offsets to fat control bytes
fat0           equ     0                        ; active file counter : disk to ram fat image disable                                                              
fat1           equ     1                        ; valid data flag: 0=disk data valid, <> 0 = new fat                                                             
;                                         data - disk data invalid
;                2 to 5                   not used
fatcon         equ     6                        ; offset to start of fat data (68 bytes)                                                   
;
;;
;;;; directory entry format
;;
;
; the directory is used to keep track of how many files are stored on a diskette
; and where the file is stored on the disk. the first granule used by the file will
; allow the fat to track down all of the granules used by the file. if the first
; byte of the directory entry is zero, the file has been killed;
; if the first byte is $ff then the directory entry has never been used.
;
;                byte                                description

dirnam         equ     0                        ; file name                      
dirext         equ     8                        ; file extension                           
dirtyp         equ     11                       ; file type                       
dirasc         equ     12                       ; ascii flag                        
dirgrn         equ     13                       ; first granule in file                                   
dirlst         equ     14                       ; number of bytes in last sector                                            
;                16 to 31                            unused
;
;;
;;;; file control block format
;;
;
; the file structure of color trs dos is controlled by a file control block (fcb)
; the fcb contains 25 control bytes and a sector long (256 bytes) data buffer.
; the control bytes control the orderly flow of data from the computer's ram to
; the diskette and vice versa. the open command initializes the fcb; the input,
; output, write, print, get and put commands transfer data through the fcb and
; the close command turns off the fcb.

; tables of offsets to fcb control bytes

;;;;; random file
;                byte                                description
fcbtyp         equ     0                        ; file type: $40=random/direct, 0=closed                                                   
fcbdrv         equ     1                        ; drive number                         
fcbfgr         equ     2                        ; first granule in file                                  
fcbcgr         equ     3                        ; current granule being used                                       
fcbsec         equ     4                        ; current sector being used (1-9)                                            
;                5                                   unused
fcbpos         equ     6                        ; current print position - always zero in random files                                                                 
fcbrec         equ     7                        ; current record number                                  
fcbrln         equ     9                        ; random file record length                                      
fcbbuf         equ     11                       ; pointer to start of this file's random access buffer                                                                  
fcbsof         equ     13                       ; sector offset to current position in record                                                         
fcbflg         equ     15                       ; get/put flag: 0=put, 1=put                                        
;                16,17                               not used
fcbdir         equ     18                       ; directory entry number (0-71)                                           
fcblst         equ     19                       ; number of bytes in last sector of file                                                    

fcbget         equ     21                       ; 'get' record counter: how many characters have been                                                                 
;                                                    pulled out of the current record
fcbput         equ     23                       ; 'put' record counter: pointer to where in the record the next                                                                           
;                                                    byte will be 'put'
;fcbcon         equ     25                          ; offset to start of fcb data buffer (256 bytes)                                                            

;;;;; sequential file (some of these are commented out as they are defined above)
;                byte                                description
;fcbtyp         equ     0                           ; file type: $10=input, $20=output, 0=closed                                                       
;fcbdrv         equ     1                           ; drive number                         
;fcbfgr         equ     2                           ; first granule in file                                  
;fcbcgr         equ     3                           ; current granule being used                                       
;fcbsec         equ     4                           ; current sector being used (1-9)                                            
fcbcpt         equ     5                        ; input file: character pointer - points to next character in                                                                        
;                                                    file to be processed.
;                                                    output file: full sector flag - if it is 1 when the file is
;                                                    closed it means 256 bytes of the last sector have been used.
;fcbpos         equ     6                           ; current print position                                   
;fcbrec         equ     7                           ; current record number: how many whole sectors have been                                                                    
;                                                    input or output to a file.
;                9 to 15                             unused
fcbcfl         equ     16                       ; cache flag: 00=cache empty, $ff=cache full                                                        
fcbcdt         equ     17                       ; cache data byte                             
;fcbdir         equ     18                          ; directory entry number (0-71)                                           
;fcblst         equ     19                          ; number of bytes in last sector of file                                                    
;                 21,22                            unused
fcbdfl         equ     23                       ; input file only: data left flag: 0=data left, $ff=no data (empty)                                                                               
fcblft         equ     24                       ; number of characters left in buffer (input file)                                                              
;                                                  number of chars stored in buffer (output file)
;fcbcon         equ     25                          ; offset to fcb data buffer (256 bytes)                                                   

;               org     $c000                                

;dosbas         fcc     'DK'                                      
;lc002          bra     lc00c                                     

dcnvec         fdb     dskcon                   ; dskcon pointer                                
dskvar         fdb     dcopc                    ; address of dskcon variables                                            
dsinit         fdb     dosini                   ; disk initialization vector                                            
dosvec         fdb     doscom                   ; dos command vector                                    

;
; Init for MMC
;
MMCInit	LBRA	RealMMCInit
;
; Init Dos
; 
DosInit
                ldx     #dbuf0                   ; point x to start of disk ram                                             

lc00f          clr     ,x+                      ; clear a byte                          
               cmpx    #dflbuf                  ; end of disk's ram?                                
               bne     lc00f                    ; no - keep clearing                             

;               ldx     #BeginStub               ; point x to rom image of command interpretation table                                                                
;               ldu     #BasStub2                ; point u to ram address of same                                              
;               ldb     #10                      ; 10 bytes per table                           
;               jsr     UtilCopyBXtoU            ; move (b) bytes from (x) to (u)                                         

;               ldd     #BasSNError              ; syntax error address                                
;               std     $03,u                    ; set jump table addresses of the user command                                                       
;               std     $08,u                    ; interpretation table to point to syntax error                                                        
;               clr     ,u                       ; clear byte 0 of user table (doesn't exist flag)                                                       
;               clr     $05,u                    ; set number of secondary user tokens to zero                                                      

; Modify Extended basic stub
               ldd     #dxcvec                  ; save new                     
               std     BasAddrDskCmdDisp        ; pointers to exbas                                
               ldd     #dxivec                  ; command and secondary                                  
               std     BasAddrDskFuncDisp       ; command interpretation routines                                              

;;;; move the new ram vectors from rom to ram
               ldx     #RamHookTable            ; Point at ram hooks
               ldu     #VectBase                ; point u to 1st ram vector                                     
lc03b          lda     #$7e                     ; op code of jmp instruction                                         
               sta     VectAccessScreen         ; set 1st byte of 'get'/'put' ram vector to 'jmp'                                                           
               sta     ,u+                      ; set 1st byte of ram vector to 'jmp'                                            
               ldd     ,x++                     ; get ram vector from rom                                 
               std     ,u++                     ; store it in ram                         
               cmpx    #DosSignonMess           ; compare to end of rom values                                         
               bne     lc03b                    ; branch if not all vectors moved                                          

               ldx     #DosHookGetPutCLS        ; get rom value of 'get'/'put' ram vector                                                    
               stx     VectAccessScreen+1       ; save it in ram                            
               ldx     #DosHookInterpret        ; get disk command interpretation loop ram vector                                                            
               stx     VectGetNextCmd+1         ; save in ram vector table                                      
    
;;;; initialize disk basic's usr vectors
               ldx     #dusrvc                  ; point x to start of disk basic usr vectors                                                       
               stx     BasUSRTableAddr          ; save start address in BasUSRTableAddr                                        
               ldu     #BasFCError              ; point u to address of 'function call' error                                                       
               ldb     #$0a                     ; 10 user vectors to initialize                                       
lc061          stu     ,x++                     ; set usr vector to 'fc' error                                           
               decb                             ; decrement usr vector counter                                   
               bne     lc061                    ; branch in not done with all 10 vectors                                                 

;               ldx     #dnmisv                  ; get address of nmi servicing routine                                                 
;               stx     SecVecNMI+1              ; save it in nmi vector                                   
;               lda     #$7e                     ; op code of jmp                        
;               sta     SecVecNMI                ; make the nmi vector a jmp                                     
               
;               ldx     #dirqsv                  ; get address of disk basic irq servicing routine                                                            
;               stx     SecVecIRQ+1              ; save it in SecVecIRQ                               
               
               lda     #$13                     ; = initialize write fat                                
               sta     wfatvl                   ; = to disk trigger value                                   
               clr     fatbl0                   ;           
               clr     fatbl1                   ; initialize the active file counter of                                                 
               clr     fatbl2                   ; each fat to zero. this will cause the fats                                                      
               clr     fatbl3                   ; to think there are no active files                                              
               ldx     #dflbuf                  ; = get the starting address of the                                              
               stx     rnbfad                   ; = random file buffer free area and dave it as the                                                             
                                                ; = start address of free ram for random file buffers
               leax    $0100,x                  ; save 256 bytes for random file buffers initially                                                              
               stx     fcbadr                   ; save start address of fcbs                                      
               leax    $01,x                    ; add one and save the starting                                         
               stx     fcbv1                    ; address of fcb1                          
               clr     fcbtyp,x                 ; clear the first byte of fcb 1 (close fcb)                                                       
               leax    fcblen,x                 ; point x to fcb 2                               
               stx     fcbv1+2                  ; save its starting address in fcb vector table                                                          
               clr     fcbtyp,x                 ; clear the first byte of fcb 2 (close fcb)                                                       
               leax    fcblen,x                 ; point x to system fcb - this fcb will only                                                         
                                                ; be used to copy, load, save, merge, etc
               stx     fcbv1+4                  ; save its address in the fcb vector table                                                     
               clr     fcbtyp,x                 ; clear the first byte of system fcb (close fcb)                                                            
               lda     #$02                     ; set the number of active reserved                                           
               sta     fcbact                   ; file buffers to 2 (1,2)                                   
               leax    fcblen,x                 ; point x to one past the end of system fcb                                                        
               tfr     x,d                      ; save the address in accd                                 
               tstb                             ; on an even 256 byte boundary?                                    
               beq     lc0bd                    ; yes              

               inca                             ; no - add 256 to address                              
lc0bd          bita    #$01                     ; check to see if accd is on an even                                                  
               beq     lc0c2                    ; 512 byte (one graphic page) boundary - add                                                     

               inca                             ; 256 (inca) to it if not                              
lc0c2          tfr     a,b                      ; copy acca to accb                               
               addb    #$18                     ; save enough room for 4 graphics pages (pclear 4)                                                           
               stb     BasStartProg             ; save new start of basic address                                           
               jsr     BasLocateScreen          ; initialize exbas variables & do a new                                                

               lda     GrDisplayStartAddr       ; get the start of current graphics page                                                  
               adda    #$06                     ; add 1.5k (6 x 256 = one graphics page)                                                 
               sta     GrLastDisplayAddr        ; save new end of graphics page                                         
               jsr     [dsinit]                 ; initialize swi2,3 jump addresses                                              

;               bsr     InitFDC                  ; go initialize the floppy disk controller                                                   

               andcc   #$af                     ; turn on irq and firq                                
               ldx     #DosSignonMess-1         ; point x to disk basic copyright message                                                     
               jsr     TextOutString            ; print copyright message to screen                                               

;               ldx     #dkwmst                  ; get disk basic warm start address                                              
;               stx     IndVecReset              ; save it in reset vector                                   
;               jmp     la0e2                    ; jump back to basic                             

                jmp	>MMC_InitDone		; do post init stuff


DOSResetVector
               nop                              ; warm start indicator                                
               bsr     InitFDC                  ; initialize the floppy disk controller                                                

               jsr     >CloseFilesInit2         ; close files and do more initialization                                                 
               rts
               
;               jmp     WarmStart                ; jump to exbas' warm start                                     

InitFDC        clr     nmiflg                   ; reset nmi flag                               
               clr     rdytmr                   ; reset drive not ready timer                                       
               clr     drgram                   ; reset ram image of dskreg (motors off)                                                  

;               clr     >dskreg                  ; reset disk control register                                       
;               lda     #WDCmdForceInt           ; force interrupt command of 1793                                         
;               sta     >fdcreg                  ; send it to 1793                           
;               exg     a,a                      ; delay              
;               exg     a,a                      ; delay some more                        
;               lda     >fdcreg                  ; get 1793 status (clear register)                                            
               rts                                     

; disk basic command interp tables
;BeginStub          
;               fcb     NoDOSCommands            ; 20 disk basic 1.1 commands                                       
;               fdb     DosCmdNames              ; disk basic's command dictionary                                          
;               fdb     DosCmdDispatch           ; command jump table                             
;               fcb     NoDOSFunctions           ; 6 disk basic secondary functions                                        
;               fdb     DosFuncNames             ; secondary function table                                   
;               fdb     DosFuncDispatch          ; secondary function jump table                                        

; ram hooks for disk basic
RamHookTable
               fdb     DosHookOpenDev
               fdb     DosHookCheckIONum
               fdb     DosHookRetDevParam                                     
               fdb     DosHookCharOut
               fdb     DosHookCharIn
               fdb     DosHookCheckOpenIn                                
               fdb     DosHookCheckOpenOut
               fdb     DosCloseAllFiles
               fdb     DosHookCloseFile                                
               fdb     CoCoVect179
               fdb     DosHookInput
               fdb     DosHookBreak                                
               fdb     DosHookReadInputLine
               fdb     DosHookTermInputLine
               fdb     DosHookEOF                                
               fdb     DosHookEvaluate
               fdb     DosHookReadInputLine                                
               fdb     DosHookSysError
               fdb     DosHookRun                                

; disk basic copyright message
DosSignonMess  
               fcc     'DISK EXTENDED COLOR BASIC 1.1'                                         
               fcb     cr                                
               fcb      0
               
               ifeq 1
               fcc     'COPYRIGHT (C) 198'                        
               fcb     cyear                                
               fcc     ' BY TANDY'                
               fcb     cr                                
               fcc     'UNDER LICENSE FROM MICROSOFT'                                   
               fcb     cr,cr,0                                
               endc 
               
;disk basic command interpretation handler
DosCmdDispatch cmpa    #dhitok                  ;compare to highest disk basic token                                                     
               bhi     lc244                    ;and branch if higher                              

               ldx     #CommandDispatchTable    ; point x to disk basic command jump table                                                    
               suba    #$ce                     ; subtract out lowest disk basic command token                                                       
               jmp     BasDoDispatch            ; jump to basic's command handler                                          


lc244          cmpa    #dhitok                  ; compare to highest disk basic token                                                      
               lbls    BasSNError               ; 'syntax' error if < disk basic command token                                                        

               jmp     [BasStub3+3]             ; process a user command token                                             


;disk basic secondary command interpretation handler
DosFuncDispatch
               cmpb    #($a7-$80)*2             ;compare modified secondary token to                                                                                    
               bls     lc256                    ;highest disk basic token & branch if higher                                                     

               jmp     [BasStub3+8]             ; jump to user secondary command handler                                                        


lc256          subb    #($a2-$80)*2             ;subtract out the smallest secondary                                                                                    
               pshs    b                        ;disk token & save modified token on the stack                                                    
               jsr     lb262                    ; syntax check for '(' and evaluate expression                                                       


               puls    b                        ; restore modified token                              
               ldx     #FunctionDipatchTable                   ; point x to secondary command jump table                                                   
               jmp     lb2ce                    ; jump to basic's secondary command handler                                                    


; error driver ram vector
DosHookSysError
	       puls    y                        ; put the return address into y                                           
               jsr     BasResetStack            ; reset the cont flag, etc                                   

               jsr     >CloseFilesInit2                   ; initialize some disk variables and close files                                                         


               pshs    y,b                      ; put return address and error number on the stack                                                          
               jsr     >DosCloseAllFiles                   ; close all files                          

               puls    b                        ; get the error number back                                 

               cmpb    #2*27                    ; compare to the lowest disk error number                                                                                  
               lbcs    CoCoVect191              ; branch to exbas error handler if not disk error number                                                                   

               leas    $02,s                    ; purge return address off the stack                                              
               jsr     la7e9                    ; turn off the cassette motor                                      

               jsr     SndDisable               ; disable the analog multiplexer                                         


               clr     TextDevN                 ; set device number to the screen                                           
               jsr     lb95c                    ; send a cr to the screen                                  

               jsr     TextOutQuestion          ; send a '?' to the screen                                   

               ldx     #DosErrorCodeTable-(2*27)            ; point x to disk basic's error table                                                                           
               jmp     SysErr2                  ; jump to basic's error handler                                        


; disk basic error messages
DosErrorCodeTable          
                fcc     'BR'                    ; 27 bad record                            
                fcc     'DF'                    ; 28 disk full                      
                fcc     'OB'                    ; 29 out of buffer space                                
                fcc     'WP'                    ; 30 write protected                            
                fcc     'FN'                    ; 31 bad file name                          
                fcc     'FS'                    ; 32 bad file structure                               
                fcc     'AE'                    ; 33 file already exists                                
                fcc     'FO'                    ; 34 field overflow                           
                fcc     'SE'                    ; 35 set to non-fielded string                                      
                fcc     'VF'                    ; 36 verification error                               
                fcc     'ER'                    ; 37 write or input past end of record                                              
                        
; disk file extensions
basext          fcc     'BAS'                   ; basic file extension                                     
defext          fcc     '   '                   ; ' blank (default) file extension                                             
datext          fcc     'DAT'                   ; data file extension                                    
binext          fcc     'BIN'                   ; binary file extension                                      
                        
; cls ram vector
DosHookGetPutCLS         pshs    x,cc                     ; save x reg and status                                      
               ldx     $03,s                    ; load x with calling address                                      
               cmpx    #l975f                   ; coming from exbas' get/put?                                        
               bne     lc2bf                    ; no             

               cmpa    #'#                      ; number sign (get#, put#)?                                    
               beq     lc2c1                    ; branch if get or put to random file                                              

lc2bf          puls    cc,x,pc                  ; restore x reg, status and return                                                   

; get/put to a direct/random file
lc2c1          leas    $05,s                    ; purge return address and registers off of the stack                                                                    
               jsr     >lc82e                   ; evaluate device number & set fcb pointer                                                   

               stx     fcbtmp                   ; save fcb pointer                            
               clr     fcbget,x                 ; reset the get                           
               clr     fcbget+1,x               ; data pointer                            
               clr     fcbput,x                 ; = reset the put                             
               clr     fcbput+1,x               ; = data pointer                              
               clr     fcbpos,x                 ; reset print position counter                                          
               lda     fcbdrv,x                 ;get the fcb drive number and                                         
               sta     dcdrv                    ;save it in dskcon variable                                    
               jsr     BasChrGetCurr            ; get current input character from basic                                                  

               beq     lc2ea                    ; branch if end of line                                

               jsr     VarCKComma               ; syntax check for comma                                    

               jsr     VarGet16Bit              ; evaluate expression - return in (x)                                              

               tfr     x,d                      ; save record number in accd                                   
lc2e6          ldx     fcbtmp                   ; point x to fcb                               
               std     fcbrec,x                 ; save record number in fcb                                       
lc2ea          ldd     fcbrec,x                 ; get record number                                    
               beq     lc30b                    ; 'bad record' error if record number = 0                                                  

               jsr     >lc685                   ; increment record number                                  

               ldd     fcbrln,x                 ; get random file record length and random file                                                           
               ldx     fcbbuf,x                 ; buffer pointer and save them on the stack -                                                         
               pshs    x,b,a                    ; these are the initial values of a temporary                                                       
                                                ; record length counter and random buffer
                                                ; pointer which are maintained on the stack
               leax    $-2,u                    ; point x to (record number -1)                                         
               jsr     l9fb5                    ; mult (unsigned) record length x (record number -1)                                                             

               pshs    u,y                      ; save product on the stack                                   
               lda     ,s+                      ; check ms byte of product                                 
               bne     lc30b                    ; 'br' error if not zero (record number too big)                                                         

               puls    x                        ; pull the bottom 3 product bytes off the stack;                                                      
               puls    b                        ; top two in x, bottom in accb; accb points to                                                    
                                                ; the first byte of the sector used by this record,
                                                ; (x) contains the sector offset (in which sector
                                                ; from the start the byte is located)
lc306          cmpx    #(trkmax-1)*secmax       ; 612 sectors max in a random file                                                       
               blo     lc310                    ; branch if record length o.k.                                       

lc30b          ldb     #2*27                    ; 'bad record' error                                                            
               jmp     SysErr                   ; jump to error handler                                

lc310          ldu     fcbtmp                   ; point u to fcb                               
               cmpx    fcbsof,u                 ; compare saved sector offset to the current sector offset                                                                       
               lbeq    lc3cf                    ; being processed - do not process a new sector if they are equal                                                                           

               pshs    x,b                      ; save byte and sector offset to record start on stack                                                              
               lda     fcbflg,u                 ; check fcb get/put flag and                                        
               beq     lc324                    ; branch if it was a get                                 

               clr     fcbflg,u                 ; force get/put to 'put'                                    
               ldb     #$03                     ; dskcon write op code                              
               bsr     lc357                    ; go write a sector - save 'put' data on disk                                                      


;; convert the sector offset to a granule and sector number
lc324          ldd     $01,s                    ; get the number of sectors to the start of                                                         
               jsr     >lc784                   ; this record number and convert them to a granule offset                                                                  

               pshs    b                        ; save granule offset on the stack                                        
               jsr     >lc779                   ; multiply granule number x 9 - convert to number of sectors                                                                     

               negb                             ; negate ls byte of granule offset and add the                                                   
               addb    $03,s                    ; ls byte of sector offset - accb = sector                                                    
                                                ; number (0-8) corresponding to the sector number within a
                                                ; granule of the last sector of the sector offset
               incb                             ; = add one - sectors saved in the fcb; start                                                  
               stb     fcbsec,u                 ; = at 1 not 0 - save it in the fcb                                               
               ldb     fcbfgr,u                 ; get first granule in file                                       
               jsr     >lc755                   ; point x to fat                         

               leau    fatcon,x                 ; point u to fat data                                  
               lda     ,s                       ; get number of granules offset to record                                               
               inca                             ; add one (compensate for deca below)                                          
lc33e          leax    ,u                       ; point x to fat data                                 
               abx                              ; point x to correct granule                                
               deca                             ; decrement granule counter                                
               beq     lc37b                    ; branch if correct granule found                                          

               stb     ,s                       ; save granule address on stack                                     
               ldb     ,x                       ; get next granule in file                                
               cmpb    #$c0                     ; last granule in file?                                
               blo     lc33e                    ; no - keep looking                            


; the granule being searched for is not presently defined in this random file
               ldb     ,s                       ; get offset to last granule                                  
               tst     vd8                      ; check get/put flag                           
               bne     lc366                    ; and branch if put                            

lc352          ldb     #2*23                    ;'input past end of file' error                                                                        
               jmp     SysErr                   ; jump to error handler                                

lc357          leax    fcbcon,u                 ; point x to fcb data buffer                                              

; read/write a sector. enter with op code in accb, buffer ptr in x
lc35a          stb     dcopc                    ; save dskcon operation code variable                                                   
               stx     dcbpt                    ; save dskcon load buffer variable                                           
               leax    ,u                       ; point x to fcb                       
               jsr     >lc763                   ; convert fcb track and sector to dskcon variables                                                           

               jmp     >DoDiskReadWrite                   ; read/write a track or sector                                       


; 'put' data into a granule not presently included in this file
lc366          pshs    x,a                      ; save granule counter and pointer to last used granule                                                                    
               jsr     >lc7bf                   ; find first free granule in fat                                         

               tfr     a,b                      ; save free granule number in accb                                         
               puls    a,u                      ; pull last granule pointer and counter off of stack                                                            
               stb     ,u                       ; save newly found granule number in address of last granule                                                                  
               deca                             ; decrement granule counter                                
               bne     lc366                    ; get another granule if not done                                          

               pshs    x,b                      ; save pointer to last granule and offset                                                 
               jsr     >lc71e                   ; write fat to disk                            

               puls    b,x                      ; restore pointer and offset                                    

; when correct granule is found, find the right sector
lc37b          leas    $01,s                    ; remove gran number from stack                                              
               ldu     fcbtmp                   ; point u to fcb                          
               stb     fcbcgr,u                 ; save current granule in fcb                                         
               lda     #$ff                     ;set fcbsof,u to illegal sector offset which will                                                         
               sta     fcbsof,u                 ;force new sector data to be read in                                                
               lda     ,x                       ; get current granule                           
               cmpa    #$c0                     ; is it the last granule?                                  
               blo     lc3b2                    ; no             

               anda    #$3f                     ; mask off last granule flag bits                                          
               cmpa    fcbsec,u                 ; compare calculated sector to current sector in fcb                                                                 
               bhs     lc3b2                    ; and branch if calculated sector is > last sector in file                                                                   

               lda     vd8                      ; = check get/put flag: if 'get' then 'input                                                   
               beq     lc352                    ; = past end of file' error                                    

               lda     fcbsec,u                 ; get current sector number from fcb,                                                 
               ora     #$c0                     ; or in the last granule flag bits                                          
               sta     ,x                       ; and save in fat                       
               jsr     >lc5a9                   ; write fat to disk if necessary                                         

               ldx     fcbrln,u                 ; get record length and check to                                            
               cmpx    #seclen                  ; see if it is seclen (exactly one sector)                                                      
               bne     lc3ad                    ; branch if it is not exactly one sector                                                 

               cmpx    fcblst,u                 ; =branch if the number of bytes in the last sector                                                                

               beq     lc3b2                    ; =is set to one sector (seclen)                                         

               lda     #$81                     ;set the presaved flag (bit15) and force                                                
lc3ac          fcb      Skip1                    ;brn
lc3ad          clra                             ; set the number of bytes in last sector to zero                                                          
               clrb                             ; clear ls byte of accd                            
               std     fcblst,u                 ; save the number of bytes in last sector                                                     

lc3b2          ldb     #$02                     ; dskcon read op code                                  
               ldx     fcbrln,u                 ; get record length and compare                                           
               cmpx    #seclen                  ; it to seclen - exactly one sector                                               
               bne     lc3c8                    ; branch if not exactly one sector long                                                

               leas    $07,s                    ; clean up stack                          
               ldx     fcbbuf,u                 ; point x to start of random file buffer                                                    
               lda     vd8                      ; check get/put flag and                               
               beq     lc3c5                    ; branch if get                        

               ldb     #$03                     ; dskcon write op code                              
lc3c5          jmp     >lc35a                   ; read/write a sector                                   

lc3c8          jsr     >lc357                   ; read a sector into fcb data buffer                                                  

               puls    b,x                      ; get back the byte offset to record: x = number of                                                           
                                                ; sectors; accb = byte pointer in sector
               stx     fcbsof,u                 ; save sector offset in fcb                                       
lc3cf          pshs    b                        ; save byte offset on stack                                      
               jsr     >lc755                   ; point x to file allocation table                                           

               leax    fatcon,x                 ; move x to fat data                                 
               ldb     fcbcgr,u                 ; get current granule number                                        
               abx                              ; point x to proper granule in fat                                      
               lda     ,x                       ; get current granule and check to                                        
               cmpa    #$c0                     ; see if it is last granule                                    
               blo     lc40a                    ; branch if this granule is < last granule                                                   

               anda    #$3f                     ; mask off last granule flag bits                                          
               cmpa    fcbsec,u                 ; compare last sector used in granule to                                                     
               bne     lc40a                    ; calculated sector; branch if not equal                                                 

               ldd     fcblst,u                 ; get number of bytes in last sector                                                

               anda    #$7f                     ; mask off presaved flag (bit 15)                                          
               pshs    b,a                      ; save number of bytes in last sector on stack                                                      
               clra                             ; load accb with the byte offset to current                                                
               ldb     $02,s                    ; record and add the remaining record length                                                     
               addd    $03,s                    ; to it - accd = end of record offset                                               
               cmpd    ,s++                     ; =compare the end of record offset to the number of                                                             
               bls     lc40a                    ; =bytes used in the last sector                                         

               tst     vd8                      ; check get/put flag and branch if 'get'                                               
               lbeq    lc352                    ; to 'input past end of file' error                                             


; if last used sector, calculate how many bytes are used
; if data is being 'put' pasth the current end of file
               cmpd    #seclen                  ; compare to one sector's length                                            
               bls     lc405                    ; branch if remainder of record length will fit in this sector                                                                       

               ldd     #seclen                  ; force number of bytes = one sector length                                                      
lc405          ora     #$80                     ; set pre-saved flag bit - all put records are                                                           
                                                ; written to disk before leaving 'put'
               std     fcblst,u                 ; save number of bytes used in last sector                                                      

lc40a          puls    b                        ; pull byte offset off of the stack                                              
               leax    fcbcon,u                 ; point x to fcb data buffer                                         
               abx                              ; move x to start of record                               
               ldu     $02,s                    ; point u to current position in random file buffer                                                            
               pshs    b                        ; save byte offset on stack                                 
               lda     #-1                      ; convert accd into a negative 2 byte number                                                   
                                                ; representing the remaining unused bytes in the sector
               addd    $01,s                    ; add temporary record length counter (subtract                                                         
                                                ; remaining bytes from temporary record length)
               bhs     lc421                    ; branch if there are enough unused bytes to finish the record                                                                       

               std     $01,s                    ; save new temporary record length counter                                                   
               puls    b                        ; restore byte counter                            
               negb                             ; negate it - accb = the number of bytes                                             
                                                ; available to a record in this sector
               bra     lc429                    ; move the data                        

; branch here if remaining record length will fit in
; what's left of the currently selected sector
lc421          ldb     $02,s                    ; get remaining record length                                           
               clr     $01,s                    ; clear the temporary record length                                            
               clr     $02,s                    ; counter on the stack                               
               leas    $01,s                    ; purge byte offset from stack                                        
lc429          lda     vd8                      ; check get/put flag and                                    
               beq     lc42f                    ; branch if get                        

               exg     x,u                      ; swap source and destination pointers                                             
lc42f          jsr     UtilCopyBXtoU            ; transfer data from source to destination buffers                                                                

               stu     $02,s                    ; save new temp record pointer on the stack (get)                                                          

; move data from fcb data buffer to the random file buffer if 'get'
; or from random file buffer to fcb data buffer if 'put'
               ldu     fcbtmp                   ; point u to fcb                          
               lda     vd8                      ; check get/put flag and                               
               beq     lc43e                    ; branch if get                        

               sta     fcbflg,u                 ; save 'put' flag in the fcb                                        
               stx     $02,s                    ; save new temporary record pointer on stack (put)                                                           
lc43e          ldx     fcbsof,u                 ; get sector offset counter and                                                
               leax    $01,x                    ; add one to it                         
               clrb                             ; set byte offset = 0                          
               ldu     ,s                       ; check the length of the temporary record length                                                       
               lbne    lc306                    ; counter and keep moving data if <> 0                                                

               puls    a,b,x,pc                 ; pull temporary record length and                                               
                                                ; buffer address off stack and return

; open ram hook
DosHookOpenDev 
               leas    $02,s                    ; pull return address off of the stack                                                     
               jsr     VarGetStr                ; evaluate an expression                                 

               jsr     BasGetStrFirst           ;get mode(i,o,r) - first byte of string expression                                                           

               pshs    b                        ;and save it on stack                           
               jsr     BasGetDevNo              ; get device number                            

               tstb                             ; set flags                
               lble    CmdOpenEntry             ; branch if not a disk file                                     
               puls    a                        ; get mode                
               pshs    b,a                      ; save mode and device number (file number)                                                   
               clr     TextDevN                 ; set device number to screen                                       
               jsr     VarCKComma               ; syntax check for comma                                    

               ldx     #datext                  ; point to 'dat' for extension                                         
               jsr     >lc938                   ; get filename from basic                                  

               ldd     #$01ff                   ; default disk file type and ascii flag                                                 
               std     dfltyp                   ; save default values: data, ascii                                            
               ldx     #seclen                  ; default record length - 1 page                                           
               jsr     BasChrGetCurr            ; get char from basic                               

               beq     lc481                    ; branch if end of line                                

               jsr     VarCKComma               ; syntax check for comma                                    

               jsr     lb3e6                    ; evaluate expression                              

               ldx     BasVarFPAcc1+3           ; get evaluated expression                                    
lc481          stx     dfflen                   ; record length                              
               lbeq    BasFCError               ; if = 0, then 'illegal function call'                                                

               jsr     la5c7                    ; error if any further characters on line                                                  

               puls    a,b                      ; get mode and file number                                  

; open disk file for read or write
lc48d          pshs    a                        ; save mode on stack                               
               jsr     >lc749                   ; point x to fcb for this file                                       

               lbne    la61c                    ; 'file already open' error if file open                                                  

               stx     fcbtmp                   ; save file buffer pointer                                    
               jsr     >GetFAT                   ; make sure file alloc table is valid                                              

               jsr     >lc68c                   ; scan directory for 'filename.ext'                                            

               puls    b                        ; get mode                
               lda     #inpfil                  ; input type file                            
               pshs    a                        ; save file type on stack                               
               cmpb    #'I                      ; input mode?                      
               bne     lc4c7                    ; branch if not                        


; open a sequential file for input
               jsr     >lc6e5                   ; check to see if directory match is found                                                   

               jsr     >lc807                   ; check to see if file already open                                            

               ldx     v974                     ; get ram directory buffer                                  
               ldd     dirtyp,x                 ; get file type and ascii flag                                          
               std     dfltyp                   ; save in ram image                             
               bsr      lc52d

               jsr     >lc627                   ; go fill data buffer                              

lc4bb          jsr     >lc755                   ; point x to proper file allocation table                                                       

               inc     fat0,x                   ; add one to fat active file counter                                              
               ldx     fcbtmp                   ; get file buffer pointer                                   
               puls    a                        ; get file type                     
               sta     fcbtyp,x                 ; save it in fcb                            
               rts                                     

lc4c7          asl     ,s                       ; set file type to output                                    
               cmpb    #'O                      ; file mode = output?                              
               bne     lc4e8                    ; branch if not                        


; open a sequential file for output
               tst     v973                     ; does file exist on directory?                                       
               beq     lc4e1                    ; branch if not                        

               jsr     >lc6cf                   ; kill the old file                            

               lda     v973                     ; get directory sector number of old file and                                                     
               sta     v977                     ; save it as first free directory entry                                               
               ldx     v974                     ; =get ram directory image of old file and                                                  
               stx     v978                     ; =save it as first free directory entry                                                

lc4e1          jsr     >lc567                   ; set up new directory entry on disk                                                  

               bsr     lc538                    ; initialize file buffer                                 

               bra     lc4bb                    ; flag and map fcb as being used                                         
lc4e8          cmpb    #'R                      ; file mode = r (random)?                                       
               beq     lc4f2                    ; branch if so                       

               cmpb    #'D                      ; file mode = d (direct)?                                  
               lbne    BasFMError               ; 'bad file mode' error if not                                        


; open a random/direct file
lc4f2          asl     ,s                       ; set file type to direct                                    
               ldd     rnbfad                   ; get address of random file buffer area                                                  
               pshs    b,a                      ; and save it on the stack                                  
               addd    dfflen                   ; add the record length                                  
               blo     lc504                    ; 'ob' error if sum > $ffff                                    

               cmpd    fcbadr                   ; is it > than fcb data area?                                        
               bls     lc509                    ; branch if not                        

lc504          ldb     #2*29                    ;'out of buffer space' error                                                                         
               jmp     SysErr                   ; jump to error handler                                

lc509          pshs    b,a                      ; save end of random buffer on stack                                                 
               tst     v973                     ; did this file exist                             
               bne     lc514                    ; branch if so                       

               bsr     lc567                    ; set up new file in directory                                       

               bra     lc519                    ; initialize fcb                         
lc514          lda     #$ff                     ; set file type match = $ff (illegal value) -                                                          
               jsr     >lc807                   ; this will force any open matched file to cause                                                         

                                                ; a 'file already open' error
lc519          bsr     lc52d                    ; initialize fcb                              

               com     fcbsof,x                 ; set fcbsof,x to $ff (illegal sector offset) which will                                                                    
                                                ; force new sector data to be read in during get/put
               inc     fcbrec+1,x               ; initialize record number = 1                                            
               puls    a,b,u                    ; u = start of random file buffer area, accd = end                                                            
               std     rnbfad                   ; save new start of random file buffer area                                                     
               stu     fcbbuf,x                 ; save buffer start in fcb                                      
               ldu     dfflen                   ; get random file record length                                         
               stu     fcbrln,x                 ; and save it in fcb                                
               bra     lc4bb                    ; set fat flag, save file type in fcb                                              

; initialize fcb data for input
lc52d          bsr     lc538                    ; initialize fcb                              

               ldu     v974                     ; get ram directory image                                 
               ldu     dirlst,u                 ;get number of bytes in last sector of file                                                        
               stu     fcblst,x                 ;save it in fcb                           

               rts                                     

; initialize file control block
lc538          ldx     fcbtmp                   ; get current file buffer                                        
               ldb     #fcbcon                  ; clear fcb control bytes                                    
lc53c          clr     ,x+                      ; clear a byte                          
               decb                             ; decrement counter                        
               bne     lc53c                    ; branch if not done                             

               ldx     fcbtmp                   ; get current file buffer address back                                                
               lda     dcdrv                    ;get current drive number and                                      
               sta     fcbdrv,x                 ;save it in fcb                           
               lda     v976                     ; =get first granule -                              
               sta     fcbfgr,x                 ; =save it as the starting granule number and                                                         
               sta     fcbcgr,x                 ; =save it as current granule number                                                
               ldb     v973                     ; get directory sector number                                     
               subb    #$03                     ; subtract 3 - directory sectors start at 3                                                    
               aslb                             ; multiply sectors                       
               aslb                             ; by 8 (8 directory                        
               aslb                             ; entries per sector)                          
               pshs    b                        ; save sector offset                          
               ldd     v974                     ; get ram directory image                                 
               subd    #dbuf0                   ; subtract ram offset                                
               lda     #$08                     ; 8 directory entries/sector                                    
               mul                              ; now acca contains 0-7                           
               adda    ,s+                      ; acca contains directory entry (0-71)                                              
               sta     fcbdir,x                 ; save directory entry number                                         
               rts                                     


; set up directory and update file allocation table entry in first unused sector
lc567          ldb     #28*2                    ;'disk full' error                                                               
               lda     v977                     ; get sector number of first empty directory entry                                                          
               lbeq    SysErr                   ; 'disk full' error if no empty directory entries                                                           

               sta     v973                     ; save sector number of first empty directory entry                                                           
               sta     dsec                     ; save sector number in dskcon register                                               
               ldb     #$02                     ; read op code                      
               stb     dcopc                    ; save in dskcon register                                  
               jsr     >DoDiskReadWrite                   ; read sector                      

               ldx     v978                     ; get address of ram image of unused directory                                                      
               stx     v974                     ; entry and save as current used ram image                                                  
               leau    ,x                       ; (tfr x,u) point u to directory ram image                                                 
               ldb     #dirlen                  ; set counter to clear 32 bytes (directory entry)                                                            
lc586          clr     ,x+                      ; clear byte                        
               decb                             ; decrement counter                        
               bne     lc586                    ; continue if not done                               

               ldx     #dnambf                  ; point to filename and extension ram image                                                      
               ldb     #11                      ; 11 bytes in filename and extension                                           
               jsr     UtilCopyBXtoU            ; move b bytes from x to u                                   

               ldd     dfltyp                   ; get file type and ascii flag                                        
               std     $00,u                    ; save in ram image                            
               ldb     #33                      ; first granule to check                               
               jsr     >lc7bf                   ; find the first free granule                                      

               sta     v976                     ; save in ram                     
               sta     $02,u                    ; save in ram image of directory track                                               
               ldb     #$03                     ; get write operation code and save                                           
               stb     dcopc                    ; it in dskcon register                                
               jsr     >DoDiskReadWrite                   ; go write a sector in directory                                         

lc5a9          pshs    u,x,b,a                  ; save registers                                 
               jsr     >lc755                   ; point x to file allocation table                                           

               inc     fat1,x                   ; indicate new data in file alloc table                                                 
               lda     fat1,x                   ; get new data flag                             
               cmpa    wfatvl                   ; have enough granules been removed from the fat to                                                              
                                                ; cause the fat to be written to the disk
               blo     lc5ba                    ; return if no need to write out allocation table                                                          

               jsr     >lc71e                   ; write file allocation sector to disk                                               

lc5ba          puls    a,b,x,u,pc               ; restore registers                                       

; console in ram vector
DosHookCharIn          lda     TextDevN                 ; get device number                                  
               lble    CoCoVect16A              ; branch if not disk file                                   
               leas    $02,s                    ; get rid of return address                                     
lc5c4          pshs    x,b                      ; save registers                             
               clr     cinbfl                   ; clear buffer not empty flag                                       
               ldx     #fcbv1-2                 ; point to file buffer vector table                                               
               ldb     TextDevN                 ; get active disk file number                                       
               aslb                             ; times 2 - two bytes per fcb address                                          
               ldx     b,x                      ; now x points to file buffer                                    
               ldb     fcbtyp,x                 ; get file type                            
               cmpb    #ranfil                  ; is this a random (direct) file?                                             
               bne     lc5ec                    ; branch if not                        


; get a byte from a random file - return char in acca
               ldd     fcbget,x                 ; get the record counter                                    
               cmpd    fcbrln,x                 ;compare to record length and                                          
               bhs     lc5fe                    ;branch to buffer empty if >= record length                                                    

               addd    #$0001                   ; = add one to record pointer and                                            
               std     fcbget,x                 ; = save it in fcb                              
               ldx     fcbbuf,x                 ; point x to start of random file buffer and                                                        
               leax    d,x                      ; add the record counter to it                                      
               lda     $-1,x                    ; get a character from the buffer                                          
               puls    b,x,pc                   ; restore registers and return                                         
; get a byte from a sequential file
lc5ec          ldb     fcbcfl,x                 ; test the cache flag and branch if an                                                       
               beq     lc5f9                    ; extra character has not been read from file                                                      

               lda     fcbcdt,x                 ; get the cache character                                     
               clr     fcbcfl,x                 ; clear the cache flag                                  
               puls    b,x,pc                   ; restore registers and return                                         

lc5f9          ldb     fcbdfl,x                 ; is any data left?                                    
               beq     lc602                    ; branch if so                       

lc5fe          com     cinbfl                   ; set flag to buffer empty                                    
               puls    b,x,pc                   ; restore registers and return                                         

lc602          ldb     fcbcpt,x                 ; get character pointer                                        
               inc     fcbcpt,x                 ; add one to character pointer                                          
               dec     fcblft,x                 ; decrement number of characters left in file buffer                                                                
               beq     lc611                    ; if last character, go get some more                                              

               abx                              ; add character counter to x                                
               lda     fcbcon,x                 ; get data character (skip past 25 fcb control bytes                                                                
               puls    b,x,pc                                 
; get a character from fcb data buffer - return char in acca
lc611          pshs    u,y                      ; save registers                             
               clra                             ;      
               leau    d,x                      ; point u to correct character                                      
               lda     fcbcon,u                 ; =get data char (skip past 25 control bytes)                                                         
               pshs    a                        ; =and save data character on stack                                         
               clr     fcbcpt,x                 ; reset char pointer to start of buffer                                                   
               lda     fcbdrv,x                 ; get drive number and save it in                                             
               sta     dcdrv                    ; dskcon variable                          
               bsr     lc627                    ; go read a sector - fill the buffer                                             

               puls    a,y,u                    ; restore registers and data character                                                
               puls    b,x,pc                   ; restore registers and return                                         
; refill the fcb input data buffer for sequential files
lc627          lda     fcbsec,x                 ; get current sector number                                            
lc629          inca                             ; add one                   
               pshs    a                        ; save new sector number on the stack                                           
               cmpa    #$09                     ; nine sectors per granule                                   
               bls     lc631                    ; branch if <= 9                         

               clra                             ; set to sector zero                         
lc631          sta     fcbsec,x                 ; save sector number                                     
               ldb     fcbcgr,x                 ; get granule numbet to fat pointer                                               
               leau    ,x                       ; point u to fcb (tfr x,u)                                 
               jsr     >lc755                   ; point x to proper file allocation table                                                  

               abx                              ; add old granule number to fat pointer                                           
               ldb     fatcon,x                 ; get granule number (6 control bytes at front of fat)                                                                  
               leax    ,u                       ; point x to fcb                       
               cmpb    #$c0                     ; is current granule last one in file?                                               
               bhs     lc64d                    ; yes              

               puls    a                        ; get sector number                         
               suba    #10                      ; was it 10? - overflow to next granule if so                                                     
               bne     lc65e                    ; branch if not                        

               stb     fcbcgr,x                 ; save new granule number                                     
               bra     lc629                    ; set variables for new granule                                        
lc64d          andb    #$3f                     ; get number of sectors used in this granule                                                          
               cmpb    #$09                     ; 9 sectors / granule                              
               bls     lc658                    ; branch if ok                       

lc653          ldb     #2*32                    ;'bad file structure' error                                                                    
               jmp     SysErr                   ; error driver                       

lc658          subb    ,s+                      ; subtract current sector number and puls a                                                        
               blo     lc67d                    ; branch if past last sector                                     

               tfr     b,a                      ; sector number to acca                              
lc65e          pshs    a                        ; save sector number difference                                          
               bsr     lc685                    ; increment record number                                  

               lda     #$02                     ;get read operation code                                
               sta     dcopc                    ;and save it in dskcon variable                                        
               jsr     >lc763                   ; get proper track and sector to dskcon variables                                                          

               leau    fcbcon,x                 ; point u to start of fcb data buffer                                                  
               stu     dcbpt                    ; and save it in dskcon variable                                         
               jsr     >DoDiskReadWrite                   ; go read a sector into fcb buffer                                           

               clr     fcblft,x                 ; number of chars left in buffer = 256                                                  
               ldb     ,s+                      ; get sector number off stack                                    
               bne     lc684                    ; return if data left; fall thru if last sector                                                        

               ldd     fcblst,x                 ; get number of bytes in the last sector                                                    

               bne     lc681                    ; branch if some bytes in last sector                                              

lc67d          clrb                             ; set number of remaining bytes = 256                                               
               com     fcbdfl,x                 ; set data left flag to $ff                                       
lc681          stb     fcblft,x                 ; save the number of chars left in buffer                                                          
lc684          rts                                          


lc685          ldu     fcbrec,x                 ; get current record number                                            
               leau    $01,u                    ; bump it                   
               stu     fcbrec,x                 ; put it back                         
               rts                                     


; scan directory for filename.ext found in dnambf. if filename found,
; return with sector number in v973, granule in v976 and ram buffer
; containing directory data in v974. if disk is full then v973,
; v977 = 0. the first unused sector returned in v977, ram image in v978
lc68c          clr     v973                     ; clear sector number                                  
               clr     v977                     ; clear temp sector counter                                   
               ldd     #$1102                   ; track 17 (directory), read operation code                                                     
               sta     dctrk                    ; save track number                            
               stb     dcopc                    ; save operation code (read)                                     
               ldb     #$03                     ; read sector 3 (first directory sector)                                                
lc69b          stb     dsec                     ; save sector number in dskcon variable                                                    
               ldu     #dbuf0                   ;buffer area number 0 as data buffer - save                                                     
               stu     dcbpt                    ;in dskcon variable                            
               jsr     >DoDiskReadWrite                   ; go read a sector                           

lc6a5          stu     v974                     ; save ram directory buffer address                                                
               leay    ,u                       ; point y to directory buffer                                    
               lda     ,u                       ; get a byte from buffer                              
               bne     lc6d6                    ; branch if not zero - file is active                                              

               bsr     lc6d9                    ; set unused file pointers if entry has been killed                                                            

lc6b0          ldx     #dnambf                  ; point to disk file name buffer                                                
lc6b3          lda     ,x+                      ;compare the filename and extension                                               
               cmpa    ,u+                      ;stored in ram at dnambf to the directory                                                 
               bne     lc6c7                    ;entry stored at ,u (branch if mismatch)                                                 

               cmpx    #dnambf+11               ; at end of file name buffer?                                            
               bne     lc6b3                    ; branch if not done checking filename                                               

               stb     v973                     ; save sector number in dskcon variable                                               
               lda     fcbfgr,u                 ;get number of first granule in file                                                
               sta     v976                     ;and save it in v976                            
               rts                                     


lc6c7          leau    dirlen,y                 ; get next directory entry (dirlen bytes per entry)                                                                     
               cmpu    #dbuf0+seclen            ; at end of buffer?                                     
               bne     lc6a5                    ; check next entry if not at end                                         

               incb                             ; next sector                  
               cmpb    #11                      ; 11 sectors max in directory                                     
               bls     lc69b                    ; branch if more sectors                                 

               rts                                     


lc6d6          coma                             ; complement first byte in directory emtry                                                    
               bne     lc6b0                    ; branch if file is active - fall thru if not used                                                           


; set pointers for first unused directory entry
lc6d9          lda     v977                     ; unused entry already found?                                          
               bne     DosHookReadInputLine                   ; return if unused entry already found                                                

               stb     v977                     ; sector containing this directory entry                                                
               stu     v978                     ; points to ram area where directory data is stored                                                           
DosHookReadInputLine         rts                                           


lc6e5          ldb     #2*26                    ; 'ne' error                                                     
               tst     v973                     ; was a directory match found?                                      
               bne     DosHookReadInputLine                   ; return if found                           

               jmp     SysErr                   ; jump to error handler if not found                                             


; kill command
CmdKill        jsr     >lc935                   ; get filename.ext from basic                                          
               jsr     la5c7                    ; 'syntax' error if more characters on line                                                    
               jsr     >GetFAT                  ; get valid fat data                             

               bsr     lc68c                    ; test for file name match in directory                                                

               bsr     lc6e5                    ; make sure the file existed                                     

lc6cf          lda     #$ff                     ; match file type = $ff; this will cause an 'ao'                                                             
                                                ; error to be generated if any file type is open
               jsr     >lc807                   ; check to make sure file is not open                                              

               ldx     v974                     ;get ram image of directory                                   
               clr     dirnam,x                 ;and zero first byte - kill file                                            
               ldb     #$03                     ; =write operation code - save                                      
               stb     dcopc                    ; =it in dskcon variable                                 
               jsr     >DoDiskReadWrite                   ; write a sector                         

               ldb     dirgrn,x                 ; get number of first granule in file                                                 
lc70f          bsr     lc755                    ; point x to proper file allocation table                                                       

               leax    fatcon,x                 ; skip 6 control bytes                                   
               abx                              ; point to correct entry                            
               ldb     ,x                       ; get next granule                        
               lda     #$ff                     ;get free granule flag and                                  
               sta     ,x                       ;mark granule as free                           
               cmpb    #$c0                     ; was this the last granule?                                     
               blo     lc70f                    ; keep freeing granules if not last one                                                

                                                ; write file allocation sector to directory - do not write
                                                ; the six control bytes at the start of the fat to the disk
lc71e          ldu     #dbuf0                   ; =point u to disk buffer 0 and                                              
               stu     dcbpt                    ; =save it as dskcon variable                                      
               ldd     #$1103                   ; write directory track - save                                        
               sta     dctrk                    ; track and write operation code in                                            
               stb     dcopc                    ; dskcon variables                           
               ldb     #$02                     ; = get file allocation sector and                                          
               stb     dsec                     ; = save in dskcon variable                                   
               bsr     lc755                    ; point x to proper file allocation table                                                  

               clr     fat1,x                   ; reset flag indicating valid fat data has been stored on disk                                                                        
               leax    fatcon,x                 ; move (x) to start of granule data                                                
               ldb     #granmx                  ; 68 bytes in fat                            
               jsr     UtilCopyBXtoU            ; move accb bytes from fat ram image to dbuf0                                                      


; zero out all of the bytes in the fat sector which do not contain the granule data
lc739          clr     ,u+                      ; clear a byte                          
               cmpu    #dbuf0+seclen            ; finished the whole sector?                                              
               bne     lc739                    ; no             

               jmp     >DoDiskReadWrite                   ; write a sector                         


; enter with accb containing file number (1-15); exit with x pointing
; to correct file buffer; flags set according to file type.

lc744          pshs    b                        ; save file number on stack                                      
               ldb     TextDevN                 ; get device number (file number)                                           
               fcb      Skip2                   ; cmpx 
lc749          pshs    b                        ; save file number on stack                                      
               aslb                             ; x2: 2 bytes per pointer                              
               ldx     #fcbv1-2                 ; point x to start of fcb pointers                                              
               ldx     b,x                      ; point x to proper fcb                              
               ldb     fcbtyp,x                 ; set flags according to file type                                              
               puls    b,pc                     ; restore file number                              

; point x to drive allocation table

lc755          pshs    b,a                      ; save accd on stack                                 
               lda     dcdrv                    ; get drive number                           
               ldb     #fatlen                  ; get length of file allocation table                                                
               mul                              ; multiply by drive number to get offset                                            
               ldx     #fatbl0                  ; start of file allocation table                                           
               leax    d,x                      ; point to right table                              
               puls    a,b,pc                   ; restore accd                         

; convert granule number to track & sector number - x must be pointing to correct
; fcb; the track and sector number will be stored in dskcon registers
lc763          ldb     fcbcgr,x                 ; get granule number                                     
               lsrb                             ; divide by 2 - 2 granules / track                                       
               stb     dctrk                    ; track number                       
               cmpb    #17                      ; track 17 = directory track                                    
               blo     lc76e                    ; branch if < directory track                                      

               inc     dctrk                    ; incr track number if > directory track                                                 
lc76e          aslb                             ; multiply track number by 2                                      
               negb                             ; negate granule number                            
               addb    fcbcgr,x                 ; b=0 if even granule; 1 if odd                                            
               bsr     lc779                    ; return b=0 for even granule number, b=9 for odd granule number                                                                         

               addb    fcbsec,x                 ; add sector number                                
               stb     dsec                     ; save sector number                            
               rts                                     

; multiply accd by 9
lc779          pshs    b,a                      ; temp store accd on stack                                       
               aslb                             ;      
               rola                             ; multiply by 2                    
               aslb                             ; =        
               rola                             ; = multiply by four                         
               aslb                             ;      
               rola                             ; multiply by eight                        
               addd    ,s++                     ; add one = multiply by nine                                     
               rts                                     


; convert accd into a granule number - return result in accb;
; enter with accd containing a number of sectors. return in accb
; the number (0-67) corresponding to the number of complete
; granules contained in that many sectors.
; divide by 90, multiply by 10 is faster than divide by 9
lc784          clr     ,-s                      ; clear a temporary slot on the stack                                                 
lc786          inc     ,s                       ; divide accd by 90 - save the                                         
               subd    #9*10                    ; quotient+1 on the stack - remainder                                                                                 
               bpl     lc786                    ; in accb                  
               lda     ,s                       ; = put the quotient+1 in acca and                                        
               stb     ,s                       ; = save remainder on stack                                 
               ldb     #10                      ; multiply (quotient+1)                              
               mul                              ; by 10           
               puls    a                        ; put the remainder in acca                                 
lc796          decb                             ; decrement the granule count by one for                                                  
               adda    #$09                     ; every nine sectors (1 granule) in the                                                
               bmi     lc796                    ; remainder - compensate for the + 1 in quotient+1                                                           
               clra                             ; clear ms byte of accd                            
lc79c          rts                                          


; make sure ram file allocation table data is valid
GetFAT          bsr     lc755                    ; point x to fat for the correct drive number                                                           

               tst     fat0,x                   ; check to see if any files are active                                                
               bne     lc79c                    ; return if any files active in this fat                                                 

               clr     fat1,x                   ; reset fat data valid flag                                     
               leau    fatcon,x                 ; load u with start of granule data buffer                                                       
               ldx     #dbuf0                   ; buffer for disk transfer                                    
               stx     dcbpt                    ; put in dskcon parameter                                  
               ldd     #$1102                   ; directory track, read sector                                        
               sta     dctrk                    ; store in dskcon track number                                       
               stb     dcopc                    ; store in dskcon op code                                  
               ldb     #$02                     ; get sector number 2 (file allocation table)                                                     
               stb     dsec                     ; store in dskcon parameter                                   
               jsr     >DoDiskReadWrite                   ; go read sector                         

               ldb     #granmx                  ; transfer file allocation table to file alloc table buffer                                                                      
               jmp     UtilCopyBXtoU            ; move b bytes from (x) to (u)                                       


; find first free granule - enter with accb containing
; granule from which to start searching. the found granule
; is marked by storing a $c0 in the granule's data byte
; to indicate that it is the last granule in the file.
; return with first free granule found in acca
lc7bf          bsr     lc755                    ; point x to file alloc table                                           

               leax    fatcon,x                 ; skip control bytes                                 
               clra                             ; use acca as granule counter                                  
               andb    #$fe                     ; mask off bit zero of search granule                                              
               clr     ,-s                      ; initialize and save a byte on stack (direction flag)                                                             
lc7c8          com     b,x                      ; is this granule free? ($ff=free)                                              
               beq     lc7fd                    ; branch if it is                          

               com     b,x                      ; restore granule data                             
               inca                             ; add one to granule counter                                 
               cmpa    #granmx                  ; granmx geanules per disk                                      
               bhs     lc7f8                    ; branch if all granules checked (disk full)                                                     

               incb                             ; incr to next granule                           
               bitb    #$01                     ; is bit 0 set?                        
               bne     lc7c8                    ; branch if odd granule number (same track)                                                    

               pshs    b,a                      ; save granule counter and current granule number                                                         
               subb    #$02                     ; subtract one track (2 granules)                                          
               com     $02,s                    ; complement direction flag                                    
               bne     lc7ec                    ; branch every other time                                  

               subb    ,s+                      ; subtract the granule counter from the current granule number                                                                      
               bpl     lc7e8                    ; branch if lower bound not exceeded                                             
               ldb     ,s                       ; restore current granule number if lower bound exceeded                                                              
lc7e6          com     $01,s                    ; complement flag - if granule number has exceeded                                                                
                                                ; bounds on either the hi or lo side, force it to go in
                                                ; the direction opposite the exceeded bound
lc7e8          leas    $01,s                    ; clean up stack                               
               bra     lc7c8                    ; check for another free granule                                         

lc7ec          addb    ,s+                      ; add the granule counter to the current granule number                                                                    
               cmpb    #granmx                  ; granmx granules per disk                                      
               blo     lc7e8                    ; branch if upper bound not exceeded                                             

               ldb     ,s                       ; restore current granule count and go twice                                                  
               subb    #$04                     ; as far as usual in opposite direction if upper bound exceeded                                                                        
               bra     lc7e6                    ; keep searching                         
lc7f8          ldb     #2*28                    ; 'disk full' error                                                            
               jmp     SysErr                   ; jump to error handler                                


; point x to first free granule position in the file allocation
; table and mark the position with a last granule in file marker
lc7fd          leas    $01,s                    ; clear up stack - remove direction flag                                                       
               tfr     b,a                      ; granule number to acca                               
               abx                              ; point x to first found granule                                    
               ldb     #$c0                     ; last granule flag                           
               stb     ,x                       ; mark the first found granule as the last granule                                                        
lc806          rts                                          


; check all active files to make sure a file is not already open - to be open
; a file buffer must match the drive number and first granule number
; in ram directory entry and the fcb type must not match the file type in acca
; an 'ao' error will not be generated if a file is being opened for
; the same mode that it has already been opened under.

lc807          pshs    a                        ; save file type on stack                                    
               ldb     fcbact                   ; number of currently open files                                          
               incb                             ; add one more to file counter                                   
lc80d          jsr     >lc749                   ; point x to fcb of this file                                           

               beq     lc829                    ; branch if buffer not being used                                          

               lda     dcdrv                    ; get drive number and check to see if it                                                  
               cmpa    fcbdrv,x                 ; matches the drive number for this buffer                                                       
               bne     lc829                    ; file exists on another drive                                       

               ldu     v974                     ; get ram directory area                                
               lda     dirgrn,u                 ; get first granule in file                                        
               cmpa    fcbfgr,x                 ; does it match this file buffer?                                              
               bne     lc829                    ; no             

               lda     fcbtyp,x                 ; get file type of this buffer                                          
               cmpa    ,s                       ; does it match the one we are looking for?                                                  
               lbne    la61c                    ; 'file already open' error if not                                            

lc829          decb                             ; decr file counter                             
               bne     lc80d                    ; branch if haven't checked all active files                                                     

               puls    a,pc                     ; restore file type and return                                       

lc82e          jsr     la5a5                    ; evaluate an expression (device number)                                                      

               clr     TextDevN                 ; set device number to screen                                       
               tstb                             ; test new device number                             
               lble    BasFCError               ; 'fc' error if device number not a disk file                                                       
               jsr     >lc749                   ; point x to fcb                         

               lda     fcbtyp,x                 ; test if buffer is in use                                      
               lbeq    la3fb                    ; 'file not open' error                                 

               cmpa    #ranfil                  ; direct/random file?                                 
               beq     lc806                    ; return if random                           

lc845          jmp     BasFMError               ; bad file mode error if not random                                                 


; input device number check ram hook
DosHookCheckOpenIn          lda     #inpfil                  ; input file type                                 
lc84a           fcb     Skip2                   ; cmpx               

; print device number check ram hook
DosHookCheckOpenOut          lda     #outfil                  ; output file type                                  
               tst     TextDevN                 ; check device number and return if                                             
               ble     lc806                    ; not a disk file                          
               stx     ,s                       ; = replace subroutine return address with x register -                                                             
                                                ; = this is the same as leas 2,s and pshs x
               jsr     >lc744                   ; point x to fcb                         

               pshs    b,a                      ; save accb and file type on stack                                          
               lda     fcbtyp,x                 ; get file type                           
               lbeq    la3fb                    ; 'file not open' error                                 

               cmpa    #ranfil                  ; random file?                          
               beq     lc868                    ; branch if random file                                

               cmpa    ,s                       ; is this fcb of the proper type?                                        
               bne     lc845                    ; 'file mode' error if not                                   

lc866          puls    a,b,x,pc                 ; restore accb,x,acca (file type) and return                                                              

lc868          ldx     $04,s                    ; get calling address from the stack and                                                      
               cmpx    #lb00c                   ; return unless coming from                                      
               bne     lc866                    ; basic's 'input' statement                                    

               jsr     VarCKComma               ; syntax check for a comma                                      

               cmpa    #'"                      ; check for a double quote                                   
               bne     lc881                    ; return to basic's 'input' command                                            

               jsr     lb244                    ; strip prompt string from basic and put it on the string stack                                                                        

               jsr     lb657                    ; purge the string put on the string stack                                                   

               ldb     #'                       ;                         ; semicolon                   
               jsr     VarCKChar                ; do a syntax check for semicolon                                          

lc881          ldx     #lb01e                   ; get modified reentry point into basic                                                      
               stx     $04,s                    ; and put it into the return address on the stack                                                          
               puls    a,b,x,pc                 ; return to basic                              

; device number validity check ram hook
DosHookCheckIONum          ble     lc8af                    ; return if not a disk file                                         
               cmpb    fcbact                   ; compare device number to highest possible                                                      
               lbhi    la61f                    ; 'device number' error if too big                                            
               puls    x,pc                     ; return                 

; set print parameters ram hook
DosHookRetDevParam          tst     TextDevN                 ;check device number and                                       
               ble     lc8af                    ;return if not disk file                                 
               leas    $02,s                    ; purge return address off of the stack                                                 
               pshs    x,b,a                    ; save registers                          
               clr     CasIOFlag                ; set print device number to non-cassette                                                   
               jsr     >lc744                   ; point x to fcb                         

               ldb     fcbpos,x                 ; get print position                                
               clra                             ; print width (256)                        
               ldx     #$1000                   ; tab field width and tab zone                                        
               jmp     la37c                    ; save the print parameters                                    


; break check ram hook
DosHookBreak         tst     TextDevN                 ; check device number and return                                                
               ble     lc8af                    ; if not a disk file                             
               leas    $02,s                    ; = purge return address off of the stack - don't                                                           
lc8af          rts                              ; = do a break check if disk file                                          


; command interpretation ram hook
DosHookInterpret         
               leas    $02,s                    ; purge return address off of the stack                                                       
lc8b2          andcc   #$af                     ; enable irq & firq                                  
               clr     pia0+2                   ; strobe all keys (column strobe)                                           
               lda     pia0                     ; read keyboard rows                            
               coma                             ; invert keyboard row data                               
               anda    #$7f                     ; mask off joystick input bit                                      
               beq     lc8c2                    ; branch if no key down                                

               jsr     BasPollKeyboard          ; go do a break check if a key is down                                               

lc8c2          ldx     BasAddrSigByte           ; get input pointer into x                                         
               stx     BasDirectTextPtr         ; temp save it                        
               lda     ,x+                      ; search for the end of current line                                           
               beq     lc8d1                    ; branch if end of line                                

               cmpa    #':                      ; check for end of sub line, too                                         
               beq     lc8f3                    ; branch if end of sub line                                    

               jmp     BasSNError               ; 'syntax' error if not end of line                                            

lc8d1          lda     ,x++                     ;get ms byte of address of next basic line                                                       
               sta     BasBreakFlag             ;and save it in BasCurrentLine                                 
               bne     lc8da                    ; branch if not end of program                                       

               jmp     lae15                    ; go 'stop' the system                               

lc8da          ldd     ,x+                      ;get line number of this line and                                             
               std     BasCurrentLine           ;save it in BasCurrentLine                            
               stx     BasAddrSigByte           ; reset basic's input pointer                                       
               lda     BasTronFlag              ; check the trace flag and                                    
               beq     lc8f3                    ; branch if trace off                              

               lda     #'[                      ; < left delimiter of tron                                  
               jsr     TextOutChar              ; send character to console out                                        

               lda     BasCurrentLine           ; get number of current line number                                             
               jsr     TextOutNum16             ; convert accd to decimal & print it on screen                                                       

               lda     #']                      ; > right delimiter of tron                                   
               jsr     TextOutChar              ; send a character to console out                                          

lc8f3          jsr     BasChrGet                ; get next character from basic                                              

               tfr     cc,b                     ; save status register in accb                                      
               cmpa    #$98                     ; csave token?                       
               bne     lc8fe                    ; no             

               jmp     l8316                    ; go check for csavem                              

lc8fe          cmpa    #$97                     ; cload token?                            
               bne     lc905                    ; no             

               jmp     l8311                    ; jump to exbas' cload routine                                       

lc905          tfr     b,cc                     ; restore status register                                      
               jsr     ladc6                    ; loop through basic's main interpretation loop                                                        

               bra     lc8b2                                

; eof ram hook
DosHookEOF         leas    $02,s                    ; purge return address off of the stack                                                       
               lda     TextDevN                 ; get device number and save                                      
               pshs    a                        ; it on the stack                       
               jsr     la5ae                    ; strip device number off of input line                                                

               jsr     la3ed                    ; verify that the file type was 'input'                                                

               tst     TextDevN                 ; check device number and                                   
               lble    la5da                    ; branch back to  basic's eof if not disk file                                                        
               jsr     >lc744                   ; point x to fcb                         

               ldb     fcbtyp,x                 ; get file type                           
               cmpb    #ranfil                  ; random file?                          
               lbeq    BasFMError               ; 'fm' bad file mode error if random                                              

               clrb                             ; file not empty flag - set to not empty                                             
               lda     fcbcfl,x                 ;check the cache flag - branch if                                             
               bne     lc932                    ;there is a character which has been cached                                                    

               ldb     fcbdfl,x                 ; get sequential input file status                                              
lc932          jmp     la5e4                    ; link back to basic's eof statement                                                  


; get filename/extension: drive number from basic
lc935          ldx     #defext                  ; point to ' ' blank (default) extension                                                        
lc938          clr     ,-s                      ; clear a byte on stack for use as a drives flag                                                       
               lda     defdrv                   ; get default disk number                                   
               sta     dcdrv                    ; store in dskcon parameter                                    
               ldu     #dnambf                  ; disk filename buffer                                 
               ldd     #$2008                   ; store 8 blanks in ram (default file name)                                                     
lc945          sta     ,u+                      ; store a blank in file name                                        
               decb                             ; decrement counter                        
               bne     lc945                    ; branch if not done                             

               ldb     #$03                     ; 3 bytes in extension                              
               jsr     UtilCopyBXtoU            ; move b bytes from (x) to (u)                                       

               jsr     l8748                    ; evaluate a string expression                                       

               leau    ,x                       ; point u to start of string                                   
               cmpb    #$02                     ; check length of string and                                     
               blo     lc96a                    ; branch if < 2                        

               lda     $01,u                    ; = get 2nd character in string and                                            
               cmpa    #':                      ; = check for colon                            
               bne     lc96a                    ; branch if no drive number                                    

               lda     ,u                       ; get 1st character                         
               cmpa    #'0                      ; in string and                        
               blo     lc96a                    ; check to see                       

               cmpa    #'3                      ; if it is in                      
               bhi     lc96a                    ; the range 0-3                        
               bsr     lc99d                    ; get drive number                           

lc96a          ldx     #dnambf                  ; point x to file name buffer                                             
               incb                             ; compensate for decb below                                
lc96e          decb                             ; decrement string length                                   
               bne     lc97d                    ; branch if more characters in string                                              

               leas    $01,s                    ; clean up stack - remove drive flag                                              
lc973          cmpx    #dnambf                  ; pointer still at start of buffer?                                                    
               bne     lc9df                    ; return if not                        

lc978          ldb     #2*31                    ; 'bad filename' error if null filename                                                                               
               jmp     SysErr                   ; error handler                        

lc97d          lda     ,u+                      ; get a character from string                                         
               cmpa    #'.                      ; look for period?                           
               beq     lc9b0                    ; yes              

               cmpa    #'/                      ; slash?                 
               beq     lc9b0                    ; yes              

               cmpa    #':                      ; colon?                 
               beq     lc994                    ; yes              

               cmpx    #dextbf                  ; compare pointer to end of filename buffer                                                       
               beq     lc978                    ; 'bad filename' error - filename too long                                                   

               bsr     lc9d0                    ; put a character in filename                                      

               bra     lc96e                    ; get another character from string                                            
lc994          bsr     lc973                    ; 'bad filename' error if no filename yet                                                       

               bsr     lc99d                    ; get drive number                           

               tstb                             ; check length of string                             
               bne     lc978                    ; ''bad filename' error if more characters left                                                        

lc99b          puls    a,pc                     ; remove drives flag from stack and return                                                        

; grab drive number
lc99d          com     $02,s                    ; toggle drive flag                                 
               beq     lc978                    ; 'bad filename' error if drive number defined twice                                                             

               lda     ,u++                     ; ascii value of drive number to acca                                             
               subb    #$02                     ; decrement string length by 2 for drive (:x)                                                      
               suba    #'0                      ; subtract ascii bias                              
               blo     lc978                    ; drive number too low - 'bad filename' error                                                      

               cmpa    #$03                     ; max of 4 drives                          
               bhi     lc978                    ; drive number too high - 'bad filename' error                                                       
               sta     dcdrv                    ; store in dskcon drive number                                       
               rts                                     


; grab extension
lc9b0          bsr     lc973                    ; 'bad filename' error if no filename yet                                                       

               ldx     #dfltyp                  ; point x to end of extension buffer                                               
               lda     #space                   ; blank                 
lc9b7          sta     ,-x                      ;             
               cmpx    #dextbf                  ; fill extension with                                 
               bne     lc9b7                    ; blanks (default)                           

lc9be          decb                             ; decrement string counter                                    
               beq     lc99b                    ; return if zero                         

               lda     ,u+                      ; get a character from string                                    
               cmpa    #':                      ;check for drive separator                                   
               beq     lc994                    ;          

               cmpx    #dfltyp                  ; =check for end of estension ram buffer &                                                      
               beq     lc978                    ; = 'bad filename' error if extension too long                                                       

               bsr     lc9d0                    ; put a character in extension buffer                                              

               bra     lc9be                    ; get another extension character                                          

; insert character into filename or extension
lc9d0          sta     ,x+                      ; store character in filename buffer                                                
               beq     lc978                    ; 'bad filename' error; zeroes are illegal                                                   

               cmpa    #'.                      ; period?                  
               beq     lc978                    ; 'bad filename' error if period                                         

               cmpa    #'/                      ; slash?                 
               beq     lc978                    ; 'bad filename' error if slash                                        

               inca                             ; check for $ff                    
               beq     lc978                    ; 'bad filename' error if $ff                                      

lc9df          rts                                          


; save command
CmdSave           cmpa    #'M                      ;              
               lbeq    lcf68                    ;branch if savem                          

               bsr     lca33                    ; go get filename, etc. from basic                                           

               ldx     Misc16BitScratch         ; zero out x reg                        
               stx     dfltyp                   ; set file type and ascii flag to zero                                                
               jsr     BasChrGetCurr            ; get current input character from basic                                                  

               beq     lca12                    ; branch if end of line                                

               jsr     VarCKComma               ; syntax check for comma                                    

               ldb     #'A                      ;ascii file?                    
               jsr     VarCKChar                ;syntax check on contents of accb                                          

               bne     lc9df                    ; return if no more characters on line                                               

               com     dascfl                   ; set crunched/ascii flag to ascii                                            
               bsr     lca04                    ; open a sequential file for output                                            

               clra                             ; set zero flag - cause entire file to be listed                                                     
               jmp     BasList                  ; 'list' the file to console out                                        


; open a sequential file for input/output - use the system
; fcb located at the top of fcbs
lca04          lda     #'O                      ; output file type                               
lca06          fcb      Skip2                   ; cmpx
lca07          lda     #'I                      ; input file type                              
               ldb     fcbact                   ; get number of reserved files currently reserved                                                           
               incb                             ; add one - use one above highest reserved fcb                                                   
               stb     TextDevN                 ; save it in device number                                    
               jmp     >lc48d                   ; open a file & initialize fcb                                       

; save a crunched file - a preamble of three bytes will preceed crunched
; files: byte 1 = $ff, 2,3 = length of basic program
lca12          bsr     lca04                    ; open a sequential file for output                                                 

               lda     #$ff                     ; basic file flag                         
               jsr     >lcc24                   ; console out                      

               ldd     BasVarSimpleAddr         ; load accd with start of variables                                             
               subd    BasStartProg             ; subtract start of basic                                    
               jsr     >lcc24                   ; console out file length ms byte                                          

               tfr     b,a                      ; pull ls byte into acca                               
               jsr     >lcc24                   ; console out file length ls byte                                          

               ldx     BasStartProg             ; point x to start of basic                                     
lca27          lda     ,x+                      ; get byte from basic                                 
               jsr     >lcc24                   ; send to console out                              

               cmpx    BasVarSimpleAddr         ; compare to end of basic                                    
               bne     lca27                    ; keep going if not at end                                   

               jmp     la42d                    ; close file                     

lca33          ldx     #basext                  ; point to 'bas' extension (default)                                               
               jmp     >lc938                   ; get filename.ext from basic                                      


; merge command
CmdMerge          clra                             ; run flag (0 = don't run)                                    
               ldb     #$ff                     ; merge flag ($ff = merge)                                  
               bra     lca50                    ; go load the file                           

; run ram vector
DosHookRun         cmpa    #'"                      ; check for filename delimiter (double quote)                                                            
               lbne    CoCoVect194              ; none - jump to exbas run ram hook                                              

               lda     #$02                     ; run flag - don't close all files before run                                                     
               bra     lca4f                    ; load the file                        

; load command
CmdLoad           cmpa    #'M                      ;              
               lbeq    lcfc1                    ;branch if loadm                          

               clra                             ; run flag = zero (don't run)                                  
lca4f          clrb                             ; clear merge flag                            
lca50          sta     drunfl                   ; run flag (0 = don't run, 2 = run)                                                  
               stb     dmrgfl                   ; merge flag (0 = no merge, $ff = merge)                                                  
               bsr     lca33                    ; go get filename, etc. from basic                                           

               jsr     BasChrGetCurr            ; get current input char                                  

               beq     lca6c                    ; branch if end of line                                

               jsr     VarCKComma               ; syntax check for comma                                    

               ldb     #'R                      ;         
               jsr     VarCKChar                ;is next char 'r'? run after load                                          

               jsr     la5c7                    ; syntax error if any more chars on line                                                 

               lda     #$03                     ;set flags to run and close all files                                             
               sta     drunfl                   ;before the file is run                                 
lca6c          bsr     lca07                    ; grab fcb for input file                                       

               lda     dascfl                   ;check ascii flag and branch                                      
               beq     lca7e                    ;if crunched basic file                                

               tst     dmrgfl                   ; is this a merge?                            
               bne     lca7b                    ; branch if merge                          

               jsr     BasNew                   ; do a 'new' - erase variables, reset variables                                                        

lca7b          jmp     lac7c                    ; go to basic's main loop, it will load program                                                             


; load in a crunched basic file
lca7e          lda     dfltyp                   ;check file type (must be basic:0) & check                                                         
               ora     dmrgfl                   ;merge flag (must be no merge: 0)                                           
               lbne    BasFMError               ; 'bad file mode' error if merge or non-basic                                                       

               jsr     BasNew                   ; do a 'new' - reset pointers, erase variables                                                       

               com     dloadfl                  ; set the load flag to $ff - this will cause a new to                                                               
                                                ; occur if an error occurs while the program is being loaded
               jsr     >lcdbc                   ; get char from buffer - should be $ff                                               

               jsr     >lcdbc                   ; get another - ms byte of length                                          

               pshs    a                        ; save ms byte on stack                             
               jsr     >lcdbc                   ; ls byte of length of program                                       

               tfr     a,b                      ; put ls byte into accb                              
               puls    a                        ; now accd contains length of program                                           
               addd    BasStartProg             ; add beginning of basic                                   
               jsr     lac37                    ; see of enough room in ram for this file                                                  

               ldx     BasStartProg             ; get start of basic                              
lcaa4          jsr     >lc5c4                   ; read a char from console in                                           

               ldb     cinbfl                   ; buffer empty?                         
               bne     lcaaf                    ; branch if so                       

               sta     ,x+                      ; store char                   
               bra     lcaa4                    ; get another character                                

lcaaf          clr     dloadfl                  ; clear load flag - load was error free                                                      
               stx     BasVarSimpleAddr         ; save new start of variables                                       
; make sure last three bytes loaded were zero
               ldb     #$03                     ; check three bytes                           
lcab6          lda     ,-x                      ; check a byte                          
               bne     lcabd                    ; branch if non-zero                             

               decb                             ; decrement counter                        
               bne     lcab6                    ; keep checking if not done                                    

lcabd          ldx     BasVarSimpleAddr         ; get start of variables                                       
lcabf          stx     BasVarSimpleAddr         ; save start of variables                                        
               clr     ,x+                      ; clear a byte                     
               decb                             ; decremrnt counter                        
               bpl     lcabf                    ; keep clearing bytes if not done                                          
lcac6          jsr     la42d                    ; close selected file                                   

               jsr     BasVect1                 ; do part of new - erase variables, reset input ptr                                                            

               jsr     CoCoVect194              ; initialize exbas graphics variables                                               

               jsr     BasVect2                 ; relocate all the basic next line pointers                                                    

               asr     drunfl                   ; check lsb of run flag                                 
               blo     lcada                    ; branch if don't close all files                                          

               jsr     la426                    ; close all files                          

lcada          asr     drunfl                   ; test bit 1 of run flag                                       
               lbcs    BasRun                   ; branch to comm interpretation loop if bit 1 set                                                           

               jmp     BasCmdMode               ; return to direct mode                                


DosHookTermInputLine         tst     TextDevN                 ; check device number and                                         
               bgt     lcac6                    ; try to run file if it is a disk file                                               
               rts                                     


; close all file buffers ram vector
DosCloseAllFiles          ldb     fcbact                   ; get the number of reserved file buffers                                                        
               incb                             ; add one              
lcaed          pshs    b                        ; save it                    
               stb     TextDevN                 ; store it in device number                                     
               bsr     lcb01                    ; close file                     

               puls    b                        ; get back number of file buffers                                       
               decb                             ; decrement file buffer counter                                    
               bne     lcaed                    ; branch if all files not closed                                         

lcaf8          rts                                          


; close file ram hook
DosHookCloseFile          tst     TextDevN                 ; check device number and return                                               
               lble    CoCoVect176              ; if not a disk file                              
               leas    $02,s                    ; purge return address off of the stack                                                 
lcb01          jsr     >lc744                   ; point x to correct fcb                                      

               clr     TextDevN                 ; set device number to screen                                       
lcb06          stx     fcbtmp                   ; save file buffer pointer                                         
               lda     fcbtyp,x                 ; get the type of this file                                       
               beq     lcaf8                    ; return if file not open                                  

               pshs    a                        ; save file type                      
               clr     fcbtyp,x                 ; close the file - zero out the file type                                                     
               ldb     fcbdrv,x                 ; get drive number and                                  
               stb     dcdrv                    ; save it in dskcon variable                                     
               cmpa    #outfil                  ; = check for output type and                                         
               bne     lcb31                    ; = branch if not output type file                                           


; close a sequential output file
               ldb     fcblft,x                 ; get the number of characters in buffer                                                    
               lda     #$80                     ; set the pre-saved bit to indicate that the data                                                         
                                                ; has already been saved on disk
               ora     fcbcpt,x                 ; 'or' in the full sector flag                                          
               std     fcblst,x                 ; save the number of bytes used in the last sector                                                              

               inc     fcbsec,x                 ; increment the sector number                                         
               ldb     fcbcgr,x                 ; get the current granule number                                            
               jsr     >lc755                   ; point x to file allocation table                                           

               sta     fat1,x                   ; set fat data not valid flag (acca <> 0)                                                   
               abx                              ; add granule offset to fat pointer                                       
               inc     fatcon,x                 ; increment granule data (add one sector to last                                                            
                                                ; granule) skip past the six fat control bytes
lcb2e          jmp     >lcbc3                   ; update fat and directory                                        

lcb31          cmpa    #ranfil                  ; random file?                               
               bne     lcb2e                    ; no - update fat and directory if sequential input file                                                                 


; close a random file
               ldd     fcbrln,x                 ; get record length                               
               ldx     fcbbuf,x                 ; point x to random file buffer                                           
               leay    d,x                      ; point y to end of random file buffer                                              
               pshs    y,x,b,a                  ; save pointers on stack                                    
               leay    ,s                       ; point y current stack pointer                                      
               ldu     BasVarSimpleAddr         ; get start of variables                                  
lcb41          cmpu    BasVarArrayAddr          ; compare to start of arrays                                            
               beq     lcb54                    ; branch if all variables checked                                          

               lda     $01,u                    ; get 2nd byte of variable name                                        
               leau    $02,u                    ; move pointer to start of descriptor                                               
               bpl     lcb4e                    ; branch if variable - numeric                                       
               bsr     lcb76                    ; adjust string variable if in random file buffer                                                          

lcb4e          leau    $05,u                    ; move pointer to next variable                                              
               bra     lcb41                    ; process another variable                                   
lcb52          puls    u                        ; get address of next array to u                                           
lcb54          cmpu    BasVarEnd                ; compare to end of arrays                                          
               beq     lcb93                    ; branch if end of arrays                                  

               tfr     u,d                      ; save array start in accd, add offset                                             
               addd    $02,u                    ; to next array and save address of                                             
               pshs    b,a                      ; next array on the stack                                 
               lda     $01,u                    ; get 2nd letter of variable name                                          
               bpl     lcb52                    ; branch if numeric                            
               ldb     $04,u                    ; get the number of dimensions                                       
               aslb                             ; x2:2 bytes per dimension                               
               addb    #$05                     ; 5 bytes constant per array descriptor                                                
               clra                             ; clear msb of offset - (only 125 dimensions allowed)                                                          
               leau    d,u                      ; point u to start of this array's variables                                                    
lcb6b          cmpu    ,s                       ; at end of this array?                                   
               beq     lcb52                    ; yes              

               bsr     lcb76                    ; adjust string variable if in random file buffer                                                          

               leau    $05,u                    ; move pointer to next descriptor                                           
               bra     lcb6b                    ; check next variable                              
;
; check to see if a string is located in the random file buffer area. if it is
; the random file buffer in question, it will be deleted. if it is higher in the random
; file buffer space than the buffer in question, the length of the current
; buffer will be subtracted from the address of the string because the current
; buffer is being deleted (closed).
lcb76          ldx     $02,u                    ; point x to start of string                                          
               cmpx    rnbfad                   ; compare to start of free random file buffer area                                                             
               bhs     lcb8b                    ; return if > start of free random file buffer area                                                            

               cmpx    $02,y                    ; compare to start of this file's random buffer                                                         
               blo     lcb8b                    ; return if < start of this file's random buffer                                                         

               cmpx    $04,y                    ; compare to end of this file's random buffer                                                       
               blo     lcb8c                    ; return if < end of this file's random buffer                                                       

               tfr     x,d                      ; save pointer in accd                             
               subd    ,y                       ; subtract record length from start of string address                                                            
               std     $02,u                    ; save new start of string address                                           
lcb8b          rts                                          

lcb8c          clr     ,u                       ; clear the length of the string                                           
               clr     $02,u                    ; clear the address                            
               clr     $03,u                    ; of the string                        
               rts                                     

; remove reserved space in random file buffer for a 'closed' random file
; adjust the start of random file buffer pointer in all random fcbs
lcb93          ldb     fcbact                   ; get the number of active files                                               
               incb                             ; add one              
lcb97          pshs    b                        ; save files count on the stack                                          
               jsr     >lc749                   ; point x to fcb                         

               lda     fcbtyp,x                 ; get file type                           
               cmpa    #ranfil                  ; is it a random file?                                  
               bne     lcbad                    ; branch if not                        

               ldd     fcbbuf,x                 ; get start of this file's random file buffer                                                         
               cmpd    $04,y                    ; compare to end of random file buffer area and                                                         
               blo     lcbad                    ; branch if < end of random file buffer area                                                     

               subd    ,y                       ; = subtract record length of selected file                                                  
               std     fcbbuf,x                 ; = save new start of random file buffer                                                    
lcbad          puls    b                        ; get the files counter                                  
               decb                             ; decrement files counter                              
               bne     lcb97                    ; branch if all files not done                                       

               puls    a,b,x,u                  ; u = end of random file buffer, x = start of random                                                                
; file buffer, accd = record length

; this would probably be the most convenient place to fix the bug which
; causes the system to hang if an error is encountered during 'copy'

;        cmpu fcbadr                               ; is the end of this fcb's buffer above the end
;                                                  ; of the start of the fcb area
;        blo     lcbb4                             no - free up the space used by this file in random buffer
;        ldx     #dflbuf                           yes - doing a 'copy'; reset start of random buffer
;        bra     lcbc0
; random file buffer area
; remove reserved space for closed file from random file buffer space

lcbb4          cmpu    rnbfad                   ; at the bottom of free random buffer area?                                                           
               beq     lcbc0                    ; branch if there                          

               lda     ,u+                      ; = grab a source byte and                                 
               sta     ,x+                      ; = move it to destination                                 
               bra     lcbb4                    ; keep moving bytes                            
lcbc0          stx     rnbfad                   ; save new start of free random buffer area                                                          
lcbc3          jsr     >lc755                   ; point x to proper file allocation table                                                       

               dec     fat0,x                   ; remove one active file                                  
               tst     fat1,x                   ; new data in fat ram image?                                      
               beq     lcbcf                    ; no             

               jsr     >lc71e                   ; write out file allocation table to disk                                                  

lcbcf          ldx     fcbtmp                   ; get file buffer pointer                                        
               puls    a                        ; get file type                     
               cmpa    #outfil                  ; is it a sequential output file?                                             
               beq     lcbdf                    ; yes              

               cmpa    #ranfil                  ; is it a random file?                                  
               bne     lcb8b                    ; return if not a random file (sequential input)                                                         

               lda     fcbflg,x                 ; test the get/put flag and                                       
               beq     lcbe9                    ; branch if 'get'                          


; write contents of file buffer to disk
lcbdf          jsr     >lc763                   ; get proper track & sector numbers                                                 

               leau    fcbcon,x                 ; point u to start of fcb data                                           
               stu     dcbpt                    ; set up file buffer pointer for dskcon                                                
               bsr     lcc15                    ; go write a sector                            

lcbe9          lda     fcblst,x                 ; check the pre-saved flag                                           

               bpl     lcb8b                    ; return if record has already been saved on disk                                                          
               ldb     fcbdir,x                 ; get directory number of this file                                               
               andb    #$07                     ; 8 entries per sector                               
               lda     #dirlen                  ; dirlen bytes per directory entry                                             
               mul                              ; get sector offset for this entry                                      
               ldu     #dbuf0                   ; get read/write buffer 0 and                                       
               stu     dcbpt                    ; save it in dskcon register                                     
               leay    d,u                      ; y points to correct directory entry                                             
               ldb     fcbdir,x                 ; get directory entry number                                        
               lsrb                             ;      
               lsrb                             ;      
               lsrb                             ; divide by 8; eight directory entries per sector                                                      
               addb    #$03                     ; add bias; first 3 sectors not directory                                                  
               stb     dsec                     ; store sector number                             
               ldd     #$1102                   ; directory track - read op code                                          
               sta     dctrk                    ; store track number                             
               bsr     lcc17                    ; go read directory                            

               ldd     fcblst,x                 ; get number of bytes in the last sector                                                    

               anda    #$7f                     ; mask off the pre-saved flag                                      
               std     dirlst,y                 ; save number of bytes in last sector of file in directory                                                                       
lcc15          ldb     #$03                     ; write op code                            
lcc17          stb     dcopc                    ; save dskcon op code variable                                            
               jmp     >DoDiskReadWrite                   ; go read/write sector                               


; console out ram hook
DosHookCharOut tst     TextDevN                 ; check device number                                    
               lble    CoCoVec167               ; branch to ex basic if not a disk file                                                 
               leas    $02,s                    ; pop return off stack                                
; send a character in acca to a disk file. a carriage return will reset the
; print position and control codes will not increment the print position.
lcc24          pshs    x,b,a                    ; save registers                               
               ldx     #fcbv1-2                 ; point x to table of file number vectors                                                     
               ldb     TextDevN                 ; get current file number                                   
               aslb                             ; 2 bytes per fcb address                              
               ldx     b,x                      ; point x to proper fcb                              
               ldb     fcbtyp,x                 ; get file type                           
               cmpb    #inpfil                  ; is it an input file?                                  
               beq     lcc6a                    ; return if so                       

               cmpa    #cr                      ; carriage return (enter)                                 
               bne     lcc3a                    ; no             

               clr     fcbpos,x                 ; clear print position if carriage return                                                     
lcc3a          cmpa    #space                   ;                 
               blo     lcc40                    ;branch if control char                                

               inc     fcbpos,x                 ; increment print position                                      
lcc40          cmpb    #ranfil                  ; is it random file?                                     
               bne     lcc5e                    ; branch if not random                               

; put a byte into a random file
               ldd     fcbput,x                 ; get 'put' byte counter                                    
               addd    #$0001                   ; add one                    
               cmpd    fcbrln,x                 ; compare to record length                                       
               lbhi    lcdcb                    ; 'fr' error if 'put' byte counter > record length                                                            
               std     fcbput,x                 ; save new 'put' byte counter                                         
               ldx     fcbbuf,x                 ; point to random file buffer pointer                                                 
               leax    d,x                      ; point to one past end of current record data                                                      
               puls    a                        ; pull data from stack                            
               sta     -1,x                     ; store in data buffer                              
               puls    b,x,pc                   ; restore registers and return                                         

; write a byte to sequential output file
lcc5e          inc     fcblft,x                 ; increment character count                                            
               ldb     fcblft,x                 ; get character count and branch                                            
               beq     lcc6c                    ; if the buffer is full                                

               abx                              ; add character count to fcb address                                        
               sta     fcbcon-1,x               ; store new character (skip past 25 control bytes at fcb start)                                                                             
lcc6a          puls    a,b,x,pc                                      

; write out a full buffer and reset buffer
lcc6c          pshs    u,y                      ; save registers                             
               sta     seclen+fcbcon-1,x        ; store last character in buffer                                                     
               ldb     fcbdrv,x                 ; get drive number and save                                       
               stb     dcdrv                    ; it in dskcon control table                                     
               inc     fcbsec,x                 ; increment sector number                                     
               jsr     >lcbdf                   ; write the file buffer to disk                                        

               leay    ,x                       ; save fcb pointer in y                              
               ldb     fcbcgr,x                 ; get granule number                                
               jsr     >lc755                   ; point x to proper allocation table                                             

               abx                              ; add the granule number to fat pointer                                           
               leau    fatcon,x                 ; point u to the correct granule in fat - skip past                                                                
; the six fat control bytes
               lda     fcbsec,y                 ; get current sector for this granule                                                 
               cmpa    #$09                     ; max sector number (9 sectors/granule)                                                
               blo     lcc99                    ; branch if not at end of granule                                          

               dec     fcbsec,y                 ; decrement sector number and increment error flag in                                                                
               inc     fcbcpt,y                 ; case error found while looking for next granule -                                                              
                                                ; the error flag is used to indicate that another sector
                                                ; must be added to the length of file following error processing.
               jsr     >lc7bf                   ; get next free granule                                

               clr     fcbsec,y                 ;clear sector number and                                    
               clr     fcbcpt,y                 ;error flag - disk was not full                                           
               sta     fcbcgr,y                 ; save new granule in fcb                                     
               fcb      Skip2                   ; cmpx
lcc99          ora     #$c0                     ; force granule number to be final granule in file                                                               
               sta     ,u                       ; store in map                    
               leax    ,y                       ; point x to fcb                       
               jsr     >lc685                   ; increment record number                                  

               jsr     >lc5a9                   ; update file allocation table                                       

               puls    y,u                      ; restore registers                           
               puls    a,b,x,pc                 ; restore registers and return                                           

; dir command
CmdDir         jsr     >GetDriveNoInB           ; scan drive number from input line                                               
               jsr     >GetFAT                  ; get fat for this drive                                 
               jsr     TextOutCRLF              ; print carriage return to console out                                               

               ldd     #$1102                   ; get track 17 and                            
               sta     dctrk                    ; read op code and                           
               stb     dcopc                    ; save in dskcon variables                                   
               ldb     #$03                     ; start with sector 3 (first directory sector)                                                      

; read a directory sector into the i/o buffer
lccbb          stb     dsec                     ; save sector number in dskcon variable                                                    
               ldx     #dbuf0                   ; use i/o buffer 0 for data transfer                                              
               stx     dcbpt                    ; save in dskcon variable                                  
               jsr     >DoDiskReadWrite                   ; read a sector                        


; send directory information to console out
lccc5          puls    u                        ; save top of stack                              
               jsr     la549                    ; go do a break check                              

               pshs    u                        ; restore stack                     
               lda     dirnam,x                 ; test file name first byte                                       
               beq     lcd08                    ; branch if killed                           

               coma                             ; ff = end of directory                            
               beq     lcd17                    ; return if end of directory                                     

               pshs    x                        ; save directory pointer on stack                                       
               ldb     #$08                     ; number characters to print                                    
               jsr     lb9a2                    ; send filename to console out                                       

               bsr     lcd1b                    ; send blank to console out                                    

               ldb     #$03                     ; number characters to print                                    
               jsr     lb9a2                    ; send extension to console out                                        

               bsr     lcd1b                    ; send blank to console out                                    

               ldb     fcbtyp,x                 ; get file type                           
               cmpb    #10                      ; check the number of decimal digits in                                               
               bhs     lcceb                    ; accb: if there is only one digit,                                            

               bsr     lcd1b                    ; send blank to console out                                    

lcceb          clra                             ; clear ms byte of acco                                 
               jsr     TextOutNum16             ; print accd in decimal to console out                                               

               bsr     lcd1b                    ; send blank to console out                                    

               ldx     ,s                       ; x now points to directory entry                                       
               lda     #'B                      ; ascii bias                      
               adda    dirasc,x                 ; add to ascii flag                                
               bsr     lcd18                    ; print character and blank to console out                                                   

               ldb     dirgrn,x                 ; get first granule in file                                       
               bsr     lcd1e                    ; count granules                         

               tfr     a,b                      ; save count in accb                           
               clra                             ; clear ms byte of accd                            
               jsr     TextOutNum16             ; print acco in decimal to console out                                               

               jsr     TextOutCRLF              ; send carriage return to console out                                              

               puls    x                        ; pull directory pointer off of the stack                                               
lcd08          leax    dirlen,x                 ; move x to next directory entry                                                  
               cmpx    #dbuf0+seclen            ; end of i/o buffer?                                      
               blo     lccc5                    ; branch if more directory entries in buffer                                                     

               ldb     dsec                     ; get current sector                            
               incb                             ; bump count                 
               cmpb    #secmax                  ; secmax sectors in directory track                                               
               bls     lccbb                    ; get next sector                          

lcd17          rts                              ; finished                   

lcd18          jsr     TextOutChar              ; send character to console out                                             

lcd1b          jmp     TextOutSpace             ; send blank to console out                                         


; enter with accb pointing to first granule in a file; return the number of
; granules in the file in acca, the granule data for the last sector in accb
lcd1e          jsr     >lc755                   ; point x to file allocation buffer                                                 

               leau    fatcon,x                 ; point u to start of granule data                                               
               clra                             ; reset granule counter                            
lcd24          inca                             ; increment granule counter                                     
               cmpa    #granmx                  ; checked all 68 granules?                                      
               lbhi    lc653                    ; yes - 'bad file structure' error                                            
               leax    ,u                       ; point u to start of granule data                                         
               abx                              ; add pointer to first granule                                  
               ldb     ,x                       ; get this granule's control byte                                       
               cmpb    #$c0                     ; is this the last granule in file?                                            
               blo     lcd24                    ; no - keep going                          

               rts                                     


; input ram hook
DosHookInput         tst     TextDevN                 ; check device number and return                                                
               ble     lcd97                    ; if not a disk file                             
               ldx     #lb069                   ; = change the return address on the stack to re-enter basic's input                                                                              
               stx     ,s                       ; = routine at a different place than the calling routine                                                               
               ldx     #BasLinInpBuff+1         ; point x to the line input buffer                                               
               ldb     #',                      ; =           
               stb     BasDelim1                ; =comma is read item separator (temporary string search flag)                                                                        
               lda     BasVarType               ; get variable type and branch if                                           
               bne     lcd4b                    ; it is a string                         

               ldb     #space                   ; space = numeric search delimiter                                            
lcd4b          bsr     lcdbc                    ; get an input character                                      

               cmpa    #space                   ; space?                   
               beq     lcd4b                    ; yes - get another character                                      

               cmpa    #'"                      ; quote?                 
               bne     lcd5f                    ; no             

               cmpb    #',                      ; search character = comma?                                    
               bne     lcd5f                    ; no - numeric search                              

               tfr     a,b                      ; save double quote as                             
               stb     BasDelim1                ; the search flag                           
               bra     lcd81                    ; save double quotes as first item in buffer                                                     

lcd5f          cmpb    #'"                      ;               
               beq     lcd74                    ;branch if inputting a string variable                                               

               cmpa    #cr                      ; is the input character a carriage return                                                  
               bne     lcd74                    ; no             

               cmpx    #BasLinInpBuff+1         ;if at the start of inputbuffer, check for a                                                          
               beq     lcdb0                    ;following line feed and exit routine                                              

               lda     -1,x                     ; =if the input character preceeding the cr was a line feed,                                                                    
               cmpa    #lf                      ; =then insert the cr in the input string, otherwise                                                            
               bne     lcdb0                    ; =check for a following line feed and exit the routine                                                                

               lda     #cr                      ; restore carriage return as the input character                                                       
lcd74          tsta                             ;check for a null (zero) input character and                                                      
               beq     lcd8e                    ;ignore it if lt is a null                                   

               cmpa    BasDelim1                ; =              
               beq     lcd98                    ; =check to see if the input character matches                                                       

               pshs    b                        ; =either accb or charac and if it does, then                                                   
               cmpa    ,s+                      ; =branch to check for item separator or                                                
               beq     lcd98                    ; =terminator sequence and exit routine                                                

lcd81          sta     ,x+                      ; store new character in buffer                                           
               cmpx    #BasLinInpBuff+lbufmx    ; end of input buffer                                        
               bne     lcd8e                    ; no             

               bsr     lcdd0                    ; get a character from console in                                          

               bne     lcd92                    ; exit routine if buffer empty                                       

               bra     lcdac                    ; check for cr or cr/lf and exit routine                                                 

lcd8e          bsr     lcdd0                    ; get a character from console in                                               

               beq     lcd5f                    ; branch if buffer not empty                                     

lcd92          clr     ,x                       ; put a zero at end of buffer when done                                                  
               ldx     #BasLinInpBuff           ; point (x) to linbuf - reset pointer                                                
lcd97          rts                                          


; check for item separator or terminator and exit the input routine
lcd98          cmpa    #'"                      ; quote?                      
               beq     lcda0                    ; yes              

               cmpa    #space                   ; space?                   
               bne     lcd92                    ; no - exit routine                            

lcda0          bsr     lcdd0                    ; get a character from console in                                          

               bne     lcd92                    ; exit routine if buffer empty                                       

               cmpa    #space                   ; space?                   
               beq     lcda0                    ; yes - get another character                                      

               cmpa    #',                      ; comma (item separator)?                                  
               beq     lcd92                    ; yes - exit routine                             

lcdac          cmpa    #cr                      ; carriage return?                               
               bne     lcdb8                    ; no             

lcdb0          bsr     lcdd0                    ; get a character from console in                                          

               bne     lcd92                    ; exit routine if buffer empty                                       

               cmpa    #lf                      ; line feed? treat cr,lf as a cr                                        
               beq     lcd92                    ; yes - exit routine                             

lcdb8          bsr     lcdd6                    ; back up ptr input pointer one                                             

               bra     lcd92                    ; exit routine                       

lcdbc          bsr     lcdd0                    ; get a char from input buffer - return in acca                                                             

               beq     lcdd5                    ; return if buffer not empty                                     

               jsr     >lc744                   ; point x to start of file buffer                                          

               ldb     fcbtyp,x                 ; get file type                           
               cmpb    #ranfil                  ; is it random file type?                                     
               lbne    lc352                    ; 'input past end of file' error if not random                                                        

lcdcb          ldb     #2*37                    ; 'write/input past end of record' error if random                                                                                            
               jmp     SysErr                   ; jump to the error handler                                    


lcdd0          jsr     la176                    ; get a char from input buffer                                            

               tst     cinbfl                   ; set flags according to console input flag                                                     
lcdd5          rts                                          


; move the input pointer back one (disk file)
lcdd6          pshs    x,b                      ; save registers on stack                                      
               jsr     >lc744                   ; point x to proper fcb                                

               ldb     fcbtyp,x                 ; get file type of this fcb                                       
               cmpb    #ranfil                  ; is it a random file?                                  
               bne     lcdec                    ; branch if not a random file                                      

               ldd     fcbget,x                 ;grab the random file 'get' pointer,                                                
               subd    #$0001                   ;move it back one and restore it                                           
               std     fcbget,x                 ;             
               puls    b,x,pc                   ; restore registers and return                                         
lcdec          sta     fcbcdt,x                 ; save the character in the cache                                                  
               com     fcbcfl,x                 ; set the cache flag to $ff - data in cache                                                       
               puls    b,x,pc                   ; restore registers and return                                         

; cvn command
FuncCvn            jsr     BasGetStrLenAddr         ; get length and address of string                                              

               cmpb    #$05                     ; five bytes in a floating point number                                                
               lbcs    BasFCError               ; 'fc' error if <> 5 bytes                                    

               clr     BasVarType               ; set variable type to numeric                                        
               jmp     lbc14                    ; copy a packed fp number from (x) to fpa0                                                   


; mkn$ command
FuncMkn            jsr     VarGetExprCC             ; 'tm' error if BasVarType=string                                         

               ldb     #$05                     ; five bytes in a floating point number                                               
               jsr     BasResStr                ; reserve five bytes in string space                                             

               jsr     lbc35                    ; pack fpa0 and store it in string space                                                 

               jmp     lb69b                    ; save string descriptor on string stack                                                 


; loc command
FuncLoc            bsr     lce19                    ; point x to file buffer                                    

               ldd     fcbrec,x                 ; get record number (random file) or sector ctr (sequential)                                                                        
lce14          std     BasVarFPAcc1+3           ;save accd in bottom 2 bytes of fpa0 and                                                       
               jmp     VarAssign16BitB          ;convert to floating point number                                          


; strip a device number from a basic statement, set print
; parameters according to it - error if file not
; open. return with (x) pointing to that file's fcb
lce19          lda     TextDevN                 ; get current device number and                                              
               pshs    a                        ; save it on the stack                            
               jsr     VarGetExprCC             ; 'tm' error if BasVarType=string                                      

               jsr     la5ae                    ; check for valid device number/set print parameters                                                             

               tst     TextDevN                 ; check device number                               
               lble    BasFCError               ; branch if not disk file 'illegal function call'                                                           
               jsr     >lc744                   ; point (x) to file buffer                                   

               puls    a                        ; get old device number off of the stack and                                                  
               sta     TextDevN                 ; save it as device number                                    
               tst     fcbtyp,x                 ; is file open?                           
               lbeq    la3fb                    ; 'file not open' error if not open                                             

               rts                                     


; lof
FuncLof            bsr     lce19                    ; point x to file buffer                                    

               lda     fcbdrv,x                 ; get drive number and save it                                          
               sta     dcdrv                    ; in dskcon variable                             
               ldb     fcbfgr,x                 ; get first granule of file                                       
               pshs    x                        ; save fcb pointer on stack                                 
               jsr     >lcd1e                   ; find total number of granules in this file                                                     

               deca                             ; subtract the last granule in the file                                            
               andb    #$3f                     ; get number of sectors used in last granule                                                     
               pshs    b                        ; save number of sectors in last granule on stack                                                       
               tfr     a,b                      ; convert acca to positive                                 
               clra                             ; 2 byte value in accd                           
               jsr     >lc779                   ; mult number of full granules by 9                                            

               addb    ,s+                      ; add number sectors in last track                                          
               adca    #$00                     ; propagate carry to ms byte of accd                                             
               puls    x                        ; get fcb pointer back                            
               pshs    a                        ; save acca on stack                          
               lda     fcbtyp,x                 ; get file type of this fcb and                                           
               cmpa    #ranfil                  ; check to see if it's a random file                                                
               puls    a                        ; restore acca                    
               bne     lce14                    ; if not a random file, then the total number of sectors in the file                                                                             

													                                   ; is the length of the file

; calculate lof for a random file - the length of a random file is the
; number of records in the file.
               pshs    x                        ; save fcb pointer on stack                                 
               subd    Misc16BitScratch         ; subtract zero from accd (number of sectors)                                                      
               beq     lce68                    ; branch if zero sectors                                 

               subd    #$0001                   ; subtract one sector - the last sector may not be iooz used                                                                       
lce68          bsr     lce14                    ; put accd into fpa0                                  

               ldb     BasVarFPAcc1             ; get exponent of fpa0                                
               beq     lce72                    ; branch if fpa0 = 0                             

               addb    #$08                     ; add 8 to exponent (multiply fpa0 by                                              
               stb     BasVarFPAcc1             ; 256 bytes/sector) and save new exponent                                                   
lce72          jsr     lbc5f                    ; save number of bytes in full sectors in fpa1                                                            

               ldx     ,s                       ; point x to fcb                      
               ldd     fcblst,x                 ; get number of bytes in last sector                                                

               anda    #$7f                     ; mask off the pre-saved byte                                      
               bsr     lce14                    ; put number bytes in last sector into fpa0                                                    

               clr     ressgn                   ; force sum sign = positive                                     
               lda     BasVarFPAcc2             ; get exponents of fpa0 and                                     
               ldb     BasVarFPAcc1             ; fpa1 prior to addition                                  
               jsr     CmdPlus                  ; add number bytes in last sector to number of                                                       

; bytes in full sectors
               jsr     lbc5f                    ; save total number of bytes in fpa1                                             

               puls    x                        ; point x to fcb                      
               ldd     fcbrln,x                 ; get record length                               
               bsr     lce14                    ; put it into fpa0                           

               clr     ressgn                   ; force quotient sign = positive                                          
               lda     BasVarFPAcc2             ; get exponents of fpa0 and                                     
               ldb     BasVarFPAcc1             ; fpa1 prior to division                                  
               jsr     CmdDivide                ; divide total number of bytes by number of bytes in a record                                                                      

               jmp     CmdINT                   ; convert fpa0 to an integer                                   


; free command
FuncFree           jsr     VarGetExprCC             ; number type check                                

               jsr     lb70e                    ;evaluate numeric expression and return value in accb                                                              

               cmpb    #$03                     ; only 4 legal drives                              
               lbhi    la61f                    ; 'device number' error if drive number is > 3                                                        
               stb     dcdrv                    ; save in drive number                               
               jsr     >GetFAT                   ; get file allocation table and store in buffer                                                        

               jsr     >lc755                   ; point x to start of file allocation table buffer                                                           

               leax    fatcon,x                 ; move to first granule data byte                                              
               clr     ,-s                      ; space for free granule counter                                       
               ldb     #granmx                  ; get maximum number of granules                                           
lceb6          lda     ,x+                      ; get granule data                              
               coma                             ;free granules $ff                       
               bne     lcebd                    ;branch if not free                            

               inc     ,s                       ; increment free granule counter                                      
lcebd          decb                             ; decrement granule counter                                     
               bne     lceb6                    ; branch if not done                             

               puls    b                        ; get free granule counter to accb                                        
               jmp     VarAssign16Bit2          ; load accb into fpa0                              


; drive command
CmdDrive          jsr     VarGet8Bit               ; evaluate expr; return value in accb                                                      

               cmpb    #$03                     ; max drive number = 3                               
               lbhi    la61f                    ; 'device #' error if drive number > 3                                                
               stb     defdrv                   ; save default drive number                                     
               rts                                     


; evaluate expression ram vector
DosHookEvaluate         lda     $04,s                    ; = check stacked precedence flag and if it is not an end                                                                        
               bne     lcee9                    ; = of operation, branch to extended basic's expression                                                                

													                                   ; = evaluation routine
               ldx     $05,s                    ;          
               cmpx    #laf9a                   ;            
               bne     lcee9                    ; check two return addresses back on the stack                                                       

               ldx     $02,s                    ; to see if the call to evaluate expression is                                                       
               cmpx    #lb166                   ; coming from the 'let' command - branch out if                                                          
               bne     lcee9                    ; not coming from 'let'                                

               ldx     #lceec                   ; = if coming from 'let', replace the return addr                                                           
               stx     $05,s                    ; = with the disk basic 'let' modifier address                                                       
lcee9          jmp     CoCoVect18B              ; extended basic expression evaluation                                                     


; let modifier
lceec          puls    a                        ; pull variable type off of the stack                                           
               rora                             ; set carry if siring, clear carry if numeric                                                  
               jsr     lb148                    ; do a 'tm' check                          

               lbeq    lbc33                    ; if numeric variable, pack fpa0 into vardes                                                      

               ldx     BasVarFPAcc1+3           ; point x to string descriptor                                        
               ldd     $02,x                    ; get address of siring                                
               cmpd    #dflbuf                  ; compare to start of random file buffers                                                     
               blo     lcf07                    ; and branch if lower                              

               subd    fcbadr                   ; subtract out the end of random file buffers                                                        
               lbcs    lafb1                    ; branch if string stored in random file buffer -                                                           

; move it into the string space
lcf07          jmp     lafa4                    ; branch back to basic's 'let' command                                                    


;modifier for exbas command interpretation handler
dxcvec         cmpa    #$ca                     ; token for dload?                                 
               beq     lcf2a                    ; yes              

               cmpa    #$c8                     ; token for pmode?                           
               lbne    l813c                    ; no              

; disk basic modifier for pmode - allows for the ram the dos uses
               jsr     BasChrGet                ; get next character from basic                                         

               cmpa    #',                      ; check for comma                          
               lbeq    l9650                    ; branch if comma                           

               jsr     VarGet8Bit               ; evaluate expression; return value in accb                                                       

               cmpb    #$04                     ; check for pmode 4                            
               lbhi    BasFCError               ; 'fc' error if pmode > 4                                   
               lda     GrStartPages             ; number blocks before graphics pages                                               
               jmp     l962e                    ; jump to exeas' pmode command                                       


; disk basic dload modifier
lcf2a          jsr     CasClosFiles             ; close files                           

               jsr     BasChrGet                ; get next character from basic                                         

               jmp     l8c1b                    ; jump to exeas' dload                               


dxivec         cmpb    #($9a-$80)*2             ; modified token for pos                                                                     
               lbne    l8168                    ; if not pos, go to exbas secondary comm handler                                                          

               jsr     lb262                    ; syntax check for '(' and evaluate expression                                                       

               lda     TextDevN                 ; get device number and                                 
               pshs    a                        ; save it on stack                        
               jsr     la5ae                    ; evaluate device number                                 

               jsr     la406                    ; test device number                             

               tst     TextDevN                 ; check device number and branch                                          
               ble     lcf5c                    ; if not a disk file                             
               jsr     >lc744                   ; point x to fcb                         

               ldb     fcbtyp,x                 ; get file type                           
               cmpb    #ranfil                  ; direct/random file?                                 
               bne     lcf5c                    ; branch if not a random file                                      

               puls    a                        ; restore device number                             
               sta     TextDevN                 ;           
               ldd     fcbput,x                 ; =grab the 'put' data item counter and convert                                                           
               jmp     givabf                   ; =it to a floating point number                                          

lcf5c          jsr     la35f                    ; set print parameters                                    

               puls    a                        ; restore device number                             
               sta     TextDevN                 ;           
               ldb     TextVDUCurrCol           ; =get print position and                                   
               jmp     VarAssign16Bit2          ; =convert it to floating point number in fpa0                                                       


; savem command
lcf68          jsr     BasChrGet                ; get next input character                                          

               bsr     lcfbb                    ; get filename, etc.                             

               jsr     l836c                    ; evaluate expression, put ii (2 bytes) on stack                                                         

               jsr     l836c                    ; ditto                

               cmpx    $02,s                    ; compare end address to start address                                                
               lbcs    BasFCError               ; if start > end, then 'illegal function call'                                                        

               jsr     l836c                    ; eval expression (transfer address), put on stack                                                           

               jsr     la5c7                    ; syntax error if any more chars on this line                                                      

               ldd     #$0200                   ; file type=2, ascii flag = crunched (0)                                                  
               std     dfltyp                   ;           
               jsr     >lca04                   ; get next unopen file and initialize fcb                                                  

               clra                             ;zero flag - first byte of preamble                                        
               bsr     lcfb5                    ;write a byte to buffer                                

               ldd     $02,s                    ; get end address                          
               subd    $04,s                    ; subtract the start address                                      
               addd    #$0001                   ; the saved data block will include both the first and last bytes                                                                            
               tfr     d,y                      ; save length in y                         
               bsr     lcfb3                    ; write file length to buffer - first argument of preamble                                                                   

               ldd     $04,s                    ; get the start address                                
               bsr     lcfb3                    ; write out the start address - second preamble argument                                                                 

               ldx     $04,s                    ; get start address                            
lcf9b          lda     ,x+                      ; grab a byte                         
               jsr     >lcc24                   ; write it out                       

               leay    -1,y                     ; decrement byte counter                                 
               bne     lcf9b                    ; branch if all bytes not done                                       

               lda     #$ff                     ; first byte of postamble                                 
               bsr     lcfb5                    ; write it out - eof record                                    

               clra                             ; first argument of postamble is                                     
               clrb                             ; a dummy - zero value                           
               bsr     lcfb3                    ; write out postamble first argument                                             

               puls    a,b,x,y                  ; get control addresses from the stack                                                  
               bsr     lcfb3                    ; write out the transfer address - 2nd argument                                                        

               jmp     la42d                    ; go close all files                             


; write accd to the buffer
lcfb3          bsr     lcfb5                    ; write acca to buffer, then swap acca,accb                                                         

lcfb5          jsr     >lcc24                   ; write acca to buffer                                    

               exg     a,b                      ; swap acca,accb                       
               rts                                     

lcfbb          ldx     #binext                  ; point to .bin extension                                         
               jmp     >lc938                   ; get filename, etc.                             


; loadm command
lcfc1          jsr     BasChrGet                ; get next input character                                         

               bsr     lcfbb                    ; get filename, etc.                             

               jsr     >lca07                   ; open next available file for input                                             

               ldd     dfltyp                   ; get file type and ascii flag                                        
               subd    #$0200                   ; for loadm file: type=2, ascii flag=0                                                 
               lbne    BasFMError               ; 'bad file mode' error                                 

               ldx     Misc16BitScratch         ; zero out x reg - default value of offset                                                  
               jsr     BasChrGetCurr            ; get current character from basic                                            

               beq     lcfde                    ; branch if end of line - no offset                                            

               jsr     VarCKComma               ; syntax check for comma                                    

               jsr     VarGet16Bit              ; evaluate expression                              

lcfde          stx     vd3                      ; store offset in vd3                                 
               jsr     la5c7                    ; syntax error if other characters on line                                                   


; get preamble/postamble
lcfe3          jsr     >lcdbc                   ; get first byte                              

               pshs    a                        ; save it on the stack                            
               bsr     ld013                    ; get first argument                             

               tfr     d,y                      ; save it in y                     
               bsr     ld013                    ; get the second argument                                  

               addd    vd3                      ; add it to the offset                              
               std     BasExecAddr              ; store it in the jump address of the exec command                                                            
               tfr     d,x                      ; save it in x                     
               lda     ,s+                      ; get the first byte off of the stack                                            
               lbne    la42d                    ; close file if postamble (eof)                                         


; get record byte(s)
lcffa          jsr     >lc5c4                   ; get byte from buffer                                    

               ldb     cinbfl                   ; get status of console in buffer                                           
               beq     ld004                    ; branch if buffer not empty                                     

               jmp     >lc352                   ; 'input past end of file' error                                         

ld004          sta     ,x                       ; store byte in memory                                 
               cmpa    ,x+                      ;test to see if it stored properly and                                              
               beq     ld00d                    ;branch if proper store (not in rom or bad ram)                                                        

               jmp     >ld709                   ; 'i/o error' if bad store                                   

ld00d          leay    -1,y                     ; decrement byte count                                    
               bne     lcffa                    ; get next byte if not done                                    

               bra     lcfe3                    ; read another pre/post amble                                      
; read two bytes from buffer - return them in accd
ld013          bsr     ld015                    ; read a byte, save it in accb                                            

ld015          jsr     >lcdbc                   ; get a character from input buffer, return it in acca                                                                    

               exg     a,b                      ; swap acca,accb                       
               rts                                     


; rename command
CmdRename         ldx     BasAddrSigByte           ; save current input pointer                                            
               pshs    x                        ; on the stack                    
               bsr     ld056                    ; get filename of source file                                      

               lda     dcdrv                    ; save drive number                            
               pshs    a                        ; on the stack                    
               bsr     ld051                    ; syntax check for 'to' and get new filename                                                     

               puls    a                        ; get source drive number                               
               cmpa    dcdrv                    ; compare to new file drive number                                            
               lbne    BasFCError               ; 'fc' error if flies on different drives                                                   

               bsr     ld059                    ; verify that new file does not already exist                                                      

               puls    x                        ; restore input pointer                             
               stx     BasAddrSigByte           ;           
               bsr     ld056                    ; get source filename again                                    

               jsr     >lc68c                   ; scan directory for source filename                                             

               jsr     >lc6e5                   ; 'ne' error if not found                                  

               bsr     ld051                    ; syntax check for 'to' and get new filename                                                     

               ldx     #dnambf                  ; point x to filename                                
               ldu     v974                     ; point u to directory entry of source file                                                   
               ldb     #$0b                     ; 11 characters in filename and extension                                                 
               jsr     UtilCopyBXtoU            ; copy new filename to source file directory ram image                                                               

               ldb     #$03                     ; get write op code and                               
               stb     dcopc                    ; save in dskcon variable                                  
               jmp     >DoDiskReadWrite                   ; write new directory sector                                     


; do a syntax check for 'to' and strip a filename from basic
ld051          ldb     #$a5                     ; 'to' token                         
               jsr     VarCKChar                ; syntax check for 'to'                                

ld056          jmp     >lc935                   ; get filename from basic                                       

ld059          jsr     >lc68c                   ; scan directory for filename                                           

               ldb     #33*2                    ; 'file already exists' error                                                                     
               tst     v973                     ; check for a match                           
               lbne    SysErr                   ; 'ae' error if file in directory                                           

               rts                                     


; write command
CmdWrite          lbeq    TextOutCRLF              ; print carriage return to console out if end of line                                                                    

               bsr     ld06f                    ; go write an item list                                

               clr     TextDevN                 ; set device number to screen                                       
ld06e          rts                                          

ld06f          cmpa    #'#                      ; check for device number flag                                            
               bne     ld082                    ; default to current device number if none given                                                         

               jsr     la5a5                    ; set device number; check validity                                            

               jsr     la406                    ; make sure selected file is an output file                                                    

               jsr     BasChrGetCurr            ; get current input character                                       

               lbeq    TextOutCRLF              ; print cr to console out if end of line                                                  

ld07f          jsr     VarCKComma               ; syntax check for comma                                         

ld082          jsr     VarGetStr                ; evaluate expression                                   

               lda     BasVarType               ; get variable type                             
               bne     ld0a7                    ; branch if string                           

               jsr     lbdd9                    ; convert fp number to ascii string                                            

               jsr     lb516                    ; put on temporary string stack                                        

               jsr     lb99f                    ; print string to console out                                      


; print item separator to console out
ld092          jsr     BasChrGetCurr            ; get current character                                      

               lbeq    TextOutCRLF              ; put cr to console out if end of line                                                

               lda     #',                      ; comma: non-cassette separator                                       
               jsr     la35f                    ; set print parameters                               

               tst     CasIOFlag                ; get console print device and                                        
               beq     ld0a3                    ; branch if not cassette                                 

               lda     #cr                      ; get carriage return - cassette item separator                                                      
ld0a3          bsr     ld0b9                    ; send separator to console out                                             

               bra     ld07f                    ; get next item                        

; print a string to console out
ld0a7          bsr     ld0b0                    ; print leading string delimiter (")                                                  

               jsr     lb99f                    ; print string to console out                                      

               bsr     ld0b0                    ; print ending string delimiter (")                                            

               bra     ld092                    ; go print separator                             

; print string delimiter (") to console out
ld0b0          jsr     la35f                    ; set print parameters                                    

               tst     CasIOFlag                ; get console print device and                                        
               bne     ld06e                    ; return if cassette                             

               lda     #'"                      ; quote: non-cassette string delimiter                                              
ld0b9          jmp     TextOutChar              ; send to console out                                   


; field command
CmdField          jsr     >lc82e                   ; evaluate device number & verify random file open                                                                

               clra                             ;      
               clrb                             ; clear total field length counter                                       
               pshs    x,b,a                    ; save fcb pointer & initialize total field length to zero                                                                    
ld0c3          jsr     BasChrGetCurr            ; get current input character                                            

               bne     ld0c9                    ; branch if not end of line                                    

               puls    a,b,x,pc                 ; clean up stack and return                                        
ld0c9          jsr     VarGetComma8             ; syntax check for comma, evaluate expression                                                           

               pshs    x,b                      ; save field length (accb) on stack, x is a dummy which will                                                                    
; reserve 2 bytes for the address which will be calculated below
; at this point the stack will have the following information on it:
; ,s = field length   1 2,s = random file buffer address
; 3 4,s = total field length  5 6,s = fcd pointer
               clra                             ; clear ms byte                    
               addd    $03,s                    ; add field length to total field length counter                                                          
               blo     ld0da                    ; 'fo' error if sum > $ffff                                    

               ldx     $05,s                    ; point x to fcb                         
               cmpd    fcbrln,x                 ; compare to record length & branch if                                                   
               bls     ld0df                    ;total field length < record length                                            

ld0da          ldb     #34*2                    ; 'field overflow' error                                                                     
               jmp     SysErr                   ; jump to error driver                               

ld0df          ldu     $03,s                    ; load u with old total length of all fields                                                          
               std     $03,s                    ; save new total field length                                      
               ldd     fcbbuf,x                 ; point accd to start of random file buffer                                                       
               leau    d,u                      ;point u to this field's slot in the random                                                   
               stu     $01,s                    ;file buffer and save it on the stack                                              
               ldb     #$ff                     ; secondary token                         
               jsr     VarCKChar                ; syntax check for secondary token                                           

               ldb     #$a7                     ; 'as' token                    
               jsr     VarCKChar                ; syntax check for 'as' token                                      

               jsr     VarGetVar                ; evaluate variable                            

               jsr     VarGetExpr               ; 'tm' error if numeric variable                                         

               puls    b,u                      ; pull string address and length                                        
               stb     ,x                       ; off of the stack and save them                                      
               stu     $02,x                    ; in string descriptor                               
               bra     ld0c3                    ; check for another field specification                                                

; rset command
CmdRset
;              lda     #$4f                        ; skip one byte                           
                fcb     $86
; lset command
CmdLset           clra                             ; lset flag = 0                        
               pshs    a                        ; save rset($4f),lset(00) flag on the stack                                                 
               jsr     VarGetVar                ; evaluate field string variable                                         

               jsr     VarGetExpr               ; 'tm' error if numeric variable                                         

               pshs    x                        ; save string descriptor on stack                                       
               ldx     $02,x                    ; point x to address of string                                       
               cmpx    #dflbuf                  ; compare string address to start of random                                                       
               blo     ld119                    ; file buffer; 'se' error if < random file buffer                                                          

               cmpx    fcbadr                   ; = compare string address to top of random file buffer                                                                  
               blo     ld11e                    ; = area - branch if string in random file buffer                                                          

ld119          ldb     #2*35                    ; 'set to non-fielded string' error                                                                           
               jmp     SysErr                   ; jump to error handler                                

ld11e          ldb     #$b3                     ;              
               jsr     VarCKChar                ; syntax check for '=' token                                     

               jsr     l8748                    ; =evaluate data string expression; return with x                                                          

													                                   ; =pointing to string; accb = length
               puls    y                        ; point y to field string descriptor                                          
               lda     ,y                       ; get length of field string                                  
               beq     ld15a                    ; return if null string                                

               pshs    b                        ; save length of data string on stack                                           
               ldb     #space                   ; prepare to fill data string with blanks                                                   
               ldu     $02,y                    ; point u to field string address                                          
; fill the fielded string with blanks
ld132          stb     ,u+                      ; store a space in fielded string                                             
               deca                             ; decrement length counter                               
               bne     ld132                    ; keep filling w/spaces if not done                                            

               ldb     ,s+                      ;get the length of the data string and                                             
               beq     ld15a                    ;return if it is null (zero)                                     

               cmpb    ,y                       ; =compare length of data string to length of field                                                          
               blo     ld143                    ; =string, branch if field string > data string                                                        

               ldb     ,y                       ;get the length of the field string and force the                                                       
               clr     ,s                       ;rset/lset flag to lset (0) if data string length is                                                          
                                                ;>= the field string length. this will cause the right
                                                ;side of the data string to be truncated
ld143          ldu     $02,y                    ; load u with the address of the field string                                                           
               tst     ,s+                      ; get the rset/lset flag from the stack                                              
               beq     ld157                    ; and branch if lset                             

; rset routine
               pshs    b                        ; save the number of bytes to move into the field string                                                              
               clra                             ; = take the 2's complement of an unsigned                                               
               negb                             ; = number in accb - leave the double byte signed                                                      
               sbca    #$00                     ; = result in accd                           
               addb    ,y                       ; add the length of the field string to the inverse                                                          
               adca    #$00                     ; of the number of bytes to be moved                                             
               leau    d,u                      ; =add result to start of field string. now u                                                     
													                                   ; =will point to (-number of bytes to move)
													                                   ;=from the right side of the field string
               puls    b                        ; get the number of bytes to move                                       
ld157          jmp     UtilCopyBXtoU            ; move accb bytes from x to u (data to field string)                                                                  

ld15a          puls    a,pc                     ; pull lset/rset flag off of stack and return                                                           

; files command
CmdFiles          jsr     TextResetVDU             ; reset sam display page and vdg mode                                                   

               ldd     fcbadr                   ; get start of file buffers                                     
               subd    #dflbuf                  ; subtract the start of random file buffer space                                                            
               pshs    b,a                      ; save default value of random file buffer space on stack                                                                 
               ldb     fcbact                   ; get current number of fcbs                                      
               pshs    b                        ; and save on the stack (default value)                                             
               jsr     BasChrGetCurr            ; get current input char                                  

               cmpa    #',                      ; check for comma                          
               beq     ld181                    ; branch if comma - no buffer number parameter given                                                             

               jsr     VarGet8Bit               ; evaluate expression (buffer number)                                                 

               cmpb    #15                      ; 15 fcbs max                     
               lbhi    BasFCError               ; branch if > 15 - 'illegal function call'                                                    
               stb     ,s                       ; save number of fcbs on stack                                    
               jsr     BasChrGetCurr            ; check current input char                                    

               beq     ld189                    ; branch if end of line                                

ld181          jsr     VarCKComma               ; syntax check for comma                                         

               jsr     lb3e6                    ; evaluate expression, return value in accd                                                    

               std     $01,s                    ; save random file buffer size on stack                                                
ld189          jsr     DosCloseAllFiles                    ; close files                           

               ldb     ,s                       ; get the number of buffers to make and                                             
               pshs    b                        ; initialize a buffer counter on the stack                                                
               ldd     #dflbuf                  ; get start of random file buffers                                             
               addd    $02,s                    ; add the newly specified random file buffer space                                                            
               blo     ld208                    ; 'out of memory' error if > $ffff                                           

               std     $02,s                    ; save start of fcbs                             
													                                   ; reserve space for fcbs
ld199          addd    #fcblen                  ; fcblen required for each buffer                                                  
               blo     ld208                    ; 'out of memory' error if > $ffff                                           

               dec     ,s                       ; decrement buffer counter                                
               bpl     ld199                    ;branch if not done - the bpl will set up one more buffer                                                                  
                                                ;than the number requested. this extra buffer is the system buffer
                                                ;and is located at the end of the normal fcbs. only system routines
                                                ;(copy, backup, merge etc.) may access this buffer.
               tstb                             ; at an exact 256 byte boundary?                                     
               beq     ld1a8                    ; yes              

               inca                             ; no - add 256                   
               beq     ld208                    ; 'out of memory' error if past $ffff                                              

ld1a8          bita    #$01                     ; on a 512 byte boundary?                                       
               beq     ld1af                    ; yes              

               inca                             ; no - add 256                   
               beq     ld208                    ; 'om' error if past $ffff                                   

ld1af          sta     ,s                       ; save ms byte of new graphic ram start                                                  
               ldd     BasVarSimpleAddr         ; get start of variables                                  
               suba    GrStartPages             ;subtract the old graphic ram start - accd contains length                                                                     
                                                ;of program plus reserved graphic ram
               adda    ,s                       ; add in the amount of ram calculated above                                                  
               blo     ld208                    ; 'out of memory' error if > $ffff                                           

               tfr     d,x                      ; save new vartab in x                             
               inca                             ;add 256 - to guarantee enough room since all calculations use                                                                   
                                                ;only the msb of the address
               beq     ld208                    ; 'out of memory' error if past $ffff                                              

               cmpd    BasVarStringBase         ; is it greater than the start of string space                                                         
               bhs     ld208                    ; 'out of memory' if > start of string space                                                     

               deca                             ; subtract 256 - compensate for inca above                                               
               subd    BasVarSimpleAddr         ; subtract start of variables                                        
               addd    BasStartProg             ; add start of basic                               
               tfr     d,y                      ; y has new start of basic                                 
               lda     ,s                       ; get the graphic ram start, subtract                                           
               suba    GrStartPages             ; the old graphic ran start and save                                               
               tfr     a,b                      ; the difference in acca and accb                                        
               adda    GrDisplayStartAddr       ; = add the old graphic page start and                                                 
               sta     GrDisplayStartAddr       ; = store the new start of graphics ram                                                 
               addb    GrLastDisplayAddr        ; add the old graphic ram end address and                                                    
               stb     GrLastDisplayAddr        ; store the new end of graphics ram                                             
               puls    a,b,u                    ; = acca=msb of start of graphic ram; accb=number of file buffers                                                                           
													                                   ; = u=start of file buffers
               sta     GrStartPages             ; save new start of graphic ram                                         
               stb     fcbact                   ; number of file buffers                                  
               stu     fcbadr                   ; start of file buffers                                 
               lda     BasCurrentLine           ; get current line number                                   
               inca                             ; are we in direct mode?                             
               beq     ld1ef                    ; yes - move basic program                                   

               tfr     y,d                      ; move new start of basic to accd                                        
               subd    BasStartProg             ; subtract old start of basic                                        
               addd    BasAddrSigByte           ; add old input pointer                                  
               std     BasAddrSigByte           ; save new input pointer                                  
ld1ef          ldu     BasVarSimpleAddr         ; point u to old start of variables                                                  
               stx     BasVarSimpleAddr         ; save new start of varibles                                      
               cmpu    BasVarSimpleAddr         ; compare old start of variables to new start of                                                           
               bhi     ld20b                    ; variables & branch if old > new                                          
; move basic program if old start address <= new start address
ld1f8          lda     ,-u                      ; get a byte                        
               sta     ,-x                      ; move lt                
               cmpu    BasStartProg             ; at start of basic program?                                       
               bne     ld1f8                    ; no             

               sty     BasStartProg             ; store new start of basic program                                            
               clr     -1,y                     ; reset start of program flag                                     
               bra     ld21b                    ; close all files                          
ld208          jmp     BasOMError               ; 'out of memory' error                                     

; move basic program if old start address > new start address
ld20b          ldu     BasStartProg             ; point u to old start of basic                                              
               sty     BasStartProg             ; save new start of basic                                   
               clr     -1,y                     ; reset start of basic flag                                   
ld212          lda     ,u+                      ; get a byte                        
               sta     ,y+                      ; move it                
               cmpy    BasVarSimpleAddr         ; at start of variables                                  
               bne     ld212                    ; no - move another byte                                 


; close all fcbs and recalculate fcb start addresses
ld21b          ldu     #fcbv1                   ; point u to file buffer pointers                                                
               ldx     fcbadr                   ; point x to start of buffers                                       
               clrb                             ; reset file counter                         
ld222          stx     ,u++                     ; store file address in vector table                                                 
               clr     fcbtyp,x                 ; reset file type to closed                                       
               leax    fcblen,x                 ; go to next fcb                             
               incb                             ; increment file counter                             
               cmpb    fcbact                   ; close all active buffers and system fcb                                                    
               bls     ld222                    ; branch if not done                             

               jmp     l96cb                    ; readjust line numbers, etc.                                      


; unload command
CmdUnload      bsr     GetDriveNoInB            ; get drive number                                 

               clrb                             ; clear file counter                         
ld236          incb                             ; increment file counter                                  
               jsr     >lc749                   ; point x to fcb                         

               beq     ld249                    ; branch if file not open                                  

               lda     fcbdrv,x                 ; check drive number                                
               cmpa    dcdrv                    ; does it match the 'unload' drive number?                                                    
               bne     ld249                    ; no match - do not close the file                                           

               pshs    b                        ; save file counter on the stack                                      
               jsr     >lcb06                   ; close fcb                    

               puls    b                        ; restore file counter                            
ld249          cmpb    fcbact                   ; checked all files?                                    
               bls     ld236                    ; no             

               rts                                     

; get drive number from basic - use the default drive if none given
GetDriveNoInB  ldb     defdrv                   ; get default drive number                                         
               jsr     BasChrGetCurr            ; get next input char                               

               beq     ld25f                    ; use default drive number if none given                                                 

ld256          jsr     VarGet8Bit               ; evaluate expression                                      

               cmpb    #$03                     ; 4 drives max                       
               lbhi    la61f                    ; 'device number error' if > 3                                        
ld25f          stb     dcdrv                    ; store in dskcon variable                                        
               rts                                     


; backup command
CmdBackup         lbeq    la61f                    ; device number error if no drive numbers given                                                               

               jsr     TextResetVDU             ; reset sam display page and vog mode                                              

               jsr     >ld256                   ; get source drive number and save                                           

               stb     dbuf0+255                ; it at top of dbuf0 (top of new stack)                                                    
               jsr     BasChrGetCurr            ; get a character from basic                                      

               beq     ld27b                    ; branch if end of line                                

               ldb     #$a5                     ; token for 'to'                        
               jsr     VarCKChar                ; syntax check for 'to'                                

               jsr     >ld256                   ; get destination drive number                                       

ld27b          lds     #dbuf0+255               ; put stack at top of dbuf0                                              
               pshs    b                        ; save destination drive number on stack                                              
               jsr     la5c7                    ; syntax error if not end of line                                          

               jsr     DosCloseAllFiles                    ; close all files                          

               clr     ,-s                      ; clear a track counter on stack                                       
               ldx     #dflbuf-1                ; point x to top of disk ram variables                                                   
ld28c          inc     ,s                       ; increment track counter                                    
               leax    secmax*seclen,x          ; increment x by one track                                                                    
               cmpx    AddrFWareRamTop          ; compare to top of non reserved ran                                               
               bls     ld28c                    ; keep going if more free ram left                                           

               dec     ,s                       ; decrement track counter                               
               lbeq    BasOMError               ; 'om' error if < 1 track of free ram                                               

               lda     #trkmax                  ; get maximum number of tracks - initialize remaining tracks ctr                                                                           
               clrb                             ; initialize tracks written counter to zero                                                
               pshs    b,a                      ; save tracks written and remaining counters on stack                                                             

; at this point the stack has the following data on it:
; ,s = tracks remaining counter; 1,s = tracks written counter
; 2,s = number of tracks which fit in ram; 3,s = destination drive number
; 4,s = source drive number
               com     dresfl                   ; set the disk reset flag to cause a reset                                                    
ld2a4          clrb                             ; initialize write track counter to zero                                                  
ld2a5          incb                             ; add one to write track counter                                          
               dec     ,s                       ; decrement remaining tracks counter                                          
               beq     ld2ae                    ; and branch if no tracks left                                       

               cmpb    $02,s                    ; = compare write track counter to number of tracks that                                                                  
               bne     ld2a5                    ; = will fit in ram and branch if room for more tracks in ram                                                                      

ld2ae          stb     BasGenCount              ; save the number of tracks to be transferred                                                            
               ldb     $04,s                    ; get source drive number                                  
               bsr     ld2fc                    ; fill ram buffer with BasGenCount tracks of data                                                     

               lda     #$ff                     ; set source/destination flag to destination                                                    
               jsr     >ld322                   ; print prompt message if needed                                         

               ldb     $03,s                    ; get destination drive number                                       
               bsr     ld2ff                    ; write BasGenCount tracks from buffer                                          

               tst     ,s                       ; test tracks remaining flag                                  
               beq     ld2cd                    ; branch if backup done                                

               clra                             ; set source/destination flag to source                                            
               jsr     >ld322                   ; print prompt message if needed                                         

               ldb     $01,s                    ; get the tracks written counter, add the number of                                                            
               addb    BasGenCount              ; tracks moved this time through loop and                                                    
               stb     $01,s                    ; save the new tracks written counter                                              
               bra     ld2a4                    ; copy some more tracks                                

ld2cd          bsr     CloseFilesInit2          ; check for dos initialization                                            

               jmp     BasCmdMode               ; jump back to basic's main loop                                         


CloseFilesInit2          
               puls    u                        ; put the return address in u                                        
               lda     dresfl                   ; test disk reset flag                                
               beq     ld2ef                    ; don't reset the dos if flag not set                                              

               ldx     #fcbv1                   ; point x to table of fcb addresses                                             
               clra                             ; set file counter to zero                               
ld2dd          clr     [,x++]                   ; mark fcb as closed                                   
               inca                             ; add one to file counter                              
               cmpa    fcbact                   ; compare to number of reserved files                                                
               bls     ld2dd                    ; branch if any files not shut down                                            

               ldx     BasStartProg             ; load x with the start of basic                                          
               clr     -1,x                     ; set first byte of basic program to zero                                                 
               jsr     BasNew                   ; go do a 'new'                        

               clr     dresfl                   ; reset the dos reset flag                                    
ld2ef          lda     dloadfl                  ; check the load reset flag and                                              
               beq     ld2fa                    ; branch if not set                            

               clr     dloadfl                  ; clear the load reset flag                                     
               jsr     BasNew                   ; go do a 'new'                        

ld2fa          jmp     ,u                       ; jump back to return address saved in u above                                                         


ld2fc          lda     #$02                     ; read op code                           
               fcb      Skip2                   ; cmpx
ld2ff          lda     #$03                     ; write op code                            
               std     dcopc                    ; save in dskcon variable                                  
               lda     $03,s                    ; get the number of the track being currently                                                      
               sta     dctrk                    ; written and save it in dskcon variable                                                 
               ldx     #dflbuf                  ; = track buffer starts at dflbuf                                            
               stx     dcbpt                    ; = save it in dskcon variable                                       
               lda     BasGenCount              ; get number of tracks to move                                        
ld30e          ldb     #$01                     ; initialize sector counter to one                                               
ld310          stb     dsec                     ; save dskcon sector variable                                          
               jsr     >DoDiskReadWrite                   ; read/write a sector                              

               inc     dcbpt                    ; move buffer pointer up one sector (256 bytes)                                                        
               incb                             ; increment sector counter                               
               cmpb    #secmax                  ; compare to maximum number of sectors per track                                                            
               bls     ld310                    ; branch if any sectors left                                     

               inc     dctrk                    ; increment track counter variable to next track                                                         
               deca                             ; decrement tracks to move counter                                       
               bne     ld30e                    ; read more tracks if any left                                       

               rts                                     


ld322          ldb     $05,s                    ; get the destinatlon drive number and                                                    
               cmpb    $06,s                    ; compare it to the source drive number                                                 

; print source/destination disk switch prompt message
ld326          bne     ld35e                    ; return if drive numbers not equal                                                 

               clr     rdytmr                   ; reset the ready timer                                 
               clr     dskreg                   ; clear dskreg - turn off all disk motors                                                   
               clr     drgram                   ; clear dskreg ram image                                  
               pshs    a                        ; save source/destination flag on stack                                             
               jsr     TextCls                  ; clear screen                       

               ldx     #ld35f                   ; point x to 'insert source' message                                              
               ldb     #13                      ; 13 bytes in message                            
               lda     ,s+                      ; get source/destination flag from the stack                                                   
               beq     ld344                    ; branch if source                           

               ldx     #ld36c                   ; point x to 'insert destination' message                                                   
               ldb     #18                      ; 18 bytes in message                            
ld344          jsr     lb9a2                    ; send message to console out                                           

               ldx     #ld37e                   ; point x to 'diskette and' message                                             
               ldb     #27                      ; 27 bytes in message                            
               jsr     lb9a2                    ; send message to console out                                      

               ldd     #$6405                   ; set up 'sound' parameters                                     
               sta     SndPitch                 ; for a beep                      
               jsr     SndBeep                  ; jump to 'sound' - do a beep                                      

ld357          jsr     TextWaitKeyCurs2         ; get a character from console in                                               

               cmpa    #cr                      ; keep looking at console in until                                          
               bne     ld357                    ; you get a carriage return                                    

ld35e          rts                                          


ld35f          fcc     'INSERT SOURCE'                         
ld36c          fcc     'INSERT DESTINATION'                              
ld37e          fcc     ' DISKETTE AND'                         
               fcb     cr                                
               fcc     'PRESS '
               fcb      '''
               fcc      'ENTER'
               fcb      '''

; push filename.ext and drive number onto the stack
ld399          puls    y                        ; save return address in y                                     
               ldb     #11                      ; 11 characters in filename and extension                                                
               ldx     #dnambf+11               ; point x to top of disk name/ext buffer                                                      
ld3a0          lda     ,-x                      ; get a character from filename.                                            
               pshs    a                        ; ext buffer and push it onto the                                       
               decb                             ; stack - decrement counter and                                    
               bne     ld3a0                    ; keep looping until done                                  

               lda     dcdrv                    ; = get drive number and push                                      
               pshs    a                        ; = it onto the stack                           
               jmp     ,y                       ; pseudo - return to calling routine                                          


; pull filename.ext and drive number from (x) to ram
ld3ad          lda     ,x+                      ; get drive number and save                                       
               sta     dcdrv                    ; it in dskcon variable                                
               ldb     #11                      ; 11 bytes in filename and extension                                           
               ldu     #dnambf                  ; point u to disk name buffer                                        
               jmp     UtilCopyBXtoU            ; move filenane.ext from (x) to dnambf                                               


; copy
; the copy process is performed by copying data from the source file
; to ram and then copying it to the destination file. the source and
; destination files are opened as random files and both use the system
; fcb above the reserved fcbs. all of available free ram above the
; variables is used as a copy buffer which speeds up the copying process
; but unfortunately the method used will allow an error encountered during
; the copy process to 'hang' the system. this is caused by pointing the fcb's
; random file buffer pointer (fcbbuf,x) to the free ram buffer. an error
; will then cause the open file to be closed with fcbbuf,x pointing to an
; area in ram where the random file buffer close routine (lcae2) will never
; look for it
CmdCopy           jsr     >lc935                   ; get source filename.ext & drive number from basic                                                                

               bsr     ld399                    ; and save them on the stack                                     

               clr     ,-s                      ; clear a byte on stack - single disk copy (sdc) flag                                                            
               jsr     BasChrGetCurr            ; get current input character                                       

               beq     ld3ce                    ; branch if end of line - single disk copy                                                   

               com     ,s                       ; set soc flag to $ff (no single disk copy)                                                 
               ldb     #$a5                     ; token for 'to'                        
               jsr     VarCKChar                ; syntax check for 'to'                                

               jsr     >lc935                   ; get destination filename.ext and drive number                                                        

ld3ce          bsr     ld399                    ; save destination filename.ext & drive number on stack                                                                     

               jsr     la5c7                    ; syntax error if more characters on line                                                  

               jsr     DosCloseAllFiles                    ; close all files                          


; count the number of sectors worth of free ram available
               clr     ,-s                      ; clear a sector counter on the stack                                            
               leax    -seclen,s                ; point x one sector length down from the top of stack                                                                    
ld3dc          inc     ,s                       ; increment sector counter                                     
               leax    -seclen,x                ; decrement x by one sector                                         
               cmpx    BasVarEnd                ; compare to top of arrays                                     
               bhs     ld3dc                    ; branch if not at bottom of free ram                                              

               dec     ,s                       ; decrement sector counter                                
               lbeq    BasOMError               ; 'om' error if not at least one full sector of free ram                                                                  

               leax    14,s                     ; point x to start of source data                                          
               bsr     ld3ad                    ; put source data into dnambf and dskcon                                                 

               jsr     >lc68c                   ; scan directory for a match                                     

               jsr     >lc6e5                   ; 'ne' error if match not found                                        

               ldx     v974                     ; point x to directory ram image of found file                                                      
               ldu     dirlst,x                 ; get number of bytes in last sector and                                                    
               ldx     dirtyp,x                 ; source file type and ascii flag                                             
               pshs    u,x                      ; and save them on the stack                                    
               jsr     >GetFAT                   ; get valid fat data                             

               ldb     v976                     ; get number of first granule in file                                             
               jsr     >lcd1e                   ; get the number of granules in file                                             

               pshs    a                        ; and save it on the stack                                
               deca                             ; subtract off the last granule                                    
               andb    #$3f                     ; mask off last granule flag bits and save the                                                       
               pshs    b                        ; number of sectors in last granule on stack                                                  
               tfr     a,b                      ; save the number of granules in accb                                            
               clra                             ; clear the ms byte of accd                                
               jsr     >lc779                   ; multiply accd by nine                                

               addb    ,s                       ; add the number of sectors in the last                                              
               adca    #$00                     ; granule to accd                          
               ldx     #$0001                   ; initialize record counter to one                                            
               pshs    x,b,a                    ; initialize sector and record counters on the stack                                                              

; at this point the control variables for copy are stored on the stack.
;   0 1,s = remaining sectors counter; 2 3,s = record counter
;     4,s = number of sectors to be copied. initially set to number of
;           sectors in the last granule.
;     5,s = gran test flag. initially set to number of grans in file
;     6,s = file type; 7,s = ascii flag; 8 9,s = number of bytes in last sector
;    10,s = number of sectors which will fit in the currently available free ram
; 11-22,s = destination filename.ext and drive number
;    23,s = single disk copy flag; 24-35,s = source filename.ext and drive number
ld41e          clrb                             ; set sector counter to zero                                      
               ldx     ,s                       ; get the number of sectors remaining in the file                                                       
               beq     ld42c                    ; branch if no sectors left                                    

ld423          incb                             ; add a sector to temporary sector counter                                                    
               leax    -1,x                     ; decrement remaining sectors counter                                              
               beq     ld42c                    ; branch if no sectors left                                    

               cmpb    10,s                     ;compare temporary counter to number of sectors which may                                                                  
                                                ;be stored in free ram
               bne     ld423                    ; branch if still room for more sectors                                                

ld42c          stx     ,s                       ; save the number of uncopied sectors remaining in the file                                                                      
               stb     $04,s                    ; save the number of sectors to be copied this time through loop                                                                         
               bsr     ld482                    ; 'get' accb sectors to ram buffer                                           

               lda     #$ff                     ; set source/destination flag to destination                                                    
               bsr     ld476                    ; print prompt message if required                                           

               tst     $05,s                    ; check the gran test flag. if <> 0, it contains the                                                             
               beq     ld45f                    ; number of grans in the file and the destination disk                                                               

                                                ; must be checked for enough room. if it is =0
                                                ; then the check has already been done
               leax    11,s                     ; point to destination file parameters                                               
               jsr     >ld3ad                   ; get destination file parameters from stack                                                     

               jsr     >ld059                   ; scan directory for file - 'ae' error if it exists                                                            

               jsr     >GetFAT                   ; get valid fat data                             


; make sure there are enough free granules on the destination disk
               jsr     >lc755                   ; point x to fat                         

               leax    fatcon,x                 ; skip past the fat control bytes                                              
               lda     $05,s                    ; get the number of grans in the file                                              
               ldb     #granmx                  ; set gran counter to maximum                                        
ld44e          com     ,x                       ; check to see if a bran is free                                           
               bne     ld455                    ; and branch if it is not free                                       

               deca                             ; = decrement counter and branch if                                        
               beq     ld45d                    ; = there are enough free granules                                           

ld455          com     ,x+                      ; restore fat byte and increment pointer                                                    
               decb                             ; decrement gran counter                             
               bne     ld44e                    ; branch if all grans not checked                                          

               jmp     >lc7f8                   ; 'disk full' error                            

ld45d          com     ,x                       ; restore fat byte                             
ld45f          bsr     ld47c                    ; 'put' data from ram buffer to destination file                                                              

               ldx     ,s                       ; get the number of remaining sectors                                           
               beq     ld472                    ; exit routine if no sectors left                                          

               ldd     $02,s                    ;          
               addb    $04,s                    ; get the current record counter, add                                               
               adca    #$00                     ; the number of sectors (records) moved                                                
               std     $02,s                    ; and save the new record counter                                          
               clra                             ; set source/destination flag to source                                            
               bsr     ld476                    ; print prompt message if required                                           

               bra     ld41e                    ; keep copying sectors                               

ld472          leas    36,s                     ; remove temporary storage variables from stack                                                             
               rts                              ;;;; copy done ;;;;                       


ld476          tst     25,s                     ;check single disk copy flag - if <> zero, then don't                                                                  
                                                ;print the prompt message
               jmp     >ld326                   ; print the prompt message if required                                               


; 'put'.'get' data from the destination/source files
ld47c          lda     #$ff                     ; 'put' flag                         
               leax    13,s                     ; point x to destination filename data                                               
               bra     ld486                    ; go 'put' some data                             
ld482          clra                             ; zero is the 'get' flag                                  
               leax    26,s                     ; point x to the source filename data                                              
ld486          sta     vd8                      ; save the 'get'/'put' flag                                       
               jsr     >ld3ad                   ; get filename and drive data from the stack                                                     

               ldx     $08,s                    ; get ascii flag and file type and save                                                
               stx     dfltyp                   ; them in the disk ram variables                                          
               ldx     #seclen                  ; = save one sector length in                                        
               stx     dfflen                   ; = ram record length variable                                        
               lda     #'R                      ; random file type flag                               
               ldb     fcbact                   ; get the highest reserved fcb number, add one                                                        
               incb                             ; and open a random file whose fcb will be one above                                                         
               jsr     >lc48d                   ; the highest reserved fcb (the system fcb)                                                    

               ldx     fcbtmp                   ; point x to the 'system' fcb                                       
               ldd     #seclen                  ; set the number of bytes in the last sector                                                       
               std     fcblst,x                 ; of the file equal to one sector length                                                    

               ldb     $06,s                    ; =get the number of sectors to move and                                                 
               beq     ld4d4                    ; =branch if none left                               

               ldb     vd8                      ;grab the 'get'/'put' flag, 'and' it with the                                                    
               andb    $07,s                    ;gran test flag - branch if 'get'ing data or this is                                                              
               beq     ld4ba                    ;not the first time through the loop                                             

               ldd     $02,s                    ; =get the number of sectors remaining to be copied and                                                                
               addb    $06,s                    ; =add the number to be copied this time through loop                                                               
               adca    #$00                     ; =            
               jsr     >lc2e6                   ;'put' the last record in the file to the system fcb.                                                              

;                                                    ;the record number is in accd.
ld4ba          ldx     fcbtmp                   ; point x to the system fcb                                          
               ldu     $04,s                    ; get the current record number                                        
               stu     fcbrec,x                 ; and save it in the fcb                                    
               ldb     $06,s                    ; get the number of the record (sector) to move                                                        
               ldu     BasVarEnd                ; end of arrays is the start of the copy free ram buffer                                                                  
ld4c4          pshs    u,b                      ; save sector counter and buffer pointer on the stack                                                                  
               ldx     fcbtmp                   ; point x to system fcb                                 
               stu     fcbbuf,x                 ;set the random file buffer pointer to the 'copy' ram buffer                                                                        
;                                                    ;this will cause the system to 'hang' if an error occurs during copy.
               jsr     >lc2ea                   ; go 'get' or 'put' data to the system fcb                                                   

               inc     $01,s                    ; add 256 (one sector) to the buffer pointer                                                     
               puls    b,u                      ; get the sector counter and buffer poiner                                                  
               decb                             ; decrement sector counter                               
               bne     ld4c4                    ; branch if all sectors not done                                         

ld4d4          ldx     fcbtmp                   ; point x to system fcb                                      
               ldu     #dflbuf                  ; reset the random file buffer pointer for the system                                                                
               stu     fcbbuf,x                 ; fcb to the bottom of random file buffer area                                                          
               ldb     vd8                      ; =grab the 'get'/'put' flag, 'and' it with the gran                                                           
               andb    $07,s                    ; =test flag - close the file if 'get'ing data and                                                            
               beq     ld4ea                    ; =this is not the first time through the loop                                                       

               clr     $07,s                    ; reset the gran test flag if first time through loop                                                              
               ldd     10,s                     ;get the number of bytes in the last sector,                                                    
               ora     #$80                     ;'or' in the pre-saved flag and                                       
               std     fcblst,x                 ;save the number of bytes in the last sector in the fcb                                                                   

ld4ea          jmp     >lcb06                   ; close the file                              


; dski$ command
CmdDski           bsr     ld527                    ; get the drive, track and sector numbers                                                      

               bsr     ld51c                    ; evaluate string variable 1 and save                                              

               pshs    x                        ; the descriptor address on the stack                                           
               bsr     ld51c                    ; = evaluate string variable 2 and save                                                

               pshs    x                        ; = the descriptor address on the stack                                             
               ldb     #$02                     ; dskcon read op code                             
               jsr     >ld58f                   ; reao a sector into dbuf0                                   

               ldu     #dbuf0+128               ; point u to top half of dbuf0                                            
               puls    x                        ; get string 2 descriptor address                                       
               bsr     ld508                    ; put string 2 into string space                                         

               ldu     #dbuf0                   ; point u to bottom half of dbuf0                                           
               puls    x                        ; get string 1 descriptor address                                       
ld508          pshs    u,x                      ; put string descriptor & source pointer on the stack                                                                  
               ldb     #128                     ;         
               jsr     BasResStr                ; reserve 128 bytes in string space                                            

               leau    ,x                       ; point u to reserved string space                                         
               puls    x                        ; get string descriptor address                                     
               stb     ,x                       ; save descriptor data (length and address)                                                 
               stu     $02,x                    ; of the new string                            
               puls    x                        ; get the source (dbuf0) pointer                                      
ld519          jmp     UtilCopyBXtoU            ; move sector data from dbuf0 to string space                                                           


ld51c          jsr     VarCKComma               ; syntax check for a comma                                           

               ldx     #VarGetVar               ; point x to evaluate variable routine                                                
               bsr     ld553                    ; evaluate a variable                              

ld524          jmp     VarGetExpr               ; 'tm' error if numeric variable                                              


; evaluate drive, track and sector numbers
ld527          jsr     VarGet8Bit               ; evaluate expression, return value in accb                                                            

               cmpb    #$03                     ; compare to 3 (highest drive number) -                                                
               bhi     ld54a                    ; 'fc' error if it's > 3                                 
               pshs    b                        ; save drive number on the stack                                      
               jsr     VarGetComma8             ; syntax check for comma. evaluate expression (track number)                                                                     

               cmpb    #trkmax-1                ; check for maximum track number                                              
               bhi     ld54a                    ; 'fc' error if track number > 34                                          
               pshs    b                        ; save track number on the stack                                      
               jsr     VarGetComma8             ; syntax check for comma, evaluate expression (sector number)                                                                      

               stb     dsec                     ; save sector number in dskcon variable                                               
               decb                             ;useless instruction. next instruction should just                                                       
               cmpb    #secmax-1                ;check for maximum sector number (secmax)                                                       
               bhi     ld54a                    ; 'fc' error if sector number too big                                              
               puls    a,b                      ; get track and drive number off of                                           
               sta     dctrk                    ; the stack and save in dskcon                                       
               stb     dcdrv                    ; variables                    
               rts                                     

ld54a          jmp     BasFCError               ; jump to 'fc' error                                  


ld54d          jsr     VarCKComma               ; syntax check for comma                                         

               ldx     #VarGetStr               ; point x to 'evaluate expression' routine address                                                            
ld553          ldb     dcdrv                    ; get the dskcon drive, track and                                               
               ldu     dctrk                    ; sector values and save them on the stack                                                   
               pshs    u,b                      ; pshs    s,b         
               jsr     ,x                       ; go evaluate an expression or a variable                                               

               puls    b,u                      ; get the drive, track and sector                                         
               stb     dcdrv                    ; numbers off of the stack and put                                           
               stu     dctrk                    ; them back into the dskcon variables                                              
               rts                                     


; dsko$ command
CmdDsko           bsr     ld527                    ; get the drive, track and sector numbers                                                      

               bsr     ld54d                    ; get the descriptor of string 1                                         

               bsr     ld524                    ; 'tm' error if numeric expression                                           

               ldx     BasVarFPAcc1+3           ; get string 1 descriptor address                                           
               pshs    x                        ; and save it on the stack                                
               bsr     ld54d                    ; get the descriptor of string 2                                         

               jsr     BasGetStrLenAddr         ;get length and address of string 2 and                                                

               pshs    x,b                      ;save them on the stack                               
               clrb                             ; set clear counter to 256 (full sector buffer)                                                    
               ldx     #dbuf0                   ; use dbuf0 as the dsko$ i/o buffer                                             
ld577          clr     ,x+                      ; clear a byte in i/o buffer                                        
               decb                             ; decrement clear counter                              
               bne     ld577                    ; branch if all 256 bytes not cleared                                              

               puls    b,x                      ; get the length and address of string 2                                                
               ldu     #dbuf0+128               ; point x to string 2 destination                                               
               bsr     ld519                    ; move string 2 data into dbuf0                                        

               puls    x                        ; point x to string 1 descriptor                                      
               jsr     VarDelVar                ; get the length and address of string 1                                                 

               ldu     #dbuf0                   ; point u to string 1 destination                                           
               bsr     ld519                    ; move string 1 data into dbuf0                                        

               ldb     #$03                     ; dskcon write op code                              
ld58f          ldx     #dbuf0                   ; point x to i/o buffer (dbuf0)                                         
               stx     dcbpt                    ;          
               stb     dcopc                    ; save new dskcon buffer pointer and op code variables                                                               
               jmp     >DoDiskReadWrite                   ; go write out a sector                                


; CmdDskini command
CmdDskini      lbeq    la61f                    ; branch to 'dn' error if no drive number specified                                                                   

               jsr     >ld256                   ; calculate drive number                                 

               ldb     #$04                     ; skip factor default value                                   
               jsr     BasChrGetCurr            ; get current input char from basic                                             

               beq     ld5b2                    ; branch if end of line                                

               jsr     VarGetComma8             ; syntax check for comma and evaluate expression                                                         

               cmpb    #17                      ; max value of skip factor = 16                                       
               lbhs    BasFCError               ; 'illegal function call' if bad skip factor                                                      

               jsr     la5c7                    ; syntax error if more characters on the line                                                      

ld5b2          	
		ifeq	1
		
	       pshs    b                        ; save skip factor on the stack                                          

               ldx     #dbuf1+secmax            ; point to end of logical sector number storage area                                                                     
               ldb     #secmax                  ; 18 sectors per track                                 
ld5b9          clr     ,-x                      ; clear a byte in the buffer                                        
               decb                             ; cleared all 18?                      
               bne     ld5b9                    ; keep going if not                            

               clra                             ; reset physical sector counter                                    
               bra     ld5ce                    ; start with first physical sector = 1                                               

; calculate logical sector numbers
ld5c1          addb    ,s                       ; add skip factor to logical sector counter                                                       
ld5c3          incb                             ; add one to logical sector counter                                             
ld5c4          subb    #secmax                  ; subtract max number of sectors                                                 
               bhs     ld5c4                    ; branch until 0 > accb >= -18                                       

               addb    #secmax                  ; add 18, now accb is 0-17                                      
               tst     b,x                      ; is anything stored here already?                                         
               bne     ld5c3                    ; yes - get another sector                                   

ld5ce          inca                             ; increment physical sector number and                                                
               sta     b,x                      ; save it in the ram buffer                                  
               cmpa    #secmax                  ; finished with all sectors?                                        
               blo     ld5c1                    ; no - keep going                          

               leas    $01,s                    ; remove skip factor from stack                                         
               ldx     #dflbuf+$1888-2          ; get top of ram used by CmdDskini                                                  
               cmpx    AddrFWareRamTop          ; is it > cleared area?                                  
               lbhi    BasOMError               ; 'out of memory' error if > cleared area                                                   

		endc

	       jsr     DosCloseAllFiles                    ; close all files                          

               com     dresfl                   ; set reset flag to $ff - this will cause a dos reset                                                               
               lds     #dbuf1+seclen            ; set stack to top of dbuf1                                            
               jsr     TextResetVDU             ; reset sam to display page zero and alpha graphics                                                            

               lda     #$00                     ; you could delete this instruction and change following sta to clr                                                                           
               sta     dcopc                    ; restore head to track zero dskcon op code                                                    
               clr     dctrk                    ; set dskcon track variable to track zero                                                  
;               jsr     >DoDiskReadWrite                   ; restore head to track zero                                     

		lda	dcdrv			; get drive number
		ldb	#trkmax			; RSdos 35 tracks / disk
		jsr     >MMC_FormatImage        ; Tell mmc to format virtual image

                jsr     >DosFunctionRestore      ; restore to track 0
             
                jmp     >ld2cd                   ; go check for a dos reset                                   

		ifeq 	1

               clr     rdytmr                   ; reset the ready timer                                 
;               lda     #WDCmdReadAddr           ; foc read address code                               
;               sta     fdcreg                   ;           
               jsr     >WaitFDCNotBusy                   ; check drive ready - wait until ready                                               

               beq     ld620                    ; branch if drives ready                                 

               jmp     >ld688                   ; error if drives not ready                                    

ld606          cmpa    #22                      ; = check for track 22 (precompensation)                                                     
               blo     ld612                    ; = and branch if < track 22 - no precomp                                                  

               lda     drgram                   ; get the ram image of dskreg, 'or'                                             
               ora     #$10                     ; in the precompensation flag and                                         
               sta     dskreg                   ; send it to dskreg                             
ld612
;               lda     #WDStepIn                ; = get step in command                                    
;               sta     fdcreg                   ; = and send it to the 1793                                     
;               exg     a,a                      ; delay after issuing command to 1793                                            
;               exg     a,a                      ;        
               jsr     >WaitFDCNotBusy                   ; check drive ready                            

               bne     ld688                    ; branch if not ready - issue an error                                               

ld620          
;               jsr     >MediumDelay             ; wait a while                            
               bsr     ld691                    ; build a formatted track in ram                                         

               ldy     #fdcreg+3                ; y points to 1793 data register                                             
               orcc    #$50                     ; disable interrupts                             
               ldx     #ld64f                   ; get return address and store                                        
               stx     dnmivc                   ; it in the non maskable interrupt vector                                                   
               ldx     #dflbuf                  ; point x to the formatted track ram image                                                     
               lda     fdcreg                   ; reset status of the 1793                                    
               lda     #$ff                     ; enable the nmi flag to vector                                       
               sta     nmiflg                   ; out of an i/o loop upon an nmi interrupt                                                    

;               ldb     #WDCmdWriteTrack         ; = get write track command and                                       
;               stb     fdcreg                   ; = send to 1793                          
               lda     drgram                   ; get the dskreg ram image and 'or' in the                                                    
               ora     #$80                     ; flag which will enable the 1793 to halt                                                 
;               sta     dskreg                   ; the 6809. send result to dskreg                                           
ld649          ldb     ,x+                      ; = get a byte from the formatted track                                                   
               stb     ,y                       ; = ram image, send it to the 1793 and                                            
               bra     ld649                    ; = loop back to get another byte                                          

ld64f          lda     fdcreg                   ; get status                           
               andcc   #$af                     ; enable interrupts                             
               anda    #$44                     ; keep only write protect & lost data                                              
               sta     dcsta                    ; and save it in the dskcon status byte                                                
               bne     ld688                    ; branch if error                          

               inc     dctrk                    ; skip to the next track                                 
               lda     dctrk                    ; get the track number                               
               cmpa    #trkmax                  ; was it the last track                                   
               bne     ld606                    ; no - keep going                          

; verify that all sectors are readable
               lda     #$02                     ; = get the dskcon read op code                                       
               sta     dcopc                    ; = and save it in the dskcon variable                                               
               ldx     #dbuf0                   ; point the dskcon buffer pointer                                           
               stx     dcbpt                    ; to dbuf0                   
               ldu     #dbuf1                   ; point u to the logical sector numbers                                                 
               clra                             ; reset the track counter to zero                                      
ld66f          sta     dctrk                    ; set the dskcon track variable                                             
               clrb                             ; reset the sector counter                               
ld672          lda     b,u                      ; get the physical sector number                                            
               sta     dsec                     ; save dskcon sector variable                                     
               jsr     >DoDiskReadWrite         ; read a sector                        

               incb                             ; increment the sector counter                                   
               cmpb    #secmax                  ; and compare it to maximum sector number                                                     
               blo     ld672                    ; and keep looping if more sectors left                                                

               lda     dctrk                    ; = get the current track number                                         
               inca                             ; = add one to it, compare to the maximum track                                                    
               cmpa    #trkmax                  ; = number and keep looping if                                          
               blo     ld66f                    ; = there are still tracks to do                                         

               jmp     >ld2cd                   ; go check for a dos reset                                   

ld688          clr     drgram                   ; clear ram image of dskreg                                          
               clr     dskreg                   ; clear dskreg - turn disk motors off                                               
               jmp     >ld701                   ; process drives not ready error                                         


; build a formatted track of data in ram starting at dflbuf.

ld691          ldx     #dflbuf                  ; start track buffer at dflbuf                                              
               ldd     #$204e                   ; get set to write 32 bytes of $4e                                            
               bsr     ld6c2                    ; go write gap iv                          

               clrb                             ; reset sector counter                           
ld69a          pshs    b                        ; save sector counter                                
               ldu     #dbuf1                   ; point u to the table of logical sectors                                                   
               ldb     b,u                      ; get logical sector number from table and                                                 
               stb     dsec                     ; save it in the dskcon variable                                        
               ldu     #ld6d4                   ; point u to table of sector formatting data                                                      
               ldb     #$03                     ; get first 3 data blocks and                                     
               bsr     ld6c8                    ; write them to buffer                               

               lda     dctrk                    ; = get track number and store lt                                          
               sta     ,x+                      ; = in the ram buffer                            
               clr     ,x+                      ; clear a byte (side number) in buffer                                             
               lda     dsec                     ; get sector number and                               
               sta     ,x+                      ; store it in the buffer                               
               ldb     #$09                     ; = get the last nine data blocks and                                             
               bsr     ld6c8                    ; = write them to the buffer                                     

               puls    b                        ; get sector counter                          
               incb                             ; next sector                  
               cmpb    #secmax                  ; 18 sectors per track                                  
               blo     ld69a                    ; branch if all sectors not done                                         

               ldd     #$c84e                   ; write 200 bytes of $4e at end of track                                                  

; write acca bytes of accb into buffer
ld6c2          stb     ,x+                      ; store a byte in the buffer                                        
               deca                             ; decrement counter                        
               bne     ld6c2                    ; branch if all bytes not moved                                        

               rts                                     

ld6c8          pshs    b                        ; save the counter on the stack                                          
               ldd     ,u++                     ; get two bytes of data from the table                                              
               bsr     ld6c2                    ; write acca bytes of accb into the buffer                                                   

               puls    b                        ; get the counter back, decrement                                       
               decb                             ; it and branch if all data blocks                                       
               bne     ld6c8                    ; not done                   

               rts                                     


; data used to format a sector on the disk

; these data are close to the ibm system 34 format for 256 byte sectors.
; double density. the format generally conforms to that specified on the
; 1793 data sheet. the gap sizes have been reduced to the minimum
; allowable. the ibm format uses $40 as the fill character for the data
; blocks while color dos uses an $ff as the fill character.
ld6d4          fcb     8,0                      ; sync field                        
               fcb     3,$f5                                
               fcb     1,$fe                    ; id address mark (am1)                                
; track, side, and sector numbers are inserted here
               fcb     1,1                      ; sector size (256 byte sectors)                                       
               fcb     1,$f7                    ; crc request                      
               fcb     22,$4e                   ; gap ii (post-id gap)                                
               fcb     12,0                     ; sync field                    
               fcb     3,$f5                                
               fcb     1,$fb                    ; data address mark (am2)                                  
               fcb     0,$ff                    ; data field (256 bytes)                                 
               fcb     1,$f7                    ; crc request                      
               fcb     24,$4e                   ; gap iii (post data gap)                                   

		endc

; dos command
CmdDos         bne     ld742                    ; return if argument given                                      
               jmp     [dosvec]                 ; jump to the dos command                                     

DoDiskReadWrite
               pshs    b                        ; save accb                      
               ldb     #$05                     ; 5 retries                   
               stb     attctr                   ; save retry count                            
               puls    b                        ; restore accb                    
ld6fb          bsr     dskcon                   ; go execute command                                   

               tst     dcsta                    ; check status                       
               beq     ld70e                    ; branch if no errors                              

ld701          lda     dcsta                    ; get dskcon error status                                       
               ldb     #2*30                    ; 'write protected' error                                                              
               bita    #$40                     ; check bit 6 of status                                
               bne     ld70b                    ; branch if write protect error                                        

ld709          ldb     #2*20                    ; 'i/o error'                                                       
ld70b          jmp     SysErr                   ; jump to error driver                                    

ld70e          pshs    a                        ; save acca                      
               lda     dcopc                    ; get operation code                             
               cmpa    #$03                     ; check for write sector command                                         
               puls    a                        ; restore acca                    
               bne     ld742                    ; return if not write sector                                     

               tst     dverfl                   ; check verify flag                             
               beq     ld742                    ; return if no verify                              

               pshs    u,x,b,a                  ; save registers                            
               lda     #$02                     ; read operation code                             
               sta     dcopc                    ; store to dskcon parameter                                    
               ldu     dcbpt                    ; point u to write buffer address                                          
               ldx     #dbuf1                   ; address of verify buffer                                    
               stx     dcbpt                    ; to dskcon variable                             
               bsr     dskcon                   ; go read sector                          

               stu     dcbpt                    ; restore write buffer                               
               lda     #$03                     ; write op code                       
               sta     dcopc                    ; save in dskcon variable                                  
               lda     dcsta                    ; check status for the read operation                                              
               bne     ld743                    ; branch if error                          

               clrb                             ; check 256 bytes                      
ld737          lda     ,x+                      ; get byte from write buffer                                        
               cmpa    ,u+                      ; compare to read buffer                                
               bne     ld743                    ; branch if not equal                              

               decb                             ; decrement byte counter and                                 
               bne     ld737                    ; branch if not done                             

               puls    a,b,x,u                  ; restore registers                               
ld742          rts                                          

ld743          puls    a,b,x,u                  ; restore registers                                    
               dec     attctr                   ; decrement the verify counter                                        
               bne     ld6fb                    ; branch if more tries left                                    

               ldb     #2*36                    ; 'verify error'                                                     
               bra     ld70b                    ; jump to error handler                                

; verify command
CmdVerify      clrb                             ; off flag = 0                         
               cmpa    #$aa                     ; off token ?                      
               beq     ld75a                    ; yes              

               comb                             ; on flag = $ff                    
               cmpa    #$88                     ; on token                   
               lbne    BasSNError               ; branch to 'syntax error' if not on or off                                                     

ld75a          stb     dverfl                   ; set verify flag                                
               jmp     BasChrGet                ; get next character from basic                                         


; dskcon routine
dskcon         pshs    u,y,x,b,a                ; save registers                                    
               lda     #$05                     ; get retry count and                             
               pshs    a                        ; save it on the stack                            
ld765          clr     rdytmr                   ; reset drive not ready timer                                            
               ldb     dcdrv                    ; get drive number                           
               ldx     #ld89d                   ; point x to drive enable masks                                         
               lda     drgram                   ; get dskreg image                            
               anda    #$a8                     ; keep motor status, double density. halt enable                                                         
               ora     b,x                      ; 'or' in drive select data                                  
               ora     #$20                     ; 'or' in double density                                
               ldb     dctrk                    ; get track number                           
               cmpb    #22                      ; precompensation starts at track 22                                            
               blo     ld77e                    ; branch if less than 22                                 

               ora     #$10                     ; turn on write precompensation if >= 22                                                
ld77e          tfr     a,b                      ; save partial image in accb                                        
               ora     #$08                     ; 'or' in motor on control bit                                      
               sta     drgram                   ; save image in ram                             
               sta     dskreg                   ; program the 1793 control register                                             
               bitb    #$08                     ; = were motors already on?                                    
               bne     ld792                    ; = don't wait for it to come up to speed if already on                                                                

;               jsr     la7d1                    ; wait a while                       
;               jsr     la7d1                    ; wait some more for motor to come up to speed                                                       

ld792          bsr     WaitFDCNotBusy           ; wait until not busy or time out                                               

               bne     ld7a0                    ; branch if timed out (door open. no disk, no power. etc.)                                                                   

               clr     dcsta                    ; clear status register                                
               ldx     #DosFunctionTable        ; point to command jump vectors                                         
               ldb     dcopc                    ; get command                      
               aslb                             ; 2 bytes per command jump address                                       
               jsr     [b,x]                    ; go do it 

                  
ld7a0          puls    a                        ; get retry count                            
               ldb     dcsta                    ; get status                     
               beq     ld7b1                    ; branch if no errors                              

               deca                             ; decrement retries counter                                
               beq     ld7b1                    ; branch if no retries left                                    

               pshs    a                        ; save retry count on stack                                 
               bsr     DosFunctionRestore       ; restore head to track 0                                  

               bne     ld7a0                    ; branch if seek error                               

               bra     ld765                    ; go try command again if no error                                           
ld7b1          lda     #120                     ; 120 ;1/60 = 2 seconds (1/60 second for each irq interrupt)                                                                         
               sta     rdytmr                   ; wait 2 seconds before turning off motor                                                   
               puls    a,b,x,y,u,pc             ; restore registers - exit dskcon     
                                             
; restore head to track 0
DosFunctionRestore          
               ldx     #dr0trk                  ; point to track table                                      
               ldb     dcdrv                    ; get drive number                           
               clr     b,x                      ; zero track number                          

;               lda     #WDRestore               ; restore head to track 0, unload the head                                                  
;               sta     fdcreg                   ; at start, 30 ms stepping rate                                         
;               exg     a,a                      ; =          
;               exg     a,a                      ; = wait for 1793 to respond to command                                              
               bsr     WaitFDCNotBusy                    ; wait till drive not busy                                   

;              bsr     MediumDelay                    ; wait some more                         

               lda      dcdrv                   ; get drive
               clrb                             ; get track (0)
               jsr      >MMC_SeekCheck          ; go seek it

               anda    #$10                     ; 1793 status : keep only seek error                                             
               sta     dcsta                    ; save in dskcon status                                
DosFunctionNOP          
                rts                                          

; wait for the 1793 to become unbusy. if it does not become unbusy,
; force an interrupt and issue a `drive not ready' 1793 error.
WaitFDCNotBusy 
		ifeq	1
               ldx     Misc16BitScratch         ; get zero to x register - long wait                                                 
ld7d3          leax    -1,x                     ; decrement long wait counter                                           
               beq     ld7df                    ; lf not ready by now, force interrupt                                               

               lda     fdcreg                   ; get 1793 status and test                                    
               bita    #$01                     ; busy status bit                          
               bne     ld7d3                    ; branch if busy                         
		endc
               clra
               rts                                     

ld7df          
;               lda     #WDCmdForceInt           ; force interrupt command - terminate any command                                                              
;               sta     fdcreg                   ; in process. do not generate a 1793 interrupt request                                                                
;               exg     a,a                      ; wait before reading 1793                                 
;               exg     a,a                      ;        
;               lda     fdcreg                   ; reset intrq (fdc interrupt request)                                               
               lda     #$80                     ; return drive not ready status if the drive did not become unbusy                                                                          
               sta     dcsta                    ; save dskcon status byte                                  
               rts                                     

; medium delay
MediumDelay    ldx     #8750                    ; delay for a while                                 
ld7f3          leax    -1,x                     ; decrement delay counter and                                           
               bne     ld7f3                    ; branch if not done                             

               rts                                     

; read one sector
DosFunctionReadSec

               lda     #DosFnReadSec            ; $80 is read flag (1793 read sector)                                                  

;              cmpx    #$86a0                      ; skip two bytes                                
		fcb	Skip2			   ; cmpx
; write one sector
DosFunctionWriteSec
               lda     #DosFnWriteSec           ; $a0 is write flag (1793 write sector)                                                    
               pshs    a                           ; save read/write flag on stack                                     
               
               ldx     #dr0trk                     ; point x to track number table in ram                                                 
               ldb     dcdrv                       ; get drive number                           
               abx                                 ; point x to correct drive's track byte                                           
               ldb     ,x                          ; get track number of current head position                                                 
;               stb     fdcreg+1                    ; send to 1793 track register                                         
;               cmpb    dctrk                       ; compare to desired track                                    
;               beq     ld82c                       ; branch if on correct track                                     
               lda     dctrk                       ; get track desired                            
;               sta     fdcreg+3                    ; send to 1793 data register                                        
               sta     ,x                          ; save in ram track image                               
               
               lda      dcdrv                      ; get drive
               ldb      dctrk                      ; get track
               jsr      >MMC_SeekCheck             ; go seek it
      
;               lda     #$17                        ; seek command for 1793: do not load the                                                
;               sta     fdcreg                      ; head at start, verify destination track,                                                    
;               exg     a,a                         ; 30 ms stepping rate - wait for                                       
;               exg     a,a                         ; valid status from 1793                               
;               bsr     ld7d1                       ; wait till not busy                             
;               bne     ld82a                       ; return if timed out                              
;               bsr     ld7f0                       ; wait some more                         

               anda    #$18                        ; keep only seek error or crc error in id field                                                        
               beq     ld82c                       ; branch if no errors - head on correct track                                                      

               sta     dcsta                       ; save in dskcon status                                
ld82a          puls    a,pc                                      

; head positioned on correct track
ld82c          lda     dsec                        ; get sector number desired                                        
               jsr      >MMC_SendHR                ; send to image. 
;               sta     fdcreg+2                    ; send to 1793 sector register                                          

;               ldx     #DoNMI                      ; point x to routine to be vectored                                             
;               stx     dnmivc                      ; to by nmi upon completion of disk i/o and save vector                                                                 

               ldx     dcbpt                       ; point x to i/o buffer                                
;               lda     fdcreg                      ; reset intrq (fdc interrupt request)                                               
;               lda     drgram                      ; get dskreg image                            
;              ora     #$80                        ; set flag to enable 1793 to halt 6809                                              

                   

               puls    b                           ; get read/write command from stack                                         
;               ldy     zero                        ; zero out y - timeout initial value                                            
               ldu     #fdcreg                     ; u points to 1793 interface registers                                                 
               com     nmiflg                      ; nmi flag = $ff: enable nmi vector                                             
;               orcc    #$50                        ; disable firq,irq                           
;               stb     fdcreg                      ; send read/write command to 1793: single record, compare                                                                   
;               exg     a,a                         ; for side 0, no 15 ms delay, disable side select                                                        
;               exg     a,a                         ; compare, write data address mark (fb) - wait for status                                                                


               cmpb    #DosFnReadSec               ; was this a read?                           
               beq     DoReadData                  ; if so, go look for data                                  
; wait for the 1793 to acknowledge ready to write data
                
               lda      dcdrv                      ; Get drive, x already contains buffer
               jsr      >MMC_WriteDOSSec           ; go write it
               bra      ReadWriteDone              ; go here when done
 
                ifeq    1

               ldb     #$02                        ; drq mask bit                      
ld85b          bitb    ,u                          ; is 1793 ready for a byte? (drq set in status byte)                                                                
               bne     ld86b                       ; branch if so                       
               leay    -1,y                        ; decrement wait timer                               
               bne     ld85b                       ; keep waiting for the 1793 drq                                        
ld863          clr     nmiflg                      ; reset nmi flag                               
               andcc   #$af                        ; enable firq,irq                           
               jmp     >ld7df                       ; force interrupt, set drive not ready error                                                     

; write a sector
ld86b          ldb     ,x+                         ; get a byte from ram                                 
               stb     fdcreg+3                    ; send it to 1793 data register                                           
               sta     dskreg                      ; reprogram fdc control register                                          
               bra     ld86b                       ; send more data                         
               endc
               
; wait for the 17933 to acknowledge ready to read data
DoReadData     
               lda      dcdrv                      ; Get drive, x already contains buffer
               jsr      >MMC_ReadDOSSec           ; go write it
               bra      ReadWriteDone              ; go here when done

                ifeq    1
                ldb     #$02                        ; drq mask bit                           
ld877          bitb    ,u                          ; does the 1793 have a byte? (drq set in status byte)                                                                 
               bne     ld881                       ; yes, go read a sector                                
               leay    -1,y                        ; decrement wait timer                               
               bne     ld877                       ; keep waiting for 1793 drq                                    
               bra     ld863                       ; generate drive not ready error                                         
               
                endc 
                
; read a sector
ld881          
                ifeq    1
                ldb     fdcreg+3                    ; get data byte from 1793 data register                                                        
               stb     ,x+                         ; put it in ram                      
               sta     dskreg                      ; reprogram fdc control register                                          
               bra     ld881                       ; keep getting data                            
                endc
                
; branch here on completion of sector read/write
ReadWriteDone   
               andcc   #$af                        ; enable irq, firo                                 
               anda    #$7c                        ; fault, record not found, crc error or lost data                                                          
               sta     dcsta                       ; save in dskcon status                                
               rts                                     

; dskcon operation code jump vectors
DosFunctionTable          
                fdb     DosFunctionRestore      ; restore head to track zero                                          
                fdb     DosFunctionNOP          ; no op - return                         
                fdb     DosFunctionReadSec      ; read sector                      
                fdb     DosFunctionWriteSec     ; write sector                       

; dskreg masks for disk drive select
ld89d          fcb     1                        ; drive sel 0                       
               fcb     2                        ; drive sel 1                  
               fcb     4                        ; drive sel 2                  
               fcb     $40                      ; drive sel 3                    

; nmi service
dnmisv         lda     nmiflg                   ; get nmi flag                              
               beq     ld8ae                    ; return if not active                               

               ldx     dnmivc                   ; get new return vector                                 
               stx     10,s                     ; store at stacked pc slot on stack                                           
               clr     nmiflg                   ; reset nmi flag                          
ld8ae          rti                                     


; irq service
dirqsv         lda     pia0+3                   ; 63.5 micro second or 60 hz interrupt?                                                       
               bpl     ld8ae                    ; return if 63.5 microsecond                                     
               lda     pia0+2                   ; reset 60 hz pia interrupt flag                                          
               lda     rdytmr                   ; get timer                     
               beq     ld8cd                    ; branch if not active                               

               deca                             ; decrement the timer                          
               sta     rdytmr                   ; save it                   
               bne     ld8cd                    ; branch if not time to turn off disk motors                                                     

               lda     drgram                   ; = get dskreg image                              
               anda    #$b0                     ; = turn all motors and drive selects off                                                  
               sta     drgram                   ; = put it back in ram image                                      
               sta     dskreg                   ; send to control register (motors off)                                                 
ld8cd          jmp     l8955                    ; jump to extended basic's irq handler                                                    


; this is the end of disk basic (except for the dos command at $df00).
; the code from this point to $df00 is garbage.
; dosbas 1.1 = 1686 wasted bytes

;                fcb     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;                fcb     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;                fcb     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;
;                fill    0,$600

;; this is the code for the dos command

;               org     $df00   

doscom         swi3                             ; do a software interrupt (#3)                                         
               clr     BasGenCount              ; reset sector counter                                
               ldd     #dosbuf                  ; ram load address for sector data                                             
               pshs    b,a                      ; save ram load address                               
ldf09          ldx     dskvar                   ; point x to dskcon variables                                            
               inc     BasGenCount              ; increment sector counter                                    
               lda     BasGenCount              ; get the sector counter                                  
               cmpa    #secmax                  ; loaded in 18 sectors? (one track)                                               
               bhi     ldf36                    ; yes - exit      
               
               sta     $03,x                    ; no - save sector number in dsec                                          
               ldd     #$0200                   ; get fdc op code (read) and drive number (0)                                                       
               sta     ,x                       ; save them in dskcon variables (bug - should be std ,x)                                                              
               lda     #34                      ; get track number (34)                              
               sta     $02,x                    ; save it in dskcon variables too                                          
               puls    a,b                      ; get ram load address                              
               std     $04,x                    ; and save it in the dskcon variables                                              
               adda    #$01                     ; add 256 (one sector) to ram load address (should be inca)                                                                    
               pshs    b,a                      ; save new ram load address                                   
               jsr     [dcnvec]                 ; go read a sector                              

               tst     $06,x                    ; check for errors                           
               beq     ldf09                    ; keep reading if none                               

	       puls    a,b                      ; pull load address off of the stack                                                 
               ldb     #2*20                    ; 'io' error                                                   
               jmp     SysErr                   ; jump to error servicing routine                                          

ldf36          puls    a,b                      ; pull ram load address off of the stack                                                
               ldd     dosbuf                   ; get first two bytes of ram data                                           
               cmpd    #"OS"                    ; look for 'os' (os9) at start of buffer                                                  
               lbeq    dosbuf+2                 ; if 'os' then branch to data loaded in ram                                                        

               clr     dosbuf                   ; otherwise clear the first two                                         

               clr     dosbuf+1                 ; bytes of ram data                                     
               jmp     bawmst                   ; jump to basic's warm start                                      


dosini         ldd     #$3b3b                   ; two rti instructions                                
               std     SecVecSWI3               ;           
               std     SecVecSWI3+2             ; load the swi2 and swi3 jump                                         
               std     SecVecSWI2+1             ; vectors with rtis                               
end            rts                                        


; end of the dos and dosini commands - the rest of the code
; to the end of the disk rom ($dfff) is garbage.
; dosbas 1.1 = 167 wasted bytes

		ifne	0
;                org     $df80
;
; the following routine is (c) 2005, p.harvey-smith, and boots the coco into all ram mode
; this of course requires a coco with at least 64k.
;

;
; this routine copies the boot routine to the first graphics page.
; 

start   leax    romcopy,pcr                     ; point to code to move
        ldy     #$600                           ; point to destination, grapics page 0
        ldu     #romcopylen                     ; get byte count
        
copyloop
        lda     ,x+                             ; fetch a byte from source
        sta     ,y+                             ; store in destination
        leau    -1,u                            ; decrement count
        cmpu    #$0                             ; done all ?
        bne     copyloop                        ; no : do next

        jmp     $600                            ; done, jump to copied code.


;
; this routine does the actual rom copy, it is first copied into ram at $600
; and then called, it switches backwards and forwards between rom and ram mode
; copying bytes.
;

romcopy orcc    #$50                            ; disable inturrupts
        ldx     #$8000                          ; point to beginning of rom
        
loop    clr     $ffde                           ; switch to rom mode
        lda     ,x                              ; pickup a byte
        clr     $ffdf                           ; switch to ram mode
        sta     ,x+                             ; save byte in ram
        cmpx    #$ff00                          ; done all bytes ?
        bne     loop                            ; no : loop again


        clr     $ffdf                           ; switch to ram mode
        andcc   #$af                            ; yes : re-enable inturrupts
        rts                                     ; return

romcopyend
        nop
        
romcopylen      equ     romcopyend-romcopy
		endc
                
__rsdos_end
