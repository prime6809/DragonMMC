;
; Mon : simple 6809 monitor
;

BuffSize    EQU     16                      ; Input buffer size

;
; Entered via a SWI
;

MonStart:
            leas    -BuffSize,s             ; Make room for input buffer
            leau    ,s                      ; Make u point at buffer
            
MonLoop:
            leax    MonPrompt,pcr           ; Point to prompt
            jsr     >CON_WriteString        ; print it
            
            leax    ,u                      ; Point to buffer
            ldb     #BuffSize               ; size of input buffer
            jsr     >CON_InputBufXB         ; go read a buffer

            bsr     MonSkipSpace            ; skip to first non space character
            beq     MonLoop                 ; reached end of line, prompt again
            
            leay    MonTable-3,pcr            ; point at command table
            
            lda     ,x                      ; get first character of command
MonScanLoop
            leay    3,y                     ; Move to next
            tst     ,y                      ; last command ?
            beq     MonLoop                 ; yes ignore try again
            
            cmpa    ,y                      ; Check command
            bne     MonScanLoop             ; not valid try next
            
            jsr     [1,y]                   ; jump to handler
            bra     MonLoop                 ; Loop again            


MonSkipSpace
            lda     #$20                    ; space
MonSkipSpaceLoop
            cmpa    ,x                      
            bne     MonSkipSpaceEnd         ; not space exit
            leax    1,x                     ; next char
            bra     MonSkipSpaceLoop        ; do next
            
MonSkipSpaceEnd
            tst     ,x                      ; set flags if zero
            rts

MonExit
            leas    2,s                     ; drop return address
MonDoExit
            leas    Buffsize,s              ; drop input buffer
            rti

MonDump     bsr     MonSkipSpace            ; skip to first non space character
            beq     MonDumpExit             ; end of string : exit

MonDumpExit
            rts
MonChange
            rts
          
MonPrompt
            fcb     $0d
            fcc     '>'
            fcb     $00
            
MonTable    
            fcb     'X'                     ; Exit
            fdb     MonExit
            
            fcb     'D'
            fdb     MonDump
            
            fcb     'C'
            fdb     MonChange
            
            fcb     $00
            fdb     $0000