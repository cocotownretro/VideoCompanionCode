; ---------------------------------------------------------------
; Keyboard visualizer.  Demonstrates the query & response
; for manually polling the keyboard via PIA 1.  To adjust
; the polling speed, scroll to just above the "SyncLoop"
; label, and change the LDA #5 to a different number.  1 =
; no delay at all.  Larger numbers give larger delays, which
; let you see exactly what happens when a single column
; of the keyboard matrix is polled (and the other columns are
; waiting their turn).  For fun, try hitting the buttons on
; the left and right joysticks.
; ---------------------------------------------------------------
        ORG     $3F00

SKP2    EQU     $8C             ; OP CODE OF CMPX # - SKIP TWO BYTES
SCRNBUF EQU     $0400           ; Screen buffer start
BLUE    EQU     %10101111

; ---------------------------------------------------------------
; Main init & loop
; ---------------------------------------------------------------
Main:
        BSR     InitScreen              ; Draw static portions of screen

MainLoop:
        BSR     DoScan                  ; Do one full scan of entire keyboard
        BRA     MainLoop                ; Repeat

; ---------------------------------------------------------------
; InitScreen
; Draw static portions of screen
; ---------------------------------------------------------------
InitScreen:
        LDX     #Title
        LDU     #SCRNBUF
        JSR     PrintString
        LDA     #' '
        LDB     #32*4
        JSR     PrintRepeatedChar
        LDA     #BLUE
        LDB     #32
        JSR     PrintRepeatedChar
        LDA     #' '
        LDB     #32*5
        JSR     PrintRepeatedChar
        LDA     #' '
        LDB     #32*5
        JSR     PrintRepeatedChar
        LDA     #BLUE
        LDU     #SCRNBUF+(32*4)+1
        STA     ,U
        STA     32*1,U
        STA     32*2,U
        STA     32*3,U
        STA     32*4,U
        STA     32*5,U
        STA     32*6,U
        STA     32*7,U
        STA     32*8,U
        RTS

; ---------------------------------------------------------------
; PrintString
; Prints null-terminated string
; Entry: X -> string
; Entry: U -> Screen buffer location to print to
; ---------------------------------------------------------------
PrintString:
        LDA     ,X+
        TSTA
        BEQ     PrintStringEnd      ; Break out of loop on terminating null
        BSR     TweakChar
        STA     ,U+                 ; Print character
        BRA     PrintString
PrintStringEnd:
        RTS

; ---------------------------------------------------------------
; TweakChar
; Converts ASCII char to video display byte
; Entry: A = char
; Exit: A = video display byte
; ---------------------------------------------------------------
TweakChar:
        ; In lower-case letter range?
        CMPA    #'a'
        BLO     TweakLowerCaseLettersDone
        CMPA    #'z'
        BHI     TweakLowerCaseLettersDone
        ; We have a lower-case letter, so perform both tweaks
        ANDA    #$DF
        EORA    #$40
TweakLowerCaseLettersDone:
        ; Any other necessary tweaks can go here
        RTS

; ---------------------------------------------------------------
; PrintRepeatedChar
; Display char 0 or more times
; Entry: A = char
; Entry: B = # times
; Entry: U = screen location to start printing
; ---------------------------------------------------------------
PrintRepeatedChar:
        TSTB
        BEQ     PRCDone
PRCLoop:
        STA     ,U+                 ; Print character
        DECB
        BNE     PRCLoop
PRCDone:
        RTS

; ---------------------------------------------------------------
; DoScan
; Do a single full scan of the keyboard and illustrate on
; the screen what's going on
; ---------------------------------------------------------------
DoScan:
        LDA     #%01111111              ; Polling bits.  Start with leftmost column.
        LDX     #KeyNamesStart          ; X -> VDG bytes to display matrix, starting at leftmost column 
        STX     KeyNamesCur             ; Remember the current column
        LDY     #SCRNBUF+(6*32)+2       ; Y -> location on screen of matrix's current column
DSLoop:
        BSR     ByteToPrintableBinary   ; Convert A into printable VDG bytes of 0 and 1 of A's binary representation
        LDX     #BTPBOutput             ; X -> beginning of those printable bytes
        LDU     #SCRNBUF+(4*32)+3       ; U -> screen buffer to display those printable bytes (the row above the matrix)
        ; Loop: Print A's binary representation just above matrix's top row
DSPrintHeaderBinaryLoop:
        LDB     ,X+
        STB     ,U
        LEAU    4,U
        CMPX    #BTPBOutput+8
        BLO     DSPrintHeaderBinaryLoop
        ; Now, let's actually scan the keyboard
        PSHS    A                       ; Remember polling bits on stack
        STA     >$FF02                  ; Write our polling bits into PIA 1, DRB
        LDB     >$FF00                  ; Read the result from PIA 1, DRA
        PSHS    Y,B                     ; Save Y (screen location of current column) and B (keyboard scan result)
        LDA     ,S                      ; A = keyboard scan result
        BSR     ByteToPrintableBinary   ; Convert keyboard scan result into printable binary representation
        ; Loop: Print result as headers going down the first column
        LDX     #BTPBOutput+1           ; Skip highest bit, as it's unused
        LDU     #SCRNBUF+(6*32)
DSPrintColumnBinaryLoop:
        LDB     ,X+
        STB     ,U
        LEAU    32,U
        CMPX    #BTPBOutput+8
        BLO     DSPrintColumnBinaryLoop
        ; Highlight the pressed keys by scanning down the column
        ; we tested, and writing the key name in the proper
        ; fg/bg color
        LDA     #%01000000              ; A = bitmask to AND against the keyboard scan result
        LDX     KeyNamesCur             ; X -> beginning of printable VDG bytes for this column
DSHighlightLoop:
        BITA    ,S                      ; Apply bitmask to keyboard scan result
        BEQ     DSHighlight             ; If 0, the key is pressed, so use the highlighted version (last two bytes)
; DSUnhighlight
        LDB     ,X                      ; Else, use the unhighlighted version (first 2 bytes)
        STB     ,Y
        LDB     1,X
        STB     1,Y
DSFinishedKeySymbol:
        LEAX    4,X                     ; X -> beginning of printable VDG bytes for NEXT column
        LEAY    32,Y                    ; Y -> location on screen of matrix's NEXT column
        LSRA                            ; Shift the 1 in the bitmask to the right
        CMPY    #SCRNBUF+(13*32)        ; Done?
        BLO     DSHighlightLoop         ; No, loop
        PULS    B,Y                     ; Yes, restore B (unused) and Y (screen location of current column)
        ; loop to slow things down
        LDA     #5                      ; Change (or poke after load) to adjust the delay
SyncLoop:               
        SYNC                            ; wait
        DECA
        BNE     SyncLoop
        ; pause is complete, continue with loop
        LDX     KeyNamesCur             ; Advance X to printable names of next column
        LEAX    4*7,X
        STX     KeyNamesCur             ; Remember it
        LEAY    4,Y                     ; Advance Y to screen location of NEXT column
        PULS    A                       ; Restore PIA polling bits
        LSRA                            ; Next bit to poll against the PIA (high bit will erroneously be 0)
        CMPA    #%01111111              ; Have we cycled back around yet?
        BEQ     DSExit                  ; Yes, done
        ORA     #%10000000              ; No, continue looping, and make this an 8-bit rotate by fixing high bit to 1
        BRA     DSLoop
DSExit:
        RTS
DSHighlight:
        LDB     2,X
        STB     ,Y
        LDB     3,X
        STB     1,Y
        BRA     DSFinishedKeySymbol

; ---------------------------------------------------------------
; ByteToPrintableBinary
; Converts a byte to a string of 0s and 1s suitable for printing
; to the screen
; Entry: A = byte to convert
; Exit: BTPBOutput -> 8 VDG-printable bytes of 0s and 1s
; ---------------------------------------------------------------
ByteToPrintableBinary:
        PSHS    A                       ; Save input to stack
        LDB     #%10000000              ; Bitmask to test each bit of input
        LDU     #BTPBOutput             ; U->output string
BTPBLoop:
        BITB    ,S                      ; Test current bit
        BNE     BTPBBitSet              ; If 1, branch
        LDA     #48                     ; Otherwise, use 48 (VDG byte for 0)
        FCB     SKP2                    ; Skip LDA #49
BTPBBitSet:
        LDA     #49                     ; Use 49 (VDG byte for 1)
        STA     ,U+                     ; Save byte to output string
        LSRB                            ; Shift the bit we're testing
        BNE     BTPBLoop                ; If we haven't shifted the 1 into oblivion, keep looping
        PULS    A                       ; Restore input to A
        RTS



; ---------------------------------------------------------------
; Data
; ---------------------------------------------------------------


Title   FCN     "     keyboard visualizer        "
; Each line below represents a column in the matrix.  It takes 4 bytes to represent
; a single keyboard key.  The first two bytes are VDG-printable bytes for the key's name
; as green-on-black (unhighlighted).  Last 2 bytes are VDG-printable bytes for the
; key's name as black-on-green (highlighted).
KeyNamesStart:
        FCB     19,8,'S','H',32,'/',32,111,32,'7',32,119,19,16,'S','P',32,23,32,'W',32,15,32,'O',32,7,32,'G'
        FCB     32,32,32,32,32,'.',32,110,32,'6',32,118,18,20,'R','T',32,22,32,'V',32,14,32,'N',32,6,32,'F'
        FCB     32,32,32,32,32,'-',32,109,32,'5',32,117,12,20,'L','T',32,21,32,'U',32,13,32,'M',32,5,32,'E'
        FCB     32,32,32,32,32,',',32,108,32,'4',32,116,4,14,'D','N',32,20,32,'T',32,12,32,'L',32,4,32,'D'
        FCB     32,32,32,32,32,';',32,123,32,'3',32,115,21,16,'U','P',32,19,32,'S',32,11,32,'K',32,3,32,'C'
        FCB     2,18,'B','R',32,':',32,122,32,'2',32,114,32,26,32,'Z',32,18,32,'R',32,10,32,'J',32,2,32,'B'
        FCB     3,12,'C','L',32,'9',32,121,32,'1',32,113,32,25,32,'Y',32,17,32,'Q',32,9,32,'I',32,1,32,'A'
        FCB     5,14,'E','N',32,'8',32,120,32,'0',32,112,32,24,32,'X',32,16,32,'P',32,8,32,'H',32,0,32,64
        
KeyNamesCur     RMB     2               ; Place to remember current column (from KeyNamesStart) being scanned
BTPBOutput      RMB     8               ; Storage to hold output string from ByteToPrintableBinary


        END     $3F00
