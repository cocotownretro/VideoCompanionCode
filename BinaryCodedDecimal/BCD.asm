; ---------------------------------------------------------------
; BCD speed test & example code
; Displays incrementing digits on the PMODE 3 graphics screen
; in two ways.  1) Using the standard representation of unsigned
; numbers with division by powers of 10 to extract individual
; digits.  2) Using binary-coded decimal representation of
; numbers, via DAA instruction.
; ---------------------------------------------------------------

        ORG     $3F00
GRAPH   EQU     $E00
Yellow  EQU     $5555


; ---------------------------------------------------------------
; Main
; ---------------------------------------------------------------

        JSR     SetDisplayMode
        JSR     ClearScreen

        ; The two tests:
        JSR     CounterDivide
        JSR     CounterBCD

Loop    JMP     Loop


; ---------------------------------------------------------------
; SetDisplayMode
; Do equivalent of PMODE 3,1:SCREEN 1,0
; ---------------------------------------------------------------
SetDisplayMode:
        ; Set screen buffer start address to $E00
        STA     >$FFCC+0
        STA     >$FFCA+1
        STA     >$FFC8+1
        STA     >$FFC6+1
        ; Set SAM control bits (to determine screen buffer size)
        STA     >$FFC0   ; SAM0
        STA     >$FFC3   ; SAM1
        STA     >$FFC5   ; SAM2
        ; Set VDG operating mode
        LDA     #$E0
        STA     >$FF22
        RTS

; ---------------------------------------------------------------
; Yellow, yellow, everywhere
; ---------------------------------------------------------------
ClearScreen:
        LDU     #GRAPH
        LDD     #Yellow
ClearLoop:
        STD     ,U++
        CMPU    #GRAPH+$1800
        BLO     ClearLoop
        RTS


;------------------------------------------------------------------------------------
; Counter comparison: divide vs. bcd
; U -> screen buffer location
; D = number to draw
;------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------
; CounterDivide
; Draws incrementing counter using standard-format bytes and division to
; extract each digit
;------------------------------------------------------------------------------------

CounterDivide:
        LDD     #0                      ; Start at 0
        PSHS    D                       ; Preserve D, as division routine modifies it to return remainder
        PSHS    X                       ; Division routine needs divisor in memory, so reserve stack space to hold it
CounterDivLoop:
        LDU     #GRAPH+(32*3)+2         ; U points to left-most digit's location in screen buffer
        ; Divide D by powers of 10 to extract and draw each digit.
        ; Each time D holds the previous divison's remainder,
        ; so it's ready to be divided again by the next
        ; power of 10
        LDX     #1000                   ; Divisor = 1000
        STX     ,S                      ; Store divisor on stack                       
        BSR     DivideAndDraw           ; D goes from dividend to remainder, ready to be divided by next lower power of 10
        LEAU    1,U                     ; Next screen location
        LDX     #100                    ; Divisor = 100
        STX     ,S                      ; Store divisor on stack
        BSR     DivideAndDraw           ; D goes from dividend to remainder, ready to be divided by next lower power of 10
        LEAU    1,U                     ; Next screen location
        LDX     #10                     ; Divisor = 10
        STX     ,S                      ; Store divisor on stack
        BSR     DivideAndDraw           ; D goes from dividend to remainder, ready to be divided by next lower power of 10
        LEAU    1,U                     ; Next screen location
        LDX     #1                      ; Divisor = 1
        STX     ,S                      ; Store divisor on stack
        BSR     DivideAndDraw           ; Final division to draw ones digit
        ; Increment and loop up
        LDD     2,S                     ; Restore D so we can increment it
        ADDD    #1
        STD     2,S
        CMPD    #9999
        BLS     CounterDivLoop
        PULS    D,X,PC


;------------------------------------------------------------------------------------
; DivideAndDraw
; Helper to divide D by divisor (2,S).  Quotient is drawn, and remainder
; is returned in D
;------------------------------------------------------------------------------------
DivideAndDraw:
        LDX     #NumeralTable           ; X -> beginning of byte data for drawing numerals
DDSubtractionLoop:
        CMPD    2,S                     ; D < divisor?
        BLO     DDDraw                  ; If yes, ready to draw
        SUBD    2,S                     ; Otherwise, D = D - divisor
        LEAX    7,X                     ; Advance X to next numeral's byte data (moral equivalent of adding 1 to the quotient)
        BRA     DDSubtractionLoop       ; Loop up
DDDraw:
        ; Numerals are 1 byte wide by 7 bytes tall.
        PSHS    B                       ; About to trash B (and thus D), so preserve
        LDB     ,X                      ; B = byte of top row of numeral
        STB     ,U                      ; Draw it
        LDB     1,X                     ; Continue with rest of rows
        STB     (1*32),U      
        LDB     2,X
        STB     (2*32),U      
        LDB     3,X
        STB     (3*32),U      
        LDB     4,X
        STB     (4*32),U      
        LDB     5,X
        STB     (5*32),U      
        LDB     6,X
        STB     (6*32),U      
        PULS    B,PC                    ; Exit

;------------------------------------------------------------------------------------
; CounterBCD
; Draws incrementing counter using BCD-formatted bytes and DAA to maintain the
; BCD format after each increment
;------------------------------------------------------------------------------------
CounterBCD:
        LDD     #0
        PSHS    D                       ; Reserve stack space for BCD-formatted number
CounterBCDLoop:
        ; At top of each iteration, stack contains BCD-format of number to
        ; draw: ,S -> high byte and 1,S -> low byte.  The A register already
        ; contains high byte at this point.
        LDU     #GRAPH+(32*3)+2
        ; Draw digits to screen
        ; High byte, High nibble
        ANDA    #%11110000              ; Grab high nibble of A = high byte
        LSRA
        LSRA
        LSRA
        LSRA
        BSR     WriteNibbleBCD          ; Draw
        ; High byte, Low nibble
        LEAU    1,U
        LDA     ,S                      ; restore A (high byte)
        ANDA    #%00001111              ; Grab low nibble of A
        BSR     WriteNibbleBCD          ; Draw
        LDA     1,S                     ; A = low-byte
        ; Low byte, high nibble
        LEAU    1,U
        ANDA    #%11110000              ; Grab high nibble of A
        LSRA
        LSRA
        LSRA
        LSRA
        BSR     WriteNibbleBCD          ; Draw
        ; low byte, Low nibble
        LEAU    1,U
        LDA     1,S                     ; Restore A = low byte
        ANDA    #%00001111              ; Grab low nibble of A
        BSR     WriteNibbleBCD          ; Draw
        ; Drawing complete.  Now, add 1 "BCD-style"
        LDA     1,S                     ; A = low byte
        ADDA    #1                      ; Add 1 to low byte
        DAA                             ; Reformat low byte to BCD
        STA     1,S                     ; preserve A in bcd form in low byte position
        LDA     ,S                      ; A = high byte
        ADCA    #0                      ; Transfer carry bit (if present) to high byte
        DAA                             ; Reformat high byte to BCD
        STA     ,S                      ; preserve A in bcd form in high byte position
        BNE     CounterBCDLoop          ; If high byte nonzero, we haven't wrapped around yet, so loop up
        TST     1,S                     ; Is low byte nonzero?
        BNE     CounterBCDLoop          ; If so, we haven't wrapped around yet, so loop up
        PULS    D,PC                    ; Both bytes wrapped around to 0, so we're done.  Exit



;------------------------------------------------------------------------------------
; WriteNibbleBCD
; Helper to draw numeral corresponding to the low nibble of A.  It is assumed
; the upper 4 bits of A are 0.
;------------------------------------------------------------------------------------
WriteNibbleBCD:
        ; Numerals are 1 byte wide by 7 bytes tall.
        ; A = 0-9 (numeral to draw)
        PSHS    D               ; Caller relies on its D, so preserve for duration of this routine
        LDB     #7              ; Each entry of numeral table is 7 bytes long
        MUL                     ; D = A * 7, so B = index into table
        LDX     #NumeralTable   ; X -> Beginning of numeral byte table
        ABX                     ; X -> bytes of digit to draw
        LDB     ,X              ; B = byte of top row of numeral
        STB     ,U              ; Draw it
        LDB     1,X             ; Continue with rest of rows
        STB     (1*32),U      
        LDB     2,X
        STB     (2*32),U      
        LDB     3,X
        STB     (3*32),U      
        LDB     4,X
        STB     (4*32),U      
        LDB     5,X
        STB     (5*32),U      
        LDB     6,X
        STB     (6*32),U      
        PULS    D,PC
                    

; Numerals are 1 byte wide by 7 bytes tall.
NumeralTable:
        FDB     $FDDD           ; 0
        FDB     $DDDD
        FDB     $DDDD
        FCB     $FD
        FDB     $5D5D           ; 1   
        FDB     $5D5D
        FDB     $5D5D
        FCB     $5D
        FDB     $FD5D           ; 2
        FDB     $5DFD
        FDB     $D5D5
        FCB     $FD
        FDB     $FD5D           ; 3
        FDB     $5DFD
        FDB     $5D5D
        FCB     $FD
        FDB     $DDDD           ; 4
        FDB     $DDFD
        FDB     $5D5D
        FCB     $5D
        FDB     $FDD5           ; 5
        FDB     $D5FD
        FDB     $5D5D
        FCB     $FD
        FDB     $FDD5           ; 6
        FDB     $D5FD
        FDB     $DDDD
        FCB     $FD
        FDB     $FD5D           ; 7
        FDB     $5D5D
        FDB     $5D5D
        FCB     $5D
        FDB     $FDDD           ; 8
        FDB     $DDFD
        FDB     $DDDD
        FCB     $FD
        FDB     $FDDD           ; 9
        FDB     $DDFD
        FDB     $5D5D
        FCB     $FD

        END     $3F00
