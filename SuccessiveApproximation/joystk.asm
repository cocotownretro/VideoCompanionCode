        ORG     $3F00

; ---------------------------------------------------------------
; Successive approximation visualizer.
; Use J/K to position the joystick (blue line) left & right.
; That simulates a particular voltage level for an axis, which
; the successive approximation algorithm will attempt to measure.
; Then repeatedly hit space key to walk through each step of
; the algorithm.  When complete, you may reposition the joystick
; and run it again.  There is no way to exit the program,
; because you will never want to do anything else for the
; remainder of your days.
; ---------------------------------------------------------------


POLCAT  EQU     $A000           ; Address of ROM's keystroke getter
POTVAL  EQU     $015A 
SKP2    EQU     $8C             ; OP CODE OF CMPX # - SKIP TWO BYTES
PIA0    EQU     $FF00           ; PERIPHERAL INPUT ADAPTER #0
PIA1    EQU     $FF20           ; PERIPHERAL INPUT ADAPTER #1
DA      EQU     PIA1+0          ; DIGITAL/ANALOG CONVERTER
SCRNBUF EQU     $0400           ; Screen buffer start
BLACK   EQU     %10000000
BLUEUL  EQU     %10101110
BLUEUR  EQU     %10101101
BLUEU   EQU     %10101100
BLUEL   EQU     %10101010
BLUER   EQU     %10100101
BLUEBL  EQU     %10101011
BLUEBR  EQU     %10100111
BLUEB   EQU     %10100011
BLUEB1  EQU     %10100010
BLUEB2  EQU     %10100001
MAGUL   EQU     %11101110
MAGUR   EQU     %11101101
MAGU    EQU     %11101100
MAGU1   EQU     %11101000
MAGU2   EQU     %11100100
MAGL    EQU     %11101010
MAGR    EQU     %11100101
MAGBL   EQU     %11101011
MAGBR   EQU     %11100111
MAGB    EQU     %11100011

; ---------------------------------------------------------------
; Main init & loop
; ---------------------------------------------------------------
Main:
        ; Default joystick position
        LDA     #20
        STA     JoystkValToFind
MainLoop:
        BSR     InitScreen              ; Draw static portions of screen
        JSR     GetJoystkVis            ; Run visualizer
        JSR     WaitForKeystroke        ; Wait for user
        BRA     MainLoop                ; Repeat

; ---------------------------------------------------------------
; DrawJoyGraphics
; Update portion of screen showing joystick's position graphically
; ---------------------------------------------------------------
DrawJoyGraphics:
        ; Black it out first
        LDU     #SCRNBUF+(7*32)
        LDA     #BLACK
        LDB     #32
        JSR     PrintRepeatedChar
        ; Bar will have 1-64 dots on it, for the 0-63 joystk range
        LDB     JoystkValToFind
        INCB
        LDU     #SCRNBUF+(7*32)
        LDA     #BLUEB                  ; double dots (bottom side of char)
        LSRB                            ; divide by 2 for the double-dot chars
        JSR     PrintRepeatedChar
        LDB     JoystkValToFind         ; restore joystick value
        BITB    #1                      ; if odd...
        BNE     DJGDone                 ; ...nothing more to do
        LDA     #BLUEB1                 ; otherwise, 1 more dot
        STA     ,U+
DJGDone:
        RTS


; ---------------------------------------------------------------
; InitScreen
; Draw static portions of screen
; ---------------------------------------------------------------
InitScreen:
        ; Title at top
        LDX     #Title1
        LDU     #SCRNBUF
        JSR     PrintString
        LDX     #Title2
        JSR     PrintString
        ; Black stripes between sections
        LDA     #BLACK
        LDB     #32
        JSR     PrintRepeatedChar
        LDU     #SCRNBUF+(6*32)
        LDB     #32
        JSR     PrintRepeatedChar
        LDU     #SCRNBUF+(8*32)
        LDB     #64
        JSR     PrintRepeatedChar
        LDU     #SCRNBUF+(15*32)
        LDB     #32
        JSR     PrintRepeatedChar
        ; Magenta borders and text inside
        LDU     #SCRNBUF+(10*32)
        LDA     #MAGUL
        LDB     #1
        JSR     PrintRepeatedChar
        LDA     #MAGU
        LDB     #30
        JSR     PrintRepeatedChar
        LDA     #MAGUR
        LDB     #1
        JSR     PrintRepeatedChar
        LDA     #MAGL
        LDB     #1
        JSR     PrintRepeatedChar
        LDU     #SCRNBUF+(11*32+31)
        LDA     #MAGR
        LDB     #1
        JSR     PrintRepeatedChar
        LDA     #MAGL
        LDB     #1
        JSR     PrintRepeatedChar
        LDU     #SCRNBUF+(12*32+31)
        LDA     #MAGR
        LDB     #1
        JSR     PrintRepeatedChar
        LDA     #MAGL
        LDB     #1
        JSR     PrintRepeatedChar
        LDU     #SCRNBUF+(13*32+31)
        LDA     #MAGR
        LDB     #1
        JSR     PrintRepeatedChar
        LDU     #SCRNBUF+(14*32)
        LDA     #MAGBL
        LDB     #1
        JSR     PrintRepeatedChar
        LDA     #MAGB
        LDB     #30
        JSR     PrintRepeatedChar
        LDA     #MAGBR
        LDB     #1
        JSR     PrintRepeatedChar
        LDU     #SCRNBUF+(11*32+1)
        LDX     #AppTxt1
        JSR     PrintString
        LDU     #SCRNBUF+(12*32+1)
        LDX     #AppSpc
        JSR     PrintString
        LDU     #SCRNBUF+(13*32+1)
        LDX     #AppTxt2
        JSR     PrintString
ISDrawJoystkInfo:
        ; Blue borders and text inside
        LDU     #SCRNBUF+(3*32)
        LDA     #BLUEUL
        LDB     #1
        JSR     PrintRepeatedChar
        LDA     #BLUEU
        LDB     #30
        JSR     PrintRepeatedChar
        LDA     #BLUEUR
        LDB     #1
        JSR     PrintRepeatedChar
        LDA     #BLUEL
        LDB     #1
        JSR     PrintRepeatedChar
        LDU     #SCRNBUF+(4*32+31)
        LDA     #BLUER
        LDB     #1
        JSR     PrintRepeatedChar
        LDU     #SCRNBUF+(5*32)
        LDA     #BLUEBL
        LDB     #1
        JSR     PrintRepeatedChar
        LDA     #BLUEB
        LDB     #30
        JSR     PrintRepeatedChar
        LDA     #BLUEBR
        LDB     #1
        JSR     PrintRepeatedChar
        LDU     #SCRNBUF+(4*32+1)
        LDX     #JoyText
        JSR     PrintString
        LDB     JoystkValToFind
        JSR     PrintHexByte
        ; Blue line for joystick position
        JSR     DrawJoyGraphics
ISKeystroke:
        ; Allow user to reposition joystick L & R
        JSR     [POLCAT]                ; Any keystoke?
        BEQ     ISKeystroke             ; If not, loop
        CMPA    #'K'
        BEQ     ISIncJoy
        CMPA    #'J'
        BEQ     ISDecJoy
        RTS                             ; If not J & K, return
ISIncJoy:
        LDA     JoystkValToFind
        INCA
        CMPA    #63
        BHI     ISKeystroke
        STA     JoystkValToFind
        BRA     ISDrawJoystkInfo
ISDecJoy:
        LDA     JoystkValToFind
        DECA
        CMPA    #0
        BLT     ISKeystroke
        STA     JoystkValToFind
        BRA     ISDrawJoystkInfo

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
; GetJoystkVis
; JOYIN, but instrumented for visualizer
; To remain as close as possible to JOYIN, all values remain in
; upper 6 bits, but when displayed are temporarily shifted to
; lower 6 bits.
; ---------------------------------------------------------------
GetJoystkVis:
        LDB     JoystkValToFind
        LSLB
        LSLB                    ; B = joystk value shifted into upper 6 bits
        STB     ShiftedJoystkValToFind
        LDD     #$4080          
TestBitVis:
        BSR     UpdateScreen
        STA     ,-S             ; TEMP STORE SHIFT COUNTER ON STACK
        PSHS    B
        CMPB    ShiftedJoystkValToFind
        BSR     DisplayComparatorResult
        PULS    B
        CMPB    ShiftedJoystkValToFind
        BLO     OutputHighVis   ; BRANCH IF COMPARATOR OUTPUT IS HIGH (i.e., DAC < joystk)
; OutputLowVis:
        SUBB    ,S              ; SUBTRACT 1/2 THE CURRENT TRIAL DIFFERENCE
        FCB     SKP2            ; SKIP NEXT TWO BYTES (ADDB ,S)
OutputHighVis:
        ADDB    ,S              ; ADD 1/2 OF THE CURRENT TRIAL DIFFERENCE
        LDA     ,S+             ; PULL SHIFT COUNTER OFF THE STACK
        LSRA                    ; SHIFT IT RIGHT ONCE
        CMPA    #1              ; HAVE ALL THE SHIFTS BEEN DONE?
        BNE     TestBitVis      ; NO
        BSR     UpdateScreen
        LSRB                    ; YES - THE DATA IS IN THE TOP 6 BYTES OF ACCB
        LSRB                    ; PUT IT INTO THE BOTTOM SIX
        STB     ResultFound     ; SAVE THE DIGITIZED VALUE
        RTS

; ---------------------------------------------------------------
; DisplayComparatorResult
; Pauses, displays high/low text, pauses, deletes text
; Entry: CC contains result of comparator
; ---------------------------------------------------------------
DisplayComparatorResult:
        BLO     DCRHigh                 ; Prior CMP did reverse of hardware comparator, so high/low are reversed
; DCRLow:
        LDX     #LoTxt
DCRResume:
        JSR     WaitForKeystroke
        LDU     #SCRNBUF+(9*32)
        BSR     PrintString
        JSR     WaitForKeystroke
        LDU     #SCRNBUF+(9*32)
        LDA     #BLACK
        LDB     #32
        BSR     PrintRepeatedChar
        RTS
DCRHigh:
        LDX     #HiTxt
        BRA     DCRResume

; ---------------------------------------------------------------
; WaitForKeystroke
; Repeatedly calls POLCAT until key is pressed (result in A)
; ---------------------------------------------------------------
WaitForKeystroke:
        JSR     [POLCAT]                ; Any keystoke?
        BEQ     WaitForKeystroke        ; If not, loop
        RTS

; ---------------------------------------------------------------
; UpdateScreen
; Updates dynamic portions of screen
; Entry: A & B are adustment & guess from JOYIN algorithm
;        (upper 6 bits)
; ---------------------------------------------------------------
UpdateScreen:
        PSHS    D
        ; Shift B & A down 2, so they're the lower 6 bits
        LSRB
        LSRB
        LSRA
        LSRA
        PSHS    D                       ; Save shifted values
        BSR     UpdateBGraphics         ; Graphically show guess & adjustment
        PULS    D                       ; Restore shifted values
        LDU     #SCRNBUF+(11*32)+29     ; Print B value
        BSR     PrintHexByte
        LDU     #SCRNBUF+(13*32)+29     ; Print A value
        TFR     A,B
        BSR     PrintHexByte
        PULS    D
        RTS

; ---------------------------------------------------------------
; UpdateBGraphics
; Graphically show guess (B register)
; ---------------------------------------------------------------
UpdateBGraphics:
        PSHS    B
        ; Black out area first
        LDU     #SCRNBUF+(8*32)
        LDA     #BLACK
        LDB     #32
        JSR     PrintRepeatedChar
        ; B in range 0-63, represented with dot at position 1-64
        LDB     ,S
        LSRB                            ; Divide by 2 to skip to correct (double-dot) char position
        LDU     #SCRNBUF+(8*32)
        LEAU    B,U
        PULS    B                       ; Restore B
        BITB    #1                      ; If B is even...
        BEQ     UBGLeftSide             ; ...dot is on the left side
        LDB     #MAGU2                  ; Otherwise, odd B means right side
        FCB     SKP2                    ; Skip next instruction
UBGLeftSide:
        LDB     #MAGU1
        STB     ,U                      ; Draw single dot
        RTS

; ---------------------------------------------------------------
; PrintHexByte
; Prints value of B register as 2 hex characters
; Entry: B = byte to print
; Entry: U = screen location to begin printing at
; ---------------------------------------------------------------
PrintHexByte:
        PSHS    B
        ; High nibble
        LSRB
        LSRB
        LSRB
        LSRB
        BSR     PrintHexNibble
        ; Low nibble
        LDB     ,S
        ANDB    #%00001111
        BSR     PrintHexNibble
        PULS    B
        RTS

; ---------------------------------------------------------------
; PrintHexNibble
; Prints value of B register as single hex character
; Entry: B = nibble to print (upper 4 bits should be 0)
; Entry: U = screen location to begin printing at
; ---------------------------------------------------------------
PrintHexNibble:
        LDX     #NibbleToVideoCodeTable
        LDB     B,X
        STB     ,U+
        RTS

; ---------------------------------------------------------------
; Data
; ---------------------------------------------------------------
NibbleToVideoCodeTable:
        FCB     $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,1,2,3,4,5,6

Title1  FCN     "    successive approximation    "
Title2  FCN     "           visualizer           "
JoyText FCN     "joystick axis position:    $"
AppTxt1 FCN     "current approximation (b): $  "
AppTxt2 FCN     "next adjustment (a):       $  "
AppSpc  FCN     "                              "
HiTxt   FCN     "comparator returns:         high"
LoTxt   FCN     "comparator returns:         low "

JoystkValToFind         RMB     1
ShiftedJoystkValToFind  RMB     1
ResultFound             RMB     1

; ---------------------------------------------------------------
; Original JOYIN routine for reference (never called)
; ---------------------------------------------------------------
GetJoystkOrig:
        JSR     $A974           ; TURN OFF AUDIO
        pragma  cd
        pragma  ct
        pragma  cc
        LDX     #POTVAL+4       ; POINT X just after JOYSTICK DATA BUFFER
        LDB     #3              ; GET FOUR SETS OF DATA (4 JOYSTICKS)
ReadOneAxis:
        pragma  cc
        LDA     #10             ; 10 TRIES TO GET STABLE READING
        STD     ,--S            ; STORE JOYSTICK NUMBER AND TRY NUMBER ON THE STACK
;        JSR     $A9A2           ; SET THE SELECT INPUTS ON ANALOG MULTIPLEXER
TryAxisOnce:
        pragma  cc
        LDD     #$4080          ; ACCA IS A SHIFT COUNTER OF HOW MANY BITS TO CONVERT
                                ; AND WILL BE $40 (6 BITS) FOR THE COLOR
                                ; COMPUTER. ACCB CONTAINS A VALUE EQUAL TO 1/2
                                ; THE CURRENT TRIAL DIFFERENCE. INITIALLY =$80 (2.5 VOLTS).
TestBit:
        pragma  cc
        STA     ,-S             ; TEMP STORE SHIFT COUNTER ON STACK
        ORB     #2              ; KEEP RS 232 SERIAL OUT MARKING
        STB     DA              ; STORE IN D/A CONVERTER
        EORB    #2              ; PUT RS232 OUTPUT BIT BACK TO ZERO
        LDA     PIA0            ; HIGH BIT IS FROM COMPARATOR
        BMI     OutputHigh      ; BRANCH IF COMPARATOR OUTPUT IS HIGH
OutputLow:
        SUBB    ,S              ; SUBTRACT 1/2 THE CURRENT TRIAL DIFFERENCE
        FCB     SKP2            ; SKIP NEXT TWO BYTES (ADDB ,S)
OutputHigh:
        ADDB    ,S              ; ADD 1/2 OF THE CURRENT TRIAL DIFFERENCE
        LDA     ,S+             ; PULL SHIFT COUNTER OFF THE STACK
        LSRA                    ; SHIFT IT RIGHT ONCE
        CMPA    #1              ; HAVE ALL THE SHIFTS BEEN DONE?
        BNE     TestBit         ; NO
        pragma  cc
        LSRB                    ; YES - THE DATA IS IN THE TOP 6 BYTES OF ACCB
        LSRB                    ; PUT IT INTO THE BOTTOM SIX
        CMPB    -1,X            ; IS THIS VALUE EQUAL TO THE LAST TRY?
        BEQ     SaveAxis        ; YES - GO SAVE THE VALUE
        DEC     ,S              ; NO - DECREMENT TRIES COUNTER
        BNE     TryAxisOnce     ; BRANCH IF YOU HAVEN'T TRIED 10 TIMES
                                ; IF YOU FALL THROUGH HERE YOU HAVE TRIED TO GET THE SAME READING
                                ; 10 TIMES AND NEVER GOTTEN A MATCH. AS A RESULT YOU JUST FALL
                                ; THROUGH AND USE THE LAST VALUE READ IN.
        pragma  cc
SaveAxis:
        STB     ,-X             ; SAVE THE DIGITIZED VALUE
        LDD     ,S++            ; GET THE NUMBER OF THE JOYSTICK JUST DONE
        DECB                    ; DECR JOYSTK NUMBER
        BPL     ReadOneAxis     ; BRANCH IF THE LAST ONE DONE WASN'T NUMBER 0
        RTS
        

        END     $3F00
