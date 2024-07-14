; ---------------------------------------------------------------
;
; Binary Playground by CoCo Town.
;
; A tool to experiment with 6809 instructions that manipulate
; numbers, including complements, rotations, arithmetic, and
; binary coded decimal.
;
; https://youtu.be/wxeHAM090G0
;
; ---------------------------------------------------------------

        ORG     $3F00
        BRA     Main

POLCAT          EQU     $A000           ; Address of ROM's keystroke getter
SKP2            EQU     $8C             ; OP CODE OF CMPX # - SKIP TWO BYTES
SCRNBUF         EQU     $0400           ; Screen buffer start
BLUE            EQU     %10101111
KEY_UP          EQU     94
KEY_DN          EQU     10
KEY_LT          EQU     8
KEY_RT          EQU     9
KEY_SPACE       EQU     32
CURSOR_ABIN     EQU     %00000010
CURSOR_BBIN     EQU     %00000011
COM_CURSOR_BIN  EQU     %11111101
CURSOR_ADECS    EQU     %00000100
CURSOR_BDECS    EQU     %00000101
COM_CURSOR_DECS EQU     %11111011
CURSOR_ADECU    EQU     %00001000
CURSOR_BDECU    EQU     %00001001
COM_CURSOR_DECU EQU     %11110111
PE_DELAY_AMOUNT EQU     $0040           ; Delay factor for auto-repeat for the fast increment/decrement


; ---------------------------------------------------------------
; Main init & loop
; ---------------------------------------------------------------
Main:
        CLRA                            ; Init A and B to 0
        STA     A
        STA     B
        LDA     #CURSOR_ADECS           ; Cursor starts at A decimal signed
        STA     CursorLocType
        LDD     #PE_DELAY_AMOUNT        ; Init auto-repeat delay
        STD     PEDelay
        LDY     #PageInfos              ; Y permanently points to current page's info
MainLoop:
        JSR     [PageInfo.InitScreen,Y] ; Init current screen (print all static portions)
PageLoop:
        JSR     UpdateScreen            ; Update dynamic portions of screen
        JSR     RenderCursor            ; Highlight cursor position
PollKbd:
        JSR     PollAndExecFastKeys     ; Poll for auto-repeat keys
        BNE     ExecKeystroke           ; When complete, this either branches up to PageLoop or down to NextPage
        JSR     [POLCAT]                ; Poll for single-click keys
        BNE     ExecKeystroke           ; When complete, this either branches up to PageLoop or down to NextPage
        BRA     PollKbd                 ; Loop up until a serviceable keypress
NextPage:
        LEAY    sizeof{PageInfo},Y      ; User hit space, so advance Y to next page's PageInfo struct
        CMPY    #PageInfos_END          ; Last page?
        BLO     NextPageContinue        ; If no, skip
        LDY     #PageInfos              ; If yes, wrap around to beginning
NextPageContinue:
        LDB     PageInfo.BLocation,Y    ; Is there a B on the new page?
        BNE     MainLoop                ; If yes, just go loop up for this page
        LDB     #%11111110              ; If no, be sure we're on the A line before going
        ANDB    CursorLocType
        STB     CursorLocType
        BRA     MainLoop                ; Loop up for this page
ExecKeystroke:
        LDX     #KeystrokeActions       ; When a key is pressed, we jump here.  Grab the key dispatch table
EKScanActionLoop:
        CMPA    KeystrokeAction.Key,X   ; Search for entry matching pressed key
        BEQ     EKFoundMatch            ; Branch when found
        LEAX    sizeof{KeystrokeAction},X
        CMPX    #KeystrokeActions_END
        BLO     EKScanActionLoop
        BRA     PollKbd                 ; No match found, return to polling loop
EKFoundMatch:
        ; Execute key dispatch routine.  It will branch back to the right place when done
        JMP     [KeystrokeAction.Routine,X]


; ---------------------------------------------------------------
; Keyboard routines to dispatch to
; ---------------------------------------------------------------

; --- Move cursor to right ---
KACursorRight:
        LDB     CursorLocType
        BITB    #CURSOR_ABIN            ; Is it on a binary?
        BNE     KAMoveLowerBit          ; If yes, branch
        BITB    #CURSOR_ADECU           ; Is it on a DECU?
        BNE     PageLoop                ; If yes, exit (nothing to its right)
;KAMoveRToDecU:
        ANDB    #COM_CURSOR_DECS        ; Reset the DECS bit
        ORB     #CURSOR_ADECU           ; Set the DECU bit
        BRA     KASaveCursorTypeAndExit
KAMoveLowerBit:
        LDA     CursorLocBit            ; Get the bit of the binary we're on
        BEQ     KAMoveRToDecS           ; Branch if on bit 0
        DEC     CursorLocBit            ; Otherwise, just move to next lower bit
        BRA     PageLoop                ; Exit
KAMoveRToDecS:
        ANDB    #COM_CURSOR_BIN         ; Reset the BIN bit
        ORB     #CURSOR_ADECS           ; Set the DECS bit
        BRA     KASaveCursorTypeAndExit

; --- Move cursor to left ---
KACursorLeft:
        LDB     CursorLocType
        BITB    #CURSOR_ABIN            ; Is it on a binary?
        BNE     KAMoveHigherBit         ; If yes, branch
        BITB    #CURSOR_ADECS           ; Is it on a DECS?
        BNE     KAMoveLToBin            ; If yes, branch
;KAMoveLToDecS:
        ANDB    #COM_CURSOR_DECU        ; Reset the DECU bit
        ORB     #CURSOR_ADECS           ; Set the DECS bit
        BRA     KASaveCursorTypeAndExit
KAMoveHigherBit:
        LDA     CursorLocBit            ; Get the bit of the binary we're on
        CMPA    #7                      ; Leftmost bit already?
        LBEQ    PageLoop                ; If yes, exit
        INC     CursorLocBit            ; Otherwise, just move to next higher bit
        JMP     PageLoop
KAMoveLToBin:
        CLR     CursorLocBit            ; Bit 0
        ANDB    #COM_CURSOR_DECS        ; Reset the DECS bit 
        ORB     #CURSOR_ABIN            ; Set the BIN bit
KASaveCursorTypeAndExit:
        STB     CursorLocType
        JMP     PageLoop

; --- Move cursor up ---
KACursorUp:
        LDB     CursorLocType
        BITB    #1                      ; Are we already on A?
        LBEQ    PageLoop                ; Yes, nowhere to go
        ANDB    #%11111110              ; No, go up
        STB     CursorLocType
        JMP     PageLoop

; --- Move cursor down ---
KACursorDown:
        LDB     PageInfo.BLocation,Y    ; Is there even a B to move down to?
        LBEQ    PageLoop                ; Nope, exit
        LDB     CursorLocType
        BITB    #1                      ; Are we already on B?
        LBNE    PageLoop                ; Yes, nowhere to go
        ORB     #1                      ; No, go down
        STB     CursorLocType
        JMP     PageLoop

; --- Increment ---
KAIncrement:
        LDB     CursorLocType
        BITB    #CURSOR_ABIN            ; Is it on a binary?
        BNE     KAIncBit                ; If yes, branch
        BITB    #1                      ; Else, it's on a decimal.  A or B?
        BEQ     KAIncA                  ; Branch if A
; KAIncB:
        INC     B                       ; Increment B
        JMP     PageLoop                ; Exit
KAIncA:
        INC     A                       ; Increment A
        JMP     PageLoop                ; Exit
KAIncBit:
        BITB    #1                      ; A or B?
        BEQ     KAIncABit               ; Branch if A
; KAIncBBit:
        LDA     B
        JSR     SetBit
        STA     B
        JMP     PageLoop                ; Exit
KAIncABit:
        LDA     A
        JSR     SetBit
        STA     A
        JMP     PageLoop                ; Exit

; --- Decrement ---
KADecrement:
        LDB     CursorLocType
        BITB    #CURSOR_ABIN            ; Is it on a binary?
        BNE     KADecBit                ; If yes, branch
        BITB    #1                      ; Else, it's on a decimal.  A or B?
        BEQ     KADecA                  ; Branch if A
; KADecB:
        DEC     B                       ; Decrement B
        JMP     PageLoop                ; Exit
KADecA:
        DEC     A                       ; Decrement A
        JMP     PageLoop                ; Exit
KADecBit:
        BITB    #1                      ; A or B?
        BEQ     KADecABit               ; Branch if A
; KADecBBit:
        LDA     B
        JSR     ClearBit
        STA     B
        JMP     PageLoop                ; Exit
KADecABit:
        LDA     A
        JSR     ClearBit
        STA     A
        JMP     PageLoop                ; Exit

; --- Next Page ---
KANextPage:
        JMP     NextPage

; --- Show Help ---
KAHelp:
        LDU     #SCRNBUF
        LDX     #Help01
        JSR     PrintString
PollForReturn:
        JSR     [POLCAT]
        CMPA    #' '
        BNE     PollForReturn
        JMP     MainLoop


; ---------------------------------------------------------------
; SetBit
; Entry: CursorLocBit = bit number (0-7) to set
; Entry: A = current value of "A byte"
; Exit: A = updated value of "A byte" with specified bit now set
; ---------------------------------------------------------------
SetBit:
        LDB     CursorLocBit
        LDX     #BitToSetMask
        ORA     B,X
        RTS

; ---------------------------------------------------------------
; ClearBit
; Entry: CursorLocBit = bit number (0-7) to clear
; Entry: A = current value of "A byte"
; Exit: A = updated value of "A byte" with specified bit now clear
; ---------------------------------------------------------------
ClearBit:
        LDB     CursorLocBit
        LDX     #BitToClearMask
        ANDA    B,X
        RTS


; ---------------------------------------------------------------
; PollAndExecFastKeys
; Used for auto-repeat inc/dec keys.  Instead of polcat, this
; polls the PIA directly.
; Exit: A = key held down (0 if none)
; ---------------------------------------------------------------
PollAndExecFastKeys:
        ; Only do something every N times this is called;
        ; otherwise it's just too fast
        LDD     PEDelay
        SUBD    #1
        STD     PEDelay
        BNE     PFReturn0
        LDD     #PE_DELAY_AMOUNT
        STD     PEDelay
        ; Poll for 'S':
        LDA     #%11110111
        STA     >$FF02                  ; Write our polling bits into PIA 1, DRB
        LDA     >$FF00                  ; Read the result from PIA 1, DRA
        BITA    #%00000100
        BEQ     PFReturnS
        ; Poll for 'X':
        LDA     #%11111110
        STA     >$FF02                  ; Write our polling bits into PIA 1, DRB
        LDA     >$FF00                  ; Read the result from PIA 1, DRA
        BITA    #%00001000
        BEQ     PFReturnX
PFReturn0:
        ; Neither S nor X pressed
        CLRA
        RTS
PFReturnX:
        LDA     #'X'
        RTS
PFReturnS:
        LDA     #'S'
        RTS

; ---------------------------------------------------------------
; RenderCursor
; ---------------------------------------------------------------
RenderCursor:
        LDB     CursorLocType                   ; Switch based on the cursor's location type
        LSLB                                    ; *2 for use as table index
        LDX     #RenderCursorTbl
        JMP     [B,X]                           ; Table tells us where below to branch
RC_ABin:
        LDU     PageInfo.ALocation,Y            ; A: one of its binary bits
RC_AdjustBinBit:
        LDA     #7                              ; Adjust U based on which bit to highlighted
        SUBA    CursorLocBit
        LEAU    A,U
        LDA     #1                              ; Highlight only one char
        BRA     DoRender
RC_ADecS:
        LDU     PageInfo.ALocation,Y            ; A: decimal signed
RC_AdjustDecS:
        ; For demos, uncomment to skip highlighting when on A signed
        * RTS
        LEAU    8+1,U                           ; Adjust U accordingly
        LDA     #4                              ; Highlight 4 chars
        BRA     DoRender
RC_ADecU:
        LDU     PageInfo.ALocation,Y            ; A: decimal unsigned
RC_AdjustDecU:
        LEAU    8+1+4+1,U                       ; Adjust U accordingly
        LDA     #3                              ; Highlight 4 chars
        BRA     DoRender
RC_BBin:                                        ; Reuse code above, but for B
        LDU     PageInfo.BLocation,Y
        BRA     RC_AdjustBinBit
RC_BDecS:
        ; For demos, uncomment to skip highlighting when on B signed
        * RTS
        LDU     PageInfo.BLocation,Y
        BRA     RC_AdjustDecS
RC_BDecU:
        LDU     PageInfo.BLocation,Y
        BRA     RC_AdjustDecU
DoRender:
        ; All above branches eventually come down to here to highlight
        LDB     ,U
        ORB     #64                             ; Highlight char at U
        STB     ,U+
        DECA
        BNE     DoRender                        ; Repeat A times
        RTS

; ---------------------------------------------------------------
; ClearScreen
; ---------------------------------------------------------------
ClearScreen:
        LDX     #$2020
        LDU     #SCRNBUF
CSEraseLoop:
        ; Erase screen to black
        STX     ,U++
        CMPU    #SCRNBUF+$200
        BLO     CSEraseLoop
        ; Print title & help string
        LDX     #Title
        LDU     #SCRNBUF
        JSR     PrintString
        LDX     #Help
        JSR     PrintString
        RTS


; ---------------------------------------------------------------
; UpdateScreen
; Renders A & B values, then calls page-specific UpdateScreen
; ---------------------------------------------------------------
UpdateScreen:
        LDU     PageInfo.ALocation,Y 
        LDA     A
        BSR     WriteByte
        LDU     PageInfo.BLocation,Y
        BEQ     USDoneWithAB
        LDA     B
        BSR     WriteByte
USDoneWithAB:
        JMP     [PageInfo.UpdateScreen,Y]       ; Callee will RTS


; ---------------------------------------------------------------
; WriteByte
; Writes binary, signed decimal, and unsigned decimal form of byte:
; Example format:     10000101 -123 133 
; Entry: A = byte to write
; Entry: U -> location to begin writing
; ---------------------------------------------------------------
WriteByte:
        BSR     WriteByteBinary
        LDB     #' '
        STB     ,U+
        PSHS    A
        BSR     WriteByteDecimal
        PULS    A
        ; On the BCD screen, also display the BCD form
        LDX     PageInfo.InitScreen,Y
        CMPX    #InitScreenBCD
        BNE     WBDone
        LDB     #' '
        STB     ,U+
        STB     ,U+
        BSR     WriteByteBCD
WBDone:
        RTS


; ---------------------------------------------------------------
; WriteByteBinary
; Entry: A = byte to write
; Entry: U -> location to begin writing
; ---------------------------------------------------------------
WriteByteBinary:
        PSHS    U
        JSR     ByteToPrintableBinary
        PULS    U
        LDX     #BTPBOutput
WBBPrintLoop:
        LDB     ,X+
        STB     ,U+
        CMPX    #BTPBOutput+8
        BLO     WBBPrintLoop
        RTS


; ---------------------------------------------------------------
; WriteByteDecimal
; Writes the signed decimal and unsigned decimal form of byte:
; Example format:     -123 133 
; Entry: A = byte to write
; Entry: U -> location to begin writing
; ---------------------------------------------------------------
WriteByteDecimal:
        PSHS    A
        BSR     WriteByteDecimalSigned
        LDB     #' '
        STB     ,U+
        PULS    A
        BRA     WriteByteDecimalUnsigned        ; Callee will RTS

; ---------------------------------------------------------------
; WriteByteDecimalSigned
; Entry: A = byte to write
; Entry: U -> location to begin writing
; ---------------------------------------------------------------
WriteByteDecimalSigned:
        TSTA                            ; >= 0?
        BGE     WBUnsigned              ; If yes, branch
        NEGA                            ; If no, twos complement
        LDB     #'-'                    ; Print negative sign
        STB     ,U+
        BSR     WriteByteDecimalUnsigned ; Print absolute value
        RTS
WBUnsigned:
        LDB     #' '                    ; Left-pad with space (instead of -)
        STB     ,U+
        BRA     WriteByteDecimalUnsigned ; Just call WriteByteDecimalUnsigned; it will RTS


; ---------------------------------------------------------------
; WriteByteDecimalUnsigned
; Entry: A = byte to write
; Entry: U -> location to begin writing
; ---------------------------------------------------------------
WriteByteDecimalUnsigned:
        TFR     A,B
        CLRA
        LDX     #PowersOfTen+4          ; Division starts at 100
        BRA     WriteDecimalUnsigned    ; Callee will RTS


; ---------------------------------------------------------------
; Writes the BCD form of byte:
; Example format:     58
; Entry: A = byte to write
; Entry: U -> location to begin writing
; ---------------------------------------------------------------
WriteByteBCD:
        PSHS    A
        ; High nibble
        ANDA    #%11110000
        LSRA
        LSRA
        LSRA
        LSRA
        BSR     WriteNibbleBCD
        ; Low nibble
        PULS    A
        ANDA    #%00001111
        BSR     WriteNibbleBCD
        RTS

WriteNibbleBCD:
        CMPA    #9
        BGT     WNBInvalid
        LDX     #VdgDigits
        LDA     A,X
        STA     ,U+
        RTS
WNBInvalid:
        LDX     #QuestionMark
        BSR     PrintString
        RTS

; ---------------------------------------------------------------
; WriteWordBinary
; Entry: D (memory, not register) = word to write
; Entry: U -> location to begin writing
; ---------------------------------------------------------------
WriteWordBinary:
        LDA     D
        BSR     WriteByteBinary                 ; Write high byte in binary
        LDA     #' '                            ; Write space
        STA     ,U+
        LDA     D+1
        BRA     WriteByteBinary                 ; Write low byte in binary, callee will RTS

; ---------------------------------------------------------------
; WriteByteDecimal
; Writes the signed decimal and unsigned decimal form of word:
; Example format:     -123 133 
; Entry: D (memory, not register) = word to write
; Entry: U -> location to begin writing
; ---------------------------------------------------------------
WriteWordDecimal:
        BSR     WriteWordDecimalSigned          ; Write signed
        LDB     #' '                            ; Write space
        STB     ,U+                             
        BRA     WriteWordDecimalUnsigned        ; Write unsigned; callee will RTS

; ---------------------------------------------------------------
; WriteWordDecimalSigned
; Entry: D (memory, not register) = word to write
; Entry: U -> location to begin writing
; ---------------------------------------------------------------
WriteWordDecimalSigned:
        LDD     D
;        TSTA                            ; >= 0?  (Testing high byte of D is sufficient for this)
        BGE     WWUnsigned              ; If yes, branch
        LDB     #'-'                    ; Print negative sign
        STB     ,U+
        LDD     #0                      ; Twos complement (no NEGD in 6809, so doing this manually)
        SUBD    D
        BSR     WriteRegWordDecimalUnsigned ; Print absolute value
        RTS
WWUnsigned:
        LDB     #' '                    ; Left-pad with space (instead of -)
        STB     ,U+
        BRA     WriteWordDecimalUnsigned ; D >= 0 so just call WriteWordDecimalUnsigned; it will RTS


; ---------------------------------------------------------------
; WriteWordDecimalUnsigned
; Entry: D (memory, not register) = word to write
; Entry: U -> location to begin writing
; ---------------------------------------------------------------
WriteWordDecimalUnsigned:
        LDD     D
; Assuming D register has number to write, this just uses that
WriteRegWordDecimalUnsigned:
        LDX     #PowersOfTen            ; Division starts at 10000
        BRA     WriteDecimalUnsigned    ; Callee will RTS


; ---------------------------------------------------------------
; WriteDecimalUnsigned
; Common code used by the word and byte versions
; Entry: X -> first power of 10 to divide (depends on whether
;        caller is writing a byte or a word)
; Entry: D (register) = byte or word to write
; Entry: U -> location to begin writing
; ---------------------------------------------------------------
WriteDecimalUnsigned:
        STX     Divisor                 ; Divisor -> power of 10 to divide by
        PSHS    X                       ; Save pointer
        BSR     Divide                  ; D / [Divisor]
        PSHS    B                       ; Save B
        LDB     ,X                      ; Find VDG digit to print
        STB     ,U+                     ; Print quotient
        PULS    B,X                     ; Restore B (thus D) and X
        LEAX    2,X                     ; Advance to next power of 10
        CMPX    #PowersOfTen_END        ; Continue through 10^0
        BLO     WriteDecimalUnsigned
        RTS


; ---------------------------------------------------------------
; Divide
; Entry: D (register) = dividend
; Entry: Divisor -> divisor
; Exit: X points to VDG-printable byte for quotient
;       (assumed to be single digit)
; Exit: D (register) = remainder
; ---------------------------------------------------------------
Divide:
        LDX     #VdgDigits              ; X -> VDG bytes for printing digits
DLoop:
        CMPD    [Divisor]               ; D < [Divisor]?
        BLO     DExit                   ; If yes, we're done
        LEAX    1,X                     ; Otherwise, increment "quotient"
        SUBD    [Divisor]               ; And subtract out [Divisor]
        BRA     DLoop                   ; Continue looping the subtraction & counting
DExit:
        RTS
Divisor RMB     2                       ; Pointer to divisor


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
        LDA     #'0'                    ; Otherwise, use VDG byte for 0
        FCB     SKP2                    ; Skip LDA #'1'
BTPBBitSet:
        LDA     #'1'                    ; Use VDG byte for 1
        STA     ,U+                     ; Save byte to output string
        LSRB                            ; Shift the bit we're testing
        BNE     BTPBLoop                ; If we haven't shifted the 1 into oblivion, keep looping
        PULS    A                       ; Restore input to A
        RTS


; ---------------------------------------------------------------
; WriteHNZVC
; Entry: CC (mem location, not register) = bits to print
; Entry: U -> screen location to begin printing
; ---------------------------------------------------------------
WriteHNZVC:
        LDB     #%00100000
        BSR     WriteCCBit
        ; Fall through to WriteNZVC

; ---------------------------------------------------------------
; WriteNZVC
; Entry: CC (mem location, not register) = bits to print
; Entry: U -> screen location to begin printing
; ---------------------------------------------------------------
WriteNZVC:
        LDB     #%00001000
        BSR     WriteCCBit
        LDB     #%00000100
        BSR     WriteCCBit
        LDB     #%00000010
        BSR     WriteCCBit
        LDB     #%00000001
        BRA     WriteCCBit              ; Callee will RTS
WriteCCBit:
        BITB    CC                      ; Test CC bit requested from B
        BNE     WPrint1                 ; Branch if 1
        LDB     #'0'                    ; Else, prep to write 0
        FCB     SKP2                    ; Skip LDB #'1'
WPrint1:
        LDB     #'1'                    ; Prep to write 1
        STB     ,U+                     ; Do write
        RTS


; ---------------------------------------------------------------
; PreserveCC
; ---------------------------------------------------------------
PreserveCC:
        TFR     CC,B
        STB     CC
        RTS


; ---------------------------------------------------------------
; **** COM / NEG Screen routines ****
; ---------------------------------------------------------------

; ---------------------------------------------------------------
; InitScreenCOM
; ---------------------------------------------------------------
InitScreenCOM:
        JSR     ClearScreen
        LDX     #HeaderA
        LDU     #SCRNBUF+(3*32)+4
        JSR     PrintString
        LDX     #NZVC
        LDU     #SCRNBUF+(5*32)+27
        JSR     PrintString
        LDX     #HeaderCOM
        LDU     #SCRNBUF+(6*32)+2
        JSR     PrintString
        LDX     #HeaderNEG
        LDU     #SCRNBUF+(8*32)+2
        JSR     PrintString
        RTS


; ---------------------------------------------------------------
; UpdateScreenCOM
; ---------------------------------------------------------------
UpdateScreenCOM:
        ; Write COM
        LDA     A
        COMA
        BSR     PreserveCC
        LDU     #SCRNBUF+(6*32)+7
        JSR     WriteByte
        LDU     #SCRNBUF+(6*32)+27
        JSR     WriteNZVC
        ; Write NEG
        LDA     A
        NEGA
        BSR     PreserveCC
        LDU     #SCRNBUF+(8*32)+7
        JSR     WriteByte                       
        LDU     #SCRNBUF+(8*32)+27
        JMP     WriteNZVC                       ; Callee will RTS


; ---------------------------------------------------------------
; **** Arithmetic Screen routines ****
; ---------------------------------------------------------------

; ---------------------------------------------------------------
; InitScreenARITH
; ---------------------------------------------------------------
InitScreenARITH:
        JSR     ClearScreen
        LDX     #HeaderA
        LDU     #SCRNBUF+(3*32)+11
        JSR     PrintString
        LDX     #HeaderB
        LDU     #SCRNBUF+(4*32)+11
        JSR     PrintString
        LDX     #HNZVC
        LDU     #SCRNBUF+(6*32)+27
        JSR     PrintString
        LDX     #HeaderADD
        LDU     #SCRNBUF+(6*32)+0
        JSR     PrintString
        LDX     #HeaderSUB
        LDU     #SCRNBUF+(8*32)+0
        JSR     PrintString
        LDX     #HeaderMUL
        LDU     #SCRNBUF+(10*32)+0
        JSR     PrintString
        LDX     #HeaderSEX
        LDU     #SCRNBUF+(13*32)+0
        JSR     PrintString
        RTS


; ---------------------------------------------------------------
; UpdateScreenARITH
; ---------------------------------------------------------------
UpdateScreenARITH:
        ; Write ADDA
        LDA     A
        ADDA    B
        JSR     PreserveCC
        LDU     #SCRNBUF+(6*32)+14
        JSR     WriteByteBinary
        LDU     #SCRNBUF+(7*32)+14
        JSR     WriteByteDecimal        
        LDU     #SCRNBUF+(7*32)+27
        JSR     WriteHNZVC
        ; Write SUBA
        LDA     A
        SUBA    B
        JSR     PreserveCC
        LDU     #SCRNBUF+(8*32)+14
        JSR     WriteByteBinary
        LDU     #SCRNBUF+(9*32)+14
        JSR     WriteByteDecimal        
        LDU     #SCRNBUF+(9*32)+27
        JSR     WriteHNZVC
        ; Write MUL
        LDA     A
        LDB     B
        MUL
        STD     D
        JSR     PreserveCC
        LDU     #SCRNBUF+(11*32)+5
        JSR     WriteWordBinary
        LDU     #SCRNBUF+(12*32)+10
        JSR     WriteWordDecimal   
        LDU     #SCRNBUF+(12*32)+27
        JSR     WriteHNZVC
        ; Write SEX
        LDB     B
        SEX
        STD     D
        JSR     PreserveCC
        LDU     #SCRNBUF+(14*32)+5
        JSR     WriteWordBinary
        LDU     #SCRNBUF+(15*32)+10
        JSR     WriteWordDecimal   
        LDU     #SCRNBUF+(15*32)+27
        JSR     WriteHNZVC
        RTS


; ---------------------------------------------------------------
; **** Rotation Left Screen routines ****
; ---------------------------------------------------------------

; ---------------------------------------------------------------
; InitScreenROTL
; ---------------------------------------------------------------
InitScreenROTL:
        JSR     ClearScreen
        LDX     #HeaderA
        LDU     #SCRNBUF+(3*32)+5
        JSR     PrintString
        LDX     #HeaderROLA1
        LDU     #SCRNBUF+(5*32)+0
        JSR     PrintString
        LDX     #HeaderROLA2
        LDU     #SCRNBUF+(6*32)+0
        JSR     PrintString
        LDX     #HeaderROLA3
        LDU     #SCRNBUF+(7*32)+0
        JSR     PrintString
        LDX     #HeaderLSLA1
        LDU     #SCRNBUF+(9*32)+0
        JSR     PrintString
        LDX     #HeaderLSLA2
        LDU     #SCRNBUF+(10*32)+0
        JSR     PrintString
        LDX     #HeaderLSLA3
        LDU     #SCRNBUF+(11*32)+0
        JSR     PrintString
        LDX     #HeaderASLA1
        LDU     #SCRNBUF+(13*32)+0
        JSR     PrintString
        LDX     #HeaderASLA2
        LDU     #SCRNBUF+(14*32)+0
        JSR     PrintString
        LDX     #HeaderASLA3
        LDU     #SCRNBUF+(15*32)+0
        JSR     PrintString
        LDX     #NZVC
        LDU     #SCRNBUF+(4*32)+28
        JSR     PrintString
        RTS


; ---------------------------------------------------------------
; UpdateScreenROTL
; ---------------------------------------------------------------
UpdateScreenROTL:
        ; RORL
        LDA     A
        ANDCC   #%11111110                      ; Clear carry flag
        ROLA
        PSHS    CC
        JSR     PreserveCC
        LDU     #SCRNBUF+(5*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(5*32)+28
        JSR     WriteNZVC
        PULS    A
        PULS    CC
        ROLA
        PSHS    CC
        JSR     PreserveCC
        LDU     #SCRNBUF+(6*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(6*32)+28
        JSR     WriteNZVC
        PULS    A
        PULS    CC
        ROLA
        JSR     PreserveCC
        LDU     #SCRNBUF+(7*32)+8
        JSR     WriteByte
        LDU     #SCRNBUF+(7*32)+28
        JSR     WriteNZVC
        ; LSLA
        LDA     A
        LSLA
        JSR     PreserveCC
        LDU     #SCRNBUF+(9*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(9*32)+28
        JSR     WriteNZVC
        PULS    A
        LSLA
        JSR     PreserveCC
        LDU     #SCRNBUF+(10*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(10*32)+28
        JSR     WriteNZVC
        PULS    A
        LSLA
        JSR     PreserveCC
        LDU     #SCRNBUF+(11*32)+8
        JSR     WriteByte
        LDU     #SCRNBUF+(11*32)+28
        JSR     WriteNZVC
        ; ASLA
        LDA     A
        ASLA
        JSR     PreserveCC
        LDU     #SCRNBUF+(13*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(13*32)+28
        JSR     WriteNZVC
        PULS    A
        ASLA
        JSR     PreserveCC
        LDU     #SCRNBUF+(14*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(14*32)+28
        JSR     WriteNZVC
        PULS    A
        ASLA
        JSR     PreserveCC
        LDU     #SCRNBUF+(15*32)+8
        JSR     WriteByte
        LDU     #SCRNBUF+(15*32)+28
        JSR     WriteNZVC
        RTS

; ---------------------------------------------------------------
; **** Rotation Right Screen routines ****
; ---------------------------------------------------------------

; ---------------------------------------------------------------
; InitScreenROTR
; ---------------------------------------------------------------
InitScreenROTR:
        JSR     ClearScreen
        LDX     #HeaderA
        LDU     #SCRNBUF+(3*32)+5
        JSR     PrintString
        LDX     #HeaderRORA1
        LDU     #SCRNBUF+(5*32)+0
        JSR     PrintString
        LDX     #HeaderRORA2
        LDU     #SCRNBUF+(6*32)+0
        JSR     PrintString
        LDX     #HeaderRORA3
        LDU     #SCRNBUF+(7*32)+0
        JSR     PrintString
        LDX     #HeaderLSRA1
        LDU     #SCRNBUF+(9*32)+0
        JSR     PrintString
        LDX     #HeaderLSRA2
        LDU     #SCRNBUF+(10*32)+0
        JSR     PrintString
        LDX     #HeaderLSRA3
        LDU     #SCRNBUF+(11*32)+0
        JSR     PrintString
        LDX     #HeaderASRA1
        LDU     #SCRNBUF+(13*32)+0
        JSR     PrintString
        LDX     #HeaderASRA2
        LDU     #SCRNBUF+(14*32)+0
        JSR     PrintString
        LDX     #HeaderASRA3
        LDU     #SCRNBUF+(15*32)+0
        JSR     PrintString
        LDX     #NZVC
        LDU     #SCRNBUF+(4*32)+28
        JSR     PrintString
        RTS


; ---------------------------------------------------------------
; UpdateScreenROTR
; ---------------------------------------------------------------
UpdateScreenROTR:
        ; RORA
        LDA     A
        ANDCC   #%11111110                      ; Clear carry flag
        RORA
        PSHS    CC
        JSR     PreserveCC
        LDU     #SCRNBUF+(5*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(5*32)+28
        JSR     WriteNZVC
        PULS    A
        PULS    CC
        RORA
        PSHS    CC
        JSR     PreserveCC
        LDU     #SCRNBUF+(6*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(6*32)+28
        JSR     WriteNZVC
        PULS    A
        PULS    CC
        RORA
        JSR     PreserveCC
        LDU     #SCRNBUF+(7*32)+8
        JSR     WriteByte
        LDU     #SCRNBUF+(7*32)+28
        JSR     WriteNZVC
        ; LSRA
        LDA     A
        LSRA
        JSR     PreserveCC
        LDU     #SCRNBUF+(9*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(9*32)+28
        JSR     WriteNZVC
        PULS    A
        LSRA
        JSR     PreserveCC
        LDU     #SCRNBUF+(10*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(10*32)+28
        JSR     WriteNZVC
        PULS    A
        LSRA
        JSR     PreserveCC
        LDU     #SCRNBUF+(11*32)+8
        JSR     WriteByte
        LDU     #SCRNBUF+(11*32)+28
        JSR     WriteNZVC
        ; ASRA
        LDA     A
        ASRA
        JSR     PreserveCC
        LDU     #SCRNBUF+(13*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(13*32)+28
        JSR     WriteNZVC
        PULS    A
        ASRA
        JSR     PreserveCC
        LDU     #SCRNBUF+(14*32)+8
        PSHS    A
        JSR     WriteByte
        LDU     #SCRNBUF+(14*32)+28
        JSR     WriteNZVC
        PULS    A
        ASRA
        JSR     PreserveCC
        LDU     #SCRNBUF+(15*32)+8
        JSR     WriteByte
        LDU     #SCRNBUF+(15*32)+28
        JSR     WriteNZVC
        RTS

; ---------------------------------------------------------------
; **** Binary-coded decimal Screen routines ****
; ---------------------------------------------------------------

; ---------------------------------------------------------------
; InitScreenBCD
; ---------------------------------------------------------------
InitScreenBCD:
        JSR     ClearScreen
        LDX     #HeaderA
        LDU     #SCRNBUF+(4*32)+1
        JSR     PrintString
        LDX     #HeaderB
        LDU     #SCRNBUF+(5*32)+1
        JSR     PrintString
        LDX     #BCD
        LDU     #SCRNBUF+(3*32)+23
        JSR     PrintString
        LDX     #HNZVC
        LDU     #SCRNBUF+(7*32)+27
        JSR     PrintString
        LDX     #HeaderADDANoDaa
        LDU     #SCRNBUF+(8*32)+1
        JSR     PrintString
        LDX     #HeaderADDADaa
        LDU     #SCRNBUF+(11*32)+1
        JSR     PrintString
        RTS


; ---------------------------------------------------------------
; UpdateScreenBCD
; ---------------------------------------------------------------
UpdateScreenBCD:
        ; Write ADDA
        LDA     A
        ADDA    B
        JSR     PreserveCC
        LDU     #SCRNBUF+(9*32)+4
        JSR     WriteByte
        LDU     #SCRNBUF+(9*32)+27
        JSR     WriteHNZVC
        ; Write ADDA + DAA
        LDA     A
        ADDA    B
        DAA
        JSR     PreserveCC
        LDU     #SCRNBUF+(12*32)+4
        JSR     WriteByte
        LDU     #SCRNBUF+(12*32)+27
        JSR     WriteHNZVC
        RTS


; ---------------------------------------------------------------
; Structures
; ---------------------------------------------------------------

; Information about a single page (screen) of this program
PageInfo                STRUCT
InitScreen              RMB     2       ; This page's custom InitScreen routine
UpdateScreen            RMB     2       ; This page's custom UpdateScreen routine
ALocation               RMB     2       ; Where on this page the high-bit of A resides (in screen buffer)
BLocation               RMB     2       ; Where on this page the high-bit of B resides (in screen buffer), or 0 if no B on this page
                        ENDSTRUCT

; Keyboard dispatch entry
KeystrokeAction         STRUCT
Key                     RMB     1       ; Keyboard key to dispatch
Routine                 RMB     2       ; Routine to dispatch to
                        ENDSTRUCT


; ---------------------------------------------------------------
; Data
; ---------------------------------------------------------------

; Array of info for each page in this app
PageInfos:
        ; COM / NEG page
        FDB     InitScreenCOM
        FDB     UpdateScreenCOM
        FDB     SCRNBUF+(3*32)+7        ; Location of A
        FDB     0                       ; Location of B
        ; Rotation L page
        FDB     InitScreenROTL
        FDB     UpdateScreenROTL
        FDB     SCRNBUF+(3*32)+8        ; Location of A
        FDB     0                       ; Location of B
        ; Rotation R page
        FDB     InitScreenROTR
        FDB     UpdateScreenROTR
        FDB     SCRNBUF+(3*32)+8        ; Location of A
        FDB     0                       ; Location of B
        ; Arithmetic page
        FDB     InitScreenARITH
        FDB     UpdateScreenARITH
        FDB     SCRNBUF+(3*32)+14       ; Location of A
        FDB     SCRNBUF+(4*32)+14       ; Location of B
        ; Binary-coded decimal page
        FDB     InitScreenBCD
        FDB     UpdateScreenBCD
        FDB     SCRNBUF+(4*32)+4        ; Location of A
        FDB     SCRNBUF+(5*32)+4        ; Location of B
PageInfos_END:

; Keyboard key dispatch table
KeystrokeActions:
        FCB     KEY_LT
        FDB     KACursorLeft
        FCB     KEY_RT
        FDB     KACursorRight
        FCB     KEY_UP
        FDB     KACursorUp
        FCB     KEY_DN
        FDB     KACursorDown
        FCB     'A'
        FDB     KAIncrement
        FCB     'Z'
        FDB     KADecrement
        FCB     'S'
        FDB     KAIncrement
        FCB     'X'
        FDB     KADecrement
        FCB     KEY_SPACE
        FDB     KANextPage
        FCB     '?'
        FDB     KAHelp
        
KeystrokeActions_END:

BitToSetMask:
        FCB     1,2,4,8,16,32,64,128
BitToClearMask:
        FCB     255-1,255-2,255-4,255-8,255-16,255-32,255-64,255-128
RenderCursorTbl:
        FDB     0,0,RC_ABin,RC_BBin,RC_ADecS,RC_BDecS,0,0,RC_ADecU,RC_BDecU
VdgDigits:
        FCB     '0','1','2','3','4','5','6','7','8','9'
PowersOfTen:
        FDB     10000,1000,100,10,1
PowersOfTen_END:



CursorLocType   RMB     1               ; Cursor location's type (one of the CURSOR_ equates)
CursorLocBit    RMB     1               ; If Cursor is on a bit, this says which one (0 = rightmost bit)
PEDelay         RMB     2               ; Delay counter for auto-repeat keys


A               RMB     1               ; Storage for the A byte
B               RMB     1               ; Storage for the B byte
D               RMB     2               ; Storage for 2-byte results
CC              RMB     1               ; Storage for condition code bits

BTPBOutput      RMB     8               ; Storage to hold output string from ByteToPrintableBinary

Title           FCN     "        binary playground       "
Help            FCN     "         ('?' for help)         "
HeaderA         FCN     "a:"
HeaderB         FCN     "b:"
NZVC            FCN     "nzvc"
HNZVC           FCN     "hnzvc"
HeaderCOM       FCN     "com:"
HeaderNEG       FCN     "neg:"
HeaderADD       FCN     "adda (a + b): "
HeaderSUB       FCN     "suba (a - b): "
HeaderMUL       FCN     "mul  (a * b): "
HeaderSEX       FCN     "sex  (b -> d): "
HeaderROLA1     FCN     "rola 1: "
HeaderROLA2     FCN     "rola 2: "
HeaderROLA3     FCN     "rola 3: "
HeaderLSLA1     FCN     "lsla 1: "
HeaderLSLA2     FCN     "lsla 2: "
HeaderLSLA3     FCN     "lsla 3: "
HeaderASLA1     FCN     "asla 1: "
HeaderASLA2     FCN     "asla 2: "
HeaderASLA3     FCN     "asla 3: "
HeaderRORA1     FCN     "rora 1: "
HeaderRORA2     FCN     "rora 2: "
HeaderRORA3     FCN     "rora 3: "
HeaderLSRA1     FCN     "lsra 1: "
HeaderLSRA2     FCN     "lsra 2: "
HeaderLSRA3     FCN     "lsra 3: "
HeaderASRA1     FCN     "asra 1: "
HeaderASRA2     FCN     "asra 2: "
HeaderASRA3     FCN     "asra 3: "
BCD             FCN     "bcd"
QuestionMark    FCN     "?"
HeaderADDANoDaa FCN     " adda (a + b): "
HeaderADDADaa   FCN     " adda (a + b), daa:  "
Help01          FCC     "arrow keys: move cursor         "
Help02          FCC     "a/z: inc/dec one at a time      "
Help03          FCC     "s/x: inc/dec fast when held     "
Help04          FCC     "space: next screen of operations"
Help05          FCC     "?: help, but you knew that      "
Help06          FCC     "                                "
Help07          FCC     "condition codes shown are the   "
Help08          FCC     "actual result of the operation, "
Help09          FCC     "but not all are defined by spec "
Help10          FCC     "                                "
Help11          FCC     "ops on page are independent,    "
Help12          FCC     "each run against original value "
Help13          FCC     "of a or b.  exception: trios of "
Help14          FCC     "rotations are run sequentially  "
Help15          FCC     "                                "
Help16          FCN     "      hit space to return       "



        END     $3F00





