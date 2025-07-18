; Visually compares the behavior of a simple shift + EOR pseudo-random
; number generator and the RND function implemented in the CoCo
; BASIC ROM.  Generates integers between 0-255, though there are
; a few tests of the shift + EOR algorithm in smaller ranges.

                ORG     $3F00

AlgInfo         STRUCT
Address         RMB     2
Name            RMB     2               
Max             RMB     1
                ENDSTRUCT

NumAlgorithmInfos   EQU     (AlgorithmInfosEnd-AlgorithmInfos)/sizeof{AlgInfo}
KEY_SPACE           EQU     $20
KEY_ENTER           EQU     $0D
KEY_CLEAR           EQU     $0C
POLCAT	            EQU	    $A000
RND                 EQU     $BF1F   ; ROM routine to generate random number
INTCNV              EQU     $B3ED   ; ROM routine to convert int in FPA0 to D
VALTYP              EQU     $6      ; Flag: 0=numeric, $FF=string
FPSCNV              EQU     $BDD9   ; ROM routine to convert FPA0 to ASCII string
GIVABF              EQU     $B4F4   ; ROM routine to convert D to FPA0
STRBUF              EQU     $03D7   ; Start of BASIC string buffer
SCRNBUF             EQU     $0400   ; Screen buffer start
SCRNEND             EQU     $0600   ; Byte after screen buffer
; Stat table: Screen memory bytes incremented on each random number generated
STATBL              EQU     SCRNBUF+(3*32)+0
ENDST               EQU     STATBL+$100

; Location of first byte of floating point accumulator
FP0EXP              EQU     $4F

; Values to fill floating point accumulator as parameter to
; pass to RND as the max random integer (256)
EXPVAL              EQU     $89
MANVAL              EQU     $80
SGNVAL              EQU     $7A


; ---------------------------------------------------------------
; main
; ---------------------------------------------------------------

        ; Initialize
        JSR     TweakLowerCaseLetters
        CLR     AlgorithmIndex
        JSR     InitScreen
InitLoop:       
        JSR     GetAlgInfo              ; Print algorithm name
        LDX     AlgInfo.Name,X
        LDY     #SCRNBUF+(0*32)+11
        JSR     PrintString
        LDX     #LBLRNG                 ; Print range
        LDY     #SCRNBUF+(0*32)+18
        JSR     PrintString
        JSR     GetAlgInfo
        LDB     AlgInfo.Max,X
        CMPB    #255
        BEQ     PrintFullRange
        ADDB    #'0'
        STB     SCRNBUF+(0*32)+24
AfterLabelsPrinted:
        LDA     #1
        STA     Pause                   ; Start off paused
        JSR     InitStatsTable          ; Init stats bytes to all 0
        CLR     RandomSeed              ; random seed = 0 for predictability
        BRA     MainLoop
MainLoop:
        JSR     RandomOnce
CheckKeystroke:
        JSR     [POLCAT]                ; Any keystoke?
        BEQ     ContinueLoop            ; If not, continue
        CMPA    #KEY_SPACE
        BEQ     OnSpace
        CMPA    #KEY_ENTER
        BEQ     OnEnter
        CMPA    #KEY_CLEAR
        BNE     ContinueLoop
; OnClear: Start over with next algorithm
        LDB     AlgorithmIndex
        INCB
        CMPB    #NumAlgorithmInfos
        BLO     SetNewAlgIndex
        CLRB
SetNewAlgIndex:
        STB     AlgorithmIndex
        BRA     InitLoop
PrintFullRange:
        LDX     #LBL255
        LDY     #SCRNBUF+(0*32)+24
        JSR     PrintString
        BRA     AfterLabelsPrinted
OnEnter:
        LDA     #1                      ; Set pause state to ensure single step
        STA     Pause
        BRA     MainLoop
OnSpace:
        NEG     Pause                   ; Toggle pause state
        ; Fall through to ContinueLoop
ContinueLoop:
        LDA     Pause                   ; For next iteration, check pause state...
        BGT     CheckKeystroke          ; ...to see if we should check keyboard first...
        BRA     MainLoop                ; ...or do next random number immediately


; ---------------------------------------------------------------
; TweakLowerCaseLetters sub
; Adjusts all bytes representing lower-case letters in the
; specified string in the same manner as BASIC ROM's PUTCHR.
; Running this once on a string allows later code to directly
; store the resulting string's bytes in the screen buffer
; and have the lower-case letters show up properly.  All
; non-lower-case-letter characters are left alone, so
; ensure they're adjusted if necessary elsewhere
; Uses: A, X
; ---------------------------------------------------------------
TweakLowerCaseLetters:
        LDX     #StringStart
TweakLoop:
        LDA     ,X+
        ; In lower-case letter range?
        CMPA    #'a'
        BLO     TweakLoopContinue
        CMPA    #'z'
        BHI     TweakLoopContinue
        ; We have a lower-case letter, so perform both tweaks
        ANDA    #$DF
        EORA    #$40
        STA     -1,X    ; Store tweaked version
TweakLoopContinue:
        CMPX    #StringEnd
        BLO     TweakLoop
        RTS


; ---------------------------------------------------------------
; InitScreen sub
; Clears screen, prints static labels
; ---------------------------------------------------------------
InitScreen:
        JSR     ClearScreen
        ; Print label for algorithm name
        LDX     #LBLALG
        LDY     #SCRNBUF+(0*32)+0
        JSR     PrintString
        ; Print label for random number actually chosen
        LDX     #LBLRND
        LDY     #SCRNBUF+(1*32)+0
        JSR     PrintString
        ; Print keystroke help labels
        LDX     #LBLSPC
        LDY     #SCRNBUF+(12*32)+0
        JSR     PrintString
        LDX     #LBLENT
        LDY     #SCRNBUF+(13*32)+0
        JSR     PrintString
        LDX     #LBLCLR
        LDY     #SCRNBUF+(14*32)+0
        JSR     PrintString
        RTS


; ---------------------------------------------------------------
; InitStatsTable sub
; Zeroes out entries in STATBL 
; ---------------------------------------------------------------
InitStatsTable:
        LDX     #STATBL
        LDD     #0
InitStatsLoop:
        CMPX    #ENDST
        BHS     InitStatsEnd
        STD     ,X++
        BRA     InitStatsLoop
InitStatsEnd:
        RTS


; ---------------------------------------------------------------
; ClearScreen sub
; Clears screen to the super-cool green-on-black
; ---------------------------------------------------------------
ClearScreen:
        LDX     #SCRNBUF
        LDD     #"  "             ; Double-space
ClearScreenLoop:
        CMPX    #SCRNEND
        BHS     ClearScreenDone
        STD     ,X++
        BRA     ClearScreenLoop
ClearScreenDone:
        RTS


; ---------------------------------------------------------------
; GetAlgInfo sub
; Exit: X -> current algorithm info structure
; ---------------------------------------------------------------
GetAlgInfo:
        LDX     #AlgorithmInfos
        LDB     AlgorithmIndex
        LDA     #sizeof{AlgInfo}
        MUL
        ABX
        RTS

; ---------------------------------------------------------------
; RandomOnce sub
; Pick random number, print it, update stats
; ---------------------------------------------------------------
RandomOnce:
        ; Produce random number via current algorithm
        BSR     GetAlgInfo
        LDB     AlgInfo.Max,X
        PSHS    B               ; Pass max to algorithm
        JSR     [AlgInfo.Address,X]
        ; Print random number returned
        STB     ,S              ; Preserve random number (reuse stack location for max above)
        JSR     GIVABF          ; D -> FPA0
        JSR     FPSCNV          ; Convert FPA0 to ASCII string in STRBUF
        LDX     #STRBUF+3
        LDY     #SCRNBUF+(1*32)+24
        JSR     PrintStringWithPad
        ; Increment entry in stat table
        PULS    B               ; Restore random number
        LDX     #STATBL
        ABX
        INC     ,X
        RTS

; ---------------------------------------------------------------
; RandomEor sub
; Pick random number from 0 to 255
; Exit: D = number produced
; ---------------------------------------------------------------
RandomEor:
        LDB     RandomSeed      ; Get last Random Number
        BEQ     DoEOR           ; Handle input of zero
        ASLB                    ; Shift it left, clear bit zero
        BEQ     RndReady        ; if the input was $80, skip the EOR
        BCC     RndReady        ; If the carry is now clear skip the EOR
DoEOR:
        EORB    #$1D            ; EOR with magic number %00011101
RndReady:
        STB     RandomSeed      ; Save the output as the new seed
        ; Adjust result to desired range, if necessary
        LDA     2,S             ; A = max, as passed by caller
        CMPA    #255            ; If max is 255, nothing to do
        BEQ     REDone
        INCA                    ; A = max + 1
        MUL                     ; A = random number in appropriate range
        TFR     A,B             ; Put return value into B
REDone:
        CLRA                    ; D = return value
        RTS                     ; Return


RandomROM:
; Initialize floating point accumulator with 256 (the parameter to RND)
        LDX     #FP0EXP
        LDA     #EXPVAL
        STA     ,X+     ; Set FP0EXP
        LDA     #MANVAL
        STA     ,X+     ; Set byte 1 of mantissa
        CLRA
        STA     ,X+     ; Clear byte 2 of mantissa
        STA     ,X+     ; Clear byte 3 of mantissa
        STA     ,X+     ; Clear byte 4 of mantissa
        LDA     #SGNVAL
        STA     ,X+     ; Set FP0SGN
        ; Clear rest of FPA0 (7 bytes: COEFCT, STRDES, FPCARY)
        LDD     #0
        STD     ,X++ 
        STD     ,X++ 
        STD     ,X++ 
        STA     ,X

; Call RND(FPA0), answer in FPA0
        JSR     RND

        CLR     VALTYP          ; Set value type to numeric
        JSR     INTCNV          ; FPA0 -> D
        SUBD    #1              ; Random number now in range 0-255
        RTS


; ---------------------------------------------------------------
; PrintStringWithPad
; Prints null-terminated string plus an extra 2 spaces afterward
; so we blank out any potential stale digits from the last
; number printed to same location
; Entry: X -> string
; Entry: Y -> Screen buffer location to print to
; Also uses: A
; ---------------------------------------------------------------
PrintStringWithPad:
        JSR     PrintString
        LDD     #$2020
        STD     ,Y              ; pad
        RTS

; ---------------------------------------------------------------
; PrintString
; Prints null-terminated string
; Entry: X -> string
; Entry: Y -> Screen buffer location to print to
; Also uses: A
; ---------------------------------------------------------------
PrintString:
        LDA     ,X+
        TSTA
        BEQ     PrintStringEnd      ; Break out of loop on terminating null
        STA     ,Y+                 ; Print character
        BRA     PrintString
PrintStringEnd:
        RTS


; ---------------------------------------------------------------
; Symbols / data
; ---------------------------------------------------------------
StringStart:
LBLALG  FCN     "algorithm:"
LBLRNG  FCN     "(0 to  )   "
LBL255  FCN     "255)"
LBLRND  FCN     "random number returned:"
LBLSPC  FCN     "SPACE pause / resume"
LBLENT  FCN     "ENTER single step"
LBLCLR  FCN     "CLEAR restart with next alg"
NMROM   FCN     "rom       "
NMEOR   FCN     "simple eor"
StringEnd:
Pause:
        RMB     1
AlgorithmIndex:
        RMB     1
RandomSeed:
        RMB     1

AlgorithmInfos:
        FDB     RandomROM,NMROM
        FCB     255
        FDB     RandomEor,NMEOR
        FCB     255
        FDB     RandomEor,NMEOR
        FCB     7
        FDB     RandomEor,NMEOR
        FCB     6
        FDB     RandomEor,NMEOR
        FCB     5
        FDB     RandomEor,NMEOR
        FCB     4
        FDB     RandomEor,NMEOR
        FCB     3
        FDB     RandomEor,NMEOR
        FCB     2
        FDB     RandomEor,NMEOR
        FCB     1
AlgorithmInfosEnd:

        END     $3F00