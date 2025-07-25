; Walks through each step of the ROM's RND function, displaying
; the state of the FP accumulators at each point.

                ORG     $3F00

POLCAT	        EQU     $A000
SCRNBUF         EQU     $0400   ; Screen buffer start
SCRNEND         EQU     $0600   ; Byte after screen buffer

V40             EQU     $40     ; Start of block containing FPA3-5
FP0EXP          EQU     $4F     ; Location of first byte (exponent) of floating point accumulator 0
FPA0            EQU     $50
FP0SGN          EQU     $54 
FP1EXP          EQU     $5C
FPA1            EQU     $5D
FP1SGN          EQU     $61
FPSBYT          EQU     $63     ; FLOATING POINT SUB BYTE (FIFTH BYTE)
FPA2            EQU     $13
FPA3            EQU     $40
VAB             EQU     $AB     ; LOW ORDER FOUR BYTES OF THE PRODUCT
VAC             EQU     $AC     ; OF A FLOATING POINT MULTIPLICATION
VAD             EQU     $AD     ; THESE BYTES ARE USE AS RANDOM DATA
VAE             EQU     $AE     ; BY THE RND STATEMENT

; Values to fill floating point accumulator as parameter to
; pass to RND (0)
EXPVAL          EQU     $0
MANVAL          EQU     $0
SGNVAL          EQU     $7A


NormalizeFPA0   EQU     $BA1C
FPA0TimesFPA1   EQU     $BAD0
FPA0ToAscii     EQU     $BDD9
RVSEED          EQU     $0115
RSEED           EQU     $BF74


; ---------------------------------------------------------------
; main
; ---------------------------------------------------------------

        ; Initialize
        JSR     TweakLowerCaseLetters
        JSR     InitScreen
        LDX     #StepDescriptionsStart
        STX     StepDescriptionCur
MainLoop:
        JSR     InitFPA0                ; Initialize input parameter to RND
        JSR     RNDROM                  ; Execute visualization version of RND
        BRA     MainLoop


WaitForKeystroke:
        JSR     [POLCAT]
        BEQ     WaitForKeystroke
        RTS


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
        LDX     #StrTitle
        LDY     #SCRNBUF+(0*32)+7
        JSR     PrintString
        LDX     #StrHeaders
        LDY     #SCRNBUF+(4*32)+8
        JSR     PrintString
        LDX     #StrFPA0
        LDY     #SCRNBUF+(5*32)+0
        JSR     PrintString
        LDX     #StrFPA1
        LDY     #SCRNBUF+(6*32)+0
        JSR     PrintString
        LDX     #StrFPA2
        LDY     #SCRNBUF+(7*32)+0
        JSR     PrintString
        LDX     #StrFPA3
        LDY     #SCRNBUF+(8*32)+0
        JSR     PrintString
        LDX     #StrRVSEED
        LDY     #SCRNBUF+(9*32)+0
        JSR     PrintString
        LDX     #StrPRODUCT
        LDY     #SCRNBUF+(11*32)+0
        JSR     PrintString
        LDX     #StrRSEED
        LDY     #SCRNBUF+(12*32)+0
        JSR     PrintString
        LDX     #StrNextStep
        LDY     #SCRNBUF+(14*32)+0
        JSR     PrintString
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
; InitFPA0
; Initialize floating point accumulator
; ---------------------------------------------------------------
InitFPA0:
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
; RNDROM
; Duplicate of code from Color BASIC ROM's RND function, with
; calls to visualize the state inserted after each step
; ---------------------------------------------------------------
RNDROM:
        ; MOVE VARIABLE RANDOM NUMBER SEED TO FPA0
        JSR     VisualizeAndWait    ; ** move rvseed to fpa0
        LDX     RVSEED+1 
        STX     FPA0     
        LDX     RVSEED+3 
        STX     FPA0+2 
UpdateSeed:
        ; MOVE FIXED RANDOM NUMBER SEED TO MANTISSA OF FPA1
        JSR     VisualizeAndWait    ; ** move rseed to fpa1
        LDX     RSEED 
        STX     FPA1  
        LDX     RSEED+2
        STX     FPA1+2  
        ; FPA0 = FPA0 * FPA1
        JSR     VisualizeAndWait    ; ** multiply fpa0 * fpa1
        JSR     FPA0TimesFPA1       ; (also modifies fpa0 and fpa2)
        JSR     VisualizeAndWait    ; ** $658b + product low 2
        LDD     VAD         ; GET THE TWO LOWEST ORDER PRODUCT BYTES
        ADDD    #$658B     ; ADD A CONSTANT
        STD     RVSEED+3    ; SAVE NEW LOW ORDER VARIABLE RANDOM # SEED
        STD     FPA0+2      ; SAVE NEW LOW ORDER BYTES OF FPA0 MANTISSA
        JSR     VisualizeAndWait    ; ** 05b0 + product high 2
        LDD     VAB         ; GET 2 MORE LOW ORDER PRODUCT BYTES
        ADCB    #$B0       ; ADD A CONSTANT
        ADCA    #5         ; ADD A CONSTANT
        STD     RVSEED+1    ; SAVE NEW HIGH ORDER VARIABLE RANDOM # SEED
        STD     FPA0        ; SAVE NEW HIGH ORDER FPA0 MANTISSA
        JSR     VisualizeAndWait    ; ** clear fpa0 sign
        CLR     FP0SGN      ; FORCE FPA0 MANTISSA = POSITIVE
        ; SET FPA0 BIASED EXPONENT TO 0 (0 < FPA0 < 1)
        JSR     VisualizeAndWait    ; ** set exponent to $80
        LDA     #$80
        STA     FP0EXP 
        JSR     VisualizeAndWait    ; ** move fpa2 byte 2 to subbyte
        LDA     FPA2+2      ; GET A BYTE FROM FPA2 (MORE RANDOMNESS)
        STA     FPSBYT      ; SAVE AS SUB BYTE
        JSR     VisualizeAndWait    ; ** normalize fpa0
        JSR     NormalizeFPA0
        JSR     VisualizeAndWait    ; ** done!
        RTS


; ---------------------------------------------------------------
; PrintStepDescription
; Prints StepDescriptionCur, then advances it to next
; description
; ---------------------------------------------------------------
PrintStepDescription:
        LDX     StepDescriptionCur
        LDY     #SCRNBUF+(15*32)+0
        JSR     PrintString
        CMPX    #StepDescriptionsEnd
        BLO     PSDDone
        LDX     #StepDescriptionsStart
PSDDone:
        STX     StepDescriptionCur
        RTS


; ---------------------------------------------------------------
; VisualizeAndWait
; Display state of FP accumulators, wait for keystroke
; ---------------------------------------------------------------
VisualizeAndWait:
        PSHS    U,Y,X,D,CC
        BSR     PrintStepDescription
        ; FPA0 exponent
        LDA     FP0EXP
        LDY     #SCRNBUF+(5*32)+8
        JSR     PrintByte
        ; FPA0 mantissa
        LDU     #FPA0
        LDY     #SCRNBUF+(5*32)+13
        JSR     Print4Bytes
        ; FPA0 subbyte
        LDA     FPSBYT
        LDY     #SCRNBUF+(5*32)+25
        JSR     PrintByte
        ; FPA0 sign
        LDA     FP0SGN
        LDY     #SCRNBUF+(5*32)+30
        BSR     PrintByte
        ; FPA1 exponent
        LDA     FP1EXP
        LDY     #SCRNBUF+(6*32)+8
        BSR     PrintByte
        ; FPA1 mantissa
        LDU     #FPA1
        LDY     #SCRNBUF+(6*32)+13
        BSR     Print4Bytes
        ; FPA1 sign
        LDA     FP1SGN
        LDY     #SCRNBUF+(6*32)+30
        BSR     PrintByte
        ; FPA2 mantissa
        LDU     #FPA2
        LDY     #SCRNBUF+(7*32)+13
        BSR     Print4Bytes
        ; FPA3 exponent
        LDA     FPA3
        LDY     #SCRNBUF+(8*32)+8
        BSR     PrintByte
        ; FPA3 mantissa
        LDU     #FPA3+1
        LDY     #SCRNBUF+(8*32)+13
        BSR     Print4Bytes
        ; RVSEED exponent
        LDA     RVSEED
        LDY     #SCRNBUF+(9*32)+8
        BSR     PrintByte
        ; RVSEED mantissa
        LDU     #RVSEED+1
        LDY     #SCRNBUF+(9*32)+13
        BSR     Print4Bytes
        ; PRODUCT mantissa
        LDU     #VAB
        LDY     #SCRNBUF+(11*32)+13
        BSR     Print4Bytes
        ; RSEED
        LDU     #RSEED
        LDY     #SCRNBUF+(12*32)+13
        BSR     Print4Bytes
        LDU     #SCRNBUF+(2*32)+0
        BSR     PrintFPA0AsFloat
        JSR     WaitForKeystroke
        PULS    CC,D,X,Y,U,PC

; Helpers for printing bytes from accumulator state
Print4Bytes:
        LDA     ,U+
        BSR     PrintByte
        LDA     ,U+
        BSR     PrintByte
        LDA     ,U+
        BSR     PrintByte
        LDA     ,U                      ; Fall through to print last byte
PrintByte:
; Print byte in A to screen memory at U in hex form
        PSHS    A                       ; Save A before shift
        LSRA
        LSRA
        LSRA
        LSRA
        BSR     PrintNibble             ; Print high nibble 
        PULS    A                       ; Restore A
        ANDA    #%00001111              ; Fall through to print low nibble
PrintNibble:
; Print low nibble of A to screen memory at U in hex form
        LDX     #NibbleToVdg
        LDA     A,X
        STA     ,Y+
        RTS

; Helper for printing FPA0 in decimal form
PrintFPA0AsFloat:
        PSHS    U
        LDX     #StrBlankLine
        LDY     ,S
        JSR     PrintString             ; Blank out previous float
        BSR     SaveFPs
        JSR     FPA0ToAscii             ; Ask ROM to convert to string of decimal form
        PULS    U
PFAPLoop:
; Print decimal string pointed to by X to screen memory at U
        LDA     ,X+
        BEQ     PFAFDone
        STA     ,U+
        BRA     PFAPLoop
PFAFDone:
        BSR     RestoreFPs
        RTS


; Helpers to save / restore state of fp accumulators
; to / from memory we own
SaveFPs:
        ; FPA3-5,0 (21 bytes)
        LDX     #V40
        LDY     #FPLocalCopy
        LDA     #21
        BSR     CopyBytes
        ; FPA1 (6 bytes)
        LDX     #FP1EXP
        LDA     #6
        BSR     CopyBytes
        ; sub byte (1 byte)
        LDX     #FPSBYT
        LDA     #1
        BSR     CopyBytes
        ; product (4 bytes)
        LDX     #VAB
        LDA     #4
        BSR     CopyBytes
        ; FPA2 (4 bytes)
        LDX     #FPA2
        LDA     #4
        ; fall through to CopyBytes, then return
CopyBytes:
; X = src, Y = dest, A = size
        LDB     ,X+
        STB     ,Y+
        DECA
        BNE     CopyBytes
        RTS
RestoreFPs:
        ; FPA3-5,0 (21 bytes)
        LDX     #FPLocalCopy
        LDY     #V40
        LDA     #21
        BSR     CopyBytes
        ; FPA1 (6 bytes)
        LDY     #FP1EXP
        LDA     #6
        BSR     CopyBytes
        ; sub byte (1 byte)
        LDY     #FPSBYT
        LDA     #1
        BSR     CopyBytes
        ; product (4 bytes)
        LDY     #VAB
        LDA     #4
        BSR     CopyBytes
        ; FPA2 (4 bytes)
        LDY     #FPA2
        LDA     #4
        BRA     CopyBytes

FPLocalCopy:
        RMB     36


; ---------------------------------------------------------------
; Symbols / data
; ---------------------------------------------------------------
StepDescriptionCur:
        FDB     2
StringStart:
StrTitle:
        FCN     "rom rnd visualizer"
StrNextStep:
        FCN     "next step:"
StrHeaders:
        FCN     "exp  mantissa    sub  sn"
StrFPA0:
        FCN     "fpa0:"
StrFPA1:
        FCN     "fpa1:"
StrFPA2:
        FCN     "fpa2:"
StrFPA3:
        FCN     "fpa3:"
StrRVSEED:
        FCN     "rvseed:"
StrPRODUCT:
        FCN     "product:"
StrRSEED:
        FCN     "rseed:"
StrBlankLine:
        FCN     "                                "
StepDescriptionsStart:
        FCN     "move rvseed to fpa0             "
        FCN     "move rseed to fpa1              "
        FCN     "multiply fpa0 * fpa1            "
        FCN     "$658b + prd low2 to fpa0 & rvsd "
        FCN     "$05b0 + prd hi2 to fpa0 & rvsd  "
        FCN     "clear fpa0 sign                 "
        FCN     "fpa0 exponent = $80             "
        FCN     "move fpa2 byte2 to subbyte      "
        FCN     "normalize fpa0 and done!        "
        FCN     "set fpa0 to 0 for next call     "
StepDescriptionsEnd:
NibbleToVdg:
        FCB     $30,$31,$32,$33,$34,$35,$36,$37,$38,$39
        FCC     "a"
        FCC     "b"
        FCC     "c"
        FCC     "d"
        FCC     "e"
        FCC     "f"
StringEnd:



        END     $3F00