; Visually compares the behavior of several pseudo-random
; number generators. Most algorithms provided by Glen Hewlett, as
; the result of his research into RNGs while working on
; his BASIC-to-6809 compiler
; (https://github.com/nowhereman999/BASIC-To-6809)

                ORG     $3F00

; Data structure with information about each RNG
AlgInfo         STRUCT
Address         RMB     2               ; Start address for RNG's routine
Name            RMB     2               ; Name to display
Returns16Bits   RMB     1               ; 0 = returns 8 bits, $FF = returns 16 bits
                ENDSTRUCT

NumAlgorithmInfos   EQU     (AlgorithmInfosEnd-AlgorithmInfos)/sizeof{AlgInfo}
KEY_SPACE           EQU     $20
KEY_ENTER           EQU     $0D
KEY_CLEAR           EQU     $0C
POLCAT              EQU     $A000
RND                 EQU     $BF1F       ; ROM routine to generate random number
SCRNBUF             EQU     $0400       ; Screen buffer start
SCRNEND             EQU     $0600       ; Byte after screen buffer
RNDMAN              EQU     $116        ; LOCATION OF BASIC RANDOM NUMBER mantissa (RVSEED+1).


; Stat table: Screen memory bytes incremented on each random number generated
STATBL              EQU     SCRNBUF+(4*32)+0
ENDST               EQU     STATBL+$100

; Location of first byte of floating point accumulator (exponent)
FP0EXP              EQU     $4F

; Values to fill floating point accumulator as parameter to
; pass to RND (0): Generates float between 0 and 1.
EXPVAL              EQU     $00
MANVAL              EQU     $00
SGNVAL              EQU     $7A


; ---------------------------------------------------------------
; main
; ---------------------------------------------------------------

Main:
        ; Initialize
        JSR     TweakLowerCaseLetters   ; Prepare strings for printing
        CLR     AlgorithmIndex          ; Init algorithm pointer
        JSR     InitScreen              ; Clear screen, display static portions
InitLoop:
        ; Outer loop, which initializes the RNG to begin, and then
        ; runs inner loop to call RNG repeatedly
        JSR     GetAlgInfo              ; Print algorithm name
        LDX     AlgInfo.Name,X
        LDY     #SCRNBUF+(0*32)+0
        JSR     PrintString
        LDA     #1                      ; Start off paused
        STA     Pause                   
        JSR     InitStats               ; Init stats bytes to all 0
        JSR     ClearSeeds              ; random seeds = 0 for predictability
MainLoop:
        ; Inner loop, each iteration calls the current RNG once, and
        ; updates the display
        JSR     RandomOnce
        ; BEGIN: Uncomment below code to force loop to stop
        ; after numcalls wraps around
        * LDD     NumCalls
        * BNE     CheckKeystroke          ; If NumCalls hasn't wrapped around yet, continue normally
        * LDA     #1                      ; Otherwise, pause
        * STA     Pause
        ; END: Uncomment above code to force loop to stop
        ; after numcalls wraps around
CheckKeystroke:
        JSR     [POLCAT]                ; Any keystoke?
        BEQ     ContinueLoop            ; If not, continue
        CMPA    #KEY_SPACE
        BEQ     OnSpace
        CMPA    #KEY_ENTER
        BEQ     OnEnter
        CMPA    #KEY_CLEAR
        BNE     ContinueLoop            ; No recognized keystroke, so continue normally
; OnClear: Start over with next algorithm
        LDB     AlgorithmIndex
        INCB
        CMPB    #NumAlgorithmInfos
        BLO     SetNewAlgIndex
        CLRB                            ; If past end of list, wrap around to beginning
SetNewAlgIndex:
        STB     AlgorithmIndex
        BRA     InitLoop                ; Break to outer loop
OnEnter:
        LDA     #1                      ; Set pause state to ensure single step
        STA     Pause
        BRA     MainLoop                ; Continue inner loop
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
        ; Print label for number of calls
        LDX     #LBLNUM
        LDY     #SCRNBUF+(1*32)+0
        JSR     PrintString
        ; Print label for random number actually chosen
        LDX     #LBLRND
        LDY     #SCRNBUF+(2*32)+0
        JSR     PrintString
        ; Print keystroke help labels
        LDX     #LBLSPC
        LDY     #SCRNBUF+(13*32)+0
        JSR     PrintString
        LDX     #LBLENT
        LDY     #SCRNBUF+(14*32)+0
        JSR     PrintString
        LDX     #LBLCLR
        LDY     #SCRNBUF+(15*32)+0
        JSR     PrintString
        RTS


; ---------------------------------------------------------------
; InitStats sub
; Zeroes out entries in STATBL, clear calls count
; ---------------------------------------------------------------
InitStats:
        LDX     #STATBL
        LDD     #0
InitStatsTblLoop:
        CMPX    #ENDST
        BHS     InitStatsTblEnd
        STD     ,X++
        BRA     InitStatsTblLoop
InitStatsTblEnd:
        LDD     #0
        STD     NumCalls
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
; Call current RNG to pick random number, print it, update stats
; ---------------------------------------------------------------
RandomOnce:
        ; Produce random number via current algorithm
        BSR     GetAlgInfo              ; X -> current RNG's structure
        PSHS    X                       ; Preserve alg ptr
        JSR     [AlgInfo.Address,X]     ; Call RNG, get result in D
        PSHS    D                       ; preserve random number
        ; Print random number returned
        LDY     #SCRNBUF+(2*32)+25
        BSR     WriteWordAsHex
        PULS    D                       ; Restore random number
        ; Increment entry in stat table for low byte
        LDX     #STATBL
        ABX
        INC     ,X
        ; If a second byte was generated by the RNG, increment its entry
        PULS    U                       ; Restore alg ptr
        LDB     AlgInfo.Returns16Bits,U ; Did RNG return 16 bits?
        BEQ     RO8Bits                 ; If no, skip
        ; Increment entry in stat table for high byte
        TFR     A,B                     ; A bit of gymnastics so we can use ABX again, cuz we want an unsigned add
        LDX     #STATBL
        ABX
        INC     ,X
        ; Increment number of calls made (16-bits -> 2 calls)
        LDD     NumCalls
        ADDD    #2
        BRA     ROPrintCallCount
RO8Bits:
        ; Increment number of calls made (8-bits -> 1 call)
        LDD     NumCalls
        ADDD    #1
ROPrintCallCount:
        STD     NumCalls
        LDY     #SCRNBUF+(1*32)+18
        BSR     WriteWordAsHex
        RTS


; ---------------------------------------------------------------
; Helpers to print words, bytes, and nibbles in hex
; ---------------------------------------------------------------
WriteWordAsHex:
        PSHS    A                       ; Push high byte
        BSR     WriteByteAsHex          ; Write high byte
        STB     ,S                      ; Push low byte
        BSR     WriteByteAsHex          ; Write low byte
        PULS    A,PC                    ; Done
WriteByteAsHex:
        ; High nibble
        LDA     2,S                     ; A = byte to print
        ANDA    #%11110000
        LSRA
        LSRA
        LSRA
        LSRA
        BSR     WriteNibbleAsHex
        ; Low nibble
        LDA     2,S                     ; A = byte print
        ANDA    #%00001111
        ; Fall through to write it
WriteNibbleAsHex:
        LDX     #VdgDigits
        LDA     A,X                     ; A = character code of hex digit
        STA     ,Y+                     ; Print A
        RTS

; Table of VDG character codes corresponding to the hex digit of their offset
VdgDigits:
        FCB     '0','1','2','3','4','5','6','7','8','9',1,2,3,4,5,6


; ---------------------------------------------------------------
; PrintString
; Prints null-terminated string
; Entry: X -> string
; Entry: Y -> Screen buffer location to print to
; Also uses: A
; ---------------------------------------------------------------
PrintString:
        LDA     ,X+
        BEQ     PrintStringEnd          ; Break out of loop on terminating null
        STA     ,Y+                     ; Print character
        BRA     PrintString             ; Loop to next character 
PrintStringEnd:
        RTS


; ---------------------------------------------------------------
; ClearSeeds
; Reset seeds for all RNGs to their starting values for
; consistency
; ---------------------------------------------------------------
ClearSeeds:
        LDD     #0
        STB     RandomSeed       
        STD     RandomSeed16
        STB     Seed1       
        STB     Seed2       
        STD     RNDSeed0
        STB     RNDA
        STB     RNDB
        STB     RNDC
        STB     RNDX
        ; RVSEED (used by ROM and "high period").  Reset to
        ; same value ROM resets it to on startup
        LDB     #$80
        STB     RNDMAN-1        ; RV seed exponent
        LDD     #$4FC7
        STD     RNDMAN          ; RV seed mantissa bytes 1 & 2
        LDD     #$5259
        STD     RNDMAN+2        ; RV seed mantissa bytes 3 & 4
        RTS  



; ---------------------------------------------------------------
; RANDOM NUMBER GENERATORS
; ---------------------------------------------------------------


; ---------------------------------------------------------------
; RandomEor sub
; Pick random number from 0 to 255
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
        CLRA                    ; D = return value
        RTS                     ; Return


; ---------------------------------------------------------------
; RandomEor16 sub
; Straightforward extension of RandomEOR to work with 16 bits
; ---------------------------------------------------------------
RandomEor16:
        LDD     RandomSeed16    ; Get last Random Number
        BEQ     DoEOR16         ; Handle input of zero
        ASLB                    ; Shift low left, clear bit zero
        ROLA                    ; Shift high left, carry into low bit
        BCC     RndReady16      ; If the carry is now clear skip the EOR
        CMPD    #0
        BEQ     RndReady16      ; if the input was $8000, skip the EOR
DoEOR16:
        ; EOR with magic number 0001 0000 0000 1011
        ; (16-bit LFSR feedback polynomial from
        ; https://en.wikipedia.org/wiki/Linear-feedback_shift_register
        ; but reversed since we're doing left-shifts)
        EORA    #$10
        EORB    #$0B            
RndReady16:
        STD     RandomSeed16    ; Save the output as the new seed
        RTS                     ; Return



; ---------------------------------------------------------------
; RandomROM sub
; Wrapper around ROM's RND function, places mantissa bytes 2 and
; 3 into D as returned value
; ---------------------------------------------------------------
RandomROM:
; Initialize floating point accumulator with 0 (the parameter to RND)
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
        ; mantissa starts right after exponent; skip first mantissa byte which is always >= $80
        LDD     FP0EXP+2
        RTS


; ---------------------------------------------------------------
; RandomZ sub
; From Glen's BASICTo6809 compiler
; B = result will be a random number from 0-255
; Uses RNDX, RNDC, Seed1, Seed2
; ---------------------------------------------------------------
RandomZ:
        ; RNDX = RNDX + 1
        INC     RNDX
        ; Seed1 = Seed1 EOR RNDC EOR RNDX
        LDB     Seed1
        EORB    RNDC
        EORB    RNDX
        STB     Seed1
        ; Seed2 = Seed2 + Seed1
        ADDB    Seed2
        STB     Seed2
        ; RNDC = (RNDC + Seed2/2) EOR Seed1
        LSRB
        ADDB    RNDC
        EORB    Seed1
        STB     RNDC
        ; RNDC is our result (in B)
        CLRA
        RTS


; ---------------------------------------------------------------
; RAND sub
; From Glen's research (modified for just 0-255 range)
* THIS IS A FAST, HIGH GRADE RANDOM NUMBER GENERATOR
* LENGTH OF NON-REPEATING SEQUENCE = 2,147,483,647
; Uses RNDMAN (which must NOT be initialized to all zeroes!)
; ---------------------------------------------------------------
RAND:
        BSR     RND2
        BSR     RND2
        BSR     RND2
        BSR     RND2
        BSR     RND2
        LDB     RNDMAN
        CLRA
        RTS
RND2:
        ; Shift full 4 bytes of RNDMAN to the right once, using the EOR of
        ; bits 28 and 31 (from left) as the new high bit
        LDB     RNDMAN+3        GET 28TH BIT
        ANDB    #%00010000      KEEP 28TH BIT
        LSRB                    00001000 29
        LSRB                    00000100 30
        LSRB                    00000010 31
        EORB    RNDMAN+3        EOR what originally were bits 28 & 31
        LSRB                    shift that into carry
        LSRB
        ROR     RNDMAN          Rotate that carry bit into high bit of high byte of RNDMAN
        ROR     RNDMAN+1        Continue rotating remaining bytes of RNDMAN
        ROR     RNDMAN+2
        ROR     RNDMAN+3
        RTS


; ---------------------------------------------------------------
; GetRandom sub
; From Glen's research: Random number generator from Mega-Bug
; The random number generator pulls values from the BASIC ROM
; from A000 to BFFF. The pointer starts at A000 and bumps up
; by 21 before each new number.
; Uses RNDSeed0
; ---------------------------------------------------------------
GetRandom:
    LDD     RNDSeed0                       ; Get offset for random numbers
    ADDD    #$0015                         ; Add $15 to offset
    ANDA    #$1F                           ; Confine offset to ROM, wraparound after $1FFF
    STD     RNDSeed0                       ; Store new offset
    ADDD    #$A000                         ; Apply offset into ROM, from $A000
    ; Clever way to dereference calculated ROM address, with result in B
    PSHS    B,A                            ; S now points to calculated ROM address
    LDB     [,S++]                         ; B = value at ROM address, S is restored
    CLRA
    RTS


; ---------------------------------------------------------------
; Symbols / data
; ---------------------------------------------------------------
StringStart:
LBLNUM  FCN     "number of calls: $"
LBLRND  FCN     "random number returned: $"
LBLSPC  FCN     "SPACE pause / resume"
LBLENT  FCN     "ENTER single step"
LBLCLR  FCN     "CLEAR restart with next alg"
NMROM   FCN     "rom                        "
NMEOR   FCN     "simple eor                 "
NMEOR16 FCN     "simple eor 16              "
NMRNDZ  FCN     "basic to 6809 8 bit        "
NMRAND  FCN     "high period                "
NMGETR  FCN     "mega-bug                   "
StringEnd:
NumCalls
        RMB     2
Pause:
        RMB     1
AlgorithmIndex:
        RMB     1
RandomSeed:
        RMB     1
RandomSeed16:
        RMB     2
Seed1:
        RMB     1
Seed2:
        RMB     1
RNDSeed0:
        RMB     2
RNDA    RMB     1
RNDB    RMB     1
RNDC    RMB     1
RNDX    RMB     1

AlgorithmInfos:
        FDB     RandomEor,NMEOR
        FCB     0
        FDB     RandomZ,NMRNDZ
        FCB     0
        FDB     RAND,NMRAND
        FCB     0
        FDB     GetRandom,NMGETR
        FCB     0
        FDB     RandomEor16,NMEOR16
        FCB     $FF
        FDB     RandomROM,NMROM
        FCB     $FF
AlgorithmInfosEnd:

        END     Main