; Realistic simulation of the underworld's Department of
; Motor Vehicles, with its highly unfair stack ordering
; for waiting patrons.
;
; A simple example of using a bitmask to draw sprites on top
; of aribrary backgrounds, with a high-flicker cheap
; full-erase strategy between animation frames.  No
; double-buffering or use of VSYNC
;
; To run, run the DMV.BAS driver.
; SPACE = push new patron onto stack
; ENTER = pull most recent patron off of stack
;
; The video superimposes Tom Mix's Dancing Devil onto the
; screen as the customer service representative.  That is not
; included in this simulation. 

        ORG     $4000

GRAPH                   EQU     $E00
GRAPHBG                 EQU     GRAPH+$1800

POLCAT                  EQU     $A000
KEY_SPACE               EQU     $20
KEY_ENTER               EQU     $0D


; Flames:
; X:0/4 - 97/4 = bytes 0-25 (adding extra byte at end so it's an even number of bytes)
; Y:29 - 46
FlamesNumRows           EQU     18
FlamesNumCols           EQU     26
FlamesSize              EQU     Flames2-Flames1
FlamesScreenStart       EQU     GRAPH+(29*32)

; Upright dude:
; X:0/4 - 12/4 = bytes 0-3
; Y:0 - 65
UpDudeNumRows           EQU     66

; Lying down dude:
; X:0/4 - 65/4 = bytes 0-16
; Y:0 - 12
DnDudeNumRows           EQU     13
DnDudeNumCols           EQU     17
DnDudeNumColWords       EQU     (DnDudeNumCols+(DnDudeNumCols%2))/2     ; Evenize columns so word count is complete

DudeTransitionPointCol  EQU     10
DudeTransitionAdjustment EQU    30
UpDudeScreenStart       EQU     GRAPH+(5*32+0)
DudeTransitionPoint     EQU     GRAPH+(5*32+DudeTransitionPointCol)
DudeExitPoint           EQU     GRAPH+(0*32+28)
BottomDudeDoneRowStart  EQU     GRAPH+(177*32)
BottomDudeDoneStart     EQU     BottomDudeDoneRowStart+8


AnimationCountdownMax   EQU     $0080



; ---------------------------------------------------------------
; Main loop
; ---------------------------------------------------------------
MainInit:
        LDX     #Flames1
        STX     FlamesCur
        LDX     #AnimationCountdownMax
        STX     AnimationCountdown
        LDX     #BottomDudeDoneRowStart+6*32            ; First dude should land at the bottom, compensate for rise at first push
        STX     CurDudeDoneRowStart
        LDX     #0
        STX     DudeScreenCur
        STX     DudeScreenPrev
        LDA     #1
        STA     DudeState
        LDA     #-1
        STA     PushOrPull
        CLR     NumStackedDudes
MainLoop:
        JSR     CheckKeyboardInput
        ; Skip animate on most iterations
        JSR     DoAnimationCountdown
        LDD     AnimationCountdown
        BNE     MainLoop
        ; Do the animation
        JSR     EraseDude
        JSR     DrawFlames
        JSR     UpdateDudeData                          ; Determine new state and location for dude
        JSR     DrawDude
        BRA     MainLoop                                ; Loop back up



; ---------------------------------------------------------------
; CheckKeyboardInput
; ---------------------------------------------------------------
CheckKeyboardInput:
        JSR     [POLCAT]
        BEQ     CKIRTS
        LDB     DudeState                               ; Ignore if dude is still animating
        BLE     CKIRTS
        CMPA    #KEY_SPACE
        BEQ     CKIPush
        CMPA    #KEY_ENTER
        BEQ     CKIPull
CKIRTS:
        RTS
CKIPush:
        LDX     #UpDudeScreenStart                      ; Init position of new dude
        STX     DudeScreenCur  
        LDX     CurDudeDoneRowStart                     ; Dude lands on top of previous dude
        LEAX    -6*32,X
        STX     CurDudeDoneRowStart
        LDA     #-1                                     ; Set state to animate right
        STA     PushOrPull
        STA     DudeState
        RTS
CKIPull:
        LDX     DudeScreenPrev                          ; Prev is now cur
        STX     DudeScreenCur  
        LDX     CurDudeDoneRowStart                     ; Next dude lands one spot lower
        LEAX    6*32,X
        STX     CurDudeDoneRowStart
        DEC     NumStackedDudes                         ; Adjust state so that current dude is fully pulled off, and prev dude is next
        JSR     UpdateDudeScreenPrev
        LDA     #1
        STA     PushOrPull
        CLR     DudeState
        RTS


; ---------------------------------------------------------------
; DrawFlames
; ---------------------------------------------------------------
DrawFlames:
        LDX     FlamesCur                               ; Src
        LDY     #FlamesScreenStart+(FlamesNumRows-1)*32 ; Dest
        LDA     #FlamesNumRows                          ; Row byte counter
DFRowOuterLoop:
        LDB     #FlamesNumCols/2                        ; Column byte counter
DFColumnInnerLoop:
        LDU     ,X++
        STU     ,Y++
        DECB
        BNE     DFColumnInnerLoop
        ; Adjust for next outer loop iteration
        LEAY    -FlamesNumCols-32,Y                     ; Y goes up a line
        DECA
        BNE     DFRowOuterLoop
        ; Update flames ptr for next time
        LDX     FlamesCur
        LEAX    FlamesSize,X
        CMPX    #Flames_END
        BHS     DFResetFlamesPtr
DFSetFlamesPtr:
        STX     FlamesCur
        RTS
DFResetFlamesPtr:
        LDX     #Flames1
        BRA     DFSetFlamesPtr

FlamesCur:
        RMB     2


; ---------------------------------------------------------------
; DoAnimationCountdown
; ---------------------------------------------------------------
DoAnimationCountdown:
        LDD     AnimationCountdown
        BEQ     DACReset
        SUBD    #1
        STD     AnimationCountdown
        RTS
DACReset:
        LDD     #AnimationCountdownMax
        STD     AnimationCountdown
        RTS

; ---------------------------------------------------------------
; EraseDude
; ---------------------------------------------------------------
EraseDude:
        LDX     DudeScreenCur
        LEAX    GRAPHBG-GRAPH,X                         ; Src (bg)
        LDY     DudeScreenCur                           ; Dest
        LDA     DudeState
        BGT     EDRTS
        BEQ     EDMovesDown
; EDMovesRight
        ; Dude is moving right or transitioning from upright to
        ; lying down.  Either case, erase entire upright dude
        LDA     #UpDudeNumRows                          ; Row byte counter
EDUprightLoop:
        ; Bytes 0-1
        LDU     ,X
        STU     ,Y
        ; Bytes 2-3
        LDU     2,X
        STU     2,Y
        ; Adjust for next iteration
        LEAX    32,X                                    ; Src down a line
        LEAY    32,Y                                    ; Dest down a line
        DECA
        BNE     EDUprightLoop
        RTS
EDMovesDown:
        LDA     #DnDudeNumRows                          ; Row byte counter
EDDownOuterRowLoop:
        LDB     #DnDudeNumColWords                      ; column counter
EDDownInnerColLoop:
        LDU     ,X++
        STU     ,Y++
        DECB                                            ; Decrement column counter
        BNE     EDDownInnerColLoop
        ; Adjust for next row
        LEAX    32-(DnDudeNumColWords*2),X              ; X goes down a line
        LEAY    32-(DnDudeNumColWords*2),Y              ; Y goes down a line
        DECA
        BNE     EDDownOuterRowLoop
EDRTS:
        RTS

; ---------------------------------------------------------------
; UpdateDudeData
; ---------------------------------------------------------------
UpdateDudeData:
        LDA     DudeState
        BEQ     UDDDown
        BGT     UDDRTS
; UDDRight:
        ; ------- Upright / moving horizontal --------------
        LDX     DudeScreenCur
        LEAX    1,X
        LDB     PushOrPull
        BGT     UDDRightPull
        ; - push -
        CMPX    #DudeTransitionPoint
        BEQ     UDUpToDown
UDDWriteAndExit:
        STX     DudeScreenCur
        RTS
UDDRightPull:
        ; - pull -
        CMPX    #DudeExitPoint                  ; Has dude exited?
        BLO     UDDWriteAndExit                 ; If not, write new position and return
        LDA     #1                              ; and advance to the non-animating state
        STA     DudeState
        RTS
UDDDown:
        ; ---------- Lying down / moving vertical -------------
        LDX     DudeScreenCur
        LDB     PushOrPull
        BGT     UDDDownPull
        ; - push -
        LEAX    3*32,X
        STX     DudeScreenCur
        CMPX    CurDudeDoneRowStart             ; Is dude stacked yet?
        BLO     UDDRTS                          ; If no, return
        INC     NumStackedDudes                 ; Else, add to the stacked count
        JSR     UpdateDudeScreenPrev            ; Update to new prev dude's position
        INC     DudeState                       ; and advance to the non-animating state
        RTS
UDDDownPull:
        ; - pull -
        LEAX    -3*32,X
        STX     DudeScreenCur
        CMPX    #DudeTransitionPoint+6*32
        BLS     UDDownToUp                      ; If moved back up, switch to horizontal
UDDRTS:
        RTS
UDUpToDown:
        ; Transition from (push) upright to falling down
        LEAX    15*32+DudeTransitionAdjustment,X
        STX     DudeScreenCur
        INCA
        STA     DudeState
        RTS
UDDownToUp:
        ; Transition from (pull) rising up to upright
        LEAX    -9*32+5,X
        STX     DudeScreenCur
        DECA
        STA     DudeState
        RTS


; ---------------------------------------------------------------
; DrawDude
; ---------------------------------------------------------------
DrawDude:
        LDU     DudeScreenCur
        BEQ     DDRTS                                   ; On startup, DudeScreenCur will be 0 until first dude is pushed, so just skip
        LDA     DudeState
        BGT     DDNonAnimating
        BEQ     DDLyingDown                             ; Draw lying if that's the state, or if we're done
; DDUpright:
        LDX     #UprightDude                            ; Src
        LEAY    (UpDudeNumRows-1)*32,U                  ; Dest
        LDU     #UprightDudeMask                        ; Src mask
        LDA     #UpDudeNumRows                          ; Row byte counter
DDUpLoop:
        ; Byte #0
        LDB     ,Y                                      ; B = screen byte
        ANDB    ,U                                      ; B gets 00 for each pixel of dude
        ORB     ,X                                      ; B = screen byte with dude overlaid
        STB     ,Y                                      ; Write it back
        ; Byte #1
        LDB     1,Y                                     ; B = screen byte
        ANDB    1,U                                     ; B gets 00 for each pixel of dude
        ORB     1,X                                     ; B = screen byte with dude overlaid
        STB     1,Y                                     ; Write it back
        ; Byte #2
        LDB     2,Y                                     ; B = screen byte
        ANDB    2,U                                     ; B gets 00 for each pixel of dude
        ORB     2,X                                     ; B = screen byte with dude overlaid
        STB     2,Y                                     ; Write it back
        ; Byte #3
        LDB     3,Y                                     ; B = screen byte
        ANDB    3,U                                     ; B gets 00 for each pixel of dude
        ORB     3,X                                     ; B = screen byte with dude overlaid
        STB     3,Y                                     ; Write it back
        ; Adjust for next iteration
        LEAX    4,X                                     ; Src to next row
        LEAU    4,U                                     ; Mask to next row
        LEAY    -32,Y                                   ; Y goes up a line
        DECA
        BNE     DDUpLoop
DDRTS:
        RTS
DDNonAnimating:
        LDB     PushOrPull
        BLT     DDLyingDown                             ; When non-animating on push, still need to draw last frame of lying-down dude
        RTS
DDLyingDown:
        ; Draw one, potentially two dudes, so use subroutines
        BSR     DDLyingDudeSub
        LDA     PushOrPull                              ; If we're pulling, redraw prevous dude we may have overwritten
        BGT     DDDrawPreviousDude
        RTS
DDDrawPreviousDude:
        LDU     DudeScreenPrev                          ; U -> previous dude's screen position
        ; Fall through to do the draw (and return)
DDLyingDudeSub:
        LDX     #LyingDownDude                          ; Src
        LEAY    (DnDudeNumRows-1)*32,U                  ; Dest
        LDU     #LyingDownDudeMask                      ; Src mask
        LDA     #DnDudeNumRows                          ; Row byte counter at tip of stack
        PSHS    A
DDDnOuterRowLoop:
        LDA     #DnDudeNumCols                          ; Column byte counter in A
DDDnInnerColLoop:
        ; Do single byte
        LDB     ,Y                                      ; B = screen byte
        ANDB    ,U+                                     ; B gets 00 for each pixel of dude
        ORB     ,X+                                     ; B = screen byte with dude overlaid
        STB     ,Y+                                     ; Write it back
        DECA                                            ; decrement column count
        BNE     DDDnInnerColLoop
        ; Adjust for next row
        LEAY    -DnDudeNumCols-32,Y                     ; Y goes up a line
        DEC     ,S                                      ; Decrement row count
        BNE     DDDnOuterRowLoop
        PULS    A,PC

; ---------------------------------------------------------------
; UpdateDudeScreenPrev
; ---------------------------------------------------------------
UpdateDudeScreenPrev:
        LDX     #BottomDudeDoneStart
        LDB     NumStackedDudes
        BEQ     UDSPDone                                ; No dudes yet?  Get outta here
        DECB                                            ; Don't apply offset to first (bottom) dude
        BEQ     UDSPDone
UDSPLoop:
        LEAX    -32*6,X
        DECB                                            ; Don't apply offset to first (bottom) dude
        BNE     UDSPLoop
UDSPDone:
        STX     DudeScreenPrev
        RTS

AnimationCountdown:
        RMB     2
PushOrPull
        RMB     1
DudeState:
        ; -1: go right; 0: go down; 1: no dude to animate
        RMB     1
DudeScreenCur:
        RMB     2
CurDudeDoneRowStart:
        RMB     2
NumStackedDudes:
        RMB     1
DudeScreenPrev
        RMB     2

Flames1:
        FCB     $55,$55,$7F,$57,$FD,$55,$55,$7D,$55,$55,$F5,$55,$55,$55,$55,$55,$57,$55,$FD,$55,$57,$FD,$55,$55,$55,$00
        FCB     $FF,$CF,$FF,$03,$FC,$00,$00,$3F,$00,$00,$F0,$00,$FF,$00,$00,$00,$3F,$00,$FF,$00,$0F,$FF,$00,$F0,$04,$00
        FCB     $FF,$FF,$FF,$C3,$FC,$00,$00,$FF,$00,$03,$F0,$00,$FF,$00,$00,$00,$FF,$00,$FF,$00,$0F,$FF,$00,$FC,$04,$00
        FCB     $FF,$CF,$FF,$C3,$FF,$00,$00,$FF,$C0,$0F,$F0,$00,$FF,$00,$00,$03,$FF,$00,$FF,$00,$0F,$FF,$00,$FF,$04,$00
        FCB     $FF,$CF,$FF,$C3,$FF,$00,$00,$FF,$C0,$0F,$FC,$00,$3F,$FC,$00,$03,$FF,$03,$FF,$C0,$3F,$FF,$00,$FF,$04,$00
        FCB     $FF,$CF,$FF,$C3,$FF,$C0,$03,$FF,$C0,$3F,$FC,$00,$3F,$FF,$00,$0F,$FF,$03,$FF,$C0,$3F,$FF,$00,$FF,$04,$00
        FCB     $FF,$CF,$FF,$F3,$FF,$F0,$03,$FF,$C0,$3F,$FC,$00,$3F,$FF,$00,$0F,$FF,$03,$FF,$FC,$3F,$FF,$00,$FF,$04,$00
        FCB     $FF,$CF,$FF,$F3,$FF,$F0,$03,$FF,$C0,$3F,$FC,$00,$0F,$FF,$00,$0F,$FF,$03,$FF,$FC,$3F,$FC,$00,$FF,$04,$00
        FCB     $3F,$CF,$FF,$F3,$FF,$F0,$03,$FF,$00,$3F,$FC,$00,$0F,$FF,$C0,$3F,$C0,$03,$FF,$FC,$3F,$FC,$00,$FF,$04,$00
        FCB     $3F,$CF,$FF,$C3,$FF,$C0,$03,$FF,$00,$3F,$FC,$00,$0F,$FF,$C0,$33,$C0,$03,$FF,$FC,$3F,$FC,$00,$FF,$04,$00
        FCB     $3F,$CF,$FF,$03,$FF,$F0,$03,$FF,$00,$3F,$FC,$00,$03,$FF,$FC,$33,$00,$03,$FF,$FC,$3F,$FF,$00,$FF,$04,$00
        FCB     $3F,$CF,$FF,$03,$FF,$FF,$03,$FF,$00,$3F,$F0,$00,$03,$FF,$FF,$F3,$00,$03,$FF,$FC,$3F,$FF,$00,$FF,$04,$00
        FCB     $3F,$CF,$FF,$C3,$FF,$FF,$03,$FF,$00,$3F,$C0,$00,$00,$FF,$FF,$FF,$00,$03,$FF,$00,$3F,$FF,$00,$FF,$04,$00
        FCB     $3F,$CF,$FF,$C3,$FF,$FF,$03,$FF,$C0,$3F,$F0,$00,$00,$3F,$FF,$C0,$00,$03,$FF,$00,$3F,$FF,$00,$FF,$04,$00
        FCB     $0F,$0F,$FF,$C3,$FF,$FF,$03,$FF,$C0,$3F,$FC,$00,$00,$0F,$FF,$00,$00,$03,$FF,$00,$3F,$FF,$00,$FC,$04,$00
        FCB     $00,$0F,$FF,$C3,$FC,$00,$03,$FF,$C0,$3F,$FC,$00,$00,$00,$00,$00,$00,$03,$FF,$00,$3F,$00,$00,$FC,$04,$00
        FCB     $00,$0F,$FF,$03,$FC,$00,$03,$FF,$C0,$3F,$FC,$00,$00,$00,$00,$00,$00,$00,$FF,$00,$3F,$00,$00,$00,$04,$00
        FCB     $00,$00,$FF,$03,$F0,$00,$00,$FF,$00,$3F,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3F,$00,$00,$00,$04,$00
Flames2:
        FCB     $55,$F5,$55,$55,$F5,$55,$55,$7F,$55,$55,$55,$F5,$55,$55,$5F,$55,$55,$7D,$57,$D5,$55,$55,$7F,$55,$55,$00
        FCB     $00,$F0,$00,$00,$F0,$00,$00,$3F,$C0,$00,$03,$FC,$00,$00,$3F,$00,$00,$3C,$03,$F0,$00,$00,$0F,$C0,$04,$00
        FCB     $00,$F0,$00,$03,$F0,$00,$00,$3F,$C0,$00,$03,$FC,$00,$00,$3F,$00,$00,$FC,$00,$FC,$00,$00,$0F,$F0,$04,$00
        FCB     $00,$F0,$00,$03,$F0,$00,$00,$FF,$C0,$00,$03,$FC,$00,$00,$3F,$00,$03,$FF,$00,$FF,$00,$00,$0F,$FC,$04,$00
        FCB     $00,$FC,$00,$03,$FC,$00,$00,$FF,$C0,$00,$03,$FF,$00,$00,$FF,$C0,$03,$FF,$00,$3F,$C0,$00,$0F,$FF,$04,$00
        FCB     $00,$FC,$00,$03,$FC,$00,$00,$FF,$C0,$00,$03,$FF,$00,$00,$FF,$C0,$03,$FF,$00,$3F,$F0,$00,$03,$FF,$04,$00
        FCB     $00,$FC,$00,$0F,$FC,$00,$03,$FF,$C0,$00,$03,$FF,$00,$00,$FF,$C0,$0F,$FF,$C0,$0F,$FC,$00,$03,$FF,$04,$00
        FCB     $00,$FF,$00,$0F,$FF,$00,$03,$FF,$C0,$00,$03,$FF,$C0,$00,$FF,$C0,$0F,$FF,$C0,$0F,$FF,$C0,$03,$FF,$C4,$00
        FCB     $00,$FF,$C0,$0F,$FF,$00,$03,$FF,$C0,$00,$0F,$FF,$C0,$03,$FF,$C0,$0F,$FF,$C0,$03,$FF,$F0,$03,$FF,$C4,$00
        FCB     $00,$FF,$F0,$0F,$FF,$00,$0F,$FF,$C0,$00,$0F,$FF,$C0,$03,$FF,$C0,$0F,$FF,$C0,$03,$FF,$FC,$03,$FF,$C4,$00
        FCB     $00,$FF,$FC,$3F,$FF,$F0,$0F,$FF,$C0,$00,$0F,$FF,$C0,$03,$FF,$C0,$0F,$FF,$C0,$00,$FF,$FC,$00,$FF,$F4,$00
        FCB     $00,$FF,$FF,$3F,$FF,$F0,$0F,$FF,$C0,$00,$0F,$FF,$00,$03,$FF,$C0,$0F,$FF,$C0,$00,$3F,$FF,$00,$FF,$F4,$00
        FCB     $00,$FF,$FF,$3F,$FF,$FC,$0F,$FF,$C0,$00,$0F,$FC,$00,$03,$FF,$C0,$0F,$FF,$C0,$00,$0F,$FF,$00,$FF,$F4,$00
        FCB     $00,$FF,$FF,$3F,$FF,$FC,$3F,$FF,$F0,$00,$0F,$FC,$00,$03,$FF,$F0,$0F,$FF,$00,$00,$03,$FF,$00,$3F,$F4,$00
        FCB     $00,$FF,$FC,$3F,$FF,$FC,$3F,$FF,$FF,$00,$0F,$F0,$00,$03,$FF,$FC,$0F,$FC,$00,$00,$00,$00,$00,$3F,$F4,$00
        FCB     $00,$FF,$F0,$3F,$FF,$F0,$3F,$FF,$FF,$00,$3F,$F0,$00,$03,$FF,$FF,$0F,$FC,$00,$00,$00,$00,$00,$3F,$F4,$00
        FCB     $00,$00,$00,$0F,$FF,$C0,$3F,$FF,$FF,$00,$3F,$F0,$00,$03,$FF,$FF,$0F,$FF,$00,$00,$00,$00,$00,$0F,$F4,$00
        FCB     $00,$00,$00,$03,$FC,$00,$0F,$FF,$FF,$00,$3F,$F0,$00,$00,$FF,$FF,$0F,$FF,$00,$00,$00,$00,$00,$00,$04,$00
Flames3:
        FCB     $55,$55,$75,$55,$55,$57,$D5,$5F,$D5,$5F,$55,$F5,$55,$77,$D5,$55,$55,$5F,$F5,$55,$FF,$55,$55,$55,$55,$00
        FCB     $00,$00,$3C,$00,$00,$0F,$C0,$0F,$C0,$0F,$C0,$F0,$00,$3F,$F0,$00,$00,$3F,$F0,$00,$FF,$00,$0F,$00,$04,$00
        FCB     $00,$00,$FF,$00,$00,$FF,$C0,$0F,$C0,$3F,$C0,$FC,$00,$3F,$FC,$00,$03,$FF,$C0,$03,$FF,$00,$0F,$00,$04,$00
        FCB     $00,$00,$FF,$00,$00,$FF,$C0,$0F,$C0,$FF,$C0,$FF,$00,$0F,$FF,$00,$0F,$FF,$C0,$03,$FF,$00,$0F,$C0,$04,$00
        FCB     $00,$03,$FF,$00,$03,$FF,$00,$0F,$C0,$FF,$C0,$FF,$00,$0F,$FF,$C0,$0F,$FF,$00,$03,$FF,$00,$0F,$F0,$04,$00
        FCB     $00,$03,$FF,$00,$0F,$FF,$00,$0F,$C3,$FF,$C0,$FF,$C0,$0F,$FF,$C0,$3F,$FF,$00,$03,$FF,$00,$0F,$F0,$04,$00
        FCB     $00,$0F,$FF,$00,$0F,$FF,$00,$0F,$C3,$FF,$00,$FF,$F0,$0F,$FF,$F0,$3F,$FC,$00,$03,$FC,$00,$0F,$F0,$04,$00
        FCB     $00,$0F,$FF,$00,$3F,$FC,$00,$0F,$F3,$FF,$00,$3F,$FC,$0F,$FF,$F0,$3F,$FC,$00,$03,$FC,$00,$0F,$F0,$04,$00
        FCB     $00,$3F,$FF,$C0,$3F,$FC,$00,$0F,$F3,$FF,$00,$3F,$FC,$03,$FF,$F0,$FF,$F0,$00,$03,$FC,$00,$0F,$F0,$04,$00
        FCB     $00,$3F,$FF,$C0,$3F,$F0,$00,$0F,$F3,$FF,$00,$3F,$FC,$03,$FF,$F0,$FF,$F0,$00,$03,$FC,$00,$0F,$F0,$04,$00
        FCB     $00,$3F,$FF,$C0,$FF,$F0,$00,$0F,$FF,$FF,$00,$3F,$FC,$03,$FF,$FC,$FF,$F0,$00,$03,$FC,$00,$0F,$F0,$04,$00
        FCB     $00,$FF,$FF,$C0,$FF,$00,$00,$0F,$FF,$FC,$00,$3F,$FC,$03,$FF,$FC,$FF,$C0,$00,$03,$F0,$00,$3F,$C0,$04,$00
        FCB     $00,$FF,$FF,$C0,$00,$00,$00,$0F,$FF,$FC,$00,$3F,$FC,$00,$FF,$FC,$FF,$C0,$00,$03,$C0,$00,$3F,$C0,$04,$00
        FCB     $00,$FF,$FF,$C0,$00,$00,$00,$0F,$FF,$FC,$00,$3F,$F0,$00,$FF,$FC,$FF,$00,$00,$00,$00,$00,$3F,$C0,$04,$00
        FCB     $03,$FF,$FF,$00,$00,$00,$00,$0F,$FF,$F0,$00,$3F,$C0,$00,$FF,$FC,$FC,$00,$00,$00,$00,$00,$00,$00,$04,$00
        FCB     $03,$FF,$FC,$00,$00,$00,$00,$0F,$FF,$F0,$00,$00,$00,$00,$3F,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$04,$00
        FCB     $03,$FF,$C0,$00,$00,$00,$00,$0F,$FF,$C0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$04,$00
        FCB     $03,$FC,$00,$00,$00,$00,$00,$0F,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$04,$00
Flames_END:
UprightDude:
        FCB     $40,$00,$00,$00
        FCB     $10,$00,$01,$00
        FCB     $10,$00,$04,$00
        FCB     $04,$00,$04,$00
        FCB     $04,$00,$10,$00
        FCB     $01,$00,$10,$00
        FCB     $00,$40,$40,$00
        FCB     $00,$41,$00,$00
        FCB     $00,$11,$00,$00
        FCB     $00,$14,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$14,$00,$00
        FCB     $00,$15,$00,$00
        FCB     $00,$44,$40,$00
        FCB     $01,$04,$10,$00
        FCB     $01,$04,$10,$00
        FCB     $04,$04,$04,$00
        FCB     $10,$04,$01,$00
        FCB     $10,$04,$00,$00
        FCB     $40,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$04,$00,$00
        FCB     $00,$05,$40,$00
        FCB     $00,$54,$50,$00
        FCB     $00,$40,$15,$00
        FCB     $01,$40,$01,$00
        FCB     $01,$00,$01,$00
        FCB     $05,$15,$41,$40
        FCB     $04,$10,$40,$40
        FCB     $04,$10,$40,$40
        FCB     $04,$10,$40,$40
        FCB     $04,$14,$40,$40
        FCB     $04,$04,$40,$40
        FCB     $04,$05,$40,$40
        FCB     $04,$00,$00,$40
        FCB     $04,$00,$00,$40
        FCB     $04,$00,$00,$40
        FCB     $04,$00,$01,$40
        FCB     $04,$10,$41,$00
        FCB     $04,$00,$05,$00
        FCB     $05,$00,$04,$00
        FCB     $01,$00,$14,$00
        FCB     $01,$00,$10,$00
        FCB     $01,$40,$50,$00
        FCB     $00,$55,$40,$00
        FCB     $00,$05,$00,$00
UprightDudeMask:
        FCB     $3F,$FF,$FF,$FF
        FCB     $CF,$FF,$FC,$FF
        FCB     $CF,$FF,$F3,$FF
        FCB     $F3,$FF,$F3,$FF
        FCB     $F3,$FF,$CF,$FF
        FCB     $FC,$FF,$CF,$FF
        FCB     $FF,$3F,$3F,$FF
        FCB     $FF,$3C,$FF,$FF
        FCB     $FF,$CC,$FF,$FF
        FCB     $FF,$C3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$C3,$FF,$FF
        FCB     $FF,$C0,$FF,$FF
        FCB     $FF,$33,$3F,$FF
        FCB     $FC,$F3,$CF,$FF
        FCB     $FC,$F3,$CF,$FF
        FCB     $F3,$F3,$F3,$FF
        FCB     $CF,$F3,$FC,$FF
        FCB     $CF,$F3,$FF,$FF
        FCB     $3F,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F3,$FF,$FF
        FCB     $FF,$F0,$3F,$FF
        FCB     $FF,$03,$0F,$FF
        FCB     $FF,$3F,$C0,$FF
        FCB     $FC,$3F,$FC,$FF
        FCB     $FC,$FF,$FC,$FF
        FCB     $F0,$C0,$3C,$3F
        FCB     $F3,$CF,$3F,$3F
        FCB     $F3,$CF,$3F,$3F
        FCB     $F3,$CF,$3F,$3F
        FCB     $F3,$C3,$3F,$3F
        FCB     $F3,$F3,$3F,$3F
        FCB     $F3,$F0,$3F,$3F
        FCB     $F3,$FF,$FF,$3F
        FCB     $F3,$FF,$FF,$3F
        FCB     $F3,$FF,$FF,$3F
        FCB     $F3,$FF,$FC,$3F
        FCB     $F3,$CF,$3C,$FF
        FCB     $F3,$FF,$F0,$FF
        FCB     $F0,$FF,$F3,$FF
        FCB     $FC,$FF,$C3,$FF
        FCB     $FC,$FF,$CF,$FF
        FCB     $FC,$3F,$0F,$FF
        FCB     $FF,$00,$3F,$FF
        FCB     $FF,$F0,$FF,$FF
LyingDownDude:
        FCB     $00,$00,$00,$00,$00,$00,$00,$10,$00,$00,$00,$00,$00,$00,$00,$00,$10
        FCB     $00,$00,$00,$00,$00,$00,$00,$05,$00,$00,$00,$00,$00,$00,$00,$01,$40
        FCB     $00,$15,$55,$55,$54,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$14,$00
        FCB     $05,$50,$00,$00,$05,$40,$00,$00,$14,$00,$00,$00,$00,$00,$00,$40,$00
        FCB     $14,$00,$00,$00,$00,$54,$00,$00,$01,$00,$00,$00,$00,$00,$05,$00,$00
        FCB     $10,$01,$00,$05,$54,$04,$00,$00,$00,$50,$00,$00,$00,$00,$50,$00,$00
        FCB     $50,$00,$00,$54,$04,$05,$55,$55,$55,$55,$55,$55,$55,$55,$40,$00,$00
        FCB     $50,$00,$00,$40,$04,$01,$00,$00,$00,$40,$00,$00,$00,$00,$14,$00,$00
        FCB     $14,$01,$00,$55,$54,$05,$00,$00,$01,$00,$00,$00,$00,$00,$01,$00,$00
        FCB     $05,$40,$00,$00,$00,$14,$00,$00,$14,$00,$00,$00,$00,$00,$00,$50,$00
        FCB     $00,$54,$00,$00,$00,$10,$00,$00,$40,$00,$00,$00,$00,$00,$00,$05,$00
        FCB     $00,$05,$40,$00,$05,$50,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$40
        FCB     $00,$00,$55,$55,$54,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
LyingDownDudeMask:
        FCB     $FF,$FF,$FF,$FF,$FF,$FF,$FF,$CF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$CF
        FCB     $FF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FC,$3F
        FCB     $FF,$C0,$00,$00,$03,$FF,$FF,$FF,$3F,$FF,$FF,$FF,$FF,$FF,$FF,$C3,$FF
        FCB     $F0,$0F,$FF,$FF,$F0,$3F,$FF,$FF,$C3,$FF,$FF,$FF,$FF,$FF,$FF,$3F,$FF
        FCB     $C3,$FF,$FF,$FF,$FF,$03,$FF,$FF,$FC,$FF,$FF,$FF,$FF,$FF,$F0,$FF,$FF
        FCB     $CF,$FC,$FF,$F0,$03,$F3,$FF,$FF,$FF,$0F,$FF,$FF,$FF,$FF,$0F,$FF,$FF
        FCB     $0F,$FF,$FF,$03,$F3,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$3F,$FF,$FF
        FCB     $0F,$FF,$FF,$3F,$F3,$FC,$FF,$FF,$FF,$3F,$FF,$FF,$FF,$FF,$C3,$FF,$FF
        FCB     $C3,$FC,$FF,$00,$03,$F0,$FF,$FF,$FC,$FF,$FF,$FF,$FF,$FF,$FC,$FF,$FF
        FCB     $F0,$3F,$FF,$FF,$FF,$C3,$FF,$FF,$C3,$FF,$FF,$FF,$FF,$FF,$FF,$0F,$FF
        FCB     $FF,$03,$FF,$FF,$FF,$CF,$FF,$FF,$3F,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$FF
        FCB     $FF,$F0,$3F,$FF,$F0,$0F,$FF,$FC,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3F
        FCB     $FF,$FF,$00,$00,$03,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF


        END     $4000
