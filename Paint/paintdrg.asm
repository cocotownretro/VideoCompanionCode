; Original PAINT ROM code from:
; https://github.com/davidlinsley/DragonBasic/blob/main/src/ECBGRP.SA
; with some symbols and comments from Extended Color BASIC Unravelled
;
; Instrumentation code, plus alternative DFS implementation
; by CoCo Town
;
; All-caps comments originally from Dragon source or Unravelled.
; Mixed-case comments added by me.
;
; To run, scroll down to "static switches" to configure which parts
; of the algorithm to skip, and how the additional instrumentation
; code should behave; change PAUSE_AT_Y to determine which Y-coordinates
; should forcibly pause the algorithm; then assemble.  Finally,
; run PAINT.BAS.  Consult its code to see how it calls this version
; of PAINT and DFS
;
; When running, SPACE toggles between pause & resume,
; ENTER does single step and then pauses
;

        ORG     $3F00
        * ORG     $98EB

; For reference, some relevant definitions from Unravelled
; 00BD HORBEG RMB 2 *DV* *PV HORIZ COORD - START POINT
; 00BF VERBEG RMB 2 *DV* *PV VERT COORD - START POINT
; 00C1 CSSYAL RMB 1 *PV SCREEN'S COLOR SET ARGUMENT
; 00C2 SETFLG RMB 1 *PV PRESET/PSET FLAG: 0=PRESET, 1=PSET
; 00C3 HOREND RMB 2 *DV* *PV HORIZ COORD - ENDING POINT
; 00C5 VEREND RMB 2 *DV* *PV VERT COORD - ENDING POINT
; 00C7 HORDEF RMB 2 *PV HORIZ COORD - DEFAULT COORD
; 00C9 VERDEF RMB 2 *PV VERT COORD - DEFAULT COORD

CHRGET  EQU     $009F
ONECRD  EQU     $93B2
GSCALE  EQU     $931D
SETFLG  EQU     $00C2
CMCOLR  EQU     $9581

; These are filled in automatically by ROM subroutines.  They're used
; by PAINT as temporary places to hand off both the paint active color
; and the border color
ACTCLR  EQU     $00B4   ; 1 byte (WCOLOR), active color minus 1 (e.g., 2)
ACTCLE  EQU     $00B5   ; 1 byte filled with pixels of the active color (e.g., $55)
PNTCLR  EQU     $00D8   ; active paint color (e.g., $55)
STMAXS  EQU     $9522
PTADRS  EQU     $928F
PNTADR  EQU     $00D9
; Current scanning direction.  UP/DN FLAG UP=1; DOWN=$FF;
; Meaning is reversed from my intuition.  UP = "greater Y-coordinates", which
; means, spatially, to go down.  Similarly, DOWN means to go spatially up
; For this reason I just used the terminology of North / South in the
; video to avoid confusion.
; 0 = "dummy entry" causes scan to return immediately. 
PDIRCT  EQU     $00D7   
PNTSTK  EQU     $00DC
PNTCHF  EQU     $00DB   ; CHGFLG from unravelled
XSTORE  EQU     $00BD   ; HORBEG from unravelled
VERBEG  EQU     $00BF
PCOUNT  EQU     $00D1
YMAX    EQU     $00D5
YLOW    EQU     $00C0
XDEC    EQU     $9514
XINC    EQU     $9506
PNTTMP  EQU     $00CB   ; Temp space to pass count from ENTERL or ENTERS to ENTCON
GETSTK  EQU     $AC33   ; ROM routine to verify there's enough stack space for B*2 bytes; returns if so, aborts with error if not
X2STOR  EQU     $00C3   ; HOREND
PCNT2   EQU     $00CD   ; Temp used just to store left count when summing left & right counts
DBLZER  EQU     $008A
XMAX    EQU     $00D3
GRPACT  EQU     $9377

; Further definitions used by instrumented code
POLCAT              EQU     $A000
KEY_SPACE           EQU     $20

; Lua values: addresses used by paint.lua to trigger
; function calls and to pass in values
LIST_START_TRIGGER  EQU     $7FF0
LIST_PUSH_TRIGGER   EQU     $7FF1
LIST_PULL_TRIGGER   EQU     $7FF2
LIST_START_ADDR     EQU     $7FF3
LIST_END_ADDR       EQU     $7FF5
CUR_PARAMS_ADDR     EQU     $7FF7


; Static switches: Modify and reassemble to modify
; instrumented behavior
SKIP_BORDER_CRAWL   EQU     0           ; Set to 1 to see what happens if algorithm omits the border scan
SKIP_RS_OVERHANG    EQU     0           ; Set to 1 to see what happens if algorithm omits check for right-side overhangs
SKIP_LS_OVERHANG    EQU     0           ; Set to 1 to see what happens if algorithm omits check for left-side overhangs
OVERHANG_PAUSE      EQU     0           ; Set to 1 to force pause-for-keystroke when overhang potential is detected
EXPLICIT_BORDER_SCAN EQU    0           ; Set to 1 to animate the border with a moving cursor
* BORDER_SCAN_PAUSE   EQU     $4000       ; Slower animated border scan
BORDER_SCAN_PAUSE   EQU     $1000       ; Quicker animated border scan

; PARAMETER BLOCK
; Calling code loads these up with the parameters instead of
; this paint code getting them from the ROM parser
PaintX              RMB     2
PaintY              RMB     2           
PaintFore           RMB     2           ; foreground color number + pixel byte of paint
PaintBord           RMB     1           ; border color (pixel byte only)

; jump table: Calling code EXECs one of these addresses to invoke
; either instrumented PAINT or instrumented DFS
JMPPAINT            JMP PAINT
JMPDFS              JMP DFS

; Auto-pause when we hit certain Y-coordinates, mainly for
; use when recording video segments to demonstrate what happens
; at certain interesting Y-coordinates
PAUSE_AT_Y:
                    ; Part A (paintline + stack)
                    * FCB     14,28,70,-1
                    ; Part B (border scan)
                    * FCB     15,28,192,29,16,-1
                    ; Part C (overhangs)
                    * FCB     70,112,192,110,-1
                    ; Stack pruning
                    * FCB     19,19,19,19,19,19,19,19,19,19,-1
                    * FCB     18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,-1
                    ; None
                    FCB     -1

; Pointer into PAUSE_AT_Y table to indicate next Y-coordinate
; to pause at
PauseAtYCur:
                    RMB     2

; Safe storage to hold current params for lua to read
; This address will be stored at CUR_PARAMS_ADDR
CUR_PARAMS_STORAGE  RMB     6

; ----------------------------------------------------------------
; Implementation of PAINT from coco / dragon, with additional
; instrumentation code
; ----------------------------------------------------------------

*
* PAINT (X,Y),PAINT COLOR,BORDER COLOR
* STARTING AT (X,Y) WE COLOR WITH PAINT COLOR UNTIL
* WE FIND THE BORDER COLOR
* ALL CONNECTED FIGURES CAN BE PROPERLY COLORED
* THE EDGE OF THE SCREEN IS CONSIDERED A BORDER
*

; RTS from prior routine preserved here as code from
; PAINT can branch back to it
ret     RTS

; PAINT routine from Dragon BASIC.  Parameters are now read from
; the parameter block above, rather than parsed by the
; BASIC interpreter
PAINT:
        LDA     #1                      ; Start off paused
        STA     Pause     
        CLR     ForcePause     
        LDX     #PAUSE_AT_Y             ; Init pause pointer
        STX     PauseAtYCur                 
        ; Set up params where ROM code expects them after parsing:
        LDX     PaintX
        STX     XSTORE
        LDX     PaintY
        STX     VERBEG
        LDA     PaintBord
        STA     PNTCLR
        LDD     PaintFore
        STD     ACTCLR
        PSHS    D
        JSR     GSCALE      NORMALIZE THE HOR, VER COORDINATES
        LDA     #&1         FLAG AS PSET CASE
        STA     SETFLG      SET PSET/PRESET FLAG TO PSET
        PULS    D           GET BACK ACTIVE COLOR (e.g., $02AA)
SCAN:
        ; MAKE A DUMMY ScanEntry ON THE STACK WITH DIRECTION OF 0
        CLRA        
        STS     LIST_START_ADDR
        PSHS    U,X,D    
        ; Inform LUA the list has started
        STS     LIST_END_ADDR
        STA     LIST_START_TRIGGER
        JSR     CheckKeystroke
        ;
        JSR     STMAXS      SETUP MAXIMUMS SO WE KNOW WHERE THE BORDER IS
        JSR     PTADRS      GET THE ADRESS MAP ROUTINE FOR
        STU     PNTADR      THIS SCREEN MODE AND SAVE IT
        JSR     SCANL       LOOK LEFT FILLING AS WE GO MAKING [D] A COUNT
        BEQ     PNTLP2      IF NOT, POP DUMMY ENTRY AND QUIT
        JSR     SCNCON      CONTINUE SCAN TO RIGHT AND PUT TOTAL IN [X]
        LDA     #&1         MAKE ENTRY TO LOOK UP
        STA     PDIRCT      UP/DN FLAG UP=1; DOWN=$FF (reversed! See comment at PDIRCT def)
        JSR     ENTERS      MAKE SCAN ENTRY WITH X2STOR & PDIRCT (up)
        NEG     PDIRCT      MAKE ENTRY FOR LOOKING DOWN (down)
        JSR     ENTERS
PNTLP2  STS     PNTSTK      Store stack pointer in case PNTCHF
                            *    equals zero since we don't initialize
                            *    PNTCHF.
; PNTLOP: Check change flag first.  If no change, prune stack (restore
; S), then fall through to PNTPUL.  Else go directly to PNTPUL
PNTLOP  TST     PNTCHF      Anything changed?
        BNE     PNTPUL      Yes, use the next stack entry.
        LDS     PNTSTK      No, prune the stack.
        ; Inform LUA of the prune
        STS     LIST_END_ADDR
        STA     LIST_PULL_TRIGGER
        JSR     CheckKeystroke
        ;

; PNTPUL: Pull topmost scan entry off stack, then fall through
; to recursive scan entrypoint.
PNTPUL:
        ; Inform LUA of the params we're about to pull
        LDX     #CUR_PARAMS_STORAGE
        STX     CUR_PARAMS_ADDR
        LDD     ,S
        STD     ,X
        LDD     2,S
        STD     2,X
        LDD     4,S
        STD     4,X
        ; Check if we're supposed to stop on this Y
        LDX     PauseAtYCur                     ; X -> next Y-coordinate to stop at
        LDA     ,X+                             ; A = next Y-coordinate to compare against
        CMPA    #-1                             ; end of list?
        BEQ     PPPullEntry                     ; If so, just continue with algorithm
        SUBA    ,S                              ; Account for the directional mod we'll make to Y after entry is pulled
        CMPA    1,S                             ; Does it match the Y we will pull + adjust?
        BNE     PPPullEntry                     ; If not, just continue with algorithm
        INC     ForcePause                      ; If so, set state to pause
        STX     PauseAtYCur                     ; ... and advance pause pointer
PPPullEntry:
        ; PULL SCAN ENTRY OFF THE STACK
        PULS    D,X,U
        ; Inform LUA of the pull
        STS     LIST_END_ADDR
        STA     LIST_PULL_TRIGGER
        JSR     CheckKeystroke
        ;
        CLR     PNTCHF                          Say nothing changed yet.
        STS     PNTSTK                          Remember stack pointer for possible pruning.

; Scan entries are (in increasing addr order): 
* [A] HAS DIRECTION
* [B] HAS Y VALUE
* [X] HAS X VALUE
* [U] HAS POINT COUNT
* THE X AND Y ARE TO THE LEFT OF THE
* PLACE TO SCAN

* THIS IS THE RECURSIVE SCAN ENTRY POINT. THE IDEA IS THAT
* ANY NON-BORDERPOINTS ARE CONNECTED STARTING TO THE RIGHT
* OF X,Y AND EXTENDING FOR COUNT POINTS. WE COLOR THE NON-BORDER
* POINTS AND MAKE NEW STACK ENTRIES FOR EACH NEW CONTIGUOUS GROUP
* OF CONNECTED POINTS. THIS WILL ALWAYS BE IN THE CURRENT SCAN DIRECTION
* UNLESS THERE IS OVERHANG ON THE LEFT OR RIGHT

        LEAX   &1,X                             MOVE TO THE RIGHT TO GET INTO THE SCAN AREA
        STX    XSTORE
        STU    PCOUNT                           ; Move count from list entry to PCOUNT; Unraveled: length of parent line
        STA    PDIRCT                           SAVE THE DIRECTION
        LBEQ   ret                              IF DUMMY ENTRY, QUIT
        BMI    SCANDN                           IF NEGATIVE (FF = down) DECREMENT Y VALUE
        ; unraveled: CHECK LINE BELOW CURRENT DATA
        INCB                                    else direction = 1 = up, so increment Y value
        CMPB   YMAX+&1                          SEE IF OFF THE BOTTOM
        BLS    SCNCN1                           IF NOT KEEP SCANNING
        CLRB                                    MAKE 0 SO BRANCH TO PNTLOP
SCANDN  TSTB
        BEQ    PNTLOP                           IF AT TOP, TRY NEXT ENTRY
        DECB                                    DECREMENT Y
SCNCN1  STB    YLOW                             SAVE Y VALUE
        JSR    SCANL                            SCAN LEFT TO EXTEND
        BEQ    SCNCN4                           ; no points scanned; skip scanning right (why?)
        ; unraveled comment about overhang potential:
        *SEE IF < 3 PIXELS WERE PAINTED - IF FEWER THAN
        *THREE PIXELS WERE PAINTED THEN THERE IS NO NEED TO
        *CHECK FOR MORE DATA TO 'PAINT' ON THE LINE TO THE
        *LEFT OF THE CURRENT POSITION IN THE OPPOSITE
        *DIRECTION THAT THE UP/DN FLAG IS CURRENTLY SET TO
        CMPD   #&3                              ENOUGH TO OVERHANG ON LEFT SIDE SO NEED TO LOOK IN OPPOSITE DIRECTION
        ; For demo purposes, skip LS overhang check if requested
 IFNE SKIP_LS_OVERHANG
        BRA     NLSENT
 ENDC
        ;
        BCS     NLSENT                          IF NOT NO NEW SCAN ENTRY
        ; For demo purposes, pause for keystroke when overhang potential identified
 IFNE OVERHANG_PAUSE
        INC     ForcePause                      ; A momentous occasion, pause regardless of single-step mode
 ENDC
        JSR     CheckKeystroke
        ;
        LEAX    -&2,X                           SUBTRACT TWO TO GET SCAN SIZE
        BSR     ENTERL                          MAKE THE ENTRY (opposite direction)
NLSENT  JSR     SCNCON                          CONTINUE SCAN TO RIGHT
SCNLC1  BSR     ENTERS                          MAKE ENTRY (same direction)
SCNCN4:                     
        COMA
        COMB                                    SUBTRACT [D]+1 FROM TOTAL
; SCNLCN: Border scan:
; Loops, repeatedly incrementing X coordinate
; and decrementing PCOUNT, until PCOUNT <= 0 (goto SCNFIN) or
; we hit a non-border pixel (goto SCNLC2 to scan right)
; Unraveled: * THIS CODE WILL INSURE THAT THE CURRENT LINE IS
* EXAMINED TO THE RIGHT FOR 'PAINTABLE' PIXELS FOR
* A LENGTH EQUAL TO THE LENGTH OF THE 'PARENT' LINE
; 'parent' being the line whose paint-line routine caused
; the current entry we're now executing to be pushed
SCNLCN  ADDD    PCOUNT                          ADD ONTO PCOUNT
        STD     PCOUNT                          AND UPDATE
 IFNE SKIP_BORDER_CRAWL
        BRA     SCNFIN
 ENDC
        BLE     SCNFIN                          IF 0 OR LESS NO MORE SCANNING
        ; Increments XSTORE / HORBEG, result also in X
        JSR     XINC                            MOVE RIGHT IF MORE POINTS
        JSR     BlinkCurPoint                   ; me: visualize scan
        JSR     TSTBRD                          SEE IF BORDER OR NOT
        BNE     SCNLC2                          IF NOT, CALL SCANR
        LDD     #-&1                            SUBTRACT ONE FROM POINT COUNT
        BRA     SCNLCN
SCNLC2  JSR     XDEC                            GO DEC HOR COORD
        BSR     SAVLOC                          SAVE LEFT POSIION
        BSR     SCANR                           SCAN FOR COMPLETE SIZE (now)
        BRA     SCNLC1                          GO MAKE ENTRY (for child scanline of that scanr) AND SUBTRACT (size of scanr from point count we're looping through as we pass through border pixels)
*
* WE ARE FINISHED EXTCEPT WE MIGHT NEED TO MAKE
* A REVERSE DIRECTION ENTRY FOR RIGHT SIDE OVERHANG
*
SCNFIN  JSR     XINC                            NO CHECK IF RIGHT OVERHANG NEEDS A SCAN
        LEAX    D,X                             SUBTRACT OVERHANG SIZE FROM STOP POSITION
        STX     XSTORE
        COMA                                    MAKE IT A POSITIVE COUNT
        COMB
        SUBD    #&1                             IF STOPPED AT END+1 ALSO NO OVERHANG
 IFNE SKIP_RS_OVERHANG
        BRA     PQUIT
 ENDC
        BLE     PQUIT                           TAKES AT LEAST 2 POINTS TO OVERHANG
        TFR     D,X
 IFNE OVERHANG_PAUSE
        INC     ForcePause                      ; A momentous occasion, pause regardless of single-step mode
 ENDC
        JSR     CheckKeystroke
        BSR     ENTERL                          ENTER OVERHANG SCAN
PQUIT   JMP     PNTLOP


Halt    JMP     Halt

; ENTERL & ENTERS push new scan entry.  Tip of stack contains
; address these subs need to return to.  So they ensure the
; scan entry goes below the return address, so the RTS will work.
;
; ENTERL: Push new scan entry, opposite direction, using
; X coord from XSTORE
ENTERL  STD     PNTTMP                          SAVE [D] WHICH HAS CURRENT COUNT
        PULS    Y                               TAKE OFF THE RETURN ADDRESS
        LDD     XSTORE                          SAVE COUNT AND X VALUE
        PSHS    X,D
        LDA     PDIRCT                          GET DIRECTION
        NEGA
; ENTCON: Common code for ENTERL & ENTERS
ENTCON:
        CMPS    #StackBoundary
        BLS     Halt
        LDB     YLOW
        PSHS    D                               SAVE DIRECTION AND Y VALUE
        ; Inform LUA of the push
        STS     LIST_END_ADDR
        STA     LIST_PUSH_TRIGGER
        JSR     CheckKeystroke
        ;
        PSHS   Y                                PUT RETURN ADDRESS BACK ON
        ; Interesting note from unraveled (bug!)
        * CODE BELOW CHECKS FOR ABILITY TO STORE FOUR BYTES IN
        * FREE RAM, HOWEVER THE PAINT ROUTINE WILL STORE SIX
        * BYTES IN FREE RAM - FIRST INSTRUCTION SHOULD BE LDB #3
        LDB     #&2                             CHECK FOR STACK SPACE
        JSR     GETSTK                          (aborts and does not return if out of stack space)
        LDD     PNTTMP                          GET BACK SAVED [D] COUNT
        RTS
; ENTERS: Push new scan entry, same direction, using
; X coord from X2STOR
ENTERS  STD     PNTTMP
        PULS    Y                               GET RETURN ADDRESS
        LDD     X2STOR                          GET X VALUE FROM SAVED POSITION
        PSHS    X,D
        LDA     PDIRCT                          USE CURRENT DIRECTION
        BRA     ENTCON                          MERGE WITH ENTERL CODE
SAVLOC  LDX     XSTORE                          MOVE PRIMARY INTO SECONDARY: XSTORE -> X2STOR
        STX     X2STOR
        RTS

* ScanConnect / ScanContinue
* CALLED HERE WHEN WANT TO  SCAN RIGHT AFTER A SCANL AND PUT THE
* TWO PIECES TOGETHER ALL AS ONE SCAN. WE HAVE STORED THE
* RIGHT POSITION OH THE LEFT SCAN IN THE SECONDARY
* WE ADD THE SIZES TOGETHER AND LEAVE THE TOTAL IN [X]
*
SCNCON  STD     PCNT2                           SAVE SIZE OF LEFT PIECE
        ; Next 3 instructions swap XSTORE <-> X2STOR
        LDY     X2STOR                          SAVE SECONDARY IN REGISTERS
        BSR     SAVLOC                          XSTORE -> X2STOR
        STY     XSTORE
        BSR     SCANR                           SCAN RIGHT MAKING COUNT IN [D]
        LDX     PCNT2                           GET SIZE OF PREVIOUS (left) SCAN
        LEAX    D,X                             COMBINE SIZE OF LEFT AND RIGHT
        ADDD    #&1                             INCLUDE FIRST POINT IN LEFT IN SCAN SIZE
        RTS

*
* Paintline routine
* THESE ARE THE MOST IMPORTANT SCAN ROUTINES
* THEY SCAN EITHER LEFT OR RIGHT UNTIL THEY FIND A BORDER
* POINT OR THE EDGE. AS THEY SCAN THEY COLOR AND KEEP A POINT COUNT
* WHICH IS RETURNED IN [D] and [X]. XSTORE IS UPDATED AS THF SCAN PROCEEDS
* THE POINT VALUE RETURNED IN XSTORE IS EITHER AN EDGE OR BORDER
; on return the CC's are set based on SUBD DBLZER, so a BEQ
; would branch if the point count were 0, for example.
*
SCANL   JSR     SAVLOC
        LDY     #XDEC                           SCAN LEFT DECREMENTS X AS IT GOES
        BRA     SCANLC
SCANR   LDY     #XINC                           SCAN RIGHT INCREMENTS X
        JSR     ,Y
; init vars used in loop
SCANLC  LDU     DBLZER                          U = point count, SET COUNT VALUE TO 0
        LDX     XSTORE                          X -> current x coord, START AT XSTORE
; loop: each iteration extends stroke by 1 pixel, increments U
SCANLP  BMI     SCLRTS                          RETURN IF NEGATIVE 1 ALREADY
        CMPX    XMAX                            SEE IF AT HIGH END
        BHI     SCLRTS                          IF HIGHER MUST BE ON EDGE
        PSHS    U,Y                             SAVE COUNT (U) AND ROUTINE ADDRESS (Y)
        BSR     TSTBRD                          SEE IF BORDER
        BEQ     SCLRT2                          IF BORDER, QUIT
        ; Does a PSET (using addr X, pixel mask A),
        ; setting PNTCHF (i.e., CHGFLG) if the PSET actually changed the state of the pixel
        ; (PNTCHF is left alone--not reset--if no change)
        ; SET THE COLOR AT THE DESIRED POINT
        JSR     GRPACT   
        PULS    Y,U                             GET BACK COUNT AND ROUTINE ADDRESS
        LEAU    &1,U                            UPDATE COUNT
        JSR     ,Y                              UPDATE X VALUE
        BRA     SCANLP                          AND CONTINUE SCANNING
SCLRT2  PULS    Y,U                             TAKE OFF COUNT AND ADDRESS
SCLRTS  TFR     U,D                             MOVE COUNT INTO [D]
        TFR     D,X
        SUBD    DBLZER
RTSSCL:
        * JSR     CheckKeystroke                ; Uncomment to allow pausing on every scan
        RTS

; helper sub: Asks if current point is border color.  Z flag set if equals border.
; So caller can do BEQ IsBorder (or BNE NotBorder)
TSTBRD:
        ; Call Routine based on PMODE
        ; sets X to ADDRESS OF THIS POINT,
        ; A to bitmask for pixel within screen byte (e.g., 00001100)
        JSR     [PNTADR]
        TFR     A,B                             SET [B] TO BE A COPY OF THE BIT MASK
        ANDB    PNTCLR                          B = MASK BORDER COLOR TO GET SELECTED BORDER BITS
        PSHS    D                               S -> A (pixel mask), B (pixel border bits) SAVE THE BIT MASK IN [A]
        ANDA    ,X                              A = actual pixel bits READ CURRENT DATA IN AND MASK
        CMPA    &1,S                            A == pixel border bits? (caller does conditional branch) SEE IF MATCH BORDER COLOR
        PULS    D,PC                            RETURN WITH CONDITION CODES SET


; ----------------------------------------------------------------
; Simple dfs alternative to PAINT
; Note: does no marking--assumes no pixels exist in the connected
; area to paint that are already in the paint color (i.e.,
; painting IS marking)
; ----------------------------------------------------------------

; For simplicity, keeping parameter block same as PAINT
* PaintX      RMB     2
* PaintY      RMB     2
* PaintFore   RMB     2       ; foreground color of paint + pixel
* PaintBord   RMB     1       ; border color (pixel byte only)

; For simplicity, keeping scan entries same as regular PAINT
; (in increasing addr order): 
* [A] HAS DIRECTION (unused, but will be filled in with values for lua's benefit)
* [B] HAS Y VALUE
* [X] HAS X VALUE
* [U] unused (HAS POINT COUNT)
* THE X AND Y ARE not TO THE LEFT OF THE
* PLACE TO SCAN--they are exact.  Lua visualization of stack entries
; will be off.
DFS:
        STS     OrigStack
        LDA     PaintBord                       ; Init ROM's vars for border & foreground colors & pixels
        STA     PNTCLR
        LDD     PaintFore
        STD     ACTCLR
        JSR     STMAXS                          SETUP MAXIMUMS SO WE KNOW WHERE THE BORDER IS
        JSR     PTADRS                          GET THE ADRESS MAP ROUTINE FOR
        STU     PNTADR                          THIS SCREEN MODE AND SAVE IT
        ; Get initial (X,Y) params from user-poked memory
        LDX     PaintX
        LDD     PaintY
        ; Inform LUA the list has started
        STS     LIST_START_ADDR
        STS     LIST_END_ADDR
        STA     LIST_START_TRIGGER
        JSR     CheckKeystroke
DFSLoop:
        PSHS    X,D                             ; Save params before we clobber them
        ; Put cur (X,Y) into known locations so ROM can help us
        STX     XSTORE
        STD     VERBEG
        ; Is cur (X,Y) the border or paint color?  (If either, skip.)
        JSR     [PNTADR]                        ; Puts (X,Y) addr into X and bitmask into A
        TFR     A,B                             SET [B] TO BE A COPY OF THE BIT MASK
        ANDB    ,X                              B = actual pixel bits
        PSHS    D                               ; S -> A (pixel mask), B (actual pixel bits)
        ANDA    PNTCLR                          ; A = border color bits for pixel
        CMPA    1,S                             ; Same as actual color bits?
        BEQ     DFSSkip                         ; If yes, skip
        LDA     ,S                              ; Restore A = mask
        ANDA    ACTCLR+1                        ; A = paint color bits for pixel
        CMPA    1,S                             ; Same as actual color bits?
        BEQ     DFSSkip                         ; If yes, skip
        ; Checks are done, need to paint this pixel
        ORA     ,X                              ; Paint it
        STA     ,X
        PULS    D                               ; Restore stack: undo temporary PSHS D, then put params back into D,X                     
        PULS    D,X
        ; Push recursive params
        CLRA
        INCA
DFSPushNorth:  
        DECB                                    ; Go north
        BLT     DFSPushSouth                    ; If too high, skip
        PSHS    U,X,B,A                         ; Push entry
        ; Let lua know we pushed
        STS     LIST_END_ADDR
        STA     LIST_PUSH_TRIGGER
        JSR     CheckKeystroke
DFSPushSouth:  
        ADDB    #2                              ; From north to south
        BVS     DFSPushWest                     ; If overflow (wrapped around to 0), skip
        PSHS    U,X,B,A                         ; Push entry
        ; Let lua know we pushed
        STS     LIST_END_ADDR
        STA     LIST_PUSH_TRIGGER
        JSR     CheckKeystroke
DFSPushWest:
        DECB                                    ; B is now original Y coordinate
        LEAX    -1,X                            ; X-coord goes west
        CMPX    #0
        BLT     DFSPushEast                     ; If too west, skip
        PSHS    U,X,B,A                         ; Push entry
        ; Let lua know we pushed
        STS     LIST_END_ADDR
        STA     LIST_PUSH_TRIGGER
        JSR     CheckKeystroke
DFSPushEast:
        LEAX    2,X                             ; From west to east
        CMPX    #255                            ; Too far east?
        BHI     DFSContinue                     ; If so, skip
        PSHS    U,X,B,A                         ; Push entry
        ; Let lua know we pushed
        STS     LIST_END_ADDR
        STA     LIST_PUSH_TRIGGER
        JSR     CheckKeystroke
DFSContinue:
        CMPS    OrigStack                       ; Any more list entries?
        BEQ     DFSRet                          ; If no, done
        PULS    A,B,X,U                         ; Get next scan entry
        ; Let lua know we pulled
        STS     LIST_END_ADDR
        STA     LIST_PULL_TRIGGER
        JSR     CheckKeystroke
        JMP     DFSLoop                         ; Process it
DFSSkip:
        PULS    Y,D,X                           ; Restore stack: undo temporary PSHS D, then put params back into D,X                     
        BRA     DFSContinue
DFSRet:
        RTS 

OrigStack:
        RMB     2

; ----------------------------------------------------------------
; Keyboard control & other instrumentation additions
; SPACE toggles between pause & resume
; ENTER does single step and then pauses
; ----------------------------------------------------------------

CheckKeystroke:
        PSHS    U,Y,X,D,CC
        LDA     ForcePause              ; Check if caller wants to force user into single-step mode
        BEQ     WFKLoop                 ; If not, proceed as normal
        LDA     #1                      ; Otherwise, enter single-step mode
        STA     Pause
        CLR     ForcePause
WFKLoop:
        JSR     [POLCAT]                ; Any keystoke?
        BEQ     WFKContinue             ; If not, continue
        CMPA    #KEY_SPACE
        BEQ     OnSpace
; OnSingleStep
        LDA     #1                      ; Set pause state to ensure single step
        STA     Pause
        BRA     WFKReturn               ; Return
OnSpace:
        NEG     Pause                   ; Toggle pause state
        ; Fall through to ContinueLoop
WFKContinue:
        LDA     Pause                   ; For next iteration, check pause state...
        BGT     WFKLoop                 ; ...to see if we should check keyboard first...
WFKReturn:
        PULS    CC,D,X,Y,U,PC           ; ...or just return

; Tracks if user wants us to pause (cuz in single-step mode)
Pause:
        RMB     1
; Tracks if caller to CheckKeystroke deliberately wants to pause until keystroke
; regardless of whether user is in single-step mode.  This forcibly pauses
; the algorithm at interesting points
ForcePause:
        RMB     1

; ----------------------------------------------------------------
; Called during border scan to explicitly blink the pixel we're
; currently scanning.  (For visualization purposes.)
; ----------------------------------------------------------------
BlinkCurPoint:
 IFEQ EXPLICIT_BORDER_SCAN
        RTS
 ENDC
        PSHS    U,Y,X,D,CC
        ; Call Routine based on PMODE
        ; sets X to ADDRESS OF THIS POINT,
        ; A to bitmask for pixel within screen byte (e.g., 00001100)
        JSR     [PNTADR]
        TFR     A,B                     SET [B] TO BE A COPY OF THE BIT MASK
        EORA    ,X                      ; Blink point
        STA     ,X  
        ; Pause by spinning in a loop
        LDY     #BORDER_SCAN_PAUSE
BCPPause:
        LEAY    -1,Y
        BNE     BCPPause
        EORB    ,X      ; Restore point
        STB     ,X  
        PULS    CC,D,X,Y,U,PC

StackBoundary       EQU      *+10

        END     PAINT