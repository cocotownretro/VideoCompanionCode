        ORG	$4000

BlockReg        EQU     $FFA3           ; The (executive set) task register we'll play with: Logical addresses $6000 - $7FFF
LogAddrStart    EQU     $6000           ; Logical address of start of block we'll play with
WriteSize       EQU     $2000-$200      ; 8K block size, but leave top $200 alone (hardware registers are accessed there for some blocks, so just keep out)
LogBlockSkipLo  EQU     $38             ; Skip physical blocks initially allocated to the 64K logical space.  This avoids us writing to...
LogBlockSkipHi  EQU     $3F             ; ...this code, our stack space, interrupt handlers, etc.

; ---------------------------------------------------------------
; Main
; ---------------------------------------------------------------

        ; Init
        LDS     #LogAddrStart   ; Start stack at end of 8K block containing this code, and just before the logical block we'll write to
        CLRB                    ; Init task register value to 0
WriteBlockLoop:
        CMPB    #LogBlockSkipLo ; Avoid writing test data into the original 64K BASIC booted us to
        BEQ     SkipBlocks
AfterSkipBlocks:
        STB     TRVal           ; B = task register value we'll use; remember it in a variable
        STB     BlockReg        ; Change map to actually use task register value
        CLRA                    ; D = TRVal extended to 2 bytes
        LSLB                    ; B *= 2
        BCC     DoneMul2        ; Increment A if there's a carry
        INCA
DoneMul2:
        TFR     D,U             ; U is now TRVal * 2 = most significant 2 bytes of physical address
        LDX     #LogAddrStart   ; X -> start of logical block to write to
        LDD     #WriteSize-2    ; D = offset into block to write to; go backward from end (high addresses) to beginning (low addresses)
WriteWordLoop:
        STU     D,X             ; Store word
        SUBD    #2              ; Next location
        BGE     WriteWordLoop   ; Again
        LDB     TRVal           ; Done with block.  Restore B with Task register value we just used
NextBlock:
        INCB                    ; Next task register value
        BNE     WriteBlockLoop  ; Outer loop iterate

Pause   BRA     Pause           ; Loop here when done

SkipBlocks:
        LDB     #LogBlockSkipHi+1
        BRA     AfterSkipBlocks

        

TRVal   RMB     1

        END     $4000



