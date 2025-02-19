H6309   EQU     0
        INCLUDE sys/os9.d
        MOD     Length,Name,Prgrm+Objct,ReEnt+1,CodeStart,DataEnd
Name:
        FCS     "hw"
Stack   RMB     $100
DataEnd EQU     .
Message_Start:
        FCC     "Hello, world!"
Message_End:
CodeStart:
        ; Let's intialize the stack area.  This ensures we don't accidentally
        ; rely on stale bytes that might have coincidentally ended up here
        ; during the loading of this module.  In the video, before I used
        ; PC-relative addresing below, I was inadvertently relying on those
        ; bytes, and everything worked when it shouldn't have.  By initializing
        ; bytes in my data area, I force a failure if I forget to use
        ; PC-relative addressing
        LDD     #$4242
        LDX     #$0
InitLoop:
        STD     ,X++
        CMPX    #DataEnd-Stack
        BLO     InitLoop

        ; Now for the real code.  Print our greeting
        LDA     #1              ; Standard out
        LEAX    Message_Start,PCR
        LDY     #Message_End-Message_Start
        OS9     I$Write
        CLRB                    ; Clear carry so no error returned
        OS9     F$Exit
        EMOD
Length  EQU     *