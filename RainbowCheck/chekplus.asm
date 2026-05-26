		; This is not where the BASIC driver actually POKEs the code,
		; but it's a convenient place just for the purposes of
		; cross-assembly & in-emulator debugging.
        ORG     $6000

Install:
        ; Initial EXEC brings us here, where we install our code
        ; as the new console in handler
        LDA     >$016A                  ; A = jump opcode from RVEC4 (console in)
        STA     <CallNextHandler,PCR    ; store it at place we'll return from
        LDA     #$7E                    ; Explicitly place JMP at RVEC4
        STA     >$016A                   
        LDX     >$016B                  ; X = original vector address for RVEC4
        STX     <CallNextHandler+1,PCR  ; store it after our return JMP instruction so we'll end up in original console in when done
        LEAX    <CHECK,PCR              ; X = addr of our CHECK routine
        STX     >$016B                  ; Make that the new RVEC4 vector address
        RTS                             ; setup complete
        ; Handler begins here: CHECK routine calculates and prints checksum
CHECK   CMPA    #$0A                    ; Down arrow key?
        BNE     CallNextHandler         ; If not, get out of here
        PSHS    X,D                     ; Save registers
        CLRA                            ; Start checksum at 0
        LDX     <$0019                  ; X = addr of start of BASIC program, -> first line's nextline ptr
ChecksumNextLine:
		; Outer loop: Each iteration completes a BASIC line
        LDB     ,X++                    ; B = first byte of nextline ptr, X -> line number
        BEQ     CalculationComplete     ; Exit loop on a zero byte (nextline ptr MSB == 0 => entire ptr must be 0)
        ADDA    ,X+                     ; A += first byte of line
ChecksumNextByte:
		; Inner loop: Each iteration completes one byte from a BASIC line
        ADDA    ,X+                     ; A += next byte of line
        LDB     ,X                      ; B = next byte of line
        BNE     ChecksumNextByte        ; Repeat if B != 0
        LEAX    1,X                     ; Else, X -> nextline ptr of next line
        BRA     ChecksumNextLine        ; Next outer loop iteration
CalculationComplete:   
        STA     >$02DE                  ; Store sum at basic line input buffer + 2
        LEAX    <BasicCmd,PCR           ; X -> beginning of embedded PRINT(PEEK(734)) command
        STX     <$00A6                  ; Save X at CHARAD
        LDA     ,X                      ; A = *X
        ANDCC   #$FE                    ; Clear carry flag (maybe an indication the next character to send to the interpreter is non-numeric)
        JSR     >$ADC6                  ; Execute BASIC command
        PULS    X,D                     ; Restore registers
CallNextHandler:
        JMP     $C5BC                   ; Done, chain back to original Console In vector
BasicCmd:
        FCB     $87                     ; Token for PRINT command
        FCB     $FF,$86                 ; Token for PEEK function
        FCN     "(734)"                 ; $2DE
        FCB     $0                      ; End of BASIC command

        END     Install
