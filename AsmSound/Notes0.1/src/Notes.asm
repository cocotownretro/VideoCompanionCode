; ------------------------------------------------------------------------------
; |                                                                            |
; |   Background 4-note chord player for CoCo I/II controllable from BASIC.    |
; |   https://github.com/cocotownretro/VideoCompanionCode/tree/main/AsmSound   |
; |                                                                            |
; |   Algorithms, tricks, and much of the code from the CoCo III's FIRQV4      |
; |   music player with the following credits:                                 |
; |                                                                            |
; |          *************************************************************     |
; |          * 4-Voice Music Player - adapted by Paul Fiscarelli         *     |
; |          *                        nuked by Simon Jonassen            *     |
; |          *                                                           *     |
; |          * Original author credits to:                               *     |
; |          *  Clell A. Dildy Jr. -'68' Micro Journal, March 1982       *     |
; |          *  Garry and Linda Howard - Color Computer News, July 1982  *     |
; |          *  Larry Konecky - Rainbow Magazine, December 1983          *     |
; |          *  Bob Ludlum - Rainbow Magazine, July 1984, Music+ 3/11/84 *     |
; |          *                                                           *     |
; |          * Original algorithm credits to:                            *     |
; |          *  Hal Chamberlain - Byte Magazine, September 1977          *     |
; |          *  https://archive.org/details/byte-magazine-1977-09        *     |
; |          *                                                           *     |
; |          *************************************************************     |
; |                                                                            |
; |   with some updates by Dave (CoCo Town)                                    |
; |                                                                            |
; ------------------------------------------------------------------------------

                                ORG     $5000

GETARG                          EQU     $B3ED   ; BASIC routine to get BASIC parameter to USR call


; ---------------------------------------------------------------
; BASIC / ASM interface
; Routine entrypoints are jump stubs.  This ensures the
; entry addresses remain unchanged even if the code of the
; routines changes.
; ---------------------------------------------------------------
SetWaveformAddr JMP     SetWaveform     ; DEFUSR0 = &H5000
SetDurationAddr JMP     SetDuration     ; DEFUSR1 = &H5003
AddChordAddr    JMP     AddChord        ; DEFUSR2 = &H5006
PlaySongAddr    JMP     PlaySong        ; DEFUSR3 = &H5009
StopSongAddr    JMP     StopSong        ; DEFUSR4 = &H500C

; ---------------------------------------------------------------
; Waveform page numbers (for use in SetWaveform)
; All waveforms preceded with "Wg" created by Paul Fiscarelli's
; Waveform Generator tool: https://github.com/pfiscarelli/CoCo_Waveform_Generator
; These are scaled down to allow 4 simultaneous voices without clipping
;
; $5E: WaveTableSine256         Sine of multiples-of-4
; $5F: WgSine                   Pure sine
; $60: WaveTableSquare256       Extreme low / high square wave
; $61: WgSquare                 Square synthesized from odd-harmonic sine waves
; $62: WgTrapez                 Trapezoid (I think?) synthesized from odd-harmonic sine waves
; $63: WgOrgan                  Synthesized organ
; $64: WgSawtooth               Sawtooth shape (gradual fall + sudden rise)
; $65: WgTriangle               Triangle shape
; $66: WgViolin                 Synthesized violin
; $67: WgImpulse                Mostly flat with single peak
;
; You can also "play" arbitrary memory as if it were a waveform.
; Note that these are NOT scaled to work with 4-voices, so you
; will get clipping if you use these with more than one voice.
; Not that it matters.  These all sound pretty terrible even
; with only a single voice.  But go for it.
;
; $00: BASIC working storage
; $01: BASIC vectors, jump addresses, tables
; $04-05: Text screen
; $06-0D: Disk BASIC working storage
; $0E-3D: Hi-res graphics screens
; $80-DF: Various ROM chips, cartridge slot
; $FF: Hardware, I/O
; ---------------------------------------------------------------

		opt	cc
                opt     cd
		opt	ct
; ---------------------------------------------------------------
; SoundOut: HS interrupt handler
; This takes up just enough cycles to get triggered every 3rd
; line (instead of on contiguous lines).  That equates to
; 5,240 times per second.
; ---------------------------------------------------------------
SoundOut:
                LDA     #$50            ; Set DP so int handler can run faster
                TFR     A,DP            ; RTI will restore original DP (+ all regs)
LdDurMlt        LDA     #$00            ; SMC: A = duration multiplier
                BEQ     CheckDuration   ; Only check durations every $FF calls
                DECA                    ; Decrement and store duration multipler
                STA     <LdDurMlt+1
                ; Voice 1 calculation
WaveOff1        LDD     #$0000          ; SMC: A = current offset into wavetable, B = fractional part
FreqValue1      ADDD    #$0000          ; SMC: A & B updated based on note's frequency
                STD     <WaveOff1+1     ; SMC: store updated A & B as next wave offset
                STA     <SumWaveTbl1+2  ; SMC: store MSB of offset as LSB of WaveTable address (summing voices below)
                ; Voice 2 calculation
WaveOff2        LDD     #$0000          
FreqValue2      ADDD    #$0000          
                STD     <WaveOff2+1     
                STA     <SumWaveTbl2+2  
                ; Voice 3 calculation
WaveOff3        LDD     #$0000          
FreqValue3      ADDD    #$0000          
                STD     <WaveOff3+1     
                STA     <SumWaveTbl3+2  
                ; Voice 4 calculation
WaveOff4        LDD     #$0000          
FreqValue4      ADDD    #$0000          
                STD     <WaveOff4+1     
                STA     <SumWaveTbl4+2  
                ; Sum voices and play result
SumWaveTbl1     LDA     >$0000          ; SMC: Get voice 1 value from wavetable
SumWaveTbl2     ADDA    >$0000          ; SMC: Add voice 2 value from wavetable
SumWaveTbl3     ADDA    >$0000          ; SMC: Add voice 3 value from wavetable
SumWaveTbl4     ADDA    >$0000          ; SMC: Add voice 4 value from wavetable
                STA     >$FF20          ; Send sum of all voices out on DAC
                LDA     >$FF00          ; ack HS irq
                RTI
CheckDuration:
                ; We land here when duration multipler decrements to 0
                DEC     <DurationLeft   ; One less duration iteration left
                BEQ     NextChord       ; If chord is finished, get next one & restore duration
RestoreDurationAndMultipler:
                ; Use SMC to initialize duration multiplier to FF
                LDA     #$FF
                STA     <LdDurMlt+1     ; Init duration multiplier (FF)
                BRA     SoundOut        ; Back up to top (to play notes)
NextChord:
LdPlayPtr       LDU     #$0000          ; (SMC) U -> next chord
                ; Use SMC (with just a teensy bit of stack blasting--thanks, Simon!)
                ; to init freq values for next chord
                PULU    D,X             ; D & X are frequency values for voices 1 & 2
                STD     <FreqValue1+1
                STX     <FreqValue2+1
                PULU    D,X             ; D & X are frequency values for voices 3 & 4
                STD     <FreqValue3+1
                STX     <FreqValue4+1
                ; Init duration left (w/ full duration val)
LdDuration      LDB     #$00            ; B = (SMC) duration
                STB     <DurationLeft   ; Write B into duration left
                ; If we hit the end, cycle to the beginning
                CMPU    <SongEnd
                BHS     ResetPlayPtr
WriteNewPlayPtr:
                ; Whether the play pointer moved forward or reset to the
                ; beginning, this saves it from U back into memory
                STU     <LdPlayPtr+1
                BRA     RestoreDurationAndMultipler
ResetPlayPtr:
                ; Reset play pointer (U) to beginning
                LDU     #SongStart
                BRA     WriteNewPlayPtr
DurationLeft    RMB     1
SongEnd         RMB     2


; ---------------------------------------------------------------
; SetWaveform sub
; Entrypoint from BASIC.  SMC to set waveform addresses to the
; waveforms specified by BASIC.  On entry, A contains the high-order
; byte from the BASIC param, and B is the low-order byte.
; Example: &H0265 sets the second voice to a triangle wave
; Entry: A = 1-based index of voice to set (or 0 to set all)
; Entry: B = waveform to use, expressed as the page number (high-order
;            byte of address where 256-byte waveform starts).  See
;            waveform list at top of this file.
; ---------------------------------------------------------------
SetWaveform:
                JSR     GETARG
                TSTA                            ; A = which voice to set
                BEQ     SetWaveformAll          ; A=0 means set ALL voices
                LSLA
                LDX     #VoiceToWaveformAddr    ; X -> beginning of table
                LDX     A,X                     ; X = addr fetched from user-specified entry of table
                STB     ,X                      ; Set waveform
                RTS
SetWaveformAll:
                STB     >SumWaveTbl1+1
                STB     >SumWaveTbl2+1
                STB     >SumWaveTbl3+1
                STB     >SumWaveTbl4+1
                RTS
VoiceToWaveformAddr:
                FDB     $0000,SumWaveTbl1+1,SumWaveTbl2+1,SumWaveTbl3+1,SumWaveTbl4+1

; ---------------------------------------------------------------
; SetDuration sub
; Entrypoint from BASIC.  SMC to set duration to that specified
; by BASIC
; Entry: BASIC param is 8-bit value to be used as duration
; ---------------------------------------------------------------
SetDuration:
                JSR     GETARG
                STB     >LdDuration+1
                RTS

; ---------------------------------------------------------------
; PlaySong sub
; Entrypoint from BASIC.  Play chords previously added.  Auto-
; repeats when done.  Returns immediately as chords played
; asynchronously in the background
; ---------------------------------------------------------------
PlaySong:
                ORCC    #$50            ; Disable all interrupts
                BSR     InitDac
                BSR     InitInterrupts
                ; Initialize self-modifying code in interrupt handler
                LDU     #SongStart      ; Current song pointer
                STU     >LdPlayPtr+1
                ; A duration multipler of 0 and a DurationLeft of 1 forces
                ; HS handler to init the rest (frequency values of
                ; first chord, and wavetable offsets)
                CLR     >LdDurMlt+1     
                LDA     #1
                STA     >DurationLeft
                ; Reset SongEntryPtr for next time a song gets entered
                LDD     #SongStart
                STD     >SongEntryPtr
                ; Initialization complete.  Begin background player
                ANDCC   #$EF            ; Enable IRQ
;INFINITE        JMP     INFINITE       ; Uncomment to disallow BASIC from doing stuff while playing
                RTS

; ---------------------------------------------------------------
; Entrypoint from BASIC.  Stop song from playing in the
; background by restoring regular FS, and disabling HS
; ---------------------------------------------------------------
StopSong:
                ORCC    #$50            ; Disable all interrupts
                JSR     SoundOff        ; Turn off DAC at master switch
                BSR     RestoreInterrupts ; Restore normal vsync
                ANDCC   #$EF            ; Enable IRQ
                RTS

; ---------------------------------------------------------------
; InitDac sub
; Talk to the PIA control registers to select the 6-bit DAC
; to output to the TV sound
; ---------------------------------------------------------------
InitDac:
                ; Set PIA1 CA2=0
                LDA     >$FF01          ; PIA1 CRA / CA2 - Select sound out 
                ANDA    #%11110111      ; CA2 outputs a low (0)
                STA     >$FF01                    
                ; Set PIA1 CB2=0
                LDA     >$FF03          ; PIA1 CRB / CB2 - Select sound out 
                ANDA    #%11110111      ; CB2 outputs a low (0)
                STA     >$FF03                    
                ; Now that CA2=0 & CB2=0, selector switches A and B are both
                ; in position 0.  For selector switch A, this means route
                ; 6=bit DAC to TV sound out.  For selector switch B, this selects
                ; right joystick pin 1, but that's an unavoidable byproduct
                ;
                ; We will still get no sound out unless we activate the
                ; master select switch (required no matter which sound
                ; source we're selecting).
                LDA     >$FF23          ; PIA2 CRB / CB2 - master select switch                
                ORA     #%00001000      ; CB2 outputs a high (1)
                STA     >$FF23                    
                ; Configure bits 0 & 1 of PIA2 DRA as inputs, so we can
                ; send a full 8 bits to $FF20 in the interrupt handler
                ; and the lower two bits will automatically be ignored
                LDB     >$FF21          ; Get PIA2 CRA
                ANDB    #%11111011      ; bit 2 = 0 -> access data direction register
                STB     >$FF21
                ; Now, $FF20 is (temporarily) the data direction register
                ; instead of the data register
                LDA     #%11111100      ; lower two bits set to 0 (input)
                STA     >$FF20          ; Save into ddra
                ; Switch $FF20 back to being the data register
                ORB     #%00000100      ; PIA2 CRA: bit 2 = 1 -> access data register
                STB     >$FF21
                RTS

; ---------------------------------------------------------------
; InitInterrupts sub
; Disable FS, enable HS, init irq vector to SoundOut
; ---------------------------------------------------------------
InitInterrupts:
                ; Save original vector for cleanup
                LDA     $10C
                STA     >OrigVectorOp
                LDX     $10D
                STX     >OrigVectorAddr
                ; Change vector to SoundOut routine
                LDA     #$7E            ; JMP opcode
                STA     >$10C
                LDX     #SoundOut
                STX     >$10D
                ; Disable FS IRQ
                LDA     >$FF03
                ANDA    #%11111110
                STA     >$FF03
                ; Enable HS IRQ
                LDA     >$FF01
                ORA     #%00000001      ; Enable IRQ
                ANDA    #%11111101      ; Activate on falling edge
                STA     >$FF01
                LDA     >$FF00          ; Clear flag bit (ack HS IRQ)
                RTS

; ---------------------------------------------------------------
; RestoreInterrupts sub
; Disable HS, enable FS, init irq vector to original value
; before InitInterrupts was called
; ---------------------------------------------------------------
RestoreInterrupts:
                ; Disable HS IRQ
                LDA     >$FF01
                ANDA    #%11111110
                STA     >$FF01
                ; Enable FS IRQ
                LDA     >$FF03
                ORA     #%00000001
                STA     >$FF03
                LDA     >$FF02          ; Clear flag bit (ack FS IRQ)
                ; Restore original vector bytes
                LDA     OrigVectorOp
                STA     >$10C
                LDX     OrigVectorAddr
                STX     >$10D
                RTS

OrigVectorOp    RMB     1
OrigVectorAddr  RMB     2

; ---------------------------------------------------------------
; SoundOff sub
; Turns off sound by just deactivating the master select switch
; The positions of selector switches A & B are left unchanged
; ---------------------------------------------------------------
SoundOff:
                LDA     >$FF23          ; PIA2 CRB / CB2 - master select switch                
                ANDA    #%11110111      ; CB2 outputs a low (0)
                STA     >$FF23                    
                RTS

; ---------------------------------------------------------------
; GetFreqValue sub
; Entry: X -> first character of 3-byte note string
; Exit: D = freq val, or 0 if string not a valid note
; ---------------------------------------------------------------
GetFreqValue:
                LDU     #NoteFreqs      ; U -> note table
NoteInfoLoop:
                LDD     ,U
                CMPD    ,X              ; First two characters of note string match?
                BNE     NextNoteInfo    ; nope
                LDA     2,U             
                CMPA    2,X             ; Last character of note string matches?
                BNE     NextNoteInfo    ; nope
                ; We have a match!
                LDD     NoteInfo.FreqValue,U
                RTS
NextNoteInfo:
                LEAU    sizeof{NoteInfo},U
                TST     ,U              ; Terminating 0?
                BNE     NoteInfoLoop    ; No, loop up
                LDD     #0              ; Yes, no match found
                RTS


; Attributes for each note
NoteInfo        STRUCT
Name            RMB     3               ; e.g., "C#4" It is assumed this field comes first
FreqValue       RMB     2
                ENDSTRUCT

; Table of NoteInfo structs
NoteFreqs:
                INCLUDE NoteFrequencyValues256.asm

; ---------------------------------------------------------------
; AddChord sub
; Entrypoint from BASIC to convert a string like
; "C 2D#4G 5F#1" into a ChordInfo, and then store it into the
; next SongEntry memory location
; Entry: Parameter passed from BASIC containing string
;        representation of chord.  Assumed to be a valid
;        format string with length b/w 3-12 & multiple of 3
; ---------------------------------------------------------------
AddChord:
                JSR     GETARG          ; BASIC string descr now in D
                TFR     D,X             ; X -> string descriptor
                LDU     >SongEntryPtr   ; U = addr to store next chord for song
                ; Init notes of chord to 0
                LDD     #0
                STD     ChordInfo.Voice1,U
                STD     ChordInfo.Voice2,U
                STD     ChordInfo.Voice3,U
                STD     ChordInfo.Voice4,U
                ; Grab info out of string descr parameter
                LDB     ,X              ; B = string length
                LDX     2,X             ; X = string start address
StoreVoice:
                PSHS    B               ; Save B & U, separate pushes to force the order
                PSHS    U
                JSR     GetFreqValue    ; D = freq value
                PULS    U               ; Restore just U from stack
                STD     ,U++            ; Store freq value into current voice for current chord
                PULS    B               ; Restore just B from stack
                LEAX    3,X             ; Advance to next voice string
                SUBB    #3              ; String length decreases by 3
                BNE     StoreVoice      ; Loop for next voice
                ; Chord is complete, so advance song entry ptr
                LDU     >SongEntryPtr
                LEAU    sizeof{ChordInfo},U
                STU     >SongEntryPtr
                STU     >SongEnd        ; This is the new song end as well
                RTS


; A song is a collection of these
ChordInfo       STRUCT
Voice1          RMB     2
Voice2          RMB     2
Voice3          RMB     2
Voice4          RMB     2
                ENDSTRUCT

; Song (sequence of ChordInfo structs) is stored here
SongStart       RMB     sizeof{ChordInfo}*300

; Where AddChord will add the next chord
SongEntryPtr    FDB     SongStart


; Waveform tables, 256-bytes each
                ALIGN   $100                    ; Ensure waveform tables begin on page boundary
                INCLUDE WaveTableSine256.asm
                INCLUDE WgSine.asm
                INCLUDE WaveTableSquare256.asm
                INCLUDE WgSquare.asm
                INCLUDE WgTrapez.asm
                INCLUDE WgOrgan.asm
                INCLUDE WgSawtooth.asm
                INCLUDE WgTriangle.asm
                INCLUDE WgViolin.asm
                INCLUDE WgImpulse.asm

                END