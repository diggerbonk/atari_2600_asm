;
; Basic 2600 ASM Template
;
; AtariMusicEngine
;
;
; Event Format
;
;     Each event consists of a three byte command;
;
;         First Byte
;             bit 7 : channel
;             bits 6-0 : event type
;         Second Byte : Value1
;         Third Byte : Value2 (Usually time to next event)
;
;     Event Types
;         1) Set volume for channel
;         2) Set pitch for channel
;         3) Set distortion for channel

;         5) Jump to another song location (address to go to. NOTE: jump to location 0 to end the song)
;         6) Set Loop Counter
;         7) Decriment loop counter
;         8) Evaluate loop counter
;
; Variables
;     aud0ptrlo
;     aud0ptrhi
;    aud0timer

    PROCESSOR 6502
    include "vcs.h"
    include "macro.h"

    SEG
    ORG $F000 ; set the ROM page


    echo "------", [*], [* - $F000]d, "macro defs"
;
; Macro Definitions
;

    MAC VBLANK_START
    lda #2              ; wait for horizontal sync
    sta  WSYNC          ; ""
    sta  VSYNC          ; turn on vsync
    sta  WSYNC          ; wait three lines
    sta  WSYNC          ; ""
    lda #0              ; ""
    sta  WSYNC          ; ""
    sta  VSYNC          ; turn off vsync
    lda  #47            ; Set the time
    sta  TIM64T         ; ""
    ENDM

    MAC VBLANK_END
waitOnVblank
    lda INTIM
    bne waitOnVblank
    lda #0              ; turn off the vblank
    sta VBLANK          ; ""
    sta WSYNC           ; wait for the next line.
    ENDM

    MAC KERNEL_START
    lda #229
    sta TIM64T
    ENDM

    MAC KERNEL_END
waitOnDraw
    lda INTIM
    bne waitOnDraw
    ENDM

    MAC OVERSCAN_START
    lda #2              ; turn on the vblank
    sta VBLANK          ; ""
    lda  #32            ; wait for 4544 cycles then sync
    sta  TIM64T         ; ""
    ENDM

    MAC OVERSCAN_END
waitOnOverscan          ; wait for overscan to finish
    lda INTIM           ; ""
    bne waitOnOverscan  ; ""
    ENDM



    echo "------", [*], [* - $F000]d, "variables"

;
; Variable declarations
;

aud0ptrlo       equ  $80
aud0ptrhi       equ  $81

meCounter       equ  $82
meLoopCount     equ  $83

tempVar         equ  $85
setFirstSong    equ  $86
frameCounter    equ  $90


    echo "------", [*], [* - $F000]d, "constants"

;
; Constants
;
; event types

ME_VOL0         equ #1
ME_VOL1         equ #129
ME_PITCH0       equ #2
ME_PITCH1       equ #130
ME_TONE0        equ #3
ME_TONE1        equ #131
ME_JUMP         equ #5
ME_SETLOOP      equ #6
ME_DECLOOP      equ #7
ME_EVALLOOP     equ #8



    echo "------", [*], [* - $F000]d, "init"

;
; Initialize system (Andrew Davie's 8 byte init)
;

sysinit
    ldx #0
    txa
sysinit_clear
    dex
    txs
    pha
    bne sysinit_clear

;
; Non-system initialization
;

    lda #0
    sta setFirstSong

    echo "------", [*], [* - $F000]d, "main loop"

;
; main_loop:
;
; This is comprised of the following sections:
;     1) vertical sync/blank
;     2) kernel
;     3) overscan

main_loop

    VBLANK_START
    VBLANK_END
    KERNEL_START
    lda #$0e
    sta COLUBK

    inc frameCounter

    echo "------", [*], [* - $F000]d, "do_audio"

do_audio0

    lda aud0ptrhi       ; 3, 3  ; if the audio pointer is not set we skip
    cmp #0              ; 2, 5  ; ""
    beq do_audio0_leap  ; 2, 7  ; ""

    dec meCounter       ; 5     ; decriment the counter, if it is 0 load the next event
    beq da0_run_next    ;       ; ""
    jmp do_audio0_end   ; 3, 22 ; nothing to do this go around.

da0_run_next            ;       ; load the next event

    ldy #0              ; 2     ; start y at 0
    sty tempVar         ; 3     ; default channel 0
    lda (aud0ptrlo),y   ; 5,    ; this is the event type
    cmp #128            ; 2     ; if > 128, this is for channel 1
    bcc da0_chan0       ; 2
    inc tempVar         ; 5
da0_chan0
    and #%01111111      ;       ; mask away the channelID
    tax                         ; and store the event type in X

    iny                 ; 2     ; preload the first event parameter into A
    lda (aud0ptrlo),y   ; 5     ; ""
    cpx #ME_VOL0        ; 2     ; call the right event handler
    beq doVolume        ; 2     ; ""
    cpx #ME_PITCH0      ; 2     ; ""
    beq doPitch         ; 2     ; ""
    cpx #ME_TONE0       ; 2     ; ""
    beq doDistortion    ; 2     ; ""
    cpx #ME_JUMP        ; 2     ; ""
    beq doJump          ; 2     ; ""
    cpx #ME_SETLOOP     ;       ; ""
    beq doSetLoopCounter; 2     : ""
    cpx #ME_DECLOOP     ;       ; ""
    beq doDecrimentLoop ; 2     ; ""
    cpx #ME_EVALLOOP    ;       ; ""
    beq doLoopEval      ;       ; ""
do_audio0_leap
    jmp do_audio0_end   ; 3     ; unknown event! skip it
doPitch
    ldx tempVar
    sta AUDF0,x         ; 4
    jmp da0_duration    ; 3
doVolume
    ldx tempVar
    sta AUDV0,x         ; 4
    jmp da0_duration    ; 3
doDistortion
    ldx tempVar
    sta AUDC0,x         ; 4
    jmp da0_duration    ; 3
doJump
    bne da0_stop
    tax
    iny                 ; 2
    lda (aud0ptrlo),y   ; 5
    sta aud0ptrhi       ; 3
    stx aud0ptrlo
    jmp da0_run_next    ; 3
doSetLoopCounter
    sta meLoopCount
    jmp da0_increment
doDecrimentLoop
    dec meLoopCount
    jmp da0_increment
doLoopEval
    tax
    lda meLoopCount
    cmp #0
    beq da0_increment
    iny
    lda (aud0ptrlo),y
    sta aud0ptrhi
    stx aud0ptrlo
    jmp da0_run_next

da0_stop
    lda #0
    sta aud0ptrhi
    jmp do_audio0_end

da0_duration
    iny                 ; 2
    lda (aud0ptrlo),y   ; 5
    sta meCounter       ; 3
    tay                 ; 2
    jmp da0_increment1

da0_increment
    ldy #0
da0_increment1
    ; add 3 to the current song pointer
    clc
    lda aud0ptrlo
    adc #3
    sta aud0ptrlo
    lda aud0ptrhi
    adc #0
    sta aud0ptrhi

    cpy #0
    bne do_audio0_end
    jmp da0_run_next

do_audio0_end

setup_song0
    lda setFirstSong
    cmp #0
    bne setup_song0_end
;    lda #<firstSong
    lda #<splinter
    sta aud0ptrlo
;    lda #>firstSong
    lda #>splinter
    sta aud0ptrhi
    lda #1
    sta meCounter
    sta setFirstSong
setup_song0_end

    KERNEL_END
    OVERSCAN_START
    OVERSCAN_END

    jmp main_loop

    echo "------", [*], [* - $F000]d, "end code"

;
; Audio Data:
;     1) eventType/channel
;     2) value
;     3) value2 (usually duration)
;

firstSong ; (eventType, value, duration)

    .byte #ME_TONE0,    #12, #0    ; set the distortion
    .byte #ME_TONE1,    #8, #0
    .byte #ME_PITCH1,   #0, #0
    .byte #ME_SETLOOP,  #10, #0                       ; set the loop counter to 10

firstSong1

    ; hit the hi hat with the first note.
    .byte #ME_VOL0,     #14,    #0
    .byte #ME_PITCH0,   #19,    #0
    .byte #ME_VOL1,     #8,     #1
    .byte #ME_VOL1,     #0,     #9
    .byte #ME_VOL0,     #0,     #1

    .byte #ME_VOL0,     #2,     #0
    .byte #ME_PITCH0,   #19,    #3
    .byte #ME_VOL0,     #0,     #9

    .byte #ME_VOL0,     #14,    #0
    .byte #ME_PITCH0,   #15,    #4
    .byte #ME_VOL0,     #0,     #1

    .byte #ME_VOL0,     #2,     #0
    .byte #ME_PITCH0,   #15,    #4
    .byte #ME_VOL0,     #0,     #9


    .byte #ME_VOL0,     #14,    #0
    .byte #ME_PITCH0,   #12,    #4
    .byte #ME_VOL0,     #0,     #1

    .byte #ME_VOL0,     #2,     #0
    .byte #ME_PITCH0,   #12,    #4
    .byte #ME_VOL0,     #0,     #9

    ; hit the hi hat along with the second note
    .byte #ME_VOL0,     #14,    #0
    .byte #ME_PITCH0,   #19,    #0
    .byte #ME_VOL1,     #8,     #1
    .byte #ME_VOL1,     #0,     #2
    .byte #ME_VOL0,     #0,     #1

    .byte #ME_VOL0,     #2,     #0
    .byte #ME_PITCH0,   #19,    #4
    .byte #ME_VOL0,     #0,     #9

    .byte #ME_VOL0,     #14,    #0
    .byte #ME_PITCH0,   #14,    #4
    .byte #ME_VOL0,     #0,     #1

    .byte #ME_VOL0,     #2,     #0
    .byte #ME_PITCH0,   #14,    #4
    .byte #ME_VOL0,     #0,     #9

    .byte #ME_VOL0,     #14,    #0
    .byte #ME_PITCH0,   #11,    #4
    .byte #ME_VOL0,     #0,     #1

    .byte #ME_VOL0,     #2,     #0
    .byte #ME_PITCH0,   #11,    #4
    .byte #ME_VOL0,     #0,     #9

    .byte #ME_DECLOOP,  #0,     #0                        ; decriment loop counter
    .byte #ME_EVALLOOP, #<firstSong1,   #>firstSong1    ; evaluate loop counter
    .byte #ME_JUMP,     #<firstSong,    #>firstSong      ; jump back to the beginning of the sequence

song120bpm

    .byte #ME_TONE1,    #8,     #0
    .byte #ME_PITCH1,   #0,     #0
    .byte #ME_VOL1,     #8,     #1
    .byte #ME_VOL1,     #0,     #31
    .byte #ME_JUMP,     #<song120bpm, #>song120bpm

splinter2

    .byte #ME_TONE0,    #6,     #0
    .byte #ME_TONE1,    #8,     #0
    .byte #ME_PITCH1,   #0,     #0

splinter2_loop

    .byte #ME_PITCH0,   #30,    #0  ; B1
    .byte #ME_VOL0,     #14,    #12
    .byte #ME_VOL0,     #2,     #12
    .byte #ME_VOL0,     #0,     #8

    .byte #ME_VOL1,     #14,    #7  ; B2
    .byte #ME_VOL1,     #0,     #8
    .byte #ME_PITCH0,   #25,    #0
    .byte #ME_VOL0,     #14,    #12
    .byte #ME_VOL0,     #2,     #4

    .byte #ME_PITCH0,   #25,    #0  ; B3
    .byte #ME_VOL0,     #14,    #12
    .byte #ME_VOL0,     #2,     #4
    .byte #ME_PITCH0,   #25,    #0
    .byte #ME_VOL0,     #14,    #12
    .byte #ME_VOL0,     #2,     #4

    .byte #ME_PITCH0,   #25,    #0  ; B4
    .byte #ME_VOL1,     #14,    #0
    .byte #ME_VOL0,     #14,    #1
    .byte #ME_VOL1,     #0,     #11
    .byte #ME_VOL0,     #2,     #4
    .byte #ME_PITCH0,   #22,    #0
    .byte #ME_VOL0,     #8,     #4
    .byte #ME_VOL0,     #12,    #8
    .byte #ME_VOL0,     #2,     #4

    .byte #ME_PITCH0,   #22,    #0  ; B1
    .byte #ME_VOL0,     #14,    #12
    .byte #ME_VOL0,     #2,     #12
    .byte #ME_VOL0,     #0,     #8

    .byte #ME_VOL1,     #14,    #1  ; B2
    .byte #ME_VOL1,     #0,     #15
    .byte #ME_PITCH0,   #18,    #0
    .byte #ME_VOL0,     #14,    #12
    .byte #ME_VOL0,     #2,     #4

    .byte #ME_PITCH0,   #18,    #0  ; B3
    .byte #ME_VOL0,     #14,    #12
    .byte #ME_VOL0,     #2,     #4
    .byte #ME_PITCH0,   #16,    #0
    .byte #ME_VOL0,     #14,    #12
    .byte #ME_VOL0,     #2,     #4

    .byte #ME_PITCH0,   #16,    #0  ; B4
    .byte #ME_VOL1,     #14,    #0
    .byte #ME_VOL0,     #14,    #1
    .byte #ME_VOL1,     #0,     #11
    .byte #ME_VOL0,     #2,     #12
    .byte #ME_VOL0,     #0,     #8

    .byte #ME_JUMP,     #<splinter2_loop, #>splinter2_loop

splinter

    .byte #ME_TONE0,    #6,     #0
    .byte #ME_TONE1,    #8,     #0
    .byte #ME_PITCH1,   #0,     #0

splinter_loop

    .byte #ME_PITCH0,   #30,    #0  ; B1
    .byte #ME_VOL0,     #14,    #11
    .byte #ME_VOL0,     #2,     #8
    .byte #ME_VOL0,     #0,     #5

    .byte #ME_VOL1,     #14,    #1  ; B2
    .byte #ME_VOL1,     #0,     #11
    .byte #ME_PITCH0,   #25,    #0
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3

    .byte #ME_PITCH0,   #25,    #0  ; B3
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3
    .byte #ME_PITCH0,   #25,    #0
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3

    .byte #ME_PITCH0,   #25,    #0  ; B4
    .byte #ME_VOL1,     #14,    #0
    .byte #ME_VOL0,     #14,    #1
    .byte #ME_VOL1,     #0,     #8
    .byte #ME_VOL0,     #2,     #4  ; i am intentionally being a bit
    .byte #ME_PITCH0,   #22,    #0  ; slippery with the time here. the
    .byte #ME_VOL0,     #12,     #3  ; two 22 notes are slightly delayed
    .byte #ME_VOL0,     #12,    #5
    .byte #ME_VOL0,     #2,     #5

    .byte #ME_PITCH0,   #22,    #0  ; B1
    .byte #ME_VOL0,     #14,    #8
    .byte #ME_VOL0,     #2,     #8
    .byte #ME_VOL0,     #0,     #6

    .byte #ME_VOL1,     #14,    #1  ; B2
    .byte #ME_VOL1,     #0,     #11
    .byte #ME_PITCH0,   #18,    #0
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3

    .byte #ME_PITCH0,   #18,    #0  ; B3
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3
    .byte #ME_PITCH0,   #16,    #0
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3

    .byte #ME_PITCH0,   #16,    #0  ; B4
    .byte #ME_VOL1,     #14,    #0
    .byte #ME_VOL0,     #14,    #1
    .byte #ME_VOL1,     #0,     #8
    .byte #ME_VOL0,     #2,     #7
    .byte #ME_VOL0,     #0,     #8

    .byte #ME_JUMP,     #<splinter_loop, #>splinter_loop


    echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"

;
; The 2600 looks at this memory location to find the program entry point
;

    ORG $FFFA
    .word sysinit          ; NMI
    .word sysinit          ; RESET
    .word sysinit          ; IRQ

END


