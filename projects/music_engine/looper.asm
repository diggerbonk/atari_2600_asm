;
; Basic 2600 ASM Template


    PROCESSOR 6502
    include "vcs.h"
    include "macro.h"

    SEG
    ORG $F000 ; set the ROM page

    echo "------", [$FFFA - *]d, "macro defs"
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


    echo "------", [$FFFA - *]d, "variable definitions"

;
; Variable declarations
;

songPos       equ  $80
songCounter       equ  $81

;meLoopCount     equ  $83

tempVar         equ  $85




frameCounter    equ  $90


    echo "------", [$FFFA - *]d, "constants"



    echo "------", [$FFFA - *]d, "init"

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

    lda #1
    sta songCounter


    echo "------", [$FFFA - *]d, "main loop"

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

; music_engine
;
; process audio loop events. events are in a 3 byte format:
;
;   first byte  :
;     bits 0-6  : event type
;     bit 7     : channel
;   second byte : value
;   third byte  : duration
;
; event types:
;   set volume : set volume for a channel
;   set pitch  : set pitch for a channel
;   set tone   : set the tone for a channel
;   end        : end of loop, go back to start

music_engine:

    dec musicEngineCount    ; 5     ; decriment the counter, if it is 0 load the next event
    beq music_next_event    ; 2-4   ; ""
    jmp music_engine_end    ; 3, 22 ; nothing to do this go around.

music_next_event            ;       ; load the next event

    ldy songPos                     ; set y to the current song position

    lda song_loop_intro,y           ; load the pitch, if it is zero this
    beq music_engine_jump           ; is a jump
    sta AUDF0                       ;

    iny
    lda song_loop_intro,y           ; load the tone
    sta AUDC0

    iny
    lda song_loop_into,y            ; load the volume
    sta AUDV0

    iny
    lda song_loop_intro,y           ; load the duration
    sta musicEngineCount

    iny
    sty songPos
    jmp music_engine_end

music_engine_jump

    iny
    lda song_loop_intro,y
    sta songPos
    jmp music_next_event

music_engine_end

    KERNEL_END
    OVERSCAN_START
    OVERSCAN_END

    jmp main_loop


    echo "------", [$FFFA - *]d, "start data"


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
    .byte #ME_VOL0,     #8,     #3  ; two 22 notes are slightly delayed
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

    .byte #ME_END,     #<splinter_loop, #>splinter_loop


    echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"

;
; The 2600 looks at this memory location to find the program entry point
;

    ORG $FFFA
    .word sysinit          ; NMI
    .word sysinit          ; RESET
    .word sysinit          ; IRQ

END


