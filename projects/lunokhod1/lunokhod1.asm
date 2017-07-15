;
; Lunokhod 1
;
; Screen Layout:
;
;     Lines     Cycles      Description
;     -----------------------------------
;     40        3040        VSYNC/VBLANK
;     10        760         Score
;     32        2432        Sky
;     130       9880        Playfield
;     20        1520        Radar
;     30        2280        Overscan

    PROCESSOR 6502
    include "vcs.h"
    include "macro.h"

    SEG
    ORG $F000 ; set the ROM page

; constantsf

MODE_ATTRACT            equ #0      ; game modes
MODE_TRANSITION         equ #1      ; 1-127
MODE_TRANSITION_TOP     equ #249    ;
MODE_IN_PROGRESS        equ #250    ;
MODE_END_WAVE           equ #251    ; 129-253
MODE_END_WAVE_TOP       equ #252    ;
MODE_GAME_OVER          equ #253    ;
MODE_RESET_DOWN         equ #254
MODE_RESET              equ #255    ;

MISSILE_FOLLOWS_SHIP    equ #0      ;
NEW_MOVE_DEBRIS         equ #0
MAXSPEED                equ #2    ; the max speed of the sled
MINSPEED                equ #-2    ; the min speed of the sled
MINXPOS                 equ #61-20  ; minimum screen positionof the sled (32)
MAXXPOS                 equ #91+20  ; maximum screen position of the sled  (112)
POSDELTA                equ #MAXXPOS-MINXPOS        ; this must be equal to maxpos-minpos (80)
P0CENTER                equ #4      ; offset to center of player 0
MISSILESPEED            equ #7
MISSILESIZE             equ #8
MISSILECOLOR            equ #$ae
;MINXSCREEN              equ #8
;MAXXSCREEN              equ #154
MINXSCREEN              equ #1
MAXXSCREEN              equ #160
MAXXSCREEN              equ #160
GAUGECOLOR              equ #$32
SCORECOLOR              equ #$0e
SCOREBGCOLOR            equ #$00
SKYCOLOR                equ #$00
MTNCOLOR                equ #$04
MTNCAPSCOLOR            equ #$04
GRADIENT1               equ #$04
GRADIENT2               equ #$04
GRADIENT3               equ #$02
RADARCOLOR              equ #$14
RADARCOLOR2             equ #$C8
GROUNDCOLOR             equ #$00
SNOWCOLOR               equ #$06
RAILCOLOR               equ #$02
PLATFORMCOLOR           equ #$04
PLAYFIELDSZ             equ #140
ENEMYHEIGHT             equ #17
SHOT_TIMING             equ #6
SPRITEINIT              equ #PLAYFIELDSZ+ENEMYHEIGHT+1
ME_VOL0                 equ #0      ; music engine volume event
ME_PITCH0               equ #255      ; music engine pitch event
ME_TONE0                equ #1      ; music engine tone event
ME_END                  equ #2      ; music engine end event
SONGTONE                equ #6

; variable assignments.

score           equ  $80    ; 80-81
p0x             equ  $82    ; screen x position of player 0 [20,132]
p0s             equ  $83    ; x speed of player 0 [-4,4]
p0vxlo          equ  $84    ; player 0 virtual x position low byte
temp1lo         equ  $85    ; temporary 16 bit var
temp1hi         equ  $86    ; temporary 16 bit var
temp2lo         equ  $87    ; temporary 16 bit var
temp2hi         equ  $88    ; temporary 16 bit var
randomSeed      equ  $89    ;
direction       equ  $8a    ;
songPos         equ  $8b    ; points to the current song position
UNUSED           equ  $8c    ;
waveCounter     equ  $8d    ; number of debris created in the current wave
nexty           equ  $8e    ;
m0vxlo          equ  $8f    ; virtual x position of missile 0
m0ys            equ  $90    ; y position of missile 0
m1vxlo          equ  $91    ; virtual x position of missile 1
m1ys            equ  $92    ; y position of missile 1
m2vxlo          equ  $93    ; virtual x position of missile 2
m2ys            equ  $94    ; y position of missile 2
m3vxlo          equ  $95    ; virtual x position of missile 3
m3ys            equ  $96    ; y position of missile 3
debrisvx        equ  $97    ; 97-9E virtual x position of debris
debrisinfo      equ  $9f    ; 9F-A6 ; info about debris aaxxxyyy
                            ; (aa = movespeed xxx = debris index,
                            ; yyy = condemned)
debrisx         equ  $a7    ; A7-AE screen x position of debris
debrisy         equ  $af    ; AF-B6 screen y position of debris
frameCount      equ  $b7    ; count the current frame, rolls over at 255
debrisCount     equ  $b8    ; number of current debris
scrollpos       equ  $b9    ; scroll position on the virtual playfield
p1dataptrlo     equ  $ba    ;
p1dataptrhi     equ  $bb    ;
wave            equ  $bc    ; 0-31, incremented by one. The first five
                            ; bits of the wave are used like this:
                            ;    bit 0 & 1    : horizontal speed
                            ;    bit 2 & 3    : vertical speed
                            ;    bit 5        : pace
p1colorlo       equ  $bd    ;
p1colorhi       equ  $be    ;
spritecount     equ  $bf    ;
spriteramidx    equ  $c0    ; C0-C7 pointers into spriteram

spriteram       equ  $c8    ; C8-D7 16 bytes of ram organized in 2 byte
                            ; chunks, each chunk containing display
                            ; data for a given debris sprite. this is
                            ; loaded with the current animation frames

effectState    equ  $D8     ; holds state information for current
                            ; audio/video effect: XXXYYYYY, where XXX
                            ; is the event type and YYYYY is the event
                            ; duration countdown.
shotcount       equ  $D9    ;
gameMode        equ  $DA    ; see MODES_* below for possible values
radlevel        equ  $DB    ;
musicEngineCount equ  $DC   ;
vxoffsetlo      equ  $DD    ;
radarRam        equ  $DE    ; DE-FD 16 bytes to store radar display data
temp3           equ  $FE

; sysinit
;
; Initialize system (Andrew Davie's 8 byte init)

    echo "------", [*], [* - $F000]d, "sysinit"

sysinit:
    ldx #0
    txa
sysinit_clear
    dex

    txs
    pha
    bne sysinit_clear
    jsr non_system_init
    jmp main_loop
sysinit_end

; non_system_init
;
; Non-system initialization

    echo "------", [*], [* - $F000]d, "non_system_init"

non_system_init:

    lda #0                          ; zero out memory locations $80,$FD
    ldx #$7c                        ;
clear_loop                          ;
    sta #$80,x                      ;
    dex                             ;
    cpx #2                          ;
    bne clear_loop                  ;

    lda #$FF                        ; set initial missile y positions
    sta m0ys                        ; $FF means hidden
    sta m1ys                        ;
    sta m2ys                        ;
    sta m3ys                        ;

    lda #<sprite1Frame1             ; initialize the player sprite
    sta p1dataptrlo                 ;
    lda #>sprite1Frame1             ;
    sta p1dataptrhi                 ;

    lda #$4d                        ; initialize initial player screen
    sta p0x                         ; and virtual positions
    lda #77                         ;
    sta p0vxlo                      ;

    lda #$FF
    sta songPos

    lda #MODE_ATTRACT
    sta gameMode

    rts
non_system_init_end

; main_loop:

    echo "------", [*], [* - $F000]d, "main_loop"

main_loop:

;
; Start the vertical blank interrupt
; TIME: ~4 scanlines
; ENDS: 8
;

    echo "------", [*], [* - $F000]d, "vblank_start"

vblank_start:
    lda #2                          ; wait for horizontal sync
    sta  WSYNC                      ; ""
    sta  VSYNC                      ; turn on vsync
    sta  WSYNC                      ; wait three lines
    sta  WSYNC                      ; ""
    lda #0                          ; ""
    sta  WSYNC                      ; ""
    sta  VSYNC                      ; turn off vsync
    lda  #45                        ; Set the timer for 2812 cycles
    sta  TIM64T                     ; ""
vblank_start_end

; process_gameMode
;
; detect game mode changes, take appropriate action

    echo "------", [*], [* - $F000]d, "process_gameMode"

process_gameMode:

    lda gameMode                    ; load gameMode, choose appropriate
    cmp #MODE_RESET                 ; action
    beq do_reset                    ;
    cmp #MODE_GAME_OVER             ;
    beq do_game_over                ;
    cmp #MODE_END_WAVE              ;
    beq do_end_wave                 ;
    cmp #MODE_IN_PROGRESS           ;
    beq process_gameMode_end        ;
    cmp #MODE_TRANSITION            ;
    beq do_start_wave               ;
    jmp process_gameMode_end        ;

do_reset

    jsr non_system_init
    lda #MODE_TRANSITION
    sta gameMode
    lda #1
    sta musicEngineCount
    lda #0
    sta score
    sta score+1
    sta songPos
    jmp process_gameMode_end

do_game_over

    jsr non_system_init
    lda #MODE_ATTRACT
    sta gameMode
    lda #$FF
    sta songPos
    lda #1
    sta musicEngineCount
    jmp process_gameMode_end

do_end_wave

   lda #MODE_TRANSITION
   sta gameMode
   jmp process_gameMode_end

do_start_wave

   lda #MODE_IN_PROGRESS
   sta gameMode
   jmp process_gameMode_end

process_gameMode_end

    inc frameCount      ; 5, 13

; do_audio
;
; perform audio processing

    echo "------", [*], [* - $F000]d, "do_audio"

do_audio:                   ; 35

    lda shotcount           ; 3     ; if there are no active shots skip
    beq skip_shot_audio     ; 2/3   ;

    dec shotcount           ; 5     ; decriment the shot counter.

    lda effectState                 ; skip shot audio if there are other
    cmp #0                          ; noises to be made
    bne skip_shot_audio             ;

    lda shotcount                   ; adjust the shot sound parameters
    adc #1                          ;
    sta AUDV1                       ;
    lda #15                 ; 2     ;
    sec                     ; 2     ;
    sbc shotcount           ; 2     ;
    clc                     ; 2     ;
    adc #24                 ; 2     ; tweak to alter timbre of shot
    sta AUDF1               ; 3     ;
    lda #8                  ; 2     ; tweak to alter shot distortion
    sta AUDC1               ; 3     ;
    jmp do_audio_end

skip_shot_audio

    lda effectState                 ; if the effect counter is > 0 set
    and #%00011111                  ; the volume for this effect, skip
    cmp #0                          ; otherwise
    bne skipAudioEnd                ;
    sta AUDV1                       ;
    sta effectState                 ;
    jmp do_audio_end

skipAudioEnd

    lsr                             ; set the volume and decriment the
    sta AUDV1                       ; counter

    dec effectState

    lda effectState                 ; jump to the correct effect handler
    cmp #%10000000                  ;
    bcs radioactive_effect          ;
    cmp #%01000000                  ;
    bcs absorber_effect             ;
    cmp #%00100000                  ;
    bcs bomb_effect                 ;
    lda #0                          ; no audio events, skip
    sta AUDV1                       ;
    jmp do_audio_end                ;

bomb_effect

    lda frameCount
    and #%00100110
    clc
    adc #%00110100
    sta AUDF1
    lda #15
    jmp noRandomAudc

absorber_effect

    lda frameCount
    and #%00100110
    clc
    adc #%11000000
    sta AUDF1
    lda frameCount
    jmp noRandomAudc

radioactive_effect

    lda frameCount
    and #%00100110
    clc
    adc #%10000100
    sta AUDF1
    lda #8

noRandomAudc
    sta AUDC1

do_audio_end

music_engine:

    ldy songPos
    cmp #$FF
    beq music_engine_end

    dec musicEngineCount    ; 5     ; decriment the counter, if it is 0 load the next event
    beq music_next_event    ; 2-4   ; ""
    jmp music_engine_end    ; 3, 22 ; nothing to do this go around.

music_next_event            ;       ; load the next event

    lda songLoopIntro,y             ; load the pitch, if it is 255 this
    cmp #255                        ; is a jump
    beq music_engine_jump           ;
    sta AUDF0                       ;

    iny
    lda songLoopIntro,y           ; load the tone
    sta AUDC0

    iny
    lda songLoopIntro,y            ; load the volume
    sta AUDV0

    iny
    lda songLoopIntro,y           ; load the duration


;; MULTIPLY
    ; adjust for tempo. multiply by delta then divide by 8.

    sta musicEngineCount
    cmp #1
    beq endMusicEngineDivide;

    ldx wave
    lda tempoTable,x
    sta temp3
    lda #0
    beq enterMultiplyLoop
doAdd
    clc
    adc temp3
theLoop
    asl temp3
enterMultiplyLoop
    lsr musicEngineCount
    bcs doAdd
    bne theLoop
end
    lsr
    lsr
    lsr
    sta musicEngineCount

endMusicEngineDivide
;
    iny
    sty songPos
    jmp music_engine_end

music_engine_jump

    iny
    lda songLoopIntro,y
    tay
    jmp music_next_event

music_engine_end


; adjust_missiles
;
; adjust missile y positions.

    echo "------", [*], [* - $F000]d, "adjust_missiles"

adjust_missiles:

    lda m0ys
    cmp #$FF
    beq adjust_1
    cmp #PLAYFIELDSZ
    bcs reset_missile_0
    clc
    adc #MISSILESPEED
    sta m0ys
    jmp adjust_1
reset_missile_0
    lda #$FF
    sta m0ys
adjust_1
    lda m1ys
    cmp #$FF
    beq adjust_2
    cmp #PLAYFIELDSZ
    bcs reset_missile_1
    clc
    adc #MISSILESPEED
    sta m1ys
    jmp adjust_2
reset_missile_1
    lda #$FF
    sta m1ys
adjust_2
    lda m2ys
    cmp #$FF
    beq adjust_3
    cmp #PLAYFIELDSZ
    bcs reset_missile_2
    clc
    adc #MISSILESPEED
    sta m2ys
    jmp adjust_3
reset_missile_2
    lda #$FF
    sta m2ys
adjust_3
    lda m3ys
    cmp #$FF
    beq adjust_missiles_end
    cmp #PLAYFIELDSZ
    bcs reset_missile_3
    clc
    adc #MISSILESPEED
    sta m3ys
    jmp adjust_missiles_end
reset_missile_3
    lda #$FF
    sta m3ys
adjust_missiles_end

    sta HMCLR           ; clear the ball init

; move_missiles
;
; move move missile sprites. there can be up to four visible missiles,
; two 'odd' and two 'even'. They are moved in alternating passes every
; other frame.

    echo "------", [*], [* - $F000]d, "move_missiles"

move_missiles:

    lda frameCount
    and #%0000001
;    cmp #0
    beq move_missiles_even
    jmp move_missiles_odd

move_missiles_even
    lda m0ys
    cmp #$FF
    beq move_missiles_even_1
    lda m0vxlo
;    sta temp2lo
    jsr convert_virtual_xpos
    clc
    adc #P0CENTER       ; add to align with center of ship
#if MISSILE_FOLLOWS_SHIP = 1
    lda p0x             ; missle follows ship
    clc                 ; ""
    adc #4              ; ""
#endif
    ldx #3
    jsr do_sprite_move
move_missiles_even_1
    lda m1ys
    cmp #$FF
    beq move_missiles_even_end
    lda m1vxlo
;    sta temp2lo
    jsr convert_virtual_xpos
    clc
    adc #P0CENTER       ; add to align with center of ship
#if MISSILE_FOLLOWS_SHIP = 1
    lda p0x             ; missle follows ship
    clc                 ; ""
    adc #4              ; ""
#endif
    ldx #2
    sta HMCLR
    jsr do_sprite_move
move_missiles_even_end
    jmp move_missiles_odd_end

move_missiles_odd
    lda m2ys
    cmp #$FF
    beq move_missiles_odd_3
    lda m2vxlo
;    sta temp2lo
    jsr convert_virtual_xpos
    clc
    adc #P0CENTER       ; add to align with center of ship
#if MISSILE_FOLLOWS_SHIP = 1
    lda p0x             ; missle follows ship
    clc                 ; ""
    adc #4              ; ""
#endif
    ldx #3
    jsr do_sprite_move
move_missiles_odd_3
    lda m3ys
    cmp #$FF
    beq move_missiles_odd_end
    lda m3vxlo
;    sta temp2lo
    jsr convert_virtual_xpos
    clc
    adc #P0CENTER       ; add to align with center of ship
#if MISSILE_FOLLOWS_SHIP = 1
    lda p0x             ; missle follows ship
    clc                 ; ""
    adc #4              ; ""
#endif
    ldx #2
    sta HMCLR
    jsr do_sprite_move
move_missiles_odd_end

move_missiles_end

; swap_missiles1
;
; We are keeping state for 4 missiles. Every other frame we
; map even or odd missiles to M0/M1.

    echo "------", [*], [* - $F000]d, "swap_missiles1"

swap_missiles1:             ; 32

    lda frameCount          ; 3
    and #%0000001           ; 2
    beq swap_missiles1_end  ; 3
    ldx m0ys                ; 3
    lda m2ys                ; 3
    sta m0ys                ; 3
    stx m2ys                ; 3
    ldx m1ys                ; 3
    lda m3ys                ; 3
    sta m1ys                ; 3
    stx m3ys                ; 3

swap_missiles1_end

    nop                 ; waste time before the HMCLR
    sta HMCLR


; remove_stale_debris
;
; Check the bottom debris moved and see if it needs to be removed,
; perform appropriate actions if we remove an absorber or radioactive
; elementl

    echo "------", [*], [* - $F000]d, "remove_stale_debris"

remove_stale_debris

    ldx debrisCount                 ; load the debris count into x
    cpx #0                          ; compare x to 0
    beq remove_stale_debris_end     ; if it = 0 then skip remove debris

    dex                             ; decriment x so that it equals
                                    ; the lowest debris index

    lda debrisy,x                   ; load the debris y position into a
    cmp #4                          ; compare a to 4
    bcs remove_stale_debris_end     ; if a >= 4 then skip remove debris
    dec debrisCount                 ; else decriment debrisCount


    lda debrisinfo,x                ; if the debris is radioactive and
    and #%00011111                  ; it is not condemned (011, 111) we
    cmp #%00011000                  ; trigger an effect and update the
    bne check_for_absorber          ; radLevel (else skip to absorber
                                    ; check

    lda #%10010111                  ; play radioactiveaudio
    sta effectState                 ;

    lda radlevel                    ; update radlevel
    lsr                             ;
    clc                             ;
    adc #128                        ;
    sta radlevel                    ;

    cmp #$FF                        ; if radLevel has reached maximum
    bne remove_stale_debris_end     ; then the game is over, set
    lda #MODE_GAME_OVER             ; gameMode to MODE_GAME_OVER and the
    sta gameMode                    ; debrisCount to 0
    lda #0                          ;
    sta debrisCount                 ;

    jmp remove_stale_debris_end

check_for_absorber

    lda debrisinfo,x               ; check if this is an uncondemned
    and #%00111111                 ;  absorber (000) if not, skip
    bne remove_stale_debris_end    ;

    lda effectState
    cmp #%10000000
    bcs skip_absorber
    lda #%01011000
    sta effectState               ; currently handling a radioactive
    lda #%01000000                 ; event.
skip_absorber

    lda radlevel                   ; adjust the radlevel
    asl                            ;
    sta radlevel                   ;

remove_stale_debris_end

; move_debris
;
; Move the debris according to the current speed and moveTable entries.

    echo "------", [*], [* - $F000]d, "move_debris"

move_debris:

    lda debrisCount                 ; skip debris move if there is none
    cmp #0                          ;
    bne no_move_debris_end          ;
    jmp move_debris_end             ;

no_move_debris_end

    lda #0
    sta spritecount
    ldx #0                          ; set loop index to 0

move_debris_loop


; begin_horizontal_move
;
; move debris left or right. Degree of movement is detrermined by
; settings in the moveTable and by the current wave.

begin_horizontal_move

#if NEW_MOVE_DEBRIS = 1
    txa
    asl
    asl
    adc frameCount
    lsr
    lsr
    and #%00000111

    tay

    lda debrisvx,x
    clc
    adc sineTable,y
    sta debrisvx,x
    jmp end_horizontal_move

move_debris_left

    lda moveTable,y
    and #%00000011
    adc debrisvx,x
    sta debrisvx,x

    jmp end_horizontal_move

move_debris_right

    ; load the move adder from moveTable, we will move the debris
    ; right by that amount
    lda moveTable,y
    and #%00000011
    eor #%11111111
    adc #%00000001
    adc debrisvx,x
    sta debrisvx,x
    jmp end_horizontal_move

#endif
end_horizontal_move

; begin_vertical_move
;


begin_vertial_move

    lda frameCount
    and #%00000000
    bne end_vertical_move

    lda debrisy,x       ; "" ; load current debrix x position
    sec                 ; "" ; set the carry flag (needed?)
    sbc #%00000001      ;    ; subtract 1
    sta debrisy,x       ; "" ; update current debris x position

end_vertical_move


    lda debrisinfo,x                ; check to see if debris is fully
    and #%00000111                  ; condemned, if not we go ahead with
    cmp #%00000111                  ; virtual position conversion
    bne continue_with_convert       ;

    lda #$FF                        ; debris is condemned, skip further
    sta debrisx,x                   ; debris processing
    jmp move_debris_skip            ;

continue_with_convert               ; debris not fully condemned,
    lda debrisvx,x                  ; convert virtual x position of the
    jsr convert_virtual_xpos        ; debris to a screen position
    sta debrisx,x                   ;
set_to_ff
    cmp #$FF                        ; if convert_virtual_xpos returns
    beq move_debris_skip            ; FF, the sprite is off screen, skip

    stx temp3                       ; store x, our debris index, so we
                                    ; can use x to look up details
                                    ; in the sprite table


    lda debrisinfo,x                ; increment the condemned counter
    and #%00000111                  ; if the counter is non-zero.
    beq spriteselect                ;
    sta temp1lo                     ;
    inc temp1lo                     ;
    lda debrisinfo,x                ;
    and #%11111000                  ;
    ora temp1lo                     ;
    sta debrisinfo,x                ;

    lda #<explosionFrameTable       ; Load the memory location of the
    sta temp2lo                     ; explosion data for use in
    lda #>explosionFrameTable       ; 'spritemove'
    sta temp2hi                     ;
    jmp spritemove            ;

spriteselect

    lda debrisinfo,x                ; load the correct sprite. we are
    and #%00111000                  ; looking up the memory location for
    lsr                             ; this sprites data in spriteTable
    lsr                             ; and storing it in temp2lo/temp2hi
    tax                             ;
    lda spriteTable,x               ;
    sta temp2lo                     ;
    lda spriteTable+1,x             ;
    sta temp2hi                     ;

spritemove

    ldx spritecount
    lda frameCount      ; 3         ; use framecount to choose which
    and #%00001100      ; 2         ; sprite animation frame to show.
    lsr                 ; 2         ; we use the memory location grabbed
    tay                             ; spritetable in "spriteselect"
    lda (temp2lo),y                 ; above to look up the sprite frame
    sta spriteram,x                 ; data.
    iny                             ;
    lda (temp2lo),y                 ;
    sta spriteram+8,x                     ;

    inc spritecount                 ; tee up the next sprite

    ldx temp3                       ; reload x

move_debris_skip

    inx                             ; increment x and tee up the next
    cpx debrisCount                 ; debris index, loop if more debris
    beq move_debris_end             ; left to move.
    jmp move_debris_loop

move_debris_end

; load_score
;
; load the score pointers to be used later in the kernel

    echo "------", [*], [* - $F000]d, "load_score"

load_score:

    lda score           ; load half the player score
    and #$0F            ; and out the low nibble
    tax
    lda scoreTable,x    ;
    sta spriteramidx    ;
    lda scoreTable+10,x ;
    sta spriteramidx+1  ;

    lda score           ; load half the player score
    lsr                 ; extract the high nibble
    lsr
    lsr
    lsr
    tax
    lda scoreTable,x    ;
    sta spriteramidx+2      ;
    lda scoreTable+10,x  ;
    sta spriteramidx+3    ;

    lda score+1           ; load half the player score
    and #$0F            ; and out the low nibble
    tax
    lda scoreTable,x    ;
    sta spriteramidx+4      ;
    lda scoreTable+10,x  ;
    sta spriteramidx+5    ;

    lda score+1           ; load half the player score
    lsr                 ; extract the high nibble
    lsr
    lsr
    lsr
    tax
    lda scoreTable,x    ;
    sta spriteramidx+6      ;
    lda scoreTable+10,x  ;
    sta spriteramidx+7    ;


    echo "------", [*], [* - $F000]d, "map_player"
;
; Map the player onto the radar
;

map_player

    ldy #$FF
    lda p0vxlo          ; calculate the x coord for the map
    sec                 ; "" divide by 65
DivideBy64              ; +1
    iny                 ; 2
    sbc #64             ; 2
    bcs DivideBy64      ; 2 repeat up to 3 times, result will be in [0,3]

    lda p0vxlo          ; divide by 8 and lookup bit offset
    lsr
    lsr
    lsr
    tax
    lda map_debris_table,x
    sta radarRam,y
map_player_end

; init_first_sprite
;
; Initialize the first sprite position, this triggers finding
; the next sprite in the kernel.
; TIME: 5 cycles

    echo "------", [*], [* - $F000]d, "init_first_sprite"

init_first_sprite:

    lda #SPRITEINIT     ; 2
    sta nexty           ; 3

init_first_sprite_end

; init_ball
;
; set up the ball, we use it to hide HMOVE bars

    echo "------", [*], [* - $F000]d, "init_ball"

init_ball:              ; WSYNC + 19

    sta WSYNC           ; 3, 0
    STA RESBL           ; 3, 3  ; Reset the ball
    LDA #$22            ; 2, 5  ; Set the horizontal ball motion (+2)
    STA HMBL            ; 3, 8  ;
    STA ENABL           ; 3, 11 ;Enable the ball (02)
    lda #%00110100      ; 2, 13
    sta CTRLPF          ; 3, 16

init_ball_end

;
; End the vertical blank
;

    echo "------", [*], [* - $F000]d, "vblank_end"

vblank_end:

waitOnVblank

    lda INTIM           ; wait for timeout to expire
    bne waitOnVblank

    sta WSYNC           ; wait for the next line.
    sta HMOVE           ; this HMOVE is paired with the init_ball routine above
    sta VBLANK          ; ""

vblank_end_end

; draw_score
;
; Draw the score

    echo "------", [*], [* - $F000]d, "main loop - draw score"

draw_score:

    lda #SCOREBGCOLOR   ; 2, 4
    sta COLUBK          ; 3, 7
    lda #SCORECOLOR
;    ldy gameMode
;    cpy #MODE_IN_PROGRESS
;    bne skipScoreBlink
;    lda frameCount
;    and #%00001111
;    ora #%00000011
skipScoreBlink
    sta WSYNC           ; 3, -
    sta COLUP0          ; 3, 3
    sta COLUP1          ; 3, 6
    lda #1              ; 2, 8
    sta HMCLR           ; 3, 11     ; clear the motion register
    sta NUSIZ0          ; 3, 14     ; two copies of each player
    sta NUSIZ1          ; 3, 17     ;
    nop                 ; 2, 19
    SLEEP 20            ; 20, 39    ; waste cycles here.
    sta RESP0           ; 3, 42     ; start drawing
    sta RESP1           ; 3, 45
    lda #%11110000      ; 2, 47
    sta HMP0            ; 3, 50
    lda #%00000000      ; 2, 52
    sta HMP1            ; 3, 55
    sty nexty           ; 3, 58
    ldy #7              ; 2, 60
    sta WSYNC           ; 3, 3
    sta HMOVE           ; 3, 3
    nop                 ; 2, 5
score_loop              ; +1, 5
    dey                 ; 2, 7
    lda (spriteramidx+6),y  ; 6, 13
    sta GRP0            ; 3, 16
    lda (spriteramidx+4),y ; 6, 22
    sta GRP1            ; 3, 25
    lda (spriteramidx),y ; 6, 31
    tax                 ; 2, 33
    lda (spriteramidx+2),y ; 6, 39
    sleep 8             ; 8, 47
    sta GRP0            ; 3, 50
    stx GRP1            ; 3, 53
    sta WSYNC           ; 3, 66
    cpy #0              ; 2, 2
    bne score_loop      ; 2, 4
    lda #0              ; 2, 6
    sta GRP1            ; 3, 9
    sta GRP0            ; 3, 12
    lda  #%00010000     ; 2, 14

    lda #MISSILECOLOR   ; 2, 16
    sta COLUP0          ; 2, 18
    sta COLUP1          ; 2, 20

; init_player_position
;
; Initialize the player horizontal position, for use later in the
; kernel.

    echo "------", [*], [* - $F000]d, "init_player_position"

init_player_position

    lda p0x             ; 2, 22
    ldx #0              ; 2, 24
    jsr do_sprite_move  ; -,

init_player_position_end

; init_playfield
;
; Between drawing the score but before the playfield, initialize our
; sprite list: load the sprite ram index  and then preload the first
; debris sprite. This ends up taking a variable amount of time
; so we set a timer here and do the things we need then wait for
; it to expire.

    echo "------", [*], [* - $F000]d, "main loop kernel - init playfield"

init_playfied:

    sta WSYNC
    sta HMCLR                   ; so HMOVES do not effect sled position
    lda #7              ; 2, 22 ;
    sta TIM64T          ; 3, 25 ;

; load_visible_sprites
;
; walk through full list of debris, add any that are on-screen to our
; spriteram index for use in draw_playfield
;
; TIME: 184 (max)

load_visible_sprites

    ldx #$FF            ; 2, 7      ;
    ldy #0              ; 2, 9      ; for spritecount
    jmp lvs_skip        ; 3
lvs_loop                            ; max loop time (when debriscount = 8) is 175 cycles
    lda debrisx,x       ; 4         ; if the x position is FF (off screen), then skip
    cmp #$FF            ; 2
    beq lvs_skip        ; 2/3
    stx spriteramidx,y  ; 4
    iny                 ; 2
lvs_skip                ; + 1
    inx                 ; 2
    cpx debrisCount     ; 3
    bne lvs_loop        ; 3         ; LOOP TIME = 22 * 8 - 1 = 175
    sty spritecount     ; 3,  184   ; set spritecount

load_visible_sprites_end

; preload pointers for the first debris sprite to be used later
; in the kernel.

preload_sprite

    lda #$EF            ; 2, 2  ; Yes, this is $EF
    sta nexty           ; 3, 5
    lda spritecount     ; 3, 8
    beq preload_sprite_end ; 2, 10

    ldx #0              ; 2, 12
    lda spriteram,x     ; 4, 16
    sta p1dataptrlo     ; 3, 19
    lda spriteram+8,x   ; 4, 23
    sta p1dataptrhi     ; 3, 26

    ldy #17             ; 2, 28 ;
    lda (p1dataptrlo),y ; 6, 34 ;
    sta p1colorlo       ; 3, 37 ;
    iny                 ; 2, 39 ;
    lda (p1dataptrlo),y ; 6, 45 ;
    sta p1colorhi       ; 3, 48 ;

    ldy #0              ; 2, 50
    ldx spriteramidx,y  ; 4, 54
    lda debrisy,x       ; 4, 58
    sta nexty           ; 3, 61
    lda debrisx,x       ; 4, 65
    ldx #1              ; 2, 67
    jsr do_sprite_move  ; ~91, 158

preload_sprite_end

    echo "------", [*], [* - $F000]d, "sky_wait"

init_playfield_wait
    lda INTIM           ; 3, --
    bne init_playfield_wait        ; 2, --


    lda  #%00010000     ; 2, 14
    sta  NUSIZ0         ; 3, 17
    sta  NUSIZ1         ; 3, 20

    sta WSYNC

init_playfield_end

; draw_top_line
;
; We draw the top line, and have a few additional cycles to set the
; playfield background color (and related visual effects). This MUST
; fit in a single scanline.
; MAX CYCLES: 46

    echo "------", [*], [* - $F000]d, "draw_top_line"

draw_top_line

    lda #$0c               ; 2, 2   ; line color between score and sky
    sta COLUBK             ; 3, 5   ;

    lda effectState        ; 3, 8
    cmp #%10000000         ; 2, 10   ; corresponding action
    bcs radioactiveEffect  ; 2, 12  ;
    cmp #%01000000         ; 2, 14  ;
    bcs absorberEffect     ; 2, 16  ;
    jmp resetBackground    ; 3, 19  ;

radioactiveEffect          ; 1, 13
    lda #$20               ; 2, 15
    jmp doneSetEffectColor ; 3, 18

absorberEffect             ; 1, 17
    lda #$80               ; 2, 19

doneSetEffectColor         ; 0, 18
    sta temp2hi            ; 3, 22
    lda effectState        ; 3, 25  ; if effectState is 0 no jump to
    beq resetBackground    ; 2, 27  ; resetBackground (done with effect)
    lsr                    ; 2, 29
    lsr                    ; 2, 31
    cmp #0                 ; 2, 33  ; if a is now zero go to set zero
    beq resetBackground    ; 2, 35
    clc                    ; 2, 37
    adc temp2hi            ; 3, 40
    jmp setBackground      ; 3, 43

resetBackground            ; +0, 19
    lda #SKYCOLOR          ; 2, 21

setBackground              ;
    sta WSYNC              ; 3, 23/46
    sta COLUBK             ; 3, 3

end_draw_top_line

; DO NOT DELETE THIS COMMENT SECTION!!!!!!!!
;    jmp draw_playfield
;AdjustCodeOrg1
;    .byte #0
;    .byte #0

    echo "------", [*], [* - $F000]d, "draw_playfield"

draw_playfield
    lda #1                  ; 2, 5     ;
    sta temp3               ; 3, 8     ;
    ldx #PLAYFIELDSZ        ; 2, 10    ;
do_playfield                ;+1, 5 21

    sec                     ; 2, 12 23 ; ---------------------------------------------
    txa                     ; 2, 14 25 ;
    sbc m1ys                ; 3, 17 28 ; draw missile 0, max 16 cycles
    adc #MISSILESIZE        ; 2, 19 30 ;
    lda #2                  ; 2, 21 32 ;
    adc #$ff                ; 2, 23 34 ;
    sta ENAM0               ; 3, 26 37 ; ---------------------------------------------
    sec                     ; 2, 28 39
    txa                     ; 2, 30 41 ; ---------------------------------------------
    sbc m0ys                ; 3, 33 44 ; draw missile 1, max 16 cycles
    adc #MISSILESIZE        ; 2, 35 46 ;
    lda #2                  ; 2, 37 48 ;
    adc #$ff                ; 2, 39 50 ;
    sta ENAM1               ; 3, 42 53 ; ---------------------------------------------
    txa                     ; 2, 44 55 ; ---------------------------------------------
    sec                     ; 2, 46 57 ; set up player1 - max  16 cycles
    sbc nexty               ; 3, 49 60 ;
    adc #ENEMYHEIGHT        ; 2, 51 62 ;
    bcc finishedSprite      ; 2, 53 64 ;
    tay                     ; 2, 55 66 ;
    sta WSYNC               ; 3, 58 69 ; 7 free cycles
    lda (p1dataptrlo),y     ; 5, 5     ; ---------------------------------------------
    sta GRP1                ; 3, 8     ; write player graphics & color - max 17 cyles
    lda (p1colorlo),y       ; 5, 13    ;
    sta COLUP1              ; 3, 16    ; ---------------------------------------------
    dex                     ; 2, 18    ;
    bne do_playfield        ; 2, 20    ;
    jmp draw_playfield_end  ; 3, 23    ;
finishedSprite              ;+1, 54 65 ;
    cmp #$FF                ; 2, 56 67 ; we just finished the last line of a sprite
    beq load_next_sprite    ; 2, 58 69 ; need to load the next one
    sta WSYNC               ; 3, 61 72 ; 4 FREE CYCLES
    dex                     ; 2, 2     ;
    bne do_playfield        ; 2, 4     ;
    jmp draw_playfield_end  ; 3, 7     ;
load_next_sprite            ;+1, 59 70 ;
    sta WSYNC               ; 3, 62 73 ; 1 FREE CYCLE

    lda temp3               ; 3, 3     ;
    cmp spritecount         ; 3, 6     ;
    bcs no_more_sprites     ; 2, 8     ;

    stx temp1lo             ; 3, 11    ; load sprite data
    ldx temp3               ; 2, 13

    lda spriteram,x         ; 4, 17    ; ""
    sta p1dataptrlo         ; 3, 20    ; ""
    lda spriteram+8,x       ; 4, 24    ; ""
    sta p1dataptrhi         ; 3, 27    ; ""

    ldy #17                 ; 2, 29    ;
    lda (p1dataptrlo),y     ; 6*, 35   ;
    sta p1colorlo           ; 3, 38    ;
    iny                     ; 2, 40    ;
    lda (p1dataptrlo),y     ; 6*, 46   ;
    sta p1colorhi           ; 3, 49    ;

    ldy temp3               ; 3, 52    ; get the next sprite from spriteramidx
    ldx spriteramidx,y      ; 4, 56    ; ""
    inc temp3               ; 3, 59    ; ""
    lda debrisy,x           ; 4, 63    ; ""
    sta nexty               ; 3, 66    ; ""

    lda debrisx,x           ; 4, 70    ; setup A and X for do_sprite_move

    sta WSYNC               ; 3, 73    ; -----------------------------------
    sec                     ; 2, 2     ; inline skipdraw
DivideLoop1                 ;+1  7  52 ; This loop MAX 54 cycles if A is < 160, MIN 4
    sbc #15                 ; 2, 4  54 ;
    bcs DivideLoop1         ; 2, 6  56 ;
    tay                     ; 2, 8  58 ;
    lda FineAdjustTableEnd,Y; 5, 13 63 ;
    ldx #1                  ; 2, 15 65 ;
    sta HMP0,X              ; 4, 19 69 ;
    sta RESP0,X             ; 4, 23 73 ;
    sta WSYNC               ; 3, 26 76 ;
    sta HMOVE               ; 3, 3     ;

    ldx temp1lo             ; 3, 6     ;
    dex                     ; 2, 8     ;
    dex                     ; 2, 10    ;
    dex                     ; 2, 12    ;
    jmp do_playfield        ; 3, 15    ; checking dex is not required here - we know there is at least one more sprite
no_more_sprites             ;+1, 9     ;
    lda #$ef                ; 2, 11
    sta nexty               ; 3, 14
    dex                     ; 2, 16    ;
    beq draw_playfield_end  ; 2, 18    ;
    jmp do_playfield        ; 3, 21    ;
draw_playfield_end          ;+1, 7 19 23 ;


; draw_hills
;
; draw the hills just above the lunokhod. we have at most 52 cycles
; before the next line draws. p1dataptr and p1color were populated in
; draw_playfield for  us.
;
; 4 scan lines
;
    echo "------", [*], [* - $F000]d, "draw_hills"

draw_hills
    sta HMCLR           ; 3, 26 ; so HMOVE's will not reposition debris

    lda scrollpos       ; 3, 29     ; calculate the PFData offset and
    and #%11111100      ; 2, 31     ; put it in x
    clc                 ; 2, 33     ;
    adc #4              ; 2, 35     ;
    tax                 ; 2, 37     ;

    lda #SNOWCOLOR      ; 2, 39     ; change playfield color
    sta COLUPF          ; 3, 42     ;

    lda #$FF            ; 2, 44     ; initialize the debris counter at
    sta temp1lo         ; 3, 47     ; FF, this allows us to continue
                                    ; using skipdraw with 'nexty' from
                                    ; draw_playfield

hills_loop              ; +1, 42 47

    sec                 ; 2, 49 ;
    sbc nexty           ; 3, 52 ;
    adc #ENEMYHEIGHT    ; 2, 54 ;
    bcs doDrawSprite    ; 2, 56 ;
    sta WSYNC           ; 3, 59      ;
    sta HMOVE           ; 3, 3  ;
    lda PFData0-1,X     ; 4, 7
    sta PF0             ; 3, 10
    jmp overSprite      ; 3, 13
doDrawSprite            ; 1, 57 ; draw the sprite
    tay                 ; 2, 59
    lda (p1dataptrlo),y ; 6, 16
    sta WSYNC           ; 3, 62
    sta HMOVE           ; 3, 3
    sta GRP1            ; 3, 19
    lda (p1colorlo),y   ; 6, 16
    sta COLUP1          ; 3, 19
    lda PFData0-1,X     ; 4, 7
    sta PF0             ; 3, 10
overSprite              ;+1, 13
    lda PFData1-1,X     ; 4, 17
    sta PF1             ; 3, 20
    lda PFData2-1,X     ; 4, 24
    sta PF2             ; 3, 27
;    sleep 9
    dex                 ; 2, 29
    dec temp1lo         ; 5, 34
    lda temp1lo         ; 3, 37
    cmp #$FB            ; 2, 39
    bne hills_loop      ; 2, 41

draw_hills_end


;
; Draw the sled and rail
;

    echo "------", [*], [* - $F000]d, "draw_sled_and_rail"

draw_sled_and_rail


    lda #0              ; 2, 43
    sta ENAM0           ; 3, 46  ; turn off missiles
    sta ENAM1           ; 3, 49  ;

    sta WSYNC
;    sta HMOVE           ; 3, 3 ;

    sta COLUPF          ; 3, 6  ; set pf color to black for HMOVE hiding
    sta PF0             ; 3, 9  ; clear playfield data
    sta PF1             ; 3, 12 ;
    sta PF2             ; 3, 15 ;

    lda #SNOWCOLOR      ; 2, 17 ; change the background color to white
    sta COLUBK          ; 3, 20 ; ""

    ldx #10

    lda temp1lo         ;

do_sled

    sec                 ; 2
    sbc nexty           ; 3,  ;
    adc #ENEMYHEIGHT    ; 2,  ;
    bcs doDrawSprite2; 2,  ;
    sta WSYNC
    lda PlayerSprite,X
    sta GRP0                   ; 45
    lda PlayerColor,X
    sta COLUP0

    jmp overSprite2
doDrawSprite2
    tay
    lda (p1dataptrlo),y ; 6,        ;
    sta WSYNC
    sta GRP1
    lda (p1colorlo),y   ; 6, 16
    sta COLUP1          ; 3, 19    ; 3,

    lda PlayerSprite,X
    sta GRP0                   ; 45
    lda PlayerColor,X
    sta COLUP0    ;

overSprite2
    dec temp1lo
    lda temp1lo
    dex
    cpx #0
    bne do_sled
;; TEMP
    stx GRP1
;; END TEMP

;    lda #RAILCOLOR
;    sta COLUBK
    lda PlayerSprite,X
    sta WSYNC
    sta GRP0

    lda #PLATFORMCOLOR
    sta COLUBK
    lda #0
    sta GRP0

    ldy #172

draw_sled_and_rail_end

    echo "------", [*], [* - $F000]d, "draw_guage"

draw_gauge
    lda #%00000111      ; 2
    sta NUSIZ0          ; 3
    lda #63             ; 2
    ldx #0              ; 2
    jsr do_sprite_move  ; -, 9
    lda radlevel       ; 3, 12
    sta GRP0            ; 3, 15

    lda frameCount
    and #%00000111
    adc #$14

;    lda #GAUGECOLOR     ; 2, 17
    sta COLUP0          ; 3, 20
    sta WSYNC
    sta HMCLR
    sta WSYNC
    sta HMOVE
    lda #0
    sta GRP0
    lda #%11110000      ; 2
    sta PF2             ; 3
    lda #%00110001      ; 2
    sta CTRLPF          ; 3
    lda #PLATFORMCOLOR
    sta COLUPF
draw_gauge_end



; draw_radar
;
; draw the radar. this is extremely tight and needs to close to
; page boundary, hence the adjustment below.

    echo "------", [*], [* - $F000]d, "draw_radar BEFORE ALIGN"

;    jmp draw_radar
;    ALIGN #255,#0

    echo "------", [*], [* - $F000]d, "draw_radar AFTER ALIGN"

draw_radar
    sta WSYNC
    sta HMOVE
    lda #1              ; 2, 2
    sta HMCLR           ; 3, 5
    sta NUSIZ0          ; 3, 8
    sta NUSIZ1          ; 3, 11

    nop
    nop
    nop
    nop
    nop
    nop
    lda #$8e
    jmp setColor

; old radar color code - leave this here for now
;    lda radarColor      ; 3, 14
;    asl                 ; 2, 16
;    sta radarColor      ; 3, 19
;    and #%10000000      ; 2, 21
;    beq normalRadarColor; 2, 23
;    lda #$1e             ; 2, 25
;    jmp setColor        ; 3, 28
;normalRadarColor        ; +1, 24
;    lda #RADARCOLOR     ; 2, 26
;    nop                 ; 2, 28

setColor
    sta COLUP0          ; 3, 31
    sta COLUP1          ; 3, 34
    nop                 ; 2, 36

;    SLEEP 17            ; 20, 39
    sta RESP0           ; 3, 42
    sta RESP1           ; 3, 45
    lda #%11110000      ; 2, 47
    sta HMP0            ; 3, 50
    lda #%00000000      ; 2, 52
    sta HMP1            ; 3, 55
;    sty nexty           ; 3, 58
    ldx #32             ; 2, 60
    lda #0
    sta COLUPF          ; 3, 6
    sta WSYNC           ; 3, 0
    sta HMOVE           ; 3, 3
    nop                 ; 2, 5
radar_loop              ; +1, 5
    dex                 ; 2, 7
    dex                 ; 2, 9
    dex                 ; 2  11
    dex                 ; 2, 13
    lda radarRam,x      ; 4, 17
    sta GRP0            ; 3, 20
    lda radarRam+1,x    ; 4, 24
    sta GRP1            ; 3, 27
    ldy radarRam+3,x    ; 4, 31
    lda radarRam+2,x    ; 4, 35
    sleep 9             ; 9, 44
    sta GRP0            ; 3, 47
    sty GRP1            ; 3, 50

    nop
    nop
    nop
    nop
    nop
    nop
    lda #$8e
    jmp setColor2
; old radar color code - leave here for now
;    lda radarColor      ; 3, 53
;    asl                 ; 2, 55
;    sta radarColor      ; 3, 58
;
;    and #%10000000      ; 2, 60
;    beq normalRadarColor2 ; 2, 62
;    lda #$1e            ; 2, 64
;    jmp setColor2       ; 3, 67
;normalRadarColor2       ; +1, 63
;    lda #RADARCOLOR     ; 2, 65
;    nop                 ; 2, 67
setColor2
    sta COLUP0          ; 3, 70
    sta COLUP1          ; 3, 73

    sta WSYNC           ; 3, 75
    cpx #4              ; 2, 2
    bne radar_loop      ; 2, 4

    lda #RADARCOLOR2    ; 2, 6
    sta COLUP1          ; 3, 9
    sta COLUP0          ; 3, 12
    lda radarRam        ; 3, 15
    nop                 ; 2, 17
    sta GRP0            ; 3, 20 ; this must be on cycle 19 or 20
    lda radarRam+1      ; 3, 23
    sta GRP1            ; 3, 26 ; this must be on cycle 26 or 27
    ldy radarRam+3      ; 3, 29
    lda radarRam+2      ; 3, 32
    sleep 12            ; 12, 44
    sta GRP0            ; 3, 47 ; these three must start on cycle 47
    sty GRP1            ; 3, 50
    sta WSYNC

    sleep 10            ; 10, 10
    ldx #0              ; 2, 12
    lda radarRam,x      ; 4, 16
    sta GRP0            ; 3, 19 ; this must be on cycle 19 or 20
    lda radarRam+1,x    ; 4, 23
    sta GRP1            ; 3, 26 ; this must be on cycle 26 or 27
    ldy radarRam+3,x    ; 4, 30
    lda radarRam+2,x    ; 4, 34
;    stx temp1lo         ; 3, 37
;    ldx #0              ; 2, 39
    sleep 10             ; 10, 44
    sta GRP0            ; 3, 47 ; these three must start on cycle 47
    sty GRP1            ; 3, 50
;    stx GRP0            ; 3, 53
;    stx GRP1            ; 3, 56

    sta WSYNC
    lda #$0e     ; 2, 16
    sta COLUP0          ; 3, 19
    sta COLUP1          ; 3, 14
    lda #0              ; 2, 6
    sta GRP1            ; 3, 9
    sta GRP0            ; 3, 12
    sta PF2
    sta NUSIZ0         ; 3, 26
    sta NUSIZ1         ; 3, 29
    sta WSYNC
    sta WSYNC
draw_radar_end

    echo "------", [*], [* - $F000]d, "overscan_start"

overscan_start
    lda #2              ; turn on the vblank
    sta VBLANK          ; ""
    lda  #34          ; wait for 4544 cycles then sync
    sta  TIM64T         ; ""
overscan_start_end

;
; Process collisions
;

    echo "------", [*], [* - $F000]d, "do_collisions"

do_collisions
    lda CXM0P           ; 3     ; Check if any collisions were measured with missile 0
    and #%10000000      ; 2     ; ""
;    cmp #0              ; 2     ; ""
    beq no_m0_collision ; 2     ; ""

    ldx #0              ; 2     ; Walk through the debris list checking for collisions
m0_collision_loop       ; +1    ; ""
    lda debrisy,x        ; 4     ; ""
    sec                 ; 2     ; ""
    sbc m1ys            ; 3     ; ""
    cmp #$FD            ; 2     ; ""
    bcs found_m0        ; 2     ; ""
    cmp #ENEMYHEIGHT    ; 2     ;
    bcc found_m0        ; 2     ; ""
    jmp no_pm0_collision ; 3    ; ""
found_m0
    lda debrisinfo,x  ; if the debris is in condemned state this is not a collision
    and #%00000111      ; ""
;    cmp #0              ; ""
    bne no_pm0_collision ; ""
    jsr remove_shot_debris
    lda #$FF
    sta m1ys
    jmp no_m0_collision
no_pm0_collision
    inx
    cpx debrisCount
    bcc m0_collision_loop

no_m0_collision
    lda CXM1P           ; 3     ; Check if missile 1 has a collision
    and #%01000000      ;       ; ""
;    cmp #0              ;       ; ""
    beq no_m1_collision ;       ; ""

    ldx #0
m1_collision_loop
    lda debrisy,x
    sec
    sbc m0ys
    cmp #$FD
    bcs found_m1
    cmp #ENEMYHEIGHT
    bcc found_m1
    jmp no_pm1_collision
found_m1
    lda debrisinfo,x  ; if the debris is in a condemned state this is not a collision
    and #%00000111      ; ""
;    cmp #0              ; ""
    bne no_pm1_collision ; ""
    jsr remove_shot_debris
    lda #$FF
    sta m0ys
    jmp no_m1_collision
no_pm1_collision
    inx
    cpx debrisCount
    bcc m1_collision_loop

no_m1_collision

    ; check for player/player collision
    lda CXPPMM
    and #%10000000
    beq do_collisions_end
    lda #MODE_GAME_OVER
    sta gameMode

do_collisions_end
    sta CXCLR


; swap_missiles2
;
; every other frame we swap even/odd missiles

    echo "------", [*], [* - $F000]d, "swap_missiles"

swap_missiles2:

    lda frameCount          ; 3
    and #%0000001           ; 2
    beq swap_missiles2_end  ; 3-4
    ldx m0ys                ; 3
    lda m2ys                ; 3
    sta m0ys                ; 3
    stx m2ys                ; 3
    ldx m1ys                ; 3
    lda m3ys                ; 3
    sta m1ys                ; 3
    stx m3ys                ; 3

swap_missiles2_end

; check console input

    echo "------", [*], [* - $F000]d, "check_console_input"

check_console_input

    lda SWCHB                   ; if the reset button is not pressed
    lsr                         ; skip the down check
    bcs resetNotPressed         ;

    lda #MODE_RESET_DOWN        ; set state to MODE_RESET_DOWN
    sta gameMode                ;
    jmp do_joystick_end

resetNotPressed

    lda gameMode                ; reset button is up and we are currently
    cmp #MODE_RESET_DOWN        ; not in
    bne do_joystick

    lda #MODE_RESET             ; set state to MODE_RESET_DOWN
    sta gameMode                ;
    jmp do_joystick_end

do_joystick

    lda gameMode
    cmp #0
    beq do_joystick_end
    cmp #252
    bcs do_joystick_end

    lda INPT4           ; first check the trigger
    bmi no_button
    jmp create_missile
create_missile_cb
no_button
    lda #%01000000      ; Left?
    bit SWCHA           ;
    bne do_joystick_1   ; Nope, check Right
    dec p0s
    jmp check_bounds
do_joystick_1
    lda #%10000000      ; Right?
    bit SWCHA           ;
    bne do_slowdown     ; Nope, start slowing down
    inc p0s
    jmp check_bounds
do_slowdown

;    lda frameCount
;    and #%00000011
;    bne do_joystick_end

    lda p0s
    beq do_joystick_end
    bmi do_left_slowdown
    dec p0s
    jmp check_bounds
do_left_slowdown
    inc p0s
check_bounds
    lda p0s
    cmp #MINSPEED
    bmi less_than_1
    cmp #MAXSPEED+1
    bpl greater_than_1
    jmp do_joystick_end
less_than_1
    lda #MINSPEED
    sta p0s
    jmp do_joystick_end
greater_than_1
    lda #MAXSPEED
    sta p0s
do_joystick_end

;
; If there was no joystick movement, then skip the positioning routines.
;
    lda p0s
    bne p0_position
    jmp skip_positioning

;
; p0_position determine the position for player 0, which also
; determines the virtual screen offset
;

    echo "------", [*], [* - $F000]d, "p0_position"

p0_position
    lda p0s
    clc
    adc p0vxlo
    sta p0vxlo
p0_position_end

;
; Compute the screen position of the player and scroll offset
;

    echo "------", [*], [* - $F000]d, "calc_offset"

calc_offset
    lda vxoffsetlo
    sta temp1lo
    lda p0s
    clc
    adc p0x
    cmp #MINXPOS
    bcc adjustOffsetDown
    cmp #MAXXPOS+1
    bcs adjustOffsetUp
    jmp noAdjustOffset
adjustOffsetDown
    sta temp3
    lda #MINXPOS
    sec
    sbc temp3
    sta temp3
    lda vxoffsetlo
    sec
    sbc temp3
    sta vxoffsetlo

    lda scrollpos
    sec
    sbc temp3
    sta scrollpos
    cmp #200
    bcs wrapUp
    jmp scrollDone
wrapUp
    clc
    adc #80
    sta scrollpos
scrollDone


    jmp noAdjustOffset
adjustOffsetUp
    sec
    sbc #MAXXPOS
    sta temp3
    lda vxoffsetlo
    clc
    adc temp3
    sta vxoffsetlo

    lda scrollpos
    clc
    adc temp3
    sta scrollpos
    cmp #80
    bcs wrapDown
    jmp scrollDone2
wrapDown
    sec
    sbc #80
    sta scrollpos
scrollDone2


noAdjustOffset

    lda p0vxlo
    jsr convert_virtual_xpos
    sta p0x

calc_offset_end

    echo "------", [*], [* - $F000]d, "create_debris"

skip_positioning

create_debris

    lda gameMode                    ; only create new debris if the game
    cmp #MODE_IN_PROGRESS           ; is in progress
    beq ce_continue                 ;
    jmp create_debris_end           ;
ce_continue                         ;

    lda #20                         ; load waveLimit into a
                                    ; todo: multiply by vertical speed
    cmp waveCounter                 ; compare a to waveCounter
    bne skip_gameMode_change2           ; if a != limit, skip the rest
    lda debrisCount                 ; load debrisCount into a
    cmp #0                          ; compare a to 0
    bne skip_gameMode_change1           ; if a != 0 skip
    lda #MODE_END_WAVE              ; if there is then we set gameMode to
    sta gameMode                        ; MODE_END_WAVE and set the waveCounter
    inc wave
    lda #0                          ; back to 0
    sta waveCounter                 ;
skip_gameMode_change1                   ;
    jmp create_debris_end           ;
skip_gameMode_change2                   ;

    lda debrisCount                 ; make sure there is a slot left
    cmp #8                          ; ""
    beq create_debris_end           ; ""

    lda debrisy                     ; make sure there is enough space
    cmp #PLAYFIELDSZ-8              ; for new debris.
    bcs create_debris_end           ;

    ldx #6                          ; shift the list down (this can take
shift_down                  ; +1    ; up to 350 cycles - ouch!)
    lda debrisvx,x          ; 4     ;
    sta debrisvx+1,x        ; 4     ;
    lda debrisinfo,x        ; 4     ;
    sta debrisinfo+1,x      ; 4     ;
    lda debrisy,x           ; 4     ;
    sta debrisy+1,x         ; 4     ;
    cpx #0                  ; 2     ;
    beq done_shift_down     ; 2     ;
    dex                     ;       ;
    jmp shift_down          ;       ;
done_shift_down                     ;

    lda #PLAYFIELDSZ+11             ; set the y position for the new
    sta debrisy                     ; debris

    jsr update_random_seed

    and #%00111000                  ; use the random number generated
    sta debrisinfo                  ; for position to set the debris
                                    ; info (type of sprite, fall
                                    ; direction)

    lda randomSeed
    cmp #128
    bcs skipDirectionChange

    lda direction
    eor #$FF
    sta direction

skipDirectionChange

    lda direction
    cmp #0
    beq rightDirection

    lda randomSeed
    ora #%0010000
    and #%0011111
    clc
    adc debrisvx,1
    sta debrisvx
    jmp doneDirection

rightDirection

    lda randomSeed
    ora #%0010000
    and #%0011111
    eor #$FF
    sec
    adc debrisvx,1
    sta debrisvx

doneDirection

    inc waveCounter
    inc debrisCount
    lda debrisCount
    cmp #8
    bcc create_debris_end
    lda #8
    sta debrisCount

create_debris_end

    lda #0
    sta COLUBK          ; so the first line on the top will be black

;
; Clear the RAM that we will use to build the radar.
; Unrolled to reduce cycle use.  (110 cycles)
;

    echo "------", [*], [* - $F000]d, "clear ram for radar"

    lda #$00            ; 2, 11
    sta radarRam        ; 3, 14
    sta radarRam+1      ; 3, 17
    sta radarRam+2      ; 3, 20
    sta radarRam+3      ; 3, 23
    sta radarRam+4      ; 3, 26
    sta radarRam+5      ; 3, 29
    sta radarRam+6      ; 3, 32
    sta radarRam+7      ; 3, 35
    sta radarRam+8      ; 3, 38
    sta radarRam+9      ; 3, 41
    sta radarRam+10     ; 3, 44
    sta radarRam+11     ; 3, 47
    sta radarRam+12     ; 3, 50
    sta radarRam+13     ; 3, 53
    sta radarRam+14     ; 3, 56
    sta radarRam+15     ; 3, 59
    sta radarRam+16     ; 3, 62
    sta radarRam+17     ; 3, 65
    sta radarRam+18     ; 3, 68
    sta radarRam+19     ; 3, 71
    sta radarRam+20     ; 3, 74
    sta radarRam+21     ; 3, 77 ----- scanline + 1
    sta radarRam+22     ; 3, 2
    sta radarRam+23     ; 3, 5
    sta radarRam+24     ; 3, 8
    sta radarRam+25     ; 3, 11
    sta radarRam+26     ; 3, 14
    sta radarRam+27     ; 3, 17
    sta radarRam+28     ; 3, 20
    sta radarRam+29     ; 3, 23
    sta radarRam+30     ; 3, 26
    sta radarRam+31     ; 3, 29
    iny                 ; 2, 31
    iny                 ; 2, 33

;
; Draw our debris onto the radar. radar memory is a 4x8 byte area arranged
; by row:
;
;                            column
;                 00       01       02       03
;        00    00000000 00000000 00000000 00000000
;        01    00000000 00000000 00000000 00000000
;      r 02    00000000 00000000 00000000 00000000
;      o 03    00000000 00000000 00000000 00000000
;      w 04    00000000 00000000 00000000 00000000
;        05    00000000 00000000 00000000 00000000
;        06    00000000 00000000 00000000 00000000
;        07    00000000 00000000 00000000 00000000
;

    echo "------", [*], [* - $F000]d, "map_debris"

map_debris
    lda debrisCount
;    cmp #0
    beq map_debris_done

    ldx #0
;    stx radarColor
    lda debrisy         ; calculate what will be the starting y coord on the map
    sec                 ; "" divide by 21
DivideBy22              ; +1
    inx                 ; 2
    sbc #22             ; 2
    bcs DivideBy22      ; 2 repeat up to 6 times, result will be in [0,6]
;    inx                 ; add 1 to make it [1,7]
    stx temp1hi
    ldx #0
map_debris_loop

;    lda radarColor
;    asl
;    sta radarColor

    lda debrisinfo,x    ; skip the enmy if it has been condemned
    and #%00000001      ; ""
    cmp #%00000001      ; ""
    beq skip_debris     ; ""

;    lda debrisinfo,x
;    and #%00011000
;    cmp #%00011000
;    bne notRadioactive
;    inc radarColor
;notRadioactive

    ldy #0
    lda debrisvx,x
    cmp #64
    bcc no1a
    iny
no1a
    cmp #128
    bcc no2a
    iny
no2a
    cmp #192
    bcc no3a
    iny
no3a
    sty temp1lo

    lda temp1hi
    asl                 ; "" (multiple result by 4)
    asl                 ; ""

    clc                 ; Add row and column offsets together to
    adc temp1lo          ; get our insert offset
    sta temp1lo          ; ""

    lda debrisvx,x       ; calculate the remainder put it in Y
    lsr                 ; ""
    lsr                 ; ""
    lsr                 ; ""
    tay                 ; ""

    lda map_debris_table,y
    sta temp2lo

    lda temp1lo
    tay
    lda radarRam,y
    ora temp2lo
    sta radarRam,y
skip_debris
    dec temp1hi
    beq map_debris_done
    inx
    cpx debrisCount
    bcc map_debris_loop
map_debris_done

    echo "------", [*], [* - $F000]d, "overscan_end"

overscan_end
waitOnOverscan          ; wait for overscan to finish
    lda INTIM           ; ""
    bne waitOnOverscan  ; ""
overscan_end_end

    jmp main_loop

;
; The trigger was pressed, create a missile if we can
;

    echo "------", [*], [* - $F000]d, "routine: create_missile"

create_missile

    lda shotcount
;    cmp #0
    bne no_free_missiles
    ldx #8
check_missile_n
    cpx #0
    beq no_free_missiles
    dex
    dex
    lda m0vxlo+1,x
    cmp #$FF
    bne check_missile_n
    lda #SHOT_TIMING
    sta shotcount
    lda #0
    sta m0vxlo+1,x
    lda p0vxlo          ; add the P0center to align with the ship
    sta m0vxlo,x
no_free_missiles
    jmp create_missile_cb

; remove_shot_debris
;
; remove debris that was shot, x is assumed to be set to the debrisinfo
; index.
;
;     * Set the condemned countdown timer in debrisinfo (00000xxx) to 001
;     * Set the sprite index in debrisinfo (00xxx000) to 100

    echo "------", [*], [* - $F000]d, "routine: remove_shot_debris"

remove_shot_debris

    lda effectState             ; play the explosion audio, but only
    cmp #%01000000              ; when there are other events playing
    bcs skipNoise

    lda #%00111111            ; 2     ;
    sta effectState   ; 3     ;

skipNoise

    lda debrisinfo,x   ; 4     ; set the condemned counter to 1 on this
    ora #%00000001     ; 2     ; debris
    sta debrisinfo,x   ; 4     ;

    lda #1                     ; adjust the score
    sed                        ;
    clc                        ;
    adc score                  ;
    sta score                  ;
    lda #0                     ;
    adc score+1                ;
    sta score+1                ;
    cld                        ;
    rts                ; 6

remove_shot_debris_end

; Horizontal position routine. Found by members of the Stella mailing list
; in code for Atari Battlezone. HMOVE needs to be called some time after
; invoking this routine.
;
; Inputs:
;   A = Desired position.
;   X = Desired object to be positioned (0-5).
; scanlines:
;   If control comes on or before cycle 73 then 1 scanline is consumed.
;   If control comes after cycle 73 then 2 scanlines are consumed.
; Outputs:
;   X = unchanged
;   A = Fine Adjustment value.
;   Y = the "remainder" of the division by 15 minus an additional 15.
;       control is returned on cycle 6 of the next scanline.
;
; TIME: 85 + whatever time was left in the current line

    echo "------", [*], [* - $F000]d, "routine: do_sprite_move"

do_sprite_move
    sta WSYNC           ; 3, ?
    sec                 ; 2, 2
DivideLoop              ; This loop MAX 54 cycles if A is < 160, MIN 4
    sbc #15             ; 2, [4
    bcs DivideLoop      ; 2/3 [6,56]
    tay                 ; 2, [8,58]
    lda FineAdjustTableEnd,Y    ; 5, [13,63]
    nop                 ; 2, [15,65]
    sta HMP0,X          ; 4  [19,69]
    sta RESP0,x         ; 4  [23,73]
    sta WSYNC           ; 3  [26,76]
    sta HMOVE           ; 3, 3
    rts                 ; 6, 9

;
; Input:
;     A : least significat byte of vx position to convert
;     vxoffsetlo : least significant byte of scroll offset
; Output:
;     A : xposition or $FF if would be off screen

    echo "------", [*], [* - $F000]d, "routine: convert_virtual_xpos"

convert_virtual_xpos
    sec                 ; 2, 2
    sbc vxoffsetlo      ; 3, 5
    cmp #MINXSCREEN              ; 2, 7
    bcc off_screen      ; 2, 9
    cmp #MAXXSCREEN            ; 2, 11
    bcs off_screen      ; 2, 13
    rts                 ; 6, 19
off_screen              ; +1, 14
    lda #$FF            ; 2, 16
    rts                 ; 6, 22
convert_virtual_xpos_end

; update our random seed

    echo "------", [*], [* - $F000]d, "update_random_seed"

update_random_seed:

    lda randomSeed
    beq doEor
    asl
    beq noEor
    bcc noEor
doEor
    eor #$1d
noEor
    sta randomSeed
    rts

update_random_seed_end


    echo "------", [*], [* - $F000]d, "code done, before ALIGN"

;    ALIGN #256,#0

    echo "------", [*], [* - $F000]d, "code done, after ALIGN"

; This table converts the "remainder" of the division by 15 (-1 to -15) to the correct
; fine adjustment value. This table is on a page boundary to guarantee the processor
; will cross a page boundary and waste a cycle in order to be at the precise position
; for a RESP0,x write

    echo "------", [*], [* - $F000]d, "FineAdjustTable"

FineAdjustTableBegin
        .byte %01100000 ;left 6
        .byte %01010000
        .byte %01000000
        .byte %00110000
        .byte %00100000
        .byte %00010000
        .byte %00000000 ;left/right 0
        .byte %11110000
        .byte %11100000
        .byte %11010000
        .byte %11000000
        .byte %10110000
        .byte %10100000
        .byte %10010000
        .byte %10000000 ;right 8
FineAdjustTableEnd      =       FineAdjustTableBegin - 241

    echo "------", [*], [* - $F000]d, "map_debris_table"

map_debris_table
    .byte #%10000000
    .byte #%01000000
    .byte #%00100000
    .byte #%00010000
    .byte #%00001000
    .byte #%00000100
    .byte #%00000010
    .byte #%00000001
    .byte #%10000000
    .byte #%01000000
    .byte #%00100000
    .byte #%00010000
    .byte #%00001000
    .byte #%00000100
    .byte #%00000010
    .byte #%00000001
    .byte #%10000000
    .byte #%01000000
    .byte #%00100000
    .byte #%00010000
    .byte #%00001000
    .byte #%00000100
    .byte #%00000010
    .byte #%00000001
    .byte #%10000000
    .byte #%01000000
    .byte #%00100000
    .byte #%00010000
    .byte #%00001000
    .byte #%00000100
    .byte #%00000010
    .byte #%00000001

; offset 48

; one entry per wave, controls vertical speed

vSpeedTable
    .byte #%00000011
    .byte #%00000000
    .byte #%00000001
    .byte #%00000000

    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000

    .byte #%00000001
    .byte #%00000001
    .byte #%00000001
    .byte #%00000001

    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000

; one entry per wave, controls game behavior
;
;     0 - 5 horizontal speed mask (11111 special case)
;     5      horizontal speed multiplier
;     6-8    vertical speed mask
;
;

hSpeedTable

    .byte #%00011111
    .byte #%00001111
    .byte #%00000111
    .byte #%00000011

    .byte #%00011111
    .byte #%00001111
    .byte #%00000001
    .byte #%10000000

    .byte #%00001111
    .byte #%00000111
    .byte #%00000011
    .byte #%00000001

    .byte #%00000111
    .byte #%00000011
    .byte #%00000001
    .byte #%00000000

tempoTable
    .byte #10
    .byte #9
    .byte #8
    .byte #7

    .byte #6
    .byte #10
    .byte #9
    .byte #8

    .byte #6
    .byte #7
    .byte #8
    .byte #9

    .byte #6
    .byte #7
    .byte #8
    .byte #9


; moveTable bits
;   0 = unused
;   1 = move left
;   2 = move right
;   3 = unused
;   4 = unused
;   5 = unused
;   6 & 7 = move adder
moveTable              ; if and table is 3 then horizontal movement is slower
    .byte #%00000001  ; 000 absorber    down
    .byte #%00100010  ; 001 medium rock right
    .byte #%00100001  ; 010 small rock  right
    .byte #%00100001  ; 011 radioactive right
    .byte #%01000010  ; 100 medium rock left
    .byte #%00000001  ; 101 rock        down
    .byte #%01000001  ; 110 small       left
    .byte #%01000001  ; 111 radioactive left

; signed integer offset for debris movement by wave

sineTable
    .byte #%00000001  ; move right 1
    .byte #%00000001  ; move right
    .byte #%00000001  ; move right 1
    .byte #%00000001  ; move right 1
    .byte #%11111111  ; move left
    .byte #%11111111  ; move left 1
    .byte #%11111111  ; move left 1
    .byte #%11111111  ; move left 1

    echo "------", [*], [* - $F000]d, "radarBitMask"

radarBitmask
    .byte #%10000000
    .byte #%01000000
    .byte #%00100000
    .byte #%00010000
    .byte #%00001000
    .byte #%00000100
    .byte #%00000010
    .byte #%00000001

    echo "------", [*], [* - $F000]d, "before player sprites"

PlayerSprite
    .byte #%01000010
    .byte #%10100101
    .byte #%01000010
    .byte #%00111100
    .byte #%01111110
    .byte #%01111110
    .byte #%11111111
    .byte #%11111111
    .byte #%00011000
    .byte #%00011000
    .byte #%00011000

PlayerColor
    .byte #$0a
    .byte #$0a
    .byte #$0a
    .byte #$0a
    .byte #$0a
    .byte #$0a
    .byte #$0c
    .byte #$0e
    .byte #$02
    .byte #$42
    .byte #$42

spriteTable
    .byte #<sprite3FrameTable  ; 000 Absorber
    .byte #>sprite3FrameTable
    .byte #<sprite7FrameTable  ; 001 Right
    .byte #>sprite7FrameTable
    .byte #<sprite5FrameTable  ; 010 Small Right
    .byte #>sprite5FrameTable
    .byte #<sprite1FrameTable  ; 011 Radioactive
    .byte #>sprite1FrameTable
    .byte #<sprite6FrameTable  ; 100 Left
    .byte #>sprite6FrameTable
    .byte #<sprite2FrameTable  ; 101 Down
    .byte #>sprite2FrameTable
    .byte #<sprite4FrameTable  ; 110 Small Left
    .byte #>sprite4FrameTable
    .byte #<sprite1FrameTable  ; 111 Radioactive
    .byte #>sprite1FrameTable

sprite1FrameTable
    .byte #<sprite1Frame1
    .byte #>sprite1Frame1
    .byte #<sprite1Frame3
    .byte #>sprite1Frame3
    .byte #<sprite1Frame5
    .byte #>sprite1Frame5
    .byte #<sprite1Frame7
    .byte #>sprite1Frame7

sprite2FrameTable
    .byte #<sprite2Frame1
    .byte #>sprite2Frame1
    .byte #<sprite2Frame1
    .byte #>sprite2Frame1
    .byte #<sprite2Frame2
    .byte #>sprite2Frame2
    .byte #<sprite2Frame2
    .byte #>sprite2Frame2

sprite3FrameTable
    .byte #<sprite3Frame1
    .byte #>sprite3Frame1
    .byte #<sprite3Frame1
    .byte #>sprite3Frame1
    .byte #<sprite3Frame1
    .byte #>sprite3Frame1
    .byte #<sprite3Frame1
    .byte #>sprite3Frame1

sprite4FrameTable           ; left left
    .byte #<sprite4Frame1
    .byte #>sprite4Frame1
    .byte #<sprite4Frame2
    .byte #>sprite4Frame2
    .byte #<sprite4Frame3
    .byte #>sprite4Frame3
    .byte #<sprite4Frame2
    .byte #>sprite4Frame2


sprite5FrameTable           ; right right
    .byte #<sprite5Frame1
    .byte #>sprite5Frame1
    .byte #<sprite5Frame2
    .byte #>sprite5Frame2
    .byte #<sprite5Frame3
    .byte #>sprite5Frame3
    .byte #<sprite5Frame2
    .byte #>sprite5Frame2

sprite6FrameTable
    .byte #<sprite6Frame1
    .byte #>sprite6Frame1
    .byte #<sprite6Frame1
    .byte #>sprite6Frame1
    .byte #<sprite6Frame2
    .byte #>sprite6Frame2
    .byte #<sprite6Frame2
    .byte #>sprite6Frame2

sprite7FrameTable
    .byte #<sprite7Frame1
    .byte #>sprite7Frame1
    .byte #<sprite7Frame1
    .byte #>sprite7Frame1
    .byte #<sprite7Frame2
    .byte #>sprite7Frame2
    .byte #<sprite7Frame2
    .byte #>sprite7Frame2

explosionFrameTable
    .byte #<explosionSprite1
    .byte #>explosionSprite1
    .byte #<explosionSprite1
    .byte #>explosionSprite1
    .byte #<explosionSprite2
    .byte #>explosionSprite2
    .byte #<explosionSprite2
    .byte #>explosionSprite2

sprite1Frame1
        .byte #%00000000
        .byte #%00000000
        .byte #%00000001
        .byte #%00000010
        .byte #%00000110
        .byte #%00001100
        .byte #%00011100
        .byte #%00111000
        .byte #%01111100
        .byte #%00111110
        .byte #%00011100
        .byte #%00111000
        .byte #%00110000
        .byte #%01100000
        .byte #%01000000
        .byte #%10000000
        .byte #%00000000
    .byte #<boltColors1
    .byte #>boltColors1
sprite1Frame3
        .byte #%00000000
        .byte #%00000000
        .byte #%00000001
        .byte #%00000010
        .byte #%00000110
        .byte #%00001100
        .byte #%00011100
        .byte #%00111000
        .byte #%01111100
        .byte #%00111110
        .byte #%00011100
        .byte #%00111000
        .byte #%00110000
        .byte #%01100000
        .byte #%01000000
        .byte #%10000000
        .byte #%00000000
    .byte #<boltColors3
    .byte #>boltColors3
sprite1Frame5
        .byte #%00000000
        .byte #%00000000
        .byte #%00000001
        .byte #%00000010
        .byte #%00000110
        .byte #%00001100
        .byte #%00011100
        .byte #%00111000
        .byte #%01111100
        .byte #%00111110
        .byte #%00011100
        .byte #%00111000
        .byte #%00110000
        .byte #%01100000
        .byte #%01000000
        .byte #%10000000
        .byte #%00000000
    .byte #<boltColors5
    .byte #>boltColors5
sprite1Frame7
        .byte #%00000000
        .byte #%00000000
        .byte #%00000001
        .byte #%00000010
        .byte #%00000110
        .byte #%00001100
        .byte #%00011100
        .byte #%00111000
        .byte #%01111100
        .byte #%00111110
        .byte #%00011100
        .byte #%00111000
        .byte #%00110000
        .byte #%01100000
        .byte #%01000000
        .byte #%10000000
        .byte #%00000000
    .byte #<boltColors7
    .byte #>boltColors7

sprite2Frame1
    .byte #%00000000
    .byte #%00111000
    .byte #%01100110
    .byte #%11111011
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%01111111
    .byte #%01111110
    .byte #%00011100
    .byte #%10000001
    .byte #%0100100
    .byte #%01000100
    .byte #%00010000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1

sprite2Frame2
    .byte #%00000000
    .byte #%00111000
    .byte #%01100110
    .byte #%11111011
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%11111111
    .byte #%01111111
    .byte #%01111110
    .byte #%00011100
    .byte #%00000000
    .byte #%00100100
    .byte #%00010000
    .byte #%00100100
    .byte #%00010000
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1

sprite3Frame1
    .byte #%00000000
    .byte #%01111100
    .byte #%11111110
    .byte #%01111100
    .byte #%01111100
    .byte #%00111000
    .byte #%00111000
    .byte #%00010000
    .byte #%00111000
    .byte #%00111000
    .byte #%01111100
    .byte #%01111100
    .byte #%11111110
    .byte #%01111100
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<absorberColor1
    .byte #>absorberColor1

sprite4Frame1
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%01110000
    .byte #%01011000
    .byte #%11111000
    .byte #%11111000
    .byte #%11101000
    .byte #%01110000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2

sprite4Frame2
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%01110000
    .byte #%01011000
    .byte #%11111000
    .byte #%11111000
    .byte #%11101000
    .byte #%01110000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2

sprite4Frame3
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%01110000
    .byte #%01011000
    .byte #%11111000
    .byte #%11111000
    .byte #%11101000
    .byte #%01110000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2


sprite5Frame1
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00001110
    .byte #%00001011
    .byte #%00011111
    .byte #%00011111
    .byte #%00011101
    .byte #%00001110
    .byte #%01010000
    .byte #%00000000
    .byte #%10000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2

sprite5Frame2
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00001110
    .byte #%00001011
    .byte #%00011111
    .byte #%00011111
    .byte #%00011101
    .byte #%00001110
    .byte #%10100000
    .byte #%00000000
    .byte #%01000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2

sprite5Frame3
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00001110
    .byte #%00001011
    .byte #%00011111
    .byte #%00011111
    .byte #%00011101
    .byte #%00001110
    .byte #%01000000
    .byte #%00000000
    .byte #%10000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor2
    .byte #>DebrisColor2

sprite6Frame1
    .byte #%00000000
    .byte #%00000000
    .byte #%01110000
    .byte #%11011000
    .byte #%10111000
    .byte #%11111000
    .byte #%11111000
    .byte #%11111010
    .byte #%11111000
    .byte #%11111010
    .byte #%01110010
    .byte #%00000100
    .byte #%01011001
    .byte #%00000010
    .byte #%00001000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1

sprite6Frame2
    .byte #%00000000
    .byte #%00000000
    .byte #%01110000
    .byte #%11011000
    .byte #%10111000
    .byte #%11111000
    .byte #%11111000
    .byte #%11111000
    .byte #%11111001
    .byte #%11111000
    .byte #%01110001
    .byte #%00000001
    .byte #%00000010
    .byte #%00101100
    .byte #%00000001
    .byte #%00000100
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1

sprite7Frame1
    .byte #%00000000
    .byte #%00000000
    .byte #%00001110
    .byte #%00011011
    .byte #%00011101
    .byte #%00011111
    .byte #%00010111
    .byte #%01011111
    .byte #%00011111
    .byte #%01011111
    .byte #%01001110
    .byte #%00100000
    .byte #%10011010
    .byte #%01000000
    .byte #%00010000
    .byte #%00000000
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1



sprite7Frame2
    .byte #%00000000
    .byte #%00000000
    .byte #%00001110
    .byte #%00011011
    .byte #%00011101
    .byte #%00011111
    .byte #%00010111
    .byte #%00011111
    .byte #%10011111
    .byte #%00011111
    .byte #%10001110
    .byte #%10000000
    .byte #%01000000
    .byte #%00110100
    .byte #%10000000
    .byte #%00100000
    .byte #%00000000
    .byte #<DebrisColor1
    .byte #>DebrisColor1


absorberColor1
    .byte #$0e
    .byte #$98
    .byte #$90
    .byte #$9a
    .byte #$90
    .byte #$9c
    .byte #$90
    .byte #$9e
    .byte #$90
    .byte #$9c
    .byte #$90
    .byte #$9a
    .byte #$90
    .byte #$98
    .byte #$90
    .byte #$98
    .byte #$90

DebrisColor1
    .byte #$0e
    .byte #$02
    .byte #$02
    .byte #$02
    .byte #$04
    .byte #$06
    .byte #$08
    .byte #$0a
    .byte #$0e
    .byte #$0c
    .byte #$0a
    .byte #$08
    .byte #$06
    .byte #$04
    .byte #$02
    .byte #$02
    .byte #$02

DebrisColor2
    .byte #$0e
    .byte #$02
    .byte #$02
    .byte #$02
    .byte #$04
    .byte #$06
    .byte #$08
    .byte #$08
    .byte #$0a
    .byte #$3a
    .byte #$3e
    .byte #$3c
    .byte #$3a
    .byte #$38
    .byte #$36
    .byte #$34
    .byte #$32

boltColors7
    .byte #$0e
    .byte #$18
    .byte #$1a
    .byte #$1e
    .byte #$1a
    .byte #$18
    .byte #$16
    .byte #$14
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
boltColors5
    .byte #$0e
    .byte #$12
    .byte #$12
    .byte #$14
    .byte #$16
    .byte #$18
    .byte #$1a
    .byte #$1e
    .byte #$1a
    .byte #$18
    .byte #$16
    .byte #$14
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
boltColors3
    .byte #$0e
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$14
    .byte #$16
    .byte #$18
    .byte #$1a
    .byte #$1e
    .byte #$1a
    .byte #$18
    .byte #$16
    .byte #$14
    .byte #$12
boltColors1
    .byte #$0e
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$12
    .byte #$14
    .byte #$16
    .byte #$18
    .byte #$1a
    .byte #$1e
    .byte #$1a

explosionSprite1
        .byte #%00000000
        .byte #%00000000
        .byte #%00101000
        .byte #%10000100
        .byte #%00101000
        .byte #%10010010
        .byte #%00101001
        .byte #%01000000
        .byte #%10001001
        .byte #%00100000
        .byte #%10001010
        .byte #%01001001
        .byte #%00000100
        .byte #%00100000
        .byte #%01001000
        .byte #%00010000
        .byte #%00000000
    .byte #<explosionColors
    .byte #>explosionColors

explosionSprite2
        .byte #%00000000
        .byte #%00000000
        .byte #%00101000
        .byte #%10010010
        .byte #%01000000
        .byte #%10001001
        .byte #%00100000
        .byte #%00101001
        .byte #%10001010
        .byte #%01001001
        .byte #%00000100
        .byte #%00100000
        .byte #%01001001
        .byte #%00000100
        .byte #%00010000
        .byte #%00100000
        .byte #%00000000
    .byte #<explosionColors
    .byte #>explosionColors

explosionColors
    .byte #$0e
    .byte #$02
    .byte #$08
    .byte #$0a
    .byte #$0a
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0e
    .byte #$0a
    .byte #$0a
    .byte #$08
    .byte #$08

    echo "------", [*], [* - $F000]d, "score sprites"

scoreTable
    .byte #<scoreZero
    .byte #<scoreOne
    .byte #<scoreTwo
    .byte #<scoreThree
    .byte #<scoreFour
    .byte #<scoreFive
    .byte #<scoreSix
    .byte #<scoreSeven
    .byte #<scoreEight
    .byte #<scoreNine
    .byte #>scoreZero
    .byte #>scoreOne
    .byte #>scoreTwo
    .byte #>scoreThree
    .byte #>scoreFour
    .byte #>scoreFive
    .byte #>scoreSix
    .byte #>scoreSeven
    .byte #>scoreEight
    .byte #>scoreNine

scoreZero
    .byte #%01111100
    .byte #%01000100
    .byte #%01000100
    .byte #%01000100
    .byte #%01000100
    .byte #%01000100
    .byte #%01111100
scoreOne
    .byte #%00000100
    .byte #%00000100
    .byte #%00000100
    .byte #%00000100
    .byte #%00000100
    .byte #%00000100
    .byte #%00000100
scoreTwo
    .byte #%01111100
    .byte #%01000000
    .byte #%01000000
    .byte #%01111100
    .byte #%00000100
    .byte #%00000100
    .byte #%01111100
scoreThree
    .byte #%01111100
    .byte #%00000100
    .byte #%00000100
    .byte #%01111100
    .byte #%00000100
    .byte #%00000100
    .byte #%01111100
scoreFour
    .byte #%00000100
    .byte #%00000100
    .byte #%00000100
    .byte #%01111100
    .byte #%01000100
    .byte #%01000100
    .byte #%01000100
scoreFive
    .byte #%01111100
    .byte #%00000100
    .byte #%00000100
    .byte #%01111100
    .byte #%01000000
    .byte #%01000000
    .byte #%01111100
scoreSix
    .byte #%01111100
    .byte #%01000100
    .byte #%01000100
    .byte #%01111100
    .byte #%01000000
    .byte #%01000000
    .byte #%01111100
scoreSeven
    .byte #%00000100
    .byte #%00000100
    .byte #%00000100
    .byte #%00000100
    .byte #%00000100
    .byte #%00000100
    .byte #%01111100
scoreEight
    .byte #%01111100
    .byte #%01000100
    .byte #%01000100
    .byte #%01111100
    .byte #%01000100
    .byte #%01000100
    .byte #%01111100
scoreNine
    .byte #%01111100
    .byte #%00000100
    .byte #%00000100
    .byte #%01111100
    .byte #%01000100
    .byte #%01000100
    .byte #%01111100

    echo "------", [*], [* - $F000]d, "radarColorTable"

radarColorTable
    .byte #$ae  ; absorbing
    .byte #$08  ; inert debris
    .byte #$08  ; inert debris
    .byte #$1e  ; radioactive
    .byte #$08  ; inert debris
    .byte #$08  ; inert debris
    .byte #$08  ; inert debris
    .byte #$1e  ; radioactive

    echo "------", [*], [* - $F000]d, "songLoopIntro"

songLoopIntro

    .byte #0, #8, #8, #1
    .byte #0, #8, #0, #24
    .byte #0, #8, #8, #1
    .byte #0, #8, #0, #24
    .byte #0, #8, #8, #1
    .byte #0, #8, #0, #24
    .byte #0, #8, #8, #1
    .byte #0, #8, #0, #24

songLoop

    .byte #30, #SONGTONE, #15, #11
    .byte #30, #SONGTONE, #2, #8
    .byte #30, #SONGTONE, #0, #17

    .byte #25, #SONGTONE, #15, #9
    .byte #25, #SONGTONE, #2, #3

    .byte #25, #SONGTONE, #15, #9
    .byte #25, #SONGTONE, #2, #3

    .byte #29, #SONGTONE, #15, #9
    .byte #29, #SONGTONE, #2, #4

    .byte #25, #SONGTONE, #15, #9
    .byte #25, #SONGTONE, #2, #4

    .byte #23, #SONGTONE, #15, #3
    .byte #23, #SONGTONE, #12, #5
    .byte #23, #SONGTONE, #2, #5

    .byte #22, #SONGTONE, #15, #12
    .byte #22, #SONGTONE, #2, #6
    .byte #22, #SONGTONE, #0, #16

    .byte #18, #SONGTONE, #15, #9
    .byte #18, #SONGTONE, #2, #3

    .byte #18, #SONGTONE, #15, #9
    .byte #18, #SONGTONE, #2, #3

    .byte #16, #SONGTONE, #15, #9
    .byte #16, #SONGTONE, #2, #3

    .byte #16, #SONGTONE, #15, #9
    .byte #16, #SONGTONE, #2, #7
    .byte #16, #SONGTONE, #0, #9

    .byte #255, #32, #0, #0

    echo "------", [*], [* - $F000]d, "include pfdata.asm"

    include "pfdata.asm"

;
; Check our free space
;

    echo "------", [*], [* - $F000]d, "end"
    echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"

;
; The 2600 looks at this memory location to find the program entry point
;

    ORG $FFFA
    .word sysinit          ; NMI
    .word sysinit          ; RESET
    .word sysinit          ; IRQ

END

;
; Each sprite has the following overhead:
;     frame table      : 16 bytes
;     color table      : 16 bytes
;     each frame       : 17 bytes
;     each color frame : 17 bytes
;     at least one entry in the index : 2 bytes
; So, a sprite with 8 full color frames takes 16 + 16 + 2 + 17x8 + 17x8 = 306 bytes
; A sprite with 2 full color frames = 102 bytes
; A sprite with 1 frame = 68 bytes
;





