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

; constants

MODE_ATTRACT            equ #0      ; game modes
MODE_TRANSITION         equ #1      ; 1-127
MODE_TRANSITION_TOP     equ #127    ;
MODE_IN_PROGRESS        equ #128    ;
MODE_END_WAVE           equ #129    ; 129-253
MODE_END_WAVE_TOP       equ #253    ;
MODE_GAME_OVER          equ #254    ;
MODE_RESET              equ #255    ;
MISSILE_FOLLOWS_SHIP    equ #0      ;
MAXSPEED                equ #2      ; the max speed of the sled
MINSPEED                equ #-2     ; the min speed of the sled
MINXPOS                 equ #61-1   ; minimum screen positionof the sled (32)
MAXXPOS                 equ #91+1   ; maximum screen position of the sled  (112)
POSDELTA                equ #MAXXPOS-MINXPOS        ; this must be equal to maxpos-minpos (80)
P0CENTER                equ #4      ; offset to center of player 0
MISSILESPEED            equ #5
MISSILESIZE             equ #9
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
RADARCOLOR2             equ #$48
GROUNDCOLOR             equ #$00
SNOWCOLOR               equ #$06
RAILCOLOR               equ #$02
PLATFORMCOLOR           equ #$04
PLAYFIELDSZ             equ #140
ENEMYHEIGHT             equ #17
SHOT_TIMING             equ #6
SPRITEINIT              equ #PLAYFIELDSZ+ENEMYHEIGHT+1
ME_VOL0                 equ #1      ; music engine volume event
ME_PITCH0               equ #2      ; music engine pitch event
ME_TONE0                equ #3      ; music engine tone event
ME_END                  equ #5      ; music engine end event

; variable assignments.

score           equ  $80    ; 80-81
p0x             equ  $82    ; screen x position of player 0 [20,132]
p0s             equ  $83    ; x speed of player 0 [-4,4]
p0vxlo          equ  $84    ; player 0 virtual x position low byte
temp1lo         equ  $85    ; temporary 16 bit var
temp1hi         equ  $86    ; temporary 16 bit var
temp2lo         equ  $87    ; temporary 16 bit var
temp2hi         equ  $88    ; temporary 16 bit var
temp3           equ  $89    ; temporary 8 bit var
radarColor      equ  $8a    ; Each bit represents whether or the
                            ; corresponding debris is radioactive, info
                            ; used to color the item differently
songPtrLo       equ  $8b    ; points to the current song position
songPtrHi       equ  $8c    ;
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
spriteramidx    equ  $c0    ; C0-C7
spriteram       equ  $c8    ; C8-D7 16 bytes of ram organized in 2 byte
                            ; chunks, each chunk containing display
                            ; data for a given debris sprite.
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
    lda #MODE_ATTRACT
    sta gameMode
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
    lda  #48                        ; Set the timer for 2812 cycles
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
    bcs do_end_wave                 ;
    cmp #MODE_IN_PROGRESS           ;
    beq process_gameMode_end        ;
    cmp #MODE_TRANSITION            ;
    bcs do_start_wave               ;
    jmp process_gameMode_end        ;

do_reset

    jsr non_system_init
    lda #MODE_TRANSITION
    sta gameMode
    lda #<song_loop_intro
    sta songPtrLo
    lda #>song_loop_intro
    sta songPtrHi
    lda #1
    sta musicEngineCount
    lda #0
    sta score
    sta score+1
    jmp process_gameMode_end

do_game_over

    lda #MODE_ATTRACT
    sta gameMode
    lda #0
    sta songPtrLo
    sta p0s

do_end_wave

   inc gameMode
   lda gameMode
   cmp #MODE_END_WAVE_TOP
   bne process_gameMode_end

   lda #MODE_TRANSITION
   sta gameMode
   jmp process_gameMode_end

do_start_wave

   inc gameMode
   lda gameMode
   cmp #MODE_TRANSITION_TOP
   bne process_gameMode_end

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


; music_engine
;
; process one-channel audio loops. events are in a 3 byte format:
;
;   first byte  :
;     bits 0-6  : event type
;   second byte : value
;   third byte  : duration
;
; event types:
;   set volume : set volume for a channel
;   set pitch  : set pitch for a channel
;   set tone   : set the tone for a channel
;   end        : end of loop, go back to start

    echo "------", [*], [* - $F000]d, "music_engine"

music_engine:

    dec musicEngineCount    ; 5     ; decriment the counter, if it is 0 load the next event
    beq music_next_event    ; 2-4   ; ""
    jmp music_engine_end    ; 3, 22 ; nothing to do this go around.

music_next_event            ;       ; load the next event

    ldy #0                  ; 2     ; start y at 0
    lda (songPtrLo),y       ; 5,    ; load the event type and store it in x
    tax                     ; 2

    iny                     ; 2     ; load second event parameter
    lda (songPtrLo),y       ; 5     ;

    cpx #ME_VOL0            ; 2     ; call the right event handler
    beq music_volume        ; 2-4   ; ""
    cpx #ME_PITCH0          ; 2     ; ""
    beq music_pitch         ; 2-4   ; ""
    cpx #ME_TONE0           ; 2     ; ""
    beq music_distortion    ; 2-4   ; ""
    cpx #ME_END             ; 2     ; ""
    beq music_end_event     ; 2-4   ; ""
do_audio_leap
    jmp music_engine_end    ; 3     ; unknown event! skip it
music_pitch
;    clc
;    sbc wave
;    adc #1
    sta AUDF0               ; 4
    jmp music_event_time    ; 3
music_volume
    sta AUDV0               ; 4
    jmp music_event_time    ; 3
music_distortion
    sta AUDC0               ; 4
    jmp music_event_time    ; 3
music_end_event
    bne music_stop          ; 3-4
    tax                     ; 2
    iny                     ; 2
    lda (songPtrLo),y       ; 5
    sta songPtrHi           ; 3
    stx songPtrLo           ; 3
    jmp music_next_event    ; 3

music_stop
    lda #0                  ; 2
    sta songPtrHi           ; 3
    jmp music_engine_end    ; 3

music_event_time
    iny                     ; 2
    lda (songPtrLo),y       ; 5
    sta musicEngineCount    ; 3
    tay                     ; 2
    jmp music_increment1    ; 3

music_increment
    ldy #0                  ; 2
music_increment1
    clc                     ; 2
    lda songPtrLo           ; 3
    adc #3                  ; 2
    sta songPtrLo           ; 3
    lda songPtrHi           ; 3
    adc #0                  ; 2
    sta songPtrHi           ; 3

    cpy #0                  ; 2
    bne music_engine_end    ; 3-4
    jmp music_next_event    ; 3

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

    ldx debrisCount                ; load the debris count into x
    cpx #0                         ; compare x to 0
    beq remove_stale_debris_end    ; if it = 0 then skip remove debris
    dex                            ; else decriment x so that it equals
                                   ; the lowest debris index

    lda debrisy,x                  ; load the debris x position into a
    cmp #4                         ; compare a to 4
    bcs remove_stale_debris_end    ; if a >= 4 then skip remove debris
    dec debrisCount                ; else decriment debrisCount


    lda debrisinfo,x               ; check if this is uncondemned
    and #%00011111                 ; radioactive  debris (011, 111).
    cmp #%00011000
    bne check_for_absorber         ; for an absorber
                                   ;

    lda #%10010111                 ; play radioactiveaudio
    sta effectState               ;

    lda radlevel                   ; update radlevel
    lsr                            ;
    clc                            ;
    adc #128                       ;
    sta radlevel                   ;

    cmp #$FF                       ; compare radlevel to FF
    bne remove_stale_debris_end    ; if not FF, then game continues
                                   ; else the game is over

    lda #MODE_GAME_OVER             ; set game gameMode to MODE_GAME_OVER
    sta gameMode                       ;

    lda #0                         ; set debrisCount to 0
    sta debrisCount                ;

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

    ; get the moveTable index and store it in y
    lda debrisinfo,x    ; 4
    lsr                 ; 2
    lsr                 ; 2
    lsr                 ; 2
    tay                 ; 2

; begin_horizontal_move
;
; move debris left or right. Degree of movement is detrermined by
; settings in the moveTable and by the current wave.

begin_horizontal_move

    lda frameCount             ; skip horizontal movent is a function
    and #%00000011             ; of or'ing the frame count with 10,
    ora #%00000010             ; and and'ing it with the current wave.
    and wave
    beq end_horizontal_move

    ; if the left move flag is set in moveTable for, move left
    lda moveTable,y
    and #%00100000
    bne move_debris_left

    ; if the right move flag is set in moveTable for, move right
    lda moveTable,y
    and #%01000000
    bne move_debris_right

    ; not left or right, skip horizontal movement
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

end_horizontal_move

; begin_vertical_move
;
; move debris downwards, according to current speed setting. Speed is
; impacted by two parameters: which frames are used to calulate moves
; and the speed adder.
; Frame mask:
;      00000000 : adjust position every frame
;      00000001 : adjust position every other frame
;      00000011 : adjust position every 4th fram
; Adder:
;      00000001 : adjust position by 1
;      00000010 : adjust position by 2 (only valid for "every frame")
;
; Note that this scheme provides for 4 possile vertical speeds.

begin_vertial_move

    lda wave
    lsr
    lsr
    and #%00000011
    cmp #0
    bne allFrames

    lda frameCount
    and #%00000001
    cmp #%00000000
    bne end_vertical_move
    jmp increment1Vertical

allFrames
    cmp #1
    beq increment1Vertical
    cmp #2
    beq increment2Vertical

    lda debrisy,x       ; "" ; load current debrix x position
    sec                 ; "" ; set the carry flag (needed?)
    sbc #%00000011      ;    ; subtract 3
    sta debrisy,x       ; "" ; update current debris x position
    jmp end_vertical_move


increment1Vertical

    lda debrisy,x       ; "" ; load current debrix x position
    sec                 ; "" ; set the carry flag (needed?)
    sbc #%00000001      ;    ; subtract 2
    sta debrisy,x       ; "" ; update current debris x position
    jmp end_vertical_move

increment2Vertical

    lda debrisy,x       ; ""        ; load current debrix x position
    sec                 ; ""        ; set the carry flag (needed?)
    sbc #%00000010      ;           ; subtract 2
    sta debrisy,x       ; ""        ; update current debris x position

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

    lda frameCount      ; 3         ; use framecount to choose which
    and #%00011100      ; 2         ; sprite animation frame to show.
    lsr                 ; 2         ; we use the memory location grabbed
    tay                             ; spritetable in "spriteselect"
    lda (temp2lo),y                 ; above to look up the sprite data.
    sta temp1lo                     ;
    iny                             ;
    lda (temp2lo),y                 ;
    sta temp1hi                     ;

    ldx temp3                       ; restore previously saved x
    txa                             ;

    ldx spritecount                 ; add this debris to the list of
    sta spriteramidx,x              ; sprites that will be drawn in
    lda temp1lo                     ; draw playfield
    sta spriteram,x                 ;
    lda temp1hi                     ;
    sta spriteram+8,x               ;

    inc spritecount                 ; tee up the next sprite

    ldx temp3                       ; store x in temp3

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

    lda #0              ; turn off the vblank
    sta VBLANK          ; ""
    sta WSYNC           ; wait for the next line.
    sta HMOVE           ; this HMOVE is paired with the init_ball routine above

vblank_end_end

; draw_score
;
; Draw the score

    echo "------", [*], [* - $F000]d, "main loop - draw score"

draw_score:

    lda #SCOREBGCOLOR   ; 2, 4
    sta COLUBK          ; 3, 7
    sta WSYNC           ; 3, -
    lda #1              ; 2, 2
    sta HMCLR           ; 3, 5
    sta NUSIZ0          ; 3, 8
    sta NUSIZ1          ; 3, 11
    lda #SCORECOLOR     ; 2, 13
    sta COLUP0          ; 3, 16
    sta COLUP1          ; 3, 19
    SLEEP 20            ; 20, 39
    sta RESP0           ; 3, 42
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
    lda #7              ; 2, 22 ;
    sta TIM64T          ; 3, 25 ;

; load the list of currently visible debris sprites
; into spriteList.
; TIME: 184 (max)

load_visible_sprites

    ldx #$FF            ; 2, 7       ; index
    ldy #0              ; 2, 9       ; for spritecount
    jmp lvs_skip        ; 3
lvs_loop                ; max loop time (when debriscount = 8) is 175 cycles
    lda debrisx,x       ; 4         ; if the x position is FF (off screen), then skip
    cmp #$FF            ; 2
    beq lvs_skip        ; 2/3
    stx spriteramidx,y ; 4
    iny                 ; 2
lvs_skip                ; + 1
    inx                 ; 2
    cpx debrisCount     ; 3
    bne lvs_loop        ; 3            ; LOOP TIME = 22 * 8 - 1 = 175
    sty spritecount     ; 3,  184      ; set spritecount

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
    ldx spriteramidx,y    ; 4, 54
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

    echo "------", [*], [* - $F000]d, "draw_top_line"

draw_top_line

    lda #$0c               ; 2, 2   ; line color between score and sky
    sta COLUBK             ; 3, 5   ;

    lda effectState
    cmp #%10000000          ; 2, 7   ; corresponding action
    bcs radioactiveEffect    ; 2, 8   ;
    cmp #%01000000               ; 2, 11  ;
    bcs absorberEffect       ; 2, 13  ;
    jmp resetBackground       ; 3, 18  ;

radioactiveEffect            ; 1, 9
    lda #$20               ; 2, 11
    jmp doneSetEffectColor ; 3, 14

absorberEffect               ; 1, 14
    lda #$80               ; 2, 16

doneSetEffectColor         ; 1, 19
    sta temp2hi            ; 3, 22
    lda effectState       ; 3, 25  ; if effectState is 0 no jump to
    beq resetBackground    ; 2, 27  ; resetBackground (done with effect)
    lsr                    ; 2, 29
    lsr                    ; 2, 31
    cmp #0                 ; 2, 33  ; if a is now zero go to set zero
    beq resetBackground    ; 2, 35
    clc                    ; 2, 37
    adc temp2hi            ; 3, 40
    jmp setBackground      ; 3, 43

resetBackground            ; 1, 36
    lda #SKYCOLOR               ; 2, 38

setBackground              ; 1, 44
    sta WSYNC              ; 2, 46
    sta COLUBK             ; 3, 49

end_draw_top_line

; DO NOT DELETE THIS COMMENT SECTION!!!!!!!!
;    jmp draw_playfield
;AdjustCodeOrg1
;    .byte #0
;    .byte #0

    echo "------", [*], [* - $F000]d, "draw_playfield"

draw_playfield
    lda #1              ; 2, 5
    sta temp3           ; 3, 8
    ldx #PLAYFIELDSZ    ; 2, 10
do_playfield            ; +1, [9,23]
    sec                 ; 2, 25 ; ---------------------------------------------
    txa                 ; 2, 27 ;
    sbc m1ys            ; 3, 30 ; draw missile 0, max 16 cycles
    adc #MISSILESIZE    ; 2, 32 ;
    lda #2              ; 2, 34 ;
    adc #$ff            ; 2, 36 ;
    sta ENAM0           ; 3, 39 ; ---------------------------------------------
    sec                 ; 2, 41
    txa                 ; 2, 43 ; ---------------------------------------------
    sbc m0ys            ; 3, 46 ; draw missile 1, max 16 cycles
    adc #MISSILESIZE    ; 2, 48 ;
    lda #2              ; 2, 50 ;
    adc #$ff            ; 2, 52 ;
    sta ENAM1           ; 3, 55 ; ---------------------------------------------
    txa                 ; 2, 57 ; ---------------------------------------------
    sec                 ; 2, 59 ; set up player1 - max  16 cycles
    sbc nexty           ; 3, 62 ;
    adc #ENEMYHEIGHT    ; 2, 64 ;
    bcc finishedSprite  ; 2, 66 ;
    tay                 ; 2, 68 ; 4 FREE CYCLES !!
    sta WSYNC           ; 3, 71 ; ---------------------------------------------
    lda (p1dataptrlo),y ; 6, 6  ; ---------------------------------------------
    sta GRP1            ; 3, 9  ; draw player 1 w/ colors - max 17 cyles
    lda (p1colorlo),y   ; 5, 14 ;
    sta COLUP1          ; 3, 17 ; ---------------------------------------------
    dex                 ; 2, 19 ;
    bne do_playfield    ; 2, 21 ;
    jmp draw_playfield_end ; 3, 24 ;
finishedSprite          ; +1, 67 ;
    cmp #$FF            ; 2, 69 ; we just finished the last line of a sprite
    beq load_next_sprite; 2, 71 ;
    sta WSYNC           ; 3, 74 ; 2 FREE CYCLES
    dex                 ; 2, 2  ;
    bne do_playfield    ; 2, 4  ;
    jmp draw_playfield_end ; 3, 7  ;
load_next_sprite        ; +1, 72
    sta WSYNC           ; 3, 75 ; 1 FREE CYCLE

    lda temp3           ; 3, 3  ;
    cmp spritecount     ; 3, 6  ;
    bcs no_more_sprites ; 2, 8 ;

    stx temp1lo         ; 3, 11 ; dynamically load sprite data
    tax                 ; 2, 13
    lda spriteram,x     ; 4, 17 ; ""
    sta p1dataptrlo     ; 3, 20 ; ""
    lda spriteram+8,x   ; 4, 24 ; ""
    sta p1dataptrhi     ; 3, 27 ; ""

    ldy #17             ; 2, 29 ;
    lda (p1dataptrlo),y ; 6*, 35 ;
    sta p1colorlo       ; 3, 38 ;
    iny                 ; 2, 40 ;
    lda (p1dataptrlo),y ; 6*, 46 ;
    sta p1colorhi       ; 3, 49 ;

    ldy temp3           ; 3, 52 ; get the next sprite from spriteramidx
    ldx spriteramidx,y ; 4, 56 ; ""
    inc temp3           ; 3, 59 ; ""
    lda debrisy,x        ; 4, 63 ; ""
    sta nexty           ; 3, 66 ; ""

    lda debrisx,x        ; 4, 70 ; setup A and X for do_sprite_move

    sta WSYNC
    sec                 ; 2, 2
DivideLoop1             ; This loop MAX 54 cycles if A is < 160, MIN 4
    sbc #15             ; 2, [4
    bcs DivideLoop1     ; 2/3 [6,56]
    tay                 ; 2, [8,58]
    lda FineAdjustTableEnd,Y    ; 5, [13,63]
    ldx #1
    sta HMP0,X          ; 3  [19,69]
    sta RESP0,x         ; 4  [23,73]
    sta WSYNC           ; 3  [26,76]
    sta HMOVE           ; 3, 3

    ldx temp1lo         ; 3, 12 ;
    dex                 ; 2, 14 ;
    dex                 ; 2, 16 ;
    dex                 ; 2, 18 ;
    jmp do_playfield    ; 2, 20 ; checking dex is not required here - we know there is at least one more sprite
no_more_sprites         ; +1, 9 ;
    lda #$ef            ; 2, 11
    sta nexty           ; 3, 14
    dex                 ; 2, 16 ;
    beq draw_playfield_end ; 2, 18 ;
    jmp do_playfield    ; 2, 20 ;
draw_playfield_end      ; [7,24]

;
; Lunar hills just above the Lunokhod. If there is a debris sprite that
; overlaps into this area it will be drawn.
;
    echo "------", [*], [* - $F000]d, "draw_hills"

draw_hills
    sta HMCLR           ; 3, 27 ; so HMOVE's will not reposition debris
    lda scrollpos       ; 3, 30
    and #%11111100      ; 2, 32
    clc                 ; 2, 34
    adc #4              ; 2, 36
    tax                 ; 2, 38
    lda #SNOWCOLOR      ; 2, 40
    sta COLUPF          ; 3, 43
    lda #$FF            ; 2, 45
    sta temp1lo         ; 3, 48 ; start the counter at FF (-1) this is
                                ; allows us to continue using skipdraw with
                                ; the current "nexty" for debris
hills_loop              ; +1, 48

    sec                 ; 2, 50 ; skipdraw
    sbc nexty           ; 3, 53 ;
    adc #ENEMYHEIGHT    ; 2, 55 ;
    bcs doDrawSprite    ; 2, 57 ;
    sta WSYNC           ;       ; no sprite
    sta HMOVE           ; 3, 3  ;
;    lda #0              ; 2, 5
;    sta GRP1            ; 3, 8
    lda PFData0-1,X     ; 4, 7
    sta PF0             ; 3, 10
    jmp overSprite      ; 4, 14
doDrawSprite            ; 1, 58 ; draw the sprite
    tay                 ; 2, 60
    sta WSYNC           ; 3, 63
    sta HMOVE           ; 3, 3
    lda PFData0-1,X     ; 4, 7
    sta PF0             ; 3, 10
    lda (p1dataptrlo),y ; 6, 16
    sta GRP1            ; 3, 19

overSprite              ; [14,19]
    lda PFData1-1,X     ; 4, 23
    sta PF1             ; 3, 26
    lda PFData2-1,X     ; 4, 30
    sta PF2             ; 3, 33

    dex                 ; 2, 35
    dec temp1lo         ; 5, 40
    lda temp1lo         ; 3, 43
    cmp #$FB            ; 2, 45
    bne hills_loop      ; 2, 47


draw_hills_end

;
; Draw the sled and rail
;

    echo "------", [*], [* - $F000]d, "draw_sled_and_rail"

draw_sled_and_rail

    lda #0              ; 2, 49
    sta GRP1            ; 3, 52  ; turn off player1 graphics
    sta ENAM0           ; 3, 55  ; turn off missiles
    sta ENAM1           ; 3, 58  ;

    sta WSYNC
    sta HMOVE           ; 3, 3 ;

    sta COLUPF          ; 3, 6  ; set pf color to black for HMOVE hiding
    sta PF0             ; 3, 9  ; clear playfield data
    sta PF1             ; 3, 12 ;
    sta PF2             ; 3, 15 ;

    lda #SNOWCOLOR      ; 2, 17 ; change the background color to white
    sta COLUBK          ; 3, 20 ; ""

    lda p0x             ; 3, 23
    ldx #0              ; 2, 25
    jsr do_sprite_move  ; -,
    ldx #10
do_sled
    lda PlayerSprite,X
    sta GRP0
    lda PlayerColor,X
    sta COLUP0
    sta WSYNC
    dex
    cpx #0
    bne do_sled

    lda #RAILCOLOR
    sta COLUBK
    lda PlayerSprite,X
    sta GRP0
    sta WSYNC

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

    lda radarColor      ; 3, 14
    asl                 ; 2, 16
    sta radarColor      ; 3, 19
    and #%10000000      ; 2, 21
    beq normalRadarColor; 2, 23
    lda #$1e             ; 2, 25
    jmp setColor        ; 3, 28
normalRadarColor        ; +1, 24
    lda #RADARCOLOR     ; 2, 26
    nop                 ; 2, 28
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

    lda radarColor      ; 3, 53
    asl                 ; 2, 55
    sta radarColor      ; 3, 58

    and #%10000000      ; 2, 60
    beq normalRadarColor2 ; 2, 62
    lda #$1e            ; 2, 64
    jmp setColor2       ; 3, 67
normalRadarColor2       ; +1, 63
    lda #RADARCOLOR     ; 2, 65
    nop                 ; 2, 67
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
    lda  #32          ; wait for 4544 cycles then sync
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
    sta CXCLR
do_collisions_end

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


    lda SWCHB                   ; check the reset switch is being
    and #%00000001              ; pressed
    bne do_joystick             ;
    lda #MODE_RESET             ;
    sta gameMode                ;
    jmp do_joystick_end

do_joystick
    lda gameMode
    cmp #255
    beq do_joystick_end
    cmp #254
    beq do_joystick_end
    cmp #0
    beq do_joystick_end

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
    lda frameCount
    and #%00000111
    bne do_joystick_end
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

    lda gameMode                    ; do not create debris if we are in
    cmp #MODE_IN_PROGRESS           ; the wrong gameMode
    beq ce_continue                 ;
    jmp create_debris_end           ;
ce_continue                         ;

    lda #40                         ; load waveLimit into a
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
    cmp #PLAYFIELDSZ-8              ; for new debris. todo: double this
    bcs create_debris_end           ; when pace bit not set

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

    lda frameCount                  ; create a random number for
    beq doEor                       ; horizontal positioning. TODO:
    asl                             ; would really like this to create
    beq noEor                       ; a repeatable pattern.
    bcc noEor
doEor
    eor #$1d
noEor                               ;
    sta debrisvx                    ;

    and #%00111000                  ; use the random number generated
    sta debrisinfo                  ; for position to set the debris
                                    ; info (type of sprite, fall
                                    ; direction)

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
    stx radarColor
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

    lda radarColor
    asl
    sta radarColor

    lda debrisinfo,x    ; skip the enmy if it has been condemned
    and #%00000001      ; ""
    cmp #%00000001      ; ""
    beq skip_debris     ; ""

    lda debrisinfo,x
    and #%00011000
    cmp #%00011000
    bne notRadioactive
    inc radarColor
notRadioactive

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
    cmp #1              ; 2, 7
    bcc off_screen      ; 2, 9
    cmp #160            ; 2, 11
    bcs off_screen      ; 2, 13
    rts                 ; 6, 19
off_screen              ; +1, 14
    lda #$FF            ; 2, 16
    rts                 ; 6, 22
convert_virtual_xpos_end

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

    echo "------", [*], [* - $F000]d, "moveTable"

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
    .byte #%00100001  ; 001 medium rock right
    .byte #%00100010  ; 010 small rock  right
    .byte #%00100001  ; 011 radioactive right
    .byte #%01000001  ; 100 medium rock left
    .byte #%00000001  ; 101 rock        down
    .byte #%01000010  ; 110 small       left
    .byte #%01000001  ; 111 radioactive left

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
    .byte #<sprite1Frame2
    .byte #>sprite1Frame2
    .byte #<sprite1Frame3
    .byte #>sprite1Frame3
    .byte #<sprite1Frame4
    .byte #>sprite1Frame4
    .byte #<sprite1Frame5
    .byte #>sprite1Frame5
    .byte #<sprite1Frame6
    .byte #>sprite1Frame6
    .byte #<sprite1Frame7
    .byte #>sprite1Frame7
    .byte #<sprite1Frame8
    .byte #>sprite1Frame8

sprite2FrameTable
    .byte #<sprite2Frame1
    .byte #>sprite2Frame1
    .byte #<sprite2Frame1
    .byte #>sprite2Frame1
    .byte #<sprite2Frame2
    .byte #>sprite2Frame2
    .byte #<sprite2Frame2
    .byte #>sprite2Frame2
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
    .byte #<sprite4Frame1
    .byte #>sprite4Frame1
    .byte #<sprite4Frame2
    .byte #>sprite4Frame2
    .byte #<sprite4Frame2
    .byte #>sprite4Frame2
    .byte #<sprite4Frame3
    .byte #>sprite4Frame3
    .byte #<sprite4Frame3
    .byte #>sprite4Frame3
    .byte #<sprite4Frame2
    .byte #>sprite4Frame2
    .byte #<sprite4Frame2
    .byte #>sprite4Frame2

sprite5FrameTable           ; right right
    .byte #<sprite5Frame1
    .byte #>sprite5Frame1
    .byte #<sprite5Frame1
    .byte #>sprite5Frame1
    .byte #<sprite5Frame2
    .byte #>sprite5Frame2
    .byte #<sprite5Frame2
    .byte #>sprite5Frame2
    .byte #<sprite5Frame3
    .byte #>sprite5Frame3
    .byte #<sprite5Frame3
    .byte #>sprite5Frame3
    .byte #<sprite5Frame2
    .byte #>sprite5Frame2
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
    .byte #<explosionSprite1
    .byte #>explosionSprite1
    .byte #<explosionSprite1
    .byte #>explosionSprite1
    .byte #<explosionSprite2
    .byte #>explosionSprite2
    .byte #<explosionSprite2
    .byte #>explosionSprite2
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
sprite1Frame2
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
    .byte #<boltColors2
    .byte #>boltColors2
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
sprite1Frame4
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
    .byte #<boltColors4
    .byte #>boltColors4
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
sprite1Frame6
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
    .byte #<boltColors6
    .byte #>boltColors6
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
sprite1Frame8
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
    .byte #<boltColors8
    .byte #>boltColors8

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

boltColors8
    .byte #$0e
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
    .byte #$12
    .byte #$12
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
boltColors6
    .byte #$0e
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
boltColors4
    .byte #$0e
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
boltColors2
    .byte #$0e
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
    .byte #$18
    .byte #$16
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
   .byte #%00111000
   .byte #%01000100
   .byte #%01000100
   .byte #%01000100
   .byte #%01000100
   .byte #%01000100
   .byte #%00111000
scoreOne
   .byte #%00111000
   .byte #%00010000
   .byte #%00010000
   .byte #%00010000
   .byte #%00010000
   .byte #%00110000
   .byte #%00010000
scoreTwo
   .byte #%01111100
   .byte #%00100000
   .byte #%00010000
   .byte #%00001000
   .byte #%00000100
   .byte #%01000100
   .byte #%00111000
scoreThree
   .byte #%00111000
   .byte #%01000100
   .byte #%00000100
   .byte #%00111000
   .byte #%00000100
   .byte #%01000100
   .byte #%00111000
scoreFour
   .byte #%00001000
   .byte #%00001000
   .byte #%00001000
   .byte #%01111100
   .byte #%01001000
   .byte #%01001000
   .byte #%01001000
scoreFive
   .byte #%00111000
   .byte #%01000100
   .byte #%00000100
   .byte #%00000100
   .byte #%01111000
   .byte #%01000000
   .byte #%01111100
scoreSix
   .byte #%00111000
   .byte #%01000100
   .byte #%01000100
   .byte #%01111000
   .byte #%01000000
   .byte #%01000100
   .byte #%00111000
scoreSeven
   .byte #%00100000
   .byte #%00100000
   .byte #%00010000
   .byte #%00001000
   .byte #%00000100
   .byte #%00000100
   .byte #%01111000
scoreEight
   .byte #%00111000
   .byte #%01000100
   .byte #%01000100
   .byte #%00111000
   .byte #%01000100
   .byte #%01000100
   .byte #%00111000
scoreNine
   .byte #%00111000
   .byte #%01000100
   .byte #%00000100
   .byte #%00111100
   .byte #%01000100
   .byte #%01000100
   .byte #%00111000

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


song_loop_intro

    .byte #ME_TONE0,    #8,     #0
    .byte #ME_PITCH0,   #0,     #0
    .byte #ME_VOL0,     #8,     #1
    .byte #ME_VOL0,     #0,     #24
    .byte #ME_TONE0,    #8,     #0
    .byte #ME_PITCH0,   #0,     #0
    .byte #ME_VOL0,     #8,     #1
    .byte #ME_VOL0,     #0,     #24
    .byte #ME_TONE0,    #8,     #0
    .byte #ME_PITCH0,   #0,     #0
    .byte #ME_VOL0,     #8,     #1
    .byte #ME_VOL0,     #0,     #24
    .byte #ME_TONE0,    #8,     #0
    .byte #ME_PITCH0,   #0,     #0
    .byte #ME_VOL0,     #8,     #1
    .byte #ME_VOL0,     #0,     #24
    .byte #ME_END,     #<song_loop, #>song_loop

song_loop

    .byte #ME_TONE0,    #6,     #0
    .byte #ME_PITCH0,   #29,    #0  ; 1
    .byte #ME_VOL0,     #14,    #11
    .byte #ME_VOL0,     #2,     #8
    .byte #ME_VOL0,     #0,     #17
    .byte #ME_TONE0,    #6,     #0
    .byte #ME_PITCH0,   #25,    #0  ; 1
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3

    .byte #ME_TONE0,    #6,     #0
    .byte #ME_PITCH0,   #25,    #0  ; 1
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3
    .byte #ME_TONE0,    #6,     #0
    .byte #ME_PITCH0,   #25,    #0  ; 1
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3

    .byte #ME_TONE0,    #6,     #0
    .byte #ME_PITCH0,   #25,    #0  ; 1
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #4
    .byte #ME_TONE0,    #6,     #0
    .byte #ME_PITCH0,   #22,    #0  ; 1
    .byte #ME_VOL0,     #12,    #3
    .byte #ME_VOL0,     #12,    #5
    .byte #ME_VOL0,     #2,     #5

    .byte #ME_TONE0,    #6,     #0
    .byte #ME_PITCH0,   #22,    #0  ; 1
    .byte #ME_VOL0,     #14,    #8
    .byte #ME_VOL0,     #2,     #8
    .byte #ME_VOL0,     #0,     #18

    .byte #ME_TONE0,    #6,     #0
    .byte #ME_PITCH0,   #18,    #0  ; 1
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3

    .byte #ME_TONE0,    #6,     #0
    .byte #ME_PITCH0,   #18,    #0  ; B3
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3
    .byte #ME_TONE0,    #6,     #0
    .byte #ME_PITCH0,   #16,    #0
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #3

    .byte #ME_TONE0,    #6,     #0
    .byte #ME_PITCH0,   #16,    #0  ; B4
    .byte #ME_VOL0,     #14,    #9
    .byte #ME_VOL0,     #2,     #7
    .byte #ME_VOL0,     #0,     #8

    .byte #ME_END,     #<song_loop, #>song_loop

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





