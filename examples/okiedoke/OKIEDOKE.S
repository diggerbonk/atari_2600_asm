     processor 6502

    include vcs.h

SCANLINE  = $80
KEYDELAY  = $81
COLUMN    = $82
GRHEIGHT  = $83
VALUE     = $84
MODE      = $86
IGNORE    = $87
BGCOLOR   = $88
DRAWROW   = $89
ROW       = $8A
ROWBIT    = $8B
GRID      = $8c ;  6 BYTES
GRTABLE   = $92 ; 12 BYTES
TEMPVAR   = $9E
CLICK     = $9F
MOVES     = $A0 ;  6 BYTES
LEVEL     = $A6
FIRE      = $A7
WINCOUNT  = $A8
SCROLL    = $A9
RAND1     = $AA
RAND2     = $AB
RAND3     = $AC
CHAR      = $AD
CRTABLE   = $AE ;  12 BYTES
LOGOCOLOR = $c0
NOTES     = $c1
DURATION  = $c5
GRID2     = $c6 ;   6 BYTES
vidmode   = $cc

    org  $f800

levels
    .byte  $ff,$ff,$ff,$ff,$ff,$ff
    .byte  $00,$50,$88,$88,$50,$00
    .byte  $50,$00,$00,$00,$00,$50
    .byte  $88,$50,$00,$00,$50,$88
    .byte  $00,$88,$88,$88,$88,$00
    .byte  $00,$00,$d8,$d8,$00,$00
    .byte  $50,$d8,$88,$88,$d8,$50
    .byte  $88,$50,$88,$88,$50,$88
    .byte  $f8,$88,$20,$20,$88,$f8
    .byte  $88,$00,$00,$00,$00,$88
    .byte  $50,$d8,$d8,$d8,$d8,$50
    .byte  $00,$20,$88,$88,$20,$00
    .byte  $88,$f8,$70,$70,$f8,$88
    .byte  $00,$00,$10,$10,$00,$00
    .byte  $00,$00,$00,$00,$10,$00
    .byte  $e0,$a0,$a0,$a0,$a8,$38
    .byte  $00,$08,$10,$20,$00,$00
    .byte  $30,$48,$a0,$48,$80,$08
    .byte  $10,$00,$80,$00,$40,$10
    .byte  $80,$ff,$90,$01,$90,$90
    .byte  $c0,$38,$c0,$28,$50,$28
    .byte  $38,$68,$b0,$48,$30,$40
    .byte  $f8,$f0,$f8,$f0,$f8,$f0
    .byte  $a8,$10,$a0,$08,$00,$08
    .byte  $d8,$f8,$e8,$98,$78,$f8
    .byte  $00,$00,$48,$10,$00,$50
    .byte  $c8,$50,$40,$a0,$10,$80
    .byte  $e0,$00,$c8,$30,$c0,$30
    .byte  $50,$01,$30,$80,$01,$81
    .byte  $10,$b0,$80,$18,$80,$78
    .byte  $00,$00,$00,$00,$00,$00
logo1
    .byte  $00,$89,$8b,$92,$e1,$88,$8b,$f7,$03,$00,$00
logo2
    .byte  $00,$c9,$09,$89,$dd,$88,$00,$ff,$00,$80,$00
logo3
    .byte  $00,$06,$09,$a9,$46,$01,$7e,$80,$00,$00,$00
logo4
    .byte  $00,$14,$2a,$49,$41,$80,$00,$00,$00,$00,$00
logo5
    .byte  $00,$35,$49,$49,$39,$80,$00,$00,$00,$00,$00
logo6
    .byte  $00,$07,$0c,$aa,$47,$00,$00,$00,$00,$00,$00

    org  $f900

off1  ds  16,0

on
;smiley
      .byte $00,$38,$7c,$6c,$d6,$ba,$ba,$fe,$fe,$d6,$7c,$7c,$38,$00,$00,$2a
;cat
      .byte $7c,$82,$6c,$ee,$aa,$82,$ba,$44,$38,$7c,$ee,$d6,$7c,$c6,$82,$ec

;check
      .byte $18,$38,$28,$68,$c8,$88,$04,$04,$04,$04,$02,$02,$02,$01,$01,$42

;plus
      .byte $38,$38,$28,$28,$ee,$ee,$82,$82,$ee,$ee,$28,$28,$38,$38,$00,$48

;diamond
      .byte $10,$10,$28,$28,$54,$54,$ba,$ba,$54,$54,$28,$28,$10,$10,$00,$AC

;key
      .byte $10,$28,$2c,$2c,$28,$28,$2c,$28,$38,$7c,$f6,$f6,$f6,$6c,$38,$0E

;turtle
      .byte $00,$82,$92,$ba,$6c,$54,$6c,$54,$6c,$ba,$92,$aa,$28,$10,$00,$C2

;spaceman
      .byte $44,$ee,$44,$44,$82,$82,$54,$7c,$6c,$fe,$ba,$7c,$38,$00,$00,$62

crsgrp
    .byte  $7c,$38,$10         ; Pattern for cursor

incchar                        ; increment random number for token
    lda  RAND3
    clc
    adc  #$10
    sta  RAND3
    cmp  #$90
    bne  charok
    lda  #$10
    sta  RAND3
charok
    rts

crdr                           ; cursor drawing routine
    lda  #$01
    sta  VDELP0
    sta  VDELP1
crdr2
    ldy  GRHEIGHT
    lda  (CRTABLE),y           ; get player0 copy1 data
    sta  GRP0
    sta  WSYNC
    lda  (CRTABLE+$2),y        ; get player1 copy1 data
    sta  GRP1
    lda  (CRTABLE+$4),y        ; get player0 copy2 data
    sta  GRP0
    lda  (CRTABLE+$6),y        ; get player1 copy2 data
    sta  TEMPVAR
    lda  (CRTABLE+$8),y        ; get player0 copy3 data
    tax
    lda  (CRTABLE+$A),y        ; get player1 copy3 data
    tay
    lda  TEMPVAR
    sta  GRP1
    stx  GRP0
    sty  GRP1
    sta  GRP0
    dec  GRHEIGHT
    bpl  crdr2                 ; loop until done
    lda  #$0
    sta  VDELP0
    sta  VDELP1
    sta  GRP1
    sta  GRP0
    sta  GRP1
    rts

drw1
    lda  SCANLINE              ; adjust scanline count
    sbc  GRHEIGHT
    sta  SCANLINE
    rts
                               ; clears the counter
clrscore
    ldy  #$5
cloop
    lda  #$0
    sta  MOVES,y
    sta  GRID2,y
    dey
    bpl  cloop
    rts

pushed                         ; set up ROWBIT for updating grid
    tya
    clc
    sbc  #$0b
    tax
    lda  GRID,x
    eor  ROWBIT
    sta  GRID,x
    rts

    org   $fa00

winpat                         ; scrolling "You Win!" message
    .byte  $00,$00,$00,$00,$00,$00,$80,$40,$38,$40,$80,$00,$70,$88,$88,$70
    .byte  $00,$f0,$08,$08,$f0,$00,$00,$e0,$18,$30,$18,$e0,$00,$88,$f8,$88
    .byte  $00,$f8,$40,$20,$10,$f8,$00,$e8,$00,$00,$00,$00,$00

playpat                        ; scrolling "Select Level" message
    .byte  $00,$00,$00,$00,$00,$00,$48,$a8,$a8,$90,$00,$f8,$a8,$88,$88,$00
    .byte  $f8,$08,$08,$08,$00,$f8,$a8,$88,$88,$00,$f8,$88,$88,$88,$00,$80
    .byte  $80,$f8,$80,$80,$00,$00,$f8,$08,$08,$08,$00,$f8,$a8,$88,$88,$00
    .byte  $c0,$30,$08,$30,$c0,$00,$f8,$a8,$88,$88,$00,$f8,$08,$08,$08,$08
    .byte  $00,$00,$00,$00,$00
curs                           ; Sets CRTABLE variable to point to cursor
    ldx  #$b
curs1
    lda  >#off1
    sta  CRTABLE,x
    dex
    lda  <#off1
    sta  CRTABLE,x
    dex
    bpl curs1
    lda  COLUMN
    clc
    sbc  #$b
    clc
    rol
    tax
    lda  <#crsgrp
    sta  CRTABLE,x
    rts

movepat                        ; Sets grid up for current level pattern
    lda  #>levels
    sta  GRTABLE+1
    sty  GRTABLE
    ldy  #$5
resgrid
    lda  (GRTABLE),y
    sta  GRID,y
    dey
    bpl  resgrid
    rts

eorgrid                        ; Combines 2 preset puzzles to make random one
    lda  #>levels
    sta  GRTABLE+1
    lda  RAND2
    sta  GRTABLE
    ldy  #$5
ep1
    lda  (GRTABLE),y
    eor  GRID,y
    sta  GRID,y
    dey
    bpl  ep1
    rts

BEGIN
    sei
    cld
    ldx  #$ff                  ; Actual start of ROM image
    txs
    inx
    txa
    ldy  #$10
clear
    sta  $0,x                  ; Clears TIA regs and RAM (Thanks Dan!)
    dex
    bne  clear
    dey
    bne  clear
    lda  #$c
    sta  COLUMN                ; Reset column
    sta  AUDC0
    lda  #$10
    sta  RAND3
    sta  CHAR
    jsr  clrscore
    lda  #$3
    sta  MODE
    jsr  incscore
Start
    dec  LOGOCOLOR
    jsr  curs
    ldx  #$ff
    lda  MODE
    cmp  #$1
    bne  nofire
    ldx  INPT4
nofire
    stx  FIRE
    dec  CLICK
    bpl  nosound
    lda  #$00
    sta  AUDV0
nosound
    lda  #$57
    STA  WSYNC                 ; wait for the horizontal sync
    STA  VBLANK                ; start vert blanking, enable I4 & I5 latches
    STA  VSYNC                 ; start vertical retrace
    LDA  #$2A
    STA  TIM8T                 ; set timer for correct length
Loop4
    LDY  INTIM
    BNE  Loop4                 ; waste some time
    STY  WSYNC                 ; wait for the horizontal sync
    STY  VSYNC                 ; stop vertical retrace period
    LDA  #$24
    STA  TIM64T                ; set timer for the big wait

; This is the main kernal of the logic for the game

;    jsr  $f000
    lda  MODE
    cmp  #$2		
    bmi  readcons              ; if MODE = 3, then scroll intro message
    bne  intro                 ; if MODE = 2, then scroll win message
    jsr  playsong              ; play the winning tune
    inc  BGCOLOR               ; change the background color
intro
    jsr  winner                ; call the scroll routine

readcons
    lda  SWCHB                 ; read the console switches
    ror
    bcs  select                ; check Reset if not pressed go check Select
    lda  RAND3                 ; get random token
    sta  CHAR
    ldx  #$0                   ; mute audio channel 1
    stx  AUDV1
    stx  BGCOLOR
    jsr  incrand               ; increase RAND1
    inx
    stx  MODE                  ; set mode to PLAY
    lda  LEVEL
    cmp  #$b4                  ; Level 31?
    bne  rc1                   ; nope, don't create random level
    lda  RAND1
    cmp  RAND2                 ; if RAND1 and RAND2 are equal, increase RAND1
    bne  rc3                   ; otherwise we end up with a blank puzzle!
    dex
    jsr  incrand
rc3 jsr  clrscore              ; reset counter
    ldy  RAND1
    jsr  movepat
    jsr  eorgrid               ; create random puzzle
    jmp  j0
rc1 jsr  clrscore              ; reset counter
    ldy  LEVEL
    jsr  movepat               ; move proper level to grid
    jmp  j0
select
    tay
    ldx  #$1
    jsr  incrand               ; increase RAND2
    jsr  incchar               ; increase RAND3
    tya
    ror                        ; Check Select switch
    bcs  js                    ; if not pressed go check joystick
    lda  KEYDELAY              ; pressed, is the keydelay set?
    bne  j0                    ; yeah, oh well, maybe next time
    inc  KEYDELAY              ; nope, activate keydelay
    lda  MODE                  ; are we already in select mode?
    beq  selmode               ; yes, forget select mode setup
    lda  #$0                   ; no, setup select mode
    sta  MODE
    sta  BGCOLOR               ; set background to black
    sta  AUDV1
    lda  #$fa                  ; set level to $fa which will be inced to $00
    sta  LEVEL
    jsr  clrscore              ; clear counter
selmode
    lda  LEVEL                 ; inc level by 6 (# of bytes per puzzle)
    clc
    adc  #$6
    cmp  #$ba                  ; did we hit level 32 yet?
    bne  notlast               ; nope, don't set level to 1
    jsr  clrscore              ; yep, set level to 1
    lda  #$0
notlast
    sta  LEVEL
    tay
    jsr  movepat               ; move level pattern to grid
    jsr  incscore              ; increment counter
    jmp  Loop3                 ; start drawing the screen
js
    ldy  #$f0                  ; load dummy into y incase not in play mode
    lda  MODE                  ; Are we in PLAY mode?
    cmp  #$1
    bne  nojs                  ; nope, don't read joystick
    lda  SWCHA                 ; read joystick
    and  #$f0
    tay
nojs
    cpy  #$f0                  ; if bits 4-7 = 1 in SWCHA, js not moved
    bne  j0
    lda  FIRE                  ; see if fire button was pressed
    bpl  j0                    ; yes, go check keydelay
    lda  #$0                   ; nope, clear keydelay
    sta  KEYDELAY              ; if nothing pressed, reset keydelay
    jmp  b10                   ; b10 too far away for a beq :(
j0
    ldx  KEYDELAY
    inc  KEYDELAY              ; increment keydelay
    cpx  #$00                  ; was old keydelay 0?
    beq  joa                   ; yeah, lets check the fire button again
    cpx  #$10                  ; nope, was old keydelay $10?
    beq  job                   ; yeah, o.k. we can check the fire button
jjj jmp  Loop3                 ; nope, forget the fire button, draw screen
job lda  #$0
    sta  KEYDELAY              ; reset keydelay
    beq  jjj                 
joa lda  FIRE                  ; check fire button again
    bpl  b6                    ; ahh, it's been pushed, let's go do something
    tya                        ; get joystick reading from y
    asl                        ; check the "right" bit
    bcs  j2                    ; nope, go check "left" bit
    ldy  COLUMN                ; move cursor right
    iny
    cpy  #$12                  ; did we move past the last column?
    bne  j1                    ; nah, we're ok
    ldy  #$c                   ; yep, reset to first column
j1  sty  COLUMN
j2  asl                        ; check the "left" bit
    bcs  j4                    ; nope, go check "down" bit
    ldy  COLUMN                ; move cursor left
    dey
    cpy  #$b                   ; did we go too far left?
    bne  j3                    ; nope
    ldy  #$11                  ; yeah, set pointer to column 6
j3  sty  COLUMN
j4
    asl                        ; check the "down" bit
    bcs  j5                    ; nope, go check "up" bit
    inc  ROW                   ; move cursor down
    lda  ROW
    cmp  #$5                   ; did we go too far down?
    bne  b6                    ; nope
    lda  #$0                   ; yeah, set row to top row
    sta  ROW
    jmp  b6
j5  asl                        ; check the "up" bit
    bcs  b6                    ; nope, check the fire button
    dec  ROW                   ; move pointer up
    bpl  b6                    ; did we go too far up? nope!
    ldy  #$4                   ; yep! set row to bottom row
    sty  ROW
b6
    lda  FIRE                  ; check for fire button
    and  #$80
    bne  b10                   ; not pressed, draw screen
    jsr  incscore              ; pressed, increase "moves" counter
b6a
    ldx  ROW                   ; invert "plus" pattern
    inx
    lda  eorpat,x
    sta  ROWBIT
    ldy  COLUMN
    jsr  pushed
    ldx  ROW
    inx
    lda  bitpat,x
    sta  ROWBIT
    jsr  pushed2
    iny
    cpy  #$6
    beq  ps1
    jsr  pushed
ps1 dey
    dey
    bmi  b10
    jsr  pushed
    ldy  #$5
    sty  TEMPVAR
checkwin                       ; routine to see if puzzle is solved
    lda  GRID,y
    and  #$f8
    beq  colok
    lda  #$0
    sta  TEMPVAR
colok
    dey
    bpl  checkwin
    lda  TEMPVAR
    beq  keepon                ; no, stay in PLAY mode
    lda  #$2                   ; YES! the puzzle is solved
    sta  MODE
    lda  #$0a                  ; set number of notes to $a
    sta  NOTES
    sta  DURATION              ; set duration for small pause befor song
    lda  #$4                   ; set type of tone to play on aud channel 1
    sta  AUDC1
    lda  #$0                   ; reset scrolling message counter
    sta  SCROLL
    sta  BGCOLOR               ; set background color to black
keepon

b10

Loop3
    lda  BGCOLOR               ; start of screen rendering routine
    sta  COLUBK                ; set background color
Loop3a
    LDY  INTIM
    BNE  Loop3a                ; waste time
    STY  WSYNC                 ; wait for horizontal sync
    ldy  #$04
    STY  VBLANK                ; end vertical blanking
    sty  WSYNC
    lda  #$85                  ; set number of scanlines
    sta  SCANLINE
    sty  WSYNC
    lda  #$00                  ; clear background
    sta  PF0
    sta  PF1
    sta  PF2
    sta  REFP0
    sta  HMP1                  ; move player 1 right 7 pixels

loop6
    stx  WSYNC
    ldx  SCANLINE
    ldy  #$0
    sty  DRAWROW               ; reset drawrow variable
    cpx  #$75                  ; top of screen?
    bne  tb1                   ; no, get outta here!
    lda  #$84                  ; set playfield color
    sta  COLUPF
    lda  #$1                   ; set playfield control
    sta  CTRLPF
    lda  BGCOLOR               ; set token color to background color
    sta  COLUP0
    sta  COLUP1

okie                           ; copies addrs to zero-page for RetroWare
    ldx  #$0b
okie7
    lda  oktbl,x
    sta  GRTABLE,x
    dex
    bpl  okie7
    jsr  drawit                ; set up draw routine
    lda  #$ff
    sta  PF2
    ldy  #$6                   ; set height of graphic to draw
    sty  GRHEIGHT
    lda  #$18                  ; set color for graphic
    sta  COLUP0
    sta  COLUP1
    jsr  loop2a                ; go draw "Okie Dokie"
    ldx  #$71
    stx  SCANLINE              ; fudge scanline #
tb1
    cpx  #$71                  ; top of grid?
    bne  tb2                   ; nope, get outta here!
    lda  #$01                  ; yeah, set side borders for puzzle
    sta  PF2
    lda  #>off1                ; put high-byte of graphics in GRTABLE
    ldy  #$d
tb1a
    sta  GRTABLE,y
    dey
    dey
    bpl  tb1a
    jmp  b4
tb2 iny
    cpx  #$70                  ; if at scanline #$70 set up row 1
    beq  b0
    iny
    cpx  #$60                  ; set up row 2
    beq  b0
    iny
    cpx  #$50                  ; set up row 3
    beq  b0
    iny
    cpx  #$40                  ; set up row 4
    beq  b0
    iny
    cpx  #$30                  ; set up row 5
    beq  b0
    iny
    cpx  #$20                  ; set up row 6
    bne  b4
    sty  DRAWROW               ; set up and draw "RetroWare"
    lda  #$ff
    sta  PF2
    jsr  name
    lda  #$8
    sta  GRHEIGHT
    jsr  loop2a
    lda  #$0
    sta  PF2
    jsr  mv
    ldy  #$9
    bne  bb
b0  sty  DRAWROW
    jsr  code
    ldy  #$e
bb  sty  GRHEIGHT
    ldx  CHAR
    lda  on-1,x                ; get token color and set it
    sta  COLUP0
    sta  COLUP1
    jsr  drw1
    jsr  loop2a                ; actually draw token row
b1
    lda  BGCOLOR
    ldx  DRAWROW
    DEX
    cpx  ROW                   ; should we draw the cursor?
    bne  bb3                   ; nope (we still draw it but in bg color)
    lda  #$43                  ; set cursor color
bb3
    sta  COLUP0
    sta  COLUP1
    ldy  #$2                   ; set up cursor height
    sty  GRHEIGHT
    jsr  crdr                  ; actually draw cursor
b4  dec  SCANLINE              ; are we done with the screen yet?
    beq  b4a                   ; YES! Thank God! let's start over.
    jmp  loop6                 ; nope, go to the next scanline
b4a
    jmp  Start


drawit
    lda  #$03                  ; set both players to 3 copies
    sta  NUSIZ0
    sta  NUSIZ1
    ldx  #$6                   ; move players 12 columns over
    ldy  #$0
    sta  WSYNC                 ; wait for scanline
loop1
    dex                        ; wait for column (15 bit wide) x
    bpl  loop1
    nop                        ; additional delay
    sta  RESP0                 ; reset player 0
    sta  RESP1                 ; reset player 1
    lda  #$d0                  ; set player 0 to move left 1 pixel
    sta  HMP0
    lda  #$e0
    sta  HMP1
    sta  WSYNC
    sta  HMOVE                 ; move player 0

loop2a
    lda  #$01
    sta  VDELP0
    sta  VDELP1
loop2
    ldy  GRHEIGHT
    lda  (GRTABLE),y           ; get player0 copy1 data
    sta  GRP0
    sta  WSYNC
    lda  (GRTABLE+$2),y        ; get player1 copy1 data
    sta  GRP1
    lda  (GRTABLE+$4),y        ; get player0 copy2 data
    sta  GRP0
    lda  (GRTABLE+$6),y        ; get player1 copy2 data
    sta  TEMPVAR
    lda  (GRTABLE+$8),y        ; get player0 copy3 data
    tax
    lda  (GRTABLE+$A),y        ; get player1 copy3 data
    tay
    lda  TEMPVAR
    sta  GRP1
    stx  GRP0
    sty  GRP1
    sta  GRP0
    dec  GRHEIGHT
    bpl  loop2                 ; loop until done
    lda  #$0
    sta  VDELP0
    sta  VDELP1
    sta  GRP1
    sta  GRP0
    sta  GRP1
    rts

okie1                          ; Graphics for "Okie Dokie"
    .byte  $00,$71,$89,$89,$89,$89,$70
okie2
    .byte  $00,$29,$4b,$8a,$41,$28,$00
okie3
    .byte  $00,$c0,$00,$80,$c0,$00,$00
okie4
    .byte  $00,$f0,$89,$89,$88,$88,$f0
okie5
    .byte  $00,$e4,$15,$16,$e5,$04,$00
okie6
    .byte  $00,$a7,$2c,$2a,$07,$a0,$00

table2                         ; Address table for "RetroWare"
    .byte  <logo1
    .byte  >logo1
    .byte  <logo2
    .byte  >logo2
    .byte  <logo3
    .byte  >logo3
    .byte  <logo4
    .byte  >logo4
    .byte  <logo5
    .byte  >logo5
    .byte  <logo6
    .byte  >logo6

name                           ; copies addresses to zero-page for RetroWare
    lda  LOGOCOLOR
    sta  COLUP0
    sta  COLUP1
    ldx  #$0b
loop7
    lda  table2,x
    sta  GRTABLE,x
    dex
    bpl  loop7
    rts

pushed2
    tya
    clc
    sbc  #$0b
    tax
    lda  GRID2,x
    eor  ROWBIT
    sta  GRID2,x
    rts

;bit-mapped graphics of digits

    org  $fe00

d0    .byte  $00,$00,$38,$44,$44,$44,$44,$44,$44,$38,$0,$0,$0,$0,$0,$0
d1    .byte  $00,$00,$10,$10,$10,$10,$10,$10,$30,$10,$0,$0,$0,$0,$0,$0
d2    .byte  $00,$00,$7c,$40,$40,$30,$08,$04,$44,$38,$0,$0,$0,$0,$0,$0
d3    .byte  $00,$00,$38,$44,$44,$04,$18,$04,$44,$38,$0,$0,$0,$0,$0,$0
d4    .byte  $00,$00,$04,$04,$7e,$44,$24,$14,$0c,$04,$0,$0,$0,$0,$0,$0
d5    .byte  $00,$00,$38,$44,$44,$04,$78,$40,$40,$7c,$0,$0,$0,$0,$0,$0
d6    .byte  $00,$00,$38,$44,$44,$78,$40,$40,$44,$38,$0,$0,$0,$0,$0,$0
d7    .byte  $00,$00,$20,$20,$10,$10,$08,$08,$04,$78,$0,$0,$0,$0,$0,$0
d8    .byte  $00,$00,$38,$44,$44,$44,$38,$44,$44,$38,$0,$0,$0,$0,$0,$0
d9    .byte  $00,$00,$38,$44,$04,$3c,$44,$44,$44,$38,$0,$0,$0,$0,$0,$0
dl    .byte  $00,$fc,$80,$80,$80,$80,$80,$80,$80,$80,$0,$0,$0,$0,$0,$0
dv    .byte  $00,$10,$28,$28,$44,$44,$82,$82,$82,$82,$0,$0,$0,$0,$0,$0
off    ds 16,0

code
    sta  WSYNC
    ldx  DRAWROW
    lda  bitpat,x
    sta  ROWBIT

    ldx  #$0d                  ; sets up addresses
    ldy  #$6                   ; for drawing the row of tokens
code1
; LOOK HERE!
;    lda  GRID,y
    lda  INPT5
    bpl  showsol
    lda  GRID,y
    jmp  showpf
showsol
    lda  GRID2,y
showpf
    and  ROWBIT
    bne  cd2
    lda  #<off1
    beq  cd3
cd2 lda  CHAR
cd3 sta  GRTABLE-1,x
    dex
    dex
    dey
    bpl  code1
    rts

mv                             ; sets up addresses in zero-page locations
    ldx  #$0b                  ; for drawing the 6-digit code for displaying
    ldy  #$5                   ; the number of moves executed.
mv1
    lda  #$fe
    sta  GRTABLE,x
    lda  MOVES,y
    sta  GRTABLE-1,x
    dex
    dex
    dey
    bpl  mv1
    lda  #<off
    sta  GRTABLE
    ldy  MODE
    bne  mv2
    lda  #<dl
    sta  GRTABLE
    sta  GRTABLE+4
    lda  #<dv
    sta  GRTABLE+2
mv2 lda  #<off
    sta  GRTABLE+10
    cpy  #$3
    bne  mv3
    sta  GRTABLE+2
    sta  GRTABLE+4
    sta  GRTABLE+6
    sta  GRTABLE+8
mv3 rts

bitpat                         ; bit look up table
   .byte $00,$80,$40,$20,$10,$08
eorpat                         ; inverting look up table
   .byte $00,$c0,$e0,$70,$38,$1c

winner                         ; scrolling message routine
    inc  WINCOUNT
    lda  WINCOUNT
    cmp  #$8
    bne  endwin
    lda  #$0
    sta  WINCOUNT
    lda  #>playpat
    sta  GRTABLE+1
    lda  #<playpat
    ldy  MODE
    cpy  #$3
    beq  play
    lda  #>winpat
    sta  GRTABLE+1
    lda  #<winpat
play
    clc
    adc  SCROLL
    sta  GRTABLE
    ldy  #$5
reswin
    lda  (GRTABLE),y
    sta  GRID,y
    dey
    bpl  reswin
    inc  SCROLL
    lda  #$27
    ldy  MODE
    cpy  #$2
    beq  win
    lda  #$40
win cmp  SCROLL
    bne  endwin
    lda  #$0
    sta  SCROLL
endwin
    rts

incscore                       ; increment counter
    lda  #$ff
    sta  AUDV0
    lda  #$10
    sta  CLICK
    sed
    ldx  #$4
digloop
    lda  MOVES,x
    clc
    adc  #$10
    sta  MOVES,x
    cmp  #$0
    bne  isexit
    dex
    cpx  #$00
    bne  digloop
isexit
    cld
    rts

incrand                        ; increment random number
    lda  RAND1,x
    clc
    adc  #$6
    sta  RAND1,x
    cmp  #$b4
    bne  randok
    lda  #$0
    sta  RAND1,x
randok
    rts

playsong                       ; routine to play a song
    lda  NOTES
    bmi  songoff
    dec  DURATION
    lda  DURATION
    cmp  #$4
    beq  songoff
    cmp  #$0
    bne  songexit
    dec  NOTES
    bmi  songexit
    lda  #$8
    sta  AUDV1
    ldx  NOTES
    lda  dur,x
    sta  DURATION
    lda  freq,x
    sta  AUDF1
songexit
    rts
songoff
    lda  #$0
    sta  AUDV1
    rts

oktbl                          ; address table for "Okie Dokie"
    .byte  <okie1
    .byte  >okie1
    .byte  <okie2
    .byte  >okie2
    .byte  <okie3
    .byte  >okie3
    .byte  <okie4
    .byte  >okie4
    .byte  <okie5
    .byte  >okie5
    .byte  <okie6
    .byte  >okie6

dur                            ; duration values for song notes
    .byte $20,$09,$09,$20,$18,$18,$18,$18,$09,$09
freq                           ; frequency values for song notes
    .byte $0e,$10,$12,$14,$11,$10,$11,$10,$12,$13

;    dc.b $a9,"RetroWare"       ; copyright notice

    org  $fffc
    .byte  <BEGIN              ; set ROM entry point
    .byte  >BEGIN
    .byte  <BEGIN              ; set INTERRUPT vector
    .byte  >BEGIN
