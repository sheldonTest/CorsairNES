.include "constants.inc"
.include "inesheader.inc"
.include "initnes.inc"
.include "utils.inc"
.include "actors.inc"
.include "state.inc"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables declared in RAM zero-page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "ZEROPAGE"
Buttons:        .res 1       ; Pressed buttons (A|B|Sel|Start|Up|Dwn|Lft|Rgt)
PrevButtons:    .res 1      ; Keep track of button pressed from previous frame

XPos:           .res 1       ; Player X 16-bit position (8.8 fixed-point): hi+lo/256px
YPos:           .res 1       ; Player Y 16-bit position (8.8 fixed-point): hi+lo/256px

XVel:           .res 1       ; Player X (signed) velocity (in pixels per 256 frames)
YVel:           .res 1       ; Player Y (signed) velocity (in pixels per 256 frames)

Frame:          .res 1       ; Counts frames (0 to 255 and repeats)
IsDrawComplete: .res 1       ; Flag to indicate when VBlank is done drawing
Clock60:        .res 1       ; Counter that increments per second (60 frames)

BgPtr:          .res 2       ; Pointer to background address - 16bits (lo,hi)
SprPtr:         .res 2       ; Pointer to the sprite address - 16bits (lo,hi)
PalPtr:         .res 2       ;Point to the first address of the color palette 16-bit (lo,hi)

XScroll:        .res 1       ; Store the horizontal scroll position
CurrNametable:  .res 1       ; Store the current starting nametable (0 or 1)
Column:         .res 1       ; Stores the column (of tiles) we are in the level
NewColAddr:     .res 2       ; The destination address of the new column in PPU
SourceAddr:     .res 2       ; The source address in ROM of the new column tiles

ParamType:      .res 1       ; Used as parameter to subroutine
ParamXPos:      .res 1       ; Used as parameter to subroutine
ParamYPos:      .res 1       ; Used as parameter to subroutine
ParamTileNum:   .res 1       ; Used as parameter to subroutine
ParamNumTiles:  .res 1       ; Used as parameter to subroutine
ParamAttribs:   .res 1       ; Used as parameter to subroutine
PreviusOAMCnt: .res 1      ;Sore the previous number of bytes sent to OAM
PreviousSubTime: .res 1    ; Store the seconds since a sub was spawned
PreviousJetTime: .res 1

Seed:           .res 2      ;Seed for 16-bit LFSR
Collision:      .res 1      ; 8-bit flag to check if a collision happened

ActorsArray:    .res MAX_ACTORS * .sizeof(Actor)

ParamRectX1:    .res 1      ;Bounding box X1 point
ParamRectX2:    .res 1      ;Bounding box X2 point
ParamRectY1:    .res 1      ;Bounding box Y1 point
ParamRectY2:    .res 1      ;Bounding box Y2 point

GameState:      .res 1
MenuSelect:       .res 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRG-ROM code located at $8000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "CODE"

.proc ReadControllers
    lda #1                   ; A = 1
    sta Buttons              ; Buttons = 1
    sta CTRL1              ; Set Latch=1 to begin 'Input'/collection mode
    lsr                      ; A = 0
    sta CTRL1              ; Set Latch=0 to begin 'Output' mode
LoopButtons:
    lda CTRL1              ; This reads a bit from the controller data line and inverts its value,
                             ; And also sends a signal to the Clock line to shift the bits
    lsr                      ; We shift-right to place that 1-bit we just read into the Carry flag
    rol Buttons              ; Rotate bits left, placing the Carry value into the 1st bit of 'Buttons' in RAM
    bcc LoopButtons          ; Loop until Carry is set (from that initial 1 we loaded inside Buttons)
    rts
.endproc

.proc LoadSinglePalette
    PPU_SETADDR $3F00
    ldy #0                   ; Y = 0
:   lda (PalPtr),y        ; Lookup byte in ROM
    sta PPU_DATA             ; Set value to send to PPU_DATA
    iny                      ; Y++
    cpy #32                  ; Is Y equal to 32?
    bne :-                   ; Not yet, keep looping
    rts                      ; Return from subroutine
.endproc

.proc LoadPalette
    ldx MenuSelect
    cpx #0
    bne :+
        lda #<PaletteDataClear
        sta PalPtr
        lda #>PaletteDataClear
        sta PalPtr + 1
        jmp LoadSinglePalette
    :
    ldx MenuSelect
    cpx #1
    bne :+
        lda #<PaletteDataCloudy
        sta PalPtr
        lda #>PaletteDataCloudy
        sta PalPtr + 1
        jmp LoadSinglePalette
    :
    ldx MenuSelect
    cpx #2
    bne :+
        lda #<PaletteDataNight
        sta PalPtr
        lda #>PaletteDataNight
        sta PalPtr + 1
        jmp LoadSinglePalette
    :
    rts
.endproc

.proc DrawNewColumn
    lda XScroll              ; We'll set the NewColAddr lo-byte and hi-byte
    lsr
    lsr
    lsr                      ; Shift right 3 times to divide XScroll by 8
    sta NewColAddr           ; Set the lo-byte of the column address

    lda CurrNametable        ; The hi-byte comes from the nametable
    eor #1                   ; Invert the low bit (0 or 1)
    asl
    asl                      ; Multiply by 4 (A is $00 or $04)
    clc
    adc #$20                 ; Add $20 (A is $20 or $24) for nametabe 0 or 1
    sta NewColAddr+1         ; Set the hi-byte of the column address ($20xx or $24xx)

    lda Column               ; Multiply (col * 32) to compute the data offset
    asl
    asl
    asl
    asl
    asl
    sta SourceAddr           ; Store lo-byte (--XX) of column source address

    lda Column
    lsr
    lsr
    lsr                      ; Divide current Column by 8 (using 3 shift rights)
    sta SourceAddr+1         ; Store hi-byte (XX--) of column source addres

                             ; Here we'll add the offset the column source address with the address of where the BackgroundData
    lda SourceAddr           ; Lo-byte of the column data start + offset = address to load column data from
    clc
    adc #<BackgroundData     ; Add the lo-byte
    sta SourceAddr           ; Save the result of the offset back to the source address lo-byte

    lda SourceAddr+1         ; Hi-byte of the column source address
    adc #>BackgroundData     ; Add the hi-byte
    sta SourceAddr+1         ; Add the result of the offset back to the source address hi-byte

    DrawColumn:
      lda #%00000100
      sta PPU_CTRL           ; Tell the PPU that the increments will be +32 mode

      lda PPU_STATUS         ; Hit PPU_STATUS to reset hi/lo address latch
      lda NewColAddr+1
      sta PPU_ADDR           ; Set the hi-byte of the new column start address
      lda NewColAddr
      sta PPU_ADDR           ; Set the lo-byte of the new column start address

      ldx #30                ; We'll loop 30 times (=30 rows)
      ldy #0
      DrawColumnLoop:
        lda (SourceAddr),y   ; Copy from the address of the column source + y offset
        sta PPU_DATA
        iny                  ; Y++
        dex                  ; X--
        bne DrawColumnLoop   ; Loop 30 times to draw all 30 rows of this column
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routine to draw attributes off-screen every 32 pixels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc DrawNewAttribs
    lda CurrNametable
    eor #1                   ; Invert low bit (0 or 1)
    asl                      ; Multiuply by 2, ($00 or $02)
    asl                      ; Multiply by 2 again ($00 or $04)
    clc
    adc #$23                 ; Add high byte of attribute base address ($23-- or $27--)
    sta NewColAddr+1         ; The hi-byte now has address = $23 or $27 for nametable 0 or 1

    lda XScroll
    lsr
    lsr
    lsr
    lsr
    lsr                      ; Divide by 32 (shift right 5 times)
    clc
    adc #$C0
    sta NewColAddr           ; The lo-byte contains (attribute base + XScroll/32)

    lda Column               ; (Column/4) * 8, since each row of attribute data in ROM is 8 bytes
    and #%11111100           ; Mask the lowest two bits to get the closest lowest multiple of 4
    asl                      ; One shift left equivelant to a multiplication by 2
    sta SourceAddr           ; Stores the lo-byte of the source attribute address offset (in ROM)

    lda Column               ; Proceed to compute the hi-byte of the source address offset in ROM
    lsr                      ; /2 Column >> 1
    lsr                      ; /4 Column >> 2
    lsr                      ; /8 Column >> 3
    lsr                      ; /16 Column >> 4
    lsr                      ; /32 Column >> 5
    lsr                      ; /64 C0lumn >> 6
    lsr                      ; /128, shift right 7 times to divide by 128
    sta SourceAddr+1         ; Stores the hi-byte of the Source address offset

    lda SourceAddr
    clc
    adc #<AttributeData      ; Add the lo-byte of the base address where AttributeData is in ROM
    sta SourceAddr           ; Stores the result of the add back into the lo-byte of the SourceAddr

    lda SourceAddr+1
    adc #>AttributeData      ; Add the hi-byte of the base address where AttributeData is in ROM
    sta SourceAddr+1         ; Stores the result of the add back into the hi-byte of the SourceAddr

    DrawAttribute:
      bit PPU_STATUS         ; Hit PPU_STATUS to reset the high/low address latch
      ldy #0                 ; Y = 0
      DrawAttribLoop:
        lda NewColAddr+1
        sta PPU_ADDR         ; Write the hi-byte of attribute PPU destination address
        lda NewColAddr
        sta PPU_ADDR         ; Write the lo-byte of attribute PPU destination address
        lda (SourceAddr),y   ; Fetch attribute byte from ROM
        sta PPU_DATA         ; Stores new attribute data into the PPU memory
        iny                  ; Y++
        cpy #8
        beq :+               ; Loop 8 times (to copy 8 attribute bytes)
          lda NewColAddr
          clc
          adc #8
          sta NewColAddr     ; Next attribute will be at (NewColAddr + 8)
          jmp DrawAttribLoop
       :
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to add new actor to the array in the first empty slot found
;; Params = ParamType, ParamXPos, ParamYPos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.proc AddNewActor
    ldx #0                            ; X = 0
  ArrayLoop:
    cpx #MAX_ACTORS * .sizeof(Actor)  ; Reached maximum number of actors allowed in the array?
    beq EndRoutine                    ; Then we skip and don't add a new actor
    lda ActorsArray+Actor::Type,x
    cmp #ActorType::NULL              ; If the actor type of this array position is NULL
    beq AddNewActorToArray            ;   Then: we found an empty slot, proceed to add actor to position [x]
   NextActor:
    txa
    clc
    adc #.sizeof(Actor)               ; Otherwise, we offset to the check the next actor in the array
    tax                               ; X += sizeof(Actor)
    jmp ArrayLoop

  AddNewActorToArray:                 ; Here we add a new actor at index [x] of the array
    lda ParamType                     ; Fetch parameter "actor type" from RAM
    sta ActorsArray+Actor::Type,x
    lda ParamXPos                     ; Fetch parameter "actor position X" from RAM
    sta ActorsArray+Actor::XPos,x
    lda ParamYPos                     ; Fetch parameter "actor position Y" from RAM
    sta ActorsArray+Actor::YPos,x
EndRoutine:
    rts
.endproc

.proc UpdateActors
    ldx #0
    ActorsLoop:
      lda ActorsArray+Actor::Type,x

      cmp #ActorType::MISSILE
      bne :+
        lda ActorsArray+Actor::YPos,x
        sec                             ;set the carry flag - slows the missle velocity
        sbc #1                         ; Decrement Y position of missiles by 1
        sta ActorsArray+Actor::YPos,x
        bcs Skip
            lda #ActorType::NULL
            sta ActorsArray+Actor::Type,x  ;Store NULL at the index of the array
        Skip:
    
        CheckCollision:
            lda #0
            sta Collision
            lda ActorsArray+Actor::XPos,x
            clc
            adc #3
            sta ParamXPos
            lda ActorsArray+Actor::YPos,x
            clc
            adc #1
            sta ParamYPos
            jsr CheckEnemyCollision
            lda Collision
            beq NoCollision
                lda #ActorType::NULL
                sta ActorsArray+Actor::Type,x
            NoCollision:

        jmp NextActor
      :
      cmp #ActorType::SUB
      bne :+
        lda ActorsArray+Actor::XPos,x
        sec                             ;set the carry flag - slows the missle velocity
        sbc #1                         ; Decrement X Position moving th the sub left
        sta ActorsArray+Actor::XPos,x
        bcs SkipSub
            lda #ActorType::NULL
            sta ActorsArray+Actor::Type,x  ;Store NULL at the index of the array
        SkipSub:
        jmp NextActor
      :
     cmp #ActorType::JET
      bne :+
        lda ActorsArray+Actor::XPos,x
        sec                             ;set the carry flag - slows the missle velocity
        sbc #1                         ; Decrement X Position moving th the sub left
        sta ActorsArray+Actor::XPos,x
        bcs JetSkip
            lda #ActorType::NULL
            sta ActorsArray+Actor::Type,x  ;Store NULL at the index of the array
        JetSkip:
        jmp NextActor
      :
      NextActor:
        txa
        clc
        adc #.sizeof(Actor)
        tax
        cmp #MAX_ACTORS * .sizeof(Actor)
        bne ActorsLoop
    rts
.endproc

.proc CheckEnemyCollision
    txa
    pha

    ldx #0
    stx Collision
EnemyCollisionLoop:
    cpx #MAX_ACTORS * .sizeof(Actor)    ;Check to see if we reached the total size of the actors
    beq CompleteCollision
        lda ActorsArray+Actor::Type,x
        cmp #ActorType::JET
        bne NextEnemy

        ;Load Bounding Box X1 , Y1, X2, Y2
        lda ActorsArray+Actor::XPos,x
        sta ParamRectX1                 ;Current X point where a jet has spawned
        lda ActorsArray+Actor::YPos,x
        sta ParamRectY1                 ;;Current Y point where a jet has spawned

        lda ActorsArray+Actor::XPos,x
        clc
        adc #22                 ; 3 - Starting Pixel * 8 pixels = 24
        sta ParamRectX2

        lda ActorsArray+Actor::YPos,X
        clc
        adc #8
        sta ParamRectY2
        jsr IsPointInBoundBox

        lda Collision
        beq NextEnemy
            lda #ActorType::NULL        ;Add explosion
            sta ActorsArray+Actor::Type,x
            jmp CompleteCollision
    NextEnemy:
        txa
        clc
        adc #.sizeof(Actor)  ;X += sizeof(Actor) Next enemy in array
        tax
        jmp EnemyCollisionLoop
CompleteCollision:
    pla
    tax

    rts
.endproc

.proc IsPointInBoundBox
    lda ParamXPos
    cmp ParamRectX1
    bcc PointOutside    ;XPos < X1
    
    lda ParamYPos
    cmp ParamRectY1
    bcc PointOutside    ;YPOS < Y1

    lda ParamXPos     
    cmp ParamRectX2     ;XPos > X2
    bcs PointOutside   

    lda ParamYPos
    cmp ParamRectY2
    bcs PointOutside    ;YPos > Y2

PointIsInside:
    lda #1
    sta Collision
    jmp EndCheck

PointOutside:
    lda #0
    sta Collision

EndCheck:
    rts
.endproc

.proc RenderActors
    lda #$02
    sta SprPtr+1
    lda #$00
    sta SprPtr                         ; Point SprPtr to $0200

    ldy #0                             ; Count how many tiles we are sending
    ldx #0                             ; Counts how many actors we are looping
    ActorsLoop:
      lda ActorsArray+Actor::Type,x
      
      cmp #ActorType::SPRITE0
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$70
        sta ParamTileNum
        lda #%00100000
        sta ParamAttribs
        lda #1
        sta ParamNumTiles 
        jsr DrawSprite                 ; Call routine to draw 1 SPRITE0 tile to the OAM
        jmp NextActor
      :
      cmp #ActorType::PLAYER
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$60
        sta ParamTileNum
        lda #%00000000
        sta ParamAttribs
        lda #4
        sta ParamNumTiles
        jsr DrawSprite                 ; Call routine to draw 4 PLAYER tiles to the OAM
        jmp NextActor
      :
      cmp #ActorType::MISSILE
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$50
        sta ParamTileNum
        lda #%00000001
        sta ParamAttribs
        lda #1
        sta ParamNumTiles
        jsr DrawSprite                 ; Call routine to draw 1 MISSILE tile to the OAM
        jmp NextActor
      :
      cmp #ActorType::SUB
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$04
        sta ParamTileNum
        lda #%00100000
        sta ParamAttribs
        lda #4
        sta ParamNumTiles
        jsr DrawSprite                 ; Call routine to draw 1 MISSILE tile to the OAM
        jmp NextActor
      :
      cmp #ActorType::JET
      bne :+
        lda ActorsArray+Actor::XPos,x
        sta ParamXPos
        lda ActorsArray+Actor::YPos,x
        sta ParamYPos
        lda #$10
        sta ParamTileNum
        lda #%00000011
        sta ParamAttribs
        lda #3
        sta ParamNumTiles
        jsr DrawSprite                 ; Call routine to draw 1 MISSILE tile to the OAM
        jmp NextActor
      :
      NextActor:
        txa
        clc
        adc #.sizeof(Actor)
        tax
        cmp #MAX_ACTORS * .sizeof(Actor)
        beq :+
            jmp ActorsLoop  ;Absolute address jmp
        :

        tya
        pha             ;Save the sprite count to the stack

        LoopTrailingTiles:
            cpy PreviusOAMCnt
            bcs :+      ;Skip
                lda $FF           ;Carry is clear Y < OAM Count
                sta (SprPtr),y
                iny
                sta (SprPtr),y
                iny
                sta (SprPtr),y
                iny
                sta (SprPtr),y
                iny
                jmp LoopTrailingTiles
            :

            pla
            sta PreviusOAMCnt ;Pull Y from the stack and save to Prev Count

    rts
.endproc

.proc SpawnActors
    lda Clock60         ;Current time
    sec
    sbc PreviousSubTime ; Clock60 - PreviousSubTime
    cmp #1              ; represent the passing of 3 seconds
    bne :+
        lda #ActorType::SUB
        sta ParamType
        lda #223
        sta ParamXPos
        jsr GenerateRandomNum
        sta ParamYPos
        jsr AddNewActor

        lda Clock60
        sta PreviousSubTime ;Save the current time as the previous
    :
    lda Clock60
    sec
    sbc PreviousJetTime
    cmp #1
    bne :+
        lda #ActorType::JET
        sta ParamType
        lda #223
        sta ParamXPos
        jsr GenerateRandomNum
        lsr                 ;Reduce the starting point by dividing by 8
        lsr
        lsr         
        clc
        adc #45             ;Increased final postioning so Jet dont crash into ship
        sta ParamYPos
        jsr AddNewActor

        lda Clock60
        sta PreviousJetTime ;Save the current time as the previous
    :
    rts
.endproc

.proc DrawSprite
    txa
    pha                                ; Save the value of the X register before anything
    
    ldx #0
    TileLoop:
       lda ParamYPos                   ; Send Y position to the OAM
       sta (SprPtr),y
       iny

       lda ParamTileNum                ; Send the Tile # to the OAM
       sta (SprPtr),y
       inc ParamTileNum                ; ParamTileNum++
       iny

       lda ParamAttribs                ; Send the attributes to the OAM
       sta (SprPtr),y
       iny

       lda ParamXPos                   ; Send X position to the OAM
       sta (SprPtr),y
       clc
       adc #8
       sta ParamXPos                   ; ParamXPos += 8

       iny

       inx                             ; X++
       cpx ParamNumTiles               ; Loop until X == NumTiles
       bne TileLoop

    pla
    tax                                ; Restore the previous value of X register

    rts
.endproc

.proc processController
    lda Buttons
    sta PrevButtons
    jsr ReadControllers      ; Read joypad and load button state
  CheckAButton:
    lda Buttons
    and #BUTTON_A
    beq :+
        lda Buttons
        and #BUTTON_A
        cmp PrevButtons
        beq :+              ;Skip if previously pressed
            lda #ActorType::MISSILE
            sta ParamType
            lda XPos
            sta ParamXPos
            lda YPos
            sta ParamYPos
            jsr AddNewActor      ; Call the subroutine to add a new missile actor
    :

rts
.endproc

.proc GenerateRandomNum
    ldy #8
    lda Seed
 :   asl
     rol Seed +1
     bcc :+
        eor #$39 ;Tap the Seed with this value polynomial
    :
    dey
    bne :--
        sta Seed
        cmp #0
        rts
.endproc

.proc LoadTitleScreen
    lda #<TitleScreenData
    sta BgPtr
    lda #>TitleScreenData
    sta BgPtr + 1
    PPU_SETADDR $2000   ;Set PPU to first nametable

    ldx #$00    ;Using X for outer loop high-byte ($00 - $04)
    ldy #$00    ;Using Y for inner loop low-byte $00 - $FF

    OuterLoop:
    InnerLoop:
        lda (BgPtr),y
        sta PPU_DATA
        iny
        cpy #0
        beq IncreaseHiByte
            jmp InnerLoop
    IncreaseHiByte:
        inc BgPtr + 1
        inx
        cpx #4
        bne OuterLoop
        rts
.endproc


.proc SwitchCHRBank
    sta $8000       ;Register for bank switch mapper 3
    rts
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reset handler (called when the NES resets or powers on)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Reset:
    INIT_NES                 ; Macro to initialize the NES to a known state

.proc TitleScreenClass
    lda #1
    jsr SwitchCHRBank       ;Use Bank 1 for the title screen

    lda #State::TITLESCREEN
    sta GameState

    lda #1
    sta MenuSelect   

    jsr LoadPalette     ;load color palette to PPU
    
   ;Set the background to cloudy by default...this is not a happy game

    jsr LoadTitleScreen

    lda #0
    sta MenuSelect

DrawMenuArrow:
    lda #93         ;Y Position
    sta $0200
    lda #$23        ;Sprite Tile
    sta $0201
    lda #%00000001  ;Attribute byte
    sta $0202
    lda #90
    sta $0203


TitleScreenLoop:
    lda Buttons
    sta PrevButtons

    jsr ReadControllers
CheckStartButton:
    lda Buttons
    and #BUTTON_START
    beq :+      ;Equal to zero
        jmp GameClass
    :
CheckButtonDown:
    lda Buttons
    and #BUTTON_DOWN
    beq :+
        cmp PrevButtons
        beq :+
            lda MenuSelect
            cmp #2
            beq :+
                inc MenuSelect
                lda $0200
                clc
                adc #17
                sta $0200
    :
CheckButtonUp:
    lda Buttons
    and #BUTTON_UP
    beq :+
        cmp PrevButtons
        beq :+
            lda MenuSelect
            cmp #0
            beq :+
                dec MenuSelect
                lda $0200
                sec
                sbc #17         ;Subtact 17 pixels
                sta $0200       ;Store result in Y Position       
    :

EnableRendering:
    lda #%10010000           ; Enable NMI and set background to use the 2nd pattern table (at $1000)
    sta PPU_CTRL
    lda #%00011110
    sta PPU_MASK 

    WaitForVBlank:           ; We lock the execution of the game logic here
      lda IsDrawComplete     ; Here we check and only perform a game loop call once NMI is done drawing
      beq WaitForVBlank      ; Otherwise, we keep looping

    lda #0
    sta IsDrawComplete       ; Once we're done, we set the DrawComplete flag back to 0
    
    jmp TitleScreenLoop

.endproc


.proc GameClass
    lda #0
    jsr SwitchCHRBank

    lda #State::PLAYING
    sta GameState

    DISABLE_PPU     ;Disable NMI and rendering

InitVariables:
    lda #0
    sta Frame                ; Frame = 0
    sta Clock60              ; Clock60 = 0
    sta XScroll              ; XScroll = 0
    sta CurrNametable        ; CurrNametable = 0
    sta Column               ; Column = 0
    lda #113
    sta XPos
    lda #165
    sta YPos
    lda #$10
    sta Seed+1
    sta Seed

Main:
    jsr LoadPalette          ; Call LoadPalette subroutine to load 32 colors into our palette

AddSprite0:
    lda #ActorType::SPRITE0
    sta ParamType
    lda #0
    sta ParamXPos
    lda #27
    sta ParamYPos
    jsr AddNewActor          ; Add new actor to array

AddPlayer:
    lda #ActorType::PLAYER
    sta ParamType
    lda XPos
    sta ParamXPos
    lda YPos
    sta ParamYPos
    jsr AddNewActor         ; Add new actor to array

InitBackgroundTiles:
    lda #1
    sta CurrNametable        ; CurrNametable = 1
    lda #0
    sta XScroll              ; XScroll = 0
    sta Column               ; Column = 0
InitBackgroundLoop:
    jsr DrawNewColumn        ; Draw all rows of new column (from top to bottom)
    
    lda XScroll
    clc
    adc #8
    sta XScroll              ; XScroll += 8
    
    inc Column               ; Column++
    
    lda Column
    cmp #32
    bne InitBackgroundLoop   ; Repeat all 32 columns of the first nametable

    lda #0
    sta CurrNametable        ; CurrNametable = 0
    lda #1
    sta XScroll              ; Scroll = 1

    jsr DrawNewColumn        ; Draw first column of the second nametable
    inc Column               ; Column++

    lda #%00000000
    sta PPU_CTRL             ; Set PPU increment to +1 mode

InitAttribs:
    lda #1
    sta CurrNametable
    lda #0
    sta XScroll
    sta Column
InitAttribsLoop:
    jsr DrawNewAttribs       ; Draw attributes in the correct place of the first nametable
    lda XScroll
    clc
    adc #32
    sta XScroll              ; XScroll += 32

    lda Column               ; Repeat for all elements of the first nametable
    clc
    adc #4
    sta Column               ; Column += 4
    cmp #32
    bne InitAttribsLoop      ; Loop until we reach Column 32

    lda #0
    sta CurrNametable
    lda #1
    sta XScroll
    jsr DrawNewAttribs       ; Draw first attributes of second nametable

    inc Column               ; Column = 33

EnableRendering:
    lda #%10010000           ; Enable NMI and set background to use the 2nd pattern table (at $1000)
    sta PPU_CTRL
    lda #0
    sta PPU_SCROLL           ; Disable scroll in X
    sta PPU_SCROLL           ; Disable scroll in Y
    lda #%00011110
    sta PPU_MASK             ; Set PPU_MASK bits to render the background

GameLoop:

    jsr processController
    jsr RenderActors
    jsr SpawnActors
    jsr UpdateActors

    WaitForVBlank:           ; We lock the execution of the game logic here
      lda IsDrawComplete     ; Here we check and only perform a game loop call once NMI is done drawing
      beq WaitForVBlank      ; Otherwise, we keep looping

    lda #0
    sta IsDrawComplete       ; Once we're done, we set the DrawComplete flag back to 0

    jmp GameLoop
.endproc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NMI interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NMI:
    SAVE_REGS                ; Macro to save register values by pushing them to the stack

    inc Frame                ; Frame++

OAMStartDMACopy:             ; DMA copy of OAM data from RAM to PPU
    lda #$02                 ; Every frame, we copy spite data starting at $02**
    sta PPU_OAM_DMA          ; The OAM-DMA copy starts when we write to $4014

    lda GameState
    cmp #State::PLAYING
    bne EndScrolling    ;Go to empty label EndScrolling if we are not in gameplay

NewColumnCheck:
    lda XScroll
    and #%00000111           ; Check if the scroll a multiple of 8
    bne :+                   ; If it isn't, we still don't need to draw a new column
      jsr DrawNewColumn      ; If it is a multiple of 8, we proceed to draw a new column of tiles!
      Clamp128Cols:
        lda Column
        clc
        adc #1               ; Column++
        and #%01111111       ; Drop the left-most bit to wrap around 128
        sta Column           ; Clamping the value to never go beyond 128
    :

NewAttribsCheck:
    lda XScroll
    and #%00011111           ; Check if the scroll is a multiple of 32 (lowest 5 bits are 00000)
    bne :+                   ; If it isn't, we still don't need to draw new attributes
      jsr DrawNewAttribs       ; It it is a multiple of 32, we draw the new attributes!
    :

SetPPUNoScroll:
    lda #0
    sta PPU_SCROLL
    sta PPU_SCROLL           ; Set *no* scroll for the status bar

EnablePPUSprite0:
    lda #%10010000           ; Enable PPU sprites for sprite 0
    sta PPU_CTRL
    lda #%00011110           ; Enable sprites and enable background
    sta PPU_MASK

WaitForNoSprite0:
    lda PPU_STATUS
    and #%01000000           ; PPU address $2002 bit 6 is the sprite 0 hit flag
    bne WaitForNoSprite0     ; Loop until we do *not* have a sprite 0 hit

WaitForSprite0:
    lda PPU_STATUS
    and #%01000000           ; PPU address $2002 bit 6 is the sprite 0 hit flag
    beq WaitForSprite0       ; Loop until we do have a sprite 0 hit

ScrollBackground:
    inc XScroll              ; XScroll++
    lda XScroll
    bne :+                   ; Check if XScroll rolled back to 0, then we swap nametables!
      lda CurrNametable
      eor #1                 ; An XOR with %00000001 will flip the right-most bit.
      sta CurrNametable      ; If it was 0, it becomes 1. If it was 1, it becomes 0.
    :
    lda XScroll
    sta PPU_SCROLL           ; Set the horizontal X scroll first
    lda #0
    sta PPU_SCROLL           ; No vertical scrolling

EndScrolling:

RefreshRendering:
    lda #%10010000           ; Enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
    ora CurrNametable        ; OR with CurrNametable (0 or 1) to set PPU_CTRL bit-0 (starting nametable)
    sta PPU_CTRL
    lda #%00011110           ; Enable sprites, enable background, no clipping on left side
    sta PPU_MASK

SetGameClock:
    lda Frame                ; Increment Clock60 every time we reach 60 frames (NTSC = 60Hz)
    cmp #60                  ; Is Frame equal to #60?
    bne :+                   ; If not, bypass Clock60 increment
    inc Clock60              ; But if it is 60, then increment Clock60 and zero Frame counter
    lda #0
    sta Frame
:

SetDrawComplete:
    lda #1
    sta IsDrawComplete       ; Set the DrawComplete flag to indicate we are done drawing to the PPU

    RESTORE_REGS

    rti                      ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRQ interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IRQ:
    rti                      ; Return from interrupt


.include "colorpalette.inc"
.include "levelone.inc"
.include "attributes.inc"

TitleScreenData:
.incbin "corsairtitle.nam"

.segment "CHARS1"
.incbin "corsair.chr"

.segment "CHARS2"
.incbin "titlescreen.chr"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors with the addresses of the handlers that we always add at $FFFA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "VECTORS"
.word NMI                    ; Address (2 bytes) of the NMI handler
.word Reset                  ; Address (2 bytes) of the Reset handler
.word IRQ                    ; Address (2 bytes) of the IRQ handler
