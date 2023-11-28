.macro INIT_NES
    sei     ;Disable all interrupts
    cld     ;Clear decimal mode
    ldx #$FF  ;Load 255 in X register
    txs        ; Initialize the stack pointer @255

    inx         ;Increment 255 which rolls back to 0
    stx PPU_CTRL    ;Disable NMI
    stx PPU_MASK    ;Disable Rendering (background and sprites)
    stx $4010       ;Disable DMC IRQ

    lda #$40
    sta $4017   ;Disable the APU frame IRQ

WaitFirstVBlank:
    bit PPU_STATUS
    bpl WaitFirstVBlank

    txa         ;Accumulator is now 0

ClearRAM:           ;Clears out zero page memory
    sta $0000,x     ;Clear memory from 0000 - 00FF
    sta $0100,x     ;Clear memory from 0100 - 01FF

    lda #$FF        ;add 256 to hide tile data because screen will clip graphics data
    sta $0200,x     ;Clear memory from 0200 - 02FF

    lda #0
    sta $0300,x     ;Clear memory from 0300 - 03FF
    sta $0400,x     ;Clear memory from 0400 - 04FF
    sta $0500,x     ;Clear memory from 0500 - 05FF
    sta $0600,x     ;Clear memory from 0600 -06FF
    sta $0700,x     ;Clear memory from 0700 - 07FF
    inx
    bne ClearRAM

WaitSecondVBlank:
    bit PPU_STATUS
    bpl WaitSecondVBlank
.endmacro