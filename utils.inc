.macro PPU_SETADDR address  ;16-bit address
    bit PPU_STATUS  ;Reset PPU "Latch" register
    lda #>address
    sta PPU_ADDR    ;Store high byte at 2006
    lda #<address
    sta PPU_ADDR    ;Store low byte at 2006
.endmacro

.macro PPU_SETDATA datum
    lda datum   ;Lookup byte data in ROM
    sta PPU_DATA 
.endmacro

.macro SAVE_REGS
    pha         ;push A to the stack
    txa         ;transfer X to A
    pha         ;Push X to the stack
    tya
    pha         ;Push Y to the stack
    php         ;Push processor status flags to the stack
.endmacro

.macro RESTORE_REGS
    plp        ;Pull status flags from the stack
    pla         ;Restore data of X from the stack
    tay         ;transfer A to Y
    pla
    tax
    pla         ;Pull A from the stack
.endmacro

.macro DISABLE_PPU
    lda #0
    sta PPU_CTRL
    sta PPU_MASK
.endmacro