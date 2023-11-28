.segment "HEADER"
.byte $4E   ;'N'
.byte $45   ;'E'
.byte $53   ;'S'
.byte $1A   ;'\n'
.byte $02   ;How many 16kb of PRG-ROM will be used (32kb is used)
.byte $02   ;How many 8kb of CHR-ROM is used (16kb is used) for two Banks
.byte %00110001   ;Horizontal (0) / Vertical mirroring (1), no battery no mapper
.byte %00000000   ;Mapper 0, playchoice
.byte $00   ;No PRG-RAM
.byte $00   ;NTSC TV Format
.byte $00   ;Extra PRG-RAM format
.byte $00   ;padding to complete 16kb of header
.byte $00
.byte $00
.byte $00
.byte $00