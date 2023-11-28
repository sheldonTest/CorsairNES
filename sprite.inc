SpriteData:
;Total Range of Sprite Buffer: $200-$300 OR 512 - 768
;     Y    Title #     attributes   X
;      $200  $201       $202   $203 ;Sprite 0 used for HUD / Split screen
.byte $27,  $70,  %0010000,    $6  ;Change from $50 to $70 - a blank background sprite

.byte $A6,  $60,  %00000000,  $70 
;      204    205       206    207
.byte $A6,  $61,  %00000000,  $78 ;Sprite 2
;     208     209       20A   20B
.byte $A6,  $62,  %00000000,  $80 ; Sprite 2
;     20C    20D        20E    20F
.byte $A6,  $63,  %00000000,  $88 ; Sprite 3


; Sprite Attribute Byte:
;-----------------------
; 76543210
; |||   ||
; |||   ++- Color Palette of sprite. Choose which set of 4 from the 16 colors to use
; |||
; ||+------ Priority (0: in front of background; 1: behind background)
; |+------- Flip sprite horizontally
; +-------- Flip sprite vertically