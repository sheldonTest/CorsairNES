.enum ActorType
    NULL = 0
    PLAYER = 1
    SUB = 2
    JET = 3
    MISSILE = 4
    BOMB = 5
    SPRITE0 = 6
.endenum

.struct Actor
    Type .byte
    XPos .byte
    YPos .byte
    XVel .byte
    YVel .byte
.endstruct