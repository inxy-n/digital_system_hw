; Program:   homework 4
; Course:    Digital Systems (Game Boy Lab)
; Author:    YE Xiayuan 999025695
; Date:      2025
;

INCLUDE "hardware.inc"

DEF OBJCOUNT EQU 4
DEF KEY_RIGHT  EQU 4
DEF KEY_LEFT   EQU 5
DEF KEY_UP     EQU 6
DEF KEY_DOWN   EQU 7



SECTION "Header", ROM0[$100]
  jp EntryPoint

  ds $150 - @, 0

EntryPoint:
  call WaitVBlank
  ld a, 0
  ld [rLCDC], a


  ld a,%11111100 ; black and white palette
  ld [rOBP0], a

  call   CopyTilesToVRAM
  ld     hl, STARTOF(OAM)
  call   ResetOAM
  ld     hl, ShadowOAM
  call   ResetOAM
  call   ResetMap
  call   InitializeObjects


  ld a, LCDC_ON | LCDC_OBJ_ON | LCDC_BG_ON | LCDC_BLOCK01
  ld [rLCDC], a

  call WaitVBlank
  ld a, 0
  
  ld [paused],a
  
  ld [rSCY],a
  ld [rSCX],a
  ld [rLCDC], a
  ld [currentPiece], a
  ld [nextPiece], a
  ld a,1
  ld [next], a
  call   ResetBG
  ld a, LCDC_ON | LCDC_OBJ_ON | LCDC_BG_ON | LCDC_BLOCK01
  ld [rLCDC], a
  ld a,8
  ld [BaseX],a
  ld a,4
  ld [BaseY],a
  ld a,0
  ld [RotationState],a

  ld a, 0
  ld [frameCounter], a

  ld a, 10
  ld [dropTime], a
  
MainLoop:
  call readKeys
  call HandleInput
  call AutoDrop
  call Pause
  call UpdateObjects

ld a, [next]
  cp 1
  jr nz, .notNext1
  call LockCurrentPieceToMap
.notNext1



  call WaitVBlank ; Wait for VBlank here, before rendering
  ld a, [next]
  cp 1
  jr nz, .notNext2
  call RenderMapToBG
  
  
  
.notNext2

  call CopyShadowOAMtoOAM
  jp MainLoop



SECTION "Functions", ROM0

AutoDrop:
  ld a,[next]
  cp 1
  jr nz, .notNew
  ld a,0
  ld [next],a
  call GenerateNewPiece
  ret
.notNew:
  ld a, [frameCounter]
  inc a
  ld [frameCounter], a
  
  ld b, a
  ld a, [dropTime]
  cp b
  ret nz
  
  ld a, 0
  ld [frameCounter], a
  ld a,[BaseY]
  cp 18 ; The biggest Y coord
  jr c, .storeDown
  ld a,1
  ld [next],a
  ld [upDate],a
  
  ret
.storeDown:
  call MoveDown
  
  ret
  
MoveDown:
  ld a, [BaseY]
  inc a

  ld [BaseY], a
  ret
; Handle the rotation and the move
HandleInput:
  ; Check the status of paused
  ld a, [paused]
  cp 1
  ret z ; If the status is paused, do nothing

  ; Get the status of key
  ld a, [current]
  
  ; Check the rotation
  bit KEY_UP, a
  jr z, .checkLeft
  call Rotate
  ret
  
.checkLeft:
  bit KEY_LEFT, a
  jr z, .checkRight
  
  ; move left
  ld a, [BaseX]
  dec a
  cp 4 ;The smallest X coord
  jr nc, .storeLeft
  ld a, 4
.storeLeft:
  ld [BaseX], a
  
.checkRight:
  bit KEY_RIGHT, a
  jr z, .checkDown
  
  ; move right
  ld a, [BaseX]
  inc a
  cp 14 ; The biggest X coord
  jr c, .storeRight
  ld a, 14
.storeRight:
  ld [BaseX], a
  
.checkDown:
  bit KEY_DOWN, a
  ret z
  
  ; move down
  ld a, [BaseY]
  inc a
  cp 18 ; The biggest Y place 20-2(since our height of Tetromino is 2)
  jr c, .storeDown
  ld a, 18
.storeDown:
  ld [BaseY], a
   
  
  
Rotate:
  ld a, [RotationState]
  add a, 8 ;since each status of rotation has length 8 
  cp 32 ; upper bound
  jr c, .store
  ld a, 0
.store:
  ld [RotationState], a
  
  ret

Pause:
  bit 1,c
  ret z
  ld a,1
  ld [next],a
  ret
  ; change the status of paused
  ld a,[paused]
  xor %00000001
  ld [paused],a
  
  ; clear the Key status
  ld a, 0
  ld [previous], a
  ld [current], a
  
  ret
  

ClearLine:
  push af
  push bc
  push de
  push hl
  ; store the line's pointer
  push hl
  ; Compute the lines we need to move ( current line )
  ; CurrentLine: (hl - Map) / 10
  push hl
  ld de, Map
  xor a
  ld a, l
  sub c
  ld l, a
  ld a, h
  sbc a, b
  ld h, a
  
  ld a, 1
  ld b, 0
.divisionLoop:
  cp 10
  jr c, .divisionDone
  sub 10
  inc b
  jr .divisionLoop
.divisionDone:
  pop hl
  ; If 0, no move ( only clear )
  ld a, b
  and a
  jr z, .clearOnly
  ; Otherwise, move
  ld b, a

.moveRows:
  push bc
  ld bc, 10
  or a ; clear carry flag
  ld a, l
  sub c
  ld l, a
  ld a, h
  sbc a, b
  ld h, a
  
  push hl
  ld d, h
  ld e, l
  ld bc, 10
  add hl, bc ; hl points to the current line

  ; copy the line
  ld bc, 10
  call CopyLine
  pop hl
  pop bc
  dec b
  jr nz, .moveRows

.clearOnly:
  pop hl
  ld b, 10
  ld a, 0
  
.clearTopLine:
  ld [hl+], a
  dec b
  jr nz, .clearTopLine

  pop hl
  pop de
  pop bc
  pop af
  ret

CopyLine:
  ; copy line from hl to de
  push af
  push bc
  push de
  push hl

.copyLoop:
  ld a, [hl+]
  ld [de], a
  inc de
  dec bc
  ld a, b
  or c
  jr nz, .copyLoop

  pop hl
  pop de
  pop bc
  pop af
  ret

GenerateNewPiece:
  ld a, [nextPiece]
  ld [currentPiece], a

  call RandomByte
  and %00000111
  cp 7
  jr nz, .getNextPiece
  xor a
.getNextPiece:
  ld [nextPiece], a

  ; reset the position and rotation
  ld a, 8
  ld [BaseX], a
  ld a, 4
  ld [BaseY], a
  ld a, 0
  ld [RotationState], a
  
;  call CheckCollision
  jr c, .gameOver

  ret

.gameOver:
  ld a, 1
  ld [GameOver], a
  pop af
  ret

CheckCollision:
  ; Check whether the piece collides with the map or the border
  push af
  push bc
  push de
  push hl

  ld a, [BaseX]
  ld d, a ; d : Xcoord
  ld a, [BaseY]
  ld e, a ; e : Ycoord
  ld a, [RotationState]
  ld c, a ; c : rotation state
  ld a, [currentPiece]
  ld b, a ; b : piece type

  call CheckCollisionAtPosition

  pop hl
  pop de
  pop bc
  pop af
  ret 

CheckCollisionAtPosition:
  ; Check collision at the given position
  push hl
  push bc
  push de

  ; Get the address of the piece data
  ld a, b ; b : piece type(0-6)
  sla a ; * 2 since each address is 2 bytes
  ld hl, PieceAddressTable
  add a, l
  ld l, a
  jr nc, .notcarray
  inc h
.notcarray:
  ld a, [hl+]
  ld h, [hl]
  ld l, a ; hl points to the piece data
  
  ; Get the rotation offset
  ld a, c ; c : rotation state
  sla a
  sla a
  sla a
  ld b, 0
  ld c, a
  add hl, bc ; hl points to the current rotation data

  ld b, 4 ; Each Tetromino has 4 blocks
.checkLoop:
  push bc
  push hl

  ; OffsetY 
  ld a, [hl+]
  add e ; e : Ycoord
  ld c, a 
  ; OffsetX
  ld a, [hl]
  add d ; d : Xcoord
  ld b, a

  ; Check the boundary
  ld a, b
  cp 4
  jr c, .collision
  cp 13
  jr nc, .collision
  ld a, c
  cp 1
  jr c, .collision
  cp 18
  jr nc, .collision

  ; Let's consider the map index
  ; The Index of piece: Y * 10 + X
  ld a, c
  dec a ; a = y - 1 (0-17)

  ; Calculate a * 10
  sla a
  ld e, a ; e = a * 2
  sla a
  sla a ; * 8
  add e ; * 10
  ld e, a ; e = 10a

  ld a, b
  sub 4 ; a = x - 4 (0-9)
  add e
  ld e, a
  ld d, 0 ; de = index in map (0-179)

  ld hl, Map
  add hl, de ; hl points to the current position in Map
  ld a, [hl]
  and a
  jr nz, .collisionMap

  pop hl
  pop bc
  inc hl
  inc hl
  dec b
  jr nz, .checkLoop

  ; no collision
  and a ; clear the carry flag (no collision)
  jr .noCollision

.collisionMap:
  pop hl
  pop bc
.collision:
  pop de
  pop bc
  pop hl
  scf ; set the carry flag (collision)
  ret
.noCollision:
  pop de
  pop bc
  pop hl
  ret


ResetBG:
  ld hl,TILEMAP0
  ld bc,1024
.loop:
  ld [hl],1 ; blank
  inc hl
  dec bc
  ld a,b
  or c
  jr nz,.loop
  ret
  
LockCurrentPieceToMap:
  ld a, [BaseX]
  ld d, a          ; d = BaseX
  ld a, [BaseY]
  ld e, a          ; e = BaseY

  ld a, [RotationState]     ; rotation offset (0 / 8 / 16 / 24)
  
  ld b, 0
  ld c, a

  ld hl,PieceAddressTable
  ld a,[currentPiece]
  add a,a
  add a,l
  ld l,a
  jr nc,.notcarray
  inc h
.notcarray:
  ld a,[hl+]
  ld h,[hl]
  ld l,a

  add hl, bc       ; hl -> 当前旋转数据
 

  ; --- Y ---
  ld a, [hl]
  inc hl
  add e 
  ld c, a          ; c = Y
  ; --- X ---
  ld a, [hl]
  inc hl
  add d
  ld b, a          ; b = X
  push hl
  push de
  call MarkPieceIntoMap
  pop de
  pop hl
ld a, [hl]
  inc hl
  add e
  ld c, a          ; c = Y
  ; --- X ---
  ld a, [hl]
  inc hl
  add d
  ld b, a          ; b = X

  push hl
  push de
  call MarkPieceIntoMap
  pop de
pop hl
ld a, [hl]
  inc hl
  add e
  ld c, a          ; c = Y
  ; --- X ---
  ld a, [hl]
  inc hl
  add d
  ld b, a          ; b = X

  push hl
  push de
  call MarkPieceIntoMap
  
  pop de
pop hl
ld a, [hl]
  inc hl
  add e
  ld c, a          ; c = Y

  ; --- X ---
  ld a, [hl]
  inc hl
  add d
  ld b, a          ; b = X

  call MarkPieceIntoMap

 
  ld a, 1
  ld [BGChanged], a
 
  ret

MarkPieceIntoMap:
  ; b = X (2–13)
  ; c = Y (1–18)

  ; HL = MapRowTable[(Y-1)] 
  dec c
  dec c
  ld a, c
  add a, a            ; *2
  ld l, a
  ld h, 0
  ld de, MapRowTable
  add hl, de
  ld a, [hl+]
  ld h, [hl]
  ld l, a             ; HL = 行起始地址
  
  ; HL += (X - 4)
  ld a, b
  sub 5
  add l
  jr nc,.notcarry
  inc h
  .notcarry
  ld l,a
  
   ; 写入
  ld a, 1
  ld [hl],a
  ret


InitializeObjects:
  ld hl,   ShadowOAM   ; hl points to first object entry
   
  
  ret
RenderMapToBG:
  ld a, [BGChanged]
  cp 1
  ret nz              ; 没变化不渲染

  ld hl, Map          ; Map 起始
  ld de, TILEMAP0
  ld a, 4             ; X 偏移 = 4
  add a, e
  ld e, a
  jr nc, .noCarryX
  inc d
.noCarryX:

  ld b, 18            ; 18 行

.rowLoop:
  push bc
  ld c, 10            ; 10 列

.colLoop:
  ld a, [hl+]         ; 读 Map
  cp 0
  jr z, .blank

  ld a, 3             ; 方块 tile
  jr .write

.blank:
  ld a, 1             ; 空白 tile

.write:
  ld [de], a
  inc de
  dec c
  jr nz, .colLoop

  ; 跳到下一行同一列（32 - 10 = 22）
  ld a, b
  cp 4
  jr nz, .endRowSkip
  call WaitVBlank
.endRowSkip:

  cp 9
  jr nz, .endRowSkip2
  call WaitVBlank
.endRowSkip2:

  
  cp 14
  jr nz, .endRowSkip3
  call WaitVBlank
.endRowSkip3:
  ld a, e
  add a, 22
  ld e, a
  jr nc, .noCarryRow
  inc d
.noCarryRow:

  pop bc
  dec b
  jr nz, .rowLoop

  xor a
  ld [BGChanged], a
  ret



CopyShadowOAMtoOAM:
  ld hl, ShadowOAM
  ld de, STARTOF(OAM)
  ld b, OBJCOUNT
.loop:
  ld a,[hl+]
  ld [de],a
  inc e
  ld a,[hl+]
  ld [de],a
  inc e
  ld a,[hl+]
  ld [de],a
  inc e
  ld a,[hl+]
  ld [de],a
  inc e
  dec b
  jr nz, .loop
  ret

UpdateObjects:
  
  
  ld a,[RotationState]
  ld b,0
  ld c,a
  ld hl,PieceAddressTable
  ld a,[currentPiece]
  add a,a
  add a,l
  ld l,a
  jr nc,.notcarray
  inc h
.notcarray:
  ld a,[hl+]
  ld h,[hl]
  ld l,a

  add hl,bc
  ld d,h
  ld e,l
  ld hl,ShadowOAM
  
  ld c,4  
.loop
  ld a,[BaseY]
  ld b,a
  ld a,[de]
  add b
  sla a
  sla a
  sla a
  
  ld [hl],a
  inc hl
  
  inc de
  ld a,[BaseX]
  ld b,a
  ld a,[de]
  add b
  sla a
  sla a
  sla a
  
  ld [hl],a
  inc hl
  inc hl
  inc hl
  
  inc de
  dec c
  jr nz,.loop
  
  ret

Random2bits:
  push bc
  call RandomByte; ld a,[rDiv] (16cy) ::  xor b (4cy) :: xor l (4cy) :: xor [hl] (8cy) (=32cy) , vs call/ret (24cy+16= 40cy)
  ld b,a

  swap a         ; Swap nibbles
  xor b          ; XOR high and low nibbles
  ld b,a
  rrca
  rrca           ; Shift right 2
  xor b          ; Mix more

  and %00000011  ; Keep 2 bits
  pop bc
  ret
  
; Alternatively (if you only keep the 2 low bits):
; REPT 3
;   rrca :: rrca :: xor b
; ENDR


RandomByte:
; Return a "random" byte into A
; by mixing a few values with XOR
  ld a,[rDIV]
  xor b
  xor l
  xor [hl]
  ret

WaitVBlank:
  ld a, [rLY]
  cp 144
  jr nz, WaitVBlank
  ret

ResetOAM:
; input: HL: location of OAM or Shadow OAM
  ld b,40*4
  ld a,0
.loop:
  ld [hl],a
  inc hl
  dec b
  jr nz,.loop
  ret

ResetMap:
  ld hl,Map
  ld b,180
  ld a,0
.loop:
  ld [hl],a
  inc hl
  dec b
  jr nz,.loop
  ret

UpdateBackGround:
ld a,[BGChanged]
cp 0
ret z
  ld hl,TILEMAP0
.loop
.lp1
  ld a,c
  cp 0
  jp z,.break1
  ld a,l
  add 32    ;column number
  jp nc,.notcarray
  inc h
.notcarray:
  ld l,a
 
  dec c
  jp .lp1
.break1:

.lp3
  ld a,d
  cp 0
  jp z,.break2
  inc hl
  dec d
  jp .lp3
.break2
  ld [hl],1 ; fog
  
  pop hl

  inc hl
  inc hl
  inc hl
  dec b
  jr nz, .loop
  ret


CopyTilesToVRAM:
  ld de, Tiles
  ld hl, STARTOF(VRAM)
  ld bc, TilesEnd - Tiles
.copy:
  ld a,[de]
  inc de
  ld [hl],a
  inc hl
  ld [hl],a
  inc hl
  dec bc
  ld a,b
  or c
  jr nz, .copy
  ret

;---------------------------------------------------------------------
readKeys:
;---------------------------------------------------------------------
; Output:
; b : raw state:   pressing key triggers given action continuously
;                  as long as it is pressed
; c : rising edge: pressing key triggers given action only once,
;                  key must be released and pressed again
; Requires to define variables `previous` and `current`
  ld    a,$20
  ldh   [rP1],a   
  ldh   a,[rP1] :: ldh a,[rP1]
  cpl
  and   $0F         ; lower nibble has down, up, left, right
  swap	a           ; becomes high nibble
  ld	b,a
  ld    a,$10
  ldh   [rP1],a
  ldh   a,[rP1] :: ldh a,[rP1] :: ldh a,[rP1]
  ldh   a,[rP1] :: ldh a,[rP1] :: ldh a,[rP1]
  cpl
  and   $0F         ; lower nibble has start, select, B, A
  or    b
  ld    b,a

  ld    a,[previous]  ; load previous state
  xor   b	      ; result will be 0 if it's the same as current read
  and   b	      ; keep buttons that were pressed during this read only
  ld    [current],a   ; store result in "current" variable and c register
  ld    c,a
  ld    a,b           ; current state will be previous in next read
  ld    [previous],a

  ld    a,$30         ; reset rP1
  ldh   [rP1],a
  ret

SECTION "Data", ROM0

MapRowTable:
  dw Map
  dw Map+10
  dw Map+20
  dw Map+30
  dw Map+40
  dw Map+50
  dw Map+60
  dw Map+70
  dw Map+80
  dw Map+90
  dw Map+100
  dw Map+110
  dw Map+120
  dw Map+130
  dw Map+140
  dw Map+150
  dw Map+160
  dw Map+170


PieceAddressTable:
    dw Tetromino_I
    dw Tetromino_J
    dw Tetromino_L
    dw Tetromino_O
    dw Tetromino_S
    dw Tetromino_Z
    dw Tetromino_T

Tiles:
; smiling face
 DB %01111110
 DB %10000001
 DB %10000001
 DB %10000001
 DB %10000001
 DB %10000001
 DB %10000001
 DB %01111110
; blank
 DB %00000000
 DB %00000000
 DB %00000000
 DB %00000000
 DB %00000000
 DB %00000000
 DB %00000000
 DB %00000000
 
 ; fog
 DB %10101010
 DB %01010101
 DB %10101010
 DB %01010101
 DB %10101010
 DB %01010101
 DB %10101010
 DB %01010101
 
 ; black
 DB %11111111
 DB %11111111
 DB %11111111
 DB %11111111
 DB %11111111
 DB %11111111
 DB %11111111
 DB %11111111
TilesEnd:

Tetromino_I:
; rotation 0: degree 0
DB 1,0,0,0,-1,0,-2,0
; rotation 1: degree 90
DB 0,-1,0,0,0,1,0,2
; rotation 2: degree 180
DB 2,0,1,0,0,0,-1,0
; rotation 3: degree 270
DB 0,2,0,1,0,0,0,-1

Tetromino_J:
DB -1,-1,-1,0,0,0,1,0
DB -1,1,0,1,0,0,0,-1
DB 1,1,1,0,0,0,-1,0
DB 1,-1,0,-1,0,0,0,1

Tetromino_L:
DB 1,0,0,0,-1,0,-1,1
DB 0,-1,0,0,0,1,1,1
DB -1,0,0,0,1,0,1,-1
DB 0,1,0,0,0,-1,-1,-1

Tetromino_O:
DB 0,0,1,0,0,1,1,1
DB 0,0,1,0,0,1,1,1
DB 0,0,1,0,0,1,1,1
DB 0,0,1,0,0,1,1,1

Tetromino_S:
DB 0,-1,0,0,1,0,1,1
DB -1,0,0,0,0,-1,1,-1
DB 0,1,0,0,-1,0,-1,-1
DB 1,0,0,0,0,1,-1,1

Tetromino_Z:
DB 1,-1,1,0,0,0,0,1
DB -1,-1,0,-1,0,0,1,0
DB -1,1,-1,0,0,0,0,-1
DB 1,1,0,1,0,0,-1,0

Tetromino_T:
DB 0,-1,0,0,0,1,-1,0
DB -1,0,0,0,1,0,0,1
DB 0,1,0,0,0,-1,1,0
DB 1,0,0,0,-1,0,0,-1


SECTION "Variables", WRAM0
ShadowOAM: DS 160
Map:DS 180
BaseX: DS 1
BaseY: DS 1
next:DS 1
current: DS 1
previous: DS 1
paused: DS 1
BGChanged: DS 1
frameCounter: DS 1
dropTime: DS 1
needLock: DS 1
upDate:DS 1
currentPiece: DS 1
nextPiece: DS 1
Score: DS 2
Lines: DS 1
GameOver: DS 1
TempY: DS 1
TempX: DS 1
TempRotateState: DS 1
Level: DS 1
RotationState: DS 1
