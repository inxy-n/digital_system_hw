; Program:   homework 4
; Course:    Digital Systems (Game Boy Lab)
; Author:    YE Xiayuan 999025695
; Date:      2025
;

INCLUDE "hardware.inc"

DEF OBJCOUNT EQU 40
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
  call   ResetBG
  ld a, LCDC_ON | LCDC_OBJ_ON | LCDC_BG_ON | LCDC_BLOCK01
  ld [rLCDC], a
  ld a,4
  ld [BaseX],a
  ld a,4
  ld [BaseY],a
  ld a,0
  ld [FUCK],a
  
MainLoop:
  call readKeys
  call HandleInput
  call Pause
  call Scroll
  call UpdateObjects
  call WaitVBlank
  ;call ModifyAndResetScrollValue
  call CopyShadowOAMtoOAM
  jp MainLoop


SECTION "Functions", ROM0
; Handle the rotation and the move
HandleInput:
  ; Check the status of paused
  ld a, [paused]
  cp 1
  ret z ; If the status is paused, do nothing
  
  ; Get the status of key
  ld a, [current]
  
  ; Check the rotation
  bit 0, a
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
  cp 13 ; The biggest X coord
  jr c, .storeRight
  ld a, 13
.storeRight:
  ld [BaseX], a
  
.checkDown:
  bit KEY_DOWN, a
  jr z, .done
  
  ; move down
  ld a, [BaseY]
  inc a
  cp 20 ; The biggest Y coord
  jr c, .storeDown
  ld a, 20
.storeDown:
  ld [BaseY], a
  
.done:
  ret
  
Rotate:
  ld a, [FUCK]
  add a, 8 ;since each status of rotation has length 8 
  cp 32 ; upper bound
  jr c, .store
  ld a, 0
.store:
  ld [FUCK], a
  
  ret

Pause:
  bit 1,c
  ret z
  
  ; change the status of paused
  ld a,[paused]
  xor %00000001
  ld [paused],a
  
  ; clear the Key status
  ld a, 0
  ld [previous], a
  ld [current], a
  
  ret
  
Scroll:
  
  ret
ResetBG:
  ld hl,TILEMAP0
  ld bc,1024
.loop:
  ld [hl],2 ; fog
  inc hl
  dec bc
  ld a,b
  or c
  jr nz,.loop
  ret


InitializeObjects:
  ld hl,   ShadowOAM   ; hl points to first object entry
   
  
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
  
  
  ld a,[FUCK]
  ld b,0
  ld c,a
  ld hl,Tetromino_L

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
BaseY:DS 1
current: DS 1
previous: DS 1
paused: DS 1
FUCK:DS 1
