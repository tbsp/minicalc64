include "64cube.inc"

Screen    EQU $f000
EntryAddr EQU (Screen + 46 * 64 + 4) ; Address to start drawing entry digits

ENUM $0
  r0          db 1 ; general purpose ZP variables
  r1          db 1
  r2          db 1
  r3          db 1
  r4          db 1
  r5          db 1
  r6          db 1
  r7          db 1

  ready       db 1 ; indicates NMI has fired

  ; General pointers
  source      dw 1 ; source pointer
  dest        dw 1 ; destination pointer

  ; Joypad handling
  heldInputs  db 1 ; inputs being held
  newInputs   db 1 ; new inputs pressed this frame

  ; Actual program state variables
  cursorX     db 1
  cursorY     db 1

  entryCursor db 1 ; position of current entry digit (0-3)
  entryDigits db 4 ; one byte per digit being entered

ENDE


  org $200
Boot:
  sei
  ldx #$ff
  txs

  lda #>Screen >> 4
  sta VIDEO

  lda #>Palette
  sta COLORS

  lda #0
  sta heldInputs

  sta cursorX
  sta cursorY

  sta entryCursor

  ; Unpack 1bpp digit tiles as color 3
  lda #<DigitTiles
  sta dest
  lda #>DigitTiles
  sta dest+1

  lda #3
  sta r0
  jsr UnpackDigitTiles

  ; Unpack 1bpp digit tiles as color 2
  lda #<DigitTilesDim
  sta dest
  lda #>DigitTilesDim
  sta dest+1

  lda #2
  sta r0
  jsr UnpackDigitTiles


  lda #<BackgroundTiles2bpp
  sta source
  lda #>BackgroundTiles2bpp
  sta source+1

  lda #<Screen
  sta dest
  lda #>Screen
  sta dest+1

  jsr Draw2bppScreen  ; Draw the main background

  _setw IRQ, VBLANK_IRQ
  cli

Main:
  lda ready     ; Wait until vblank has fired
  beq Main

  lda #0        ; DrawCursor with color 0 to clear it
  sta r0
  jsr DrawCursor

  jsr ReadInputs

  lda #2        ; DrawCursor with color 3 at new location
  sta r0        ;  (might not have changed, but that's fine)
  jsr DrawCursor

  lda #0        ; Clear vblank flag
  sta ready
  jmp Main

; NMI Handler
IRQ:
  lda #1
  sta ready     ; Flag that vblank fired
  rti

; Draw a cursor based on cursorX/cursorY with a given color
DrawCursor:
  color = r0
  width = r1
  height = r2

  lda cursorY
  asl ; Y*16
  asl
  asl
  asl
  adc cursorX ; +X*4
  adc cursorX
  adc cursorX
  adc cursorX
  tax
  lda (CursorDetails), x    ; Read cursor details for selection
  sta dest
  lda (CursorDetails+1), x
  sta dest+1
  lda (CursorDetails+2), x
  sta width
  tay
  lda (CursorDetails+3), x
  sta height

  lda color       ; Draw top line
- sta (dest), y
  dey
  bne -

  ; Note: This actually draws two extra row pixels, but I'm not gonna
  ;  worry about that performance hit right now
- clc             ; advance destination line
  lda dest
  adc #$40
  sta dest
  lda dest+1
  adc #0
  sta dest+1

  lda color       ; draw one row of edge
  ldy width
  sta (dest), y
  ldy #1
  sta (dest), y

  dec height
  bne -

  lda color       ; Draw bottom line
  ldy width
- sta (dest), y
  dey
  bne -

  rts


; Read the joypad and update the display as required
ReadInputs:
  ; First figure out which buttons were newly pressed this frame
  lda heldInputs  ; get previous held inputs
  eor INPUT       ; A now contains inputs which changed state
  and INPUT       ; A now contains inputs which were newly pressed
  sta newInputs
  lda INPUT
  sta heldInputs

  ; Now check for inputs we're interested in
  lda newInputs
  and #PAD_DOWN
  bne @padDown
  lda newInputs
  and #PAD_UP
  bne @padUp 
  lda newInputs
  and #PAD_LEFT
  bne @padLeft
  lda newInputs
  and #PAD_RIGHT
  bne @padRight

  lda newInputs
  and #PAD_A
  bne @padA
  lda newInputs
  and #PAD_B
  bne @padB
  lda newInputs
  and #PAD_C
  bne @padC
@done
  rts
@padDown
  lda cursorY
  ora #0
  beq @done
  dec cursorY
  rts
@padUp
  lda cursorY
  cmp #6
  beq @done
  inc cursorY
  rts
@padLeft
  lda cursorX
  ora #0
  beq @done
  dec cursorX
  rts
@padRight
  lda cursorX
  cmp #3
  beq @done
  inc cursorX
  rts

@padA
  lda cursorY
  cmp #4
  bcs @specialButton
  ; Digit button
  lda entryCursor
  cmp #4        ; check if digits already full
  beq @done
  lda cursorY
  asl ; Y*4
  asl
  adc cursorX   ; A now contains the digit value
  ldx entryCursor
  sta entryDigits, x
  inc entryCursor
  jmp DrawEntry

@specialButton
  rts

@padB
  rts

@padC
  lda entryCursor
  cmp #0
  beq btnPop
  dec entryCursor
  jmp DrawEntry


; Special button handlers
btnPush:
btnPop:
btnSwap:
btnDupe:
btnAdd:
btnSub:
btnMul:
btnDiv:
btnAnd:
btnEor:
btnShl:
btnShr:
  rts


; Draw the current entry digits (and clear entries after the cursor)
DrawEntry:
  digitPtr = r7 ; digit we're drawing

  lda #<EntryAddr   ; setup screen destination address
  sta dest
  lda #>EntryAddr
  sta dest+1

  ; Draw digits up to entryCursor, then erase digits until 4
  lda entryCursor   ; check for zero digit case
  cmp #0
  beq +

  lda #0            ; start printing at the 0th digit
  sta digitPtr

-
  ldx digitPtr
  lda entryDigits, x
  sta r0
  jsr DrawDigit

  inc digitPtr
  lda digitPtr
  cmp entryCursor
  bne -
  ; Fall through to eraseRemainder

+
  lda dest
  cmp #<(EntryAddr + 4 * 4)
  beq @done
  jsr EraseDigit
  jmp +

@done
  rts

; Erase a digit at the current destination
EraseDigit:
  lda #<(DigitEmpty-1)
  sta source
  lda #>(DigitEmpty-1)
  sta source+1
  
  jmp DrawCharacter

; Draw a digit of value r0 at the current dest
DrawDigit:
  digit = r0

  ; Offset base address -1 because dey loops skip the 0th pixel
  lda #<(DigitTiles-1)
  sta source
  lda #>(DigitTiles-1)
  sta source+1

  ; Add 20 bytes per digit
  lda digit
  asl ; digit*4
  asl
  tax
  adc source  ; add the *4 component, which cannot overflow
  sta source
  txa
  asl ; digit*16
  asl
  adc source
  sta source
  lda source+1
  adc #0
  sta source+1
  ; Fall through to DrawCharacter

DrawCharacter:
  destCache = r1 ; 2 bytes to cache dest ptr

  ; Cache initial destination for after drawing
  lda dest
  sta destCache
  lda dest+1
  sta destCache+1

  ; Draw 5 rows of 4 pixels each
  ldx #5
--
  ldy #4
- lda (source), y
  sta (dest), y
  dey
  bne -

  lda source ; advance source a row
  adc #4
  sta source

  lda dest  ; advance destination a row
  adc #$40
  sta dest
  lda dest+1
  adc #0
  sta dest+1

  dex
  bne --

  lda destCache   ; restore dest and advance for next char
  clc
  adc #4
  sta dest
  lda destCache+1
  sta dest+1

  rts


; Unpack a full 2bpp background image to the screen
; source: Pointer to source data (0x400 bytes 2bpp, page aligned)
; dest:   Pointer to destination (Screen start addr, page aligned)
Draw2bppScreen:
  ; Define local aliases for clarity
  wrk = r0  ; working 2bpp byte
  sft = r1  ; working byte with shifted bits
  src = r2  ; source index
  dst = r3  ; destination index

  lda #0
  sta src   ; initialize source offset
  sta dst   ; initialize dest offset
-
  ldy src   ; get source offset
  lda (source), y  ; get 2bpp byte
  sta wrk   ; store working 2bpp byte
 
  ; Unpack 4 bytes
  ldy dst   ; get destination offset
  REPT 4
    lda #0
    sta sft ; initialize value to zero
    lda wrk ; recover working 2bpp byte
    asl     ; shift MSB into carry
    rol sft ; rotate carry into LSB bit
    asl     ; shift MSB into carry
    rol sft ; sft now contains the first unpacked byte
    sta wrk ; backup working 2bpp byte
    lda sft
    sta (dest), y ; write unpacked byte to screen
    iny     ; point at next pixel
  ENDR
  bne +
    inc dest+1  ; increment destination high byte
  + sty dst ; store destination offset

  inc src ; point at next source 2bpp byte
  bne +
    inc source+1  ; increment source high byte
+ lda #(>BackgroundTiles2bpp) + 4
  cmp source+1    ; loop until we've copied 4 pages
  bne -

  rts

; Digit tiles are packed as 1bpp to save ROM space
;  (because we're totally short on that, right?!)
UnpackDigitTiles:
  color = r0  ; color to map set bits to
  wrk = r1  ; working 1bpp byte
  src = r2  ; source index
  dst = r3  ; destination index
  bits = r4  ; bit counter
  bytes = r5 ; byte counter

  lda #<DigitTiles1bpp
  sta source
  lda #>DigitTiles1bpp
  sta source+1

  lda #0
  sta src   ; initialize source offset
  sta dst   ; initialize dest offset

  lda #40   ; unpack 40 bytes
  sta bytes
--
    ldy src   ; get source offset
    lda (source), y  ; get 1bpp byte
    sta wrk   ; store working 1bpp byte
  
    ; Unpack 8 bits
    lda #8
    sta bits

    ldy dst   ; get destination offset
  - lda wrk   ; get working 1bpp byte
      asl       ; shift MSB into carry
      sta wrk   ; backup working 1bpp byte
      lda #0    ; setup for unset bit
      bcc +
      lda color ; replace with color value
    + sta (dest), y
      iny       ; point at next pixel
      bne +
      inc dest+1 ; increment high byte of destination
      +
      dec bits
      bne -     ; loop 8 times
    sty dst

    inc src   ; point at next source 1bpp byte
    dec bytes
    bne --

  rts



  org $0500
Palette:
  ; 4 color palette
  hex 9bbc0f 8bac0f 306230 0f380f

  ; ; ZX Spectrum Palette
  ; hex 000000 0000c0 c00000 c000c0
  ; hex 00c000 00c0c0 c0c000 c0c0c0
  ; hex 000000 0000ff ff0000 ff00ff
  ; hex 00ff00 00ffff ffff00 ffffff

; Details on where to draw the cursor for a given cursorX/cursorY combination
; 4 bytes per button (Screen addr, width, height)
; Note: We might as well just bake in the X component, even though it seems
;  like it'd be easy to calculate, because we have to store the word for the
;  vertical position anyways
MACRO button x,y,w,h
  dw Screen + #y * 64 + #x
  db #w, #h-1
ENDM

CursorDetails:
@zeroToThree
  button 30, 53, 9, 11
  button 38, 53, 9, 11
  button 46, 53, 9, 11
  button 54, 53, 9, 11
@fourToSeven
  button 30, 43, 9, 11
  button 38, 43, 9, 11
  button 46, 43, 9, 11
  button 54, 43, 9, 11
@eightToB
  button 30, 33, 9, 11
  button 38, 33, 9, 11
  button 46, 33, 9, 11
  button 54, 33, 9, 11
@CToF
  button 30, 23, 9, 11
  button 38, 23, 9, 11
  button 46, 23, 9, 11
  button 54, 23, 9, 11
@Ops
  button 30, 16, 9, 8
  button 38, 16, 9, 8
  button 46, 16, 9, 8
  button 54, 16, 9, 8
@math
  button 30, 8, 9, 9
  button 38, 8, 9, 9
  button 46, 8, 9, 9
  button 54, 8, 9, 9
@bitwise
  button 30, 0, 9, 9
  button 38, 0, 9, 9
  button 46, 0, 9, 9
  button 54, 0, 9, 9

; Table with routines to call for each button pressed,
;  aside from the digit buttons which are handled otherwise
ButtonJumpTable:
  dw btnPush
  dw btnPop
  dw btnSwap
  dw btnDupe

  dw btnAdd
  dw btnSub
  dw btnMul
  dw btnDiv

  dw btnAnd
  dw btnEor
  dw btnShl
  dw btnShr

  org $0600
; Packed tile data
DigitTiles1bpp:
  incbin "digitTiles.1bpp"

  org $1000
BackgroundTiles2bpp:
  incbin "backgroundTiles.2bpp"

; Location to unpack digit tiles to
; (Background tiles will be unpacked directly to the screen)
  org $1400
  ;org Screen ; debug, unpack to screen itself!

; Include a padding byte so we can do DigitEmpty-1 and stay on the page
Padding:
  dsb 1

; Empty tile (used to erase old digits)
DigitEmpty:
  dsb 4*5 ; (assembler will zero fill this)

; Primary tiles used for digit entry (color 3)
DigitTiles:
  dsb 4*80

  org $1600 ; new page to simplify DrawDigit
; Dim tiles used for stack (color 2)
DigitTilesDim:
  dsb 4*80