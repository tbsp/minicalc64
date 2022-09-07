; This file contains functions/data for drawing the calculator,
;  as well as the graphical data itself.

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


; Draw the last 5 entires of the stack, from the bottom up, erasing
;  entries up to the top.
DrawStack:
  tmp = r3
  ePtr = r4 ; pointer to stack entry we're drawing
  len = r5 ; length of digits to draw (shared with DrawFourDigits)

  ; Setup the tiles to draw with
  lda #<(DigitTilesDim-1)
  sta a0
  lda #>(DigitTilesDim-1)
  sta a0+1

  lda #<StackAddr
  sta dest
  lda #>StackAddr
  sta dest+1

  ldx sPtr        ; copy stack ptr to working ptr
  stx ePtr

@rowLoop
  lda #0
  sta len         ; init length counter for this row

  ldx ePtr
  cpx #0
  beq @drawRow    ; draw empty row (length == 0)

  dex             ; step down the stack one entry
  dex
  stx ePtr

  ; Unpack entry into scratch as up to 4 bytes (depending on leading zeroes)
  lda stack+1, x  ; get high byte
  tay             ; cache for low nibble below
  lsr             ; >> 4
  lsr
  lsr
  lsr
  cmp #0          ; check if it's a leading zero
  beq @lz0        ; don't add it to scratch or increment the length if it's zero
  ldx len
  sta scratch, x  ; store the digit at the current length offset
  inc len
@lz0
  tya             ; recover cached high byte
  and #$0f        ; mask off high nibble
  tay             ; cache for leading zero check
  ora len         ; check if the value as well as the length are zero
  beq @lz1        ; the value is zero, and our length is zero, so don't add the digit
  ldx len
  sty scratch, x
  inc len
@lz1

  ldx ePtr
  lda stack, x    ; get low byte
  sta tmp         ; cache the low byte
  lsr             ; >> 4
  lsr
  lsr
  lsr
  tay             ; cache for leading zero check
  ora len
  beq @lz2
  ldx len
  sty scratch, x
  inc len
@lz2
  lda tmp         ; recover cached low byte
  and #$0f        ; mask off high nibble
  tay             ; cache for leading zero check
  ora len
  beq @lz3
  ldx len
  sty scratch, x
  inc len
@lz3

@drawRow
  jsr DrawFourDigits

  ; Adjust destination for next row
  sec
  lda dest
  sbc #<($40 * 6 + 4 * 4)
  sta dest
  lda dest+1
  sbc #>($40 * 6 + 4 * 4)
  sta dest+1

  lda dest+1
  cmp #>(StackAddr - ($40 * 6 + 4 * 4) * 5) ; why doesn't 6 work here?
  bne @rowLoop    ; keep drawing until we've reached the top row

@done
  rts

; Draw the current entry digits (and clear entries after the cursor)
DrawEntry:
  ; Copy current entry to scratch RAM
  lda entryDigits
  sta scratch
  lda entryDigits+1
  sta scratch+1
  lda entryDigits+2
  sta scratch+2
  lda entryDigits+3
  sta scratch+3

  ; Setup the tiles to draw with
  lda #<(DigitTiles-1)
  sta a0
  lda #>(DigitTiles-1)
  sta a0+1

  lda entryCursor
  sta r5

  lda #<EntryAddr   ; setup screen destination address
  sta dest
  lda #>EntryAddr
  sta dest+1

  ; Fall through into DrawFourDigits

; Draw 4 digits from 'scratch' in the ZP
DrawFourDigits:
  length = r5 ; how many digits to draw (remainder will be cleared)

  digitPtr = r6 ; digit we're drawing
  endAddr = r7 ; address that we're done drawing at

  lda dest
  clc
  adc #(4*4)
  sta endAddr
  
  ; Draw digits up to entryCursor, then erase digits until 4
  lda length        ; check for zero digit case
  cmp #0
  beq +

  lda #0            ; start printing at the 0th digit
  sta digitPtr

-
  ldx digitPtr
  lda scratch, x
  sta r0
  jsr DrawDigit

  inc digitPtr
  lda digitPtr
  cmp length
  bne -
  ; Fall through to erase the rest of the entries

+
@eraseLoop
  lda dest
  cmp endAddr
  beq @done
  jsr EraseDigit
  jmp @eraseLoop

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
  ; Setup source pointer based on normal/dim tiles to draw with
  lda a0
  sta source
  lda a0+1
  sta source+1

  digit = r0

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
  lda source+1
  adc #0
  sta source+1

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
  button 31, 54, 7, 9
  button 39, 54, 7, 9
  button 47, 54, 7, 9
  button 55, 54, 7, 9
@fourToSeven
  button 31, 44, 7, 9
  button 39, 44, 7, 9
  button 47, 44, 7, 9
  button 55, 44, 7, 9
@eightToB
  button 31, 34, 7, 9
  button 39, 34, 7, 9
  button 47, 34, 7, 9
  button 55, 34, 7, 9
@CToF
  button 31, 24, 7, 9
  button 39, 24, 7, 9
  button 47, 24, 7, 9
  button 55, 24, 7, 9
@Ops
  button 31, 17, 7, 6
  button 39, 17, 7, 6
  button 47, 17, 7, 6
  button 55, 17, 7, 6
@math
  button 31, 9, 7, 7
  button 39, 9, 7, 7
  button 47, 9, 7, 7
  button 55, 9, 7, 7
@bitwise
  button 31, 1, 7, 7
  button 39, 1, 7, 7
  button 47, 1, 7, 7
  button 55, 1, 7, 7


; Packed tile data
DigitTiles1bpp:
  ; packed with: png2cube.py -d 1 -o digitTiles.1bpp digits.png
  incbin "digitTiles.1bpp"

BackgroundTiles2bpp:
  ; packed with: png2cube.py -d 2 -o backgroundTiles.2bpp background.png
  incbin "backgroundTiles.2bpp"
