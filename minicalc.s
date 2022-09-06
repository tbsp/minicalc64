include "64cube.inc"

Screen    EQU $f000
EntryAddr EQU (Screen + 46 * 64 + 4) ; Address to start drawing entry digits
StackAddr EQU (Screen + 36 * 64 + 4) ; Address to start drawing stack digits

MaxEntryStack EQU $20

ENUM $0
  r0          dsb 1 ; general purpose ZP variables
  r1          dsb 1
  r2          dsb 1
  r3          dsb 1
  r4          dsb 1
  r5          dsb 1
  r6          dsb 1
  r7          dsb 1

  ready       dsb 1 ; indicates NMI has fired

  ; General pointers
  source      dsw 1 ; source pointer
  dest        dsw 1 ; destination pointer

  ; Joypad handling
  heldInputs  dsb 1 ; inputs being held
  newInputs   dsb 1 ; new inputs pressed this frame

  ; Actual program state variables
  cursorX     dsb 1
  cursorY     dsb 1

  entryCursor dsb 1 ; position of current entry digit (0-3)
  entryDigits dsb 4 ; one byte per digit being entered

  sPtr        dsb 1 ; stack pointer
  stack       dsb MaxEntryStack ; 2 bytes per entry (always 16bit entries)
                                ;  (only the most recent 5 entries are visible)

  scratch     dsb 4 ; 'large' scratch buffer (used for staging digits to print)

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

  ; minicube64's memory is initialized to zero
  ; lda #0
  ; sta heldInputs

  ; sta cursorX
  ; sta cursorY

  ; sta entryCursor

  lda #0
  sta sPtr

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
  pha
  lda #1
  sta ready     ; Flag that vblank fired
  pla
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

  lda heldInputs
  and #PAD_C
  bne @padCHeld

  lda newInputs
  and #PAD_A
  bne @padA
  lda newInputs
  and #PAD_B
  bne @padB
  lda newInputs
  and #PAD_START
  bne @padStart

@done
  rts
@padDown
  dec cursorY
  lda cursorY
  cmp #$ff
  bne @downDone
  lda #6
  sta cursorY
@downDone
  rts
@padUp
  inc cursorY
  lda cursorY
  cmp #7
  bne @doneUp
  lda #0
  sta cursorY
@doneUp
  rts
@padLeft
  dec cursorX
  jmp @leftRight
@padRight
  inc cursorX
@leftRight
  lda cursorX
  and #%00000011
  sta cursorX
  rts

@padA
  lda cursorY
  sec
  sbc #4
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
  asl ; Y*4
  asl
  adc cursorX
  asl           ; A now contains the jump offset for the handler

  tax
  lda ButtonJumpTable, x
  sta source
  lda ButtonJumpTable+1, x
  sta source+1
  jmp (source)

@padB
  ; backspace OR pop (if no digits entered)
  lda entryCursor
  cmp #0
  beq btnPop

  dec entryCursor
  ldx entryCursor ; we have to zero the digit or partial pushes in the future will be wrong
  lda #0
  sta entryDigits, x
  jmp DrawEntry

@padCHeld
  ; C+A = dupe
  lda newInputs
  and #PAD_A
  bne btnDupe

  ; C+B = swap
  lda newInputs
  and #PAD_B
  bne btnSwap

  ; C+Start = clear stack
  lda newInputs
  and #PAD_START
  bne @padCStart

  rts

@padStart
  jmp btnPush

@padCStart
  lda #0
  sta sPtr
  jmp DrawStack

; Pop the last entry off the stack (if there is one)
btnPop:
  ldx sPtr
  cpx #0
  beq + ; stack empty, do nothing

  dex
  dex
  stx sPtr

  jsr DrawStack ; we've actually altered the stack, so draw it!
+
  rts

; Swap the last two items on the stack (if there are two)
btnSwap:
  ldx sPtr
  cpx #4
  bcc + ; stack doesn't have two items, do nothing
  lda stack-1, x
  sta r0
  lda stack-2, x

  ldy stack-3, x
  sty stack-1, x
  ldy stack-4, x
  sty stack-2, x

  sta stack-4, x
  lda r0
  sta stack-3, x
  jsr DrawStack ; we've actually altered the stack, so draw it!
+
  rts

; Duplicate the last item on the stack (if there is one)
btnDupe:
  ldx sPtr
  cpx #2
  bcc + ; stack doesn't have an item, do nothing
  lda stack-2, x
  sta stack, x
  lda stack-1, x
  sta stack+1, x

  inx
  inx
  stx sPtr
  jsr DrawStack ; we've actually altered the stack, so draw it!
+
  rts

; Push the current entry to the stack, as long as
;  there is an entry and the stack isn't full
btnPush:
  lda sPtr        ; verify that the stack isn't full
  cmp #MaxEntryStack
  bne @checkEntry
  rts

@checkEntry
  lda entryCursor ; verify the user has entered something
  cmp #0
  bne @continue
  rts
@continue
  
  len = r0

  ; If a user types less than 4 digits, we need to zero pad the leading
  ;  nibbles when storing it as 16bit. entryCursor tells us how many digits
  ;  they entered (if they manually entered leading zeroes we don't care).

  ; I can't think of a clever approach, so I'll just move the entered digits
  ;  to the right, insert zeroes, and then treat it as if they entered all 4.

  sec
  sbc #4
  beq @noShifting

  eor #$ff ; invert to get number of shiftLoop passes to perform
  clc
  adc #1
  tay

@shiftPass
  ldx #3
    @shiftLoop
      lda entryDigits-1, x
      sta entryDigits, x
      dex
    bne @shiftLoop
    lda #0
    sta entryDigits, x
    dey
  bne @shiftPass

@noShifting
  ; From here we can act as if they entered 4 digits
  ldx sPtr

  lda entryDigits+2
  asl
  asl
  asl
  asl
  ora entryDigits+3
  sta stack, x  ; store low byte on stack
  inx

  lda entryDigits
  asl
  asl
  asl
  asl
  ora entryDigits+1
  sta stack, x
  dex

  ora stack, x  ; OR high and low bytes
  beq @clearEntry ; entry was zero, clear the entry and don't touch the stack pointer

  inc sPtr
  inc sPtr

  jsr DrawStack ; we've actually altered the stack, so draw it!

@clearEntry
  ; Clear entryDigits and entryCursor
  lda #0
  sta entryDigits
  sta entryDigits+1
  sta entryDigits+2
  sta entryDigits+3
  sta entryCursor
  jmp DrawEntry
@noEntry
  rts

; Cleanup after all stack-affecting operations
FinalizeOperation:

  ; Decrement the stack pointer as long as the last value is zero
  lda stack-1, x  ; compare low/high bytes to see if the result is zero
  ora stack-2, x
  bne ++
  dex             ; result is zero, remove from stack
  dex
  stx sPtr
++

  jsr DrawStack

  rts

; Add the last two items on the stack and push the result
btnAdd:
  ldx sPtr
  cpx #4
  bcs +
  rts ; stack doesn't have two items, do nothing
+

  clc
  lda stack-2, x
  adc stack-4, x
  sta stack-4, x
  lda stack-1, x
  adc stack-3, x
  sta stack-3, x

  dex
  dex
  stx sPtr

  jmp FinalizeOperation

; Subtract the last item on the stack from the second last and push the result
btnSub:
  ldx sPtr
  cpx #4
  bcs +
  rts ; stack doesn't have two items, do nothing
+

  ; TODO: Fix this so (say) 0x10 - 0x20 = 0xfff0 (not 0xf0) - sign extension?
  sec
  lda stack-4, x  ; get low byte of 2nd last value
  sbc stack-2, x  ; subtract low byte of last value
  sta stack-4, x  ; store result
  lda stack-3, x  ; get high byte of 2nd last value
  sbc stack-1, x  ; subtract high byte of last value
  sta stack-3, x  ; store result

  dex
  dex
  stx sPtr

  jmp FinalizeOperation

; Multiply the last two items on the stack and push the result
btnMul:
  ldx sPtr
  cpx #4
  bcs +
  rts ; stack doesn't have two items, do nothing
+

  lda stack-4, x  ; copy 2nd last stack value into scratch space
  sta r0
  lda stack-3, x
  sta r1

  lda #0          ; zero the destination entry
  sta stack-4, x
  sta stack-3, x

  ldy #$10         ; loop 16 times
- asl stack-4, x  ; shift the full 32bit value over once
  rol stack-3, x
  rol stack-2, x
  rol stack-1, x
  bcc ++

  clc
  lda r0
  adc stack-4, x
  sta stack-4, x
  lda r1
  adc stack-3, x
  sta stack-3, x

  lda #0
  adc stack-2, x
  sta stack-2, x

++
  dey
  bne -         ; loop until we're done

  dex
  dex
  stx sPtr

  jmp FinalizeOperation

; Divide the last item on the stack by the second last and push the result
btnDiv:
  ldx sPtr
  cpx #4
  bcs +
  rts ; stack doesn't have two items, do nothing
+

  lda #0
  sta r0  ; initilize remainder
  sta r1

  ldy #$10
@loop:
  asl stack-4, x
  rol stack-3, x
  rol r0
  rol r1
  lda r0
  sec
  sbc stack-2, x
  sta r3
  lda r1
  sbc stack-1, x
  bcc @skip
  sta r1
  lda r3
  sta r0
  inc stack-4, x
@skip:
  dey
  bne @loop

  dex
  dex
  stx sPtr

  jmp FinalizeOperation

; Bitwise AND the last two items on the stack and push the result
btnAnd:
  ldx sPtr
  cpx #4
  bcs +
  rts ; stack doesn't have two items, do nothing
+

  lda stack-3, x
  and stack-1, x
  sta stack-3, x

  lda stack-4, x
  and stack-2, x
  sta stack-4, x

  dex
  dex
  stx sPtr

  jmp FinalizeOperation

; Bitwise OR the last two items on the stack and push the result
btnEor:
  ldx sPtr
  cpx #4
  bcs +
  rts ; stack doesn't have two items, do nothing
+

  lda stack-3, x
  ora stack-1, x
  sta stack-3, x

  lda stack-4, x
  ora stack-2, x
  sta stack-4, x

  dex
  dex
  stx sPtr

  jmp FinalizeOperation

; Shift the last item on the stack left (logically)
btnShl:
  ldx sPtr
  cpx #0
  bne +
  rts ; stack empty, do nothing
+

  asl stack-2, x
  rol stack-1, x

  jmp FinalizeOperation

; Shift the last item on the stack right (logically)
btnShr:
  ldx sPtr
  cpx #0
  bne +
  rts ; stack empty, do nothing
+

  lsr stack-1, x
  ror stack-2, x

  lda stack-1, x  ; compare low/high bytes to see if the result is zero
  ora stack-2, x
  bne ++
  dex             ; result is zero, remove from stack
  dex
  stx sPtr
++

  jmp FinalizeOperation

; Draw the last 5 entires of the stack, from the bottom up, erasing
;  entries up to the top.
DrawStack:
  tmp = r3
  ePtr = r4 ; pointer to stack entry we're drawing
  len = r5 ; length of digits to draw (shared with DrawFourDigits)

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
  ; Offset base address -1 because dey loops skip the 0th pixel
  lda #<(DigitTiles-1)
  sta source
  lda #>(DigitTiles-1)
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

; Packed tile data
DigitTiles1bpp:
  incbin "digitTiles.1bpp"

BackgroundTiles2bpp:
  incbin "backgroundTiles.2bpp"


  org $0C00 ; this must be page aligned for the palette copy to work
Palette:
  ; 4 color palette
  hex 9bbc0f 8bac0f 306230 0f380f

  ; ; ZX Spectrum Palette
  ; hex 000000 0000c0 c00000 c000c0
  ; hex 00c000 00c0c0 c0c000 c0c0c0
  ; hex 000000 0000ff ff0000 ff00ff
  ; hex 00ff00 00ffff ffff00 ffffff

; Location to unpack digit tiles to
; (Background tiles will be unpacked directly to the screen)
  org $0D00
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

  org $0F00 ; new page to simplify DrawDigit
; Dim tiles used for stack (color 2)
DigitTilesDim:
  dsb 4*80