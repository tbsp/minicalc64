; This file contains the input handling and subsequent processing
;  of calculator operations

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
  dw btnOra
  dw btnShl
  dw btnShr

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

  ; Note: held checks go first to give them priority
  lda heldInputs
  and #PAD_C
  beq @noCheld
  jmp @padCHeld
@noCheld

  lda heldInputs
  and #PAD_START
  beq @noStartHeld
  jsr @padStartHeld
@noStartHeld

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
  and #PAD_START
  beq @noStart
  jmp @padStart
@noStart

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
  bne @notEmpty
  jmp btnPop
@notEmpty

  dec entryCursor
  ldx entryCursor ; we have to zero the digit or partial pushes in the future will be wrong
  lda #0
  sta entryDigits, x
  jmp DrawEntry

@padCHeld
  ; C+A = dupe
  lda newInputs
  and #PAD_A
  beq @noCA
  jmp btnDupe
@noCA

  ; C+B = swap
  lda newInputs
  and #PAD_B
  beq @noCB
  jmp btnSwap
@noCB

  ; C+Start = clear stack
  lda newInputs
  and #PAD_START
  beq @noCS
  jmp @padCStart
@noCS

  ; C+up = add
  lda newInputs
  and #PAD_UP
  beq @noCU
  jmp btnAdd
@noCU

  ; C+down = sub
  lda newInputs
  and #PAD_DOWN
  beq @noCD
  jmp btnSub
@noCD

  ; C+left = mul
  lda newInputs
  and #PAD_LEFT
  beq @noCL
  jmp btnMul
@noCL

  ; C+right = div
  lda newInputs
  and #PAD_RIGHT
  beq @noCR
  jmp btnDiv
@noCR

  rts

@padStartHeld
  ; Start+up = AND
  lda newInputs
  and #PAD_UP
  beq @noSA
  pla ; remove ReadInputs return address from stack
  pla
  jmp btnAnd
@noSA

  ; Start+down = OR
  lda newInputs
  and #PAD_DOWN
  beq @noSD
  pla ; remove ReadInputs return address from stack
  pla
  jmp btnOra
@noSD

  ; Start+left = Shift left
  lda newInputs
  and #PAD_LEFT
  beq @noSL
  pla ; remove ReadInputs return address from stack
  pla
  jmp btnShl
@noSL

  ; Start+right = Shift right
  lda newInputs
  and #PAD_RIGHT
  beq @noSR
  pla ; remove ReadInputs return address from stack
  pla
  jmp btnShr
@noSR

  ; return to handle other inputs
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
btnOra:
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

  jmp FinalizeOperation
