; This file contains functions for unpacking 1bpp/2bpp graphical data

; Unpack a full 2bpp background image to the screen
; source: Pointer to source data (0x400 bytes 2bpp, page aligned)
; dest:   Pointer to destination (Screen start addr, page aligned)
Unpack2bppScreen:
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