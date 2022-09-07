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

  a0          dsw 1 ; general purpose address

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

  jsr Unpack2bppScreen  ; Draw the main background

  _setw IRQ, VBLANK_IRQ
  cli

Main:
  lda ready     ; Wait until vblank has fired
  beq Main

  lda #1        ; DrawCursor with color 0 to clear it
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


; Include additional source files
include operations.s
include drawing.s
include unpack.s


  org $0D00 ; this must be page aligned for the palette copy to work
Palette:
  ; 4 color palette
  hex 000000 221133 338877 ffffff

; Location to unpack digit tiles to
; (Background tiles will be unpacked directly to the screen)
  org $0E00

  dsb 1 ; Include a padding byte so we can use <addr>-1 and stay on the page

; Empty tile (used to erase old digits)
DigitEmpty:
  dsb 4*5 ; (assembler will zero fill this)

; Primary tiles used for digit entry (color 3)
DigitTiles:
  dsb 4*80

; Dim tiles used for stack (color 2)
  org $1000 ; new page to simplify DrawDigit
  dsb 1 ; padding byte

DigitTilesDim:
  dsb 4*80