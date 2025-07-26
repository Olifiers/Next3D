INCLUDE "globals.inc"

SECTION code_user    ;; put in main binary
EXTERN  _x1:        db  0
EXTERN  _y1:        db  0
EXTERN  _x2:        db  0
EXTERN  _y2:        db  0
EXTERN  _colour:    db  0


; extern void setCPU(void)
PUBLIC _setCPU
_setCPU:

    ;;NEXTREG 03h,%00110011 ; Set machine to Spectrum Next
    NEXTREG 07h,3           ; Set CPU to 28MHz
    RET


; void initL2(void)
PUBLIC _initL2
_initL2:

    ; Enable Layer 2
    LD BC, 0x123B
    LD A, 2
    OUT (C), A

    ; Setup starting Layer2 16K bank
    NEXTREG 12h, 9
    RET


; void clearL2(unsigned char colour) __z88dk_fastcall
; param is in L if it's 8-bit, HL if it's 16-bit, DEHL if it's 32-bit.
PUBLIC _clearL2
_clearL2:

    LD D, 0             ; D=Y, start at top of the screen

nextY:
    ;Calculate bank number and swap it in
    LD A, D             ; Copy current Y to A
    AND %11100000       ; 32100000 (3MSB = bank number)
    RLCA                ; 21000003
    RLCA                ; 10000032
    RLCA                ; 00000321
    ADD A, START_8K_BANK    ; A=bank number to swap in
    NEXTREG 56h, A      ; Swap bank

    ; Convert DE (yx) to screen memory location starting at $C000
    PUSH DE             ; (DE) will be changed to bank offset
    LD A, D             ; Copy current Y to A
    AND %00011111       ; Discard bank number
    OR 0xC0             ; Screen starts at $C000
    LD D, A             ; D=high byte for $C000 screen memory

    ; Loop X through 0..255, we dont have to deal with bank swapping
    ; here because it only occurs when changing Y
    LD E, 0             ; X=0 (X is E, Y is D)

nextX:
    LD A, L             ; A=current X
    LD (DE), A          ; Use L as colour. Variable is passed via DEHL - L if its 8-bit, HL if its 16-bit, DEHL if its 32-bit.
    LD A, E             ; A=current X
    INC E               ; Increment to next X
    JR NZ, nextX        ; Loop to nextX until E rolls over

    ; Continue with next line or exit
    POP DE              ; Restore DE to coordinates
    INC D               ; Increment to next Y
    LD A, D             ; A=current Y
    CP 192              ; Did we just complete last line?
    JP C, nextY         ; No, continue with next line
    RET


; extern void PlotPixel8K(uint8_t xcoord, uint8_t ycoord, uint8_t colour) __preserves_regs(b,c,iyl,iyh) __z88dk_callee
PUBLIC _PlotPixel8K, PlotPixel8K
_PlotPixel8K:
    	
    ;jp _PlotPixel8K
   	pop hl          ; Loads the stack value (sp) into hl for restoring later
   	pop de          ; Loads next stack entry into e = x, d = y
   	dec sp          ; Moves the stack up, discarding a value and getting us to the third param, colour
   	ex (sp), hl     ; Restores the original value of the stack from hl, and puts the colour on h from the stack 
   	ex de, hl       ; Put y and x into hl and the colour into d
    ld b, d         ; Puts colour into b in order to free d for the drawline

PlotPixel8K:
;===========================================================================
;	HL = YX, D = colour
;===========================================================================

	ld a, h 				    ; 0-31 per bank (8k)
	and %11100000			    ; 3 bits for the 8 banks we can use
	swapnib
	rrca
	add a, START_8K_BANK		; 8k bank for l2
	nextreg MMU_REGISTER_0,a  	; set bank to write into
	ld a, h
	and %00011111 		        ; this is our Y (0-31)
	ld h, a 				    ; put it back in H
	ld (hl), b			        ; draw our pixel (originally d for colour, now b)
	ret


; ===================================================================================
; extern void drawLine(uint8_t x1, uint8_t y1, uint8_t x2, uint8_t y2) __z88dk_callee
PUBLIC _drawLine, drawLine
_drawLine:

   	;pop hl
   	;pop de       ; e = x, d = y
   	;dec sp
   	;ex (sp), hl   ; h = colour 
   	;ex de, hl

drawLine:
    fraction: defb 0        ; Which direction the next pixel travels
    iterations: defb 0      ; Fancy loop word
    stepx: defb 0           ; Direction of travel for the line on x
    stepy: defb 0           ; Direction of travel for the line on y
    steps: defb 0           ; How many pixels in total the line has, ie its lenght

abs_deltax:                 ; C equivalent of deltax = abs(x2 - x1)
    xor a                   ; Cheap clear a and flags
    ld a, (_x2)              ; Loads the line end x
    ld l, a                 ; x2 (end) stored on l
    ld a, (_x1)              ; Loads the line start x
    sbc a, l                ; Subtract the two points (x2 - x1) and return the result
    jp p, abs_deltax_done   ; If bit not set, it's positive, jump to end; otherwise carry on
    neg                     ; Number is negative, invert all bits
abs_deltax_done:
    ld d, a                 ; Assign the value held in a (deltax) to d

step_x:                     ; The C equivalent to stepx = (x1 < x2) ? 1 : -1 -- this has been evaluated once, use those results
    jp c, pos_deltax        ; If no carry set, result is positive, meaning x2 is larger, drawing to right
neg_deltax:
    ld a, -1                ; Sets a to -1
    ld (stepx), a           ; Sets stepx with a negative 1
    jp abs_deltay           ; Skips pos_deltax because we know it's negative
pos_deltax:
    ld a, 1                 ; Sets a to 1
    ld (stepx), a           ; Sets stepx with positive 1

abs_deltay:                 ; C equivalent of deltay = abs(y2 - y)
    xor a                   ; Cheap clear a and flags
    ld a, (_y2)              ; Loads the line end y
    ld l, a                 ; y2 (end) stored on l
    ld a, (_y1)              ; Loads the line start y
    sbc a, l                ; Subtract the two points (y2 - y1) and return the result
    jp p, abs_deltay_done   ; If bit not set, it's positive, jump to end; otherwise carry on
    neg                     ; Number is negative, invert all bits
abs_deltay_done:
    ld e, a                 ; Assign the value held in a to e, so de is deltax and deltay

step_y:                     ; The C equivalent to stepx = (y1 < y2) ? 1 : -1
    jp c, pos_deltay
neg_deltay:
    ld a, -1                ; Sets a to -1
    ld (stepy), a           ; Sets stepy with a negative 1
    jp find_steps
pos_deltay:
    ld a, 1                 ; Sets a to 1
    ld (stepy), a           ; Sets stepy with positive 1

find_steps:                 ; The C equivalent to steps = max(deltax, deltay) -- we stored these in de on abs_deltax and abs_deltay
    ld h, e                 ; Stores lenght in deltay axis on h 
    ld a, d                 ; Load a with lenght in pixels of x axis (deltax)
    cp h                    ; Compares against deltay
    jr c, max_deltay        ; If carry flag is set, deltay is the larger of the two, otherwise deltax is the larger, carry on...
max_deltax:
    ld a, d                 ; Since deltax is the larger, load into a 
    jr max_steps            ; Jumps to the end result of steps with the larger value of the two assigned to steps
max_deltay:
    ld a, e                 ; If we jumped here, deltay is the larger, so load into a and proceed
max_steps:
    ld (steps), a           ; Assign the larger of them, stored in a, to the steps variable at long last

deltax_deltay_loop:         ; The C equivalent to if (deltax > deltay) -- deltax and deltay are stored in de by abs_deltax and abs_deltay
    ld h, d                 ; Loads deltax into h
    ld a, e                 ; Loads deltay into a 
    cp h                    ; Compares deltax and deltay
    jp nc, deltay_larger    ; If there's no carry set, then deltay is larger and we jump, otherwise continue
deltax_larger:              ; deltax is the larger here (deltax > deltay)
    jp deltax_case          ; We go straight to deltax_case and deal with the fraction slope etc.
deltay_larger:              ; deltay is the larger here (deltay >= deltax)
    jp deltay_case          ; We jump to deltay_case and deal with the fraction slope etc.

deltax_case:                ; When the if (deltax > deltay) is true, we need to do the C fraction = deltay - (deltax >> 1)
    ld a, d                 ; For deltax >> 1 we load deltax (d) into a
    srl a                   ; Shift byte to the right by 1 bit to divide by 2
    ld c, a                 ; Load into low byte of bc
    ld a, e                 ; Load deltay into a
    sub c                   ; Subtract deltay - deltax
    ld (fraction), a        ; Loads result into variable fraction
deltax_loop:                ; The C equivalent of for (iterations = 0; iterations <= steps;  iterations++)
    ld a, (iterations)      ; Loads into a how many times we'll loop
    ld h, a                 ; Stores loops into h 
    ld a, (steps)           ; Loads into a the amount of pixels aka steps
    cp h                    ; Compares with the iterations
    ret z                   ; If there are no steps left (zero) we break out and end the routine altogether
    ; DRAWING BIT =========================
    ld a, (_x1)             ; Load x1 coord into a 
    ld l, a                 ; Load x1 coord into l (PlotPixel8K uses hl for yx)
    ld a, (_y1)             ; Load y1 coord into a 
    ld h, a                 ; Load y1 coord into h (PlotPixel8K uses hl for yx)
    ld a, (_colour)         ; Loads a temp colour into a, white
    ld b, a                 ; Loads colour into d (PlotPixel8K uses d for colour)
    call PlotPixel8K        ; Calls our next plotting routine
    ; END DRAWING BIT =====================
check_deltax_fraction:      ; The C code equivalent of if (fraction >= 0)
    xor a                   ; Clears our a and all flags -- we'll check if fraction is less than 0
    ld l, a                 ; Sets l to 0
    ld a, (fraction)        ; Loads fraction into a
    sub l                   ; Compare if fraction is less than 0
    jp m, add_x_fraction    ; If l less than zero, flag on, jump, otherwise continues into sub_x_fraction
sub_x_fraction:             ; The C equivalent of if (fraction >= 0)
    sbc a, d                ; Subtract new fraction value and deltax (d)
    ld (fraction), a        ; Puts new value back into fraction
    ld a, (_y1)             ; We're going to y1 += stepy
    ld h, a                 ; Move y1 to h 
    ld a, (stepy)           ; Load stepy into a 
    add a, h                ; Add y1 and stepy
    ld (_y1), a             ; Stores result back into y1
add_x_fraction:             ; The C equivalent of x1 += stepx & fraction += deltay
    ld a, (fraction)        ; Loads fraction value into a
    add a, e                ; Adds deltay to fraction
    ld (fraction), a        ; Puts new value back into fraction
    ld a, (_x1)             ; The C equivalent of x1 += step_x
    ld h, a                 ; Move a into h
    ld a, (stepx)           ; Loads stepx into a
    add a, h                ; Adds x1 and stepx
    ld (_x1), a             ; Writes new value into x1
deltax_loop_inc:            ; Increments the loop value
    ld a, (steps)           ; Loads the number of steps left into a
    dec a                   ; One less step
    ld (steps), a           ; Stores new steps
    jp deltax_loop          ; Goes back to the start of the loop

deltay_case:                ; Same as deltax, but deltay -- the C equivalent of deltax < deltay in the main loop
	ld a, e
	srl a
	ld c, a
    ld a, d
    sub c
    ld (fraction), a
    
deltay_loop:                ; The C equivalent of for (iterations = 0; iterations <= steps; iterations++)
	ld a, (iterations)
	ld h, a
	ld a, (steps)
	cp h
	ret z
    ; DRAWING BIT =========================
    ld a, (_x1)             ; Load x1 coord into a 
    ld l, a                 ; Load x1 coord into l (PlotPixel8K uses hl for yx)
    ld a, (_y1)             ; Load y1 coord into a 
    ld h, a                 ; Load y1 coord into h (PlotPixel8K uses hl for yx)
    ld a, (_colour)         ; Loads a temp colour into a, white
    ld b, a                 ; Loads colour into d (PlotPixel8K uses d for colour)
    call PlotPixel8K        ; Calls our next plotting routine
    ; END DRAWING BIT =====================
check_deltay_fraction:
    xor a
    ld l, a
    ld a, (fraction)
    sub l
    jp m, add_y_fraction
sub_y_fraction:
    sbc a, e
    ld (fraction), a
    ld a, (_x1)
    ld h, a
    ld a, (stepx)
    add a, h
    ld (_x1), a
add_y_fraction:
	ld a, (fraction)
    add a, d
    ld (fraction), a
    ld a, (_y1)
    ld h, a
    ld a, (stepy)
    add a, h
    ld (_y1), a
deltay_loop_inc:
    ld a, (steps)
    dec a
    ld (steps), a
    jp deltay_loop

    ret                     ; We are done and return to the C code