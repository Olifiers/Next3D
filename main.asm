INCLUDE "globals.inc"

SECTION code_user    ;; put in main binary
EXTERN  _x1:        db  0
EXTERN  _y1:        db  0
EXTERN  _x2:        db  0
EXTERN  _y2:        db  0
EXTERN  _colour:    db  0


; extern void stop(void)
; A debugging tool. Jump here (jp _stop) from your code so you can stop and inspect registers etc.
; ============================================================================================
PUBLIC _stop
_stop:

    jp _stop


; extern void setCPU(void)
; Sets the Next CPU to maximum speed, 28MHz
; ===========================================================================================
PUBLIC _setCPU
_setCPU:

    ;;NEXTREG 03h,%00110011 ; Set machine to Spectrum Next
    NEXTREG 07h,3           ; Set CPU to 28MHz
    RET


; void initL2(void)
; Initialises the Layer 2 Next screen mode into 256x192 256 colours in front of the ULA screen
; ============================================================================================
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
; A terrible screen cleaning routine we'll have to optmise the hell out of soon!
; =============================================================================================
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


; extern void PlotPixel8K(uint8_t xcoord, uint8_t ycoord, uint8_t colour) __z88dk_callee
; Generic plotting routine that can be called from C
; ========================================================================================
PUBLIC _PlotPixel8K, PlotPixel8K
_PlotPixel8K:
    	
    ;jp _PlotPixel8K
   	pop hl          ; Loads the stack value (sp) into hl for restoring later and moves variables into top of stack
   	pop de          ; Loads next stack entry into e = x, d = y
   	dec sp          ; Moves the stack up 1 byte, discarding a value and getting us to the third param, colour
   	ex (sp), hl     ; Restores the original value of the stack from hl, and puts the colour on h from the stack 
   	ex de, hl       ; Put y and x into hl and the colour into d
    ld iyl, d       ; Puts colour into iyl in order to free d for the drawline

PlotPixel8K:
;===========================================================================
;	HL = YX, IYL = colour -- IMPORTANT: DESTROYS H (and A)
;===========================================================================

	ld a, h 				    ; 0-31 per bank (8k)
	and %11100000			    ; 3 bits for the 8 banks we can use
	swapnib
	rrca
	add a, START_8K_BANK		; 8L bank for L2
	nextreg MMU_REGISTER_0,a  	; Set bank to write into
	ld a, h
	and %00011111 		        ; This is our y (0-31)
	ld h, a 				    ; Puts y it back in h
    ld a, iyl                   ; Loads colour from iyl into a
	ld (hl), a			        ; Draw our pixel
	ret


PlotPixel8KCol:
;===========================================================================
; This has no C calls and must be called from assembly!!!
;
;	HL = YX -- IMPORTANT: DESTROYS H (and A)
; We preset the colour so we can use it directly
; by setting plotPixel8KColour with self-modifying code
;===========================================================================

	ld a, h 				    ; 0-31 per bank (8k)
	and %11100000			    ; 3 bits for the 8 banks we can use
	swapnib
	rrca
	add a, START_8K_BANK		; 8L bank for L2
	nextreg MMU_REGISTER_0,a  	; Set bank to write into
	ld a, h
	and %00011111 		        ; This is our y (0-31)
	ld h, a 				    ; Puts y it back in h
plotPixel8KColour:	
    ld (hl), 0			        ; Draw our pixel (colour is going to be set by automodifying the code)
	ret    


; extern void drawL2(uint8_t x1coord, uint8_t y1coord, uint8_t x2coord, uint8_t y2coord, uint8_t colour) __z88dk_callee
; A Bresenham's line drawing catering for every type of line and direction, inspired by a bunch of Speccy algos online
; ====================================================================================================================
; Credits to Andy Dansby (https://github.com/andydansby/bresenham_torture_test/blob/main/bresenham_line3.asm)
; Credits to Dean Belfield (http://www.breakintoprogram.co.uk)
; Credits to Gabrield Gambetta's great book 'Computer Graphics From Scratch'
; Credits to Mike Flash Ware for helping optimise it!
PUBLIC _drawL2, drawL2
_drawL2:

; or, even better, set the colour in AF, and store that in the PlotPixel8KCol
    pop bc          ; Loads the stack value (sp) into bc for restoring later and moves variables into top of stack
    pop hl          ; Loads y1x1 into hl
    pop de          ; Loads y2x2 into de
    pop iy          ; Use iyl for the colour
    push bc         ; Restores the stack value from bc

drawL2:
;=========================================================================
;   HL = Y1X1, DE = Y2X2, IYL = colour
;=========================================================================
    ld a, iyl           ; Loads colour into a
    ld (plotPixel8KColour + 1), a ; Store the colour in the plotPixel8KColour through self-modifying the code
    ld a, d             ; Loads y2 into a. We'll see if we need to swap coords to draw downwards
    cp h                ; Compares y1 with y2
    jr nc, draw_line_1  ; No need to swap the coords, jump
    ex de, hl           ; Swapped coordinates to ensure y2 > y1, so we draw downwards
draw_line_1:
    ld a, d             ; Loads y2 into a
    sub h               ; y2 - y1
    ld b, a             ; b becomes deltay
    ld a, e             ; Loads x2 into a
    sub l               ; x2 - x1, a now contains deltax
    jr c, draw_line_x1  ; If carry is set (x2 - x1 is negative) we are drawing right to left
    ld c, a             ; c becomes deltax
    ld a, 0x2C          ; Replaces original code above to increase x1 as we're drawing left to right. 0x2C is inc l, and we modify the code to have this
    jr draw_line_x2     ; Skips next part of the code
draw_line_x1:
    neg                 ; deltax in a is negative, make it positive
    ld c, a             ; c becomes deltax
    ld a, 0x2D          ; Replaces original code above to decrease x1 as we're drawing right to left. Self-modifying, puts dec l into the code
draw_line_x2:
    ld (draw_line_q1_m2), a ; a contains either inc l or dec l, and modifies the code accordingly
    ld (draw_line_q2_m2), a ; Same as above for verticalish lines
    ld a, b             ; We'll check if deltay (b) and deltax (ixl) are 0
    or c                ; Checking...
    jp z, PlotPixel8KCol    ; When reaching zero, we're done, draw last pixel
    ; STATUS: b = deltay | c = deltax | d is free
draw_line_q:            ; Find out what kind of diagonal we're dealing with, if horizontalish or verticalish
    ld a, b             ; Loads deltay into a
    cp c                ; Compares with deltax
    jr nc, draw_line_q2 ; If no cary, line is verticalish (or perfectly diagonal)
draw_line_q1:
    ld a, c             ; a becomes deltax
    ld (draw_line_q1_m1 + 1), a ; Self-modifying code: loads deltax onto the value of the opcode, in this case the loop
    ld c, b             ; c becomes deltay
    ld b, a             ; b becomes deltax for the loop counter
    ld e, b             ; e becomes deltax temporarily...
    srl e               ; now e = deltax / 2 -- aka Bresenham's error
; loop uses d as temp, hl bc e
draw_line_q1_l:
    ld d, h             ; OPTIMISE? Backs up h into d
    call PlotPixel8KCol ; PlotPixel8KCol destroys h, so we need to preserve it
    ld h, d             ; OPTIMISE? Restores h from d
    ld a, e             ; Loads Bresenham's error into a
    sub c               ; error - deltay
    ld e, a             ; Stores new error value into e
    jr nc, draw_line_q1_m2  ; If there's no cary, jump
draw_line_q1_m1:
    add a, 0            ; This 0 here will be modified by the self-modifying code above e = e + deltax
    ld e, a             ; Stores new error e = e + deltax back into e
    inc h               ; Increases line slope by adding to y1
draw_line_q1_m2:        ; This either increases or decreases l by the self modified code that targeted this
    inc l               ; Self-modified code: It will be either inc l or dec l depending on direction of horizontal drawing
draw_line_q1_s:         ; Tests to loop and keep drawing line
    djnz draw_line_q1_l ; Loops until line is drawn and zero flag set
    jp PlotPixel8KCol    ; This is the last pixel, draws and quits
draw_line_q2:           ; Here the line is verticalish or perfectly diagonal
    ld (draw_line_q2_m1 + 1), a ; Self-modifies the code to store deltay in the loop
    ld e, b             ; e = deltay
    srl e               ; e = deltay / 2 (Bressenham's error)
; loop uses d as temp, hl bc e
draw_line_q2_l:         ; The main drawline loop for this case
    ld d, h             ; OPTIMISE? Backs up h into d
    call PlotPixel8KCol    ; PlotPixel8KCol destroys h, so we need to preserve it
    ld h, d             ; OPTIMISE? Restores h from d
    ld a, e             ; Adds deltax to the error
    sub c               ; As above
    jr nc, draw_line_q2_s   ; If we don't get a carry, skip the next part
draw_line_q2_m1:
    add a, 0            ; This is a target of self-modified code: e = e + deltax
draw_line_q2_m2:
    inc l               ; Self-modified code: It will be either inc l or dec l depending on direction of horizontal drawing
draw_line_q2_s:
    ld e, a             ; Restores the error value back in
    inc h               ; Increases y1
    djnz draw_line_q2_l ; While zero flag not set, loop back to main loop
    jp PlotPixel8KCol   ; This is the last pixel drawn, all done


;extern void trigL2(Point pt0, Point pt1, Point pt2, uint8_t colour) __z88dk_callee;
; A triangle wireframe drawing routine, highly optimised (I hope!)
;=================================================================================================
PUBLIC _trigL2, trigL2
_trigL2:
    pop iy              ; Pops sp into iy
    pop hl              ; Pops p0.y and p0.x into hl
    ;ld (l1 + 1), hl     ; Self-modifying code, loads hl into l1: hl (l1 start)
    ld (l3 + 4), hl     ; Self-modifying code, loads hl into l3: de (l3 end)
    pop de              ; Pops p1.y and p1.x into de
    ;ld (l1 + 4), de     ; Self-modifying code, loads de into l1: de (l1 end)
    ld (l2 + 1), de     ; Self-modifying code, loads de into l2: hl (l2 start)
    pop bc              ; Pops p2.y and p2.x into bc
    ld (l2 + 4), bc     ; Self-modifying code, loads bc into l2: de (l2 end)
    ld (l3 + 1), bc     ; Self-modifying code, loads cd into l3: hl (l3 start)
    pop bc              ; Pops colour value into c
    push iy             ; Restore stack
    ld iyl, c           ; Loads colour into iyl

trigL2:
l1:                     ; Line 1
    call drawL2         ; Because hl and de are already set correctly, just draw it
l2:                     ; Line 2
    ld hl, 0            ; Self-modified target from above
    ld de, 0            ; Seld-modified target from above
    call drawL2         ; Draws line with the modified hl and de with the self-modifying code
l3:                     ; Line 3
    ld hl, 0            ; Self-modified target from above
    ld de, 0            ; Self-modified target from above
    jp drawL2           ; Draws line with the modified hl and de with the self-modifying code
    ret                 ; We're done, return nicely


;PUBLIC _trigL2, trigL2
;_trigL2:
;    pop iy              ; Pops sp into iy
;    pop hl              ; Pops p0.y and p0.x into hl
;    ld (iy + 02h), l    ; Loads p0.x into iy + 2
;    ld (iy + 03h), h    ; Loads p0.y into iy + 3
;    pop de              ; Pops p1.y and p1.x into de
;    ld (iy + 04h), e    ; Loads p1.x into iy + 4
;    ld (iy + 05h), d    ; Loads p1.y into iy + 5
;    pop bc              ; Pops p2.y and p2.x into bc
;    ld (iy + 06h), c    ; Loads p2.x into iy + 6
;    ld (iy + 07h), b    ; Loads p2.y into iy + 7
;    pop bc              ; Pops colour into c
;    ld ixl, c           ; Loads colour into ixl
;
;trigL2:
;    call drawL2         ; hl and de already have p0.y, p0,x, p1,y and p1,x
;    ld l, (iy + 04h)
;    ld h, (iy + 05h)
;    ld e, (iy + 06h)
;    ld d, (iy + 07h)
;    call drawL2
;    ld l, (iy + 06h)
;    ld h, (iy + 07h)
;    ld e, (iy + 02h)
;    ld d, (iy + 03h)
;    call drawL2
;    ld a, 255
;    push iy
;    ret