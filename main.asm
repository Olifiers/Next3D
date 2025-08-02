INCLUDE "globals.inc"

SECTION code_user    ;; put in main binary
EXTERN  _x1:        db  0
EXTERN  _y1:        db  0
EXTERN  _x2:        db  0
EXTERN  _y2:        db  0
EXTERN  _colour:    db  0

; ===========================================================================================
    ; Perenial definitions for trigFillL2 (routine to draw filled triangles)
    ; Triangle line p0-p2
    Tri_ac_r0: defs 1   ; x
    Tri_ac_r1: defs 1   ; Width/deltax
    Tri_ac_r2: defs 1   ; Height/deltay
    Tri_ac_r3: defs 1   ; Error Bresenhams
    Tri_ac_r4: defs 1   ; Quadrant... Direction... Bit? 0 or 1 -- Flags
    ; Triangle line p0-p1
    Tri_ab_r0: defs 1   ; x
    Tri_ab_r1: defs 1   ; Width/deltax
    Tri_ab_r2: defs 1   ; Height/deltay
    Tri_ab_r3: defs 1   ; Error Bresenhams
    Tri_ab_r4: defs 1   ; Quadrant... Direction... Bit? 0 or 1 -- Flags
    ; Triangle point coordinates
    p0: defs 2          ; p0.y and p0.x
    p1: defs 2          ; p1.y and p1.x
    p2: defs 2          ; p2.y and p2.x
; ix: 5 bytes required per line
; +00: x
; +01: deltax
; +02: deltay
; +03: Bresenhams error
; +04: Flags / Quadrant
    XCoord: = 0
    DeltaX: = 1
    DeltaY: = 2
    Error: = 3
    Flags: = 4
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
    NEXTREG 07h,0           ; Set CPU to 28MHz (CURRENTLY SET TO 0 FOR DEBUG!!!)
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
    ; TEMP: SLOW DOWN FOR DEBUG
    ld a, 255
    ;ld e, 255
loopy:
    nop
    nop
    nop
    nop
    nop
    ;dec e
    ;jr nz, loopy
    dec a
    jr nz, loopy
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
    jp PlotPixel8KCol   ; This is the last pixel, draws and quits
draw_line_q2:           ; Here the line is verticalish or perfectly diagonal
    ld (draw_line_q2_m1 + 1), a ; Self-modifies the code to store deltay in the loop
    ld e, b             ; e = deltay
    srl e               ; e = deltay / 2 (Bressenham's error)
; loop uses d as temp, hl bc e
draw_line_q2_l:         ; The main drawline loop for this case
    ld d, h             ; OPTIMISE? Backs up h into d
    call PlotPixel8KCol ; PlotPixel8KCol destroys h, so we need to preserve it
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


;extern void fillTrigL2(Point pt0, Point pt1, Point pt2, uint8_t colour) __z88dk_callee;
; A filled triangle drawing routine
;=================================================================================================
PUBLIC _fillTrigL2, fillTrigL2
_fillTrigL2:
    ;p0: Variable (mem address) to store p0
    ;p1: Variable (mem address) to store p1
    ;p2: Variable (mem address) to store p2
    ; End of definitions
    pop iy              ; Pops sp into iy
    pop hl              ; Pops p0.y and p0.x into hl
    pop de              ; Pops p1.y and p1.x into de
    ld a, d             ; Loads p1.y into a
    cp h                ; Compares p1.y with p0.y
    jr nc, _seq1        ; If p1.y > p0.y, no need to swap, jump
    ex de, hl           ; Swap p0 and p1, de now has the largest y of the two
    ;jp _stop
_seq1:                  ; Here we have p0.y < p1.y | h < d, now we need to check p2
    pop bc              ; Pops p2.y and p2.x into bc
    ld a, b             ; Loads p2.y into a
    cp d                ; Compares p2.y with p1.y
    jr nc, _seq2        ; If p2.y > p1.y, and here we already have p1.y > p0.y, we are done and jump out
    push bc             ; If p2.y < p1.y we need to swap p2 with p1, so we push p2 into stack
    push de             ; Push p1 into stack
    pop bc              ; Pop p1 into bc, becoming the new p2 > p1 on y
    pop de              ; Pop p2 into de, becoming the new p1 < p2 on y
    ld a, d             ; Lastly, we need to compare the new p1 < p0, if not swap them
    cp h                ; Compares the new p1.y with p0.y
    jr nc, _seq2        ; If p1.y > p0.y, no need to swap, jump
    ex de, hl           ; Swap p0 and p1. Now we have p0 < p1 < p2 on the y axis (lh < de < bc)
    ;jp _stop          ; DEBUG: All good here: hl 3C5A | de 7896 | bc B464 -- the correct testing coords ******
_seq2:
    ld (p0), hl         ; Stores p0
    ld (p1), de         ; Stores p1
    ld (p2), bc         ; Stores p2
    pop bc              ; Pops colour value into c
    push iy             ; Restores stack
    ld iyl, c           ; Loads colour into iyl
    ;jp _stop           ; DEBUG: All good here

fillTrigL2:
    ld ix, Tri_ac_r0    ; Sets ix with the address of the first var of the p0-p2 line, so we can access other params sequentially from it
    call pri_tri_p0p2   ; Calls the set up of the long side of the triangle between p0 and p2
    ld ix, Tri_ab_r0    ; Sets ix with the address of the first var of the p0-p1 line, so we can access other params sequentially from it
    call pri_tri_p0p1   ; Calls the set up of the upper short side of the triangle between p0 and p1
    call pri_tri_lc     ; Calls the triangle loop counter
    ld hl, (p0)         ; Loads p0.y onto h
    ;jp _stop
    call draw_triangle  ; call 1f... Find out WHAT THE HELL IS IN ADDRESS 1F OF THIS CODE. WHAT A WAY OF DOING THIS...
    push hl             ; Stores hl on the stack
    ld ix, Tri_ab_r0    ; Sets ix with the address of the first var of the p0-p1 line, so we can access other params sequentially from it
    call pri_tri_p1p2   ; Calls the set up of the lower short side of the triangle between p1 and p2
    call pri_tri_lc     ; Calls the triangle loop counter
    pop hl              ; Restores hl from stack

draw_triangle:
    ld a, b             ; We'll check if the triangle height is zero, first we load the loop counter here from pri_tri_l
    or a
    ret z               ; If it's zero, we are done, return
 
    push bc             ; Stack the loop counter - important because pri_tri_b destroys b (our counter)
    push hl             ; Stacks y
    ld ix, Tri_ac_r0    ; Switches to p0-p2 line
    ;jp _stop            ; DEBUG: ALL SEEMS IN ORDER UP TO HERE
    call pri_tri_b      ; Advances the long p0-p2 line to its next pixel
    ld ix, Tri_ab_r0    ; Switches to p0-p1 line
    call pri_tri_b      ; Advances the short p0-p1 line to its next pixel
    ;jp _stop            ; DEBUG: SEEMS OK UP TO HERE
    pop hl              ; Brings hl back from stack
    push hl             ; Stacks hl again, but hl remains restored
    ld bc, (Tri_ac_r0)  ; Loads the first point of the horizontal line
    ld de, (Tri_ab_r0)  ; Loads the second point of the horizontal line
    
;    call pri_line_hor   ; Draws the line between the two points, filling this step of the triangle
    call pri_line_hor_quick   ; Draws the line between the two points, filling this step of the triangle, only sets bank at start
    pop hl              ; Restores hl
    inc h               ; Increments y *** ORIGINAL CODE IS HL, BUT I THINK IT WANTS TO INCREMENT Y ***
    pop bc              ; Restores loop counter and y index
    dec b               ; Decreases loop / y index *** THIS SURELY WONT WORK AND DOESN'T NEED TO BE 16bits - ORIGINALLY BC, CHANGED TO B
    jr draw_triangle    ; Loops back to top

pri_tri_b:              ; Bresenhams calculations go here
    ld h, (ix + Error)      ; Loads Bresenhams error into h, works for both lines depending on where ix is pointing to (Tri_ac_r0 or Tri_ab_r0)
    ld b, (ix + DeltaX)      ; Loads deltax into b
    ld d, (ix + DeltaY)      ; Loads deltay into d
    bit 1, (ix + Flags)     ; Checks quadrant of line but comparing with the bit on the flag store
    jr z, pri_tri_b_seq2; If zero, skip piece of code below
    ld a, h             ; Loads error into a
    sub d               ; error = error - deltay
    ld h, a             ; Puts error back in h after operation above
    jr nc, pri_tri_b_seq1; If no carry, skip code below, otherwise continues - DEBUG: 3C-0A, NOT ZERO, JUMPS
    add b               ; error = error + deltax
    ld h, a             ; Stores error back on h
    call pri_tri_x      ; Moves the x coordinate ***CHECK THIS!!!***
pri_tri_b_seq1:         ; Jumps to store the error if no carry, skipping the error + deltax step
    ld (ix + Error), h      ; Stores the error into its place for this specific line
    ret                 ; We're done here and return

    
pri_tri_b_seq2:
    call pri_tri_x      ; Moves the x coordinate of the line left or right depending on case
    ld a, h             ; Loads error into a
    sub d               ; Subtracts deltay (error = error - deltay)
    ld h,a
    jr nc, pri_tri_b_seq2; If no carry, loops right back up to work on error again until carry set
    add a,b               ; Carry was set, we now add deltax (error = error + deltax)
    ld (ix + Error), a      ; Stores the error into its place for this specific line
    ret                 ; We're done here and return

pri_tri_x:              ; Moves x coord 1 pixel left or right depending on the line type
    bit 0, (ix + Flags)     ; Sets the direction we're headed towards with x, left or right
    jr nz, pri_tri_x_seq1; If not zero, we're going left, not right, so decrease
    inc (ix + XCoord)        ; Increases line's x
    ret                 ; *** SIMPLIFIED CODE FROM 16bit, THERE WAS MORE TO IT ORIGINALLY, COULD BREAK
pri_tri_x_seq1:
    dec (ix + XCoord)        ; Decreases line's x
    ret                 ; *** SIMPLIFIED CODE FROM 16bit, THERE WAS MORE TO IT ORIGINALLY, COULD_BREAK

; Initialises a block of variables for a line
; ix points at the start of the block
; b points to line start coordinates in p
; c points to line end coordinates in p'
; ix: 5 bytes required per line
; +00: x
; +01: deltax
; +02: deltay
; +03: Bresenhams error
; +04: Flags / Quadrant
pri_tri_p0p1:           ; Initialises the line p0-p1 *** THIS IS HEAVILLY 16bits, NEEDS CAREFUL REVIEW TO SEE IF CHANGES WORK!
    ld hl, (p1)         ; Loads P1.y into h and p1.x into l
    ld de, (p0)         ; Loads p0.y into d and p0.x into e
    ld a, h             ; Loads p1.y into a
    sub d               ; p1.y - p0.y = deltay
    ld d, a             ; Stores deltay into d
    ;jp _stop            ; DEBUG: IT SEEMS IX GOT A BIT MESSED UP HERE... ix+0 HAS 3C02 WHICH ARE VALUES FROM THE OTHER ix START ADDRESS!
    jr pri_tri_i        ; Make sure pri_tri_i takes l and e as x rather than hl and bc as in the original code

pri_tri_p1p2:           ; Initialises the line p1-p2
    ld hl, (p2)         ; Loads P2.y into h and p2.x into l
    ld de, (p1)         ; Loads p1.y into d and p1.x into e
    ld a, h             ; Loads p2.y into a
    sub d               ; p2.y - p1.y = deltay
    ld d, a             ; Stores deltay into d
    jr pri_tri_i        ; Make sure pri_tri_i takes l and e as x rather than hl and bc as in the original code

pri_tri_p0p2:           ; Initialises the line p0-p2 (actually p2-p0)
    ld hl, (p2)         ; Loads P2.y into h and p2.x into l
    ld de, (p0)         ; Loads p0.y into d and p0.x into e
    ld a, h             ; Loads p2.y into a
    sub d               ; p2.y - p0.y = deltay
    ld d, a             ; Stores deltay into d
    ;jp _stop            ; DEBUG: ALL GOOD UP TO HERE
    ;jr pri_tri_i        ; Unnecessary here as pri_tri_i is right below
    

; L and E = x1 and x2, d = deltay
pri_tri_i:              ; Generic primitive triangle initialisation
    
	
	
	ld (ix + XCoord), e      ; Stores first x from calling code back into the address - DEBUG: ix+0 has 5A here
    ld a, l             ; Loads the second x from calling code into a
    sub e               ; Subtract both x to get to deltax
    ld h, a             ; h now has deltax
    ld c, 0x00          ; Zeroes c
    ;jp _stop            ; DEBUG: ALL GOOD UP TO HERE - WITH OUR TEST TRIANGLE deltax is POS, so goes to seq1
    jp p, pri_tri_i_seq1;
    neg                 ; Neg a (which still has deltax)
    ld h, a             ; Stores new neg'ed deltax
    ld c, 0x01          ; c = 1
pri_tri_i_seq1:
    ld a, h             ; Loads deltax into a
    cp d                ; Compares with deltay, if no carry, deltax > deltay
    ;jp _stop            ; DEBUG: ALL GOOD UP TO HERE. deltax < deltay (10 < 120) so we will set a carry here
    jr nc, pri_tri_i_seq2   ; Jumps to seq2 in the code skipping next part
    ld a, h             ; If deltay > deltax we swap them around
    ld h, d             ; Swapping...
    ld d, a             ; deltax and deltay are now swapped
    set 1, c            ; And we set the deltay > deltax flag this way
    ;jp _stop            ; DEBUG: ALL GOOD UP TO HERE. deltay (d) has 0A, deltax (h) has 78, x (ix+0) has 5A, c has 02
pri_tri_i_seq2:
    ld (ix + DeltaX), h      ; We store back deltax into its place in memory for the current line
    ld (ix + DeltaY), d      ; We store back deltay into its place in memory for the current line
    ;SRL H RR L !!! *** NEEDS DOUBLE CHECKING THIS
    srl h               ; error = error / 2
    
    ld (ix + Error), h      ; New error stored into its place in memory for the current line
    ld (ix + Flags), c      ; New flags stored into its place in memory for the current line
    ;jp _stop            ; DEBUG: ALL GOOD UP TO HERE. x (ix+0) has 5A, deltax (ix+1) has 78, deltay (ix+2) has 0A, error (ix+3) has 3C (half of deltay), flags (ix+4) has 02, ix has 94F8
    ret

pri_tri_lc:
    bit 1, (ix + Flags)     ; Return the correct loop counter by checking the flag state storage, if the value in ix + Flags has a bit 1 on pos 1
    jr z, pri_tri_lc_seq; If loop counter returns zero, jump
    ld b, (ix + DeltaX)      ; Loads deltax into b for looping
    ret
pri_tri_lc_seq:
    ld b, (ix + DeltaY)      ; Loads deltay into b for looping
    ;jp _stop            ; DEBUG: SEEMS LIKE ALL GOOD UP TO HERE?
    ret 

    ; E and C 

;================================================================================================= 
; Quicker version setting bank at the start
; L = Y, IYL = colour
; C = X start, E = X end 
;================================================================================================= 
pri_line_hor_quick:          	; A horizontal line drawing routine
    ld a, e         	    	; Loads x2 into a
    sub c   		            ; Subtracts x1 to get length of line
    ret z               		; If x2 - x1 = 0, we have no line to draw, end it.
	jr c, pri_line_back		; If carry is set, we are drawing right to left, so jump to pri_line_back

	ld b, a             		; Stores length of line as loop counter in b
    ld l,c
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

; do our loop
@qloop:
	ld (hl), a			        ; Draw our pixel
    inc l               		; Increases x1
    djnz @qloop 					; Decrease loop counter and jump back to draw next pixel
    ret

pri_line_back:
;=================================================================================================
	neg
	ld b, a             		; Stores length of line as loop counter in b
    ld l,c
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

; do our loop
@qloop2:
	ld (hl), a			        ; Draw our pixel
    dec l               		; Increases x1
    djnz @qloop2 					; Decrease loop counter and jump back to draw next pixel
    ret


;================================================================================================= 
;    HL = YX
;================================================================================================= 
pri_line_hor:           ; bc has x1 on c; de has x2 on e
    ld a, e             ; Loads x2 into a
    sub c               ; Subtracts x1 to get length of line
    ret z               ; If x2 - x1 = 0, we have no line to draw, end it.

    ld b, a             ; Stores length of line as loop counter in b
    ld l, c             ; h already has y; loads x1 into l, so PlotPixel8K has y,x into h,l
pri_line_hor_loop:      ; Start drawing!
    ;jp _stop            ; DEBUG: OUR FIRST TIME HERE WE HAVE x1=5A, x2=5B, y=3C CORRECT | BUT x1 ISN'T INCREASING AFTER THIS IS EXECUTED...
    ld c, h             ; Let's preserve h since PlotPixel8K destroys it
    call PlotPixel8K    ; Draw! Yey!
    ld h, c             ; Restore h after PlotPixel8K's execution
    inc l
    djnz pri_line_hor_loop; Decrease loop counter and jump back to draw next pixel
    ret

;================================================================================================= 
;extern void fillTrigL2(Point pt0, Point pt1, Point pt2, uint8_t colour) __z88dk_callee;
; A filled triangle drawing routine
;=================================================================================================
;PUBLIC _fillTrigL2, fillTrigL2
;_fillTrigL2:            ; The triangle drawns from top to bottom, we need to ensure p0 < p1 < p2 on its y, so we order it as such
;    p0: defw 0          ; Variable (mem address) to store p0
;    p1: defw 0          ; Variable (mem address) to store p1
;    p2: defw 0          ; Variable (mem address) to store p2
;    x01: defw 0xFA00    ; Memory start of x01 array is 64000
;    x12: defw 0xFAC0    ; Memory start of x12 array is 64192
;    x02: defw 0xFB80    ; Memory start of x02 array is 64384
;    pop iy              ; Pops sp into iy
;    pop hl              ; Pops p0.y and p0.x into hl
;    pop de              ; Pops p1.y and p1.x into de
;    ld a, d             ; Loads p1.y into a
;    cp h                ; Compares p1.y with p0.y
;    jr nc, _seq1        ; If p1.y > p0.y, no need to swap, jump
;    ex de, hl           ; Swap p0 and p1, de now has the largest y of the two
;    ;jp _stop
;_seq1:                  ; Here we have p0.y < p1.y | h < d, now we need to check p2
;    pop bc              ; Pops p2.y and p2.x into bc
;    ld a, b             ; Loads p2.y into a
;    cp d                ; Compares p2.y with p1.y
;    jr nc, _seq2        ; If p2.y > p1.y, and here we already have p1.y > p0.y, we are done and jump out
;    push bc             ; If p2.y < p1.y we need to swap p2 with p1, so we push p2 into stack
;    push de             ; Push p1 into stack
;    pop bc              ; Pop p1 into bc, becoming the new p2 > p1 on y
;    pop de              ; Pop p2 into de, becoming the new p1 < p2 on y
;    ld a, d             ; Lastly, we need to compare the new p1 < p0, if not swap them
;    cp h                ; Compares the new p1.y with p0.y
;    jr nc, _seq2        ; If p1.y > p0.y, no need to swap, jump
;    ex de, hl           ; Swap p0 and p1. Now we have p0 < p1 < p2 on the y axis (lh < de < bc)
;    ;jp _stop           DEBUG: All good here: hl 3C5A | de 7896 | bc B464 -- the correct testing coords ******
;_seq2:
;    ld (p0), hl         ; Stores p0
;    ld (p1), de         ; Stores p1
;    ld (p2), bc         ; Stores p2
;    pop bc              ; Pops colour value into c
;    push iy             ; Restores stack
;    ld iyl, c           ; Loads colour into iyl
;    ;jp _stop           DEBUG: All good here
;
;fillTrigL2:             ; We begin with certainty that p0 < p1 < p2 on y
;    nop
;interpolate_p0:         ; Interpolates p0.y, p0.x (hl) with p1.y, p1.x (de) and fills memory in iy with the interpolations
;    ld iy, x01          ; Sets the starting address of the array x01 at 64000 (FA00)
;    ld a, d             ; Loads p1.y into a for comparison with p0.y
;    cp h                ; Compares p0.y with p1.y
;    jr nz, inter_p0_1   ; If Z is not set, jump. If it is, p0.y = p1.y, do we just return p0.x
;    ld (iy), l          ; Return p0.x only, that's all, by loading it into the array x01 kept in memory
;    ret                 ; We are done
;inter_p0_1:
;    sub h               ; We have got p0.y on h and p1.y on d as we just compared them, thus a now has deltay
;    ld b, a             ; Loads deltay into b
;    ld a, e             ; Loads p1.x into e
;    sub l               ; Subtracts p1.x - p0.x to get deltax
;    ld c, a             ; Loads deltax into c
;    ;Self modify here to reuse deltax befor we destroy it
;    
;    ret