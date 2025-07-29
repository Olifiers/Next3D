
/*
3D Next engine, by Henrique Olifiers
*/

#include <arch/zxn.h>
#include <stdio.h>
#include <stdlib.h>

#define SCREEN_WIDTH 255       // Our resolution
#define SCREEN_HEIGHT 191

static int loop;
uint8_t x = 0;
uint8_t y = 0;
extern uint8_t colour = 15;       // One byte var
extern uint8_t x1 = 20;
extern uint8_t y1 = 10;
extern uint8_t x2 = 90;
extern uint8_t y2 = 80;

typedef struct {           // The Point struct containing the coords of each triangle's vertex
    uint8_t x;
    uint8_t y;
} Point;

Point p0 = {100, 10};          // Initialises the trig points with zero (not really needed, but best to)
Point p1 = {150, 120};
Point p2 = {170, 60};
Point swap = {0, 0};

extern void stop(void);                                             // asm to stop program for debugging
extern void setCPU(void);                                           // asm to set CPU parameters to 28MHz etc.
extern void initL2(void);                                           // asm to initialise Layer2 screen mode, addresses, banks etc.
extern void clearL2(uint8_t colour) __z88dk_fastcall;               // asm to clear the Layer2 screen (horribly done)
extern void PlotPixel8K(uint8_t xcoord, uint8_t ycoord, uint8_t colour) __z88dk_callee;
extern void drawL2(uint8_t x1coord, uint8_t y1coord, uint8_t x2coord, uint8_t y2coord, uint8_t colour) __z88dk_callee;
extern void trigL2(Point pt0, Point pt1, Point pt2, uint8_t colour) __z88dk_callee;
uint8_t max(uint8_t a, uint8_t b);                                  // max implementation, finds the larger of two numbers
void drawFillTrig(Point pt0, Point pt1, Point pt2, uint8_t colour);    // Filled triangle routine declaration
uint8_t interpolate(uint8_t i0, uint8_t d0, uint8_t i1, uint8_t d1, uint8_t* values);


uint8_t max(uint8_t a, uint8_t b)                                   // Finds larger between two values
{
    if(a > b){
        return a;
    }else{
        return b;
    }
}

uint8_t interpolate(uint8_t i0, uint8_t d0, uint8_t i1, uint8_t d1, uint8_t* values) {
    if (i0 == i1) {
        values[0] = d0;
        return 1;
    }

    uint8_t length = i1 - i0 + 1;
    int16_t a = ((int16_t)d1 - d0) * 100 / (i1 - i0);  // fixed-point step (×100)
    int16_t d = d0 * 100;

    for (uint8_t i = 0; i < length; i++) {
        values[i] = (d + 50) / 100;  // round to nearest integer
        d += a;
    }

    return length;
}

// ==========================
// Oli's filled triangle algo
// ==========================
void drawFillTrig(Point p0, Point p1, Point p2, uint8_t color) {
    // Sort points by y, with the smallest y becoming p0, then p1 and p2
    Point tmp;
    if (p1.y < p0.y) { tmp = p0; p0 = p1; p1 = tmp; }
    if (p2.y < p0.y) { tmp = p0; p0 = p2; p2 = tmp; }
    if (p2.y < p1.y) { tmp = p1; p1 = p2; p2 = tmp; }

    // Interpolated x-values
    uint8_t x01[SCREEN_HEIGHT], x12[SCREEN_HEIGHT], x02[SCREEN_HEIGHT];
    uint8_t len01 = interpolate(p0.y, p0.x, p1.y, p1.x, x01);
    uint8_t len12 = interpolate(p1.y, p1.x, p2.y, p2.x, x12);
    uint8_t len02 = interpolate(p0.y, p0.x, p2.y, p2.x, x02);

    // Merge x01 and x12 into x012
    uint8_t x012[SCREEN_HEIGHT];
    for (uint8_t i = 0; i < len01 - 1; i++) {
        x012[i] = x01[i];
    }
    for (uint8_t i = 0; i < len12; i++) {
        x012[len01 - 1 + i] = x12[i];
    }

    // Choose left/right
    uint8_t* x_left;
    uint8_t* x_right;
    uint8_t total_len = len02;
    uint8_t m = total_len / 2;
    if (x02[m] < x012[m]) {
        x_left = x02;
        x_right = x012;
    } else {
        x_left = x012;
        x_right = x02;
    }

    // Horizontal lines
    for (uint8_t y = p0.y; y <= p2.y; y++) {
        uint8_t yi = y - p0.y;
        for (uint8_t x = x_left[yi]; x <= x_right[yi]; x++) {
            PlotPixel8K(x, y, color);
        }
    }
}

void main(void)
{
    setCPU();
    initL2();

    zx_border(INK_BLUE);
    clearL2(colour);
    zx_border(INK_RED);
	

    colour = 0;

    x = 10;
    y = 40;
    //for (loop = 0; loop <= 30; loop++)
    //{
    //    drawLine((x + loop), (y - loop), 100, 120, colour);
    //}


    //PlotPixel8K(1, 2, 3);
    //drawFillTrig (p0, p1, p2, colour);

    drawL2(15, 100, 230, 190, 0);   // Test horizontalish right drawing
    drawL2(200, 10, 30, 90, 240);   // Test horizontalish left drawing
    drawL2(30, 20, 40, 170, 255);   // Test verticalish right drawing
    drawL2(255, 30, 200, 130, 250); // Test verticalish left drawing
    drawL2(50, 40, 250, 40, 200);   // Test Horizontal
    drawL2(128, 170, 128, 20, 210); // Test Vertical


    trigL2(p0, p1, p2, 3);
    zx_border(INK_MAGENTA);

    while(1){};


    return;
}
