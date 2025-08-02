# Next3D - a 3D engine for the ZX Spectrum Next

## v0.2 -- DEBUG MODE, NOT READY FOR PRODUCTION
** PlotPixel8K has 256,000 T-States injected to make the engine run slow, pixel by pixel, for debug purposes at this point.**


The Next3D implementation is meant to be used in C via z88dk, but since we are aiming for high performance, all its core functionalities (plot, draw, triangle, filled triangle, projections, lightining etc.) are implemented in Z80 Assembly fully documented and built to take advantage of the Next hardware as much as possible.

This means the core library is easily portable to any environment, and its parts (such as plot or triangle) can easily be used in any C, Boriel Basic or other languages inline.

I took a lot of care to describe each single line of the asm code for easiness of following what's going on, targetting anyone willing to reuse it (or improve!)

For now, this is what works:

**initL2: 256x192 video mode with 256 colours**  
I haven't implemented other video modes as they would require 16 bit logic and for now, Next3D is a 8 bit engine.

**PlotPixel8K: fast plotting (soon to be renamed plotL2)**  
Plots a pixel on the screen at x, y with colour.
_TEMPORARILY INJECTED DEBUG CODE TO MASSIVELY SLOW IT DOWN, WILL REMOVE SHORTLY_

**drawL2: fast drawing**  
Draws a line on the screen from x1, y1 to x2, y2 with colour.

**trigL2: fast wireframe triangle**  
Draws a triangle on the screen with p0, p1, p2 and colour (p being a structure containing x, y coordinates)

**fillTrigL2: fast filled triangle**  
Draws a filled triangle on the screen with p0, p1, p2 and colour (p being a structure containing x, y coordinates)  
_STILL HAS BUGS BEING WORKED ON_

**clearL2: clear the screen**  
Clear the screen with the colour of your choice.  
_TEMPORARY VERY VERY INEFFICIENT CODE_

**setCPU: sets CPU speed**  
Your choice of MHz. Default value is 3 (28MHz).
