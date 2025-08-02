Next3D - a 3D engine for the ZX Spectrum Next
<br>
v0.2 -- DEBUG MODE, NOT READY FOR PRODUCTION
>>> PlotPixel8K has 256,000 T-States injected to make the engine run slow, pixel by pixel, for debug purposes at this point.
<br>
<br>
The Next3D implementation is meant to be used in C via z88dk, but since we are aiming for high performance, all its core functionalities (plot, draw, triangle, filled triangle, projections, lightining etc.) are implemented in Z80 Assembly fully documented and built to take advantage of the Next hardware as much as possible.
<br>
This means the core library is easily portable to any environment, and its parts (such as plot or triangle) can easily be used in any C, Boriel Basic or other languages inline.
<br>
I took a lot of care to describe each single line of the asm code for easiness of following what's going on, targetting anyone willing to reuse it (or improve!)
<br>
For now, this is what works:<br>
<br>
- initL2: 256x192 video mode with 256 colours<br>
I haven't implemented other video modes as they would require 16 bit logic and for now, Next3D is a 8 bit engine.
<br>
- PlotPixel8K: fast plotting (soon to be renamed plotL2)<br>
Plots a pixel on the screen at x, y with colour.
*** TEMPORARILY INJECTED DEBUG CODE TO MASSIVELY SLOW IT DOWN, WILL REMOVE SHORTLY ***
<br>
- drawL2: fast drawing<br>
Draws a line on the screen from x1, y1 to x2, y2 with colour.
<br>
- trigL2: fast wireframe triangle<br>
Draws a triangle on the screen with p0, p1, p2 and colour (p being a structure containing x, y coordinates)
<br>
- fillTrigL2: fast filled triangle<br>
Draws a filled triangle on the screen with p0, p1, p2 and colour (p being a structure containing x, y coordinates)
*** STILL HAS BUGS BEING WORKED ON ***
<br>
- clearL2: clear the screen<br>
Clear the screen with the colour of your choice.
*** TEMPORARY VERY VERY INEFFICIENT CODE ***
<br>
- setCPU: sets CPU speed<br>
<br>
Your choice of MHz. Default value is 3 (28MHz).
