// SENSIBLE DEFAULTS FOR NEX

// Main Binary ORG

#pragma output CRT_ORG_CODE = 0x8000    // default = 0x8000  : org of main binary

// Stack Location

#pragma output REGISTER_SP    = 0x7ff0  // default = 65368  : location of stack
#pragma output CRT_STACK_SIZE = 256     // default =   512  : reserved space for stack - only relevant to malloc with heap size -1

// Interrupts

#pragma output CRT_ENABLE_EIDI         = 0x01  // default = 0x01  : bit flags: 0x01 = di on start, 0x02 = ei on start, 0x10 = di on exit, 0x20 = ei on exit
#pragma output CRT_INTERRUPT_MODE      = -1    // default =   -1  : insert im mode instruction before main if 0, 1, 2
#pragma output CRT_INTERRUPT_MODE_EXIT = -1    // default =    1  : insert im mode instruction on exit if 0, 1, 2

// Dynamic Memory

#pragma output CLIB_MALLOC_HEAP_SIZE   = 0  // default =  -1  : size of malloc heap (0 = do not create a heap, -1 = place heap between bss and bottom of stack)
#pragma output CLIB_STDIO_HEAP_SIZE    = 0  // default = 256  : size of stdio heap (0 = do not allocate bytes for opening of files)

#pragma output CLIB_BALLOC_TABLE_SIZE  = 0  // default =   0  : number of queues created for block memory allocator

// Exit Behaviour

#pragma output CLIB_EXIT_STACK_SIZE      = 0  // default = 0  : max number of functions that can be registered with atexit()
#pragma output CLIB_QUICKEXIT_STACK_SIZE = 0  // default = 0  : max number of functions that can be registered with at_quick_exit()

#pragma output CRT_ENABLE_CLOSE = 0   // default = 1  : non-zero closes open FILEs on exit

#pragma output CRT_ON_EXIT = 0x10001  // default = 0x30002  : 0x10001 = halt & loop, 0x30002 = return to basic

// append file mmap.inc to memory map

// #pragma output CRT_APPEND_MMAP = 1     // only if you extend the memory map
