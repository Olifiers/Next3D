
del /Q *.lis
zcc +zxn -v --list -startup=8 -clib=sdcc_iy -SO3 --max-allocs-per-node200000 @zproject.lst -o test -pragma-include:zpragma.inc -subtype=nex -Ca"-no-synth" -Cz"--clean --main-fence=0xc000" -create-app