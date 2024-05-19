rm -f *.bin
rm -f *.sym

/home/trent/soft/dasm/bin/dasm looper.asm -I/home/trent/soft/dasm/machines/atari2600 -f3 -olooper.bin -slooper.sym
