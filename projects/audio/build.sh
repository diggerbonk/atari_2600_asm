rm -f *.bin
rm -f *.sym

/home/trent/soft/dasm/bin/dasm audio.asm -I/home/trent/soft/dasm/machines/atari2600 -f3 -oaudio.bin -saudio.sym
