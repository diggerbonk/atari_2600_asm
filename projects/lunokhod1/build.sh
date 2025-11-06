rm -f *.bin
rm -f *.sym

if [[ "$1" == "clean" ]] ; then
    rm -f pfdata.asm
    rm -f lunokhod1.bin
    rm -f lunokhod1.sym
    rm -f *.zip
    exit 0
else

    java -cp ../../utils GeneratePlayfield pfdata.txt > pfdata.asm

    /home/trent/soft/dasm/bin/dasm lunokhod1.asm \
        -I/home/trent/soft/dasm/machines/atari2600 \
        -f3 -olunokhod1.bin -slunokhod1.sym

    if [[ "$1" == "build" ]] ; then
        mkdir -p releases
        mv lunokhod1.bin releases/lunokhod1-$2.bin
        cp lunokhod1.asm releases/lunokhod1-$2.asm
        zip lunokhod1-$2.zip releases/lunokhod1-$2.bin releases/lunokhod1-$2.asm pfdata.asm todo.txt changelog.txt
        mv lunokhod1-$2.zip releases/lunokhod1-$2.zip
    fi
fi
