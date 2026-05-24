ENTRY(main)
SECTIONS{
    .main 0x0fa00000 :
    {
        KEEP(*(.mainentry))
        *.o (.text)
        *.o (.rodata*)
        *.o (.bss)
        *.o (.data)
    }
}