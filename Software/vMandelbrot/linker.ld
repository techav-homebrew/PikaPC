ENTRY(main)
SECTIONS{
    .main 0x7fe60000 :
    {
        KEEP(*(.mainentry))
        *.o (.text)
        *.o (.rodata*)
        *.o (.bss)
        *.o (.data)
    }
}