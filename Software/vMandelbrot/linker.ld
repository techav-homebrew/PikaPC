ENTRY(main)
SECTIONS{
    .main 0x7fe60000 :
    {
        vMandelbrot.o (text)
        *.o (.text)
        *.o (.rodata*)
        *.o (.bss)
        *.o (.data)
    }
}