ENTRY(main)
SECTIONS{
    .main 0x7fe6000 :
    {
        vMandelbrot.o (text)
        *.o (text)
        *.o (bss)
        *.o (data)
    }
}