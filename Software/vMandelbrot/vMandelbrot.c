// some of this code is adapted from NetBSD amiga cv driver
// https://github.com/NetBSD/src/blob/trunk/sys/arch/amiga/dev/grf_cvreg.h

#include "vMandelbrot.h"

int main()
{
/*     // disable interrupts
    __asm__(
        "stwu 3,-4(1)\n\t"
        "li 3,0\n\t"
        "mtexier 3\n\t"
        "lwzu 3,4(1)\n\t"
    ); */
    println("vMandelbrot");

    // vga_init13h();
    
    mandel();

    return 0;
}

/*
void vga_init13h()
{
    volatile void *ba;
    unsigned char test;
    //int i;

    println("VGA Initializing ...");

    ba = (volatile void *)VGA_IO32;

    // vga enable (must be first; chip will not respond before this is sent)
    vgaw(ba, GREG_VGA_ENABLE, 0x01);
    delay(100);

    // reset index
    test = vgar(ba, GREG_MISC_OUTPUT_R);
    __USE(test);
    delay(100);

    // disable output
    vgaw(ba, ACT_ADDRESS_W, 0x00);
    delay(100);

    // initial attribute configuration
    prints("\tInitial attributes:");
    prints(" 10");
    WAttr(ba, ACT_ID_ATTR_MODE_CNTL, 0x41);     // 256 color, gfx mode
    prints(" 11");
    WAttr(ba, ACT_ID_OVERSCAN_COLOR, 0x00);     // black border
    prints(" 12");
    WAttr(ba, ACT_ID_COLOR_PLANE_ENA, 0x0f);    // enable all planes
    prints(" 13");
    WAttr(ba, ACT_ID_HOR_PEL_PANNING, 0x00);    // no hoz panning
    prints(" 14");
    WAttr(ba, ACT_ID_COLOR_SELECT, 0x00);       // no pix padding
    println(" done");

    // misc output register
    vgaw(ba, GREG_MISC_OUTPUT_W, 0x63);
    delay(100);

    // sequence registers
    println("\tSequence");
    WSeq(ba, SEQ_ID_RESET, 0x03);               // legacy VGA reset
    WSeq(ba, SEQ_ID_CLOCKING_MODE, 0x01);       // 8 char clks, no pix double
    WSeq(ba, SEQ_ID_MAP_MASK, 0x0f);            // enable all planes
    WSeq(ba, SEQ_ID_CHAR_MAP_SELECT, 0x00);     // font plane 0
    WSeq(ba, SEQ_ID_MEMORY_MODE, 0x0e);         // 256kB; sequential; chain 4
    WSeq(ba, SEQ_ID_UNLOCK_EXT, 0x06);          // unlock ext SR regs $09-$1c

    // crtc registers
    println("\tUnlock");
    WCrt(ba, CRT_ID_END_VER_RETR, 0x0e);        // unlock CRTC regs
    WCrt(ba, CRT_ID_REGISTER_LOCK_1, 0x48);     // unlock CRTC regs $2d-$3f
    WCrt(ba, CRT_ID_REGISTER_LOCK_2, 0xa0);     // unlock CRTC regs $40+
    WCrt(ba, CRT_ID_SYSTEM_CONFIG, 0x01);       // unlock enhanced regs

    vgaw16(ba, 0x42e8, 0x08000);                // reset gfx engine
    delay(1);
    vgaw16(ba, 0x42e8, 0x04000);                // enable gfx, no irq
    delay(100);

    println("\tCRTC");
    WCrt(ba, CRT_ID_HOR_TOTAL,        0x5f);    // h total
    WCrt(ba, CRT_ID_HOR_DISP_ENA_END, 0x4f);    // h enable end
    WCrt(ba, CRT_ID_START_HOR_BLANK,  0x50);    // h blank start
    WCrt(ba, CRT_ID_END_HOR_BLANK,    0x82);    // h blank end
    WCrt(ba, CRT_ID_START_HOR_RETR,   0x54);    // h retrace start
    WCrt(ba, CRT_ID_END_HOR_RETR,     0x80);    // h retrace end
    WCrt(ba, CRT_ID_VER_TOTAL,        0xbf);    // v total
    WCrt(ba, CRT_ID_OVERFLOW,         0x1f);    // overflow
    WCrt(ba, CRT_ID_PRESET_ROW_SCAN,  0x00);    // preset row scan
    WCrt(ba, CRT_ID_MAX_SCAN_LINE,    0x41);    // max scanline
    WCrt(ba, CRT_ID_CURSOR_START,     0x00);    // cursor start 0
    WCrt(ba, CRT_ID_CURSOR_END,       0x00);    // cursor end 0
    WCrt(ba, CRT_ID_START_ADDR_HIGH,  0x00);    // start addr hi
    WCrt(ba, CRT_ID_START_ADDR_LOW,   0x00);    // start addr lo
    WCrt(ba, CRT_ID_CURSOR_LOC_HIGH,  0x00);    // cursor addr hi
    WCrt(ba, CRT_ID_CURSOR_LOC_LOW,   0x00);    // cursor addr lo
    WCrt(ba, CRT_ID_START_VER_RETR,   0x9c);    // v retrace start
    WCrt(ba, CRT_ID_VER_DISP_ENA_END, 0x8f);    // v display end
    WCrt(ba, CRT_ID_SCREEN_OFFSET,    0x28);    // screen width
    WCrt(ba, CRT_ID_UNDERLINE_LOC,    0x40);    // ??
    WCrt(ba, CRT_ID_START_VER_BLANK,  0x96);    // v blank start
    WCrt(ba, CRT_ID_END_VER_BLANK,    0xb9);    // v blank end
    WCrt(ba, CRT_ID_MODE_CONTROL,     0xa3);    // ??
    WCrt(ba, CRT_ID_LINE_COMPARE,     0xff);    // line compare pos

    // graphics registers
    println("\tGfx");
    WGfx(ba, GCT_ID_SET_RESET,        0x00);    // reset data
    WGfx(ba, GCT_ID_ENABLE_SET_RESET, 0x00);    // reset data
    WGfx(ba, GCT_ID_COLOR_COMPARE,    0x00);    // no color compare
    WGfx(ba, GCT_ID_DATA_ROTATE,      0x00);    // rotate count 0
    WGfx(ba, GCT_ID_READ_MAP_SELECT,  0x00);    // read plane 0
    WGfx(ba, GCT_ID_GRAPHICS_MODE,    0x40);    // write mode 0; 256 color
    WGfx(ba, GCT_ID_MISC,             0x05);    // gfx mode, 64k@0xA0000
    WGfx(ba, GCT_ID_COLOR_XCARE,      0x0f);    // color compare all
    WGfx(ba, GCT_ID_BITMASK,          0xff);    // allow write all

    // colors for text mode
    println("\tPalette16");
    for (int i = 0; i <= 0xf; i++)
        WAttr (ba, i, i);
    
    println("\tDAC Mask");
    vgaw(ba, VDAC_MASK, 0xFF);              //DAC Mask
    delay(100);
    
    // reset index
    __USE(vgar(ba, 0x3cf));
    delay(100);

    // enable output (normal operation)
    println("\tEnable");
    vgaw(ba, ACT_ADDRESS_W, 0x20);
    delay(100);
    vgaw(ba, ACT_ADDRESS_W, 0x20);
    delay(100);
    vgaw(ba, ACT_ADDRESS_W, 0x20);
    
    println("\tComplete.");
}
*/

void putc(char c)
{
    volatile char * com_spls = (char *)0x40000000;
    volatile char * com_sptb = (char *)0x40000009;
    while(!(*com_spls & 0x04));
    *com_sptb = c;
}

void prints(char * str)
{
    int i=0;
    while(str[i] != 0)
    {
        putc(str[i++]);
    }
}

void println(char * str)
{
    prints(str);
    putc(0x0d);
    putc(0x0a);
}


void mandel()
{
    int x1 = 320;
    int y1 = 200;
    float i1 = -1.0;
    float i2 =  1.0;
    float r1 = -2.0;
    float r2 =  1.0;
    float s1 = (r2 - r1) / x1;
    float s2 = (i2 - i1) / y1;
    int n;

    for(int y=0; y<x1; y++)
    {
/*         prints("Y:");
        printHexHalf((short)y);
        println(""); */

        float i3 = i1 + s2 * y;
        for(int x=0; x<x1; x++)
        {
/*             prints("\tX:");
            printHexHalf((short)x); */

            float r3 = r1 + s1 * x;
            float z1 = r3;
            float z2 = i3;
            for(n=0; n<256; n++)
            {
                float a = z1 * z1;
                float b = z2 * z2;
                if((a + b) > 4.0) break;
                z2 = 2 * z1 * z2 + i3;
                z1 = a - b + r3;
            }

/*             prints("\tN:");
            printHexByte((char)n);
            println(""); */
            WPix(x, y, (char)(255 - n));
        }
    }
}



/* 
void mandel()
{
    int x1 = 320;
    int y1 = 200;
    int i1 = -1 * FIXEDPOINT;
    int i2 =  1 * FIXEDPOINT;
    int r1 = -2 * FIXEDPOINT;
    int r2 =  1 * FIXEDPOINT;
    int s1 = (r2 - r1) / x1;
    int s2 = (i2 - i1) / y1;
    int n;

    for(int y=0; y<y1; y++)
    {
        prints("Y:");
        printHexHalf((short)y);
        println("");

        int i3 = s2 * y / FIXEDPOINT + i1;
        for(int x=0; x<x1; x++)
        {
            prints("\tX:");
            printHexHalf((short)x);

            int r3 = s1 * x / FIXEDPOINT + r1;
            int z1 = r3;
            int z2 = i3;
            for(n=0; n<256; n++)
            {
                int a = z1 * z1 / FIXEDPOINT;
                int b = z2 * z2 / FIXEDPOINT;
                prints("\tA:");
                printHexWord(a);
                prints("\tB:");
                printHexWord(b);
                if((a + b) > LIMIT) break;
                z2 = (z1 * z2 / FIXEDPOINT) * 2 + i3;
                z1 = a - b + r3;
            }
            prints("\tN:");
            printHexByte((char)n);
            println("");
            WPix(x, y, (char)(255 - n));
        }
    }
}
 */

/* 
void vga_init()
{
    volatile void *ba;
    unsigned char test;
    int i;

    ba = (volatile void *)VGA_IO32;

    // vga enable (must be first; chip will not respond before this is sent)
    vgaw(ba, GREG_VGA_ENABLE, 0x01);

    delay(100);

    test = vgar(ba, GREG_MISC_OUTPUT_R);
    __USE(test);

    // configure for color emulation & enable CPU access
    vgaw(ba, GREG_MISC_OUTPUT_W, 0x03);

    // unlock registers
    delay(1);
    WCrt(ba, CRT_ID_END_VER_RETR, 0x0e);    // unlock CR 0-7
    delay(1);
    WCrt(ba, CRT_ID_REGISTER_LOCK_1, 0x48);	// unlock S3 VGA regs
    WCrt(ba, CRT_ID_REGISTER_LOCK_2, 0xA5);	// unlock syscontrol
    delay(1);
    WCrt(ba, CRT_ID_SYSTEM_CONFIG, 0x01);   // unlock enhanced regs
    delay(1);

    //
    // bit 1=1: enable enhanced mode functions
    // bit 4=1: enable linear addressing
    //
    vgaw(ba, ECR_ADV_FUNC_CNTL, 0x11);

    // enable color mode (bit0), CPU access (bit1), high 64k page (bit5)
    delay(1);
    vgaw(ba, GREG_MISC_OUTPUT_W, 0xe3);
    delay(1);

    // CPU base addr
    WCrt(ba, CRT_ID_EXT_SYS_CNTL_4, 0x00);

    // Reset. This does nothing, but everyone does it:)
    WSeq(ba, SEQ_ID_RESET, 0x03);

    WSeq(ba, SEQ_ID_CLOCKING_MODE, 0x01);   // 8 Dot Clock
    WSeq(ba, SEQ_ID_MAP_MASK, 0x0f);	    // Enable write planes
    WSeq(ba, SEQ_ID_CHAR_MAP_SELECT, 0x00);	// Character Font

    WSeq(ba, SEQ_ID_MEMORY_MODE, 0x02);	    // Complete mem access

    WSeq(ba, SEQ_ID_UNLOCK_EXT, 0x06);	    // Unlock extensions

    WSeq(ba, SEQ_ID_BUS_REQ_CNTL, 0x00);    // 2MB, 3 clock writes

    WSeq(ba, SEQ_ID_RAMDAC_CNTL, 0xC0);     // faster LUT write

    // skip mem clock setup; we'll leave it at default

    // initialize text mode
    
    WCrt(ba, CRT_ID_HOR_TOTAL, 0x5F);
    WCrt(ba, CRT_ID_HOR_DISP_ENA_END, 0x4F);
    WCrt(ba, CRT_ID_START_HOR_BLANK, 0x50);
    WCrt(ba, CRT_ID_END_HOR_BLANK, 0x82);
    WCrt(ba, CRT_ID_START_HOR_RETR, 0x54);
    WCrt(ba, CRT_ID_END_HOR_RETR, 0x80);
    WCrt(ba, CRT_ID_VER_TOTAL, 0xBF);

    WCrt(ba, CRT_ID_OVERFLOW, 0x1F);	    // overflow reg

    WCrt(ba, CRT_ID_PRESET_ROW_SCAN, 0x00); // no panning

    WCrt(ba, CRT_ID_MAX_SCAN_LINE, 0x40);   // vscan

    WCrt(ba, CRT_ID_CURSOR_START, 0x00);
    WCrt(ba, CRT_ID_CURSOR_END, 0x00);

    // Display start address
    WCrt(ba, CRT_ID_START_ADDR_HIGH, 0x00);
    WCrt(ba, CRT_ID_START_ADDR_LOW, 0x00);

    // Cursor location
    WCrt(ba, CRT_ID_CURSOR_LOC_HIGH, 0x00);
    WCrt(ba, CRT_ID_CURSOR_LOC_LOW, 0x00);

    // Vertical retrace
    WCrt(ba, CRT_ID_START_VER_RETR, 0x9C);
    WCrt(ba, CRT_ID_END_VER_RETR, 0x0E);

    WCrt(ba, CRT_ID_VER_DISP_ENA_END, 0x8F);
    WCrt(ba, CRT_ID_SCREEN_OFFSET, 0x50);

    WCrt(ba, CRT_ID_UNDERLINE_LOC, 0x00);

    WCrt(ba, CRT_ID_START_VER_BLANK, 0x96);
    WCrt(ba, CRT_ID_END_VER_BLANK, 0xB9);

    WCrt(ba, CRT_ID_MODE_CONTROL, 0xE3);

    WCrt(ba, CRT_ID_LINE_COMPARE, 0xFF);

    WCrt(ba, CRT_ID_BACKWAD_COMP_3, 0x10);  //  FIFO enabled

    // Refresh count 1, High speed text font, enhanced color mode
    WCrt(ba, CRT_ID_MISC_1, 0x35);

    // start fifo position
    WCrt(ba, CRT_ID_DISPLAY_FIFO, 0x5a);

    WCrt(ba, CRT_ID_EXT_MEM_CNTL_2, 0x70);

    // address window position
    WCrt(ba, CRT_ID_LAW_POS_LO, 0x40);

    // N Parameter for Display FIFO
    WCrt(ba, CRT_ID_EXT_MEM_CNTL_3, 0xFF);

    WGfx(ba, GCT_ID_SET_RESET, 0x00);
    WGfx(ba, GCT_ID_ENABLE_SET_RESET, 0x00);
    WGfx(ba, GCT_ID_COLOR_COMPARE, 0x00);
    WGfx(ba, GCT_ID_DATA_ROTATE, 0x00);
    WGfx(ba, GCT_ID_READ_MAP_SELECT, 0x00);
    WGfx(ba, GCT_ID_GRAPHICS_MODE, 0x40);
    WGfx(ba, GCT_ID_MISC, 0x01);
    WGfx(ba, GCT_ID_COLOR_XCARE, 0x0F);
    WGfx(ba, GCT_ID_BITMASK, 0xFF);

    // colors for text mode
    for (i = 0; i <= 0xf; i++)
        WAttr (ba, i, i);

    WAttr(ba, ACT_ID_ATTR_MODE_CNTL, 0x41);
    WAttr(ba, ACT_ID_OVERSCAN_COLOR, 0x01);
    WAttr(ba, ACT_ID_COLOR_PLANE_ENA, 0x0F);
    WAttr(ba, ACT_ID_HOR_PEL_PANNING, 0x00);
    WAttr(ba, ACT_ID_COLOR_SELECT, 0x00);

    vgaw(ba, VDAC_MASK, 0xFF);              //DAC Mask

    // initialize greyscale color palette
    vgaw(ba, VDAC_ADDRESS_W, 0);
    delay(1);
    for (i = 255; i >= 0 ; i--) {
        vgaw(ba, VDAC_DATA, i);
        delay(1);
        vgaw(ba, VDAC_DATA, i);
        delay(1);
        vgaw(ba, VDAC_DATA, i);
        delay(1);
    }

    WCrt(ba, CRT_ID_LAW_CNTL, 0x12);        // set LAW size 22MB

    // skip Initialize graphics engine 
}

 */
