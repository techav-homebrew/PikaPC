// some of this code is adapted from NetBSD amiga cv driver
// https://github.com/NetBSD/src/blob/trunk/sys/arch/amiga/dev/grf_cvreg.h

#include "vMandelbrot.h"

int main()
{
    vga_init();

    restart:

    prints("vMandelbrot ... ");

    // vga_init13h();

    int width = 640;
    int height = 480;
    int escape = 0x0000200;
    int isteps = 12;

    fixed startLeft = int2fix(-2);
    fixed startRight = int2fix(1);
    fixed startTop = int2fix(1);
    fixed startBottom = int2fix(-1);

    fixed stopLeft = 0xfff616a1;
    fixed stopRight = 0xfff61b7f;
    fixed stopTop = 0x000abda2;
    fixed stopBottom = 0x000ab9fc;

    fixed left = startLeft;
    fixed right = startRight;
    fixed top = startTop;
    fixed bottom = startBottom;

    for(int i=0; i<isteps; i++)
    {
        mandel(
            width,
            height,
            left,
            right,
            top,
            bottom,
            escape
        );

        left = ((stopLeft - left) >> 2) + left;
        right = ((stopRight - right) >> 2) + right;
        top = ((stopTop - top) >> 2) + top;
        bottom = ((stopBottom - bottom) >> 2) + bottom;
    }

    // and let's go one more at the target
    mandel(
        width,
        height,
        stopLeft,
        stopRight,
        stopTop,
        stopBottom,
        escape
    );

    println("Done");

    delay(80000);   // hold last frame roughly 5 seconds

    goto restart;
    return 0;
}


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

void mandelPixel(int x, int y, short value)
{
    //short color = 0x1c63;
    short color = 0;
    color |= (value & 0x0007) << 2; // B
    color |= (value & 0x0038) << 5; // G
    color |= (value & 0x01c0) << 7; // R

    WPix(x, y, color);
}

void mandel(int width, int height, fixed left,
    fixed right, fixed top, fixed bottom, int escape)
{
    int x1 = width;
    int y1 = height;
    fixed i1 = bottom;
    fixed i2 = top;
    fixed r1 = left;
    fixed r2 = right;
    fixed s1 = fix_div((r2 - r1), int2fix(x1));
    fixed s2 = fix_div((i2 - i1), int2fix(y1));
    int n;

    for(int y=0; y<y1; y++)
    {
        fixed i3 = i1 + fix_mul(s2, int2fix(y));
        for(int x=0; x<x1; x++)
        {
            fixed r3 = r1 + fix_mul(s1, int2fix(x));
            fixed z1 = r3;
            fixed z2 = i3;
            for(n=0; n<escape; n++)
            {
                mandelPixel(x, y, (short)(n));
                fixed a = fix_mul(z1, z1);
                fixed b = fix_mul(z2, z2);
                if((a + b) > int2fix(4)) break;
                z2 = i3 + fix_mul((z1 + z1), z2);
                z1 = a - b + r3;
            }
            mandelPixel(x, y, (short)(n));
        }
    }
}


void vga_init()
{
    volatile void *ba;
    unsigned char test;
    
    println("VGA Initializing ...");

    ba = (volatile void *)VGA_IO32;

    // enable vga
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
    prints("\tInitial attributes ... ");
    WAttr(ba, ACT_ID_ATTR_MODE_CNTL, 0x41);
    WAttr(ba, ACT_ID_OVERSCAN_COLOR, 0x00);
    WAttr(ba, ACT_ID_COLOR_PLANE_ENA, 0x0f);
    WAttr(ba, ACT_ID_HOR_PEL_PANNING, 0x00);
    WAttr(ba, ACT_ID_COLOR_SELECT, 0x00);
    println("OK");

    vgaw(ba, GREG_MISC_OUTPUT_W, 0xef);
    delay(100);

    // sequence registers
    prints("\tProgramming sequencer ... ");
    WSeq(ba, SEQ_ID_RESET, 0x10);               // $1000
    WSeq(ba, SEQ_ID_CLOCKING_MODE, 0x01);       // $0101
    WSeq(ba, SEQ_ID_MAP_MASK, 0x0f);            // $0f02
    WSeq(ba, SEQ_ID_CHAR_MAP_SELECT, 0x00);     // $0003
    WSeq(ba, SEQ_ID_MEMORY_MODE, 0x0e);         // $0e04
    WSeq(ba, SEQ_ID_UNLOCK_EXT, 0x06);          // $0608
    WSeq(ba, SEQ_ID_EXT_MISC_SEQ, 0x00);        // $000b
    WSeq(ba, SEQ_ID_RAMDAC_CNTL, 0x40);         // $4018
    WSeq(ba, SEQ_ID_CLKSYN_CNTL_2, 0x00);       // $0015
    WSeq(ba, SEQ_ID_EXT_MISC_SEQ, 0x00);        // $000b
    WSeq(ba, SEQ_ID_CLKSYN_CNTL_1, 0x00);       // $0014
    WSeq(ba, SEQ_ID_RAMDAC_CNTL, 0x40);         // $4018
    println("OK");

    // register unlock
    prints("\tUnlocking registers ... ");
    WCrt(ba, CRT_ID_END_VER_RETR, 0x0e);        // $0e11
    WCrt(ba, CRT_ID_REGISTER_LOCK_1, 0x48);     // $4838
    WCrt(ba, CRT_ID_REGISTER_LOCK_2, 0xa0);     // $a039
    WCrt(ba, CRT_ID_SYSTEM_CONFIG, 0x01);       // $0140
    println("OK");

    // gfx engine
/*     prints("\tInitializing graphics engine ... ");
    vgaw16(ba, 0x42e8, 0x08000);                // reset gfx engine
    vgaw16(ba, 0x42e8, 0x04000);                // enable gfx, no irq
    println("OK"); */

    // CRTC
    prints("\tInitializing CRTC enhanced ... ");
    WCrt(ba, CRT_ID_REGISTER_LOCK_1, 0x48);     // $4838
    WCrt(ba, CRT_ID_REGISTER_LOCK_2, 0xa5);     // $a539
    WCrt(ba, CRT_ID_BACKWAD_COMP_1, 0x40);      // $4032
    WCrt(ba, CRT_ID_BACKWAD_COMP_2, 0x00);      // $0033
    WCrt(ba, CRT_ID_REGISTER_LOCK, 0x00);       // $0035
    WCrt(ba, CRT_ID_LACE_CONTROL, 0x00);        // $0042
    WCrt(ba, CRT_ID_EXT_MODE, 0x00);            // $0043
    WCrt(ba, CRT_ID_HWGC_MODE, 0x00);           // $0048
    WCrt(ba, CRT_ID_EXT_MEM_CNTL_1, 0x00);      // $0053
    WCrt(ba, CRT_ID_EX_SYNC_1, 0x00);           // $0055
    WCrt(ba, CRT_ID_LAW_CNTL, 0x83);            // $8358
    WCrt(ba, CRT_ID_EXT_MISC_CNTL, 0x00);       // $0065
    WCrt(ba, CRT_ID_EXT_MISC_CNTL_1, 0x00);     // $0066
    WCrt(ba, CRT_ID_EXT_SYS_CNTL_3, 0x00);      // $0069
    WCrt(ba, CRT_ID_EXT_SYS_CNTL_4, 0x00);      // $006a
    println("OK");

    // legacy CRTC
    prints("\tInitializing CRTC legacy ... ");
    WCrt(ba, CRT_ID_HOR_TOTAL, 0xc2);           // $c200
    WCrt(ba, CRT_ID_HOR_DISP_ENA_END, 0x9f);    // $9f01
    WCrt(ba, CRT_ID_START_HOR_BLANK, 0xa0);     // $a002
    WCrt(ba, CRT_ID_END_HOR_BLANK, 0x84);       // $8403
    WCrt(ba, CRT_ID_START_HOR_RETR, 0xa3);      // $a304
    WCrt(ba, CRT_ID_END_HOR_RETR, 0x1b);        // $1b05
    WCrt(ba, CRT_ID_VER_TOTAL, 0x0c);           // $0c06
    WCrt(ba, CRT_ID_OVERFLOW, 0x3e);            // $3e07
    WCrt(ba, CRT_ID_PRESET_ROW_SCAN, 0x00);     // $0008
    WCrt(ba, CRT_ID_MAX_SCAN_LINE, 0x40);       // $4009
    WCrt(ba, CRT_ID_CURSOR_START, 0x00);        // $000A
    WCrt(ba, CRT_ID_CURSOR_END, 0x00);          // $000B
    WCrt(ba, CRT_ID_START_ADDR_HIGH, 0x00);     // $000C
    WCrt(ba, CRT_ID_START_ADDR_LOW, 0x00);      // $000D
    WCrt(ba, CRT_ID_CURSOR_LOC_HIGH, 0xff);     // $ff0E
    WCrt(ba, CRT_ID_CURSOR_LOC_LOW, 0x00);      // $000F
    WCrt(ba, CRT_ID_START_VER_RETR, 0xe9);      // $e910
    WCrt(ba, CRT_ID_END_VER_RETR, 0x0b);        // $0b11
    WCrt(ba, CRT_ID_VER_DISP_ENA_END, 0xdf);    // $df12
    WCrt(ba, CRT_ID_SCREEN_OFFSET, 0xa0);       // $a013
    WCrt(ba, CRT_ID_UNDERLINE_LOC, 0x60);       // $6014
    WCrt(ba, CRT_ID_START_VER_BLANK, 0xe7);     // $e715
    WCrt(ba, CRT_ID_END_VER_BLANK, 0x04);       // $4016
    WCrt(ba, CRT_ID_MODE_CONTROL, 0xab);        // $ab17
    WCrt(ba, CRT_ID_LINE_COMPARE, 0xff);        // $ff18
    println("OK");

    // more extended CRTC
    prints("\tInitializing CRTC extended ... ");
    WCrt(ba, CRT_ID_MEMORY_CONF, 0x09);         // $0931
    WCrt(ba, CRT_ID_BACKWAD_COMP_3, 0x10);      // $1034
    WCrt(ba, CRT_ID_MISC_1, 0x15);              // $153a
    WCrt(ba, CRT_ID_DISPLAY_FIFO, 0x00);        // $003b
    WCrt(ba, CRT_ID_LACE_RETR_START, 0x61);     // $613c
    WCrt(ba, CRT_ID_SYSTEM_CONFIG, 0x01);       // $0140
    WCrt(ba, CRT_ID_EXT_SYS_CNTL_1, 0x50);      // $5050
    WCrt(ba, CRT_ID_EXT_SYS_CNTL_2, 0x00);      // $0051
    WCrt(ba, CRT_ID_EXT_MEM_CNTL_2, 0xf8);      // $f854
    WCrt(ba, CRT_ID_EXT_MEM_CNTL_3, 0xff);      // $ff60
    WCrt(ba, CRT_ID_EXT_HOR_OVF, 0x00);         // $005d
    WCrt(ba, CRT_ID_EXT_VER_OVF, 0x00);         // $005e
    WCrt(ba, CRT_ID_EXT_MISC_CNTL_2, 0x50);     // $5067
    println("OK");

    // graphics registers
    prints("\tInitializing legacy graphics ... ");
    WGfx(ba, GCT_ID_SET_RESET, 0x00);           // $0000
    WGfx(ba, GCT_ID_ENABLE_SET_RESET, 0x00);    // $0001
    WGfx(ba, GCT_ID_COLOR_COMPARE, 0x00);       // $0002
    WGfx(ba, GCT_ID_DATA_ROTATE, 0x00);         // $0003
    WGfx(ba, GCT_ID_READ_MAP_SELECT, 0x00);     // $0004
    WGfx(ba, GCT_ID_GRAPHICS_MODE, 0x40);       // $4005
    WGfx(ba, GCT_ID_MISC, 0x05);                // $0506
    WGfx(ba, GCT_ID_COLOR_XCARE, 0x0f);         // $0f07
    WGfx(ba, GCT_ID_BITMASK, 0xff);             // $ff08
    println("OK");

    // attribute & legacy palette registers
    prints("\tInitializing attributes ... ");
    WAttr(ba, ACT_ID_PALETTE0, 0x00);           // $0000
    WAttr(ba, ACT_ID_PALETTE1, 0x01);           // $0101
    WAttr(ba, ACT_ID_PALETTE2, 0x02);           // $0202
    WAttr(ba, ACT_ID_PALETTE3, 0x03);           // $0303
    WAttr(ba, ACT_ID_PALETTE4, 0x04);           // $0404
    WAttr(ba, ACT_ID_PALETTE5, 0x05);           // $0505
    WAttr(ba, ACT_ID_PALETTE6, 0x06);           // $0606
    WAttr(ba, ACT_ID_PALETTE7, 0x07);           // $0707
    WAttr(ba, ACT_ID_PALETTE8, 0x10);           // $1008
    WAttr(ba, ACT_ID_PALETTE9, 0x11);           // $1109
    WAttr(ba, ACT_ID_PALETTE10, 0x12);          // $120a
    WAttr(ba, ACT_ID_PALETTE11, 0x13);          // $130b
    WAttr(ba, ACT_ID_PALETTE12, 0x14);          // $140c
    WAttr(ba, ACT_ID_PALETTE13, 0x15);          // $150d
    WAttr(ba, ACT_ID_PALETTE14, 0x16);          // $160e
    WAttr(ba, ACT_ID_PALETTE15, 0x17);          // $170f
    println("OK");

    // enable linear addressing
    prints("\tInitializing linear addressing window ... ");
    WCrt(ba, CRT_ID_EXT_MEM_CNTL_1, 0x1c);      // $1c53
    WCrt(ba, CRT_ID_EXT_MEM_CNTL_2, 0xfa);      // $fa54
    WCrt(ba, CRT_ID_LAW_POS_HI, 0x00);          // $0059
    WCrt(ba, CRT_ID_LAW_CNTL, 0x00);            // $0058
    WCrt(ba, CRT_ID_LAW_CNTL, 0x92);            // $9258
    println("OK");

    // finish up
    prints("\tFinishing up ... ");
    vgaw(ba, VDAC_MASK, 0xff);                  // enable all DAC mask bits
    delay(100);

    test = vgar(ba, GREG_MISC_OUTPUT_R);        // reset attribute index
    __USE(test);
    delay(100);

    vgaw(ba, ACT_ADDRESS_W, 0x20);              // enable normal operation
    println("DONE");
}

