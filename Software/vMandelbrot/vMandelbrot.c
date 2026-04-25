// some of this code is adapted from NetBSD amiga cv driver
// https://github.com/NetBSD/src/blob/trunk/sys/arch/amiga/dev/grf_cvreg.h

#include "vMandelbrot.h"

int main()
{
    restart:

    println("vMandelbrot");

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

/* void mandel(int width, int height, float left, 
    float right, float top, float bottom, int escape)
{
    int x1 = width;     // 320
    int y1 = height;    // 200
    float i1 = bottom;  // -1.0;
    float i2 = top;     // 1.0;
    float r1 = left;    // -2.0;
    float r2 = right;   //  1.0;
    float s1 = (r2 - r1) / x1;
    float s2 = (i2 - i1) / y1;
    int n;

    for(int y=0; y<y1; y++)
    {
        float i3 = i1 + s2 * y;
        for(int x=0; x<x1; x++)
        {
            float r3 = r1 + s1 * x;
            float z1 = r3;
            float z2 = i3;
            for(n=0; n<escape; n++)
            {
                WPix(x, y, (char)(escape - 1 - n));
                float a = z1 * z1;
                float b = z2 * z2;
                if((a + b) > 4.0) break;
                z2 = 2 * z1 * z2 + i3;
                z1 = a - b + r3;
            }
            WPix(x, y, (char)(escape - 1 - n));
        }
    }
} */
 
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

