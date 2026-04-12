#include "vMandelbrot.h"

int main()
{
    vga_init();

    return 0;
}

// this code is adapted from NetBSD amiga cv driver
// https://github.com/NetBSD/src/blob/trunk/sys/arch/amiga/dev/grf_cvreg.h
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

    /*
     * bit 1=1: enable enhanced mode functions
     * bit 4=1: enable linear addressing
     */
    vgaw(ba, ECR_ADV_FUNC_CNTL, 0x11);

    /* enable color mode (bit0), CPU access (bit1), high 64k page (bit5) */
    delay(1);
    vgaw(ba, GREG_MISC_OUTPUT_W, 0xe3);
    delay(1);

    /* CPU base addr */
    WCrt(ba, CRT_ID_EXT_SYS_CNTL_4, 0x00);

    /* Reset. This does nothing, but everyone does it:) */
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

    /* Display start address */
    WCrt(ba, CRT_ID_START_ADDR_HIGH, 0x00);
    WCrt(ba, CRT_ID_START_ADDR_LOW, 0x00);

    /* Cursor location */
    WCrt(ba, CRT_ID_CURSOR_LOC_HIGH, 0x00);
    WCrt(ba, CRT_ID_CURSOR_LOC_LOW, 0x00);

    /* Vertical retrace */
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

    /* Refresh count 1, High speed text font, enhanced color mode */
    WCrt(ba, CRT_ID_MISC_1, 0x35);

    /* start fifo position */
    WCrt(ba, CRT_ID_DISPLAY_FIFO, 0x5a);

    WCrt(ba, CRT_ID_EXT_MEM_CNTL_2, 0x70);

    /* address window position */
    WCrt(ba, CRT_ID_LAW_POS_LO, 0x40);

    /* N Parameter for Display FIFO */
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

    /* colors for text mode */
    for (i = 0; i <= 0xf; i++)
        WAttr (ba, i, i);

    WAttr(ba, ACT_ID_ATTR_MODE_CNTL, 0x41);
    WAttr(ba, ACT_ID_OVERSCAN_COLOR, 0x01);
    WAttr(ba, ACT_ID_COLOR_PLANE_ENA, 0x0F);
    WAttr(ba, ACT_ID_HOR_PEL_PANNING, 0x00);
    WAttr(ba, ACT_ID_COLOR_SELECT, 0x00);

    vgaw(ba, VDAC_MASK, 0xFF);              //DAC Mask

    /* initialize greyscale color palette */
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


