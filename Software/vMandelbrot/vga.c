// this code is adapted from NetBSD amiga cv driver
//https://github.com/NetBSD/src/blob/trunk/sys/arch/amiga/dev/grf_cv.c


/*	$NetBSD: grf_cv.c,v 1.64 2022/03/28 12:38:57 riastradh Exp $ */

/*
 * Copyright (c) 1995 Michael Teske
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by Ezra Story, by Kari
 *      Mettinen, Michael Teske and by Bernd Ernesti.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "vgareg.h"
#include "pikapc_print.h"

/*
 * Graphics routines for the CyberVision 64 board, using the S3 Trio64.
 *
 * Modified for CV64 from
 * Kari Mettinen's Cirrus driver by Michael Teske 10/95
 *
 * Thanks to Tekelec Airtronic for providing me with a S3 Trio64 documentation.
 * Thanks to Bernd 'the fabulous bug-finder' Ernesti for bringing my messy
 * source to NetBSD style :)
 * Thanks to Harald Koenig for providing information about undocumented
 * Trio64 Bugs.
 */

#define MAXPIXELCLOCK 135000000 /* safety */

/*
 * Memory clock (binpatchable).
 * Let's be defensive: 50 MHz runs on all boards I know of.
 * 55 MHz runs on most boards. But you should know what you're doing
 * if you set this flag. Again: This flag may destroy your CV Board.
 * Use it at your own risk!!!
 * Anyway, this doesn't imply that I'm responsible if your board breaks
 * without setting this flag :-).
 */
#ifdef CV_AGGRESSIVE_TIMING
long cv_memclk = 55000000;
#else
long cv_memclk = 50000000;
#endif

/*
 * Get framebuffer memory size.
 * Return 0 for 2MB, 1 for 4MB
 * Only 2MB is supported on VLB
 */
/* static int
cv_has_4mb(volatile void *fb)
{
    return 0;
} */

/*
 * Computes M, N, and R values from
 * given input frequency. It uses a table of
 * precomputed values, to keep CPU time low.
 *
 * The return value consist of:
 * lower byte:  Bits 4-0: N Divider Value
 *	        Bits 5-6: R Value          for e.g. SR10 or SR12
 * higher byte: Bits 0-6: M divider value  for e.g. SR11 or SR13
 */

static unsigned short
cv_compute_clock(unsigned long freq)
{
    static unsigned char *mnr, *save;	/* M, N + R vals */
    unsigned long work_freq, r;
    unsigned short erg;
    long diff, d2;

    println("cv_compute_clock");

    if (freq < 12500000 || freq > MAXPIXELCLOCK) {
        // printf("grfcv: Illegal clock frequency: %ldMHz\n", freq/1000000);
        // printf("grfcv: Using default frequency: 25MHz\n");
        // printf("grfcv: See the manpage of grfconfig for more informations.\n");
        freq = 25000000;
    }

    mnr = clocks;	/* there the vals are stored */
    d2 = 0x7fffffff;

    while (*mnr) {	/* mnr vals are 0-terminated */
        work_freq = (0x37EE * (mnr[0] + 2)) / ((mnr[1] & 0x1F) + 2);

        r = (mnr[1] >> 5) & 0x03;
        if (r != 0)
            work_freq=work_freq >> r;	/* r is the freq divider */

        work_freq *= 0x3E8;	/* 2nd part of OSC */

        diff = abs(freq - work_freq);

        if (d2 >= diff) {
            d2 = diff;
            /* In save are the vals for minimal diff */
            save = mnr;
        }
        mnr += 2;
    }
    erg = *((unsigned short *)save);

    return (erg);
}




static inline void
cv_write_port(unsigned short bits, volatile void *BoardAddr)
{
    volatile char *addr;
    static unsigned char CVPortBits = 0;	/* mirror port bits here */

    println("cv_write_port");

    addr = (volatile char*)BoardAddr + 0x40001;
    if (bits & 0x8000)
        CVPortBits |= bits & 0xFF;	/* Set bits */
    else {
        bits = bits & 0xFF;
        bits = (~bits) & 0xFF ;
        CVPortBits &= bits;	/* Clear bits */
    }

    *addr = CVPortBits;
}



/* void
cv_boardinit(struct grf_softc *gp) */

void
cv_boardinit()
{
    volatile void *ba;
    unsigned char test;
    unsigned int clockpar;
    int i;
    // struct grfinfo *gi;

    println("cv_boardinit");

    // ba = gp->g_regkva;   // ba is base address for I/O port
    ba = (volatile void *)VGA_IO32;

    /* Wakeup Chip */
    vgaw(ba, SREG_VIDEO_SUBS_ENABLE, 0x10);
    vgaw(ba, SREG_OPTION_SELECT, 0x01);
    vgaw(ba, SREG_VIDEO_SUBS_ENABLE, 0x08);

    vgaw(ba, GREG_MISC_OUTPUT_W, 0x03);

    WCrt(ba, CRT_ID_REGISTER_LOCK_1, 0x48);	/* unlock S3 VGA regs */
    WCrt(ba, CRT_ID_REGISTER_LOCK_2, 0xA5);	/* unlock syscontrol */

    test = RCrt(ba, CRT_ID_SYSTEM_CONFIG);
    test = test | 0x01;	/* enable enhanced register access */
    test = test & 0xEF;	/* clear bit 4, 0 wait state */
    WCrt(ba, CRT_ID_SYSTEM_CONFIG, test);

    /*
     * bit 1=1: enable enhanced mode functions
     * bit 4=1: enable linear addressing
     * bit 5=1: enable MMIO
     */
    vgaw(ba, ECR_ADV_FUNC_CNTL, 0x31);

    /* enable color mode (bit0), CPU access (bit1), high 64k page (bit5) */
    vgaw(ba, GREG_MISC_OUTPUT_W, 0xe3);

    /* CPU base addr */
    WCrt(ba, CRT_ID_EXT_SYS_CNTL_4, 0x00);

    /* Reset. This does nothing, but everyone does it:) */
    WSeq(ba, SEQ_ID_RESET, 0x03);

    WSeq(ba, SEQ_ID_CLOCKING_MODE, 0x01);	/* 8 Dot Clock */
    WSeq(ba, SEQ_ID_MAP_MASK, 0x0f);	/* Enable write planes */
    WSeq(ba, SEQ_ID_CHAR_MAP_SELECT, 0x00);	/* Character Font */

    WSeq(ba, SEQ_ID_MEMORY_MODE, 0x02);	/* Complete mem access */

    WSeq(ba, SEQ_ID_UNLOCK_EXT, 0x06);	/* Unlock extensions */
    test = RSeq(ba, SEQ_ID_BUS_REQ_CNTL);	/* Bus Request */

    /* enable 4MB fast Page Mode */
    test = test | 1 << 6;
    WSeq(ba, SEQ_ID_BUS_REQ_CNTL, test);
    /* faster LUT write */
    WSeq(ba, SEQ_ID_RAMDAC_CNTL, 0xC0);

    test = RSeq(ba, SEQ_ID_CLKSYN_CNTL_2);	/* Clksyn2 read */

    /* immediately Clkload bit clear */
    test = test & 0xDF;

    /* 2 MCLK Memory Write.... */
    if (cv_memclk >= 55000000)
        test |= 0x80;

    WSeq(ba, SEQ_ID_CLKSYN_CNTL_2, test);

    /* Memory CLK */
    clockpar = cv_compute_clock(cv_memclk);
    test = (clockpar & 0xFF00) >> 8;
    WSeq(ba, SEQ_ID_MCLK_HI, test);		/* PLL N-Divider Value */

    test = clockpar & 0xFF;
    WSeq(ba, SEQ_ID_MCLK_LO, test);		/* PLL M-Divider Value */

    if (RCrt(ba, CRT_ID_REVISION) == 0x10)	/* bugfix for new S3 chips */
        WSeq(ba, SEQ_ID_MORE_MAGIC, test);

    /* We now load an 25 MHz, 31 kHz, 640x480 standard VGA Mode. */
    /* DCLK */
    WSeq(ba, SEQ_ID_DCLK_HI, 0x13);
    WSeq(ba, SEQ_ID_DCLK_LO, 0x41);

    test = RSeq (ba, SEQ_ID_CLKSYN_CNTL_2);
    test = test | 0x22;

    /* DCLK + MCLK Clock immediate load! */
    WSeq(ba,SEQ_ID_CLKSYN_CNTL_2, test);

    /* DCLK load */
    test = vgar(ba, 0x3cc);
    test = test | 0x0c;
    vgaw(ba, 0x3c2, test);

    /* Clear bit 5 again, prevent further loading. */
    WSeq(ba, SEQ_ID_CLKSYN_CNTL_2, 0x02);

    WCrt(ba, CRT_ID_HOR_TOTAL, 0x5F);
    WCrt(ba, CRT_ID_HOR_DISP_ENA_END, 0x4F);
    WCrt(ba, CRT_ID_START_HOR_BLANK, 0x50);
    WCrt(ba, CRT_ID_END_HOR_BLANK, 0x82);
    WCrt(ba, CRT_ID_START_HOR_RETR, 0x54);
    WCrt(ba, CRT_ID_END_HOR_RETR, 0x80);
    WCrt(ba, CRT_ID_VER_TOTAL, 0xBF);

    WCrt(ba, CRT_ID_OVERFLOW, 0x1F);	/* overflow reg */

    WCrt(ba, CRT_ID_PRESET_ROW_SCAN, 0x00);	/* no panning */

    WCrt(ba, CRT_ID_MAX_SCAN_LINE, 0x40);	/* vscan */

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

    WCrt(ba, CRT_ID_BACKWAD_COMP_3, 0x10);	/* FIFO enabled */

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

    vgaw(ba, VDAC_MASK, 0xFF);	/* DAC Mask */

    *((volatile unsigned long *)((volatile char*)ba + ECR_FRGD_COLOR)) = 0xFF;
    *((volatile unsigned long *)((volatile char*)ba + ECR_BKGD_COLOR)) = 0;

    /* colors initially set to greyscale */

    vgaw(ba, VDAC_ADDRESS_W, 0);
    for (i = 255; i >= 0 ; i--) {
        vgaw(ba, VDAC_DATA, i);
        vgaw(ba, VDAC_DATA, i);
        vgaw(ba, VDAC_DATA, i);
    }

    /* GFx hardware cursor off */
    WCrt(ba, CRT_ID_HWGC_MODE, 0x00);

    /* Set first to 4 MB, so test will work */
    WCrt(ba, CRT_ID_LAW_CNTL, 0x13);

    /* find *correct* fbsize of z3 board */
    // cv_fbsize = 1024 * 1024 * 2;
    WCrt(ba, CRT_ID_LAW_CNTL, 0x12); /* 2 MB */

    /* Initialize graphics engine */
    GfxBusyWait(ba);
    vgaw16(ba, ECR_FRGD_MIX, 0x27);
    vgaw16(ba, ECR_BKGD_MIX, 0x07);

    vgaw16(ba, ECR_READ_REG_DATA, 0x1000);
    delay(200000);
    vgaw16(ba, ECR_READ_REG_DATA, 0x2000);
    GfxBusyWait(ba);
    vgaw16(ba, ECR_READ_REG_DATA, 0x3fff);
    GfxBusyWait(ba);
    delay(200000);
    vgaw16(ba, ECR_READ_REG_DATA, 0x4fff);
    GfxBusyWait(ba);

    vgaw16(ba, ECR_BITPLANE_WRITE_MASK, ~0);

    GfxBusyWait (ba);
    vgaw16(ba, ECR_READ_REG_DATA, 0xe000);
    vgaw16(ba, ECR_CURRENT_Y_POS2, 0x00);
    vgaw16(ba, ECR_CURRENT_X_POS2, 0x00);
    vgaw16(ba, ECR_READ_REG_DATA, 0xa000);
    vgaw16(ba, ECR_DEST_Y__AX_STEP, 0x00);
    vgaw16(ba, ECR_DEST_Y2__AX_STEP2, 0x00);
    vgaw16(ba, ECR_DEST_X__DIA_STEP, 0x00);
    vgaw16(ba, ECR_DEST_X2__DIA_STEP2, 0x00);
    vgaw16(ba, ECR_SHORT_STROKE, 0x00);
    vgaw16(ba, ECR_DRAW_CMD, 0x01);
    GfxBusyWait (ba);

    /* It ain't easy to write here, so let's do it again */
    vgaw16(ba, ECR_READ_REG_DATA, 0x4fff);

    vgaw16(ba, ECR_BKGD_COLOR, 0x01);
    vgaw16(ba, ECR_FRGD_COLOR, 0x00);

    /* Enable Video Display (Set Bit 5) */
    WAttr(ba, 0x33, 0);

    /* gi = &gp->g_display;
    gi->gd_regaddr	= (void *) kvtop (__UNVOLATILE(ba));
    gi->gd_regsize	= 64 * 1024;
    gi->gd_fbaddr	= (void *) kvtop (__UNVOLATILE(gp->g_fbkva));
    gi->gd_fbsize	= cv_fbsize; */
}

