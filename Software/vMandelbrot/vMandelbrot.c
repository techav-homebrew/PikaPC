#include "pikapc_print.h"
#include "vgareg.h"
#include "vMandelbrot.h"

#define VID_BASE 0x708a0000
#define SCREEN_W 320
#define SCREEN_H 200

#define FIXED_POINT 1000
#define LIMIT 4 * FIXED_POINT

int main()
{
    // disable interrupts
    __asm__(
        "stwu 3,-4(1)\n\t"
        "li 3,0\n\t"
        "mtexier 3\n\t"
        "lwzu 3,4(1)\n\t"
    );

    // vga_init();
    println("Initializing video ...");

    cv_boardinit();

    println("Starting render ...");
    mandel();

    // re-enable interrupts before returning
    __asm__(
        "stwu 3,-4(1)\n\t"
        "lis 3,0x800\n\t"
        "mtexier 3\r\n"
        "lwzu 3,4(1)\n\t"
    );
    return 0;
}

void mandel()
{
    int i1 = -1 * FIXED_POINT;
    int i2 = 1 * FIXED_POINT;
    int r1 = -2 * FIXED_POINT;
    int r2 = 1 * FIXED_POINT;
    int s1 = (r2 - r1) / SCREEN_W;
    int s2 = (i2 - i1) / SCREEN_H;
    int n;
    for(int y=0; y<SCREEN_H; y++)
    {
        int i3 = s2 * y / FIXED_POINT + i1;
        for(int x=0; x<SCREEN_W; x++)
        {
            int r3 = s1 * x / FIXED_POINT + r1;
            int z1 = r3;
            int z2 = i3;
            for(n=0; n<256; n++)
            {
                int a = z1 * z1 / FIXED_POINT;
                int b = z2 * z2 / FIXED_POINT;
                if((a+b) > LIMIT) break;
                z2 = (z1 * z2 / FIXED_POINT) * 2 + i3;
                z1 = a - b + r3;
            }
            draw_pixel(x, y, (unsigned char)(255-n));
        }
    }
}

void draw_pixel(int x, int y, unsigned char color)
{
    unsigned char* vbuf;
    vbuf = (unsigned char*)((VID_BASE + (y * SCREEN_W) + x) ^ 0x03);
    *vbuf = color;
}

/*
void vga_index_write(reg16 reg, unsigned char idx, unsigned char value)
{
    unsigned short x;
    x = ((unsigned short)(value) << 8);
    x |= ((unsigned short)(idx)) & 0x0ff;

    *reg = x;
}

void vga_regset_write(unsigned char * regset, reg16 reg, int size)
{
    for(int i=0; i<size; i++)
    {
        vga_index_write(reg, (unsigned char)i, regset[i]);
    }
}

void vga_unlock_regs()
{
    vga_index_write(vga_crtc, 0x11, 0x0e);
    vga_index_write(vga_crtc, 0x38, 0x48);
    vga_index_write(vga_crtc, 0x39, 0xa0);
    vga_index_write(vga_crtc, 0x40, 0x01);
    vga_index_write(vga_seq,  0x08, 0x06);
}

void vga_init()
{
    unsigned char i;
    *vga_enable = 0x01;     // enable VGA
    i = *vga_misc_out_r;    // reset index
    *vga_attr = 0x00;       // disable output

    vga_unlock_regs();

    vga_regset_write(&vga_mode13_sr, vga_seq, REGSET_SR);
    vga_regset_write(&vga_mode13_cr, vga_crtc, REGSET_CR);
    vga_regset_write(&vga_mode13_gr, vga_gfx, REGSET_GR);
    vga_regset_write(&vga_mode13_ar, vga_attr, REGSET_AR);

    *vga_dac_mask = 0xff;

    i = *vga_misc_out_r;

    *((volatile unsigned char*)vga_attr) = (unsigned char)0x20;    // normal operation
}
    */

