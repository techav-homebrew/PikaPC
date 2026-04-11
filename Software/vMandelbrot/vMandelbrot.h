


int main();
void mandel();
void draw_pixel(int, int, unsigned char);

/*
void vga_index_write(reg16, unsigned char, unsigned char);
void vga_regset_write(unsigned char *, reg16, int);
void vga_unlock_regs();
void vga_init();
*/




















/*
typedef volatile unsigned char *const reg8;
typedef volatile unsigned short *const reg16;

reg8 vga_misc_out_w = (reg8)((VGA_IO + 0x3c2) ^ 0x03);
reg8 vga_misc_out_r = (reg8)((VGA_IO + 0x3cc) ^ 0x03);

reg8 vga_feature_w  = (reg8)((VGA_IO + 0x3da) ^ 0x03);
reg8 vga_feature_r  = (reg8)((VGA_IO + 0x3ca) ^ 0x03);

reg8 vga_status0_r  = (reg8)((VGA_IO + 0x3c2) ^ 0x03);
reg8 vga_status1_r  = (reg8)((VGA_IO + 0x3da) ^ 0x03);

reg8 vga_enable     = (reg8)((VGA_IO + 0x3c3) ^ 0x03);

reg16 vga_seq       = (reg16)((VGA_IO + 0x3c4) ^ 0x03 - 1);
reg16 vga_crtc      = (reg16)((VGA_IO + 0x3d4) ^ 0x03 - 1);
reg16 vga_gfx       = (reg16)((VGA_IO + 0x3ce) ^ 0x03 - 1);
reg16 vga_attr      = (reg16)((VGA_IO + 0x3c0) ^ 0x03 - 1);

reg8 vga_dac_mask   = (reg8)((VGA_IO + 0x3c6) ^ 0x03);
reg8 vga_dac_idx_w  = (reg8)((VGA_IO + 0x3c7) ^ 0x03);
reg8 vga_dat_stat_r = (reg8)((VGA_IO + 0x3c7) ^ 0x03);
reg8 vga_dat_idx_w  = (reg8)((VGA_IO + 0x3c8) ^ 0x03);
reg8 vga_dat_rw     = (reg8)((VGA_IO + 0x3c9) ^ 0x03);

#define REGSET_CR 25
unsigned char vga_mode13_cr[] = {
    0x5f, 0x4f, 0x50, 0x82, 0x54, 0x80, 0xbf, 0x1f,
    0x00, 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x9c, 0x0e, 0x8f, 0x28, 0x40, 0x96, 0xb9, 0xa3,
    0xff
};

#define REGSET_AR 21
unsigned char vga_mode13_ar[] = {
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x41, 0x00, 0x0f, 0x00, 0x00
};

#define REGSET_SR 5
unsigned char vga_mode13_sr[] = {
    0x03, 0x01, 0x0f, 0x00, 0x0e
};

#define REGSET_GR 9
unsigned char vga_mode13_gr[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x05, 0x0f,
    0xff
};

*/
