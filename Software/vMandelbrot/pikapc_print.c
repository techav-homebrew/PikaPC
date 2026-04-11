#include "pikapc_print.h"

reg8 com_spls = (reg8)(PPCIO + 0);  // Line State
reg8 com_sphs = (reg8)(PPCIO + 2);  // Handshake State
reg8 com_brdh = (reg8)(PPCIO + 4);  // BRG hi
reg8 com_brdl = (reg8)(PPCIO + 5);  // BRG low
reg8 com_spctl= (reg8)(PPCIO + 6);  // Control
reg8 com_sprc = (reg8)(PPCIO + 7);  // Rx Command
reg8 com_sptc = (reg8)(PPCIO + 8);  // Tx Command
reg8 com_sptb = (reg8)(PPCIO + 9);  // Rx Buffer
reg8 com_sprb = (reg8)(PPCIO + 9);  // Tx Buffer

void putc(char c)
{
    unsigned char s;
    do {
        s = (*com_spls & 0x04);
    } while (s == 0);
    *com_sptb = c;
}

void prints(char * str)
{
    char * s = str;
    while(*s != 0)
    {
        putc(*s);
        s++;
    }
}

void println(char * str)
{
    prints(str);
    putc(0x0d);
    putc(0x0a);
}

