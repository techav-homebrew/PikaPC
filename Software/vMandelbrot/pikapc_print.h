#include "pikapc_types.h"

#ifndef _PikaPC_Print
#define _PikaPC_Print

#define PPCIO 0x40000000

void putc(char);
void prints(char *);
void println(char *);

#endif  // _PikaPC_Print
