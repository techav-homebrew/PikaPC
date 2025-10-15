\ from https://rosettacode.org/wiki/Leonardo_numbers#Forth

decimal

: leo-next swap dup >R + over + R> ;
: leo-print 0 do dup . leo-next loop drop drop drop ;
: run
." First 100 Leonardo numbers:" cr
1 1 1 100 leo-print cr
." First 100 Fibonacci numbers:" cr
0 1 0 100 leo-print cr ;
