; PPCForth local customization file
; Version v0.42b LCB 05/31/2000
; Licensed under the GNU GPL (see LICENSE file for details)


; After reset exec begins at 0xffff:fffc (or equivalently 0x7fff:fffc)
; bank 0 is mapped from 0xfff0:0000 to 0xffff:ffff
; For non-PPC403 users, memory is dual-mapped on the 403, so addresses with
; the most-significant bit set/reset are the same, such as 0x70000000 and
; 0xf0000000



; CHANGE THIS <<<<<<<<<<<---------------------------------------|
b_of_4th equ    0x7fe74000      ; bottom of forth system (RAM)  |
rambase equ     0x7fe00000      ; base of system ram            |
rombase equ     0x7ff00000      ; base of system rom            |
loadpt	equ	0x7fe60000	; used for loads to RAM		|
				; (if rom is not defined!)	|
ramsize equ     0x100000        ; 1M of RAM			|
romsize	equ	0x20000		; 128K of ROM	 		|
ledaddr	equ	0x70110000	; latch (to LED) here		|
szbtblk	equ	0x10000		; 64K boot block (size of block	|
				; for FLASH, holding boot code)	|
systack	equ	(rambase+ramsize-4) ; system stack		|
; Baud rate divisors when using the main system clock as the	|
; source for generating the serial clock			|				
baud33	equ	17		; 115,200 at 33 MHz		|
baud25	equ	13		; 115,200 at 24/25 MHz		|
baud184	equ	9		; 115,200 at 18.432 MHz		|
baud737 equ     11              ; 38,400 at 7.3728 MHz          |
;                                                               |
; --------------------------------------------------------------|



; This is where all the exception vectors are stored
; It must be aligned on a 64K boundary (and be in RAM) to work!
vector	equ	rambase+0x70000

; Now place a branch instruction at the end of the memory we're booting from.
; Power-on reset vector... 
; THIS ASSUMES A 512K-BYTE SIZED MEMORY!!!
	ifdef	rom		; Start at 0 if rom defined
        org     romsize-4
        b       start
        elseif
        org     (rombase+romsize)-4 ; first exec point in ROM after reset
        b       start
        endif


