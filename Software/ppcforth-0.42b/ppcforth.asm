; ----------------------------------------------------------------------\
;|  PPCForth- A forth-like language					|
;|  by Larry Battraw (c) 05/31/2000					|
;|  Version v0.42b							|
;|  Licensed under the GNU GPL (see LICENSE file for details)		|
;|  Questions?  Email me at battraw@home.com				|
;|______________________________________________________________________/
;
;       
;
;       Architecture
;
;       Base of (first) dictionary is always in r31; r30 is the virtual
;       machine stack.  r29 holds the second stack pointer (used for
;       conditionals and looping).
;
;
;       r28 is the base pointer-- all *data* references are based on
;       this register.  It holds the address of the base of the data in ram.
;	The macro lwax (load-word-absolute-indexed) will load the address
;	specified, using r28 as the base.  Pass offsets from r28 to this
; 	macro and it will load it into the specified register and add r28
;	to it.  	
;       
;	r27 is the base of forth system in ROM (or RAM).  The macro lwar
;	(load-word-absolute-in-rom) loads an address offset from r27. 
;	Identical to lwax but uses r27 instead of r28.  Sorry about the
;	lame macro names ;(
;
;	It proved advantageous to have two base pointers (r27,r28 for ROM and
;	RAM, respectively) because it allows code and data to live in seperate
;	places and to be moved independently of each other. 
;
;       r26 is the stack used for deferred execution of functions.
;       For example: emit(1).  The address of emit is pushed on the
;       stack and is recalled and executed upon the closing parentheses.
;	r14 is the 'I' and r15 is 'J' within do-loop constructs 
;
;       r1 is actual machine stack for register saving/etc.
;
;       Registers r0, r2-r9 are open for use by routines, as well as
; 	r10-r25.   When a line is parsed, r12 contains a pointer to base of
;	what's left of the tib, and r13 contains the length.  Abort is the
;	only word that uses this info, but IT CAN CRASH THE SYSTEM because
;	it attempts to print what's being pointed at by r12 if there's an
;	error in compilation.  Most words/routines use the r3-9 regs with
;	few exceptions.  
;
;       When dictionary additions are started by create, colon, or variable
;       there are two variables which hold state information-- t_colon and
;       c_colon.  t_colon points to the top (or base, if you will) of the
;       definition in progress.  c_colon points to the next free word to
;       write in data for the word and is updated by comma, allot, dolit,
;       etc.  Please note that allot takes the number of 32-bit words to
;       allocate, NOT bytes.  The current state (compiling/immediate) is
;       kept in mode.
;
;       Finally, please don't expect this "forth" to act like an ANS-
;       compatible forth.  It is both an educational experience for me
;       and a chance to play with various ideas as well as creating a
;       minimalist interactive environment for developement and testing.
;
;

; standard header info

	CPU	PPC403
	BIGENDIAN ON
	include "stddef60.inc"
	include "ppcforth.inc"
	include "local.inc"
	
; Ultralite mode?  Don't compile in much of anything...	
	ifdef	ULTRALITE
LITE	equ	1
	endif


; The org corresponds to either the RAM or ROM location
; you'll be keeping ppcforth at; the b_of_4th value is where all
; volatile data and new dictionary entries will start at.       
;                                                               
	ifdef	rom		; Start at 0 if rom defined		
	org	0		;			
	elseif			;			
	org	loadpt		; loadpt otherwise	
        endif			

start   li      r3,0
        li      r4,0
        mtmsr   r3              ; clear all enables for interrupts
        
ivcache iccci   r0,r4           ; invalidate all i-cache (GC-2K/GCX-16K)
        addi    r3,r3,1
        addi    r4,r4,16
	ifdef	p403gcx
        cmplwi  0,r3,512        ; GC-64; GCX-512
	elseif	
	cmplwi  0,r3,64
	endif
        bne     0,ivcache

        li      r3,0
        li      r4,0
dvcache dccci   r0,r4           ; invalidate all d-cache
        addi    r3,r3,1
        addi    r4,r4,16
	ifdef	p403gcx
        cmplwi  0,r3,256        ; GC-32; GCX-256
	elseif
	cmplwi  0,r3,32
	endif
        bne     0,dvcache


; ------------------------------
; START of PPC403-dependent code
; This allows direct booting to
; 44 as a ROM monitor.
; Delete if you're not using the
; 403, or want to use your own
; init routines.

        li      r3,0
        mtspr   tblo,r3         ; set time base to 0
	mtspr	tbhi,r3
        mtspr   sgr,r3          ; update sgr
        li      r3,2            ; f0000000-f7ffffff d-cacheable
        mtspr   dccr,r3
        li      r3,-1           ; all inst cacheable
        mtspr   iccr,r3         ; update iccr (all inst cacheable)
        lwa     r3,0xff1e0fa0   ; get br0 value 3/3 ws ff1e0fa0 ff1c05a0
        mtdcr   br0,r3          ; set up br0
;        lwa     r3,0xfe1b0a10   ; get br1 value
	lwa 	r3,0xfe1e8500	; set br1 for 1MB 16-bit SRAM, Burst 1-1 wait
        mtdcr   br1,r3          ; set up br1 (ram bank)

; techav - not using br2 or br3 on 403ga
;        lwa     r3,0x011c8fce   ; get br2/br3 value (for IO config)
	
;
;	I have two boards-- one based on the 403gc which has a bank of IO on
;	br2, and a 403gcx board which has IO on br3
;	
	
	
;	ifdef   p403gcx		; This also determines which BRs are used
				; for IO, and whether turbo is enabled
;        mtdcr   br3,r3          ; I/O is on bank reg 3	
;	lwa	r3,0xb0006021	; get io config reg value 0xb0006021 is turbo
	
;        elseif
;        mtdcr   br2,r3          ; I/O is on bank reg 2
;        lwa     r3,0xb0002021   ; get io config reg value
;	endif
        
;	mtdcr   iocr,r3
	lwa 	r3,0x00000023	; user serclk & rts/cts, no timerclk, no debug
	mtdcr 	iocr,r3

; initialize serial port

        bl	initser		; call init routine
	b	nextinit	; then proceed with initialization
	
; It's useful to call this as a subroutine because in a pinch other words
; can call it when the settings may have been munged....	
initser	li      r3,0
        lis     r4,0x4000
        lbz     r5,_spls(r4)    ; clear any errors
        stb     r5,_spls(r4)
        lbz     r5,_sphs(r4)    ; clear any handshake errors
        stb     r5,_sphs(r4)
        stb     r3,_brdh(r4)
				; set baud rate divisor for 33MHz is 403gcx
				; is defined, 25/24 otherwise
;	ifdef	p403gcx
;        li      r3,baud33       
;	elseif
;	li	r3,baud25	; default to 25 or 24 MHz (24 is more accurate)
;	endif
	li 	r3,baud737	; 38400 from 7.3728 MHz serclk

        stb     r3,_brdl(r4)
        li      r3,0x18         ; 8-n-1 RTS active 
        stb     r3,_spctl(r4)
        li      r3,0xb8         ; a8 no DMA, rbr int, hard control of RTS
        stb     r3,_sprc(r4)
        li      r3,0x80         ; transmit enabled
        stb     r3,_sptc(r4)
	blr
	
nextinit 
	lis	r3,0x800	; allow interrupts from serial port
	mtdcr	exier,r3
	li	r3,0
	mtdcr	besr,r3

; END of PPC403-dependent init code
; ---------------------------------


        bl      gbase
gbase   mflr    r27
        addi    r27,r27,(start-*+4) ; get base addr to reference all data
        lwa     r28,b_of_4th
        lwa     r1,systack 	; system stack

; auto-start word if "auto-start" is present
; must set up standard registers first 
	lwar    r31,dict1
        lwax    r3,dict1ptr     ; dict1ptr is really never used...
        stw     r31,0(r3)       ; r31 is used in every case

        lwax    r4,mode
        li      r3,0
        stw     r3,0(r4)        ; set mode to normal (0)

	lwax    r26,pstack
        lwax    r29,rstack
        lwax    r30,dstack
        lwax    r3,tdstack      ; these are set once and are fixed (abort
        stw     r30,0(r3)       ; doesn't need to reset them....)
        lwax    r3,trstack
        stw     r29,0(r3)
        lwax    r3,tpstack
        stw     r26,0(r3)
        lwax    r4,base
        li      r3,16           ; base 16 (hex) as default
        stw     r3,0(r4)

	
	li	r4,0
	lwax	r3,serptr	; reset fifo pointers for serial input
	stw	r4,0(r3)
	lwax	r3,gsptr
	stw	r4,0(r3)
	
	
; -----------------------------
; exception table load code
; -----------------------------

	lwar	r4,crinth
	li	r7,0x100
	bl	instvect

	lwar	r4,mcheckh
	li	r7,0x200
	bl	instvect

	lwar	r4,dsexh
	li	r7,0x300
	bl	instvect

	lwar	r4,isexh
	li	r7,0x400
	bl	instvect
	
	lwar	r4,exth
	li	r7,0x500
	bl	instvect

	lwar	r4,alexh
	li	r7,0x600
	bl	instvect

; Changed to a syscall handler instead of being used to wake tasks--
; it's still possible to call rttaskman by passing 0 in r3 
;	lwar	r4,rttaskman	; initially used to pass control after
				; sending a message to a task
	lwar	r4,systemcall
	li	r7,0xc00
	bl	instvect

	lwar	r4,wdh
	li	r7,0x1020
	bl	instvect
	
	lwar	r4,dtlbm
	li	r7,0x1100
	bl	instvect
	
	lwar	r4,itlbm
	li	r7,0x1200
	bl	instvect

	lwar	r4,debugh
	li	r7,0x2000
	bl	instvect

        lwa     r3,evector      ; get exception vector table address
        mtspr   evpr,r3         ; point prefix reg to it
	
;	lwa     r3,0x5800000    ; allow PIT or FIT events (fit is 2^13)
;	mtspr   tcr,r3
	
	li	r3,0
	mtspr	tcr,r3
	
        mfdcr   r3,exisr        ; reset any interrupt flags
        mtdcr   exisr,r3

	ifndef	LITE
        
	lwar	r4,rttaskman	; install task managers
	li	r7,0x1000
	bl	instvect
	lwar	r4,taskman
	li	r7,0x1010
	bl	instvect

	endif
	
	lwa	r3,0x1200	; 9200 is with EE enabled
	mtmsr	r3		; turns machine/debug exceptions
	
	
	; DEFINING AUTOSTART WILL CRASH YOUR BOARD UNLESS YOU USE NVRAM
	; OR THE RAM IS INITIALIZED
	
	ifdef 	autostart

 	lwar    r4,bootstr	; name of string to print when
				; booting/waiting
        bl      puts
        lwa     r4,0x2fffff     ; amount of time to wait before starting
btlp    bl      igetchq         ; key pressed? (note the i prefix)
        bne     0,abortld       ; is so, skip autostart
        addic.  r4,r4,-1
        bne     0,btlp                                  

	lwar	r4,autostr
	li	r5,10		; PLEASE NOTE THAT IF YOU CHANGE THE NAME OF
				; OF THE AUTO-START WORD, YOU MUST CHANGE THIS
				; TO CORRESPOND WITH THE LENGTH OF THAT NAME
	
	bl	crlf
	bl      isword
        lwz     r3,0(r30)       ; is it a word?
        cmplwi  0,r3,0
        bne     0,go_auto	; if so, execute it
	addi	r30,r30,4	; drop result otherwise
	b 	warm

go_auto	bl	exec	
	b	warm

abortld 
	bl	igetchw		; grab key in buffer
	endif
	

; then fall through to entry
; Always cold start 

;	ifdef	nvram
;	b       warm
;	elseif
	b       cold
;	endif

; cold start location
cold    prstr	"<cold start>\r\n"
	li      r3,0
	mtspr	tcr,r3		; turn off timer events
        
; We re-invalidate the cache because things may have changed in RAM and the
; processor may have cached the old data (very annoying).  This way what you
; have in RAM is what you get.
	
	li      r4,0
cicache	iccci   r0,r4           ; invalidate all i-cache (GC-2K/GCX-16K)
        addi    r3,r3,1
        addi    r4,r4,16
        ifdef   p403gcx
        cmplwi  0,r3,512        ; GC-64; GCX-512
        elseif
        cmplwi  0,r3,64
        endif
        bne     0,cicache

	lwax    r3,dict2ptr     ; get addr of storage location for dict2
        lwax    r4,dict2        ; get address of dict2 in RAM
        stw     r4,0(r3)
        li      r3,0
        stw     r3,0(r4)        ; store 0 at first dict2 address
	lwax	r4,extvect	; secondary external int vector
	stw	r3,0(r4)	; 0==no other handler

; warm start location
warm    
	; this is duplicated from above so we will always set the dict2ptr
	lwax    r3,dict2ptr     ; get addr of storage location for dict2
        lwax    r4,dict2        ; get address of dict2 in RAM
        stw     r4,0(r3)
	
	lwa	r3,0x9200	; 9200
	mtmsr	r3		; turns on ext ints, machine/debug exceptions
	prstr	"<warm start>\r\n"
	lwa     r1,(rambase+ramsize-4)  ; system stack
	prints  entry   	; all entry points end up here
        lwar    r31,dict1
        lwax    r3,dict1ptr     ; dict1ptr is really never used...
        stw     r31,0(r3)       ; r31 is used in every case

        lwax    r4,mode
        li      r3,0
	stw	r3,0(r4)	; set mode to normal (0)
				; set to 1 for compile
        lwax    r26,pstack
        lwax    r29,rstack
        lwax    r30,dstack
        lwax    r3,tdstack      ; these are set once and are fixed (abort
        stw     r30,0(r3)       ; doesn't need to reset them....)
        lwax    r3,trstack
        stw     r29,0(r3)
        lwax    r3,tpstack
        stw     r26,0(r3)
        lwax    r4,base
        li      r3,16           ; base 16 (hex) as default
        stw     r3,0(r4)
	
	li	r4,0
	lwax	r3,serptr	; reset fifo pointers for serial input
	stw	r4,0(r3)
	lwax	r3,gsptr
	stw	r4,0(r3)
	
	lwax	r3,srbase
	stw	r4,0(r3)	; set srec load base (offset) to 0

	ifdef 	LED
	lwa     r4,0x70110000
	li	r3,0
	stb	r3,0(r4)	; turn off LED
	endif


; -----------------------------
; multi-tasking init code
; -----------------------------

        lwax    r4,tasklst
        lwax    r5,taskptr
        stw     r4,0(r5)        ; point to top of list
        li      r3,0
        lwax    r4,ntasks
        stw     r3,0(r4)        ; no tasks yet
	lwax	r4,curtask
	stw	r3,0(r4)	; current task number=0
	lwax	r4,tmanstat
	stw	r3,0(r4)	
	lwax	r4,tm_quedepth	; get queptr
	stw	r3,0(r4)	; point to top of que

	ifndef	LITE
; add Main as only task
        lwar    r3,_multimn
        stwu    r3,-4(r30)
        bl      addtsk          ; add the main loop as the first task
	lwar	r3,_syswatch
	stwu    r3,-4(r30)
        bl      addtsk 
;        bl      mton            ; turn on multi-tasking
	lwax	r3,tasklst	
	stmw	r0,mt_r0(r3)	; the main task uses the standard stacks, 
				; not private
        endif
	
	b       Main		; begin execution of Main loop

; Install exception vector handler
; pass address of handler in r4, offset in r7
instvect lwa    r3,evector 	; addr of base exception vector 
	add	r3,r3,r7
        lwa     r5,_bcode
        subf    r6,r3,r4        ; get diff in addr of taskman-vector
        lwa     r4,0x3ffffff
        and     r6,r6,r4        ; and off high 6 bits
        or      r5,r5,r6        ; or into opcode
        stw     r5,0(r3)        ; store opcode
	blr




; need to provide more info when an exception occurs!
setupex	macro	str
	mtspr	sprg0,r4
	lwar	r4,str
	b	exception
	endm

crinth	setupex crint	

mcheckh	setupex	mcheckd

dsexh	setupex	dsex

isexh   setupex	isex

alexh	setupex	alex
       
prexh   setupex	prex
        
scallh	b systemcall

fith	setupex	fite
        
wdh	setupex	wde
        
dtlbm	setupex	dtlbmiss
	
itlbm	setupex	itlbmiss
	

; System Calls
; 0 - call RT task manager
; 1 - get a character (return -1 if no char ready)
; 2 - check for character (returns 0 if so, -1 otherwise)

systemcall
	mtspr	sprg0,r4
	mtspr	sprg1,r3
	mfcr	r3
	mtspr	sprg2,r3
	mflr	r3
	mtspr	sprg3,r3
; sprg
; 0 - r4
; 1 - r3
; 2 - cr
; 3 - lr
	stwu	r5,-4(r1)
	stwu	r6,-4(r1)
	stwu	r28,-4(r1)
		
	lwa     r28,b_of_4th
	
	mfspr	r3,sprg1	; grab r3 again (the sc number)
	
	ifndef	LITE
	cmplwi	0,r3,0
	beq	docalltm	; call the rt task manager
	endif
	
	cmplwi	0,r3,1
	beq	0,do_getch
	cmplwi	0,r3,2
	beq	0,do_getchq
	cmplwi	0,r3,3
	beq	0,do_putch
	cmplwi	0,r3,4
	beq	0,do_puts
	
scexit	lwz	r28,0(r1)
	lwz	r6,4(r1)
	lwz	r5,8(r1)

	addi	r1,r1,12
	mtspr	sprg1,r3	; save r3 for a sec
	mfspr	r3,sprg3
	mtlr	r3
	mfspr	r3,sprg2
	mtcrf	0xff,r3
	mfspr	r4,sprg0	; restore r3/r4
	mfspr	r3,sprg1
	rfi
	
	
do_puts	
	bl	puts
	b	scexit	
	
do_putch 
	bl 	putch
	b	scexit
		
do_getchq
	bl	getchq
	beq	0,nochrdy
	li	r3,-1
	b	scexit
	
nochrdy	li	r3,0
	b	scexit

do_getch
	bl	getchq
	beq	0,nochrdy
	bl	getchw
	b	scexit

	ifndef	LITE
docalltm
	lwz	r28,0(r1)
	lwz	r6,4(r1)
	lwz	r5,8(r1)
	
	addi	r1,r1,12
	
	mfspr	r3,sprg3
	mtlr	r3
	mfspr	r3,sprg2
	mtcrf	0xff,r3
	mfspr	r4,sprg0
	mfspr	r3,sprg1
	b	rttaskman
	endif


; Called on debug events-- prints registers and waits for a key
debugh	dmpreg	
	stwu	r0,-4(r1)
	stwu	r3,-4(r1)
	stwu	r4,-4(r1)
	stwu	r5,-4(r1)
	stwu	r6,-4(r1)
	stwu	r7,-4(r1)
	stwu	r8,-4(r1)
	stwu	r9,-4(r1)
	mflr	r4
	mfcr	r5
	bl	igetchw		; internal getch-- no interrupt needed
	cmplwi	0,r3,resetchar	; do a reset
	beq	0,chipreset
	mfspr	r3,dbsr
	mtspr	dbsr,r3		; clear debug event
	mtlr	r4
	mr	r3,r5
	mtcrf  	0xff,r3
	lwz	r9,0(r1)
	lwz	r8,4(r1)
	lwz	r7,8(r1)
	lwz	r6,12(r1)
	lwz	r5,16(r1)
	lwz	r4,20(r1)
	lwz	r3,24(r1)
	lwz	r0,28(r1)
	addi	r1,r1,32
	rfci
	
chipreset
	prstr	"\r\nDoing chip reset...\r\n"
	lwa	r3,0x100000
crwtlp	addi	r3,r3,-1
	cmplwi	0,r3,0
	bne	crwtlp
 	lwa	r3,0x20000000	; system reset
	mtspr	dbcr,r3
; bye bye....	


exception
	stwu	r2,-4(r1)
	mfcr	r2
	stwu	r2,-4(r1)
	stwu	r3,-4(r1)
	stwu	r5,-4(r1)
	mr	r2,r4
	bl	initser		; make sure serial port is set up
	mr	r4,r2
	bl	puts
	lwz	r5,0(r1)
	lwz	r3,4(r1)
	lwz	r2,8(r1)
	mtcrf	0xff,r2
	lwz	r2,12(r1)
	mfspr	r4,sprg0
	addi	r1,r1,16
	dmpreg
	
	bl	igetchw
	cmplwi	0,r3,26		; control-Z?
	bne	0,start		; if not, restart
; otherwise zap the external vector and the RAM dictionary
	lwax    r3,dict2ptr     ; get addr of storage location for dict2
        lwax    r4,dict2        ; get address of dict2 in RAM
        stw     r4,0(r3)
        li      r3,0
        stw     r3,0(r4)        ; store 0 at first dict2 address
        lwax    r4,extvect      ; secondary external int vector
        stw     r3,0(r4)        ; 0==no other handler       
	b	start
	
	
endless	b	endless

;
; handles external interrupts-- serial port for now...
exth	stwu	r3,-4(r1)
	stwu	r4,-4(r1)
	stwu	r5,-4(r1)
	stwu	r6,-4(r1)
	stwu	r7,-4(r1)
	stwu	r8,-4(r1)
	stwu	r9,-4(r1)
	stwu	r27,-4(r1)	; base pointer for lwar ops THIS IS NOT SET EXCEPT IN PPCForth
	stwu	r28,-4(r1)	; this is the base pointer for lwax ops!
		
	lwa	r28,b_of_4th
	mflr	r3		; save lr
	stwu	r3,-4(r1)
	mfcr	r3		; and cr
	stwu	r3,-4(r1)

	mfdcr	r3,exisr
	andis.	r3,r3,0x800
	bne	0,serpt
	lwax	r4,extvect	; is there another handler in place?
	lwz	r3,0(r4)
	cmplwi	0,r3,0
	bne	dosecvect	; if not==0, jump to secondary handler
	addi	r1,r1,40	; drop stacked regs
	lwar	r4,extex
	b	exception	; unhandled external interrupt
	
dosecvect 
	stwu	r0,-4(r1)
	stwu	r2,-4(r1)
	stwu	r9,-4(r1)
	stwu	r10,-4(r1)
	mfctr	r4
	stwu	r4,-4(r1)
	mtctr	r3
	bctrl			; call secondary handler...
				; PLEASE NOTE:  The secondary handler MUST NOT
				; do a rfi (as opposed to a blr)!!!!!  It is
				; called as a normal routine only.  This also
				; means no int_entry/exit is required.
	lwz	r4,0(r1)
	mtctr	r4
	lwz	r10,4(r1)
	lwz	r9,8(r1)
	lwz	r2,12(r1)
	lwz	r0,16(r1)	
	addi	r1,r1,20
	b	retser

serpt	mtdcr	exisr,r3	; clear flags
	lis     r9,0x4000
        lbz     r3,_spls(r9)
        andi.   r3,r3,0x78      ; clear any serial errors
        stb     r3,_spls(r9)                          
	lwax	r4,serfifo
	lwax	r5,serptr	; get both pointers
	lwax	r7,gsptr
	lwz	r6,0(r5)
	lwz	r8,0(r7)
	
	addi	r6,r6,1		; inc put ptr
	cmplw	0,r6,r8
	addi	r6,r6,-1	; undo inc
	beq	0,qfull		; if put+1==get, que full!
	
	cmplwi	0,r6,sdepth-1	; check for at end of fifo
	bne	0,stchar
	cmplwi	0,r8,0		; if get ptr=start, que full!
	beq	0,qfull	
	
stchar	lbz	r3,_sprb(r9)	; get char
	
	cmplwi	0,r3,resetchar-1 
	beq	0,do_wake	; either wake or sleep (toggle MSR[WE)
	cmplwi	0,r3,resetchar	;
	beq	0,chipreset	; do_reset
	cmplwi	0,r3,warmrchar	
	beq	0,do_warm
	cmplwi	0,r3,coldrchar	; --clear memory/reset
	beq	0,do_cold
	cmplwi	0,r3,srchar	; -- show regs
	beq	0,show_regs
		
	lwax	r9,dosleep	; time to sleep?
	lwz	r9,0(r9)
	cmplwi	0,r9,0
	beq	0,notsleeping
	mfspr	r9,srr1
	andis.	r9,r9,4
	beq	0,notsleeping	; if bit is zero, we're not asleep
	mfspr	r9,srr1		; otherwise toggle
	xoris	r9,r9,4
	mtspr	srr1,r9
	
notsleeping
	stbx	r3,r4,r6	; save char
	cmplwi	0,r6,sdepth-1	; check for at end of fifo (again)
	beq	0,rst_sptr
	addi	r6,r6,1
retrsp	stw	r6,0(r5)	; save pointer
	
retser	lwz	r3,0(r1)
	mtcrf 	0xff,r3
	lwz	r3,4(r1)
	mtlr	r3
	lwz	r28,8(r1)
	lwz	r27,12(r1)
	lwz	r9,16(r1)
	lwz	r8,20(r1)
	lwz	r7,24(r1)
	lwz	r6,28(r1)
	lwz	r5,32(r1)
	lwz	r4,36(r1)
	lwz	r3,40(r1)
	addi	r1,r1,44
	rfi
	
	
rst_sptr li	r6,0		; reset put ptr
	b	retrsp		; return
	
qfull	cmplwi	0,r3,resetchar
	beq	0,chipreset
	
	li	r3,0x98		; recv int off 88
	lis	r4,0x4000
	stb	r3,_sprc(r4)
	b	retser


show_regs 
	dmpreg
	b	retser

do_wake	mfspr	r3,srr1
	xoris	r3,r3,4
	mtspr	srr1,r3
	b	retser
	
do_warm	mfspr	r3,srr1
	mtmsr	r3
	b	warm

do_cold	
;	mfspr	r3,srr1
;       mtmsr   r3
	li	r3,0
	mtmsr	r3
        b       cold




; ========================================================================
; Main loop for execution and interpretation
; =======================================================================
_multimn dd     (multimn_-*)
        dd      (Main-*)
        db      "Main"
        dd      0
        dd      IMM|MULTI
Main	prints	hi
Mainlp	prints	prompt

        lwax    r4,strbuf      	; point to buffer for string entry
	li	r5,158		; max len
        bl      gets
srechk  lbz     r3,0(r4)
        cmplwi  0,r3,83         ; loading an S-record?
        bne     0,parselp
        lbz     r3,1(r4)
        cmplwi  0,r3,48
        blt     0,parselp
        cmplwi  0,r3,57
        bgt     0,parselp

srecord cmplwi  0,r3,48         ; 0 header
        beq     0,sr_hdr
        cmplwi  0,r3,49         ; 1
        beq     0,rec16
        cmplwi  0,r3,50         ; 2
        beq     0,rec24
        cmplwi  0,r3,51         ; 3
        beq     0,rec32
        cmplwi  0,r3,53         ; 5
        beq     0,adrchg
        cmplwi  0,r3,55         ; 7
        beq     0,eofrec
        cmplwi  0,r3,56         ; 8
        beq     0,eofrec
        b       parselp         ; if it's not any of the above, parse it

sr_hdr  lwar    r4,srhdrm
        bl      puts
        b       ok

adrchg  lwar    r4,adrchgm
        bl      puts
        b       ok

eofrec  lwar    r4,eofrecm
        bl      puts
        b       ok

junk    bl      puts
        lwar    r4,badsr  ; badsrm
        bl      puts
        b       ok

badchk  lwar    r4,badchks
        bl      puts
        b       ok

; chksum reg is r9, len is r8, minus header
rec32   addi    r4,r4,2         ; skip past Sx
        li      r5,2            ; decode length bytes
        cvtnum
        cmplwi  0,r5,0          ; make sure it read it all
        bne     0,junk
        mr      r9,r3           ; checksum reg init
        addi    r8,r3,-5        ; exclude checksum,length address bytes (4+1)
        addi    r4,r4,2         ; skip past length
        li      r5,8            ; decode load address (4 bytes)
        cvtnum
        cmplwi  0,r5,0          ; make sure it read it all
        bne     0,junk
        addi    r4,r4,8         ; skip past load address

        li      r6,32           ; rotate count+8
chkla32 addi    r6,r6,-8        ; do checksum on load addr (all 4 bytes)
        srw     r7,r3,r6
        andi.   r7,r7,0xff
        add     r9,r9,r7
        cmplwi  0,r6,0
        bne     0,chkla32
        
eofcla  addi    r6,r3,-1        ; load address-1
	lwax    r10,srbase
        lwz     r10,0(r10)
        add     r6,r6,r10
rdblp   li      r5,2            ; read a byte at a time
        cvtnum
        cmplwi  0,r5,0          ; make sure it read it all
        bne     0,junk
        add     r9,r9,r3        ; update checksum
        stbu    r3,1(r6)        ; store it in RAM
        addi    r4,r4,2         ; skip to next byte
        addic.  r8,r8,-1        ; dec length
        bne     rdblp

srend   li      r5,2            ; read checksum
        cvtnum
        cmplwi  0,r5,0          ; make sure it read it all
        bne     0,junk
        nor     r9,r9,r9
        andi.   r9,r9,0xff      ; truncate checksum to byte
        cmplw   0,r9,r3
        bne     0,badchk
        b       ok

rec16 	addi    r4,r4,2         ; skip past Sx
        li      r5,2            ; decode length bytes
        cvtnum
        cmplwi  0,r5,0          ; make sure it read it all
        bne     0,junk
        mr      r9,r3           ; checksum reg init
        addi    r8,r3,-3        ; exclude checksum,length address bytes (4+1)
        addi    r4,r4,2         ; skip past length
        li      r5,4            ; decode load address (2 bytes)
        cvtnum
        cmplwi  0,r5,0          ; make sure it read it all
        bne     0,junk
        addi    r4,r4,4         ; skip past load address

        li      r6,32           ; rotate count+8
chkla16 addi    r6,r6,-8        ; do checksum on load addr (all 2 bytes)
        srw     r7,r3,r6
        andi.   r7,r7,0xff
        add     r9,r9,r7
        cmplwi  0,r6,0
        bne     0,chkla16
	b	eofcla

rec24	addi    r4,r4,2         ; skip past Sx
        li      r5,2            ; decode length bytes
        cvtnum
        cmplwi  0,r5,0          ; make sure it read it all
        bne     0,junk
        mr      r9,r3           ; checksum reg init
        addi    r8,r3,-4        ; exclude checksum,length address bytes (4+1)
        addi    r4,r4,2         ; skip past length
        li      r5,6            ; decode load address (3 bytes)
        cvtnum
        cmplwi  0,r5,0          ; make sure it read it all
        bne     0,junk
        addi    r4,r4,6         ; skip past load address

        li      r6,32           ; rotate count+8
chkla24 addi    r6,r6,-8        ; do checksum on load addr (all 3 bytes)
        srw     r7,r3,r6
        andi.   r7,r7,0xff
        add     r9,r9,r7
        cmplwi  0,r6,0
        bne     0,chkla24



; -----------------------end of S-record section-----------------------


parselp bl      chop            ; isolates semantic enitities denoted by
				; spaces/cr/null/etc.
	lbz	r3,0(r4)	; see if we've reached the terminator
	cmplwi	0,r3,0
	beq	0,ok

        cmplwi  0,r3,41         ; check before looking in dict if close-para
        beq     0,close_par

        bl      isword
	lwz	r3,0(r30)	; is it a word?
	cmplwi	0,r3,0
	bne	0,execwd

; falls through from attempting to find name match to here
numcvt  addi    r30,r30,4       ; drop stacked 0
	mr	r12,r4
	mr	r13,r5		; save r5
	stwu	r5,-4(r30)
	subf	r4,r28,r4	; make relative
	stwu	r4,-4(r30)
	bl	asc2b
	cmplwi	0,r5,0		; converted all digits? (0 if so)
	mr	r4,r12
	mr	r5,r13		; restore r4,r5
	bne	0,abort 	; must be garbage/abort interp
	add	r4,r4,r5	; add in length to skip to next token
        lwax    r6,mode         ; see if we're compiling
	lwz	r6,0(r6)
        andi.   r7,r6,2         ; deferred comp?
        bne     0,parselp
        andi.   r7,r6,1
        bne     0,compnum       ; must be compiling, compile in lit

        lbz     r6,0(r4)
        cmplwi  0,r6,41         ; right para?
        beq     0,close_par     ; exec deferred word
        b       parselp

compnum mr      r12,r4          ; save r4
        bl      dolit
        mr      r4,r12          ; restore r4
        lbz     r6,0(r4)
        cmplwi  0,r6,41         ; right para?
        beq     0,close_par     ; exec deferred word
        b       parselp


iabort  bl      cprints
	b	abort

execwd	add	r4,r4,r5	; add in length to skip to next token
	mr	r12,r4		; save r4 (pointer to string interp)
        lbz     r6,0(r4)
        cmplwi  0,r6,40         ; left para?
        beq     0,open_par      ; parse aruments within parentheses
        bl      exec            ; execute word entry on stack
        mr      r4,r12          ; restore r4
        lbz     r6,0(r4)        ;  and char
        cmplwi  0,r6,41         ; right para?
        beq     0,close_par     ; exec deferred word
	b	parselp


ok      lwax    r3,mode
        lwz     r3,0(r3)
        andi.   r3,r3,1         ; compile mode?
        bne     0,Mainlp
        lwar    r4,okmsg
	bl	puts
	b	Mainlp

; deferred execution handling-- pushes exec addr on r26 stack 
; when a closing parenthese is encountered, the addr is popped 
; and executed
; r3 contains exec addr
open_par addi    r4,r4,1        ; skip left parenthese
        stwu    r3,-4(r26)      ; stack exec addr
        addi    r30,r30,4       ; drop stacked version
        b       parselp

close_par
        lwz     r3,0(r26)
        addi    r26,r26,4	; skip right parenthese
        stwu    r3,-4(r30)      ; stack exec addr
        addi    r4,r4,1
        mr      r12,r4
        bl      exec            ; execute word entry on stack
        mr      r4,r12
        b       parselp



; ------------------------------------------------------------------------
; Subroutines-- may use r3-r9 indiscriminately

; pass source in r4, len in r5, dest in r6
; uses r7,r8
cstrcpy li	r7,0
cscplp	cmplw	0,r7,r5
	beq	0,cscpend
	lbzx	r8,r4,r7
	stbx	r8,r6,r7
	addi	r7,r7,1
	b	cscplp
cscpend blr


; uses nothing (cr0)
crlf	stwu	r3,-4(r1)
	stwu	r4,-4(r1)
	mfspr	r4,lr
	li	r3,13
	bl	putch
	li	r3,10
	bl	putch
	mtspr	lr,r4
	lwz	r4,0(r1)
	lwzu	r3,4(r1)
	addi	r1,r1,4
	blr


; Converts number in r3 to string pointed to by r4
; pass number in r3,string buff addr in r4, length in r5 (if b2hexn called)
; uses r3-r8
b2hexn	mr	r6,r5
	b	b2hsave
b2hex	li	r6,8		; default is full 32-bit/8 digits
b2hsave mr	r8,r4		; save buffer start address
	add	r4,r4,r6	; move pointer+n; numbers are written backwds
        lwar    r7,hextbl       ; load addr to translate table
b2hlp	andi.	r5,r3,0xf
	lbzx	r5,r7,r5	; load from offset to table
	stbu	r5,-1(r4)	; store ASCII value
	cmplw	r4,r8		; see if all digits are done
	beq	b2hdone
	srawi	r3,r3,4 	; rotate out digit
	b	b2hlp

b2hdone li	r3,0
	stbx	r3,r4,r6	; add terminator at end of string
	mr	r4,r8
	blr



; pass pointer in r4, N digits in r5, number returned in r3
; r5 corresponds to number of digits (toconvert-converted) (0 if all)
; uses r3,-r9
hex2bn	mfspr	r8,lr
        li      r7,0            ; reg for building number
        li      r9,0            ; index

h2blp	lbzx	r3,r4,r9	; get a new character
	cmplwi	0,r3,0		; check for terminator
	beq	0,eofnum
	cmplwi	0,r3,48 	; lt '0'?
	blt	0,badc
	cmplwi	0,r3,58 	; lt ':'? (9 is 57)
	blt	0,dec_c
hexc	cmpwi	0,r3,65
	blt	0,badc
	cmplwi	0,r3,71
	blt	0,uhex		; if<76 (F), OK (uppercase char)
	cmpwi	0,r3,97 	; 'a'
	blt	0,badc		; nope
	cmpwi	0,r3,102	; 'f'
	bgt	0,badc
	addi	r3,r3,-87	; sub out offset
	b	h2bnxt

dec_c	addi	r3,r3,-48
	b	h2bnxt

eofnum	cmplwi	0,r9 ,0
	beq	0,badc
	b	h2bend

uhex	addi	r3,r3,-55	; sub out offset
h2bnxt	li	r6,4		; number of times to shift left
	slw	r7,r7,r6
	or	r7,r7,r3	; or in most recent digit
	addi	r9,r9,1
	cmplw	0,r9,r5 	; done all digits
	beq	0,h2bend
	b	h2blp

badc	li	r3,0
	mr	r5,r9
	addi	r5,r5,1 	; return (num of chars done)+1
	b	h2be2

h2bend	mr	r3,r7
	xor	r5,r5,r5	; return 0 if everything ok
h2be2	mtspr	lr,r8
	blr



; ouput string--r4 contains string addr
; uses r3-5
puts    mflr    r3
        stwu    r3,-4(r1)
	li	r5,0
putslp	lbzx	r3,r4,r5
	andi.	r3,r3,255
	beq	0,psend
	bl	putch
	addi	r5,r5,1
	b	putslp

psend   lwz     r3,0(r1)
        addi    r1,r1,4
        mtlr    r3
	blr



; Receives a cr-termed string to a buffer pointed to by r4
; pass pointer to buffer in r4
; uses r3-r6
gets	mfspr	r6,lr
	stwu	r7,-4(r1)
	stwu	r8,-4(r1)
	li	r8,0	
	mr	r7,r5		; save r5 (len if !=0)
	mr	r5,r4		; save r4
gslp	bl	getchw
	cmpwi	0,r3,13
	beq	0,gsend
	cmpwi	0,r3,10 	; ignore LF
	beq	0,gslp
	cmpwi	0,r3,8		; backspace?
	beq	0,bksp
	cmpwi   0,r3,9          ; tab?
        bne     0,nottab
	li	r3,32		; make it a space
nottab	cmpwi	0,r3,0x7f
	beq	0,bksp
	stb	r3,0(r4)
	addi	r4,r4,1
	addi	r8,r8,1		; current length
bkspret bl	putch
	cmplwi	0,r7,0		; len==0?  unlimited len...
	beq	0,gslp
	cmplw	0,r7,r8		; done maximum len?
	beq	0,gsend
	b	gslp

bksp    cmplw   0,r4,r5
        beq     0,gslp
        li      r3,8    ; move cursor back one
        bl      putch
        li      r3,32   ; print space
        bl      putch
        li      r3,8    ; move it back again
        bl      putch
        addi    r4,r4,-1
        b       gslp

gsend	bl	crlf
	
	lwz	r8,0(r1)
	lwz	r7,4(r1)
	addi	r1,r1,8
	li	r3,0
	stb	r3,0(r4)
	mr	r4,r5
	mtspr	lr,r6
	blr


dmpregs	mtspr	sprg0,r1	; save r1
	mfcr	r3
	mtspr	sprg2,r3	; save CR
	mflr	r3
	stwu	r3,-4(r1)	; save LR so we can return from this routine
	
; After save, we have the following in sprgX...
; 0 - r1
; 1 - r3
; 2 - cr
; 3 - lr
;	lwa	r3,systack	
;	cmplw	0,r1,r3
;	bgt	stkcrpt		; sp is corrupt (reset to default)
;	lwa	r3,rambase
;	cmplw	0,r1,r3
;	blt	stkcrpt
;	b	drok
	
;stkcrpt	lwa	r1,systack	; restore sp sanity
		
drok	addi    r1,r1,-128
        stmw    r0,0(r1)        ; save all registers
	
	
	bl	initser		; make sure serial port is properly set up
        bl      crlf
	
        
	prhex   r0		; print r0
        twospcs
	mfspr	r3,sprg0	; print r1
        prhex   r3
        twospcs
        prhex   r2		; print r2
        twospcs
	mfspr	r3,sprg1	; restore r3
	prhex	r3		; print it
	twospcs
	
        li      r2,16           ; start loop pointing to saved r4
dmplp   lwzx    r3,r1,r2        ; get saved reg
        prhex   r3
        twospcs
        addi    r2,r2,4
        cmplwi  0,r2,128
        bne     0,dmplp
	
	prstr	"srr0-3: "
	mfspr	r3,srr0
	prhex	r3
	twospcs
	mfspr   r3,srr1
        prhex	r3
	twospcs
        mfspr   r3,srr2
        prhex	r3
	twospcs
        mfspr   r3,srr3
        prhex	r3
	twospcs
	prstr	"esr: "
	mfspr	r3,esr
	prhex	r3
	twospcs
	prstr	"dear: "
	mfspr	r3,dear
	prhex	r3
        bl      crlf
	prstr	"bear: "
	mfdcr	r3,bear
	prhex	r3
	twospcs
	prstr	"besr: "
	mfdcr	r3,besr
	prhex	r3
	twospcs
	
	prstr	"msr: "
	mfmsr	r3
	prhex	r3
	twospcs
	
	prstr	"lr: "
	mfspr	r3,sprg3	; get saved LR value
	prhex	r3
	bl	crlf
	
;	bl      igetchw 	; wait for a key before continuing
	
	lmw     r0,0(r1)	; restore all registers
        addi    r1,r1,128	; de-allocate stack space
        
	lwz	r3,0(r1)	; restore LR for return
	mtlr	r3
	addi	r1,r1,4
	mfspr	r1,sprg0	; and, just in case we messed up the stack pointer, restore r1
	mfspr	r3,sprg2	; then restore CR
	mtcrf	0xff,r3
	blr			; bye bye...



; CHANGE THIS <<<<<<<<<<<---------------------------------------\
; output character in r3
; uses nothing
putch   addi    r1,r1,-8	; set up stack frame
        stw     r7,0(r1)	; save register
        stw     r8,4(r1)	; save register
        lis     r8,0x4000	; load I/O address 0x40000000
putchq  lbz     r7,_spls(r8)	; get serial register address
        andi.   r7,r7,4		; check TX ready bit
        beq     0,putchq	; loop until ready
        stb     r3,_sptb(r8)	; print byte
        lwz     r8,4(r1)	; restore register
        lwz     r7,0(r1)	; restore register
        addi    r1,r1,8		; clear stack frame
        blr

; CHANGE THIS <<<<<<<<<<<---------------------------------------\
; returns cr0 flag (NE if char waiting) as well as spls in r3
; uses r3,r5


igetchq lis     r5,0x4000
        lbz     r3,_spls(r5)
	andi.	r5,r3,0x78	; check for errors and reset
	bne	0,igetchq	; start over if errors
        andi.   r3,r3,0x80
        blr
; returns NE if char ready



; CHANGE THIS <<<<<<<<<<<---------------------------------------\
; Receives single char, waiting as necessary-- returns in r3
; uses r0,r3,r9
igetchw	lis	r9,0x4000
	b	getchwq
	lbz     r3,_spls(r9)
        stb     r3,_spls(r9)
        lbz     r3,_sphs(r9)
        stb     r3,_sphs(r9)
getchwq lbz     r3,_spls(r9)
	andi.   r0,r3,0x78      ; clear any errors
        stb     r0,_spls(r9)      
	bne	0,getchwq
        andi.   r0,r3,128
        bne     0,getch
	
        b       getchwq
getch	lbz	r3,_sprb(r9)
	blr
	
	
getchq	stwu	r4,-4(r1)
	stwu	r5,-4(r1)
	stwu	r6,-4(r1)
	stwu	r7,-4(r1)
	stwu	r8,-4(r1)
	lwax	r4,serfifo
	lwax	r5,serptr
	lwax	r7,gsptr
	lwz	r6,0(r5)
	lwz	r8,0(r7)
	cmplw	0,r6,r8
gcrregs	lwz	r8,0(r1)
	lwz	r7,4(r1)
	lwz	r6,8(r1)
	lwz	r5,12(r1)
	lwz	r4,16(r1)
	addi	r1,r1,20
	blr

getchw	stwu	r4,-4(r1)
	stwu	r5,-4(r1)
	stwu	r6,-4(r1)
	stwu	r7,-4(r1)
	stwu    r8,-4(r1)
	
	lis	r4,0x4000
	li	r3,0xb8
	lbz	r6,_sprc(r4)
	cmplw	0,r3,r6
	beq	0,ldregs
	stb	r3,_sprc(r4)	; enable ints if turned off

ldregs	lwax	r4,serfifo
	lwax	r5,serptr
	lwax	r7,gsptr

w4char	lwz	r6,0(r5)
	lwz	r8,0(r7)
	cmplw	0,r6,r8
	beq	0,w4char
	
charrdy	lbzx	r3,r4,r8	; get char
	cmplwi	0,r8,sdepth-1
	beq	0,rst_gptr
	addi	r8,r8,1
	stw	r8,0(r7)	; save pointer
	b	gcrregs		; restore regs (above)
	
rst_gptr li	r8,0
	stw	r8,0(r7)
	b	gcrregs
	


; Uses r4-r7; pointer is passed and returned in r4 (altered); r5 contains
; length of token before break character encountered
; parses off strings seperated by white space, NL, etc
chop	mfspr	r7,lr
	addi	r4,r4,-1
spclp	lbzu	r6,1(r4)	; read until non-space char hit
	cmplwi	0,r6,0
	beq	0,nxtend	; if term found, end
        cmplwi  0,r6,92         ; comment?
        beq     comment
	cmplwi	0,r6,' '
	beq	0,spclp
	cmplwi	0,r6,13
	beq	0,spclp

	li	r5,1
	bl	isbrk		; is it a breaking char?
        beq     0,nxtend        ; yep, stop and let someone else handle it

brklp   lbzx    r6,r4,r5
	bl	isbrk
	beq	nxtend		; yep, stop and let someone else handle it
	addi	r5,r5,1
	b	brklp

comment li      r6,0
        stb     r6,0(r4)        ; put a null where comment starts
        blr

nxtend  mtspr  lr,r7
	blr

; currently the only syntactically significant chars are:
; <space> <doublequote> <cr> <null>
isbrk	cmplwi	0,r6,' '
	beq	0,isb
	cmplwi	0,r6,13
	beq	0,isb
        cmplwi  0,r6,40         ; left-para '('
        beq     0,isb
        cmplwi  0,r6,41         ; right-para ')'
        beq     0,isb
        cmplwi  0,r6,0          ; eof line/input
	beq	0,isb
isb	blr



; Uses r3,r6-r9; parms passed in r4,r5
cprints li	r6,0
	mfspr	r9,lr
cplp	cmplw	0,r5,r6
	beq	0,cpend
	lbzx	r3,r4,r6
	cmplwi	0,r3,0		; stop if null found
	beq	0,cpend
	bl	putch
	addi	r6,r6,1
	b	cplp
cpend	mr	r5,r6
	mtspr	lr,r9
	blr

; Uses no registers; returns NE/EQ (cr0)
; pass strings to compare in r4,r5 (0-term'd)
; returns ne flag/eq flag

strcmp	addi	r1,r1,-12
	stw	r6,0(r1)
	stw	r7,4(r1)
	stw	r8,8(r1)
	li	r6,0
sclp	lbzx	r7,r4,r6
	lbzx	r8,r5,r6
	addi	r6,r6,1
	cmplw	0,r7,r8
	bne	0,scend
	cmplwi	0,r7,0		; term?
	bne	sclp
scend	lwz	r8,8(r1)
	lwz	r7,4(r1)
	lwz	r6,0(r1)
	addi	r1,r1,12
	blr




; Uses no registers; returns NE/EQ (cr0)
; pass strings to compare in r4,r5 (0-term'd), length in r3
; CHECK THAT STRINGS ARE SAME LENGTH FIRST (will give false eq for
; strings that match up to spec'd len, i.e. len:3  "thi" and "this")!!!!
; returns ne flag/eq flag
cstrcmp addi	r1,r1,-12
	stw	r6,0(r1)
	stw	r7,4(r1)
	stw	r8,8(r1)
	li	r6,0
csclp	lbzx	r7,r4,r6
	lbzx	r8,r5,r6
	addi	r6,r6,1
	cmplw	0,r7,r8
	bne	0,cscend
	cmplw	0,r3,r6 	; compared full length?
	beq	0,cscend
	cmplwi	0,r7,0		; term?
	bne	0,csclp
cscend	lwz	r8,8(r1)
	lwz	r7,4(r1)
	lwz	r6,0(r1)
	addi	r1,r1,12
	blr


; Uses r3,r6,r7,r8,r9; pass addr of string in r4, len in r5; returns addr of
; word entry (or 0) on vm stack
; r31 MUST point to beginning of dict1

isword  mflr    r3
        stwu    r3,-4(r1)       ; save lr
	mr	r8,r5		; save len
	addi	r5,r31,8	; get addr of first string
        li      r7,0            ; indicates we're on first dict
iswlp	b	sanity_check	; make sure this is really a valid entry
	
; passed sanity check on word's dict entry
we_are_sane
	addi	r3,r3,-8	; skip len of len word ;) and pfa
iswpad	addi	r3,r3,-1	; sub out terminator/padding bytes
	lbzx	r9,r5,r3
	cmplwi	0,r9,0
	beq	0,iswpad	; keep eating 0's
	addi	r3,r3,1
        cmplw   0,r3,r8         ; make sure they're the same length!
        beql    cstrcmp
        beq     0,iwtrue        ; equal
        andi.   r3,r6,3         ; check for alignment
        bne     0,iwcd
        add     r5,r5,r6        ; skip to next word
	b	iswlp


printhex stwu	r5,-4(r1)
	stwu	r6,-4(r1)
	stwu	r7,-4(r1)
	stwu	r8,-4(r1)
	stwu	r9,-4(r1)
	prhex	r3
	lwz	r9,0(r1)
	lwzu	r8,4(r1)
	lwzu	r7,4(r1)
	lwzu	r6,4(r1)
	lwzu	r5,4(r1)
	addi	r1,r1,4
	blr
	
	
sanity_check
	
;	dodebug
	lwz	r6,-8(r5)	; first word<=0? (len of entire word)
;	enddebug
	cmplwi	0,r6,0
	ble	0,iwfalse
	mr	r0,r4		; save r4
	andi.	r4,r3,3		; 4 byte aligned size?
	bne	iwcd	
	lwa	r4,max_sane_size ; get max len of "real" word
	cmplw	0,r6,r4		; bigger than sane size?
	bgt	0,iwcd
	lwz	r3,-4(r5)	; get len of string
	
	li	r4,max_string_size
	cmplw	0,r3,r4		; bigger than max string size?
	bgt	iwcd
	
	cmplwi	0,r3,0		; negative/zero len?
	ble	iwcd
	andi.	r4,r3,3		; 4 byte aligned size?
	bne	iwcd
	mr	r4,r0		; restore r4
	b	we_are_sane	; must be kosher
	
iwcd	lwar    r4,cordic       ; corrupt dictionary (not 4-aligned)
        bl      puts
        b       iwf2

iwfalse cmplwi  0,r7,0
        bne     0,iwf2          ; not 0? word not found in second dict either
        lwax    r5,dict2ptr
        lwz     r5,0(r5)        ; get addr of second dict
        li      r7,1
        addi    r5,r5,8         ; point to string
        b       iswlp

iwf2    li      r3,0            ; no word found
	mr	r5,r8		; restore len parm
	stwu	r3,-4(r30)
        lwz     r3,0(r1)
        addi    r1,r1,4
        mtlr    r3
	blr

iwtrue	addi	r5,r5,-8	; point to header
        stwu    r5,-4(r30)      ; store header addr
	mr	r5,r8		; restore len parm
        lwz     r3,0(r1)
        addi    r1,r1,4
        mtlr    r3
	blr

; multi info
        dd      10  	; time before pre-emption
        dd      0	; priority
multimn_
; end of main function

	
entry   db      "Welcome to PPCForth v0.42b, copyright Larry C. Battraw 05/31/2000"
	ifdef	ULTRALITE
		db	" (Ultra-LITE Version)"
	elseif
		ifdef	LITE
		db	" (LITE Version)"
		endif
	endif
	db	13,10,0
hi      db      "Hi.",13,10,0
prompt	db	"-> ",0
autostr	db	"auto-start",0
bootstr db	"[Ready to load PPCForth <press any key to abort autoload word>]",13,10,0
scmp	db	"string",0
hextbl	db	"0123456789abcdef"
whatmsg db	" :huh?",13,10,0
okmsg	db	" ok",13,10,0
notalstr db	" <internal error> colon entry not aligned!",13,10,0
noname	db	" no name provided for colon def!",13,10,0
ionlymsg db	" only allowed in immediate mode!",13,10,0
conlymsg db	" only allowed during compilation!",13,10,0
btbmsg  db      " distance between if-then exceeds 0x3fffc!",13,10,0
notifmsg db     " else must follow IF!",13,10,0
notelseif db    " then must follow IF or ELSE!",13,10,0
nbegmsg db      " until must follow BEGIN!",13,10,0
notdomsg db     " loop must follow DO!",13,10,0
sqmsg   db      " found null before closing quote!",13,10,0
ntdefrd db      " not in deferred compilation mode!",13,10,0
rstackf db	" Unterminated loop or conditional!",13,10,0
uflmsg  db      " <underflow>",13,10,0
tosmsg  db      " <- top ",0
emptymsg db     " (empty) ",0
nonmsg  db      " no name found!",13,10,0
nodefmsg db     " no definition found to forget!",13,10,0
noinline db     " inline words may not be executed directly!",13,10,0
cordic  db      " corrupt dictionary!",13,10,0
a2b_ic	db	" invalid character in number!",13,10,0
notskmsg db     " Must have at least one task to start multitasking!",13,10,0
nstmsg	db	"No such task!",13,10,0
quefmsg db	"Task's message que is full!",13,10,0
mtflags	db	"#SBZ!XRFPM",0

srhdrm  db      "<Header>",13,10,0
adrchgm db      "<Address change>",13,10,0
badsr   db      "<Improperly formatted record!>",13,10,0
badchks db      "<Bad checksum!>",13,10,0
eofrecm db      "<End of download... Now would be a good time to execute it>",13,10,0
;
; Error messages for exceptions
crint	db	"Critical interrupt pin activated.",13,10,0
mcheckd	db	"Machine check D-side.",13,10,0
mchecki db      "Machine check I-side.",13,10,0
dsexpb  db      "Data storage exception (protection bounds).",13,10,0
dsex	db	"Data storage exception.",13,10,0
isex	db	"Instruction storage exception.",13,10,0
extex	db	"Unitialized external interrupt error!",13,10,0
alex	db	"Alignment exception.",13,10,0
prex	db	"Program exception.",13,10,0
scall	db	"System call.",13,10,0
fite	db	"FIT exception.",13,10,0
wde	db	"WD exception.",13,10,0
dtlbmiss db	"Data TLB miss.",13,10,0
itlbmiss db	"Instruction TLB miss.",13,10,0
debuge	db	"Debug exception.",13,10,0


	align	4



; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


dict1
 	
_base   dd      (base_-*)
        dd      (rbase-*)
        db      "base"
        dd      0
        dd      IMM
rbase   lwa     r3,base         ; return as relative, not absolute!
        stwu    r3,-4(r30)
        blr
base_

; uses r3-r8
; number buffer b2asc 
_b2a    dd      (b2a_-*)
        dd      (b2asc-*)
        db      "b2asc"
        db      0,0,0
        dd      IMM
b2asc   mflr    r8
        lwz     r4,0(r30)       ; get string buffer to write result
        addi    r30,r30,4
        stwu    r4,-4(r1)       ; save it (we write a temp buffer first
        lwax    r4,strbuf2      ; because numbers are decoded backwards)
        lwax    r5,base
        lwz     r5,0(r5)
        lwz     r3,0(r30)
        addi    r30,r30,4
        cmplwi  0,r3,0
        beq     0,b2a0		; it's simply a zero
b2alp   divwu   r6,r3,r5
        mr      r7,r6           ; save quotient
        mullw   r6,r6,r5
        subf    r6,r6,r3        ; get remainder
        mr      r3,r7           ; update dividend
        bl      b2aputn         ; add to string
        cmplwi  0,r3,0          ; are we done?
        bne     0,b2alp

b2adone stb     r3,0(r4)        ; save 0 as term for string
        lwz     r5,0(r1)        ; get base of buffer again
        addi    r1,r1,4         ; (drop number)
        lwax    r6,strbuf2
        subf    r7,r6,r4        ; get len of string
        add     r5,r5,r7        ; start writing string at len+buf
        add     r5,r5,r28       ; and make absolute
        stb     r3,0(r5)        ; terminate dest str

b2acp   lbz     r3,0(r6)
        addi    r6,r6,1
        cmplwi  0,r3,0
        beq     0,b2acpd
        stbu    r3,-1(r5)
        b       b2acp

b2acpd  subf    r5,r28,r5       ; make rel
        stwu    r5,-4(r30)      ; return string base addr
	lwax	r6,nlen
	stw	r7,0(r6)	; save number len
        mtlr    r8
        blr


b2aputn cmplwi  0,r6,9          ; see if we need to push it up to a-z
        bgt     0,b2alpha
        addi    r6,r6,48        ; add ascii offset
        stb     r6,0(r4)        ; save it
        addi    r4,r4,1
        blr

b2alpha addi    r6,r6,87        ; add ascii offset (10-ASCII 'a')
        stb     r6,0(r4)        ; save it
        addi    r4,r4,1
        blr

b2a0    lwz     r5,0(r1)
        addi    r1,r1,4
        add     r5,r5,r28       ; make absolute
        li      r3,48
        stb     r3,0(r5)
        li      r3,0
        stb     r3,1(r5)	
	li	r7,1		; length of one	
        b       b2acpd
b2a_



; buffer a2b number
; uses r0,r3-r9; if r5 is not=0 then there was an error in conversion. This is
; unfortunately not visible from the interpreter side, and so special care
; should be taken to detect erroneous strings *before* passing them to a2b
; An extension to automatically (and temporarily) change the base of the
; number based on its prefix has been added.  Use 0x<number> or $<number>
; for hex, and #<number> or %<number> for binary.

_a2b    dd      (a2b_-*)
        dd      (asc2b-*)
        db      "asc2b"
        db      0,0,0
        dd      IMM
asc2b	lwax	r3,base
	lwz	r3,0(r3)
	lwz	r4,0(r30)	; pointer to str to convert
	addi	r30,r30,4
	add	r4,r4,r28	; make absolute
	lwz	r5,0(r30)	; len of number
	li	r0,0		; set sign to positive (==0)
	addi	r30,r30,4
; extension to detect either $/0x for hex, and #/% for binary
	lbz     r6,0(r4)
	cmplwi	0,r5,3		; make sure len>=3 for 0x prefix
	blt     0,hx2pref
	cmplwi	0,r6,'0'
	bne	0,hx2pref
	lbz	r6,1(r4)	; get next char
	cmplwi	0,r6,'x'
	bne	0,hx2pref
	li	r3,16		; must be hex prefix
	addi	r4,r4,2		; skip prefix
	addi	r5,r5,-2	; sub out prefix len
	b	a2bcvt
hx2pref	cmplwi	0,r6,'$'
	bne	0,binpref
	li	r3,16		; hex prefix
	addi    r4,r4,1         ; skip prefix
        addi    r5,r5,-1        ; sub out prefix len
	b	a2bcvt

binpref	cmplwi  0,r6,'#'
        bne     0,bn2pref
	li	r3,2
        addi    r4,r4,1         ; skip prefix
        addi    r5,r5,-1        ; sub out prefix len

bn2pref cmplwi  0,r6,'%'
        bne     0,a2bneg
        li      r3,2
        addi    r4,r4,1         ; skip prefix
        addi    r5,r5,-1        ; sub out prefix len

a2bneg	cmplwi  0,r6,'-'
        bne     0,a2bcvt
	addi	r4,r4,1		; skip sign
	addi    r5,r5,-1        ; sub out prefix len
	li	r0,1		; it's negative

a2bcvt	addi	r5,r5,-1	; (so addr+len points to last char)
	li	r7,1		; multiplier
	li	r8,0		; number in conversion
a2blp	lbzx	r6,r4,r5
	cmplwi	0,r6,32		; stop on space or null
	beq	0,a2bdone
	cmplwi	0,r6,0
	beq	0,a2bdone
	cmplwi	0,r6,'0'	
        blt     0,a2bad         ; bogus char (a2b_bad)
	cmplwi	0,r6,'9'
	bgt	0,a2b_hex	; most likely a hex char
hexcret addi	r6,r6,-48	; convert to real number
	cmplw	0,r6,r3		; make sure number not greater/eq than base
        bge     0,a2bad         ; a2b_bad
	mullw	r9,r7,r6	; multiply number*multiplier
	add	r8,r8,r9	; add into result
	cmplwi	0,r5,0
	beq	0,a2bdone		
	addi	r5,r5,-1	; sub out len
	mullw	r7,r7,r3	; multipler=multipler*base;
	b	a2blp

a2bdone	cmplwi	0,r0,1
	bne	0,ispos
	neg	r8,r8
ispos	stwu	r8,-4(r30)	; save result
	blr

a2b_hex	addi	r6,r6,-39	; sub out to make a=10 (39+10+48=97 ASCII)
	b	hexcret

a2bad   cmplwi  0,r5,0          ; see if on last char
        beq     0,bump          ; bump number down one if so (0=ok)
        b       a2bdone

bump    li      r5,-1
        b       a2bdone
a2b_

 
_abort	dd	(abort_-*)
	dd	(abort-*)
	db	"abort"
	db	0,0,0
	dd	IMM
abort	mfspr	r6,lr
        lwax     r3,mode
        andi.   r3,r3,1         ; see we're compiling
        beq     rstreg
        lwax     r4,t_colon
        li      r3,0
        stw     r3,0(r4)        ; set top of current to 0 (eof dict)
rstreg  mr      r4,r12		; pointer to what's left of TIB
	mr	r5,r13		; restore r4,r5 (we assume they are holding
				; string ptr and len)
	bl	cprints
        lwar    r4,whatmsg
	bl	puts
        lwa     r1,(rambase+ramsize-4)  ; drop any stacked data
        lwax    r3,tpstack
        lwz     r26,0(r3)
        lwax    r3,trstack
        lwz     r29,0(r3)
        lwax    r3,tdstack
        lwz     r30,0(r3)

        lwax     r4,mode
	li	r3,0
	stw	r3,0(r4)	; set mode to normal
	mtspr	lr,r6
        b       Mainlp
abort_
 

_dolit  dd      (dolit_-*)
        dd      (dolit-*)
        db      "dolit"
	db	0,0,0
        dd      XCOMP           ; not allowed in imm mode
dolit   mflr    r7              ; save lr
        lwz     r3,0(r30)
        addi    r30,r30,4       ; drop number
        li      r5,16           ; number of times to shift
        srw     r6,r3,r5        ; shift number over to drop low 16 bits
        lwa     r4,_lis_r3
        or      r4,r4,r6        ; or in top 16
        stwu    r4,-4(r30)
        bl      comma           ; compile it in
        andi.   r6,r3,-1        ; and off high 16
        lwa     r4,_ori_r3
        or      r4,r4,r6
        stwu    r4,-4(r30)
        bl      comma           ; compile it in
        lwa     r4,_stwu_r3     ; get opcode to place r3 on stack
        stwu    r4,-4(r30)
        bl      comma
        mtlr    r7
        blr
dolit_
 

; pulls word off stack and jumps to it (never returns-- word called does blr)
; address must be of dict ENTRY, NOT the actual execution point
_exec   dd      (exec_-*)
	dd	(exec-*)
	db	"exec"
	dd	0
	dd	IMM
exec    lwz     r4,0(r30)       ; get addr of word
	addi	r30,r30,4	; drop addr
        mr      r6,r4           ; save base of word
	lwz	r3,4(r4)	; get length of name
	addi	r3,r3,4 	; skip past first len (len of word)
	add	r3,r3,r4	; point to code field
        lwax    r4,mode
	lwz	r4,0(r4)
        andi.   r5,r4,2         ; deferred compilation mode?
        bne     0,defrd         ; execute it if possible

        andi.   r5,r4,1         ; compiling?
	bne	0,compin	; if so, compile word in, don't exec it
                                ; unless XCOMP

; not compiling, check to make sure it's ok to execute

        lwz     r4,-4(r3)       ; get pfa
        andi.   r5,r4,IMM       ; is it an immediate word (may be or'd with
                                ; XCOMP)
        bne     0,exec2
	andi.	r5,r4,COMP|XCOMP ; only allowed in compile mode?
        bne     0,componly

immchk  andi.   r5,r4,INLINE
        bne     0,no_inline
        andi.   r5,r4,VARIABLE  ; is it a variable?
        bne     0,pushvar       ; push offset addr of data on stack
	andi.   r5,r4,CONSTANT  ; is it a variable?
        bne     0,pushconst     ; push data on stack

; actually execute word
exec2   mtspr   ctr,r3
	bctr

defrd   lwz     r4,-4(r3)       ; get pfa
        andi.   r5,r4,NOCOMP    ; not allowed in compile mode?
        bne     0,nocompal
        b       immchk          ; now process it as a normal immediate word

no_inline lwar  r4,noinline
        bl      puts
        b       abort

; compile in word or exec (XCOMP)
compin  lwz     r4,-4(r3)       ; get pfa
        andi.   r5,r4,NOCOMP    ; not allowed in compile mode?
        bne     0,nocompal
        andi.   r5,r4,XCOMP     ; execute in compile mode?
        bne     0,exec2
        andi.   r5,r4,INLINE
        bne     0,do_inline
        andi.   r5,r4,VARIABLE  ; is it a variable?
        bne     0,compvar       ; compile in code to push addr on stack
        andi.   r5,r4,CONSTANT  ; is it a constant?
        bne     0,compconst     ; compile in code to push data on stack

        lwax    r5,c_colon      ; compiling addr for calculating displacement
        lwz     r5,0(r5)
        lwa     r4,_blcode      ; load up generic bl instruction
        subf    r6,r5,r3        ; r6=target-current
        andi.   r3,r6,3         ; and off to make sure 4-aligned
        bne     0,notalng
        lwa     r3,0x03ffffff
        and     r6,r6,r3        ; and off upper 6 bits (op code)
        or      r4,r4,r6        ; or in offset
        stwu    r4,-4(r30)      ; push on stack
        b       comma           ; write it in and exit


; r6 should contain base of word
do_inline mflr  r7              ; we call comma recursively-- save lr
        lwz     r3,0(r6)        ; get total len
        lwz     r4,4(r6)        ; get string/pfa len
        subf    r3,r4,r3        ; sub out string len
        addi    r3,r3,-4        ; and len of first word
        cmplwi  0,r3,0
        ble     0,eofin         ; make sure we don't try copying nothing
        add     r4,r4,r6        ; get pointer to start of data
                                ; (skip string and pfa len)
        addi    r4,r4,4         ; skip first word (total len)

doinlp  lwz     r5,0(r4)
        addi    r4,r4,4
        stwu    r5,-4(r30)
        bl      comma           ; write it in
        addic.  r3,r3,-4
        bne     0,doinlp

eofin   mtlr    r7
        blr

pushvar subf    r3,r28,r3       ; adjust so r28 is base (0)
        stwu    r3,-4(r30)      ; push addr of data on stack
        blr

pushconst
        lwz   r5,0(r3)
        stwu    r5,-4(r30)
	blr


componly lwar   r4,conlymsg
	bl	puts
	b	abort

nocompal lwar   r4,ionlymsg
        bl      puts
	b	abort



compvar subf    r3,r28,r3       ; adjust so r28 is base (0)
        stwu    r3,-4(r30)      ; push on stack
        b       dolit           ; write in code and exit

compconst lwz     r5,0(r3)
	stwu	r5,-4(r30)
	b	dolit		; compile in code
	

notalng lwar    r4,notalstr
	bl	puts
	b	abort
exec_

; uses r8,r9
_comma	dd	(comma_-*)
	dd	(comma-*)
	db	","
	db	0,0,0
        dd      IMM|XCOMP       ; kind of dubious, but we DO allow COMMA'ing
                                ; in immediate mode to do inline-type words
comma   lwax    r9,c_colon      ; get current compiling addr
	lwz	r9,0(r9)
	lwz	r8,0(r30)	; get stacked value
	addi	r30,r30,4	; drop it
	stw	r8,0(r9)	; store it
        li      r8,0
	addi	r9,r9,4 	; inc compile ptr
        stw     r8,0(r9)        ; store in null to terminate dictionary
        lwax    r8,c_colon
        stw     r9,0(r8)        ; update current dict ptr

        stwu    r7,-4(r1)
        lwax    r7,t_colon
        lwz     r7,0(r7)        ; get ptr to top of def
        subf    r8,r7,r9        ; current-top=size
        stw     r8,0(r7)        ; update size
        lwz     r7,0(r1)
        addi    r1,r1,4
        blr
comma_


; This accepts an absolute address and then executes from there
_jump   dd      (jump_-*)
        dd      (jump-*)
        db      "jump"
        dd      0
        dd      IMM
jump    mflr    r3
        stwu    r3,-4(r1)
        lwz     r3,0(r30)
        addi    r30,r30,4
        mtctr   r3
        bctrl
        lwz     r3,0(r1)
        addi    r1,r1,4
        mtlr    r3
        blr
jump_


; I'm including the "words" function so it's possible to see what
; you're running with...

_words  dd      (words_-*)
        dd      (words-*)
        db      "words"
        db      0,0,0
        dd      IMM
words   mflr    r9
        stwu    r9,-4(r1)
        li      r9,0            ; when 0, on first dict
wds     addi    r5,r31,8        ; get addr of first string
wswlp   lwz     r6,-8(r5)       ; first word==0?
	cmplwi	0,r6,0
        beq     0,eofwds
        stwu    r5,-4(r1)
        stwu    r6,-4(r1)
        mr      r4,r5
        bl      puts
        li      r3,32
        bl      putch
        bl      putch
        lwz     r6,0(r1)
        lwz     r5,4(r1)
        addi    r1,r1,8
	add	r5,r5,r6	; skip to next word
        b       wswlp

eofwds  cmplwi  0,r9,0
        bne     0,eofwd2        ; end of dict2
        lwax    r5,dict2ptr
        lwz     r5,0(r5)
        addi    r5,r5,8
        li      r9,1            ; on second dict2
        b       wswlp

eofwd2  bl	crlf
	lwz     r9,0(r1)
        addi    r1,r1,4
        mtlr     r9
        blr
words_

; include the rest of the dictionary...

	ifndef 	ULTRALITE
	include "dict.asm"
	endif

dictend dd      0

; Mark end of dictionary
        end

