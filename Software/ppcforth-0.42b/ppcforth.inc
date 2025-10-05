; Main include file for PPCForth 
; Version v0.42b LCB 05/31/2000
; Licensed under the GNU GPL (see LICENSE file for details)
; Edit carefully....
;

; special chars to act on

warmrchar	equ	3	; control C for warm restart
srchar		equ	18	; control R to activate sleep mode (set MSR[WE])
resetchar	equ	24	; control X for chip reset
coldrchar	equ	26	; control Z for cold restart


; variables in RAM
; all variable data is accessed using offset from r28)
strbuf  equ     0               ; string buffers for string ops
strbuf2 equ     strbuf+160	; ""
sbuf    equ     strbuf2+160	; ""
argv	equ	sbuf+160	; location to store args for C program calls
argc	equ	argv+64		; enough space for sixteen pointers
mode    equ     argc+4		; operation mode (normal/compile==1)

t_colon equ     mode+4          ; top of colon def
c_colon equ     t_colon+4       ; current pointer to colon def (moves as words
                                ; are compiled in)
var_i   equ     c_colon+4	; do-loop counter vars
var_j   equ     var_i+4		; ""
extvect	equ	var_j+4		; offers a secondary location for handling external ints
				; set to the address of the routine to be called to use...
				; (this is a normal routine, but must only use regs r2-r10 and
				;  do a blr to return)
dosleep	equ	extvect+4	; determines if the chip sets MSR[WE] (stops execution)
base    equ     dosleep+4	; number base for output/interpretation
nlen	equ	base+4		; length of number output by b2a routine
srbase	equ	nlen+4		; base of S record being loaded
serfifo	equ	srbase+4	; FIFO for serial port
sdepth	equ	80		; depth of FIFO
serptr	equ	serfifo+sdepth	; "put" pointer to fifo
gsptr	equ	serptr+4	; "get" pointer from fifo
tpstack equ     gsptr+4         ; these locations store the addresses of the
tdstack equ     tpstack+4       ; top of each stack
trstack equ     tdstack+4	; return stack (for loops/conditionals)
dict1ptr equ    trstack+4       ; first dictionary ptr (stores addr of dict1)
dict2ptr equ    dict1ptr+4      ; second dictionary ptr (stores addr of dict2)
ntasks  equ     dict2ptr+4      ; number of tasks
taskptr equ     ntasks+4        ; points to current task (addr)
curtask	equ	taskptr+4	; current task number
rtptr	equ	curtask+4	; current RT task
tmanstat equ	rtptr+4		; taskman status (indicates RT mode, running)
tm_quedepth equ	tmanstat+4	; pointer in taskman que
tm_que	equ	tm_quedepth+4


; currently uses only lower word for flags (andi.)
tman_multi	equ	1	; currently enabled multitasking
tman_rt		equ	2	; rt task(s) present
tman_fault	equ	4	; fault-- all tasks stopped
tman_rtexec	equ	8	; executing RT words actively (do NOT alter)
tman_msg	equ	16	; message being sent to taskman 


maxtasks equ 10

; defines size of stacks/lists
rstack  equ     (tm_que+128+0x1000) ; 4K R,D,P stacks
dstack  equ     rstack+0x1000   ;
pstack  equ     dstack+0x1000   ; para stack for deferred execution
tasklst equ     pstack+0x1000   ; task list (enough room for 100 tasks)
dict2   equ     tasklst+(tasksz*maxtasks) ; start of second dict (constant)

; multi-tasking status flags

STOPPED   equ   1	; # paused, restartable 
SLEEP     equ   2	; S waiting for some external stimulus
BLOCKED   equ   4	; B waiting on i/o
ZOMBIE    equ   8	; Z tagged by taskman as such (no response to signals)
FAULT     equ   16	; ! tagged by taskman after illegal operation (stopped)
DEAD	  equ	32	; X the task's space in the list may be used; not an
			; active task any longer
RTFLAG	  equ	64 	; real time task
FIXEDRT   equ   128     ; fixed-rate; cannot be called for messages specially
MSGFLAG	  equ	256	; message to task in progress
MSGRDY	  equ   512	; message waiting in que
VINPUT	  equ	1024	; virtual input waiting in message que (needs service)
VOUTPUT	  equ	2048	; virtual output waiting in message que
BLKINP	  equ   4096	; blocked, waiting for virtual input
BLKOUT	  equ	8192	; blocked, waiting for virtual output
SERVICE   equ	16384   ; user-level flag which may be set to make sure the
			; current task has been preempted at least once....
			; this assures that any messages to the taskman have
			; been processed

nflags	  equ	10	; 7 flag types to print
max_sane_size equ 0x7fff ; most words are NOT bigger than 32K!
max_string_size equ 0x7f ; the name of the word cannot be larger than 128 bytes!




; Command values for inter-task messages 
; --------------------------------------
; format for messages is:
;	command value (low 16 is used)
;	task sending message
;	destination task (taskman is 0)
;	<32 bit word for signal/pointer/io/etc..>
;
; Either a command or signal value is ok in the command slot; if a
; signal (0-9) is received, it is autmatically routed to the taskman que.
; Tasks cannot send signals directly to each other...
;
msg_signal	equ	10	; send signal to task
msg_pointer	equ	11	; send a pointer to buffer to task
msg_int		equ	12	; send an integer to task
msg_vinp	equ	13	; feed virtual input to task
msg_voutp	equ	14	; virtual output from task
msg_ping	equ	15	; "ping" task for response; value returned 
				; is 1, value sent is 0 
msg_result	equ	16	; result of signal/etc.

; offsets for message (from end of message+4)
message_command	equ	-16
message_source	equ	-12
message_dest	equ	-8
message_val	equ	-4
message_size	equ	16


; -------------
; Signal values 
; When messages are sent, the command field is compared against sighi; if it
; is equal to or less than it, the message is passed to the taskmanager

SIGINT		equ	1	; soft kill-- sends task kill signal and waits 
; For tasks that are not mt-enhanced this signal will immediately terminate
; the task without waiting, as there is no way to tell the the task it must
; exit
SIGSTOP		equ	2	; temporary suspend
SIGSTART 	equ	3	; restart of suspend
SIGTERM		equ	4	; a "sure" kill-- kills without asking task
SIGHUP		equ	5	; restart task (if possible)
; This sends the task the HUP signal, which will either cause it to exit, or
; to restart if it has provisions for such an activity
SIGBLOCK        equ     6       ; I/O request is pending; buffer full
SIGSETPRI	equ	7
SIGSETPRE	equ	8
SIGSETCB	equ	9

sighi		equ	10	; highest signal value


; Offset table for task
mt_size     equ 0   ; size of task block
mt_word     equ 4   ; location of word (top of header)
mt_lcalllo  equ 8   ; last call back lo/hi
mt_lcallhi  equ 12
mt_st_lo    equ 16  ; start time low and high order words
mt_st_hi    equ 20
mt_quedepth equ 24
mt_status   equ 28
mt_priority equ 32  ; used to determine execute privilege between tasks
		    ; on non-RT tasks this is skip count before executing
		    ; i.e. 0=execute every pass through list
		    ; For RT tasks, this determines which task will be 
		    ; executed if there is a conflict over more than one
		    ; task being queued for execution.
mt_callback equ 36  ; length of time before returning to task (a RT task)
		    ; On non-RT tasks, this is used as the counter for the
		    ; skip count (aka priority) before service
mt_prempt   equ 40  ; length of time to allow a RT task to run (0=no preempt)
mt_sr	    equ 44
mt_cr       equ 48
mt_lr       equ 52
mt_ctr      equ 56
mt_pc       equ 60
mt_r0       equ 64
mt_r1       equ 68
mt_r2       equ 72
mt_r3       equ 76
mt_r4       equ 80
mt_r5       equ 84
mt_r6       equ 88
mt_r7       equ 92
mt_r8       equ 96
mt_r9       equ 100
mt_r10      equ 104
mt_r11      equ 108
mt_r12      equ 112
mt_r13      equ 116
mt_r14      equ 120
mt_r15      equ 124
mt_r16      equ 128
mt_r17      equ 132
mt_r18      equ 136
mt_r19      equ 140
mt_r20      equ 144
mt_r21      equ 148
mt_r22      equ 152
mt_r23      equ 156
mt_r24      equ 160
mt_r25      equ 164
mt_r26      equ 168
mt_r27      equ 172
mt_r28      equ 176
mt_r29      equ 180
mt_r30      equ 184
mt_r31      equ 188
mt_msgque   equ 192 ; 64 byte msg que (there's a 128 byte gap because the
mt_r26stk   equ 320 ;                  stacks grow down and the que grows up)
mt_r29stk   equ 384 ;
mt_r30stk   equ 448 ;
mt_r1stk    equ 512 ; 64 if a 44 word, 256 otherwise
mt_eof      equ 512

tasksz  equ mt_eof

; offsets from end of word for multitasking info 
mt_tsk_callback	equ	-12	; not used for non-RT tasks
mt_tsk_prempt 	equ	-8	; not used for non-RT tasks
mt_tsk_pri	equ	-4


; FIXED POINT:::: exception vector table; must reside on a 64K boundary!!!
evector equ     (rambase+0x70000) ; exception vector table


; serial port addresses based on 0
_spls   equ     0
_sphs   equ     2
_brdh   equ     4
_brdl   equ     5
_spctl  equ     6
_sprc   equ     7
_sptc   equ     8
_sprb   equ     9
_sptb   equ     9




; various opcodes for compilation
_bcode  equ     0x48000000      ; branch (b)
_blcode equ     0x48000001      ; or in offset (must have bits 30,31=0)
_blrcode equ    0x4E800020      ; blr
_mflr_r9 equ    0x7D2802A6      ; mflr r9
_stwu_r9 equ    0x953EFFFC      ; stwu r9,-4(r30)
_mtlr_r9 equ    0x7D2803A6      ; mtlr r9
_push_r9 equ    0x9521FFFC      ; stwu r9,-4(r1)
_sub_r9 equ     0x7D3C4850      ; subf r9,r28,r9  (convert to rel address)
_pop_r9 equ     0x81210000      ; lwz r9,0(r1)
_dropval equ    0x38210004      ; addi r1,r1,4
_lis_r3 equ     0x3C600000      ; lis r3,<val>
_ori_r3 equ     0x60630000      ; ori r3,r3,<val>
_stwu_r3 equ    0x947EFFFC      ; actually stwu r3,-4(r30)
_lwz_r9 equ     0x813E0000      ; lwz r9,0(r30)
_addi_r30 equ   0x3BDE0004      ; addi r30,r30,4
_cmplwi_r9 equ  0x28090000      ; cmplwi 0,r9,0
_beq    equ     0x41820000      ; beq <disp>
_bne    equ     0x40820000      ; bne <disp>


; PFA flags-- some combinations don't make sense....
IMM     equ     1       ; execute only in immediate mode/add in compile
COMP    equ     2       ; allowed in compile mode only
XCOMP   equ     4       ; allowed in compile mode only-- execute then
NOCOMP  equ     8       ; not allowed in compile mode (:)
VARIABLE equ    16      ; used for any data storage such as variable
CONSTANT equ  	32  	; used to return a single 32 bit word
INLINE  equ 	64
MULTI 	equ 	128 	; multi-tasking enhanced
MTRT	equ 	256 	; multi-tasking enhanced and realtime dependent
MTFR	equ	512 	; multi-tasking fixed rate
; Codes for If-Else-Then addresses, Begin-Until, Leave
; Placed in lower two bits of address stacked on R stack
IFTYPE      equ     0
ELSETYPE    equ     1
BEGINTYPE   equ     2   ; NOTE: this type is also used by do-loop!!!
LEAVETYPE   equ     3



; Macros, macros, macros
li      macro   dest,src
        addi    dest,r0,src
        endm

lis     macro   dest,src
        addis   dest,r0,src
        endm

ints_off macro
	stwu	r3,-4(r30)
	mfmsr	r3
	stwu	r3,-4(r1)
	andi.	r3,r3,0x7fff
	mtmsr	r3
	lwz	r3,0(r30)
	addi	r30,r30,4
	endm

ints_on	macro
;	stwu    r3,-4(r30)
;       mfmsr   r3
;        ori	r3,r3,0x8000
;        mtmsr   r3
;        lwz     r3,0(r30)
;        addi    r30,r30,4
	stwu    r3,-4(r30)
	lwz	r3,0(r1)
	mtmsr	r3
	addi	r1,r1,4
	lwz     r3,0(r30)
	addi    r30,r30,4
        endm




lwa     macro   dest,src
        addis   dest,r0,(src>>16)
        ori     dest,dest,(src&0xffff)
        endm


lwi     macro   dest,src
        lwz     dest,(src-start)(r28)
        endm

stwi    macro   dest,src
        stw     dest,(src-start)(r28)
        endm

; loads an address, relative to r27 (base of RAM area)
lwax    macro   dest,src
        addis   dest,r0,(src>>16)
        ori     dest,dest,(src&0xffff)
        add     dest,r28,dest
        endm

; loads an address, relative to r27 (base of 4th system)
lwar    macro   dest,src
        addis   dest,r0,((src-start)>>16)
        ori     dest,dest,((src-start)&0xffff)
        add     dest,r27,dest
        endm


; will only print constant strings (those that are referenced by offset from r27)
prints  macro   src
        lwar    r4,src
        bl      puts
        endm

; Nifty macro to print a string without having to define it with a DB elsewhere...
; Rather expensive in terms of code though.	
prstr	macro 	string
	stwu	r3,-4(r1)
	stwu	r4,-4(r1)
	mflr	r3
	stwu	r3,-4(r1)
	mfcr	r3
	stwu	r3,-4(r1)
	bl	pscode
	db	string
	db	0
	align	4
pscode	mflr	r4
	bl	puts
	lwz	r3,0(r1)
	mtcrf	r3
	lwz	r3,4(r1)
	mtlr	r3
	lwz	r4,8(r1)
	lwz	r3,12(r1)
	addi	r1,r1,16
	endm


twospcs macro
        li      r3,32
        bl      putch
        bl      putch
        endm

svregs	macro
	addi	r1,r1,-28
	stw	r3,0(r1)
	stw	r4,4(r1)
	stw	r5,8(r1)
	stw	r6,12(r1)
	stw	r7,16(r1)
	stw	r8,20(r1)
	stw	r9,24(r1)
	endm

rstregs	macro
	lwz	r9,24(r1)
	lwz	r8,20(r1)
	lwz	r7,16(r1)
	lwz	r6,12(r1)	
	lwz	r5,8(r1)
	lwz	r4,4(r1)
	lwz	r3,0(r1)
	addi	r1,r1,28
	endm




prhex   macro   src
        mr      r3,src
        stwu    r4,-4(r1)
        lwax    r4,strbuf2
        bl      b2hex
        bl      puts
        lwz     r4,0(r1)
        addi    r1,r1,4
        endm

dmpreg2	macro
	endm


dmpreg	macro
	mtspr	sprg1,r3	; save r3
	mflr	r3		; and LR
	mtspr	sprg3,r3
	bl	dmpregs
	mfspr	r3,sprg3	; restore LR
	mtlr	r3
	mfspr	r3,sprg1	; restore r3
	endm	





cvtnum  macro
        addi    r1,r1,-20
        stw     r4,0(r1)
        stw     r6,4(r1)
        stw     r7,8(r1)
        stw     r8,12(r1)
        stw     r9,16(r1)
        bl      hex2bn
        lwz     r9,16(r1)
        lwz     r8,12(r1)
        lwz     r7,8(r1)
        lwz     r6,4(r1)
        lwz     r4,0(r1)
        addi    r1,r1,20
        endm

; 0x3f2=dbcr
; starts tracing
dodebug	macro
	stwu	r3,-4(r1)
	lwa	r3,0x48000000
	mtspr	dbcr,r3
	lwz	r3,0(r1)
	addi	r1,r1,4
	endm

enddebug macro
	stwu	r3,-4(r1)
	li	r3,0
	mtspr	dbcr,r3
	lwz	r3,0(r1)
	addi	r1,r1,4
	endm
;pid	equ	0x3b1
;r0	equ	0
;r1	equ	1
;r2	equ	2
;r3	equ	3
;r4	equ	4
;r5	equ	5
;r6	equ	6
;r7	equ	7


;tlbwehi	macro	rs,ra
;	dd	(0x7c0007a4|(rs<<21)|(ra<<16))
;	endm
;
;tlbwelo	macro	rs,ra
;	dd	(0x7c000fa4|(rs<<21)|(ra<<16))
;	endm
;
;tlbsx	macro	rt,ra,rb
;	dd	(0x7c000724|(rt<<21)|(ra<<16)|(rb<<11))
;	endm
	
;tlbsx.	macro	rt,ra,rb
;	dd	(0x7c000725|(rt<<21)|(ra<<16)|(rb<<11))
;	endm
	
;tlbrehi	macro	rs,ra
;	dd	(0x7c000764|(rs<<21)|(ra<<16))
;	endm
;	
;tlbrelo	macro	rs,ra
;	dd	(0x7c000f64|(rs<<21)|(ra<<16))
;	endm
	
;tlbia	macro
;	dd	0x7c0002e4
;	endm
;
;tlbsync	macro
;	dd	0x7c00046c
;	endm
