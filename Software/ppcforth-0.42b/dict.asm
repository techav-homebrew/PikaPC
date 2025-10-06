; PPCForth Dictionary
; Version v0.42b LCB 05/31/2000
; Licensed under the GNU GPL (See the LICENSE file for details)
;
; Dictionary Rules:
;
; The start of the entry is called _<name>  The last addr of the entry is
; <name>_
; Try to keep register usage between r0-r9 (all bets are off for the 
; multitasking words)
       
       	align	4
_emit   dd      (emit_-*)
        dd      (emit-*)
        db      "emit"
        dd      0
        dd      IMM
emit    lwz     r3,0(r30)       ; pull top word on stack
        addi    r30,r30,4       ; drop word
        b       putch
emit_


_key    dd      (key_-*)
        dd      (key-*)
        db      "key"
        db      0
        dd      IMM
key     mflr    r4
        bl      getchw
        stwu    r3,-4(r30)      ; save it on stack
        mtlr    r4
        blr
key_


_keyq   dd      (keyq_-*)
        dd      (keyq-*)
        db      "key",63
        dd      0
        dd      IMM
keyq    mflr    r4
        bl      getchq
        mtlr    r4
        bne     0,keyrdy
        li      r3,0
        stwu    r3,-4(r30)
        blr

keyrdy  li      r3,-1
        stwu    r3,-4(r30)
        blr
keyq_


_cls    dd      (cls_-*)
        dd      (cls-*)
        db      "cls"
        db      0
        dd      IMM
cls     li      r3,12
        stwu    r3,-4(r30)
        b       emit
cls_


; puts only works for strings in RAM (indexed to r28)
_puts   dd      (puts_-*)
        dd      (putstr-*)
        db      "puts"
        dd      0
        dd      IMM
putstr  lwz     r4,0(r30)
        addi    r30,r30,4
        add     r4,r4,r28       ; make absolute
        b       puts
puts_


_gets   dd      (gets_-*)
        dd      (getstr-*)
        db      "gets"
        dd      0
        dd      IMM
getstr  li	r5,0		; unlimited length
	lwz     r4,0(r30)	; dest
        addi    r30,r30,4
        add     r4,r4,r28       ; make absolute
        b       gets
gets_

_cgets  dd      (cgets_-*)
        dd      (cgetstr-*)
        db      "cgets"
        db      0,0,0
        dd      IMM
cgetstr lwz	r5,4(r30)	; len
	lwz     r4,0(r30)	; dest
        addi    r30,r30,8
        add     r4,r4,r28       ; make absolute
        b       gets
cgets_


_plus	dd	(plus_-*)
	dd	(plus-*)
	db	"+"
	db	0,0,0
	dd	IMM
plus	lwz	r3,0(r30)
	lwz	r6,4(r30)
	add	r3,r3,r6
	stw	r3,4(r30)
	addi	r30,r30,4	  ; bump SP up less one word
	blr
plus_

_minus	dd	(minus_-*)
	dd	(minus-*)
	db	"-"
	db	0,0,0
	dd	IMM
minus	lwz	r3,0(r30)
	lwz	r6,4(r30)
	subf	r3,r3,r6
	stw	r3,4(r30)
	addi	r30,r30,4	  ; bump SP up less one word
	blr
minus_

_mul	dd	(mul_-*)
	dd	(mul-*)
	db	"*"
	db	0,0,0
	dd	IMM
mul	lwz	r3,0(r30)
	lwz	r6,4(r30)
	mullw	r3,r3,r6
	stw	r3,4(r30)
	addi	r30,r30,4	  ; bump SP up less one word
	blr
mul_

_div	dd	(div_-*)
	dd	(div-*)
	db	"/"
	db	0,0,0
	dd	IMM
div	lwz	r3,0(r30)
	lwz	r6,4(r30)
	divw	r3,r6,r3
	stw	r3,4(r30)
	addi	r30,r30,4	  ; bump SP up less one word
	blr
div_

_and    dd      (and_-*)
        dd      (and-*)
        db      "and"
        db      0
	dd	IMM
and     lwz     r3,0(r30)
	lwz	r6,4(r30)
        and     r3,r6,r3
	stw	r3,4(r30)
	addi	r30,r30,4	  ; bump SP up less one word
	blr
and_

_or     dd      (or_-*)
        dd      (or-*)
        db      "or"
        db      0,0
	dd	IMM
or      lwz     r3,0(r30)
	lwz	r6,4(r30)
        or      r3,r6,r3
	stw	r3,4(r30)
	addi	r30,r30,4	  ; bump SP up less one word
	blr
or_

_xor    dd      (xor_-*)
        dd      (xor-*)
        db      "xor"
        db      0
	dd	IMM
xor     lwz     r3,0(r30)
	lwz	r6,4(r30)
        xor     r3,r6,r3
	stw	r3,4(r30)
	addi	r30,r30,4	  ; bump SP up less one word
	blr
xor_

_nor    dd      (nor_-*)
        dd      (nor-*)
        db      "nor"
        db      0
	dd	IMM
nor     lwz     r3,0(r30)
	lwz	r6,4(r30)
        nor     r3,r6,r3
	stw	r3,4(r30)
	addi	r30,r30,4	  ; bump SP up less one word
	blr
nor_


_not    dd      (not_-*)
        dd      (not-*)
        db      "not"
        db      0
	dd	IMM
not     lwz     r3,0(r30)
        nor     r3,r3,r3
        stw     r3,0(r30)
        blr
not_


_cfetch dd      (cfetch_-*)
        dd      (cfetch-*)
        db      "c@"
        db      0,0
	dd	IMM
cfetch  lwz     r3,0(r30)
        lbzx    r3,r28,r3
        stw     r3,0(r30)
        blr
cfetch_

_cstore dd      (cstore_-*)
        dd      (cstore-*)
        db      "c!"
        db      0,0
	dd	IMM
cstore  lwz     r3,0(r30)
        lwz     r4,4(r30)
        stbx    r4,r28,r3
        addi    r30,r30,8
        blr
cstore_


_acfetch dd     (acfetch_-*)
        dd      (acfetch-*)
        db      "ac@"
        db      0
        dd      IMM
acfetch lwz    r3,0(r30)
        lbz     r3,0(r3)
        stw     r3,0(r30)
        blr
acfetch_

_acstore dd     (acstore_-*)
        dd      (acstore-*)
        db      "ac!"
        db      0
        dd      IMM
acstore lwz     r3,0(r30)
        lwz     r4,4(r30)
        stb     r4,0(r3)
        addi    r30,r30,8
        blr
acstore_

_ahfetch dd     (ahfetch_-*)
        dd      (ahfetch-*)
        db      "ah@"
        db      0
        dd      IMM
ahfetch lwz     r3,0(r30)
        lhz     r3,0(r3)
        stw     r3,0(r30)
        blr
ahfetch_

_ahstore dd     (ahstore_-*)
        dd      (ahstore-*)
        db      "ah!"
        db      0
        dd      IMM
ahstore lwz     r3,0(r30)
        lwz     r4,4(r30)
        sth     r4,0(r3)
        addi    r30,r30,8
        blr
ahstore_


_fetch  dd      (fetch_-*)
        dd      (fetch-*)
        db      "@"
        db      0,0,0
	dd	IMM
fetch   lwz     r3,0(r30)
        lwzx    r3,r3,r28
        stw     r3,0(r30)
        blr
fetch_

_store  dd      (store_-*)
        dd      (store-*)
        db      "!"
        db      0,0,0
	dd	IMM
store   lwz     r3,0(r30)
        lwz     r4,4(r30)
        stwx    r4,r3,r28
        addi    r30,r30,8
        blr
store_

_pstore  dd	(pstore_-*)
        dd	(pstore-*)
        db      "+!"
        db      0,0
	dd	IMM
pstore  lwz     r3,0(r30)
        lwz     r4,4(r30)
        lwzx   	r5,r3,r28
	add	r5,r5,r4
	stwx	r5,r3,r28
        addi    r30,r30,8
        blr
pstore_


_afetch  dd     (afetch_-*)
        dd      (afetch-*)
        db      "a@"
        db      0,0
	dd	IMM
afetch  lwz     r3,0(r30)
        lwz     r3,0(r3)
        stw     r3,0(r30)
        blr
afetch_

_astore dd      (astore_-*)
        dd      (astore-*)
        db      "a!"
        db      0,0
	dd	IMM
astore  lwz     r3,0(r30)
        lwz     r4,4(r30)
        stw     r4,0(r3)
        addi    r30,r30,8
        blr
astore_


_toabs  dd      (toabs_-*)
        dd      (toabs-*)
        db      ">abs"
        dd      0
        dd      IMM
toabs   lwz     r3,0(r30)
        add     r3,r3,r28       ; r28 always contains base of forth
        stw     r3,0(r30)
        blr
toabs_

_frabs  dd      (frabs_-*)
        dd      (frabs-*)
        db      "<abs"
        dd      0
        dd      IMM
frabs   lwz     r3,0(r30)
        subf    r3,r28,r3       ; r28 always contains base of forth
        stw     r3,0(r30)
        blr
frabs_


_swap   dd      (swap_-*)
        dd      (swap-*)
        db      "swap"
        dd      0
	dd	IMM
swap    lwz     r3,0(r30)
        lwz     r4,4(r30)
        stw     r3,4(r30)
        stw     r4,0(r30)
        blr
swap_

_over   dd      (over_-*)
        dd      (over-*)
        db      "over"
        dd      0
        dd      IMM
over    lwz     r3,4(r30)
        stwu    r3,-4(r30)
        blr
over_

_tor    dd      (tor_-*)
        dd      (tor-*)
        db      ">R"
        db      0,0
        dd      IMM
tor	lwz	r3,0(r30)
	addi	r30,r30,4
	stwu	r3,-4(r29)
	blr
tor_

_fromr  dd      (fromr_-*)
        dd      (fromr-*)
        db      "R>"
        db      0,0
        dd      IMM
fromr	lwz	r3,0(r29)
	addi	r29,r29,4
	stwu	r3,-4(r30)
	blr
fromr_


; a string that contains only a null (0) is returned as len=0
_strlen dd      (strlen_-*)
        dd      (strlen-*)
        db      "strlen"
        db      0,0
        dd      IMM
strlen	lwz	r4,0(r30)
	addi	r30,r30,4
	add	r4,r4,r28
	li	r3,0
strcnt	lbzx	r5,r4,r3
	cmplwi	0,r5,0
	beq	0,strlend
	addi	r3,r3,1
	b	strcnt
strlend	stwu	r3,-4(r30)
	blr	
strlen_

; pass source in r4, len in r5, dest in r6
; uses r7,r8
; ( len src dest  cstrcpy  - )
_cstrcpy dd     (cstrcpy_-*)
        dd      (c_strcpy-*)
        db      "cstrcpy"
        db      0
        dd      IMM
c_strcpy lwz 	r6,0(r30)
	lwzu	r4,4(r30)
	lwzu	r5,4(r30)
	add	r4,r4,r28	; make src/dest absolute
	add	r6,r6,r28
	addi	r30,r30,4
	b	cstrcpy 
cstrcpy_

; ( src dest  strcpy  - )
_strcpy dd     	(strcpy_-*)
        dd      (strcpy-*)
        db      "strcpy"
        db      0,0
        dd      IMM
strcpy	lwz	r5,0(r30)
	lwzu	r4,4(r30)
	addi	r30,r30,4
	add	r4,r4,r28
	add	r5,r5,r28
	li	r3,0
strcplp	lbzx	r6,r4,r3
	stbx	r6,r5,r3
	cmplwi	0,r6,0
	beq	0,strcpd
	addi	r3,r3,1
	b	strcplp

strcpd	blr
strcpy_
	
; pass strings to compare in r4,r5 (0-term'd), length in r3
; PLEASE make sure strings are same length because they are only compared for
; the length specified!
; ( len str1 str2  cstrcmp  - )
_cstrcmp dd     (cstrcmp_-*)
        dd      (c_strcmp-*)
        db      "cstrcmp"
        db      0
        dd      IMM
c_strcmp mflr	r9
	lwz	r5,0(r30)
	lwzu	r4,4(r30)
	lwzu	r3,4(r30)
	addi	r30,r30,4
	add	r4,r4,r28
	add	r5,r5,r28
	bl	cstrcmp
strchk	beq	0,streq
	li	r3,0
	stwu	r3,-4(r30)
	mtlr	r9
	blr

streq	li	r3,-1
	stwu    r3,-4(r30)
        mtlr    r9
        blr
cstrcmp_

; pass strings to compare in r4,r5 (0-term'd) 
; ( str1 str2  strcmp  - )
_strcmp dd     (strcmp_-*)
        dd      (str_cmp-*)
        db      "strcmp"
        db      0,0
        dd      IMM
str_cmp mflr   r9
        lwz     r5,0(r30)
        lwzu    r4,4(r30)
        addi    r30,r30,4
        add     r4,r4,r28
        add     r5,r5,r28
        bl      strcmp
	b	strchk
strcmp_


_ominus dd      (ominus_-*)
        dd      (ominus-*)
        db      "1-"
        db      0,0
	dd	IMM
ominus  lwz     r3,0(r30)
        addi    r3,r3,-1
        stw     r3,0(r30)
        blr
ominus_

_tminus dd      (tminus_-*)
        dd      (tminus-*)
        db      "2-"
        db      0,0
	dd	IMM
tminus  lwz     r3,0(r30)
        addi    r3,r3,-2
        stw     r3,0(r30)
        blr
tminus_

_fminus dd      (fminus_-*)
        dd      (fminus-*)
        db      "4-"
        db      0,0
	dd	IMM
fminus  lwz     r3,0(r30)
        addi    r3,r3,-4
        stw     r3,0(r30)
        blr
fminus_


_oplus  dd      (oplus_-*)
        dd      (oplus-*)
        db      "1+"
        db      0,0
        dd      IMM
oplus   lwz     r3,0(r30)
        addi    r3,r3,1
        stw     r3,0(r30)
        blr
oplus_

_tplus  dd      (tplus_-*)
        dd      (tplus-*)
        db      "2+"
        db      0,0
        dd      IMM
tplus   lwz     r3,0(r30)
        addi    r3,r3,2
        stw     r3,0(r30)
        blr
tplus_

_fplus  dd      (fplus_-*)
        dd      (fplus-*)
        db      "4+"
        db      0,0
        dd      IMM
fplus   lwz     r3,0(r30)
        addi    r3,r3,4
        stw     r3,0(r30)
        blr
fplus_

_rshift dd      (rshift_-*)
        dd      (rshift-*)
        db      "rshift"
        db      0,0
        dd      IMM
rshift  lwz     r3,0(r30)
        lwz     r4,4(r30)
        srw     r4,r4,r3
        stw     r4,4(r30)
        addi    r30,r30,4
        blr
rshift_

_lshift dd      (lshift_-*)
        dd      (lshift-*)
        db      "lshift"
        db      0,0
        dd      IMM
lshift  lwz     r3,0(r30)
        lwz     r4,4(r30)
        slw     r4,r4,r3
        stw     r4,4(r30)
        addi    r30,r30,4
        blr
lshift_



_div2   dd      (div2_-*)
        dd      (div2-*)
        db      "2/"
        db      0,0
        dd      IMM
div2    lwz     r4,0(r30)
        li      r3,1
        srw     r4,r4,r3
        stw     r4,0(r30)
        blr
div2_

_div4   dd      (div4_-*)
        dd      (div4-*)
        db      "4/"
        db      0,0
        dd      IMM
div4    lwz     r4,0(r30)
        li      r3,2
        srw     r4,r4,r3
        stw     r4,0(r30)
        blr
div4_

_mul2   dd      (mul2_-*)
        dd      (mul2-*)
        db      "2*"
        db      0,0
        dd      IMM
mul2    lwz     r4,0(r30)
        li      r3,1
        slw     r4,r4,r3
        stw     r4,0(r30)
        blr
mul2_

_mul4   dd      (mul4_-*)
        dd      (mul4-*)
        db      "4*"
        db      0,0
        dd      IMM
mul4    lwz     r4,0(r30)
        li      r3,2
        slw     r4,r4,r3
        stw     r4,0(r30)
        blr
mul4_



_gt     dd      (gt_-*)
        dd      (gt-*)
        db      ">"
	db	0,0,0
	dd	IMM
gt      lwz     r3,0(r30)
	lwz	r6,4(r30)
        cmplw   0,r6,r3
        bgt     gt_true
        li      r3,0
gt_save stw     r3,4(r30)
        addi    r30,r30,4         ; bump SP up less one word- result pushed on
	blr
gt_true li      r3,-1
        b       gt_save
gt_

_lt     dd      (lt_-*)
        dd      (lt-*)
        db      "<"
	db	0,0,0
	dd	IMM
lt      lwz     r3,0(r30)
	lwz	r6,4(r30)
        cmplw   0,r6,r3
        blt     lt_true
        li      r3,0
lt_save stw     r3,4(r30)
        addi    r30,r30,4         ; bump SP up less one word- result pushed on
	blr
lt_true li      r3,-1
        b       lt_save
lt_

_eq     dd      (eq_-*)
        dd      (eq-*)
        db      "="
	db	0,0,0
	dd	IMM
eq      lwz     r3,0(r30)
        lwz     r4,4(r30)
        cmplw   0,r3,r4
        beq     eq_true
        li      r3,0
eq_save addi    r30,r30,4         ; bump SP up less one word- result pushed on
        stw     r3,0(r30)
	blr
eq_true li      r3,-1
        b       eq_save
eq_

_neq    dd      (neq_-*)
        dd      (neq-*)
        db      "!="
        db      0,0
        dd      IMM
neq     mflr    r9
        bl      eq
        nor     r3,r3,r3        ; r3 holds result from eq
        stw     r3,0(r30)
        mtlr    r9
        blr
neq_

   
_zeq     dd      (zeq_-*)
        dd      (zeq-*)
        db      "0="
        db      0,0
	dd	IMM
zeq     lwz     r3,0(r30)
        li      r4,0
        li      r5,-1
        cmplwi  0,r3,0
        beq     zeq_true
        stw     r4,0(r30)
	blr

zeq_true stw     r5,0(r30)
        blr
zeq_

_cr     dd      (cr_-*)
        dd      (cr-*)
        db      "cr"
        db      0,0
        dd      IMM
cr      li      r3,13
        mflr    r9
        bl      putch
        li      r3,10
        bl      putch
        mtlr    r9
        blr
cr_

_spcs   dd      (spcs_-*)
        dd      (spaces-*)
        db      "spaces"
        db      0,0
        dd      IMM
spaces  mflr	r9
	li      r3,32
	lwz	r4,0(r30)	
	addi	r30,r30,4
pspclp	cmplwi	0,r4,0
	beq	0,eofspcs
	bl	putch	
	addic.	r4,r4,-1
	b	pspclp
eofspcs	mtlr	r9
	blr
spcs_


_tab    dd      (tab_-*)
        dd      (tab-*)
        db      "tab"
        db      0
        dd      IMM
tab     li      r3,32
        mflr    r9
        bl      putch
        bl      putch
	bl	putch
	bl	putch
        mtlr    r9
        blr
tab_


_dupe   dd      (dupe_-*)
        dd      (dupe-*)
        db      100,117,112,0
        dd      IMM
dupe    lwz     r3,0(r30)
        stwu    r3,-4(r30)
        blr
dupe_

_drop   dd      (drop_-*)
        dd      (drop-*)
        db      "drop"
        dd      0
        dd      IMM
drop    addi    r30,r30,4
        blr
drop_

_rot	dd	(rot_-*)
	dd	(rot-*)
	db	"rot"
	db	0
	dd	IMM
rot	lwz	r3,0(r30)
	lwz	r4,4(r30)
	lwz	r5,8(r30)
	stw	r3,4(r30)
	stw	r4,8(r30)
	stw	r5,0(r30)
	blr
rot_	

_sbuf   dd      (sbuf_-*)
        dd      (rsbuf-*)
        db      "sbuf"
        dd      0
        dd      IMM
rsbuf   lwa     r3,sbuf         ; return as relative, not absolute!
        stwu    r3,-4(r30)
        blr
sbuf_


_hex    dd      (hex_-*)
        dd      (hexb-*)
        db      "hex"
        db      0
        dd      IMM
hexb	lwax	r4,base
	li	r3,16
	stw	r3,0(r4)
	blr
hex_

_dec    dd      (dec_-*)
        dd      (decb-*)
        db      "decimal"
        db      0
        dd      IMM
decb    lwax    r4,base
        li      r3,10
        stw     r3,0(r4)
        blr
dec_


_bin    dd      (bin_-*)
        dd      (binb-*)
        db      "binary"
        db      0,0
        dd      IMM
binb    lwax    r4,base
        li      r3,2
        stw     r3,0(r4)
        blr
bin_


_dot	dd	(dot_-*)
	dd	(dot-*)
	db	"."
	db	0,0,0
	dd	IMM
dot     addi	r1,r1,-8
	stw	r4,0(r1)
        mflr    r4
	stw	r4,4(r1)
        lwa     r4,sbuf         
        stwu    r4,-4(r30)   	; number is already on stack (passed to dot)
        bl      b2asc
        lwz    r4,0(r30)
        addi   r30,r30,4
        add    r4,r4,r28       	; make absolute
fintrunc bl     puts
        li      r3,32
        bl      putch
	lwz	r4,4(r1)
	mtlr	r4
	lwz	r4,0(r1)
	addi	r1,r1,8 	; drop saved r4,lr
        blr
dot_

_nlen   dd      (nlen_-*)
        dd      (nlength-*)
        db      "nlen"
        dd      0
        dd      IMM
nlength	lwax	r3,nlen
	lwz	r3,0(r3)
	stwu	r3,-4(r30)
	blr
nlen_


_spfa   dd      (spfa_-*)
        dd      (spfa-*)
        db      "pfa!"
        dd      0
        dd      IMM
spfa    lwz     r4,0(r30)       ; word
        lwz     r6,4(r30)       ; flag to store
        addi    r30,r30,8
        lwz     r5,4(r4)        ; get total string/pfa len
        stwx    r6,r4,r5        ; save pfa back
        blr
spfa_

; context-sensitive pfa store (used immediately after word created)
_cspfa  dd      (cspfa_-*)
        dd      (cspfa-*)
        db      "cpfa!"
        db      0,0,0
        dd      IMM|XCOMP
cspfa   lwax    r3,t_colon      ; get top of current created def
        lwz     r3,0(r3)        ; get addr
        lwz     r4,4(r3)        ; get string/pfa len
        lwz     r5,0(r30)       ; get intended pfa
        addi    r30,r30,4
        stwx    r5,r3,r4        ; save pfa
        blr
cspfa_


; useful for adding several flags to a word
; flag word_addr orpfa
_orpfa  dd      (orpfa_-*)
        dd      (orpfa-*)
        db      "orpfa"
        db      0,0,0
        dd      IMM
orpfa   lwz     r4,0(r30)       ; word
	lwz	r7,4(r30)	; flag to or in
        addi    r30,r30,8
        lwz     r5,4(r4)        ; get total string/pfa len
        lwzx    r6,r4,r5        ; get pfa
        or      r6,r6,r7
        stwx    r6,r4,r5        ; save pfa back
        blr
orpfa_


_timm   dd      (timm_-*)
        dd      (timm-*)
        db      "IMM"
        db      0
        dd      CONSTANT
timm    dd      IMM
timm_

_tcomp  dd      (tcomp_-*)
        dd      (tcomp-*)
        db      "COMP"
        dd      0
        dd      CONSTANT
tcomp   dd      COMP
tcomp_

_txcomp dd      (txcomp_-*)
        dd      (txcomp-*)
        db      "XCOMP"
        db      0,0,0
        dd      CONSTANT
txcomp  dd      XCOMP
txcomp_

_tnocomp dd      (tnocomp_-*)
        dd      (tnocomp-*)
        db      "NOCOMP"
        db      0,0
        dd      CONSTANT
tnocomp dd      NOCOMP
tnocomp_

_tvar   dd      (tvar_-*)
        dd      (tvar-*)
        db      "VAR"
        db      0
        dd      CONSTANT
tvar    dd      VARIABLE
tvar_

_tconst dd      (tconst_-*)
        dd      (tconst-*)
        db      "CONST"
        db      0,0,0
        dd      CONSTANT
tconst  dd      CONSTANT
tconst_

_tinl   dd      (tinl_-*)
        dd      (tinl-*)
        db      "INLINE"
        db      0,0
        dd      CONSTANT
tinl    dd      INLINE
tinl_

_tmulti dd      (tmulti_-*)
        dd      (tmulti-*)
        db      "MULTI"
        db      0,0,0
        dd      CONSTANT
tmulti  dd      MULTI
tmulti_

_tmtrt  dd      (tmtrt_-*)
        dd      (tmtrt-*)
        db      "MTRT"
        dd      0
        dd      CONSTANT
tmtrt 	dd      MTRT
tmtrt_

_tmtfr	dd      (tmtfr_-*)
        dd      (tmtfr-*)
        db      "MTFR"
        dd      0
        dd      CONSTANT
tmtfr  	dd      MTFR
tmtfr_



; Creates an empty definition in the dictionary
; Note that this modifies the dictionary, and so cannot be called while
; within a colon def.  --This is used to create variables and colon
; definitions in immediate mode only.
_create dd      (create_-*)
        dd      (create-*)
        db      "create"
        db      0,0
        dd      IMM|NOCOMP
create  mflr    r3
        stwu    r3,-4(r1)       ; save lr

        lwax    r5,dict2ptr     ; compile into RAM dictionary
        lwz     r5,0(r5)

iselp   lwz     r6,0(r5)        ; first word==0?
        cmplwi  0,r6,0
        beq     0,fndend
        add     r5,r5,r6        ; skip to next word
        b       iselp

fndend  lwax    r3,t_colon
        stw     r5,0(r3)        ; save start of word
        mr      r4,r12          ; restore pointer to interpreted string
        bl      chop            ; get next word (name of colon)
        add     r12,r4,r5       ; skip past name of def (so parse doesn't see)
        cmplwi  0,r5,0          ; zero length name?
        beq     0,c_abort       ; if so, abort!

        lwz     r3,0(r3)        ; restore start of word ptr
        addi    r6,r3,8         ; point to name area (dest)

        bl      cstrcpy         ; copy it in
        li      r7,0
stzlp   stbx    r7,r6,r5        ; store a null in
        addi    r5,r5,1         ; push ptr forward one byte
        andi.   r8,r5,3
        bne     0,stzlp

; r3 should _still_ contain pointer to top of colon def!!!!!!
        stwx    r7,r6,r5        ; store 0 in pfa (IMM)
        addi    r5,r5,8         ; add in length of name length word and pfa
        stw     r5,4(r3)	; store in string/pfa len 
        addi    r5,r5,4         ; add in len of offset to next word

        stw     r5,0(r3)        ; save total length of def
        add     r5,r5,r3        ; add total header length to current addr
        li      r4,0
        stw     r4,0(r5)        ; add dict term
        lwax    r3,c_colon
        stw     r5,0(r3)        ; save current compile addr
        lwz     r3,0(r1)        ; restore lr
        addi    r1,r1,4
        mtlr    r3
        blr

c_abort lwar    r4,noname
        bl      puts

        b       abort
create_



; compiles in new definition for word
; uses r3-r8 (cstrcpy uses r7 and r8)
_colon	dd	(colon_-*)
	dd	(colon-*)
	db	":"
	db	0,0,0
	dd	NOCOMP	; definitely not allowed while compiling ;-)

colon   mflr    r9              ; save lr (create,cstrcpy,chop leave r9 alone)
        bl      create          ; create def from name in tib
        lwax    r3,mode
        li      r4,1            ; compile mode
        stw     r4,0(r3)

        lwax    r5,t_colon      ; get top compile addr
        lwz     r5,0(r5)
        li      r4,0
        stw     r4,0(r5)        ; store a zero in len so dict search stops
                                ; without attempting to search beyond this
                                ; Necessary so len doesn't need to be updated
                                ; and term moved with every word compiled in


        lwax    r5,c_colon      ; get current compile addr
        lwz     r5,0(r5)

        lwa     r4,_mflr_r9     ; load up mflr r9 op-code
	stw	r4,0(r5)
	addi	r5,r5,4 	; inc ptr
        lwa     r4,_push_r9     ; load up stwu r9,-4(r1) op-code
	stw	r4,0(r5)
	addi	r5,r5,4 	; inc ptr

        lwax     r4,c_colon
	stw	r5,0(r4)

col_end mtlr    r9              ; restore lr
	blr

colon_


_semicolon dd	(semicolon_-*)
	dd	(semicolon-*)
	db	";"
	db	0,0,0
	dd	XCOMP	; executes while compiling
semicolon
        lwax    r3,mode
	li	r4,0
	stw	r4,0(r3)	; clear compile mode
	lwax	r3,trstack	; get top of R stack
	lwz	r3,0(r3)
	cmplw	0,r3,r29	; make sure R stack is empty
	bne	0,rstkerr	

        lwax    r4,c_colon
	lwz	r4,0(r4)	; get actual address pointed to

        lwa     r3,_pop_r9
	stw	r3,0(r4)
        lwa     r3,_dropval
	stw	r3,4(r4)
	lwa	r3,_mtlr_r9
	stw	r3,8(r4)
	lwa	r3,_blrcode
	stw	r3,12(r4)
	li	r3,0
	stw	r3,16(r4)	; and end of dict code word
        lwax     r3,t_colon
	lwz	r5,0(r3)	; get start of def pointer
	subf	r3,r5,r4
	addi	r3,r3,16	; add in four opcode lengths for total length
	stw	r3,0(r5)	; save
	blr

rstkerr	lwar	r4,rstackf	; R stack still has contents
	bl	puts
	b	abort

semicolon_



_if	dd	(if_-*)
	dd	(if-*)
	db	"if"
	db	0,0
        dd      XCOMP
if      mflr    r3
        stwu    r3,-4(r1)
        lwax     r3,c_colon
        lwz     r3,0(r3)        ; get current compile addr
        lwa     r4,_lwz_r9
	lwa	r5,_addi_r30
	lwa	r6,_cmplwi_r9
        lwa     r7,_beq
        stwu    r4,-4(r30)
        bl      comma
        stwu    r5,-4(r30)
        bl      comma
        stwu    r6,-4(r30)
        bl      comma
        stwu    r7,-4(r30)
        bl      comma
	addi	r4,r3,12	; get addr of beq opcode to push
;        addi    r3,r3,16
        ori     r4,r4,IFTYPE    ; or in IF addr type
;        lwax    r5,c_colon
	stwu	r4,-4(r29)	; push on return stack
;        stw     r3,0(r5)

        lwz     r3,0(r1)
        addi    r1,r1,4
        mtlr    r3
	blr
if_


_else   dd      (else_-*)
        dd      (else-*)
        db      "else"
	dd	0
        dd      XCOMP
else    mflr    r7
        lwax    r3,c_colon
	lwz	r3,0(r3)	; get current compile addr
	lwz	r4,0(r29)	; pull prev pushed addr of beq off R stack
        li      r5,3
        and     r5,r5,r4        ; and off high bits, leave addr type
        cmplwi  0,r5,IFTYPE     ; make sure else is following if
        bne     0,notif
        xor     r4,r4,r5        ; xor out addr type
        subf    r5,r4,r3        ; offset from if (beq)
        addi    r5,r5,4         ; skip jump laid down
        lwa     r6,0x3fffc      ; make sure disp isn't bigger than 18 bits
        and     r5,r5,r6
        lwz     r6,0(r4)        ; get opcode in mem
	or	r5,r6,r5	; or in offset
	stw	r5,0(r4)	; update opcode
        lwa     r5,_bcode       ; load up branch to follow code
        stwu    r5,-4(r30)      ; store it
        bl      comma
        lwax    r3,c_colon
        lwz     r3,0(r3)
        addi    r3,r3,-4        ; back up one to point to branch
        stw     r3,0(r29)       ; save the addr for branch (note this replaces
                                ; val on stack already)
        mtlr    r7
	blr

notif   lwar    r4,notifmsg
        bl      puts
        b       abort
else_


; THEN must pull off and save any addresses of the LEAVE type, restoring
; them once it has found and pulled of an address of the IF type
_then	dd	(then_-*)
	dd	(then-*)
	db	"then"
	dd	0
        dd      XCOMP
then    lwax    r3,c_colon
	lwz	r3,0(r3)	; get current compile addr
        mr      r6,r30          ; save current D stack state
        stwu    r6,-4(r1)       ; save it on stack
thenlp  lwz     r4,0(r29)       ; pull prev pushed addr of beq off R stack
	addi	r29,r29,4	; drop it
        andi.   r5,r4,3         ; and off high 30 bits, leave addr type
        cmplwi  0,r5,LEAVETYPE  ; see if leave is in way
        beq     isleave
        cmplwi  0,r5,IFTYPE     ; make sure then is following if
        beq     0,eqif
        cmplwi  0,r5,ELSETYPE   ; make sure else is following if
        bne     0,notelse
        b       uncond          ; else is an unconditional branch

isleave stwu    r4,-4(r30)      ; save leave addr
        b       thenlp

eqif    xor     r4,r4,r5        ; xor out lower two bits (TYPE)
        subf    r5,r4,r3        ; offset from if (beq)

cond    lwa     r6,0x3fffc      ; make sure disp isn't bigger than 18 bits
andin   and     r5,r5,r6
        lwz     r6,0(r4)        ; get opcode in mem
	or	r5,r6,r5	; or in offset
	stw	r5,0(r4)	; update opcode

restlv  lwz     r6,0(r1)        ; restore saved SP ptr
        addi    r1,r1,4         ; drop saved val
rstlvlp cmplw   0,r30,r6        ; if equal, nothing pushed on
        beq     0,thenend
        lwz     r3,0(r30)
        addi    r30,r30,4       ; drop saved leave addr
        stwu    r3,-4(r29)      ; re-stack leave addr
        b       rstlvlp

thenend blr

uncond  lwa     r6,0x3fffffc    ; disp is allowed 24 bits
        b       andin

notelse lwar    r4,notelseif
        bl      puts
        b       abort

then_

_i      dd      (i_-*)
        dd      (icode-*)
        db      "i"
        db      0,0,0
        dd      IMM
icode   stwu    r14,-4(r30)
        blr

i_

_j      dd      (j_-*)
        dd      (jcode-*)
        db      "j"
        db      0,0,0
        dd      IMM
jcode   stwu    r15,-4(r30)
        blr
j_



; DO-LOOP
; this is a big, ugly mess, but I can see no other way of doing it
_do     dd      (do_-*)
        dd      (do-*)
        db      "do"
        db      0,0
        dd      XCOMP
do      mflr    r3
        lwar    r4,topdo        ; get ptr to table on opcode to copy
        stwu    r3,-4(r1)

        li      r6,0
docp    addi    r6,r6,1
        lwz     r5,0(r4)
        cmplwi  0,r6,5		; number of words to copy
        stwu    r5,-4(r30)
        bl      comma
        addi    r4,r4,4
        bne     0,docp


        lwax    r3,c_colon
        lwz     r3,0(r3)
        ori     r3,r3,BEGINTYPE ; save as type begin (be careful)
        stwu    r3,-4(r29)
        lwz     r3,0(r1)
        addi    r1,r1,4         ; drop val
        mtlr    r3
        blr
; end of compile-time code

; executed at run-time once for top of loop
topdo   stwu    r14,-4(r29)     ; i below j                             4
        stwu    r15,-4(r29)     ; j on top                              8
        lwz     r14,0(r30)      ; get i off stack                       12
        lwz     r15,4(r30)      ;   " j "                               16
        addi    r30,r30,8       ; drop them                             20
do_

; loop portion

_loop   dd      (loop_-*)
        dd      (loop-*)
        db      "loop"
        dd      0
        dd      XCOMP
loop    mflr    r3
        lwar    r4,botdo        ; get ptr to table on opcode to copy
        stwu    r3,-4(r1)

        li      r6,0
lpcp    addi    r6,r6,1
        lwz     r5,0(r4)
        cmplwi  0,r6,2		; number of words to copy
        stwu    r5,-4(r30)
        bl      comma
        addi    r4,r4,4
        bne     0,lpcp


; Calculate jump and check for do-loop pairing
; remember begin and do types are the same
        mr      r3,r30
        stwu    r3,-4(r1)       ; save D stack top addr
looplp  lwz     r3,0(r29)       ; get loop top addr
        addi    r29,r29,4       ; drop loop addr off stack
        andi.   r5,r3,3         ; and off high bits, leave addr type
        cmplwi  0,r5,LEAVETYPE
        beq     0,svleave       ; if leave, save leave addr
        cmplwi  0,r5,BEGINTYPE  ; make sure loop is following do
        bne     0,notdo
        xor     r3,r3,r5        ; xor out type

        lwax    r5,c_colon
        lwz     r5,0(r5)        ; get current compile ptr
        subf    r5,r5,r3        ; offset from loop top
        lwa     r6,0x0000fffc   ; and mask
        lwa     r7,_bne
        and     r5,r5,r6        ; and off bits
        or      r7,r7,r5        ; or in offset
        stwu    r7,-4(r30)
        bl      comma           ; write it in

; now write in trailer after writing calculated branch (r4 holds read addr)

        li      r6,0
lpcp2   addi    r6,r6,1
        lwz     r5,0(r4)
        cmplwi  0,r6,5
        stwu    r5,-4(r30)
        bl      comma
        addi    r4,r4,4
        bne     0,lpcp2

; now pull leaves off stack and fix up branch addresses
; this routine is used by several other words!!! (until, loop...)
fixleave lwax    r5,c_colon
        lwz     r5,0(r5)        ; get current compile ptr
        lwz     r3,0(r1)        ; pull off D stack state before possible
        addi    r1,r1,4         ; leaves pushed on
fixlvlp cmplw   0,r3,r30
        beq     0,eofloop       ; if equal, exit
        lwz     r4,0(r30)
        addi    r30,r30,4       ; drop leave addr
        xori    r4,r4,LEAVETYPE ; xor out type
        lwa     r7,_bcode
        subf    r6,r4,r5        ; get offset between leave and current addr
        or      r7,r7,r6        ; or in offset
        stw     r7,0(r4)        ; store it back in mem
        b       fixlvlp


eofloop lwz     r3,0(r1)
        addi    r1,r1,4
        mtlr    r3
        blr
; end of loop runtime

svleave stwu    r3,-4(r30)      ; save leave addr
        b       looplp

; code copied in for runtime use
botdo   addi    r14,r14,1       ; inc i                                 4
        cmplw   0,r14,r15       ;                                       8
; executed every loop iteration
; bne calculated and inserted here

        lwz     r15,0(r29)      ; j on top                              4
        lwz     r14,4(r29)      ; i below j                             8
        addi    r29,r29,8       ; drop old i/j                          12
; execute after loop completes

notdo   lwar    r4,notdomsg
        bl      puts
        b       abort
loop_



_begin  dd      (begin_-*)
        dd      (begin-*)
        db      "begin"
        db      0,0,0
        dd      XCOMP
begin   lwax    r3,c_colon
        lwz     r3,0(r3)        ; get current compile addr
        ori     r3,r3,BEGINTYPE
        stwu    r3,-4(r29)      ; push on return stack
        blr
begin_

_until  dd      (until_-*)
        dd      (until-*)
        db      "until"
        db      0,0,0
        dd      XCOMP
until   mflr    r3
        stwu    r3,-4(r1)       ; save LR
        mr      r3,r30
        stwu    r3,-4(r1)       ; save current D stack SP
        lwax    r3,c_colon
        lwz     r3,0(r3)        ; get current compile addr
        lwa     r4,_lwz_r9
	lwa	r5,_addi_r30
        lwa     r6,_cmplwi_r9
        lwa     r7,_beq
        stwu    r4,-4(r30)      ; store lwz,addi,cmplwi opcodes
        bl      comma
        stwu    r5,-4(r30)
        bl      comma
        stwu    r6,-4(r30)
        bl      comma

untillp lwz     r4,0(r29)       ; get loop top addr
        addi    r29,r29,4       ; drop it
        andi.   r5,r4,3         ; and off high bits, leave addr type
        cmplwi  0,r5,LEAVETYPE
        beq     0,svleave2      ; if leave, save leave addr
        cmplwi  0,r5,BEGINTYPE      ; make sure until is following begin
        bne     0,notbeg
        xori    r4,r4,BEGINTYPE

        lwax    r5,c_colon      ; get current ptr again
        lwz     r3,0(r5)
        subf    r5,r3,r4        ; offset from loop top
;        addi    r5,r5,4         ; adjust
        lwa     r6,0x0000fffc   ; all bit but last 2 ==1
        and     r5,r5,r6
        or      r7,r7,r5        ; or in offset
        stwu    r7,-4(r30)      ; store opcode
        bl      comma

; now pull leaves off stack and fix up branch addresses
;        mflr    r3
;        stw     r3,0(r1)        ; save current LR
;        stwu    r4,-4(r1)       ; then push on saved SP (so it's on top)
        b       fixleave


svleave2 stwu    r4,-4(r30)      ; save leave addr
        b       untillp

notbeg  lwar    r4,nbegmsg
        bl      puts
        b       abort

until_

_leave  dd      (leave_-*)
        dd      (leave-*)
        db      "leave"
        db      0,0,0
        dd      XCOMP
leave   lwax    r4,c_colon
        lwa     r3,_bcode
        lwz     r4,0(r4)
        stwu    r3,-4(r30)
        ori     r4,r4,LEAVETYPE ; or in type of address on R stack
        stwu    r4,-4(r29)      ; push on stack
        b       comma           ; comma in b code and exit
leave_


; these two words (ping1/ping2) save a lot of registers-- not really necessary
_ping1  dd      (ping1_-*)
        dd      (ping1-*)
        db      "ping1"
        db      0,0,0
        dd      IMM
ping1   mflr    r6
        li      r3,'*'
        bl      putch
        mtlr    r6
        blr
ping1_

_ping2  dd      (ping2_-*)
        dd      (ping2-*)
        db      "ping2"
        db      0,0,0
        dd      IMM
ping2  	mflr    r6
        li      r3,'!'
        bl      putch
        mtlr    r6
        blr
ping2_

_dots   dd      (dots_-*)
        dd      (dots-*)
        db      ".s"
        db      0,0
        dd      IMM
dots    mflr    r9
        lwax     r5,tdstack
        lwz     r5,0(r5)
        cmplw   0,r5,r30
        blt     0,underfl       ; stack underflow?
        subf    r3,r30,r5       ; find depth
        cmplwi  0,r3,0
        beq     0,isempty       ; must be empty
dslp    add     r4,r3,r30       ; search through stack
        cmplw   0,r4,r30
        beq     0,at_tos
        addi    r1,r1,-20
        stw     r3,0(r1)
        stw     r4,4(r1)
        stw     r5,8(r1)
        stw     r6,12(r1)
        stw     r9,16(r1)
        lwz     r3,-4(r4)
        stwu    r3,-4(r30)
        bl      dot
        li      r3,32
        bl      putch
        bl      putch
        lwz     r9,16(r1)
        lwz     r6,12(r1)
        lwz     r5,8(r1)
        lwz     r4,4(r1)
        lwz     r3,0(r1)
        addi    r3,r3,-4
        addi    r1,r1,20
        b       dslp

at_tos  lwar     r4,tosmsg
dotsx   bl      puts
        mtlr    r9
        blr

isempty lwar     r4,emptymsg
        b       dotsx

underfl lwar    r4,uflmsg
        mr      r30,r5          ; restore stack to empty state
        b       dotsx
dots_


_depth  dd      (depth_-*)
        dd      (depth-*)
        db      "depth"
        db      0,0,0
        dd      IMM
depth   lwax     r4,tdstack
        lwz     r4,0(r4)
        subf    r3,r30,r4       ; find depth
        srawi   r3,r3,2
        stwu    r3,-4(r30)
        blr
depth_


_tick   dd      (tick_-*)
        dd      (tick-*)
        db      "'"
	db	0,0,0
        dd      IMM  
tick    mflr    r3
        stwu    r3,-4(r1)       ; save lr
        mr      r4,r12          ; restore pointer to interpreted string
        bl      chop            ; get next word (name of def)
        add     r12,r4,r5       ; skip past name of def (so parse doesn't see)
	cmplwi	0,r5,0		; zero length name?
        beq     0,t_abort       ; if so, abort!
; isword trashes r6-9
        bl      isword          ; pushes 0/addr of word on stack
        lwz     r3,0(r30)
        cmplwi  0,r3,0          ; if 0, def was not found
        beq     0,t_abort
tickend lwz     r3,0(r1)
        mtlr    r3
        addi    r1,r1,4
        blr

; shared by tick and variable for not found/no name error
t_abort lwar    r4,nonmsg
        bl      puts
        b       abort

tick_

_var    dd      (var_-*)
        dd      (var-*)
        db      "variable"
        dd      0
        dd      (IMM|NOCOMP)
var     mflr    r3
        stwu    r3,-4(r1)       ; save lr
        bl      create          ; calling create updates vars t_colon/c_colon
        lwax    r3,c_colon
        lwz     r4,0(r3)        ; get addr pointed to by c_colon
        lwa     r5,VARIABLE
        stw     r5,-4(r4)       ; store def type in PFA (-4 offset from first
                                ; code entry)
        li      r3,1            ; number of words to allocate
        stwu    r3,-4(r30)      ; push on stack
        bl      allot
        lwz     r5,0(r1)        ; get lr
        addi    r1,r1,4
        mtlr    r5
        blr
var_

_const  dd      (const_-*)
        dd      (const-*)
        db      "constant"
        dd      0
        dd      (IMM|NOCOMP)
const   mflr    r3
        stwu    r3,-4(r1)       ; save lr
        bl      create          ; calling create updates vars t_colon/c_colon
        lwax    r3,c_colon
        lwz     r4,0(r3)        ; get addr pointed to by c_colon
        lwa     r5,CONSTANT
        stw     r5,-4(r4)       ; store def type in PFA (-4 offset from first
                                ; code entry)
        bl      comma           ; write value on stack
        lwz     r5,0(r1)        ; get lr
        addi    r1,r1,4
        mtlr    r5
        blr
const_



; BE CAREFUL-- calling this word without CREATE'ing a word/variable first
; will have dire and unexpected consequences
_allot  dd      (allot_-*)
        dd      (allot-*)
        db      "allot"
        db      0,0,0
        dd      (IMM|NOCOMP)
allot   li      r3,0
        li      r7,2            ; number of times to shift word
        lwax    r5,c_colon      ; get current compile addr
        lwz     r4,0(r5)
        mr      r8,r4           ; save last compile addr
        lwz     r6,0(r30)       ; number of words to allot
        addi    r30,r30,4       ; drop number of words to allot
        cmplwi  0,r6,0          ; (check for 0 word allot call)
        beq     0,noallot
        slw     r6,r6,r7        ; multiply count by 4 to get bytes
        add     r4,r4,r6        ; add in for current compile addr
        stw     r4,0(r5)        ; update compile addr

        lwax    r5,t_colon      ; get top of def (len word)
        lwz     r4,0(r5)        ; now have pointer to top of def
        lwz     r3,0(r4)        ; now have what's in top of def
        add     r3,r3,r6        ; add in for total word len
        stw     r3,0(r4)        ; update

        li      r3,0
        addi    r6,r6,4         ; store an extra 0 to term dictionary
strzlp  stw     r3,0(r8)        ; store 0 in alloc'd space
        addi    r8,r8,4
        addic.  r6,r6,-4
        bne     0,strzlp
        stwu    r3,4(r8)        ; push null (dict term) back too
noallot blr
allot_


; Skip a whole bunch of words if compiling minimal version

	ifndef	LITE
; Copies string in tib (pointed to by r12) into current colon def,
; preceded by a bl opcode to branch around it
; string must be terminated with a quote (")
_string dd      (string_-*)
        dd      (string-*)
        db      "string"
        db      0,0
        dd      XCOMP
string  lwax    r4,c_colon
        addi    r12,r12,1       ; skip space between calling word and string
        lwz     r5,0(r4)
        li      r6,0
        mr      r7,r12
        addi    r5,r5,4         ; make room for bl opcode
str_cplp lbzx    r8,r7,r6        ; get a char from the tib
        cmplwi  0,r8,0
        beq     0,strabt        ; found a null? abort
        cmplwi  0,r8,34         ; found the terminating quote?
        beq     0,eofstr
        stbx    r8,r5,r6        ; otherwise store it in
        addi    r6,r6,1
        b       str_cplp

eofstr  li      r7,0
        add     r12,r12,r6      ; skip past string
        addi    r12,r12,1       ; skip quote mark
straln  stbx    r7,r5,r6        ; store a null in
        addi    r6,r6,1
        andi.   r9,r6,3         ; see if we're 4-aligned yet
        beq     0,eofstr2
        b       straln

eofstr2 lwa     r3,_blcode
        or      r3,r3,r6        ; branch around string
        addi    r3,r3,4
        stw     r3,-4(r5)       ; save opcode right before string
        add     r5,r5,r6        ; move c_colon
        lwa     r3,_mflr_r9     ; mflr  r9
        stw     r3,0(r5)
        lwa     r3,_sub_r9      ; subf  r9,r28,r9
        stw     r3,4(r5)
        lwa     r3,_stwu_r9     ; stwu  r9,-4(r30)
        stw     r3,8(r5)

        addi    r5,r5,12        ; move past opcodes
        stw     r5,0(r4)        ; save it (c_colon)
        blr

strabt  lwar    r4,sqmsg
        bl      puts
        b       abort
string_



_squote dd      (squote_-*)
        dd      (squote-*)
        db      115,34
        db      0,0
        dd      XCOMP
squote  b       string
squote_


_dquote dd      (dquote_-*)
        dd      (dquote-*)
        db      46,34   ; ."
        db      0,0
        dd      XCOMP|IMM
dquote  mflr    r3
        stwu    r3,-4(r1)       ; putch uses r7,r8
        lwax    r4,mode
        lwz     r4,0(r4)
        andi.   r3,r4,1         ; not compiling?
        beq     0,printstr      ; print the string, don't compile it in
        andi.   r3,r4,2         ; deferred complilation?
        bne     0,printstr      ; print the string, don't compile it in
        bl      string
        lwa     r3,_blcode
        lwax    r4,c_colon
        lwz     r5,0(r4)
        lwar    r6,putstr
        subf    r6,r5,r6
        lwa     r7,0x3fffffc    ; make sure disp isn't bigger than 18 bits
        and     r6,r6,r7
        or      r3,r3,r6        ; or in disp
        stw     r3,0(r5)
        addi    r5,r5,4         ; inc c_colon ptr
        stw     r5,0(r4)
eofdq   lwz     r3,0(r1)
        addi    r1,r1,4
        mtlr    r3
        blr

; putch uses r7,r8
printstr lbzu   r3,1(r12)       ; read tib
        cmplwi  0,r3,34         ; found quote?
        beq     0,eofps
        cmpwi   0,r3,0          ; null? (better quit too)
        beq     0,eofdq
        bl      putch           ; otherwise print it
        b       printstr

eofps   addi    r12,r12,1       ; skip quote
        b       eofdq

dquote_


_ccolon dd      (ccolon_-*)
        dd      (ccolon-*)
        db      "c_colon"
        db      0
        dd      XCOMP|IMM
ccolon  lwax    r3,c_colon
        lwz     r3,0(r3)
        stwu    r3,-4(r30)
        blr
ccolon_ 


_tcolon dd      (tcolon_-*)
        dd      (tcolon-*)
        db      "t_colon"
        db      0
        dd      XCOMP|IMM
tcolon  lwax    r3,t_colon
        lwz     r3,0(r3)
        stwu    r3,-4(r30)
        blr
tcolon_ 


_compx  dd      (compx_-*)
        dd      (compx-*)
        db      "["
        db      0,0,0
        dd      XCOMP
compx   lwax    r3,mode
        lwz     r4,0(r3)
        ori     r4,r4,2         ; turn on deferred compile mode
        stw     r4,0(r3)
        blr
compx_

_compe  dd      (compe_-*)
        dd      (compe-*)
        db      "]"
        db      0,0,0
        dd      IMM
compe   lwax    r3,mode
        lwz     r4,0(r3)
        andi.   r4,r4,2
        lwz     r4,0(r3)
        beq     0,notdefrd      ; make sure we're in def'd comp mode
        xori    r4,r4,2         ; toggle it off
        stw     r4,0(r3)
        blr

notdefrd lwar   r4,ntdefrd
        bl      puts
        b       abort
compe_  


; Old-style forget (I really like this better than marker)
; stores a null in the first word of forgotten def, terminating dictionary.
; As with the original forget, all words def'd after word will become
; invisible.
_forget dd      (forget_-*)
        dd      (forget-*)
        db      "forget"
        db      0,0
        dd      IMM|NOCOMP
forget  mflr    r3
        stwu    r3,-4(r1)
        bl      tick            ; get addr
        lwz     r4,0(r30)
        li      r3,0
        addi    r30,r30,4
        cmplwi  0,r4,0
        beq     0,nodef
        stw     r3,0(r4)
	li      r3,0
        li      r4,0
ficache iccci   r0,r4           ; invalidate all i-cache (GC-2K/GCX-16K)
        addi    r3,r3,1
        addi    r4,r4,16
        ifdef   big2
        cmplwi  0,r3,512        ; GC-64; GCX-512
        elseif
        cmplwi  0,r3,64
        endif
        bne     0,ficache

	lwz     r3,0(r1)
        addi    r1,r1,4
	mtlr	r3
        blr

nodef   lwar    r4,nodefmsg
        bl      puts
        b       abort
forget_

	endif

_dump   dd      (dump_-*)
        dd      (dump-*)
        db      "dump"
        dd      0
        dd      IMM

dump    mflr    r3
        stwu    r3,-4(r1)
        lwz     r9,0(r30)       ; dump addr
        addi    r30,r30,4
        lwax    r4,strbuf2
        li      r7,8            ; number of rows
        stwu    r6,-4(r1)
        stwu    r7,-4(r1)
dmplp1  mr      r3,r9           ; get current dump address
        bl      b2hex  
        bl      puts
        li      r3,32
        bl      putch
        bl      putch
        li      r6,0            ; index
        stw     r6,4(r1)
        li      r5,2            ; number of characters to print (xx)
dmplp2  lwz     r6,4(r1)
        lbzx    r3,r9,r6
        bl      b2hexn
        bl      puts                
        li      r3,32
        bl      putch
        bl      putch
        lwz     r6,4(r1)
        addi    r6,r6,1
        cmplwi  0,r6,8
        stw     r6,4(r1)
        bne     0,dmplp2
        li      r3,32
        bl      putch
        bl      putch
        li      r6,0            ; index
dmplp3  lbzx    r3,r9,r6
        cmplwi  0,r3,32
        blt     0,prper         ; just print a period
        cmplwi  0,r3,127
        bgt     0,prper         ; just print a period
ppret   bl      putch
        addi    r6,r6,1
        cmplwi  0,r6,8
        bne     0,dmplp3

        addi    r9,r9,8         ; skip to next 8 bytes
        bl      crlf
        lwz     r7,0(r1)
        addic.  r7,r7,-1
        stw     r7,0(r1)
        bne     0,dmplp1

        lwz     r3,8(r1)
        addi    r1,r1,12
        mtlr    r3
        blr

prper   li      r3,46
        b       ppret

dump_

	ifndef	LITE

_char   dd      (char_-*)
        dd      (char-*)
        db      "char"
        dd      0
        dd      IMM|XCOMP
char    addi    r12,r12,2       ; skip leading space and char in tib
        lbz     r3,-1(r12)      ; get char now
        stwu    r3,-4(r30)      ; stack value
        lwax    r3,mode
        lwz     r3,0(r3)
        andi.   r3,r3,1         ; compiling?
        bne     0,litchar       ; if so, compile in literal
        blr

litchar b       dolit
char_


_argc   dd      (argc_-*)
        dd      (rargc-*)
        db      "argc"
        dd      0
        dd      IMM
rargc   lwa     r3,argc         ; return as relative, not absolute!
        stwu    r3,-4(r30)
        blr
argc_

_argv   dd      (argv_-*)
        dd      (rargv-*)
        db      "argv"
        dd      0
        dd      IMM
rargv   lwa     r3,argv         ; return as relative, not absolute!
        stwu    r3,-4(r30)
        blr
argv_



; CALLC
; Use this word to call C programs, etc.
; Usage:
; argc argv routine_addr callc
; routine_addr is the absolute address of the routinte to jump to
; argc and argv are normal variables that may be stored/fetched
;
; You MUST use return(val); in your C program to have this work-- not exit
; Use exit if you just JUMP to the program, otherwise it will not exit
; properly!

_callc	dd	(callc_-*)
	dd	(callc-*)
	db	"callc"
	db	0,0,0
	dd	IMM
callc	lwz	r3,8(r30)	; argc
	add	r3,r3,r28	; make absolute addr
	lwz	r3,0(r3)	; get actual value for argc
	lwz	r4,4(r30)	; argv
	add	r4,r4,r28	; make absolute
	lwz	r5,0(r30)	; routine address
	addi	r30,r30,8	; bump SP to drop top two 
	lwax	r6,var_i	; used to save pointer to r30
	lwz	r7,0(r6)
	stwu	r7,-4(r30)	; save contents of var_i just in case
	
	mflr	r7		; save CPU state
	mfmsr	r8
	stmw	r0,-128(r30)	; store all regs on forth stack
	stw	r30,0(r6)	; save pointer to forth stack in var_i
	
	mtctr	r5		; now jump to C program address
	bctrl

; retrieve the address of the forth system so we can restore state	
	bl      gbasec
gbasec	mflr    r27
	addi    r27,r27,(start-*+4) ; get base addr to reference all data
	lwa     r28,b_of_4th
	lwax	r6,var_i	; get r30 again...
	lwz	r30,0(r6)
	stw	r3,4(r30)	; save C program's return value
	lmw	r0,-128(r30)	; restore regs
	
	mtlr	r7		; restore CPU state
	mtmsr	r8
	lwz	r5,0(r30)	; pull off value of var_i
	stw	r5,0(r6)	; restore it
	addi	r30,r30,4	; pop off
	blr	
callc_

	endif
	
_cold   dd      (cold_-*)
        dd      (coldst-*)
        db      "cold"
        dd      0
        dd      IMM
coldst  b       cold
cold_

_warm   dd      (warm_-*)
        dd      (warmst-*)
        db      "warm"
        dd      0
        dd      IMM
warmst  b       warm
warm_


	ifndef	LITE

_dict1f dd      (dict1f_-*)
        dd      (dict1f-*)
        db      "dict1@"
        db      0,0
        dd      IMM
dict1f  mr      r3,r31
        stwu    r3,-4(r30)
        blr
dict1f_


; use this one with caution!  Having a missing dictionary is BAD
_dict1s dd      (dict1s_-*)
        dd      (dict1s-*)
        db      "dict1!"
        db      0,0
        dd      IMM
dict1s  lwz     r31,0(r30)
        addi    r30,r30,4
        blr
dict1s_

_dict2f dd      (dict2f_-*)
        dd      (dict2f-*)
        db      "dict2@"
        db      0,0
        dd      IMM
dict2f  lwax    r3,dict2ptr
        lwz     r3,0(r3)
        stwu    r3,-4(r30)
        blr
dict2f_

_dict2s dd      (dict2s_-*)
        dd      (dict2s-*)
        db      "dict2!"
        db      0,0
        dd      IMM
dict2s  lwax    r4,dict2ptr
        lwz     r3,0(r30)
        stw     r3,0(r4)
        addi    r30,r30,4
        blr
dict2s_

; returns an absolute address
_dict2e dd      (dict2e_-*)
        dd      (dict2e-*)
        db      "dict2_eof"
        db      0,0,0
        dd      IMM
dict2e  lwax    r4,dict2ptr
        lwz     r4,0(r4)
d2lp    lwz     r3,0(r4)
        cmplwi  0,r3,0
        beq     0,fndd2e
        add     r4,r4,r3        ; skip to next entry
        b       d2lp
fndd2e  stwu    r4,-4(r30)
        blr
dict2e_


	endif

; returns an absolute address
_borom  dd      (borom_-*)
        dd      (borom-*)
        db      "b_of_rom"
        dd      0
        dd      IMM
borom   stwu    r27,-4(r30)
        blr
borom_

; returns an absolute address
_boram  dd      (boram_-*)
        dd      (boram-*)
        db      "b_of_ram"
        dd      0
        dd      IMM
boram   stwu    r28,-4(r30)
        blr
boram_


	ifndef	LITE

_ftblo  dd      (ftblo_-*)
        dd      (ftblo-*)
        db      "tblo@"
        db      0,0,0
        dd      IMM
ftblo   mfspr   r3,tblo
        stwu    r3,-4(r30)
        blr
ftblo_

_ftbhi  dd      (ftbhi_-*)
        dd      (ftbhi-*)
        db      "tbhi@"
        db      0,0,0
        dd      IMM
ftbhi   mfspr   r3,tbhi
        stwu    r3,-4(r30)
        blr
ftbhi_


_stblo  dd      (stblo_-*)
        dd      (stblo-*)
        db      "tblo!"
        db      0,0,0
        dd      IMM
stblo   lwz     r3,0(r30)
        addi    r30,r30,4
	mtspr   tblo,r3
        blr
stblo_

_stbhi  dd      (stbhi_-*)
        dd      (stbhi-*)
        db      "tbhi!"
        db      0,0,0
        dd      IMM
stbhi  	lwz	r3,0(r30)
	addi	r30,r30,4
	mtspr   tbhi,r3
        blr
stbhi_



_fpit   dd      (fpit_-*)
        dd      (fpit-*)
        db      "pit@"
        dd      0
        dd      IMM
fpit    mfspr   r3,pit
        stwu    r3,-4(r30)
        blr
fpit_

_spit   dd      (spit_-*)
        dd      (spit-*)
        db      "pit!"
        dd      0
        dd      IMM
spit    lwz     r3,0(r30)
        addi    r30,r30,4
        mtspr   pit,r3
        blr
spit_


_ftcr   dd      (ftcr_-*)
        dd      (ftcr-*)
        db      "tcr@"
        dd      0
        dd      IMM
ftcr    mfspr   r3,tcr
        stwu    r3,-4(r30)
        blr
ftcr_

_stcr   dd      (stcr_-*)
        dd      (stcr-*)
        db      "tcr!"
        dd      0
        dd      IMM
stcr    lwz     r3,0(r30)
        addi    r30,r30,4
        mtspr   tcr,r3
        blr
stcr_


_ftsr   dd      (ftsr_-*)
        dd      (ftsr-*)
        db      "tsr@"
        dd      0
        dd      IMM
ftsr    mfspr   r3,tsr
        stwu    r3,-4(r30)
        blr
ftsr_


_stsr   dd      (stsr_-*)
        dd      (stsr-*)
        db      "tsr!"
        dd      0
        dd      IMM
stsr    lwz     r3,0(r30)
        addi    r30,r30,4
        mtspr   tsr,r3
        blr
stsr_

_fmsr   dd      (fmsr_-*)
        dd      (fmsr-*)
        db      "msr@"
        dd      0
        dd      IMM
fmsr    mfmsr   r3
        stwu    r3,-4(r30)
        blr
fmsr_


_smsr   dd      (smsr_-*)
        dd      (smsr-*)
        db      "msr!"
        dd      0
        dd      IMM
smsr    lwz     r3,0(r30)
        addi    r30,r30,4
        mtmsr   r3
        blr
smsr_

_fiocr  dd      (fiocr_-*)
        dd      (fiocr-*)
        db      "iocr@"
        db      0,0,0
        dd      IMM
fiocr   mfdcr   r3,iocr
        stwu    r3,-4(r30)
        blr
fiocr_


_siocr  dd      (siocr_-*)
        dd      (siocr-*)
        db      "iocr!"
        db      0,0,0
        dd      IMM
siocr   lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   iocr,r3
        blr
siocr_

_fexier dd      (fexier_-*)
        dd      (fexier-*)
        db      "exier@"
        db      0,0
        dd      IMM
fexier  mfdcr   r3,exier
        stwu    r3,-4(r30)
        blr
fexier_


_sexier dd      (sexier_-*)
        dd      (sexier-*)
        db      "exier!"
        db      0,0
        dd      IMM
sexier  lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   exier,r3
        blr
sexier_


_fexisr dd      (fexisr_-*)
        dd      (fexisr-*)
        db      "exisr@"
        db      0,0
        dd      IMM
fexisr  mfdcr   r3,exisr
        stwu    r3,-4(r30)
        blr
fexisr_


_sexisr dd      (sexisr_-*)
        dd      (sexisr-*)
        db      "exisr!"
        db      0,0
        dd      IMM
sexisr  lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   exisr,r3
        blr
sexisr_

_ssrr0  dd      (ssrr0_-*)
        dd      (ssrr0-*)
        db      "srr0!"
        db      0,0,0
        dd      IMM
ssrr0   lwz     r3,0(r30)
        addi    r30,r30,4
        mtspr   srr0,r3
        blr
ssrr0_

_fsrr0  dd      (fsrr0_-*)
        dd      (fsrr0-*)
        db      "srr0@"
        db      0,0,0
        dd      IMM
fsrr0   mfspr   r3,srr0
        stwu    r3,-4(r30)
        blr
fsrr0_


_ssrr1  dd      (ssrr1_-*)
        dd      (ssrr1-*)
        db      "srr1!"
        db      0,0,0
        dd      IMM
ssrr1   lwz     r3,0(r30)
        addi    r30,r30,4
        mtspr   srr1,r3
        blr
ssrr1_


_fsrr1  dd      (fsrr1_-*)
        dd      (fsrr1-*)
        db      "srr1@"
        db      0,0,0
        dd      IMM
fsrr1   mfspr   r3,srr1
        stwu    r3,-4(r30)
        blr
fsrr1_

_sbr0   dd      (sbr0_-*)
        dd      (sbr0-*)
        db      "br0!"
        dd      0
        dd      IMM
sbr0    lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   br0,r3
        blr
sbr0_


_fbr0   dd      (fbr0_-*)
        dd      (fbr0-*)
        db      "br0@"
        dd      0
        dd      IMM
fbr0    mfdcr   r3,br0
        stwu    r3,-4(r30)
        blr
fbr0_

_sbr1   dd      (sbr1_-*)
        dd      (sbr1-*)
        db      "br1!"
        dd      0
        dd      IMM
sbr1    lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   br1,r3
        blr
sbr1_


_fbr1   dd      (fbr1_-*)
        dd      (fbr1-*)
        db      "br1@"
        dd      0
        dd      IMM
fbr1    mfdcr   r3,br1
        stwu    r3,-4(r30)
        blr
fbr1_

_sbr2   dd      (sbr2_-*)
        dd      (sbr2-*)
        db      "br2!"
        dd      0
        dd      IMM
sbr2    lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   br2,r3
        blr
sbr2_


_fbr2   dd      (fbr2_-*)
        dd      (fbr2-*)
        db      "br2@"
        dd      0
        dd      IMM
fbr2    mfdcr   r3,br2
        stwu    r3,-4(r30)
        blr
fbr2_

_sbr3   dd      (sbr3_-*)
        dd      (sbr3-*)
        db      "br3!"
        dd      0
        dd      IMM
sbr3    lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   br3,r3
        blr
sbr3_


_fbr3   dd      (fbr3_-*)
        dd      (fbr3-*)
        db      "br3@"
        dd      0
        dd      IMM
fbr3    mfdcr   r3,br3
        stwu    r3,-4(r30)
        blr
fbr3_

_sbr4   dd      (sbr4_-*)
        dd      (sbr4-*)
        db      "br4!"
        dd      0
        dd      IMM
sbr4    lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   br4,r3
        blr
sbr4_


_fbr4   dd      (fbr4_-*)
        dd      (fbr4-*)
        db      "br4@"
        dd      0
        dd      IMM
fbr4    mfdcr   r3,br4
        stwu    r3,-4(r30)
        blr
fbr4_


_sbr5   dd      (sbr5_-*)
        dd      (sbr5-*)
        db      "br5!"
        dd      0
        dd      IMM
sbr5    lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   br5,r3
        blr
sbr5_


_fbr5   dd      (fbr5_-*)
        dd      (fbr5-*)
        db      "br5@"
        dd      0
        dd      IMM
fbr5    mfdcr   r3,br5
        stwu    r3,-4(r30)
        blr
fbr5_


_sbr6   dd      (sbr6_-*)
        dd      (sbr6-*)
        db      "br6!"
        dd      0
        dd      IMM
sbr6    lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   br6,r3
        blr
sbr6_


_fbr6   dd      (fbr6_-*)
        dd      (fbr6-*)
        db      "br6@"
        dd      0
        dd      IMM
fbr6    mfdcr   r3,br6
        stwu    r3,-4(r30)
        blr
fbr6_


_sbr7   dd      (sbr7_-*)
        dd      (sbr7-*)
        db      "br7!"
        dd      0
        dd      IMM
sbr7    lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   br7,r3
        blr
sbr7_


_fbr7   dd      (fbr7_-*)
        dd      (fbr7-*)
        db      "br7@"
        dd      0
        dd      IMM
fbr7    mfdcr   r3,br7
        stwu    r3,-4(r30)
        blr
fbr7_


_sesr   dd      (sesr_-*)
        dd      (sesr-*)
        db      "esr!"
        dd      0
        dd      IMM
sesr    lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   esr,r3
        blr
sesr_


_fesr   dd      (fesr_-*)
        dd      (fesr-*)
        db      "esr@"
        dd      0
        dd      IMM
fesr    mfdcr   r3,esr
        stwu    r3,-4(r30)
        blr
fesr_


_sbear  dd      (sbear_-*)
        dd      (sbear-*)
        db      "bear!"
        db      0,0,0
        dd      IMM
sbear   lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   bear,r3
        blr
sbear_


_fbear  dd      (fbear_-*)
        dd      (fbear-*)
        db      "bear@"
        db      0,0,0
        dd      IMM
fbear   mfdcr   r3,bear
        stwu    r3,-4(r30)
        blr
fbear_

_sdccr  dd      (sdccr_-*)
        dd      (sdccr-*)
        db      "dccr!"
        db      0,0,0
        dd      IMM
sdccr   lwz     r3,0(r30)
        addi    r30,r30,4
        mtspr   dccr,r3
        blr
sdccr_


_fdccr  dd      (fdccr_-*)
        dd      (fdccr-*)
        db      "dccr@"
        db      0,0,0
        dd      IMM
fdccr   mfspr   r3,dccr
        stwu    r3,-4(r30)
        blr
fdccr_


_siccr  dd      (siccr_-*)
        dd      (siccr-*)
        db      "iccr!"
        db      0,0,0
        dd      IMM
siccr   lwz     r3,0(r30)
        addi    r30,r30,4
        mtspr   iccr,r3
        blr
siccr_


_ficcr  dd      (ficcr_-*)
        dd      (ficcr-*)
        db      "iccr@"
        db      0,0,0
        dd      IMM
ficcr   mfspr   r3,iccr
        stwu    r3,-4(r30)
        blr
ficcr_

_sbesr  dd      (sbesr_-*)
        dd      (sbesr-*)
        db      "besr!"
        db      0,0,0
        dd      IMM
sbesr   lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   besr,r3
        blr
sbesr_


_fbesr  dd      (fbesr_-*)
        dd      (fbesr-*)
        db      "besr@"
        db      0,0,0
        dd      IMM
fbesr   mfdcr   r3,besr
        stwu    r3,-4(r30)
        blr
fbesr_



_sdmacc0 dd     (sdmacc0_-*)
        dd      (sdmacc0-*)
        db      "dmacc0!"
        db      0
        dd      IMM
sdmacc0 lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   dmacc0,r3
        blr
sdmacc0_


_fdmacc0 dd     (fdmacc0_-*)
        dd      (fdmacc0-*)
        db      "dmacc0@"
        db      0
        dd      IMM
fdmacc0 mfdcr   r3,dmacc0
        stwu    r3,-4(r30)
        blr
fdmacc0_




_sdmacr0 dd      (sdmacr0_-*)
        dd      (sdmacr0-*)
        db      "dmacr0!"
        db      0
        dd      IMM
sdmacr0 lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   dmacr0,r3
        blr
sdmacr0_


_fdmacr0 dd     (fdmacr0_-*)
        dd      (fdmacr0-*)
        db      "dmacr0@"
        db      0
        dd      IMM
fdmacr0 mfdcr   r3,dmacr0
        stwu    r3,-4(r30)
        blr
fdmacr0_

_sdmact0 dd     (sdmact0_-*)
        dd      (sdmact0-*)
        db      "dmact0!"
        db      0
        dd      IMM
sdmact0 lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   dmact0,r3
        blr
sdmact0_


_fdmact0 dd     (fdmact0_-*)
        dd      (fdmact0-*)
        db      "dmact0@"
        db      0
        dd      IMM
fdmact0 mfdcr   r3,dmact0
        stwu    r3,-4(r30)
        blr
fdmact0_




_sdmada0 dd     (sdmada0_-*)
        dd      (sdmada0-*)
        db      "dmada0!"
        db      0
        dd      IMM
sdmada0 lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   dmada0,r3
        blr
sdmada0_


_fdmada0 dd     (fdmada0_-*)
        dd      (fdmada0-*)
        db      "dmada0@"
        db      0
        dd      IMM
fdmada0 mfdcr   r3,dmada0
        stwu    r3,-4(r30)
        blr
fdmada0_




_sdmasa0 dd     (sdmasa0_-*)
        dd      (sdmasa0-*)
        db      "dmasa0!"
        db      0
        dd      IMM
sdmasa0 lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   dmasa0,r3
        blr
sdmasa0_


_fdmasa0 dd     (fdmasa0_-*)
        dd      (fdmasa0-*)
        db      "dmasa0@"
        db      0
        dd      IMM
fdmasa0 mfdcr   r3,dmasa0
        stwu    r3,-4(r30)
        blr
fdmasa0_




_sdmasr dd      (sdmasr_-*)
        dd      (sdmasr-*)
        db      "dmasr!"
        db      0,0
        dd      IMM
sdmasr  lwz     r3,0(r30)
        addi    r30,r30,4
        mtdcr   dmasr,r3
        blr
sdmasr_


_fdmasr dd      (fdmasr_-*)
        dd      (fdmasr-*)
        db      "dmasr@"
        db      0,0
        dd      IMM
fdmasr  mfdcr   r3,dmasr
        stwu    r3,-4(r30)
        blr
fdmasr_



; MMU-specific words
; EPN- effective page number-- 22-8 bits, depending on page size.
; Used to compare for a TLB match.  Unused bits must be zero!
; SIZE- 3 bit field specifying page size, starting at 1K and going in 
; multiples of 4 (1K=000, 4K, 16K, 64K, 256K, 1M, 4M, 8M, 16M=111)
; V- bit flag to determine if TLB entry is valid
; TID- translation ID-- compared to current value of PID determine 
; whether the TLB entry is a match (in addition to the address). 
; Setting this to 0 matches all TLB entries with a valid address.
; RPN- real page number-- 22-8 bits, depending on page size.
; Used to replace the effective page address bits with the translated
; real (physical) address.  Unused bits must be zero!
; ZSEL- zone select-- 4 bits.  Used to select a group of TLB entries
; with the same zone number.
; EX- bit flag set to allow execution in a page.
; WR- bit flag set to allow writes to a page.
; W- writethrough bit (do not use-- unimplemented).
; I- caching inhibited bit-- bypass cache when reading this page.
; M- memory coherent bit(not implemented)
; G- guarded bit-- do not prefetch or speculatively execute in this
; page.  Attempted execution out of a page marked with this will 
; cause an instruction storage exception.
        ifdef	p403gcx
; (epn[0:21]+size[22:24]+v[25]) tid tlb_entry
_tlbwehi dd  	(tlbwehi_-*)
        dd      (tlb_wehi-*)
        db      "tlbwehi"
        db      0
        dd      IMM
tlb_wehi lwz	r4,0(r30)
	lwz	r3,4(r30)
	lwz	r5,8(r30)
	addi	r30,r30,12
	mfspr	r6,pid
	mtspr	pid,r3 
	tlbwehi	r5,r4
	mtspr	pid,r6
        blr
tlbwehi_
        endif

        ifdef	p403gcx
; (rpn[0:21]+ex[22]+wr[23]+zsel[24:27]+wimg[28:31]) tlb_entry
_tlbwelo dd  	(tlbwelo_-*)
        dd      (tlb_welo-*)
        db      "tlbwelo"
        db      0
        dd      IMM
tlb_welo lwz	r3,0(r30)
	lwz	r4,4(r30)
	addi	r30,r30,8
	tlbwelo	r4,r3
        blr
tlbwelo_
        endif

        ifdef	p403gcx
; tlb_entry - tlbhi pid
_tlbrehi dd  	(tlbrehi_-*)
        dd      (tlb_rehi-*)
        db      "tlbrehi"
        db      0
        dd      IMM
tlb_rehi lwz	r3,0(r30)
	mfspr	r6,pid
	tlbrehi	r4,r3
	mfspr	r5,pid
	mtspr	pid,r6	; restore pid 
	stwu	r5,-4(r30)
	stw	r4,4(r30)
        blr
tlbrehi_
        endif

        ifdef	p403gcx
; tlb_entry - tlblo
_tlbrelo dd  	(tlbrelo_-*)
        dd      (tlb_relo-*)
        db      "tlbrelo"
        db      0
        dd      IMM
tlb_relo lwz	r3,0(r30)
	tlbrelo	r4,r3
	stw	r4,0(r30)
 	blr
tlbrelo_
        endif

        ifdef	p403gcx
_fpid 	dd  	(fpid_-*)
        dd      (fpid-*)
        db      "pid@"
        dd	0
        dd      IMM
fpid	mfspr	r3,pid
	stwu	r3,-4(r30)
	blr
fpid_

_spid 	dd  	(spid_-*)
        dd      (spid-*)
        db      "pid!"
        dd	0
        dd      IMM
spid	lwz	r3,0(r30)
	mtspr	pid,r3
	addi	r30,r30,4
	blr
spid_
        endif

        ifdef	p403gcx
; (effective address)  pid -- returns TLB number (0-63) for match, 
; -1 for no match
_tlbsx	dd	(tlbsx_-*)
	dd	(tlb_sx-*)
	db	"tlbsx"
	db	0,0,0
	dd	IMM
tlb_sx	lwz	r3,0(r30)
	lwz	r4,4(r30)
	addi	r30,r30,4
	mfspr	r5,pid
	mtspr	pid,r3
	tlbsx.	r3,r0,r4
	mtspr	pid,r5
	beq	0,tlbhit
	li	r3,-1
tlbhit	stw	r3,0(r30)
	blr
tlbsx_
        endif


_getpc	dd      (getpc_-*)
        dd      (getpc-*)
        db      "getpc"
        db      0,0,0
        dd      IMM
getpc	mflr	r3
	stwu	r3,-4(r30)
	blr
getpc_

_ccache	dd      (ccache_-*)
        dd      (ccache-*)
        db      "clearcache"
        db      0,0
        dd      IMM
ccache	li	r3,-1
	mtspr   sgr,r3          ; sgr=all guarded
        li      r3,0            ; no d-cacheable
        mtspr   dccr,r3
        mtspr   iccr,r3         ; update iccr-- no inst cacheable
ivcache3 iccci   r0,r4           ; invalidate all i-cache (GC-2K/GCX-16K)
        addi    r3,r3,1
        addi    r4,r4,16
	ifdef	big2
        cmplwi  0,r3,512        ; GC-64; GCX-512
	elseif	
	cmplwi  0,r3,64
	endif
        bne     0,ivcache3

        li      r3,0
        li      r4,0
dvcache3 dccci   r0,r4           ; invalidate all d-cache
        addi    r3,r3,1
        addi    r4,r4,16
	ifdef	big2
        cmplwi  0,r3,256        ; GC-32; GCX-256
	elseif
	cmplwi  0,r3,32
	endif
        bne     0,dvcache3
	li	r3,0
	mtspr   sgr,r3          ; update sgr
        li      r3,2            ; f0000000-f7ffffff d-cacheable
        mtspr   dccr,r3
        li      r3,-1           ; all inst cacheable
        mtspr   iccr,r3         ; update iccr (all inst cacheable)
	blr
ccache_

        ifdef	p403gcx
_tlbia	dd      (tlbia_-*)
        dd      (tlb_ia-*)
        db      "tlbia"
        db      0,0,0
        dd      IMM
tlb_ia	tlbia
	blr
tlbia_
        endif

_isync	dd      (isync_-*)
        dd      (i_sync-*)
        db      "isync"
        db      0,0,0
        dd      IMM
i_sync	isync
	blr
isync_

_sleepv dd      (sleepv_-*)
        dd      (sleepv-*)
        db      "dosleep"
        db      0
        dd      IMM
sleepv	lwax	r4,dosleep
        stwu    r4,-4(r30)
        blr
sleepv_

_sc 	dd      (sc_-*)
        dd      (dosc-*)
        db      "sc"
        dw	0
        dd      IMM
dosc	lwz	r3,0(r30)
	sc
	stw	r3,0(r30)
        blr
sc_




_svreg  dd      (svreg_-*)
        dd      (svreg-*)
        db      "int_entry"
        db      0,0,0
        dd      INLINE
svreg   addi    r29,r29,-32
	stw	r3,0(r29)
	stw	r4,4(r29)
	stw     r5,8(r29)
        stw     r6,12(r29)
	stw     r7,16(r29)
        stw     r8,20(r29)
	stw     r9,24(r29)
        mfcr    r9
        stw     r9,28(r29)
	mflr    r9
        stwu    r9,-4(r1)	; put lr on system stack
svreg_

_rsrreg dd      (rsreg_-*)
        dd      (rsreg-*)
        db      "int_exit"
        dd      0
        dd      INLINE
rsreg   lwz     r9,0(r1)
        mtlr    r9
	addi	r1,r1,4
        lwz     r3,28(r29)
;        mtcrf 	0xff,r3
        dd      0x7c6ff120
        lwz     r3,0(r29)
        lwz     r4,4(r29)
        lwz     r5,8(r29)
        lwz     r6,12(r29)
        lwz     r7,16(r29)
        lwz     r8,20(r29)
        lwz     r9,24(r29)
        addi    r29,r29,32
        rfi
rsreg_

_spitv  dd      (spitv_-*)
        dd      (spitv-*)
        db      "pit_vec!"
        dd      0
        dd      IMM
spitv	lwz	r9,0(r30)
	addi	r30,r30,4
        lwz     r8,4(r9)        ; get len of string/pfa
        add     r9,r9,r8
        addi    r9,r9,12        ; skip first word (len), and save lr/stack
	lwa	r4,evector	; get absolute vector table address
	lwar	r5,svtbl	; get table address

	lwz	r3,0(r5)	
	stw	r3,0x1000(r4)	; save to vector entry 
	
	lwz     r3,4(r5)
        stw     r3,0x1004(r4)   ; save to vector entry

        lwz     r3,8(r5)
        stw     r3,0x1008(r4)   ; save to vector entry

        lwz     r3,12(r5)        ; high 16 bits of addr
	srawi	r8,r9,16	; drop high 16 bits
	or	r3,r3,r8
        stw     r3,0x100c(r4)

        lwz     r3,16(r5)       ; low 16 bits of addr
	andi.	r8,r9,0xffff
	or	r3,r3,r8
        stw     r3,0x1010(r4)

        lwz     r3,20(r5)
        stw     r3,0x1014(r4)   ; save to vector entry

        lwz     r3,24(r5)
        stw     r3,0x1018(r4)   ; save to vector entry

        lwz     r3,28(r5)
        stw     r3,0x101c(r4)   ; save to vector entry
	blr

svtbl   stwu    r10,-4(r1)      ; save r10
        mflr    r10
        stwu    r10,-4(r1)	; save lr
        addis   r10,r0,0        ; needs fix up by routine above for real
        ori     r10,r10,0	; address of service routine
        mtlr    r10		
        blr
spitv_

_sevec	dd      (sevec_-*)
        dd      (sevec-*)
        db      "extvec!"
        db      0
        dd      IMM
sevec	lwz	r3,0(r30)
	lwax	r4,extvect
	stw	r3,0(r4)	; store in secondary external vector location
	addi	r30,r30,4
	blr
sevec_


_fevec	dd      (fevec_-*)
        dd      (fevec-*)
        db      "extvec@"
        db      0
        dd      IMM
fevec	lwax	r3,extvect	; store in secondary external vector location
	lwz	r3,0(r3)
	stwu	r3,-4(r30)
	blr
fevec_

; Install exception vector handler
; offset address ivec!
; pass address of handler in r4, offset in r7
_ivec   dd      (ivec_-*)
        dd      (ivec-*)
        db      "ivec!"
        db      0,0,0
        dd      IMM
ivec	lwz	r4,0(r30)
	lwz	r3,4(r4)	; get len of header
	add	r4,r4,r3
	addi	r4,r4,12	; skip first 4 bytes, save lr/reg instructions
	lwz	r7,4(r30)	; vector number
	stw	r4,4(r30)
	mflr	r8
	bl	instvect
	mtlr	r8
	addi	r30,r30,4
	blr
ivec_

	endif	    

; This controls an LED I have attached my board
_sled   dd      (sled_-*)
        dd      (sled-*)
        db      "led!"
        dd      0
        dd      IMM
sled    lwa     r4,ledaddr
        lwz     r3,0(r30)
        addi    r30,r30,4
        stb     r3,0(r4)
        blr
sled_

; ( len src dest memcpy )
_memcpy dd      (memcpy_-*)
	dd	(memcpy-*)
        db      "memcpy"
        db      0,0
        dd      IMM
memcpy  lwz     r4,0(r30)
        lwz     r6,4(r30)
        lwz     r5,8(r30)
        addi    r30,r30,12
        li      r7,0
memcplp cmplw   0,r7,r5
        beq     0,mcpend
        lbzx    r8,r4,r7
        stbx    r8,r6,r7
        addi    r7,r7,1
        b       memcplp
mcpend  blr
memcpy_

_sbase 	dd      (sbase_-*)
        dd      (sbase-*)
        db      "srbase"
        db      0,0
	dd      IMM 
sbase  	lwax    r4,srbase
        stwu	r4,-4(r30)
        blr
sbase_



_sflash dd      (sflash_-*)
        dd      (sflash-*)
        db      "flashc!"
        db      0
        dd      IMM
sflash 	lwa     r4,rombase      ; this should be the base of the flash 
        lwz     r6,0(r30)       ; addr to store to (RELATIVE!)
        lwz     r3,4(r30)       ; byte to store
        addi    r30,r30,8
        li      r5,0xaa
        stb     r5,0x5555(r4)
        li      r5,0x55
        stb     r5,0x2aaa(r4)
        li      r5,0xa0
        stb     r5,0x5555(r4)
        stbx    r3,r4,r6
        mfspr   r7,dccr         ; save caching state
        li      r5,0
        mtspr   dccr,r5         ; turn off data caching (so reads from flash
                                ; are NOT cached)
; bits 7- inverted while writing/error  6- toggles while writing/error
;      5- 0 if writing/1 if error  3- 0 if writing/error
flchk   lbzx    r5,r4,r6
        cmplw   0,r5,r3         ; see if data is equal
        beq     0,sfend
        andi.   r5,r5,0x20      ; see if d5==1 (error)
        bne     0,fpgmerr
        b       flchk
        blr

sfend   li      r3,0
        stwu    r3,-4(r30)      ; all ok if==0
	mtspr   dccr,r7
        blr

fpgmerr	mtspr   dccr,r7 
	stwu    r6,-4(r30)      ; return flash erase address
        blr
sflash_

; ( len src dest<-RELATIVE!!! flcpy )
_flcpy 	dd      (flcpy_-*)
        dd      (flcpy-*)
        db      "flcpy"
        db      0,0,0
        dd      IMM
flcpy  	mflr	r3
	stwu	r3,-4(r1)
	lwz     r4,0(r30)
        lwz     r6,4(r30)
        lwz     r5,8(r30)
        addi    r30,r30,12
        li      r7,0
flcplp 	cmplw   0,r7,r5		; copy done?
        beq     0,fcpok
        lbzx    r8,r6,r7
	stwu	r8,-4(r30)
	add	r8,r4,r7	; add rel address and offset
	stwu	r8,-4(r30)
	addi	r1,r1,-16
	stw	r4,0(r1)
	stw	r5,4(r1)
	stw	r6,8(r1)
	stw	r7,12(r1)
	bl	sflash	
	lwz	r7,12(r1)
	lwz	r6,8(r1)
	lwz	r5,4(r1)
	lwz	r4,0(r1)
	addi	r1,r1,16
	lwz	r3,0(r30)
	addi	r30,r30,4
	cmplwi	0,r3,0
	bne	0,fcpend	; error in store?	
        addi    r7,r7,1
        b       flcplp
fcpok	li	r3,0
fcpend 	stwu	r3,-4(r30)
	lwz	r3,0(r1)
	addi	r1,r1,4
	mtlr	r3
	blr
flcpy_



_flsecte dd     (flsecte_-*)
        dd      (flsecte-*)
        db      "flash_sector_erase"
        db      0,0
        dd      IMM
flsecte lwa     r4,rombase		; this should be the base of the flash
        lwz     r6,0(r30)       ; sector to erase (must be 64K aligned, rel)
        addi    r30,r30,4
        li      r5,0xaa
        stb     r5,0x5555(r4)
        li      r5,0x55
        stb     r5,0x2aaa(r4)
        li      r5,0x80
        stb     r5,0x5555(r4)
        li      r5,0xaa
        stb     r5,0x5555(r4)
        li      r5,0x55
        stb     r5,0x2aaa(r4)
        li      r5,0x30
        stbx    r5,r4,r6
		mfspr   r7,dccr         ; save caching state
        li      r5,0
        mtspr   dccr,r5         ; turn off data caching (so reads from flash
                                ; are NOT cached)
; bits 7- inverted while writing/error  6- toggles while writing/error
;      5- 0 if writing/1 if error  3- 1 if writing/error
	li	r3,0xff		; all 1's in byte (erased)
flchk2  lbzx    r5,r4,r6
        cmplw   0,r5,r3         ; see if data is equal
        beq     0,efend
        andi.   r8,r5,0x20      ; see if d5==1 (error)
        bne     0,feraerr
        b       flchk2

efend   li      r3,0
        stwu    r3,-4(r30)      ; all ok if==0
	mtspr   dccr,r7
        blr

feraerr stwu    r6,-4(r30)      ; return flash address that failed 
	mtspr   dccr,r7
        blr
flsecte_


_reflash dd     (reflash_-*)
        dd      (reflash-*)
        db      "reflash"
        db      0
        dd      IMM     
reflash	prstr	"Preparing to rewrite flash with RAM copy of PPCForth:\n\r"
	lwa	r3,rombase
	cmplw	0,r3,r27
	bgt	0,rft2		; make sure we're in RAM somewhere, not flash!
inflash	prstr	"Can't execute while running from FLASH!\n\r"
	blr
rft2	lwa	r4,romsize	; make sure we're not beyond what's defined as ROM/FLASH
	add	r3,r3,r4
	cmplw	0,r3,r27
	blt	inflash
	li	r3,0
	stwu	r3,-4(r30)
	mflr	r3
	stwu	r3,-4(r1)
	bl	flsecte
	lwz	r3,0(r30)
	addi	r30,r30,4
	cmplwi	0,r3,0
	bne	0,fle_err
	prstr	"Flash erased...  Now burning a copy of PPCForth:\r\n"
	lwa	r3,0x10000
	stwu	r3,-4(r30)
	stwu	r27,-4(r30)
	li	r3,0
	stwu	r3,-4(r30)
	bl	flcpy
	lwz	r3,0(r30)
	addi	r30,r30,4
	cmplwi	0,r3,0
	bne	0,flb_err
	prstr	"Burn successful!\n\r"
eof_rf	lwz	r3,0(r1)
	addi	r1,r1,4
	mtlr	r3
	blr

fle_err	prstr	"Error erasing flash!!!  You MUST fix this problem or PPCForth will not boot!!!!!!\r\n"
	b	eof_rf

flb_err	prstr	"Error burning flash!!!  You MUST fix this problem or PPCForth will not boot!!!!!!\r\n"
	b	eof_rf
reflash_
	

; Syntax is: len (in bytes) source addr (abs) dest addr (rel/128 aligned) 
_atfwr	dd     	(atfwr_-*)
        dd      (atfwr-*)
        db      "atmel_fwrite"
        dd      0
        dd      IMM
atfwr	lwa     r4,rombase	; this should be the base of the flash
        lwz     r6,0(r30)       ; sector to rewrite (must be 128 aligned, rel)
	add	r6,r6,r4	; make absolute
        lwz	r7,4(r30)	; get src addr (absolute address)
	lwz	r8,8(r30)	; get len in bytes
	srawi	r8,r8,7		; divide count by 128
	cmplwi	0,r8,0		; less than 128 bytes? -- abort
	beq	0,wfend
	mfspr   r0,dccr         ; save caching state
        li      r5,0
        mtspr   dccr,r5         ; turn off data caching (so reads from flash
                                ; are NOT cached)
		
flwrlp 	lwa	r4,rombase
	li      r5,0xaa		; write sector in protect mode
        stb     r5,0x5555(r4)
        li      r5,0x55
        stb     r5,0x2aaa(r4)
        li      r5,0xa0
        stb     r5,0x5555(r4)	
		
 	li	r10,0		; index for src/dest
; bits 7- inverted while writing/error  6- toggles while writing/error
;      5- 0 if writing/1 if error  3- 1 if writing/error
		
fwsec	lbzx	r3,r7,r10	; get src data
	stbx	r3,r6,r10	; write it
	addi	r10,r10,1
	cmplwi	0,r10,128	; wrote a sector yet?
	bne	fwsec
	
	li	r10,0
	lwa	r9,0xfffff	; timeout value	
dofchk	addi	r10,r10,1
	cmplw	0,r9,r10	; timed out?
	beq	fwrerr
	lbz	r3,0(r6)	; check for busy 
	lbz	r4,0(r7)	
	cmplw	0,r3,r4
	bne	0,dofchk
	lbz	r3,0(r6)	; second read to check for toggling
	cmplw	0,r3,r4		; make sure it's still equal	
	bne	0,dofchk
	
		
	addi	r6,r6,128	; bump src,dest addrs up a sector's worth
	addi	r7,r7,128
	addic.	r8,r8,-1
	bne	0,flwrlp	
		
		
wfend   li      r3,0
	addi	r30,r30,8	; drop two args, rewrite third
        stw     r3,0(r30)      	; all ok if==0
	mtspr   dccr,r0
        blr

fwrerr 	addi	r30,r30,8	; drop two args, rewrite third
;	lwa	r4,rombase
;	subf	r6,r6,r4	; make rel again
	stw     r6,0(r30)      	; return flash sector address that failed 
	mtspr   dccr,r0
        blr
		
atfwr_

_atunp	dd	(atunp_-*)
	dd	(atunp-*)
	db	"at_unprotect"
	dd	0
	dd	IMM
atunp	lwa	r4,rombase
	lwax	r5,strbuf2	; get temp area
	li	r3,0
cromlp	lwzx	r6,r4,r3	; get ROM data
	stwx	r6,r5,r3	; save it	
	addi	r3,r3,4
	cmplwi	0,r3,128	; done 128 bytes?
	bne	cromlp

	li	r3,0xaa
	stb	r3,0x5555(r4)
	li	r3,0x55
	stb	r3,0x2aaa(r4)
	li	r3,0x80
	stb	r3,0x5555(r4)
	li	r3,0xaa
	stb	r3,0x5555(r4)
	li	r3,0x55
	stb	r3,0x2aaa(r4)
	li	r3,0x20
	stb	r3,0x5555(r4)
       	li      r3,0
cromlp2 lwzx    r6,r5,r3        ; get RAM data
        stwx    r6,r4,r3        ; save it
        addi    r3,r3,4
        cmplwi  0,r3,128        ; done 128 bytes?
        bne     cromlp2
	blr
atunp_
	


_fast   dd      (fast_-*)
        dd      (fast-*)
        db      "turbo"
        db      0,0,0
        dd      IMM
fast    mfdcr   r3,iocr
        li      r4,0x4000
        or      r3,r3,r4
        mtdcr   iocr,r3
        blr
fast_

_slow   dd      (slow_-*)
        dd      (slow-*)
        db      "slow"
        dd      0
        dd      IMM
slow    mfdcr   r3,iocr
        lwa     r4,0xffffbfff
        and     r3,r3,r4
        mtdcr   iocr,r3
        blr
slow_


	ifndef	LITE

_taskman dd     (taskman_-*)
        dd      (taskman-*)
        db      "taskman"
        db      0
        dd      IMM
taskman stwu    r2,-4(r1)
        lwax    r2,taskptr
        lwz     r2,0(r2)
        stmw    r0,mt_r0(r2)    ; store all registers
        mr      r3,r2
        lwz     r4,0(r1)        ; get original r2
        addi    r1,r1,4         ; drop saved val
        stw     r4,mt_r2(r3)    ; copy in over bad value
        stw     r1,mt_r1(r3)    ; adjust saved r1 because r2 was saved
        mfcr    r4
        mflr    r5
        mfctr   r6
        mfspr   r7,srr0
        stw     r4,mt_cr(r3)
        stw     r5,mt_lr(r3)
        stw     r6,mt_ctr(r3)
        stw     r7,mt_pc(r3)
; context save complete

	lwax	r4,tm_quedepth	
	lwz	r5,0(r4)
	cmplwi	1,r5,0		; see if any messages in que
	lwax	r7,tmanstat	
	lwz	r7,0(r7)
	andi.	r0,r7,tman_msg	; see if message in progress
	bne	0,retmsg	; skip getting message if so
	bne	1,gettm_msg	; if not, get them!
		

retmsg  lwax    r6,ntasks	; get number of tasks
        lwz     r6,0(r6)
        cmplwi  0,r6,1
        beq     0,notasks       ; only one task? (skip switch)

stsetup	lwa     r8,(STOPPED|FAULT|ZOMBIE|DEAD|RTFLAG) ; skip mask
	lwax    r4,curtask      ; get current task number
        lwz     r5,0(r4)

;
; this is interleaved for maximum performance, hence the disjoint style
scantsk addi    r3,r3,tasksz    ; skip to next task
	addi    r5,r5,1         ; indicate switch to next task
	lwz	r7,mt_status(r3) ; see if task is stopped
	cmplw   1,r6,r5         ; at number of tasks? (over count)
	and.	r7,r7,r8
	beq     1,rsttptr       ; if==ntasks, reset to start of list, exit
	bne	0,scantsk	; if so, keep scanning		
	
; Main is always the first task, and it is NEVER stopped so resetting to the
; top of the list and executing that task will always work

rstpret	lwz	r9,mt_callback(r3) ; see if the task gets skipped or exec'd
	cmplwi	0,r9,0
	bne	0,skiptsk	; if it's not 0, skip the task for now

	lwz	r9,mt_priority(r3) ; load up skip count again
	stw	r9,mt_callback(r3) ; write it back
	
	stw	r5,0(r4)	; save current task number
     
notasks lwax    r4,taskptr
        stw     r3,0(r4)        ; update taskptr
	
; capture time and write to last service slots
nrecap	mfspr	r20,tbhi	; capture current time
	mfspr	r21,tblo
	mfspr	r22,tbhi	; check for rollover during between low-high
	cmplw	0,r22,r20	
	bne	0,nrecap
	stw	r20,mt_lcallhi(r3) ; write service time
	stw	r21,mt_lcalllo(r3)
	
	lwz	r4,mt_status(r3)
	andi.	r0,r4,SERVICE
	beq	0,restcon
	
	andi.	r4,r4,(~SERVICE) ; clear service flag
	stw	r4,mt_status(r3)

; restore context
restcon lwz     r7,mt_pc(r3)
        lwz     r6,mt_ctr(r3)
        lwz     r5,mt_lr(r3)
        lwz     r4,mt_cr(r3)
        mtspr   srr0,r7
        mtctr   r6
        mtlr    r5
        dd      0x7c8ff120      ; mtcr    r4
	lis	r4,0x400	; tsr flag
        mtspr   tsr,r4
	lmw     r0,mt_r0(r3)    ; restore registers
        lwz     r3,mt_r3(r3)    ; restore r3 (untouched by lmw)
        rfi

skiptsk addi 	r9,r9,-1	; decrement skip count
	stw	r9,mt_callback(r3)
	b	scantsk		; look for another task

rsttptr	lwax	r6,tasklst
	mr	r3,r6		; reset task pointer to start of list
	li	r5,0		; on first task
	b	rstpret


; r4 is pointing to tm_queptr
; r5 is tm_queptr
; r6 is tm_que
gettm_msg
	lwax	r6,tm_que 
gtmsg	add	r9,r5,r6	; add depth and que base
	lwz	r8,message_command(r9)
	cmplwi	0,r8,sighi
	addi	r5,r5,-16	; drop 16 bytes of depth
	stw	r5,0(r4)
	ble	0,sendsig
gtmm_ret cmplwi	0,r5,0
	bne	0,gtmsg
	b	retmsg

sendsig	lwz	r7,message_dest(r9) ; find out who to send it to
	cmplwi	0,r8,SIGINT
	cmplwi	1,r8,SIGTERM
	cmplwi	2,r8,SIGHUP
	beq	0,task_kill	; all equivalent to killing task
	beq	1,task_kill
	beq	2,task_kill
	cmplwi	0,r8,SIGSTOP	; pause task?
	cmplwi	1,r8,SIGSTART	; resume task?
	beq	0,task_stop
	beq	1,task_start
	cmplwi	0,r8,SIGSETPRI
	cmplwi	1,r8,SIGSETPRE
	cmplwi	2,r8,SIGSETCB
	beq	0,task_setpri
	beq	1,task_setpre
	beq	2,task_setcb
	b	gtmm_ret


task_kill 
	lwz	r8,mt_status(r7)
	ori	r8,r8,DEAD
	stw	r8,mt_status(r7)
	b	gtmm_ret

task_stop 
	lwz	r8,mt_status(r7)
	ori	r8,r8,STOPPED
	stw	r8,mt_status(r7)
	b	gtmm_ret

task_start
	lwz	r8,mt_status(r7)
	andi.	r8,r8,(~STOPPED)
	stw	r8,mt_status(r7)
	b	gtmm_ret
	
	
task_setpri
	lwz	r8,message_val(r9)
	stw	r8,mt_priority(r7)
	b	gtmm_ret

task_setpre
	lwz	r8,message_val(r9)
	stw	r8,mt_prempt(r7)
	b	gtmm_ret
	
task_setcb
	lwz	r8,message_val(r9)
	stw	r8,mt_callback(r7)
	b	gtmm_ret


taskman_





; [[[[[[[[[[[[[[[[[Real time version of task man ]]]]]]]]]]]]]]]]]]]]]]]]
		
putp2	macro
	endm
putp	macro	char
	stwu	r3,-4(r1)
	mfcr	r3
	stwu	r3,-4(r1)
	stwu	r20,-4(r1)
	mflr	r20
	li	r3,char
	bl	putch
	mtlr	r20
	lwz	r20,0(r1)
	addi	r1,r1,4
	lwz	r4,0(r1)
	addi	r1,r1,4
	dd      0x7c8ff120      ; mtcr    r4
	lwz	r3,0(r1)
	addi	r1,r1,4
	endm

prstr2	macro
	endm


	
; main code
_rttaskman dd   (rttaskman_-*)
        dd      (rttaskman-*)
        db      "rttaskman"
        db      0,0,0
        dd      IMM
rttaskman stwu    r2,-4(r1)
	mfcr	r2
	stwu	r2,-4(r1)	; save CR temporarily
	lwax	r2,tmanstat
	lwz	r2,0(r2)
	andi.	r2,r2,tman_rtexec ; are we returning from RT word?
	beq	0,notretrt
	lwax	r2,rtptr	; otherwise get the RT task pointer
	b	rtret

notretrt lwax    r2,taskptr
rtret   lwz     r2,0(r2)
        stmw    r0,mt_r0(r2)    ; store all registers
        mr      r3,r2
        lwz     r4,4(r1)        ; get original r2-- 0(r1)
        stw     r4,mt_r2(r3)    ; copy in over bad value
        
	lwz	r4,0(r1)	; get saved CR

        mflr    r5
        mfctr   r6
        mfspr   r7,srr0
        stw     r4,mt_cr(r3)
        stw     r5,mt_lr(r3)
        stw     r6,mt_ctr(r3)
        stw     r7,mt_pc(r3)

	addi	r1,r1,8		; drop saved r2/CR
	stw     r1,mt_r1(r3)    ; adjust saved r1 because r2 was saved
; context save complete...
	
recap	mfspr	r20,tbhi	; capture current time
	mfspr	r21,tblo
	mfspr	r22,tbhi	; check for rollover during between low-high
	cmplw	0,r22,r20	
	bne	0,recap	

; r10 is task id
; r11 is priority
; r12 is diff in time between callback time and current time
; r13 is preempt time for task
; r14 is task header pointer
; r15 is new task id
; r16 is new priority
; r17 is new diff in time
; r18 is new preempt time
; r19 is new task header pointer
; 

; setup and then scan all tasks
rtsetup lwax	r3,tasklst	; reset to top of list of tasks
	li	r10,-1		; no task id yet
	li	r11,-1		; lowest priority possible (so when new
	li	r12,-1		; task information is loaded it will
				; overwrite this junk info)
	lwa     r8,(STOPPED|FAULT|ZOMBIE|DEAD) ; skip mask
	lwax    r6,ntasks	; get number of tasks
	li	r5,0
        lwz     r6,0(r6)
	
;
; this is interleaved for maximum performance, hence the disjoint style
scantsk2 addi   r3,r3,tasksz  	; skip to next task
	addi    r5,r5,1         ; indicate switch to next task
	lwz	r7,mt_status(r3) ; see if task is stopped, dead, etc.
	cmplw   1,r6,r5         ; at number of tasks? (over count)
	and.	r9,r7,r8
	beq     1,tcend   	; if==ntasks, we're done scanning
	bne	0,scantsk2	; if it matches mask, keep scanning

; Fall through to this if the mask doesn't match (ok to exec)

	andi.	r9,r7,RTFLAG	; see if it's RT
	beq	0,scantsk2	; if not, keep searching...
	andi.	r9,r7,(MSGRDY|FIXEDRT)
	beq	0,scantsk2	; if not, keep searching...


; Fall through to this if the mask doesn't match and it really is an
; RT task.  We still have tasks to scan through; this will compare 
; priorities, etc.



; load new task info 
		
gettask lwz	r4,mt_callback(r3)
	lwz	r15,mt_word(r3)
	lwz	r16,mt_priority(r3)
	lwz	r17,mt_lcalllo(r3)
	lwz	r18,mt_prempt(r3)
	mr	r19,r3		; save current pointer to task
	lwz	r22,mt_lcallhi(r3) ; get time of last callback
	
; ********************************************************
; now compare new task against previously loaded task info

cmptsk	addc	r17,r17,r4	; add in callback time
	addze	r22,r22		; add in possible carry
	subfc	r17,r17,r21	; get diff in time between cb and time
	subfe.	r22,r22,r20	; i.e. (current_time-callback+lcall)
	

; negative is pending (not due), zero=due, positive is overdue

; we need to adjust the lower word so the upper word is 
; not needed; if uw<-1, then the lw=0x80000000, and if the
; uw>0 then the lw=0x7fffffff

	cmplwi	0,r22,0		; see if upper word is 0
	beq	0,tchk		

; if not, compensate for upper word

chkuw	andis.	r0,r22,0x8000	; see if the high word is negative
	beq	0,notneg
	li	r24,-1
	cmplw	0,r22,r24	; see if it's simply -1
	beq	0,tchk		; if so, skip altering anything
	lis	r17,0x8000	; otherwise load with lowest neg number
	b	pending


notneg	lwa	r17,0x7fffffff	; largest positive number 
	b	due
	
	
; by this time we now have a signed lower word (r17) in the 
; range of 0-0x7fffffff (positive) and 0xffffffff-0x80000000 (negative)
tchk	andis.	r0,r17,0x8000	; negative?
	bne	0,pending
	
; fall through if due/overdue

; task is due (or overdue)
due	cmplw	1,r11,r16
	andis.	r0,r12,0x8000	; see if current task is real
	bgt	1,wrnew		; compare pri-- if greater write new
	bne	0,wrnew		; if it's not due, write in new task
	blt	1,scantsk2	; if current is real, and higher pri

dpeq	andis.	r0,r12,0x8000	; must be equal priorities-- test due time
	cmplw	1,r17,r12	; compare due times if both due
	bne	0,wrnew		; if current isn't due, write new task in
	bgt	1,wrnew		; if current due time>new, write new in
	b	scantsk2


pending	andis.	r0,r12,0x8000	; see if current is due
	cmplw	1,r11,r16	; compare priorities
	beq	0,tstpre	; if current is due, check its preempt
	bgt	1,wrnew		; if new pri>current (not due) write new in
	beq	1,tstdue	; if pri are equal compare due times
	b	scantsk2

; if the current task is due, but the pending task has a higher priority,
; shave off the difference in its due time from the current task's preempt
tstpre	bgt	1,truncpre	; if the new task is a higher pri, do it
	b	scantsk2

; truncate preempt time from current task
truncpre nor	r24,r17,r17	; get new task's due time (negative), invert
	subf	r25,r24,r13	; get (duetime-preempt)
	andis.	r0,r25,0x8000	; negative? (duetime>preempt)
	bne	0,scantsk2	; ok to let current run unaltered
	subf	r13,r25,r13	; if not, sub out diff in preempt time
	cmplwi	0,r13,200	; if less than 200 for preempt, skip task
	blt	0,wrnew		; write new one in its place
	b	scantsk2

tstdue	cmplw	0,r12,r17
	blt	0,wrnew		; if new is more due, write in
	b	scantsk2

wrnew	mr	r10,r15		; id
	mr	r11,r16		; priority
	mr	r12,r17		; callback
	mr	r13,r18		; preempt
	mr	r14,r19		; pointer to header
	b	scantsk2	; look at other tasks



; end of task process loop -------------------------------------------


tcend  	andis.	r0,r11,0x8000	; see if task captured (not -1 pri)
	bne	0,no_rt		; no rt task to execute
	mr	r3,r14		; get chosen task's header pointer
	andis.	r0,r12,0x8000	; see if a word is really due
	bne	0,notdue	; nope

rtword	mfspr	r4,tcr		; we're entering to call a RT word
	andis.	r4,r4,(~0x80)   ; turn off FIT during RT task 
	mtspr	tcr,r4
     
	lwax	r4,tmanstat
	lwz	r9,0(r4)	; get taskman stat
	ori	r9,r9,tman_rtexec ; set RT execution flag
	stw	r9,0(r4)
	lwax	r4,rtptr	; get rt task pointer
	stw	r3,0(r4)	; save current task addr
	stw	r20,mt_lcallhi(r3) ; update last callback time
	stw	r21,mt_lcalllo(r3)
	mfspr   r4,tsr          ; reset timer interrupt flags
        mtspr   tsr,r4
        mtspr   pit,r13         ; set timer with time before pre-emption
	cmplwi	0,r13,0		; see if pit is set to 0
	beq	0,nopit
	b	retask		; return to task

nopit	prstr	"Pit set to 0!"
	dmpreg
	b	retask

no_rt	li	r9,0
	mtspr	pit,r9		; reset PIT
	lis	r9,0x800
	mtspr	tsr,r9		; reset PIT int (most probable cause)
	lwax	r3,taskptr
	lwz	r3,0(r3)	; resume previous non-rt task
	lwax	r4,tmanstat
	lwz	r9,0(r4)	; get taskman stat
	andi.	r9,r9,(~tman_rtexec) ; and off rt exec flag
	stw	r9,0(r4)
	mfspr	r4,tcr		; if no RT task ready turn on FIT
	oris	r4,r4,0x80	; turn on FIT interrupt again
	mtspr	tcr,r4
	b	retask
;
;
	
; when the RT word is done and none are due, the last non-RT task
; that was pre-empted needs to be restored.

notdue	lwax	r4,tmanstat
	lwz	r9,0(r4)	; get taskman stat
	andi.	r9,r9,(~tman_rtexec) ; and off rt exec flag
	stw	r9,0(r4)

	lwz	r4,mt_status(r3)
	andi.	r0,r4,MSGRDY
	bne	0,skipcap	; skip time stamp if MSGRDY
	lwz	r4,mt_callback(r3) 
; take lcallback+(callback*2)-current_time -> PIT
; capture current time and calculate callback time
	lwz	r22,mt_lcallhi(r3)
	lwz	r17,mt_lcalllo(r3)
	lwz	r4,mt_callback(r3)
recap2	mfspr	r20,tbhi
	mfspr	r21,tblo	
	mfspr   r9,tbhi
        cmplw   0,r9,r20        ; check for rollover 
        bne     0,recap2

	addc    r17,r17,r4      ; add in callback time
	addze   r22,r22         ; add in possible carry
        subfc   r17,r21,r17     ; get diff in time between cb and time
        subfe.  r22,r20,r22   	; i.e. ((callback+lcall)-current_time)

	addic.  r17,r17,-16     ; sub minimum delay between capture -> PIT load
	beq	0,rtsetup	; make sure callback isn't 0 (immediately due)
	andis.	r0,r17,0x8000	; it's negative service immediately!
	bne	0,rtsetup	; jump back to top of routine

	
; now get set up for returning to non-RT task

skipcap	lwax	r3,taskptr	; taskptr only holds non-RT addresses
	lwz	r3,0(r3)	; restore it so we return to non-RT

	mfspr	r4,tcr		; if no RT task ready turn on FIT
	oris	r4,r4,0x80	; turn on FIT interrupt again
	mtspr	tcr,r4
	lis	r4,0x800	; reset PIT int
        mtspr   tsr,r4
	mtspr	pit,r17		; finally load PIT with callback time

	
; restore context
retask  lwz     r7,mt_pc(r3)
        lwz     r6,mt_ctr(r3)
        lwz     r5,mt_lr(r3)
        lwz     r4,mt_cr(r3)
        mtspr   srr0,r7
        mtctr   r6
        mtlr    r5
        dd      0x7c8ff120      ; mtcr    r4
	lmw     r0,mt_r0(r3)    ; restore registers
        lwz     r3,mt_r3(r3)    ; restore r3 (untouched by lmw)
        rfi			; return to routine

rttaskman_

; ((((((((((((((((End of RT Section))))))))))))))))))))))))
	
							
; command value destination  sendmsg  [return value 0/-error]
_sendmsg dd	(sendmsg_-*)
	dd	(sendmsg-*)
	db	"sendmsg"
	db	0
	dd	IMM
sendmsg	lwz	r3,8(r30)	; command
	lwz	r4,4(r30)	; value
	lwz	r5,0(r30)	; dest
	addi	r30,r30,12
	mr	r9,r5		; save task number
	addi	r5,r5,-1
	lwax	r6,tasklst
	mulli	r5,r5,tasksz
	add	r5,r5,r6	; get real pointer to task
	lwax	r7,ntasks
	lwz	r7,0(r7)
	li	r8,1
	
fndtsk	cmplw	1,r9,r8		; task number found?
	cmplw	0,r7,r8		; at number of tasks?
	addi	r8,r8,1	
	beq	1,foundtsk
	beq	0,ateoflst	; didn't find a task of that id
	b	fndtsk	
	
foundtsk cmplwi	0,r3,sighi	; compare command with highest signal
	ble	0,is_sig
; otherwise we're sending directly to a task
	ints_off
	lwz	r6,mt_quedepth(r5)
	cmplwi	0,r6,64		; make sure que isn't full
	beq	0,quefull
	lwz	r7,mt_status(r5)
	ori	r7,r7,MSGFLAG	; signal message being received-
	stw	r7,mt_status(r5) ; so que isn't modified while we write
	ints_on
	lwax	r7,tmanstat	; see if we're in a RT/no-RT word
	lwz	r7,0(r7)
	andi.	r0,r7,tman_rtexec
	bne	0,svcrt		; we're servicing a RT word
	lwax	r7,taskptr
gtskrt	lwz	r7,0(r7)	; get current task we're executing
	addi	r6,r6,16
	stw	r6,mt_quedepth(r5)
	addi	r8,r5,mt_msgque	; get address of que
	add	r8,r8,r6	; add in total depth
	stw	r7,message_source(r8) 	; save task id of source
	stw	r3,message_command(r8) 	; command
	stw	r4,message_val(r8)	; value
	stw	r5,message_dest(r8)	; destination
	lwz	r7,mt_status(r5)
	ori	r7,r7,MSGRDY	; message in que
	andi.	r7,r7,(~MSGFLAG) ; signal message reception done
	stw	r7,mt_status(r5)
recap3	mfspr	r6,tbhi
	mfspr	r8,tblo
	mfspr	r9,tbhi
	cmplw	0,r9,r6
	bne	0,recap3
	stw	r6,mt_lcallhi(r5)
	stw	r8,mt_lcalllo(r5)
	li	r3,0		; set for taskman syscall 
	sc			; do a system call to call dest task
;	li	r3,0
	stwu	r3,-4(r30)	
	blr

; tm_que, tm_quedepth
is_sig	lwax	r6,tm_quedepth
	lwz	r6,0(r6)
	cmplwi	0,r6,128	
	beq	0,quefull
	lwax	r7,tmanstat
	lwz	r8,0(r7)
	andi.	r0,r8,tman_msg	; message in progess already?
	bne	0,msginprg
	ori	r8,r8,tman_msg
	stw	r8,0(r7)
	andi.	r0,r8,tman_rtexec
	bne	0,svcrt2	; we're servicing a RT word
	lwax	r7,taskptr
gtskrt2	lwz	r7,0(r7)	; get current task we're executing
	addi	r6,r6,16	; add 16 to que depth
	lwax	r8,tm_quedepth
	stw	r6,0(r8)
	lwax	r8,tm_que	
	add	r8,r8,r6	; get address of last message word+4
	stw	r7,message_source(r8) 	; save task id of source
	stw	r3,message_command(r8) 	; command
	stw	r4,message_val(r8)	; value
	stw	r5,message_dest(r8)	; destination
	lwax	r7,tmanstat
	lwz	r8,0(r7)
	andi.	r8,r8,(~tman_msg) ; signal message reception done
	stw	r8,0(r7)
	li	r3,0
	stwu	r3,-4(r30)	
	blr 

ateoflst li	r3,-1
	stwu	r3,-4(r30)
	blr

quefull	li	r3,-2
	stwu	r3,-4(r30)
	blr	

msginprg li	r3,-3
	stwu	r3,-4(r30)
	blr 
	
svcrt	lwax	r7,rtptr
	b	gtskrt

svcrt2	lwax	r7,rtptr
	b	gtskrt2

sendmsg_	



; - getmsg  dest src val command
_getmsg	dd	(getmsg_-*)
	dd	(getmsg-*)
	db	"getmsg"
	db	0,0
	dd	IMM
getmsg	lwax	r3,tmanstat
	lwz	r3,0(r3)
	andi.	r0,r3,tman_rtexec	; executing RT task?
	bne	0,ex_rt
	lwax	r3,taskptr
	b	gettptr
ex_rt	lwax	r3,rtptr
gettptr	lwz	r3,0(r3)
	ints_off
	lwz	r7,mt_status(r3)
	andi.	r0,r7,MSGFLAG
	bne	0,msginprog	; msg in progess 
	ori	r7,r7,MSGFLAG
	stw	r7,mt_status(r3)
	lwz	r5,mt_quedepth(r3) ; get que depth
	ints_on
	addi	r6,r3,mt_msgque	; get que base
	cmplwi	0,r5,0		; see if there's any message
	beq	0,nomsg
	add	r9,r5,r6	; add depth and que base
	lwz	r8,message_command(r9)
	stwu	r8,-4(r30)
	lwz	r8,message_val(r9)
	stwu	r8,-4(r30)
	lwz	r8,message_source(r9)
	stwu	r8,-4(r30)
	lwz	r8,message_dest(r9)
	stwu	r8,-4(r30)
	addi	r5,r5,-16	; drop 16 bytes of depth
	stw	r5,mt_quedepth(r3)
	cmplwi	1,r5,0
	lwz	r7,mt_status(r3)
	bne	1,msginq
	andi.	r7,r7,(~MSGRDY)  ; no more messages
msginq	andi.	r7,r7,(~MSGFLAG) ; unlock que
	stw	r7,mt_status(r3)
	blr

msginprog ints_on
	blr

nomsg	li	r3,-1
	stwu	r3,-4(r30)
	stwu	r3,-4(r30)	
	stwu	r3,-4(r30)
	stwu	r3,-4(r30)
	blr
getmsg_



_addtsk	dd	(addtsk_-*)
	dd	(addtsk-*)
	db	"addtask"
	db	0
	dd	IMM
addtsk 	lwax	r9,tasklst	; get top of list
        lwax    r4,ntasks
        lwz     r3,0(r4)
        li      r5,tasksz
        mullw   r5,r5,r3
	mr	r6,r9		; save start of list
        add     r9,r9,r5        ; get pointer to free space for next task
        mr      r4,r9
	li	r3,tasksz

; reap space allocated to dead tasks	
fnddead	cmplw 	0,r6,r9		; at end of list?
	beq	0,no_dead
	lwz	r7,mt_status(r6)
	andi.	r7,r7,DEAD	; dead task?
	bne	0,isdead
	addi	r6,r6,tasksz
	b	fnddead
	
isdead	mr	r4,r6		; point to dead task go into init
	mr	r9,r6		; likewise
	li	r10,1		; dead task was found
	b 	clrmem
	
no_dead li	r10,0		; no dead task was found
	b	clrmem
	
clrmem	li	r5,0
	li	r6,0
	
clr_lp	cmplwi	0,r10,0		; replacing dead word in list?
	beq	0,newtask	; ok to zero status
	cmplwi	0,r6,mt_status	; about to clear status word?
	beq	0,skipclrsw	; word MUST remain dead until all info is correct
				; to avoid a crash because the taskman executed the
				; word before all info has been replaced
newtask	stwx	r5,r4,r6	; clear out task's space
skipclrsw addic. r3,r3,-4
	addi	r6,r6,4
	bne	0,clr_lp
	
	
	li	r11,0		; zero temp copy of status register
	lwz	r4,0(r30)	; pull addr of word to add
	addi	r30,r30,4	
	lwz     r3,4(r4)        ; get string/pfa len
        lwzx    r5,r4,r3        ; get pfa
	addi	r3,r3,4	
	add	r6,r4,r3	; find address of code to execute
	stw	r6,mt_pc(r9)	; save it
        andi.  	r6,r5,MULTI	; see if word is enhanced
        beq     0,notenh        ; not multi-tasking enhanced

	andi.  	r6,r5,MTRT	; see if it's a RT word
	beq	0,notrt

	li	r3,RTFLAG		
	andi.	r6,r5,MTFR 	; fixed rate?
	beq	0,notfr
	ori	r3,r3,FIXEDRT	; or in fixed rate mt flag
notfr	mr	r11,r3		; save status in temp register
	lwax	r6,tmanstat	
	lwz	r5,0(r6)
	ori	r5,r5,tman_rt	; or in RT task present flag
	stw	r5,0(r6)	; save it	

notrt   lwz     r3,0(r4)        ; get total word len
        add     r5,r4,r3        ; get address after word's end
	mr	r6,r11		; get temp copy of status 
	andi.	r6,r6,RTFLAG
	beq	0,skippre	; if not, RT leave 0's in preempt and callback

       	lwz     r6,mt_tsk_prempt(r5) ; get time before pre-emption
	stw	r6,mt_prempt(r9) 
	lwz     r6,mt_tsk_callback(r5) ; get time before pre-emption
       	stw     r6,mt_callback(r9)
skippre lwz     r6,mt_tsk_pri(r5) ; get priority

ne_ret	stw	r6,mt_priority(r9)
        stw     r4,mt_word(r9)  ; save word address
	mfspr	r4,tblo
	mfspr	r5,tbhi
	stw	r4,mt_st_lo(r9)
	stw	r5,mt_st_hi(r9)	
	stw	r4,mt_lcalllo(r9) ; set last call back times
	stw	r5,mt_lcallhi(r9)
        stmw    r0,mt_r0(r9)    ; save current register states
	li	r4,tasksz
	stw	r4,mt_size(r9)	; save size of task header

; now set the state of the task's internal stacks
        addi    r3,r9,mt_r1stk
        stw     r3,mt_r1(r9)
        addi    r3,r9,mt_r26stk
        stw     r3,mt_r26(r9)
        addi    r3,r9,mt_r29stk
        stw     r3,mt_r29(r9)
        addi    r3,r9,mt_r30stk
  	stw     r3,mt_r30(r9)
; finally write status after all info is current
	stw	r11,mt_status(r9)
	
	lwax	r4,tasklst
	subf	r9,r4,r9
	li	r4,tasksz
	divwu	r9,r9,r4
	addi	r9,r9,1
	stwu	r9,-4(r30)	; return pointer to task header
	
	cmplwi	0,r10,1		; found dead task in list?
	beq	0,skipat	; if so, number of tasks hasn't changed
	lwax	r4,ntasks	; finally update number of tasks (so we don't 
				;  crash when the new task is seen prematurely)
	lwz	r3,0(r4)
	addi    r3,r3,1         ; inc number of tasks
        stw     r3,0(r4)        ; save it
skipat	lwax	r4,tmanstat
	lwz	r5,0(r4)	
	andi.	r3,r5,tman_rt	; see if we are in RT state yet
	bne	0,start_rt
at_exit blr


start_rt andi.	r3,r5,tman_rtexec ; actually executing right now?
	bne	0,at_exit	; if it's already running, exit
	li	r3,
	li	r3,1
	mtspr	pit,r3		; otherwise trip the pit ;)
	blr
	
notenh	li	r6,0x1000	; preempt
	stw	r6,mt_prempt(r9)
	li	r6,0x10		; priority
	b	ne_ret	
addtsk_	


_tasklst dd    	(tasklst_-*)
        dd      (tasklist-*)
        db      "tasklist"
        dd      0
        dd      IMM
tasklist lwax	r3,tasklst
	stwu	r3,-4(r30)
	blr
tasklst_

_ntasks dd      (ntasks_-*)
        dd      (numtasks-*)
        db      "numtasks"
        dd      0
        dd      IMM
numtasks lwax   r3,ntasks
	stwu	r3,-4(r30)
	blr
ntasks_

_mton   dd      (mton_-*)
        dd      (mton-*)
        db      "multi_on"
        dd      0
        dd      IMM
mton    lwax    r3,ntasks
        lwz     r3,0(r3)
        cmplwi  0,r3,0          ; make sure number of tasks!=0
        beq     0,notskerr
        li      r3,0
        mtspr   pit,r3          ; make sure pit==0
        mfspr   r3,tsr
        mtspr   tsr,r3          ; clear any pending interrupts
        lwa     r3,0x5800000    ; enable PIT/FIT at 2^13
        mtspr   tcr,r3          ; enable interrupts
        lis     r3,0x800
        mtdcr   exier,r3        ; external interrupts allowed
        mfdcr   r3,exisr
        mtdcr   exisr,r3        ; clear any pending interrupts
        lwa     r3,0x9200	; 0x8000+0x1000 (machine check enable)
        mtmsr   r3              ; allow interrupts
	lwax	r3,tmanstat
	lwz	r4,0(r3)
	ori	r4,r4,tman_multi ; say we're multitasking now
	stw	r4,0(r3)
	andi.	r5,r4,tman_rt	; see if any RT words present
	bne	0,go_rt
        blr

go_rt	li	r3,1
	mtspr	pit,r3
	blr	

notskerr lwar   r4,notskmsg
        bl      puts
        b       abort
mton_


_mtoff  dd      (mtoff_-*)
        dd      (mtoff-*)
        db      "multi_off"
        db      0,0,0
        dd      IMM
mtoff   li      r3,0
        mtspr	tcr,r3		; turn off timer ints
	lwax	r3,tmanstat
	lwz	r4,0(r3)
	andi.	r4,r4,(~tman_multi)
	stw	r4,0(r3)
        blr
mtoff_


; Please note that the code used for this function will NEVER allow the LED
; to blink faster than the rate set by the bit that is observed from tblo!!!
; HOWEVER, you may end up with some odd effects if tasks of a high priority
; habitually shove this task out of the way....
_syswatch dd    (syswatch_-*)
        dd      (syswatch-*)
        db      "syswatch"
        dd      0
        dd      IMM|MULTI
syswatch mfspr   r20,tblo
        andis.  r20,r20,0x80    ; approx 2 toggles/sec at 66 MHz
        beq     0,ledoff
        li      r20,0xff
        b       ledstor
ledoff  li      r20,0
ledstor lwa     r21,0x70110000	; this is the address of the LED latch
        stb     r20,0(r21)
        b	syswatch
	dd	100000		; callback time
        dd      1000  		; time before pre-emption
	dd	100		; priority (higher the number, lower the pri)
syswatch_


; this routine will blink the LED faster or slower based on the 
; amount of time allotted to it.
_syswatch3
	 dd    (syswatch3_-*)
        dd      (syswatch3-*)
        db      "syswatch3"
        db      0,0,0
        dd      IMM|MULTI
syswatch3
	li 	r3,0
ledlp	addi	r3,r3,1
	andi.	r5,r3,0x400
	beq     0,ledoff2
        li      r20,0xff
        b       ledstor2
ledoff2  li      r20,0	
ledstor2 lwa     r21,0x70110000	; this is the address of the LED latch
        stb     r20,0(r21)
        b	ledlp		; loop forever
	
	dd	100000		; callback time
        dd      1000  		; time before pre-emption
	dd	100		; priority (higher the number, lower the pri)
syswatch3_


_msgtst dd  	(msgtst_-*)
        dd      (msgtst-*)
        db      "msgtst"
        db      0,0
        dd      IMM|MULTI|MTRT
msgtst 	bl	getmsg
	bl	dot
	bl	cr
	bl	dot
	bl	cr
	bl	dot
	bl	cr
	bl	dot
	bl	cr
	li	r4,1
	mtspr	pit,r4		; trigger preempt
	b	msgtst
	


        dd      0		; callback time
	dd	0x10000000	; time before pre-emption
	dd	100		; priority (higher the number, lower the pri)
msgtst_


_syswatchrt dd  (syswatchrt_-*)
        dd      (syswatchrt-*)
        db      "syswatchrt"
        db      0,0
        dd      IMM|MULTI|MTRT|MTFR
syswatchrt li	r3,42
	bl	putch
	li	r4,1
	mtspr	pit,r4		; trigger preempt
	b	syswatchrt
	
        dd      0x4ffffff 	; callback time
	dd	0x1000		; time before pre-emption
	dd	100		; priority (higher the number, lower the pri)
syswatchrt_

_syswatchrt2 dd (syswatchrt2_-*)
        dd      (syswatchrt2-*)
        db      "syswatchrt2"
        db      0
        dd      IMM|MULTI|MTRT|MTFR
syswatchrt2 li	r3,43
	bl	putch
	li	r4,1
	mtspr	pit,r4		; trigger preempt
	b	syswatchrt2
	
        dd      0x4ffffff 	; callback time
	dd	0x1000		; time before pre-emption
	dd	100		; priority (higher the number, lower the pri)
syswatchrt2_





smartspc macro  minspc
	lwax    r3,nlen         ; number of digits printed
        lwz     r3,0(r3)
        subfic   r3,r3,minspc	; at least 2 spaces between numbers
	andis.	r0,r3,0x8000
	bne	0,(*+12)
        stwu    r3,-4(r30)
        bl      spaces
	endm

_lsttsk dd      (lsttsk_-*)
        dd      (lsttsk-*)
        db      "ps"
        db      0,0
        dd      IMM
lsttsk	mflr	r9
	lwax	r5,ntasks
	lwz	r5,0(r5)
	cmplwi	0,r5,0
	beq	0,notsklst
	li	r6,tasksz
	lwax	r7,tasklst
	li	r8,1		; starting task number

prtsklp	svregs			; save r3-9	
	lwz	r3,mt_status(r7)
	stwu	r3,-4(r30)
	lwz	r3,mt_st_lo(r7) ; start time low/high words
	stwu	r3,-4(r30)
	lwz     r3,mt_st_hi(r7)
        stwu    r3,-4(r30)
	stwu	r7,-4(r30)	; task header address
	lwz	r3,mt_prempt(r7)
	stwu	r3,-4(r30)
	lwz	r3,mt_callback(r7)
	stwu	r3,-4(r30)
	lwz	r3,mt_priority(r7) ; task priority
	stwu	r3,-4(r30)
        stwu    r8,-4(r30)	; task number
	bl	dot		; print task number
	smartspc 4
	bl	dot		; task priority
 	smartspc 4		; we'll assume a task is never at pri>0xffff
	bl	dot		; callback
	smartspc 6		; never larger than 0xffffff
	bl	dot		; preempt
	smartspc 6		; never larger than 0xffffff
	bl      dot             ; task header address
	smartspc 10
	bl	dot		; high start time
	smartspc 8
	li	r3,58
	bl	putch
	bl	dot		; low start time
	smartspc 10

	lwz	r4,0(r30)	; print status flags
	lwar	r5,mtflags	; string of possible flags
	li	r6,0
	addi	r30,r30,4
flglp	andi.	r7,r4,1
	bne	0,prflag
	li	r3,45
prfret	bl	putch
	srawi	r4,r4,1		; shift out flag
	addi	r6,r6,1
	cmplwi	0,r6,nflags
	bne	0,flglp
	li	r3,32
	bl	putch
	bl	putch

	rstregs			; restore registers r3-9	
	lwz	r4,4(r7)	; get word address
	addi	r4,r4,8		; point to start of string
	svregs
	bl	puts		; print it
	rstregs
	bl	crlf
	addi	r7,r7,tasksz
	addi	r8,r8,1
	addic.	r5,r5,-1
	bne	0,prtsklp
	mtlr	r9
notsklst blr

prflag	lbzx	r3,r5,r6	; get flag character
	b	prfret
lsttsk_

_stptask dd      (stptask_-*)
        dd      (stptask-*)
        db      "stoptask"
        dd      0
        dd      IMM
stptask	mflr	r10
	lwz	r3,0(r30)
	addi	r30,r30,4
	li	r4,SIGSTOP
	stwu	r4,-4(r30)
	stwu	r4,-4(r30)	; we send the signal as the val as well
	stwu	r3,-4(r30)	; (val doesn't do anything)
trymsg 	bl	sendmsg
	lwz	r3,0(r30)
	addi	r30,r30,4
	neg	r3,r3		; change error flag to positive
	cmplwi	0,r3,1
	beq	0,nosuchtsk
	cmplwi	0,r3,2
	beq	0,tqfull
	cmplwi	0,r3,3
	beq	0,trymsg
	mtlr	r10
	blr

nosuchtsk lwar	r4,nstmsg
	bl	puts
	mtlr	r10
	blr	

tqfull	lwar	r4,quefmsg
	bl	puts
	mtlr	r10
	blr
stptask_

; wait until current task has been preempted at least once
_wtsvc  dd     	(wtsvc_-*)
        dd      (wtsvc-*)
        db      "wait4svc"
        dd      0
        dd      IMM
wtsvc	lwax	r3,taskptr
	lwz	r3,0(r3)
	lwz	r4,mt_status(r3)
	ori	r4,r4,SERVICE
	stw	r4,mt_status(r3)
wt4slp	lwz	r4,mt_status(r3)
	andi.	r4,r4,SERVICE
	bne	0,wt4slp
	blr
wtsvc_
	

; force context switch 
_next 	dd      (next_-*)
        dd      (next-*)
        db      "next"
        dd      0
        dd      IMM
next    lwax    r3,taskptr
        lwz     r3,0(r3)
        lwz     r4,mt_status(r3)
	andi. 	r5,r4,FIXEDRT	; fixed rate task?
	beq	wtsvc		; if not, simply wait for service	 
	li	r3,1
	mtspr	pit,r3		; force rttaskman call
	blr
next_



; There is a slight chance a FR task will be either added or stopped
; while looking at the task list, thus the PIT may be started in error
_strtask dd     (strtask_-*)
        dd      (strtask-*)
        db      "startask"
        dd      0
        dd      IMM
strtask mflr	r13
	
	lwa     r8,(STOPPED|FAULT|ZOMBIE|DEAD) ; skip mask
	lwax    r6,ntasks	; get number of tasks
	li	r5,0
        lwz     r6,0(r6)
	lwax	r3,tasklst
	
;
; this is interleaved for maximum performance, hence the disjoint style
scantsk3 addi   r3,r3,tasksz  	; skip to next task
	addi    r5,r5,1         ; indicate switch to next task
	lwz	r7,mt_status(r3) ; see if task is stopped, dead, etc.
	cmplw   1,r6,r5         ; at number of tasks? (over count)
	and.	r0,r7,r8
	beq     1,stdone  	; if==ntasks, we're done scanning
	bne	0,scantsk3	; if it matches mask, keep scanning

; Fall through to this if the mask doesn't match (ok to exec)

	andi.	r0,r7,RTFLAG	; see if it's RT
	beq	0,scantsk3	; if not, keep searching...
	andi.	r0,r7,FIXEDRT
	beq	0,scantsk3	; if not, keep searching...

	li	r10,1		; we have at least one FR task active
	b	st_nxt

stdone	li	r10,0		; no FR tasks active
st_nxt	lwz	r3,0(r30)
	addi	r30,r30,4	
	mr	r11,r3		; save r3
	li	r4,SIGSTART
	stwu	r4,-4(r30)
	stwu	r4,-4(r30)	; we send the signal as the val as well
	stwu	r3,-4(r30)	; (val doesn't do anything)
	bl	sendmsg		; uses r3-r9
	bl	wtsvc		; wait for message to be received

	mr	r3,r11		; restore r3
	addi	r3,r3,-1
	lwax	r4,tasklst
	mulli	r3,r3,tasksz
	add	r3,r3,r4	; get real pointer to task

	lwz	r4,0(r30)
	addi	r30,r30,4
	cmplwi	0,r4,0
	bne	0,nosuchtsk
	lwz	r4,mt_status(r3)
	andi.	r0,r4,FIXEDRT	; fixed rate word restarted?
	beq	0,st_end

	cmplwi	0,r10,0		; did we have any FR tasks active?
	bne	0,st_end	; if so, skip starting PIT
	prstr	"(re-priming PIT)\r\n"
	li	r4,1
	mtspr	pit,r4		; start pit to run FR task		
st_end	mtlr	r13
	blr
strtask_


_subtsk dd    	(subtsk_-*)
        dd      (subtask-*)
        db      "-task"
        db      0,0,0
        dd      IMM
subtask	lwax	r3,ntasks
	lwz	r4,0(r3)
	cmplwi 	0,r4,1	
	beq	0,stend
	addi	r4,r4,-1
	stw	r4,0(r3)	
stend	blr
subtsk_


_killtsk dd	(killtsk_-*)
	dd	(killtsk-*)
	db	"killtask"
	dd	0
	dd	IMM
killtsk	mflr	r10
	lwz     r3,0(r30)	; task
	li	r5,0		; no data to pass in message
	addi	r30,r30,4	; drop task number
	li	r4,SIGTERM	; one of the designated kill codes
	stwu	r4,-4(r30)
	stwu	r5,-4(r30)	
	stwu	r3,-4(r30)
	b	trymsg		; attempt to send message
killtsk_


_setpri dd      (setpri_-*)
        dd      (setpri-*)
        db      "set-priority"
        dd      0
        dd      IMM
setpri	mflr	r10
	lwz     r3,0(r30)	; task
	lwz	r5,4(r30)	; priority
	addi	r30,r30,8
	li	r4,SIGSETPRI
	stwu	r4,-4(r30)
	stwu	r5,-4(r30)	
	stwu	r3,-4(r30)
	b	trymsg
setpri_


_setpre dd      (setpre_-*)
        dd      (setpre-*)
        db      "set-preempt"
        db      0
        dd      IMM
setpre	mflr	r10
	lwz     r3,0(r30)	; task
	lwz	r5,4(r30)	; preempt
	addi	r30,r30,8
	li	r4,SIGSETPRE
	stwu	r4,-4(r30)
	stwu	r5,-4(r30)	
	stwu	r3,-4(r30)	
	b	trymsg
setpre_


_setcb  dd      (setcb_-*)
        dd      (setcb-*)
        db      "set-callback"
        dd      0
        dd      IMM
setcb	mflr	r10
	lwz     r3,0(r30)	; task
	lwz	r5,4(r30)	; callback
	addi	r30,r30,8
	li	r4,SIGSETCB
	stwu	r4,-4(r30)
	stwu	r5,-4(r30)	
	stwu	r3,-4(r30)	
	b	trymsg
setcb_

_mkmt   dd      (mkmt_-*)
        dd      (mkmt-*)
        db      "make-multi"
        db      0,0
        dd      IMM
mkmt    lwz     r3,0(r30)       ; task
        addi    r30,r30,4
        lwz     r5,4(r3)        ; get total string/pfa len
        lwzx    r6,r3,r5        ; get pfa
        lwa     r7,MULTI
        or      r6,r6,r7
        stwx    r6,r3,r5        ; save pfa back
        blr
mkmt_



_mkrt   dd      (mkrt_-*)
        dd      (mkrt-*)
        db      "make-rt"
        db      0
        dd      IMM
mkrt	lwz     r3,0(r30)	; task
	addi	r30,r30,4
	lwz	r5,4(r3)	; get total string/pfa len
	lwzx	r6,r3,r5	; get pfa
	lwa	r7,(MTRT|MULTI)
	or	r6,r6,r7
	stwx	r6,r3,r5	; save pfa back
	blr
mkrt_

_mkfr   dd      (mkfr_-*)
        dd      (mkfr-*)
        db      "make-fr"
        db      0
        dd      IMM
mkfr	lwz     r3,0(r30)	; task
	addi	r30,r30,4
	lwz	r5,4(r3)	; get total string/pfa len
	lwzx	r6,r3,r5	; get pfa
	lwa	r7,MTFR
	or	r6,r6,r7
	stwx	r6,r3,r5	; save pfa back
	blr
mkfr_


_tmstat dd      (tmstat_-*)
        dd      (tmstat-*)
        db      "tmanstat"
        dd      0
        dd      IMM
tmstat	lwax   	r3,tmanstat	; task man status
	lwz	r4,0(r3)	
	stwu	r4,-4(r30)
	blr
tmstat_

	endif

	ifdef	bootload
_boot	dd      (boot_-*)
        dd      (boot-*)
        db      "auto-start"
        db      0,0
        dd      IMM
boot	lwa	r3,(rombase+szbtblk)
	mtlr	r3
	blr
boot_
	endif

_go	dd      (go_-*)
        dd      (go-*)
        db      "go"
        db      0,0
        dd      IMM
go	lwa	r3,loadpt
	mtlr	r3
	blr
go_

; end of dictionary
; END of Dictionary entries ---------------------------------------------
