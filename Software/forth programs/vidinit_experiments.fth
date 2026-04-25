
: mode480p16
ping1
$01 $740003c0 ac!               \ enable VGA
$740003cf ac@ drop              \ reset index
$00 $740003c3 ac!               \ disable output
ping1
$700003c2                       \ ATTRIBUTE REGISTERS
$4110 over ah!                  \ AR10: 256 clr, gfx
$0011 over ah!                  \ AR11: border: black
$0f12 over ah!                  \ AR12: enable all planes
$0013 over ah!                  \ AR13: no hoz panning
$0014 over ah!                  \ AR14: no pix padding
ping1
drop $ef $740003c1 ac!          \ misc output
ping1
$700003c6                       \ SEQUENCE REGISTERS
$1000 over ah!                  \ SR00: legacy VGA reset
$0101 over ah!                  \ SR01: 8 char clks; no pix double
$0f02 over ah!                  \ SR02: enable all planes
$0003 over ah!                  \ SR03: font plane
$0e04 over ah!                  \ SR04: 256kB; sequential; chain 4
$0608 over ah!                  \ SR08: unlock SR regs[09:1c]
$000b over ah!                  \ SR0B: color mode 0000
$4018 over ah!                  \ SR18
$0015 over ah!                  \ SR15
ping1
$000b over ah!                  \ vbios extended SR init
$0014 over ah!
$4018 over ah!
ping1
drop $700003d6                  \ CRTC REGISTERS
$0e11 over ah!                  \ CR11: unlock CRTC regs
$4838 over ah!                  \ CR38: unlock CRTC regs[2d:3f]
$a039 over ah!                  \ CR39: unlock CRTC regs[40-ff]
$0140 over ah!                  \ CR40: unlock enhanced regs
ping2
$8000 $700042e8 ah!             \ reset gfx engine
$4000 $700042e8 ah!             \ enable gfx, no irq
ping2
$4838 over ah!                  \ vbios extended crtc init
$a539 over ah!
$4032 over ah!
$0033 over ah!
$0035 over ah!
$0042 over ah!
$0043 over ah!
$0045 over ah!
$0053 over ah!
$0055 over ah!
$8358 over ah!
$0065 over ah!
$0066 over ah!
$0069 over ah!
$006a over ah!
ping2
$c200 over ah!                  \ CR00
$9f01 over ah!                  \ CR01
$a002 over ah!                  \ CR02
$8403 over ah!                  \ CR03
$a304 over ah!                  \ CR04
$1b05 over ah!                  \ CR05
$0c06 over ah!                  \ CR06
$3e07 over ah!                  \ CR07
$0008 over ah!                  \ CR08
$4009 over ah!                  \ CR09
$000a over ah!                  \ CR0a
$000b over ah!                  \ CR0b
$000c over ah!                  \ CR0c
$000d over ah!                  \ CR0d
$ff0e over ah!                  \ CR0e
$000f over ah!                  \ CR0f
$e910 over ah!                  \ CR10
$0b11 over ah!                  \ CR11
$df12 over ah!                  \ CR12
$a013 over ah!                  \ CR13
$6014 over ah!                  \ CR14
$e715 over ah!                  \ CR15
$0416 over ah!                  \ CR16
$ab17 over ah!                  \ CR17
$ff18 over ah!                  \ CR18
$0931 over ah!                  \ 
$1034 over ah!                  \ 
$153a over ah!                  \ 
$003b over ah!                  \ 
$613c over ah!                  \ 
$0140 over ah!                  \ 
$5050 over ah!                  \ CR50
$0051 over ah!                  \ 
$f854 over ah!                  \ CR54
$ff60 over ah!                  \ CR60
$005d over ah!                  \ CR5d
$005e over ah!                  \ CR5e
$5067 over ah!                  \ CR67
ping1
drop $700003cc                  \ GRAPHICS REGISTERS
$0000 over ah!                  \ GR00: reset data
$0001 over ah!                  \ GR01: reset data
$0002 over ah!                  \ GR02: no color compare
$0003 over ah!                  \ GR03: rotate count 0
$0004 over ah!                  \ GR04: read plane 0
$4005 over ah!                  \ GR05: write mode 0; 256clr
$0506 over ah!                  \ GR06: gfx mode, 64k@a0000
$0f07 over ah!                  \ GR07: color compare all
$ff08 over ah!                  \ GR08: allow write all
ping1
drop $700003c2                  \ ATTRIBUTE REGISTERS
$0000 over ah!                  \ set color palette
$0101 over ah!
$0202 over ah!
$0303 over ah!
$0404 over ah!
$0505 over ah!
$0606 over ah!
$0707 over ah!
$1008 over ah!
$1109 over ah!
$120a over ah!
$130b over ah!
$140c over ah!
$150d over ah!
$160e over ah!
$170f over ah!
ping1
$700003d6                       \ enable linear addressing
$1c53 over ah!                  \ enable MMIO
$fa54 over ah!                  \ endian swap
$0059 over ah!                  \ LAW-High
$0058 over ah!                  \ LAW-Low
$9258 over ah!                  \ Enable linear
ping1
drop $ff $740003c5 ac!          \ DAC mask
$740003cf ac@                   \ reset index
$20 $740003c3 ac!               \ normal operation
;

: clearScreen 96000 0 do 0 71800000 i + ac! loop ;
: whiteScreen 96000 0 do ff 71800000 i + ac! loop ;
: greyScreen 96000 0 do ff 71800000 i + ac! loop ;

: init
mode480p16 mode480p16
$20 $740003c3 ac! ping2 $20 $740003c3 ac! ping2 $20 $740003c3 ac!
;

: addrfix dup not 03 and swap fffffffc and or ;
: filli ffff 0 do i 71800000 i + addrfix ac! loop ;


init
clearScreen


: whiteScreen 25800 0 do ffffffff 71800000 i 4 * + a! loop ;
: clearScreen 25800 0 do 0 71800000 i 4 * + a! loop ;

: highbit 25800 0 do 80008000 71800000 i 4 * + a! loop ;

: fillscreen 25800 0 do dup 71800000 i 4 * + a! loop ;

: testbits
80008000 fillscreen
40004000 fillscreen
20002000 fillscreen
10001000 fillscreen
08000800 fillscreen
04000400 fillscreen
02000200 fillscreen
01000100 fillscreen
00800080 fillscreen
00400040 fillscreen
00200020 fillscreen
00100010 fillscreen
00080008 fillscreen
00040004 fillscreen
00020002 fillscreen
00010001 fillscreen
00000000 fillscreen
;

: testcolors 4b000 0 do i 0ffff and 71800000 i 2 * + ah! loop ;

































: mode101
ping1
$01 $740003c0 ac!               \ enable VGA
$740003cf ac@ drop              \ reset index
$00 $740003c3 ac!               \ disable output
ping1
$700003c2                       \ ATTRIBUTE REGISTERS
$4110 over ah!                  \ AR10: 256 clr, gfx
$0011 over ah!                  \ AR11: border: black
$0f12 over ah!                  \ AR12: enable all planes
$0013 over ah!                  \ AR13: no hoz panning
$0014 over ah!                  \ AR14: no pix padding
ping1
drop $ef $740003c1 ac!          \ misc output
ping1
$700003c6                       \ SEQUENCE REGISTERS
$1000 over ah!                  \ SR00: legacy VGA reset
$0101 over ah!                  \ SR01: 8 char clks; no pix double
$0f02 over ah!                  \ SR02: enable all planes
$0003 over ah!                  \ SR03: font plane
$0e04 over ah!                  \ SR04: 256kB; sequential; chain 4
$0608 over ah!                  \ SR08: unlock SR regs[09:1c]
$000b over ah!                  \ SR0B: color mode 0000
$4018 over ah!                  \ SR18
$0015 over ah!                  \ SR15
ping1
$000b over ah!                  \ vbios extended SR init
$0014 over ah!
$4018 over ah!
ping1
drop $700003d6                  \ CRTC REGISTERS
$700003d6
$0e11 over ah!                  \ CR11: unlock CRTC regs
$4838 over ah!                  \ CR38: unlock CRTC regs[2d:3f]
$a039 over ah!                  \ CR39: unlock CRTC regs[40-ff]
$0140 over ah!                  \ CR40: unlock enhanced regs
ping2
$8000 $700042e8 ah!             \ reset gfx engine
$4000 $700042e8 ah!             \ enable gfx, no irq
ping2
$4838 over ah!                  \ vbios extended crtc init
$a539 over ah!
$0531 over ah!
$4032 over ah!
$0033 over ah!
$0034 over ah!
$0035 over ah!
$053a over ah!
$103c over ah!
$0040 over ah!
$0042 over ah!
$0043 over ah!
$0045 over ah!
$0051 over ah!
$0053 over ah!
$3054 over ah!
$0055 over ah!
$8358 over ah!
$005d over ah!
$005e over ah!
$0f60 over ah!
$0065 over ah!
$0066 over ah!
$0067 over ah!
$0069 over ah!
$006a over ah!
ping2
$5f00 over ah!                  \ CR00: h total
$4f01 over ah!                  \ CR01: h enable end
$5002 over ah!                  \ CR02: h blank start
$0203 over ah!                  \ CR03: h blank end
$5304 over ah!                  \ CR04: h retrace start
$9f05 over ah!                  \ CR05: h retrace end
$0b06 over ah!                  \ CR06: v total
$3e07 over ah!                  \ CR07: overflow
$0008 over ah!                  \ CR08: preset row scan
$4009 over ah!                  \ CR09: max scanline
$000a over ah!                  \ CR0A: cursor start 0
$000b over ah!                  \ CR0B: cursor end 0
$000c over ah!                  \ CR0C: start addr hi
$000d over ah!                  \ CR0D: start addr lo
$ff0e over ah!                  \ CR0E: cursor addr hi
$000f over ah!                  \ CR0F: cursor addr lo
$ea10 over ah!                  \ CR10: v retrace start
$0c11 over ah!                  \ CR11: v retrace end
$df12 over ah!                  \ CR12: v display end
$8013 over ah!                  \ CR13: screen width
$6014 over ah!                  \ CR14: ??
$e715 over ah!                  \ CR15: v blank start
$0416 over ah!                  \ CR16: v blank end
$ab17 over ah!                  \ CR17: 
$ff18 over ah!                  \ CR18: line compare pos
$4050 over ah!                  \ CR50: 640px, 8bpp
$f854 over ah!                  \ CR54
$ff60 over ah!                  \ CR60
$005d over ah!                  \ CR5D
$005e over ah!                  \ CR5E
$0067 over ah!                  \ CR67: color mode 0000
$103a over ah!                  \ CR3A: 256+ color mode
ping1
drop $700003cc                  \ GRAPHICS REGISTERS
$0000 over ah!                  \ GR00: reset data
$0001 over ah!                  \ GR01: reset data
$0002 over ah!                  \ GR02: no color compare
$0003 over ah!                  \ GR03: rotate count 0
$0004 over ah!                  \ GR04: read plane 0
$4005 over ah!                  \ GR05: write mode 0; 256clr
$0506 over ah!                  \ GR06: gfx mode, 64k@a0000
$0f07 over ah!                  \ GR07: color compare all
$ff08 over ah!                  \ GR08: allow write all
ping1
drop $700003c2                  \ ATTRIBUTE REGISTERS
$0000 over ah!                  \ set color palette
$0101 over ah!
$0202 over ah!
$0303 over ah!
$0404 over ah!
$0505 over ah!
$0606 over ah!
$0707 over ah!
$1008 over ah!
$1109 over ah!
$120a over ah!
$130b over ah!
$140c over ah!
$150d over ah!
$160e over ah!
$170f over ah!
ping1
$700003d6                       \ enable linear addressing
$1c53 over ah!                  \ enable MMIO
$fa54 over ah!                  \ endian swap
$0059 over ah!                  \ LAW-High
$0058 over ah!                  \ LAW-Low
$9258 over ah!                  \ Enable linear
ping1
drop $ff $740003c5 ac!          \ DAC mask
$740003cf ac@                   \ reset index
$20 $740003c3 ac!               \ normal operation
;

: init
mode101 mode101 mode101 mode101
$20 $740003c3 ac! $20 $740003c3 ac! $20 $740003c3 ac!
;




: colors
$100 $0 do
i $740003cb ac!
i $3 and $40 * dup . $740003ca ac!
i $1c and 8 * dup . $740003ca ac!
i $c0 and dup . $740003ca ac!
cr
loop
;

\ set palette to 256 greys
: grey256
$ff $0 do
i $740003cb ac!
i $740003ca ac!
i $740003ca ac!
i $740003ca ac!
loop
;








: fillstack 708a7fff 708a0000 do dup i ac! loop ;










: palette16
$01 $740003cb ac!
$00 $740003ca ac!
$00 $740003ca ac!
$00 $740003ca ac!
$02 $740003cb ac!
$9d $740003ca ac!
$9d $740003ca ac!
$9d $740003ca ac!
$03 $740003cb ac!
$ff $740003ca ac!
$ff $740003ca ac!
$ff $740003ca ac!
$04 $740003cb ac!
$be $740003ca ac!
$26 $740003ca ac!
$33 $740003ca ac!
$11 $740003cb ac!
$e0 $740003ca ac!
$6f $740003ca ac!
$8b $740003ca ac!
$12 $740003cb ac!
$49 $740003ca ac!
$3c $740003ca ac!
$2b $740003ca ac!
$13 $740003cb ac!
$a4 $740003ca ac!
$64 $740003ca ac!
$22 $740003ca ac!
$14 $740003cb ac!
$eb $740003ca ac!
$89 $740003ca ac!
$31 $740003ca ac!
$21 $740003cb ac!
$f7 $740003ca ac!
$e2 $740003ca ac!
$6b $740003ca ac!
$22 $740003cb ac!
$2e $740003ca ac!
$48 $740003ca ac!
$4e $740003ca ac!
$23 $740003cb ac!
$44 $740003ca ac!
$89 $740003ca ac!
$1a $740003ca ac!
$24 $740003cb ac!
$a3 $740003ca ac!
$ce $740003ca ac!
$27 $740003ca ac!
$31 $740003cb ac!
$1b $740003ca ac!
$26 $740003ca ac!
$32 $740003ca ac!
$32 $740003cb ac!
$00 $740003ca ac!
$57 $740003ca ac!
$84 $740003ca ac!
$33 $740003cb ac!
$31 $740003ca ac!
$a2 $740003ca ac!
$f2 $740003ca ac!
$34 $740003cb ac!
$b2 $740003ca ac!
$dc $740003ca ac!
$ef $740003ca ac!
;
