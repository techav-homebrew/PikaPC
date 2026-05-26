\\\\\\\\\\\\\\ NEW ADDRESSING \\\\\\\\\\\\\\\

: mode13
01 740003c0 ac!                 \ enable VGA
740003cf ac@ drop               \ reset index
00 740003c3 ac!                 \ disable output
700003c2                        \ ATTRIBUTE REGISTERS
4110 over ah!                   \ AR10: 256 clr, gfx
0011 over ah!                   \ AR11: border: black
0f12 over ah!                   \ AR12: enable all planes
0013 over ah!                   \ AR13: no hoz panning
0014 over ah!                   \ AR14: no pix padding
drop 63 740003c1 ac!            \ misc output
700003c6                        \ SEQUENCE REGISTERS
0300 over ah!                   \ SR00: legacy VGA reset
0101 over ah!                   \ SR01: 8 char clks; no pix double
0f02 over ah!                   \ SR02: enable all planes
0003 over ah!                   \ SR03: font plane
0e04 over ah!                   \ SR04: 256kB; sequential; chain 4
0608 over ah!                   \ SR08: unlock SR regs[09:1c]
drop 700003d6                   \ CRTC REGISTERS
0e11 over ah!                   \ CR11: unlock CRTC regs
4838 over ah!                   \ CR38: unlock CRTC regs[2d:3f]
a039 over ah!                   \ CR39: unlock CRTC regs[40-ff]
0140 over ah!                   \ CR40: unlock enhanced regs
8000 700042e8 ah!               \ reset gfx engine
4000 700042e8 ah!               \ enable gfx, no irq
5f00 over ah!                   \ CR00: h total
4f01 over ah!                   \ CR01: h enable end
5002 over ah!                   \ CR02: h blank start
8203 over ah!                   \ CR03: h blank end
5404 over ah!                   \ CR04: h retrace start
8005 over ah!                   \ CR05: h retrace end
bf06 over ah!                   \ CR06: v total
1f07 over ah!                   \ CR07: overflow
0008 over ah!                   \ CR08: preset row scan
4109 over ah!                   \ CR09: max scanline
000a over ah!                   \ CR0A: cursor start 0
000b over ah!                   \ CR0B: cursor end 0
000c over ah!                   \ CR0C: start addr hi
000d over ah!                   \ CR0D: start addr lo
000e over ah!                   \ CR0E: cursor addr hi
000f over ah!                   \ CR0F: cursor addr lo
9c10 over ah!                   \ CR10: v retrace start
8f12 over ah!                   \ CR12: v display end
2813 over ah!                   \ CR13: screen width
4014 over ah!                   \ CR14: ??
9615 over ah!                   \ CR15: v blank start
b916 over ah!                   \ CR16: v blank end
a317 over ah!                   \ CR17: 
ff18 over ah!                   \ CR18: line compare pos
drop 700003cc                   \ GRAPHICS REGISTERS
0000 over ah!                   \ GR00: reset data
0001 over ah!                   \ GR01: reset data
0002 over ah!                   \ GR02: no color compare
0003 over ah!                   \ GR03: rotate count 0
0004 over ah!                   \ GR04: read plane 0
4005 over ah!                   \ GR05: write mode 0; 256clr
0506 over ah!                   \ GR06: gfx mode, 64k@a0000
0f07 over ah!                   \ GR07: color compare all
ff08 over ah!                   \ GR08: allow write all
drop 700003c2                   \ ATTRIBUTE REGISTERS
0000 over ah!                   \ set color palette
0101 over ah!
0202 over ah!
0303 over ah!
0404 over ah!
0505 over ah!
0606 over ah!
0707 over ah!
0808 over ah!
0909 over ah!
0a0a over ah!
0b0b over ah!
0c0c over ah!
0d0d over ah!
0e0e over ah!
0f0f over ah!
drop ff 740003c5 ac!            \ DAC mask
740003cf ac@                    \ reset index
20 740003c3 ac!                 \ normal operation
;
mode13






: textmode                      \ mode 3 (80x25 char)
01 740003c0 ac!                 \ enable VGA
740003cf ac@                    \ reset index
00 740003c3 ac!                 \ disable output
drop 700003c2                   \ ATTRIBUTE REGISTERS
0c10 over ah!                   \ AR10
0011 over ah!                   \ AR11
0f12 over ah!                   \ AR12
0813 over ah!                   \ AR13
0014 over ah!                   \ AR14
67 740003c1 ac!                 \ misc output reg
drop 700003c6                   \ SEQUENCE REGISTERS
0001 over ah!                   \ SR01
0003 over ah!                   \ SR03
0704 over ah!                   \ SR04
drop 700003cc                   \ GRAPHICS REGISTERS
1005 over ah!                   \ GR05
0e06 over ah!                   \ GR06
drop 700003d6                   \ CRTC REGISTERS
8e11 over ah!                   \ CR11: unlock CRTC regs
4838 over ah!                   \ CR38: unlock CRTC regs[2d:3f]
a039 over ah!                   \ CR39: unlock CRTC regs[40-ff]
0140 over ah!                   \ CR40: unlock enhanced regs
5f00 over ah!                   \ CR00: h total
4f01 over ah!                   \ CR01: h enable end
5002 over ah!                   \ CR02: h blank start
8203 over ah!                   \ CR03: h blank end
5504 over ah!                   \ CR04: h retrace start
8105 over ah!                   \ CR05: h retrace end
bf06 over ah!                   \ CR06: v total
1f07 over ah!                   \ CR07: overflow
0008 over ah!                   \ CR08: preset row scan
4f09 over ah!                   \ CR09: max scanline
000a over ah!                   \ CR0A: cursor start 0
000b over ah!                   \ CR0B: cursor end 0
000c over ah!                   \ CR0C: start addr hi
000d over ah!                   \ CR0D: start addr lo
000e over ah!                   \ CR0E: cursor addr hi
000f over ah!                   \ CR0F: cursor addr lo
9c10 over ah!                   \ CR10: v retrace start
8f12 over ah!                   \ CR12: v display end
2813 over ah!                   \ CR13: screen width
1f14 over ah!                   \ CR14: ??
9615 over ah!                   \ CR15: v blank start
b916 over ah!                   \ CR16: v blank end
a317 over ah!                   \ CR17: 
ff18 over ah!                   \ CR18: line compare pos
drop 700003c2                   \ ATTRIBUTE REGISTERS
0000 over ah!                   \ set color palette
0101 over ah!
0202 over ah!
0303 over ah!
0404 over ah!
0505 over ah!
0606 over ah!
0707 over ah!
0808 over ah!
0909 over ah!
0a0a over ah!
0b0b over ah!
0c0c over ah!
0d0d over ah!
0e0e over ah!
0f0f over ah!
20 740003c3 ac!                 \ normal operation
drop ff 740003c5 ac!            \ DAC mask
;
textmode






: textmode                      \ mode 3 (80x25 char)
$01 $740003c0 ac!               \ enable graphics display
$740003cf ac@ drop              \ set index
$740003c3                       \ ATTRIBUTE REGISTERS
$10 over ac! $0c over 1 - ac!   \ AR10: txt, 16 clr, enable line glyphs
                                \       enable blink; no pan
$11 over ac! $00 over 1 - ac!   \ AR11: border clr: black*
$12 over ac! $0f over 1 - ac!   \ color plane enable
$13 over ac! $08 over 1 - ac!   \ horizontal panning
$14 over ac! $00 over 1 - ac!   \ color select
drop $67 $740003c1 ac!          \ misc output
$740003c7
$01 over ac! $00 over 1 - ac!   \ clock mode
$03 over ac! $00 over 1 - ac!   \ char select
$04 over ac! $07 over 1 - ac!   \ memory mode
drop $740003cd
$05 over ac! $10 over 1 - ac!   \ mode
$06 over ac! $0e over 1 - ac!   \ misc
drop $740003d7
$00 over ac! $5f over 1 - ac!   \ h total
$01 over ac! $4f over 1 - ac!   \ h enable end
$02 over ac! $50 over 1 - ac!   \ h blank Start
$03 over ac! $82 over 1 - ac!   \ h blank end
$04 over ac! $55 over 1 - ac!   \ h retrace start
$05 over ac! $81 over 1 - ac!   \ h retrace end
$06 over ac! $bf over 1 - ac!   \ v total
$07 over ac! $1f over 1 - ac!   \ overflow
$08 over ac! $00 over 1 - ac!   \ preset row scan
$09 over ac! $4f over 1 - ac!   \ max scanline
$10 over ac! $9c over 1 - ac!   \ v retrace start
$11 over ac! $8e over 1 - ac!   \ v retrace end
$12 over ac! $8f over 1 - ac!   \ v enable end
$13 over ac! $28 over 1 - ac!   \ logical width
$14 over ac! $1f over 1 - ac!   \ underline loc
$15 over ac! $96 over 1 - ac!   \ v blank start
$16 over ac! $b9 over 1 - ac!   \ v blank end
$17 over ac! $a3 over 1 - ac!   \ mode control
drop $ff $740003c5 ac!          \ init DAC mask
;

: startloadfont
740003cf ac@ drop               \ reset index
00 740003c3 ac!                 \ disable output
0402 700003c6 ah!               \ SR02: write plane 2
0704 700003c6 ah!               \ SR04: 256kB, sequential
0204 700003cc ah!               \ GR04: read plane 2
0005 700003cc ah!               \ GR05: R/W Modes 0
0406 700003cc ah!               \ GR06: txt, A0 normal, A0000
1fff 0 do
i ff and i 708a0000 + ac!
loop
." load font to 708a0000" cr
;

: startloadtext
0302 700003c6 ah!               \ SR02: write planes 0&1
0304 700003c6 ah!               \ SR04: 256kB, even/odd
0004 700003cc ah!               \ GR04: read plane 0
1005 700003cc ah!               \ GR05: even/odd
0e06 700003cc ah!               \ GR06: txt, e/o, B8000
740003cf ac@                    \ reset index
20 740003c3 ac!                 \ normal operation
708bffff 708b8000 do
0 i ac!
loop
." load text to 708b8000" cr
;

: inittext
textmode
startloadfont
startloadtext
;
inittext








: readStrap
4838 740003d6 ah!
a539 740003d6 ah!
36 740003d7 ac! 740003d6 ac@ .
37 740003d7 ac! 740003d6 ac@ .
68 740003d7 ac! 740003d6 ac@ .
6f 740003d7 ac! 740003d6 ac@ .
;
readStrap



















: mode13
$01 $740003c0 ac!               \ enable graphics display
$740003cf ac@ drop              \ set index
$740003c3                       \ ATTRIBUTE REGISTERS
$10 over ac! $41 over 1 - ac!   \ AR10: 256 clr, gfx
$11 over ac! $00 over 1 - ac!   \ AR11: border clr: black
$12 over ac! $0f over 1 - ac!   \ AR12: enable all planes
$13 over ac! $00 over 1 - ac!   \ AR13: no hoz panning
$14 over ac! $00 over 1 - ac!   \ AR14: no pix padding
drop $63 $740003c1 ac!          \ misc output
$740003c7                       \ SEQUENCE REGISTERS
$00 over ac! $03 over 1 - ac!   \ SR00: legacy VGA reset
$01 over ac! $01 over 1 - ac!   \ SR01: 8 char clks; no pix doubl
$02 over ac! $0f over 1 - ac!   \ SR02: enable all planes
$03 over ac! $00 over 1 - ac!   \ SR03: in txt mode: font in plane 2
$04 over ac! $0e over 1 - ac!   \ SR04: 256kB; sequential; chain 4
$740003d7                       \ CRTC REGISTERS
$11 over ac! $0e over 1 - ac!   \ CR11: unlock CRTC regs
$00 over ac! $5f over 1 - ac!   \ CR00: h total
$01 over ac! $4f over 1 - ac!   \ CR01: h enable end
$02 over ac! $50 over 1 - ac!   \ CR02: h blank Start
$03 over ac! $82 over 1 - ac!   \ CR03: h blank end
$04 over ac! $54 over 1 - ac!   \ CR04: h retrace start
$05 over ac! $80 over 1 - ac!   \ CR05: h retrace end
$06 over ac! $bf over 1 - ac!   \ CR06: v total
$07 over ac! $1f over 1 - ac!   \ CR07: overflow
$08 over ac! $00 over 1 - ac!   \ CR08: preset row scan
$09 over ac! $41 over 1 - ac!   \ CR09: max scanline
$0a over ac! $00 over 1 - ac!   \ CR0A: cursor start 0
$0b over ac! $00 over 1 - ac!   \ CR0B: cursor end 0
$0c over ac! $00 over 1 - ac!   \ CR0C: start addr hi
$0d over ac! $00 over 1 - ac!   \ CR0D: start addr lo
$0e over ac! $00 over 1 - ac!   \ CR0E: cursor addr hi
$0f over ac! $00 over 1 - ac!   \ CR0F: cursor addr lo
$10 over ac! $9c over 1 - ac!   \ CR10: v retrace start
$12 over ac! $8f over 1 - ac!   \ CR12: v display end
$13 over ac! $28 over 1 - ac!   \ CR13: screen width
$14 over ac! $40 over 1 - ac!   \ CR14: Doubleword RAM <--???
$15 over ac! $96 over 1 - ac!   \ CR15: v blank start
$16 over ac! $b9 over 1 - ac!   \ CR16: v blank end
$17 over ac! $a3 over 1 - ac!   \ CR17: en sync; word mode; addr wrap
                                \       byte mode addressing;
                                \       h retrace clk; bank 4 mode
                                \       bank 2 mode
$18 over ac! $ff over 1 - ac!   \ CR18: line compare pos
$740003cd                       \ GRAPHICS REGISTERS
$00 over ac! $00 over 1 - ac!   \ GR00: reset data
$01 over ac! $00 over 1 - ac!   \ GR01: reset data
$02 over ac! $00 over 1 - ac!   \ GR02: color compare
$03 over ac! $00 over 1 - ac!   \ GR03: rotate count; NOP
$04 over ac! $00 over 1 - ac!   \ GR04: read plane select 0
$05 over ac! $40 over 1 - ac!   \ GR05: write mode 0; read from planes
                                \       std addressing; normal shift;
                                \       256 color mode
$06 over ac! $05 over 1 - ac!   \ GR06: gfx mode; A0 unchanged;
                                \       $a0000-$affff mem map
$07 over ac! $0f over 1 - ac!   \ GR07: color compare all
$08 over ac! $ff over 1 - ac!   \ GR08: allow write all bits
$740003c3                       \ ATTRIBUTE REGISTERS
$00 over ac! $00 over 1 - ac!   \ set color palette
$01 over ac! $01 over 1 - ac!
$02 over ac! $02 over 1 - ac!
$03 over ac! $03 over 1 - ac!
$04 over ac! $04 over 1 - ac!
$05 over ac! $05 over 1 - ac!
$06 over ac! $06 over 1 - ac!
$07 over ac! $07 over 1 - ac!
$08 over ac! $08 over 1 - ac!
$09 over ac! $09 over 1 - ac!
$0a over ac! $0a over 1 - ac!
$0b over ac! $0b over 1 - ac!
$0c over ac! $0c over 1 - ac!
$0d over ac! $0d over 1 - ac!
$0e over ac! $0e over 1 - ac!
$0f over ac! $0f over 1 - ac!
$ff 740003c5 ac!                \ DAC mask
;
mode13

\ set palette to 256 greys
: grey256
$ff $0 do
i $740003cb ac!
i $740003ca ac!
i $740003ca ac!
i $740003ca ac!
loop
;
grey256

: fillplanea5
$708a7fff $708a0000 do
$a5 i ac!
loop
;
fillplanea5











\\\\\\\\\\\\\\ OLD ADDRESSING \\\\\\\\\\\\\\
\ initialize Mode 13 (complete)
: mode13
$01 $740003c3 ac!               \ enable graphics display
$740003cc ac@ drop              \ set index
$740003c0                       \ ATTRIBUTE REGISTERS
$10 over ac! $41 over 1 + ac!   \ AR10: 256 clr, gfx
$11 over ac! $00 over 1 + ac!   \ AR11: border clr: black
$12 over ac! $0f over 1 + ac!   \ AR12: enable all planes
$13 over ac! $00 over 1 + ac!   \ AR13: no hoz panning
$14 over ac! $00 over 1 + ac!   \ AR14: no pix padding
drop $63 $740003c2 ac!          \ misc output
$740003c4                       \ SEQUENCE REGISTERS
$00 over ac! $03 over 1 + ac!   \ SR00: legacy VGA reset
$01 over ac! $01 over 1 + ac!   \ SR01: 8 char clks; no pix doubl
$02 over ac! $0f over 1 + ac!   \ SR02: enable all planes
$03 over ac! $00 over 1 + ac!   \ SR03: in txt mode: font in plane 2
$04 over ac! $0e over 1 + ac!   \ SR04: 256kB; sequential; chain 4
$740003d4                       \ CRTC REGISTERS
$11 over ac! $0e over 1 + ac!   \ CR11: unlock CRTC regs
$00 over ac! $5f over 1 + ac!   \ CR00: h total
$01 over ac! $4f over 1 + ac!   \ CR01: h enable end
$02 over ac! $50 over 1 + ac!   \ CR02: h blank Start
$03 over ac! $82 over 1 + ac!   \ CR03: h blank end
$04 over ac! $54 over 1 + ac!   \ CR04: h retrace start
$05 over ac! $80 over 1 + ac!   \ CR05: h retrace end
$06 over ac! $bf over 1 + ac!   \ CR06: v total
$07 over ac! $1f over 1 + ac!   \ CR07: overflow
$08 over ac! $00 over 1 + ac!   \ CR08: preset row scan
$09 over ac! $41 over 1 + ac!   \ CR09: max scanline
$0a over ac! $00 over 1 + ac!   \ CR0A: cursor start 0
$0b over ac! $00 over 1 + ac!   \ CR0B: cursor end 0
$0c over ac! $00 over 1 + ac!   \ CR0C: start addr hi
$0d over ac! $00 over 1 + ac!   \ CR0D: start addr lo
$0e over ac! $00 over 1 + ac!   \ CR0E: cursor addr hi
$0f over ac! $00 over 1 + ac!   \ CR0F: cursor addr lo
$10 over ac! $9c over 1 + ac!   \ CR10: v retrace start
$12 over ac! $8f over 1 + ac!   \ CR12: v display end
$13 over ac! $28 over 1 + ac!   \ CR13: screen width
$14 over ac! $40 over 1 + ac!   \ CR14: Doubleword RAM <--???
$15 over ac! $96 over 1 + ac!   \ CR15: v blank start
$16 over ac! $b9 over 1 + ac!   \ CR16: v blank end
$17 over ac! $a3 over 1 + ac!   \ CR17: en sync; word mode; addr wrap
                                \       byte mode addressing;
                                \       h retrace clk; bank 4 mode
                                \       bank 2 mode
$18 over ac! $ff over 1 + ac!   \ CR18: line compare pos
$740003ce                       \ GRAPHICS REGISTERS
$00 over ac! $00 over 1 + ac!   \ GR00: reset data
$01 over ac! $00 over 1 + ac!   \ GR01: reset data
$02 over ac! $00 over 1 + ac!   \ GR02: color compare
$03 over ac! $00 over 1 + ac!   \ GR03: rotate count; NOP
$04 over ac! $00 over 1 + ac!   \ GR04: read plane select <--???
$05 over ac! $40 over 1 + ac!   \ GR05: write mode 0; read from planes
                                \       std addressing; normal shift;
                                \       256 color mode
$06 over ac! $05 over 1 + ac!   \ GR06: gfx mode; A0 unchanged;
                                \       $a0000-$affff mem map
$07 over ac! $0f over 1 + ac!   \ GR07: color compare all
$08 over ac! $ff over 1 + ac!   \ GR08: allow write all bits
$740003c0                       \ ATTRIBUTE REGISTERS
$00 over ac! $00 over 1 + ac!   \ set color palette
$01 over ac! $01 over 1 + ac!
$02 over ac! $02 over 1 + ac!
$03 over ac! $03 over 1 + ac!
$04 over ac! $04 over 1 + ac!
$05 over ac! $05 over 1 + ac!
$06 over ac! $06 over 1 + ac!
$07 over ac! $07 over 1 + ac!
$08 over ac! $08 over 1 + ac!
$09 over ac! $09 over 1 + ac!
$0a over ac! $0a over 1 + ac!
$0b over ac! $0b over 1 + ac!
$0c over ac! $0c over 1 + ac!
$0d over ac! $0d over 1 + ac!
$0e over ac! $0e over 1 + ac!
$0f over ac! $0f over 1 + ac!
;
mode13

\ set palette to 256 greys
: grey256
$ff $0 do
i $740003c8 ac!
i $740003c9 ac!
i $740003c9 ac!
i $740003c9 ac!
loop
;
grey256



\ test words

: writeall
$04 $740003ce ac! $00 $740003cf ac!  \ read planar 0
$02 $740003c4 ac! $0f $740003cf ac!  \ write planar all
;

: plane0
$04 $740003ce ac! $00 $740003cf ac!  \ read planar 0
$02 $740003c4 ac! $01 $740003cf ac!  \ write planar 0
;
: plane1
$04 $740003ce ac! $01 $740003cf ac!  \ read planar 1
$02 $740003c4 ac! $02 $740003cf ac!  \ write planar 1
;
: plane2
$04 $740003ce ac! $02 $740003cf ac!  \ read planar 2
$02 $740003c4 ac! $04 $740003cf ac!  \ write planar 2
;
: plane3
$04 $740003ce ac! $03 $740003cf ac!  \ read planar 3
$02 $740003c4 ac! $08 $740003cf ac!  \ write planar 3
;
: fillplane
$700a7fff $700a0000 do
$00 i ac!
loop
;
: read16
$10 0 do
i 4 * $708a0000 + a@ .
loop
;
: fillplanea5
$700a7fff $700a0000 do
$a5 i ac!
loop
;
: fillplaneff
$700a7fff $700a0000 do
$ff i ac!
loop
;
: filllong00
$700affff $700a0000 do
$00 i ac!
loop
;
: filllongff 
$700affff $700a0000 do
$ff i ac!
loop
;
: filllonga5 
$700affff $700a0000 do
$a5 i ac!
loop
;
: filllongi 
$700affff $700a0000 do
i $ff and i ac!
loop
;
: filladdr
$700afffc 4 / $700a0000 4 / do
i 4 * dup dup dup a! . a@ . cr
loop
;


\ slow down M-Clock

: slowmclk                      \ roughly 17MHz
$01 $740003c3 ac!               \ enable graphics display
$740003c4
$08 over ac! $06 over 1 + ac!   \ unlock ext. seq. regs
$10 over ac! $62 over 1 + ac!   \ set MCLK N,R
$11 over ac! $24 over 1 + ac!   \ set MCLK M
$15 over ac! $05 over 1 + ac!   \ load MCLK PLL
$15 over ac! $04 over 1 + ac!   \ finish load
drop
;

: mclk25
$01 $740003c3 ac!               \ enable graphics display
$740003c4
$08 over ac! $06 over 1 + ac!   \ unlock ext. seq. regs
$10 over ac! $66 over 1 + ac!   \ set MCLK N,R
$11 over ac! $6e over 1 + ac!   \ set MCLK M
$15 over ac! $05 over 1 + ac!   \ load MCLK PLL
$15 over ac! $04 over 1 + ac!   \ finish load
drop
;

: mclk33
$01 $740003c3 ac!               \ enable graphics display
$740003c4
$08 over ac! $06 over 1 + ac!   \ unlock ext. seq. regs
$10 over ac! $61 over 1 + ac!   \ set MCLK N,R
$11 over ac! $36 over 1 + ac!   \ set MCLK M
$15 over ac! $05 over 1 + ac!   \ load MCLK PLL
$15 over ac! $04 over 1 + ac!   \ finish load
drop
;


\ quick init modes

: textmode                      \ mode 3 (80x25 char)
$01 $740003c3 ac!               \ enable graphics display
$740003cc ac@ drop              \ set index
$740003c0                       \ ATTRIBUTE REGISTERS
$10 over ac! $0c over 1 + ac!   \ AR10: txt, 16 clr, enable line glyphs
                                \       enable blink; no pan
$11 over ac! $00 over 1 + ac!   \ AR11: border clr: black*
$12 over ac! $0f over 1 + ac!   \ color plane enable
$13 over ac! $08 over 1 + ac!   \ horizontal panning
$14 over ac! $00 over 1 + ac!   \ color select
drop $67 $740003c2 ac!          \ misc output
$740003c4
$01 over ac! $00 over 1 + ac!   \ clock mode
$03 over ac! $00 over 1 + ac!   \ char select
$04 over ac! $07 over 1 + ac!   \ memory mode
drop $740003ce
$05 over ac! $10 over 1 + ac!   \ mode
$06 over ac! $0e over 1 + ac!   \ misc
drop $740003d4
$00 over ac! $5f over 1 + ac!   \ h total
$01 over ac! $4f over 1 + ac!   \ h enable end
$02 over ac! $50 over 1 + ac!   \ h blank Start
$03 over ac! $82 over 1 + ac!   \ h blank end
$04 over ac! $55 over 1 + ac!   \ h retrace start
$05 over ac! $81 over 1 + ac!   \ h retrace end
$06 over ac! $bf over 1 + ac!   \ v total
$07 over ac! $1f over 1 + ac!   \ overflow
$08 over ac! $00 over 1 + ac!   \ preset row scan
$09 over ac! $4f over 1 + ac!   \ max scanline
$10 over ac! $9c over 1 + ac!   \ v retrace start
$11 over ac! $8e over 1 + ac!   \ v retrace end
$12 over ac! $8f over 1 + ac!   \ v enable end
$13 over ac! $28 over 1 + ac!   \ logical width
$14 over ac! $1f over 1 + ac!   \ underline loc
$15 over ac! $96 over 1 + ac!   \ v blank start
$16 over ac! $b9 over 1 + ac!   \ v blank end
$17 over ac! $a3 over 1 + ac!   \ mode control
drop $ff $740003c6 ac!          \ init DAC mask
;

: mode13
$01 $740003c3 ac!               \ enable graphics display
$740003cc ac@ drop              \ set index
$740003c0
$10 over ac! $41 over 1 + ac!   \ mode control
$11 over ac! $00 over 1 + ac!   \ overscan
$12 over ac! $0f over 1 + ac!   \ color plane enable
$13 over ac! $00 over 1 + ac!   \ horizontal panning
$14 over ac! $00 over 1 + ac!   \ color select
drop $63 $740003c2 ac!          \ misc output
$740003c4
$01 over ac! $01 over 1 + ac!   \ clock mode
$03 over ac! $00 over 1 + ac!   \ char select
$04 over ac! $0e over 1 + ac!   \ memory mode
drop $740003ce
$05 over ac! $40 over 1 + ac!   \ mode
$06 over ac! $05 over 1 + ac!   \ misc
drop $740003d4
$00 over ac! $5f over 1 + ac!   \ h total
$01 over ac! $4f over 1 + ac!   \ h enable end
$02 over ac! $50 over 1 + ac!   \ h blank Start
$03 over ac! $82 over 1 + ac!   \ h blank end
$04 over ac! $54 over 1 + ac!   \ h retrace start
$05 over ac! $80 over 1 + ac!   \ h retrace end
$06 over ac! $bf over 1 + ac!   \ v total
$07 over ac! $1f over 1 + ac!   \ overflow
$08 over ac! $00 over 1 + ac!   \ preset row scan
$09 over ac! $41 over 1 + ac!   \ max scanline
$10 over ac! $9c over 1 + ac!   \ v retrace start
$11 over ac! $8e over 1 + ac!   \ v retrace end
$12 over ac! $8f over 1 + ac!   \ v enable end
$13 over ac! $28 over 1 + ac!   \ logical width
$14 over ac! $40 over 1 + ac!   \ underline loc
$15 over ac! $96 over 1 + ac!   \ v blank start
$16 over ac! $b9 over 1 + ac!   \ v blank end
$17 over ac! $a3 over 1 + ac!   \ mode control
drop $ff $740003c6 ac!          \ init DAC mask
;



: startloadfont
$05 $740003ce ac!               \ clear even/odd mode
$06 $740003ce ac!               \ select gfx register 6
$04 $740003cf ac!               \ map VGA memory to $0a0000
$02 $740003c4 ac!               \ index 2
$04 $740003c5 ac!               \ set bitplane 2
$04 $740003c4 ac!               \ clear even/odd mode another way
$06 $740003c5 ac!               \
$04 $740003ce ac! $02 $740003cf ac!  \ read planar 2
;

: startloadtext
$02 $740003c4 ac!               \ restore normal operation
$03 $740003c5 ac!
$04 $740003c4 ac!
$02 $740003c5 ac!
$05 $740003ce ac!
$10 $740003cf ac!
$06 $740003ce ac!
$0e $740003cf ac!
;


: clearall
$05 $740003ce ac!  \ GR05
$00 $740003cf ac!  \ normal?
$06 $740003ce ac!  \ GR06
$00 $740003cf ac!  \ 128k seq txt
$02 $740003c4 ac!  \ SR02
$0f $740003c5 ac!  \ all planes
$04 $740003c4 ac!  \ SR04
$06 $740003c5 ac!  \ seq 256kB
$700a7fff $700a0000 do 
i $0ff and i ac!
i ac@ .
loop
;
clearall

: clearall8
$05 $740003ce ac!  \ GR05
$00 $740003cf ac!  \ normal?
$06 $740003ce ac!  \ GR06
$00 $740003cf ac!  \ 128k seq txt
$02 $740003c4 ac!  \ SR02
$0f $740003c5 ac!  \ all planes
$04 $740003c4 ac!  \ SR04
$06 $740003c5 ac!  \ seq 256kB
$708a7fff $708a0000 do 
i $0ff and i ac!
i ac@ .
loop
;
clearall8


: trioID
$38 $740003d4 ac! $48 $740003d5 ac!
$39 $740003d4 ac! $a5 $740003d5 ac!
$2d $740003d4 ac! $740003d5 ac@ . cr
$2e $740003d4 ac! $740003d5 ac@ . cr
$2f $740003d4 ac! $740003d5 ac@ . cr
$30 $740003d4 ac! $740003d5 ac@ . cr
;
trioID

: virgeID
$38 $740003d4 ac! $48 $740003d5 ac!
$39 $740003d4 ac! $a5 $740003d5 ac!
$2d $740003d4 ac! $740003d5 ac@ . cr
$2e $740003d4 ac! $740003d5 ac@ . cr
$2f $740003d4 ac! $740003d5 ac@ . cr
$30 $740003d4 ac! $740003d5 ac@ . cr
;
virgeID

: readStrap
$38 $740003d4 ac! $48 $740003d5 ac!
$39 $740003d4 ac! $a5 $740003d5 ac!
$36 $740003d4 ac! $740003d5 ac@ .
$37 $740003d4 ac! $740003d5 ac@ .
$68 $740003d4 ac! $740003d5 ac@ .
$6f $740003d4 ac! $740003d5 ac@ .
;
readStrap

: clearbmp
$05 $740003ce ac!  \ GR05
$00 $740003cf ac!  \ normal?
$06 $740003ce ac!  \ GR06
$01 $740003cf ac!  \ 128k seq gfx
$02 $740003c4 ac!  \ SR02
$0f $740003c5 ac!  \ all planes
$04 $740003c4 ac!  \ SR04
$06 $740003c5 ac!  \ seq 256kB
$700a7fff $700a0000 do $ff i ac! loop
;
clearbmp
