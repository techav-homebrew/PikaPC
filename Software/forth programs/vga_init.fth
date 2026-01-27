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
