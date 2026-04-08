
: mode13
$01 $740003c0 ac!               \ enable VGA
$740003cf ac@ drop              \ reset index
$00 $740003c3 ac!               \ disable output
$700003c2                       \ ATTRIBUTE REGISTERS
$4110 over ah!                  \ AR10: 256 clr, gfx
$0011 over ah!                  \ AR11: border: black
$0f12 over ah!                  \ AR12: enable all planes
$0013 over ah!                  \ AR13: no hoz panning
$0014 over ah!                  \ AR14: no pix padding
drop $63 $740003c1 ac!          \ misc output
$700003c6                       \ SEQUENCE REGISTERS
$0300 over ah!                  \ SR00: legacy VGA reset
$0101 over ah!                  \ SR01: 8 char clks; no pix double
$0f02 over ah!                  \ SR02: enable all planes
$0003 over ah!                  \ SR03: font plane
$0e04 over ah!                  \ SR04: 256kB; sequential; chain 4
$0608 over ah!                  \ SR08: unlock SR regs[09:1c]
drop $700003d6                  \ CRTC REGISTERS
$0e11 over ah!                  \ CR11: unlock CRTC regs
$4838 over ah!                  \ CR38: unlock CRTC regs[2d:3f]
$a039 over ah!                  \ CR39: unlock CRTC regs[40-ff]
$0140 over ah!                  \ CR40: unlock enhanced regs
$8000 $700042e8 ah!             \ reset gfx engine
$4000 $700042e8 ah!             \ enable gfx, no irq
$5f00 over ah!                  \ CR00: h total
$4f01 over ah!                  \ CR01: h enable end
$5002 over ah!                  \ CR02: h blank start
$8203 over ah!                  \ CR03: h blank end
$5404 over ah!                  \ CR04: h retrace start
$8005 over ah!                  \ CR05: h retrace end
$bf06 over ah!                  \ CR06: v total
$1f07 over ah!                  \ CR07: overflow
$0008 over ah!                  \ CR08: preset row scan
$4109 over ah!                  \ CR09: max scanline
$000a over ah!                  \ CR0A: cursor start 0
$000b over ah!                  \ CR0B: cursor end 0
$000c over ah!                  \ CR0C: start addr hi
$000d over ah!                  \ CR0D: start addr lo
$000e over ah!                  \ CR0E: cursor addr hi
$000f over ah!                  \ CR0F: cursor addr lo
$9c10 over ah!                  \ CR10: v retrace start
$8f12 over ah!                  \ CR12: v display end
$2813 over ah!                  \ CR13: screen width
$4014 over ah!                  \ CR14: ??
$9615 over ah!                  \ CR15: v blank start
$b916 over ah!                  \ CR16: v blank end
$a317 over ah!                  \ CR17: 
$ff18 over ah!                  \ CR18: line compare pos
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
drop $700003c2                  \ ATTRIBUTE REGISTERS
$0000 over ah!                  \ set color palette
$0101 over ah!
$0202 over ah!
$0303 over ah!
$0404 over ah!
$0505 over ah!
$0606 over ah!
$0707 over ah!
$0808 over ah!
$0909 over ah!
$0a0a over ah!
$0b0b over ah!
$0c0c over ah!
$0d0d over ah!
$0e0e over ah!
$0f0f over ah!
drop $ff $740003c5 ac!            \ DAC mask
$740003cf ac@                    \ reset index
$20 $740003c3 ac!                 \ normal operation
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

\ fill screen with color 0xff
: clearScreen
$140 $c8 * $0 do
$ff $708a0000 i + ac!
loop
;

decimal

variable I3
variable R3
variable Z1
variable Z2
variable N
variable A
variable B
variable X
variable Y

: mandel
  200 0 do
    i Y !
    10 i * -1000 + I3 !
    320 0 do
      i X !
      i 9 * -2000 + dup R3 ! Z1 !
      I3 @ Z2 !
      0 N !
      256 0 do
        Z1 @ dup * 1000 / A ! Z2 @ dup * 1000 / B !
        A @ B @ + 4000 - 0x7fffffff > if 
        2 Z1 @ * Z2 @ * 1000 / I3 @ + Z2 !
        A @ B @ - R3 @ + Z1 !
        N @ 1 + N !
        then
      loop
      255 N @ - 
      Y @ 320 * X @ + $708a0000 +
      dup $fffffffc and swap not $3 and or ac!
    loop
  loop
;

: init
mode13 mode13 mode13 mode13 mode13
;
