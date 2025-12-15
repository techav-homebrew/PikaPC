: textmode                      \ mode 3 (80x25 char)
$01 $710003c3 ac!               \ enable graphics display
$710003cc ac@ drop              \ set index
$710003c0
$10 over ac! $0c over 1 + ac!   \ mode control
$11 over ac! $00 over 1 + ac!   \ overscan
$12 over ac! $0f over 1 + ac!   \ color plane enable
$13 over ac! $08 over 1 + ac!   \ horizontal panning
$14 over ac! $00 over 1 + ac!   \ color select
drop $67 $710003c2 ac!          \ misc output
$710003c4
$01 over ac! $00 over 1 + ac!   \ clock mode
$03 over ac! $00 over 1 + ac!   \ char select
$04 over ac! $07 over 1 + ac!   \ memory mode
drop $710003ce
$05 over ac! $10 over 1 + ac!   \ mode
$06 over ac! $0e over 1 + ac!   \ misc
drop $710003d4
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
drop $ff $710003c6 ac!          \ init DAC mask
;
textmode

: checkid
$38 $710003d4 ac!   \ select CR38
$48 $710003d5 ac!   \ unlock extended crtc regs
$30 $710003d4 ac!   \ select CR30
$710003d5 ac@ . cr  \ read CR30
$30 $710003d4 ac!   \ select CR30
$710003d5 ac@ . cr  \ read CR30
$30 $710003d4 ac!   \ select CR30
$710003d5 ac@ . cr  \ read CR30
$30 $710003d4 ac!   \ select CR30
$710003d5 ac@ . cr  \ read CR30
$30 $710003d4 ac!   \ select CR30
$710003d5 ac@ . cr  \ read CR30
;
checkid
