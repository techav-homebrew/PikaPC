: wCmd 0x7fc00001 ac! ;                     \ write byte to command register
: rStat 0x7fc00001 ac@ ;                    \ read byte from status register
: wBuf 0x7fc00000 ac! ;                     \ write byte to data register
: rBuf 0x7fc00000 ac@ ;                     \ read byte from data register
: clrBuf rStat 1 and 0 > if                 \ clear data buffer
    rBuf then ;
: waitRx do rStat 1 and 0 > until ;         \ wait for rx bit set
: waitTx do rStat 2 and 0 = until ;         \ wait for tx bit clear
: sendCmd ." SendCmd Start:" .s cr
    do dup .s ." SendCmd:" . cr          \ send command until Ack
    waitTx wCmd waitRx 0xfa until(=) ;
: ctlTest ." Ctrl Self-Test:"               \ ps/2 controller self-test
    clrBuf 0xaa wCmd
    waitRx rBuf . cr ;
: kbdRst ." Keyboard Reset:"                \ keyboard reset
    0xff sendCmd
    waitRx rBuf . cr ;
: mouseCmd 0xd4 wCmd                        \ send command to mouse
    do rStat 2 and 0 > until wBuf ;
: init
    ." Initializing HID" cr
    0xad wCmd 0xa7 wCmd                     \ disable both ports
    ctlTest                                 \ test controller
    ." Enabling Interrupts" cr
    0x37 0x60 wCmd wBuf                     \ enable interrupts
    ." Enabling Keyboard Port" cr
    0xae wCmd                               \ enable port 1
    clrBuf
    kbdRst                                  \ reset keyboard
    clrBuf
    ." Enabling Mouse Port" cr
    0xa8 wCmd                               \ enable mouse port
    ." Resetting Mouse" cr
    0xff mouseCmd                           \ reset mouse
    waitRx clrBuf
    ." Enabling Mouse Reporting" cr
    0xf4 mouseCmd                           \ enable mouse reporting
    waitRx clrBuf
;
: scan
    rStat dup 1 and 0 if(>)
        0x20 and 0 if(>)
            ." Mouse:" rBuf . cr 
            else ." Key:" rBuf . cr
        then
    else drop
    then
;
: scanLp do scan 0 until ;







: statWait do rStat 1 and 0 > until ;
: kInit
    0xad wCmd                               \ disable port 1
    0xa7 wCmd                               \ disable port 2
    clrBuf
    0xaa wCmd                               \ test ps/2 controller
    do rBuf 0xf0 and 0 > until              \ wait for response
    0x37 0x60 wCmd wBuf                     \ enable interrupts
    0xae wCmd                               \ enable port 1
    clrBuf
    0xff wBuf                               \ reset keyboard
    clrBuf
;
: scan
    rStat dup 1 and 0 if(>)
        0x20 and 0 if(>)
            ." Mouse:" rBuf . cr 
            else ." Key:" rBuf . cr
        then
    else drop
    then
;
: scanLp do scan 0 until ;
: mpEnable 0xa8 wCmd ;
: mReset 0xd4 wCmd statWait clrBuf 0xff wBuf statWait clrBuf ;
: mEnable 0xd4 wCmd statWait clrBuf 0xf4 wBuf statWait clrBuf ;

: mRead 0xeb 0xd4 wCmd wBuf ;

