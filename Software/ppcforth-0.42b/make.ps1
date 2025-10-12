$src = "ppcforth"

$asdir = "..\..\Utilities\aswcurr"
$bindir = "$asdir\bin"
$incdir = "$asdir\include"
$asl = "$($bindir)\asl.exe"
$p2hex = "$($bindir)\p2hex.exe"
$p2bin = "$($bindir)\p2bin.exe"

write-host $asl

# iex "$asl $src -D rom -D p403gcx -w -L -i $incdir"
iex "$asl $src.asm -D rom -w -L -i $incdir"
#iex "$p2hex $src.p -F moto -r 0x7fe00000-0x7fe80000"
iex "$p2hex $src.p -F moto"
iex "$p2bin $src.p"
