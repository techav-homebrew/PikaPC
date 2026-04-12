$src = "vMandelbrot"

$target = "ppc-elf"

$udir = "..\..\Utilities\MinGW-PowerPC-ELF_cilo\ppc"
$bindir = "$udir\bin"
$bindir2= "$udir\$target\bin"

$gcc = "$bindir\$target-gcc.exe"
$gas = "$bindir2\as.exe"
$cpp = "$bindir\$target-cpp.exe"
$gld = "$bindir2\ld.exe"
$make = "$bindir\make.exe"
$objcopy = "$bindir\$target-objcopy.exe"

$files = "vMandelbrot.c"

$gccop = "-std=gnu99 -mcpu=403 -nostdlib -nodefaultlibs -Wall"
$gccop+= " -save-temps -O1 -Xlinker --entry=main"
$gccop+= " -Xlinker -Ttext -Xlinker 0x7fe60000"
$gccop+= " -Xlinker -Map -Xlinker $src.map"
$gccop+= " -Xassembler -a=`"$src.lst`""

$gasop = "-m403 -v --warn -a=`"$src.lst`""

$gldop = "--entry=main -Ttext 0x7fe60000 -Map $src.map"

$objop = "--srec-forceS3 --change-addresses 0x7fe60000"

iex "$gcc $($gccop) $files"
# iex "$gas $($gasop) -o $src.o $src.s"
# iex "$gld $($gldop) $src.o"
iex "$objcopy $objop $src.o -O srec $src.srec"
