$src = "vMandelbrot"

$target = "ppc-elf"

$udir = "..\..\Utilities\MinGW-PowerPC-ELF_cilo\ppc"
$bindir = "$udir\bin"

$gcc = "$bindir\$target-gcc.exe"
$make = "$bindir\make.exe"
$objcopy = "$bindir\$target-objcopy.exe"

# $gccop = "-std=c99 -O1 -save-temps -nostdlib -nodefaultlibs"
# $gccop +=" -fno-builtin -mcpu=403 -nostartfiles"
# $gccop +=" -Xlinker --entry=main"

$gccop = "-std=gnu99 -mcpu=403 -nostdlib -nodefaultlibs -Wall -Werror"
$gccop+= " -Xlinker -Map -Xlinker $src.map"
# $gccop+= " -Xlinker -Ttext -Xlinker 0x7fe60000"
# $gccop+= " -Xlinker --entry=main"
$gccop+= " -Xlinker -T -Xlinker linker.ld"
$gccop+= " -save-temps -O1"

$files = "$src.c vga.c pikapc_print.c"

iex "$gcc $($gccop) -o $src.o $files"
iex "$objcopy $src.o -O srec $src.srec"
