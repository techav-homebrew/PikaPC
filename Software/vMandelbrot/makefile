
PROJECT=vMandelbrot

# the following options are required to prevent gcc from adding a bunch
# of debugging garbage that is not relevant to this system:
#    -fno-asynchronous-unwind-tables
#    -fomit-frame-pointer
#    -fno-exceptions
#    -fno-unwind-tables
#
CC=gcc
CFLAGS=-std=gnu99 -mcpu=403 -Wall -nostdlib -nodefaultlibs \
       -fno-builtin -save-temps -O1 \
       -fno-asynchronous-unwind-tables \
       -fomit-frame-pointer -fno-exceptions -fno-unwind-tables \
       -Xassembler -a=$(PROJECT).lst

LDFLAGS=-Xlinker --entry=main \
        -Xlinker -T -Xlinker linker.ld \
        -Xlinker -Map -Xlinker $(PROJECT).map

OBJCOPY=objcopy

SOURCES=$(PROJECT).c

OBJECTS=$(SOURCES:.c=.o)

TARGET=$(PROJECT)


.PHONY: all clean


all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJECTS) $(LDFLAGS)
	$(OBJCOPY) $(TARGET) -O srec $(TARGET).srec

.c.o:
	$(CC) $(CFLAGS) -c $<

# $(TARGET): $(OBJECTS)
# 	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

# .PHONY: clean

clean:
	@rm -f $(TARGET) $(OBJECTS) *.i *.s *.map *.lst *.srec
