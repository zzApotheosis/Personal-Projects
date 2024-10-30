KDIR = /lib/modules/`uname -r`/build

obj-m += lkm.o
lkm-y += module.o foo.o

all: clean modules

zig:
	zig build-obj -O ReleaseSmall foo.zig
	touch .foo.o.cmd

clean:
	make -C $(KDIR) M=`pwd` clean

modules: zig
	make -C $(KDIR) M=`pwd` modules foo.o
