all: clean modules

zig:
	zig build-obj -O ReleaseSmall foo.zig
	touch .foo.o.cmd

clean:
	make -C /lib/modules/`uname -r`/build M="$(PWD)" clean

modules: zig
	make -C /lib/modules/`uname -r`/build M="$(PWD)" modules

test: modules
	-sudo rmmod lkm2
	sudo dmesg -C
	sudo insmod lkm2.ko
	sudo dmesg
