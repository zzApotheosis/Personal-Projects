obj-m += lkm.o

all: clean modules

clean:
	make -C /lib/modules/`uname -r`/build M="$(PWD)" clean

modules:
	make -C /lib/modules/`uname -r`/build M="$(PWD)" modules

test:
	-sudo rmmod lkm
	sudo dmesg -C
	sudo insmod lkm.ko
	sudo dmesg

