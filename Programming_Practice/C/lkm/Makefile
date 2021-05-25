obj-m += lkm.o
obj-m += lkm2.o

all: clean modules

clean:
	make -C /lib/modules/`uname -r`/build M="$(PWD)" clean

modules:
	make -C /lib/modules/`uname -r`/build M="$(PWD)" modules

test:
	-sudo rmmod lkm
	-sudo rmmod lkm2
	sudo dmesg -C
	sudo insmod lkm.ko
	sudo insmod lkm2.ko
	sudo dmesg

