obj-m += lkm.o
obj-m += lkm2.o

all:
	make -C /lib/modules/`uname -r`/build M="$(PWD)" modules

clean:
	make -C /lib/modules/`uname -r`/build M="$(PWD)" clean

test:
	-sudo rmmod lkm
	-sudo rmmod lkm2
	sudo dmesg -C
	sudo insmod lkm.ko
	sudo insmod lkm2.ko
	sudo dmesg

