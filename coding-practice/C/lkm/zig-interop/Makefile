KDIR = /lib/modules/`uname -r`/build

kbuild: foo.o
	make -C $(KDIR) M=`pwd`

foo.o:
	zig build-obj -fPIE foo.zig

clean:
	make -C $(KDIR) M=`pwd` clean
