.PHONY: all

FLAGS=$(shell pkg-config --libs --cflags gio-2.0 gio-unix-2.0 glib-2.0)
all: server client
server: server.o testbus.o
	gcc -o $@ $^ $(FLAGS)
client: client.o testbus.o
	gcc -o $@ $^ $(FLAGS)
server.o: server.c
	gcc -o $@ $^ -c $(FLAGS)
client.o: client.c
	gcc -o $@ $^ -c $(FLAGS)
testbus.o: testbus.c
	gcc -o $@ $^ -c $(FLAGS)

clean:
	rm -f *.o server client

# gen stub
gen:
#	gdbus-codegen --generate-c-code testbus --c-namespace TestBus --interface-prefix com.testbus. com.testbus.xml
	gdbus-codegen --generate-c-code testbus --interface-prefix com.testbus. com.testbus.xml

# dbus
introspect:
	gdbus introspect -e -d com.fatminmin -o /com/fatminmin/GDBUS
call:
	gdbus call -e -d com.fatminmin -o /com/fatminmin/GDBUS -m com.fatminmin.GDBUS.HelloWorld fatminmin
