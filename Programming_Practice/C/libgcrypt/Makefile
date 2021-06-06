SOURCES=main.c
HEADERS=main.h
TARGET=main

.PHONY: all clean compile build
.DEFAULT_GOAL: all

all: clean main

clean:
	rm -rfv *.o
	rm -rfv $(TARGET)

main: main.o
	gcc `libgcrypt-config --cflags --libs` -o $(TARGET) *.o

main.o:
	gcc -c $(SOURCES)

test:
	valgrind ./$(TARGET)

