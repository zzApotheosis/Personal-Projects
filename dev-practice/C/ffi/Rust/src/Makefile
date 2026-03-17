RUSTC = rustc
TARGET = rust-app
CFLAGS = -Wall -g
LDLIBS = # Put libraries here, such as -lssl -lcrypto -lwhatever

OBJECTS = main.o libfoo.a

#.PHONY: clean all

all: $(TARGET)

clean:
	rm -fv $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(LDFLAGS) $(LDLIBS)

%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<

%.a: %.rs
	$(RUSTC) --crate-type staticlib -o $@ $<

