RUSTC = rustc
TARGET = rust-app
CFLAGS = -Wall -g
LDLIBS = # Put libraries here, such as -lssl -lcrypto -lwhatever

OBJECTS = main.o
RUST_ARCHIVES = foo.a

.PHONY: clean all

all: $(TARGET)

clean:
	rm -fv $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS) $(RUST_ARCHIVES)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(RUST_ARCHIVES) $(LDFLAGS) $(LDLIBS)

%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<

%.a: %.rs
	$(RUSTC) --crate-type staticlib -o $@ $<

