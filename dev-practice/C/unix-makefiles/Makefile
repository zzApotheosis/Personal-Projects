TARGET = my-app
CFLAGS = -Wall -g
LDLIBS = # Put libraries here, such as -lssl -lcrypto -lwhatever

OBJECTS = main.o app.o

all: $(TARGET)

clean:
	rm -fv $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(LDFLAGS)

$(OBJECTS): %.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<


