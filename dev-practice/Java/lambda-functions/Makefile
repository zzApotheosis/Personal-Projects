JC = javac
TARGET = lambda-functions-example
MAINCLASS = App.Main
OBJS = App/Main.class \
	App/util/Util.class
JAR = $(TARGET).jar

.PHONY: clean all

all: $(TARGET)

clean:
	rm -rfv $(OBJS)
	rm -rfv $(JAR)

jar: all
	find -type f -iname '*.class' | xargs jar cvfe $(JAR) $(MAINCLASS)

$(OBJS): %.class: %.java
	$(JC) $<

$(TARGET): $(OBJS)

