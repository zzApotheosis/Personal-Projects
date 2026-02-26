JAVA = java
JAVAC = javac
TARGET = lambda-functions-example
MAINCLASS = App.Main
OBJS = App/Main.class \
	App/util/Util.class
JAR = $(TARGET).jar

.PHONY: clean all run

all: $(TARGET)

clean:
	rm -rfv $(OBJS)
	rm -rfv $(JAR)

run: all
	@$(JAVA) $(MAINCLASS)

jar: all
	find -type f -iname '*.class' | xargs jar cvfe $(JAR) $(MAINCLASS)

$(OBJS): %.class: %.java
	$(JAVAC) $<

$(TARGET): $(OBJS)

