JC = javac
TARGET = app
MAINCLASS = App.Main
SRCDIR = src
OBJS = $(SRCDIR)/App/Main.class \
	$(SRCDIR)/App/util/Util.class
JAR = $(TARGET).jar

.PHONY: clean all

all: $(TARGET)

clean:
	rm -rfv $(OBJS)
	rm -rfv $(JAR)

jar: all
	cd $(SRCDIR) && find -type f -iname '*.class' | xargs jar cvfe $(JAR) $(MAINCLASS)
	mv -fv "$(SRCDIR)/$(JAR)" .

$(OBJS): %.class: %.java
	$(JC) $<

$(TARGET): $(OBJS)

