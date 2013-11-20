DEBUGGER := debugger/*.aj debugger/*.java

AJC := ajc -1.5

RUN := rlwrap aj5

EXAMPLES := $(patsubst %.java,%.class,$(wildcard *.java))
EX_NAMES := $(patsubst %.java,%,$(wildcard *.java))

./%.class: %.java
	$(AJC) $(DEBUGGER) $^

compile: $(EXAMPLES)

define RUN_template
$2:
	$(RUN) $1
endef

$(foreach d,$(EX_NAMES),$(eval $(call RUN_template,$d,$(shell echo $d | sed -r s/\([A-Z][a-z]*\)\([A-Z].*\)/\\1/g | tr A-Z a-z))))

h:
	@echo $(shell echo $(EX_NAMES) | sed -r s/\([A-Z][a-z]*\)\([A-Z].*\)/\\1/g | tr A-Z a-z)

clean:
	/bin/rm -f *.class
	/bin/rm -f debugger/*.class
