DEBUGGER := debugger/*.aj debugger/*.java

AJC := ajc -1.5

RUN := rlwrap aj5

EXAMPLES := $(patsubst %.java,%.class,$(wildcard *.java))
EX_NAMES := $(patsubst %.java,%,$(wildcard *.java))

./%.class: %.java
	$(AJC) $(DEBUGGER) $^

compile: $(EXAMPLES)

define RUN_template
$2: $3
	$(RUN) $1
endef

# Converts: VarWatch.class -> Var or Stack.class -> Stack
# Keeps only the first capital word
SED := sed -r s/\([A-Z][a-z]*\)\([A-Z]\|\..*\)/\\1/g 

$(foreach d,$(EXAMPLES),$(eval $(call RUN_template,$(patsubst %.class,%,$d),$(shell echo $d | $(SED) | tr A-Z a-z),$d)))

h:
	@echo $(foreach d,$(EXAMPLES),$(shell echo $d | $(SED) | tr A-Z a-z))

clean:
	/bin/rm -f *.class
	/bin/rm -f debugger/*.class
