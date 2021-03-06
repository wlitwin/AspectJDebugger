# Mostly for presenting that's what all the magic at the bottom is for
# default make target builds debugger.jar

AJC := ajc -1.6

RUN := rlwrap aj5

EXAMPLES := $(patsubst %.java,%.class,$(wildcard *.java))
EX_NAMES := $(patsubst %.java,%,$(wildcard *.java))

debugger.jar: debugger/*.java debugger/*.aj debugger/META-INF/aop.xml
	$(AJC) debugger/*.aj debugger/*.java -outjar debugger.jar
	cd debugger;zip -g ../debugger.jar META-INF/aop.xml

compile: debugger.jar $(EXAMPLES)

./%.class: %.java
	$(AJC) $^

define RUN_template
$2: debugger.jar $3
	$(RUN) -cp .:debugger.jar $1
endef

# Converts: VarWatch.class -> Var or Stack.class -> Stack
# Keeps only the first capital word (+ lowercase letters and numbers)
SED := sed -r s/\([A-Z][a-z0-9]*\)\([A-Z]\|\..*\)/\\1/g 

$(foreach d,$(EXAMPLES),$(eval $(call RUN_template,$(patsubst %.class,%,$d),$(shell echo $d | $(SED) | tr A-Z a-z),$d)))

h:
	@echo $(foreach d,$(EXAMPLES),$(shell echo $d | $(SED) | tr A-Z a-z))

clean:
	/bin/rm -f *.class

cleanall: clean
	/bin/rm -f debugger.jar
