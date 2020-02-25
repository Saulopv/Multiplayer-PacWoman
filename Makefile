.PHONY: play startServer runGUI killServer compile compileJava compileErlang test testErlang testJava doc docErlang docJava clean cleanErlang cleanJava cleanDoc

MAKEFLAGS += --no-print-directory

APPLICATION_NAME:=PacWomen
ERLANG_COOKIE:=bitter
ID?=

# Linux version
#LOCAL_IP:=$(shell ip -f inet addr | grep 'inet' | grep -v '127.0.0.1' | awk {'print $$2'} #| cut -d '/' -f 1)
# Mac version
LOCAL_IP:= 192.168.1.79
IP?=$(LOCAL_IP)

JAVA_SRC_DIR:=src/java_src
JAVA_OUT_DIR:=out/classes
JAVA_DOC_DIR:=doc/java
JAVA_CLASS_PATH:=$(JAVA_OUT_DIR):$(JAVA_SRC_DIR)/OtpErlang.jar
JAVA_TEST_MODULE:=org.ioopm.calculator.Tests

ERLANG_SRC_DIR:=src/erlang
ERLANG_OUT_DIR:=out/ebin
ERLANG_DOC_DIR:=doc/erlang
ERLANG_SRC_FILES:=$(wildcard $(ERLANG_SRC_DIR)/*.erl)
ERLANG_TARGETS:=$(patsubst $(ERLANG_SRC_DIR)/%.erl,$(ERLANG_OUT_DIR)/%.beam,$(ERLANG_SRC_FILES))
SEPARATOR:=,
ERLANG_MODULES:=$(subst $() $(),$(SEPARATOR),$(patsubst $(ERLANG_SRC_DIR)/%.erl,%,$(ERLANG_SRC_FILES)))


# ----------------------------------------------------------------------------
# -------------------------------- Running -----------------------------------
# ----------------------------------------------------------------------------
play: 
	make startServer && make runGUI && make killServer

startServer: compile
	erl -noshell -name pacServer@$(LOCAL_IP) -setcookie $(ERLANG_COOKIE) -pa $(ERLANG_OUT_DIR) -eval 'server:start("$(LOCAL_IP)")' &

runGUI: compileJava
	java -cp $(JAVA_CLASS_PATH) dev/pacwomen/game/Launcher "$(ID)" "$(LOCAL_IP)" "$(IP)"

killServer:
	pidof beam.smp | xargs kill

killServer2:
	ps -e | grep beam | cut -d ' ' -f 2 | xargs kill


# ---------------------------------------------------------------------------
# -------------------------------- Compiling --------------------------------
# ---------------------------------------------------------------------------
compile: compileErlang compileJava

compileErlang: $(ERLANG_TARGETS)

$(ERLANG_OUT_DIR)/%.beam: $(ERLANG_SRC_DIR)/%.erl $(ERLANG_OUT_DIR)
	erlc -o $(ERLANG_OUT_DIR) $<

compileJava: $(JAVA_OUT_DIR)
	find $(JAVA_SRC_DIR) -name '*.java' | xargs javac -cp $(JAVA_CLASS_PATH) -d $(JAVA_OUT_DIR)
	cp -R src/resources/* $(JAVA_OUT_DIR)
	cp $(JAVA_SRC_DIR)/dev/pacwomen/worlds/world.txt $(JAVA_OUT_DIR)/dev/pacwomen/worlds 


# --------------------------------------------------------------------------
# -------------------------------- Testing ---------------------------------
# --------------------------------------------------------------------------
test: compile testErlang testJava

testErlang: compileErlang
	erl -noshell -pa $(ERLANG_OUT_DIR) -eval "eunit:test([$(ERLANG_MODULES)], [verbose])" -s init stop

testJava: compileJava
	java -cp $(JAVA_CLASS_PATH) $(JAVA_TEST_MODULE)


# -------------------------------------------------------------------------------
# -------------------------------- Documentation --------------------------------
# -------------------------------------------------------------------------------
doc: docErlang docJava

docErlang: $(ERLANG_DOC_DIR)
	erl -noshell -pa $(ERLANG_DOC_DIR) -eval "edoc:application('$(APPLICATION_NAME)', '$(ERLANG_SRC_DIR)', [{dir, '$(ERLANG_DOC_DIR)'}])" -s init stop

docJava: $(JAVA_DOC_DIR)
	find $(JAVA_SRC_DIR) -name '*.java' | xargs javadoc -quiet -cp $(JAVA_CLASS_PATH) -d $(JAVA_DOC_DIR)


# --------------------------------------------------------------------------
# -------------------------------- Cleaning --------------------------------
# --------------------------------------------------------------------------
clean: cleanJava cleanErlang cleanDoc

cleanErlang:
	find ./ -name '*.beam' | xargs rm -f
	find ./ -name '*erl_crash.dump' | xargs rm -f

cleanJava:
	rm -rf $(JAVA_OUT_DIR)/*

cleanDoc:
	rm -f $(ERLANG_DOC_DIR)/*.html $(ERLANG_DOC_DIR)/*.css $(ERLANG_DOC_DIR)/edoc-info $(ERLANG_DOC_DIR)/*.png
	rm -rf $(JAVA_DOC_DIR)/*


# ------------------------------------------------------------------------------
# -------------------------------- Directories  --------------------------------
# ------------------------------------------------------------------------------
$(ERLANG_OUT_DIR):
	mkdir $(ERLANG_OUT_DIR)
$(ERLANG_DOC_DIR):
	mkdir $(ERLANG_DOC_DIR)
$(JAVA_OUT_DIR):
	mkdir $(JAVA_OUT_DIR)
$(JAVA_DOC_DIR):
	mkdir $(JAVA_DOC_DIR)
