include properties.mk
SOURCE_DIR:=src
INCLUDE_DIR:=include
EBIN_DIR:=ebin
LOG_DIR:=log
CRASH_DUMP:=erl_crash.dump

ERL_CALL:=erl -noshell -noinput -pa $(EBIN_DIR) -s init stop -s

ERL_CALL_VERBOSE:=erl -pa $(EBIN_DIR) -rsh ssh -sname master -s

#other args: 
#+native +"{hipe,[o3,verbose]}" -Ddebug=true +debug_info +no_strict_record_tests
ERLC_OPTS:=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v -Ddebug=true +debug_info

MODULES:=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS:=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(MODULES))

.PHONY:
all:init $(TARGETS)


######## Do the pre-build initialization  #########
.PHONY:
init:
	@mkdir -p $(EBIN_DIR);\
	mkdir -p $(LOG_DIR);\

######## Build the targets if any source file modofied #########

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDE_DIR)/trace.hrl
	erlc $(ERLC_OPTS) -pa $(EBIN_DIR) $<;\



run:all
	$(ERL_CALL_VERBOSE) master run -slave_machines $(REMOTE_SLAVE_MACHINES) -slave_node_name $(SLAVE_NODE_NAME) -log_machine $(REMOTE_LOGGER_MACHINE) -log_node_name $(LOGGER_NODE_NAME)

.PHONY:
clean:
	@rm -r -f $(EBIN_DIR);\
	rm -r -f $(LOG_DIR);\
	rm -r -f $(CRASH_DUMP);\