SOURCE_DIR:=src
INCLUDE_DIR:=include
EBIN_DIR:=ebin

ERL_CALL:=erl -noshell -noinput -pa $(EBIN_DIR) -s init stop -s

ERL_CALL_VERBOSE:=erl -pa $(EBIN_DIR) -s

#other args: 
#+native +"{hipe,[o3,verbose]}" -Ddebug=true +debug_info +no_strict_record_tests
ERLC_OPTS:=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v -Ddebug=true +debug_info

MODULES:=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS:=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(MODULES))

.PHONY:
all: $(TARGETS)


$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl
	mkdir $(EBIN_DIR);\
	erlc $(ERLC_OPTS) -pa $(EBIN_DIR) $<;\

run:all
	$(ERL_CALL_VERBOSE) systolic_array run_from_make 10

clean:
	@rm -r -f $(EBIN_DIR)
