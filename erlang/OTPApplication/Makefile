SOURCE_DIR:=src
EBIN_DIR:=ebin
EUNIT_DIR:=/home/gik/env/otp_src_R14B/lib/eunit/ebin
INCLUDE_DIR:=include
LOG_DIR=log
DATA_DIR=data

INCLUDES:=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES:=$(wildcard $(SOURCE_DIR)/*.erl)
BEAM_TARGETS:=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES))
ALL_TARGETS:=$(BEAM_TARGETS)

#other args: +native +"{hipe,[o3,verbose]}"  +no_strict_record_tests
ERLC_OPTS:=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v +debug_info -Ddebug=true +warn_unused_vars +warn_unused_import -W

ERL_CALL:=erl -noshell -pa $(EBIN_DIR) $(EUNIT_DIR) -boot start_clean -s init stop -s

.PHONY:
all: $(ALL_TARGETS)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl
	erlc $(ERLC_OPTS) -pa $(EBIN_DIR) $<

#$(DEPS_FILE): $(SOURCES) $(INCLUDES)
#	rm -f $@
#	escript generate_deps $(INCLUDE_DIR) $(SOURCE_DIR) \$$\(EBIN_DIR\) $@

#$(EBIN_DIR)/rabbit.app: $(EBIN_DIR)/rabbit_app.in $(BEAM_TARGETS) generate_app
#	escript generate_app $(EBIN_DIR) $@ < $<

clean:
	@rm -f $(EBIN_DIR)/*.beam

cleanLog:
	@rm -f $(LOG_DIR)/*.log

cleandb:
	@rm -rf $(MNESIA_DIR)/*


# Here are the targets to run the apps
# First compile the file(s), then run it

merge_array: all
	$(ERL_CALL) array_test merge_arrays_test

binomial_heap: all
	$(ERL_CALL) binomial_heap_test test

calendar: all
	$(ERL_CALL) calendar_test trace_line "This is just a trace Line!"

leftist_heap: all
	$(ERL_CALL) leftist_heap_tests test

personRecord: all
	$(ERL_CALL) personRecord demo

#Todo: write an eunit test for this thing (for crap sakes)!!
red_black_tree: all
	$(ERL_CALL) red_black_tree createRBTree

run_all: merge_array binomial_heap calendar leftist_heap personRecord
	@echo "All apps just Ran!!"

