SOURCE_DIR:=src
INCLUDE_DIR:=include
EBIN_DIR:=ebin
YAWS_SERVER_DIR=/home/gik/env/yaws-1.89/yaws-1.89
YAWS_SERVER_INCLUDE_DIR=$(YAWS_SERVER_DIR)/include
YAWS_SRC_DIR:=yaws
YAWS_DIR:=/home/gik/env/dev/yaws_domain
YAWS_EBIN_DIR:=$(YAWS_DIR)/ebin
YAWS_PUBLISH_DIR:=$(YAWS_DIR)/www
APP_DIR:=test1
LOG_DIR:=log
CRASH_DUMP:=erl_crash.dump

#other args: 
#+native +"{hipe,[o3,verbose]}" -Ddebug=true +debug_info +no_strict_record_tests
ERLC_OPTS:=-I $(INCLUDE_DIR) -I $(YAWS_SERVER_INCLUDE_DIR) -o $(EBIN_DIR) -Wall -v -Ddebug=true +debug_info

MODULES:=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS:=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(MODULES))
YAWS_EBINS:=$(patsubst $(SOURCE_DIR)/%.erl, $(YAWS_EBIN_DIR)/%.beam, $(MODULES))

.PHONY:
all: init $(TARGETS)


######## Do the pre-build initialization  #########
.PHONY:
init:
	@mkdir -p $(EBIN_DIR);\
	mkdir -p $(LOG_DIR);\
	mkdir -p $(YAWS_PUBLISH_DIR)/$(APP_DIR);\


######## Build the targets if any source file modofied #########
$(EBIN_DIR)/%.beam:$(SOURCE_DIR)/%.erl $(INCLUDE_DIR)/trace.hrl
	erlc $(ERLC_OPTS) -pa $(EBIN_DIR) $<;\

deploy:all
	@cp -f $(YAWS_SRC_DIR)/*.yaws $(YAWS_PUBLISH_DIR)/$(APP_DIR);\
	cp -f $(TARGETS) $(YAWS_EBIN_DIR);\

undeploy:
	@rm -f $(YAWS_PUBLISH_DIR)/$(APP_DIR)/*;\
	rm  -f $(YAWS_EBINS);\


.PHONY:
clean:
	@rm -r -f $(EBIN_DIR);\
	rm -r -f $(LOG_DIR);\
	rm -r -f $(CRASH_DUMP);\
