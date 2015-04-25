## -*- makefile -*-

######################################################################
## Erlang

ERL := erl
ERLC := $(ERL)c

##INCLUDE_DIRS := ../include $(wildcard ../deps/*/include)
##EBIN_DIRS := $(wildcard ../deps/*/ebin)
ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %)

ifdef DEBUG
  ERLC_FLAGS += +debug_info
endif

ifdef TEST
  ERLC_FLAGS += -DTEST
endif

##EBIN_DIR := ../ebin
DOC_DIR  = ../edoc
EMULATOR := beam

ERL_SOURCES := $(wildcard *.erl)
ERL_MODULES := $(ERL_SOURCES:%.erl=%)
ERL_HEADERS := $(wildcard *.hrl) $(wildcard ./include/*.hrl)
ERL_OBJECTS := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.$(EMULATOR))
ERL_DOCUMENTS = $(ERL_SOURCES:%.erl=$(DOC_DIR)/%.html)
ERL_OBJECTS_LOCAL := $(ERL_SOURCES:%.erl=./%.$(EMULATOR))
APP_FILES := $(wildcard *.app)
EBIN_FILES = $(ERL_OBJECTS) $(APP_FILES:%.app=$(EBIN_DIR)/%.app)
MODULES = $(ERL_SOURCES:%.erl=%)

$(EBIN_DIR)/%.app: %.app
	cp $< $@

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(DOC_DIR)/%.html: %.erl
	$(ERL) -noshell -eval "edoc:files(['$<'],[{dir,\"$(DOC_DIR)\"}]),init:stop()"
