SHELL := /bin/bash
.PHONY: all test edoc dialyzer clean proto
PLT=".dialyzer_plt"

all:
	(cd src;$(MAKE))
	(cd src/library;$(MAKE))
	(cd src/game_mod;$(MAKE))
	(cd test;$(MAKE))


proto:
	@(cd scripts; $(SHELL) rebuild_proto.sh)

test: 
	(cd src;$(MAKE) TEST=true)
	(cd src/library;$(MAKE) TEST=true)
	(cd src/game_mod;$(MAKE) TEST=true)
	(cd unittest;$(MAKE) TEST=true)
	(erl -pa ./ebin -eval "eunit:test(\"./ebin/mgee_unittest_all.beam\",[verbose]), init:stop()")

comm_test:
	(mkdir -p ./test/log)
	(run_test -logdir ./test/log -dir ./test/)

edoc: 
	(mkdir -p ./edoc)
	(cd src; $(MAKE) edoc)

plt: 
	(./scripts/gen_plt.sh -a sasl)

dialyzer: clean
	(cd src;$(MAKE) DEBUG=true)
	(dialyzer --plt $(PLT) -Werror_handling -Wrace_conditions  -r .)

clean:
	(cd src;$(MAKE) clean)
	(cd src/library;$(MAKE) clean)
	(cd src/game_mod;$(MAKE) clean)
	(cd unittest;$(MAKE) clean)

