#
# LibHotSwap Makefile
#
ERL=$(shell which erl)
REBAR=$(shell which rebar || echo $(CURDIR)/bin/rebar)
DEPSOLVER_PLT=$(CURDIR)/.depsolver.plt
DIALYZER_WARN=-Wrace_conditions -Werror_handling -Wunmatched_returns -Wunderspecs
DIALYZER_ARGS=--plt $(DEPSOLVER_PLT) --no_check_plt $(DIALYZER_WARN)
TYPER_ARGS=--plt $(DEPSOLVER_PLT)
USED_APPS=kernel stdlib erts syntax_tools compiler crypto hipe

.PHONY: libhotswap docs testall eunit dialyzer typer clean distclean

##BUILDING
libhotswap:
	$(REBAR) compile

docs: 
	$(REBAR) doc

##TESTING
testall: libhotswap eunit dialyzer typer

eunit:
	-$(REBAR) eunit

$(DEPSOLVER_PLT):
	dialyzer --output_plt $(DEPSOLVER_PLT) --build_plt --apps $(USED_APPS)

dialyzer: $(DEPSOLVER_PLT)
	dialyzer $(DIALYZER_ARGS) --src ./src -I ./include

typer: $(DEPSOLVER_PLT)
	typer $(TYPER_ARGS) -r ./src -I ./include

##CLEANING
clean:
	-$(REBAR) clean

distclean: clean
	-rm $(DEPSOLVER_PLT)


