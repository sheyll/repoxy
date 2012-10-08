REBAR="./rebar"
SHELL = /bin/sh

.DEFAULT_GOAL := repoxy

.PHONY = compile clean test

repoxy: compile
	$(REBAR) escriptize

compile:
	$(REBAR) get-deps
	$(REBAR) compile

test:
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean
	rm -f repoxy
