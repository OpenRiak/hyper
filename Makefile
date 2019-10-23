.PHONY: compile rel cover test dialyzer
REBAR=./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

cover: test
	$(REBAR) cover

test: compile
	$(REBAR) as test do eunit

dialyzer:
	$(REBAR) dialyzer

xref:
	$(REBAR) as test do xref

check: test dialyzer xref
