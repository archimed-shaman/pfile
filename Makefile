REBAR:=rebar

.PHONY: all erl test clean doc build_plt check

all: erl

erl:
	$(REBAR) get-deps compile

test: all
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean
	-rm -rvf deps ebin doc .eunit

doc:
	$(REBAR) doc

build_plt:
	(dialyzer --build_plt --output_plt dialyzer.plt --apps erts kernel stdlib crypto mnesia sasl common_test ssl reltool eunit )

# dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl eunit xmerl
check:
	(dialyzer --plt dialyzer.plt -I ./include/ -c ./src/*.erl)