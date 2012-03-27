REBAR := ../../rebar

compile: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

shell: compile
	ERL_LIBS=deps erl -pa ebin -s logger +W w
