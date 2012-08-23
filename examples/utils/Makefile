REBAR := ../../rebar

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

shell: compile
	erl -pa ebin -s utils -s e2_reloader

test:
	$(REBAR) test
