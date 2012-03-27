REBAR := ../../rebar

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

shell: compile
	erl -pa ebin -s utils

test:
	$(REBAR) test

