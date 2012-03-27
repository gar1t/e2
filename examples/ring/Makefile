REBAR := ../../rebar

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

shell: compile
	erl +P 1000000 -pa ebin
