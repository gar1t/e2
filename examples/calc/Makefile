REBAR := ../../rebar
YRL_SOURCES := $(wildcard *.yrl)
YRL_PARSERS := $(YRL_SOURCES:%.yrl=%.erl)

compile: $(YRL_PARSERS)
	$(REBAR) compile

clean:
	$(REBAR) clean

shell: compile
	@if [ -f priv/calc.config ]; \
	then \
	erl -pa ebin -s calc -config priv/calc; \
	else \
	erl -pa ebin -s calc; \
	fi

test:
	$(REBAR) test

%.erl: %.yrl
	erl -noshell -eval "yecc:file(\"$<\"), init:stop()"
