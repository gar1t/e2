rebar = ./rebar
scripts = start stop status

compile: deps
	$(rebar) compile

quick:
	$(rebar) compile skip_deps=true

deps:
	$(rebar) get-deps

refresh-deps:
	$(rebar) delete-deps
	$(rebar) get-deps

tests=""

.PHONY: test
test: compile
ifeq ($(tests), "")
	$(rebar) -j1 eunit
else
	$(rebar) -j1 eunit suite=$(tests)
endif

.PHONY: doc
doc:
	$(rebar) doc

clean:
	$(rebar) clean

opts=
shell: compile
	erl -pa ebin $(wildcard deps/*/ebin) -s e2_reloader ${opts}

env-appid:
	@if [ -z "${appid}" ]; then \
	  echo "ERROR: appid is required"; exit 1; fi

env-appdir:
	@if [ -z "${appdir}" ]; then \
	  echo "ERROR: appdir is required"; exit 1; fi

env-module:
	@if [ -z "${module}" ]; then \
	  echo "ERROR: module is required"; exit 1; fi

new-project: env-appid env-appdir
	$(rebar) create template=e2app appid=${appid} dest="${appdir}"
	$(foreach var,$(scripts),chmod 755 ${appdir}/$(var);)

new-service: env-module
	$(rebar) create template=e2service module=${module} dest="$${appdir-.}" skip_deps=true
	@echo "TODO: Add ${module} to a supervisor hierarchy (e.g. *_app file)"

new-task: env-module
	$(rebar) create template=e2task module=${module} dest="$${appdir-.}" skip_deps=true
	@echo "TODO: Add ${module}_sup to a supervisor hierarchy (e.g. *_app file)"
