#
# Targets
#

REBAR=`which rebar`

.PHONY: build deps

all: deps build

clean:
	rm -rf apps/*/ebin
	rm -rf apps/*/log
	rm -rf apps/*/logs
	$(REBAR) skip_deps=true clean

deps:
	$(REBAR) get-deps

build:
	$(REBAR) compile
	$(MAKE) xref

package: all
	rm -rf rel/package
	$(REBAR) generate -f

doc:
	$(REBAR) skip_deps=true doc

#
# Tests
#

unit: build
	rm -rf apps/*/.eunit
	$(REBAR) eunit skip_deps=true suite=$(T)

integration: build
	$(REBAR) ct skip_deps=true suites=$(T)

test: unit integration

#
# Run
#

DEPS=deps/*/ebin
ERL=exec erl -pa apps/totochtin/ebin $(DEPS) -sname totochtin

.PHONY: boot noboot

console: package
	rel/package/bin/totochtin console

boot: build
	$(ERL) -s totochtin

noboot: build
	$(ERL)

#
# Analysis
#

PLT=./plt/R15B.plt

WARNINGS=-Werror_handling \
  -Wrace_conditions \
  -Wunderspecs \
  -Wunmatched_returns

APPS=kernel stdlib sasl erts ssl \
  tools os_mon runtime_tools crypto \
  inets xmerl webtool snmp public_key \
  mnesia eunit syntax_tools compiler

build-plt: all
	dialyzer --build_plt --output_plt $(PLT) \
	  --apps $(APPS) $(DEPS)

dialyzer: build
	dialyzer apps/*/ebin --plt $(PLT) $(WARNINGS) \
	  | grep -v 'lager_not_running'

xref:
	$(REBAR) skip_deps=true xref

typer: build
	typer --annotate --plt $(PLT) -I deps/ -I apps/totochtin/ -r apps/
