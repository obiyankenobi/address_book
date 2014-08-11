# This requires rebar to be installed and in the system path.


# Check if Erlang and rebar are installed

ERL=$(shell which erl)
ifeq ($(ERL),)
	$(error "Erlang not available on this system")
endif

REBAR=$(shell which rebar)
ifeq ($(REBAR),)
	$(error "Rebar not available on this system")
endif


all: deps compile


deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) skip_deps=true clean

distclean: clean
	$(REBAR) delete-deps

release: deps compile
	$(REBAR) generate

relclean: clean
	@rm -rf rel/address_book
