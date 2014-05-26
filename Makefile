RIAK_CORE= /media/Data/development/riak_core

all: release

release: ebin
	mkdir -p rel
	./make_release.escript

#ebin: riak_core
ebin:
	erl -pa $(RIAK_CORE)/ebin -make
	cp src/address_book.app.src ebin/address_book.app

riak_core:
	$(MAKE) -C $(RIAK_CORE)

clean:
	rm -rf ebin/*
	rm -rf rel/*

.PHONY: clean ebin release
