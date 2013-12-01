# PROJECT = urusai

.DEFAULT_GOAL := build
.PHONY: exmpp deps build clean

META=.build-meta

exmpp:
	mkdir -p deps
	sh build-exmpp deps/exmpp

deps: exmpp
	rebar get-deps

build:
	date > $(META)
	git rev-parse HEAD >> $(META)
	rebar compile

clean:
	rm -rf ebin
