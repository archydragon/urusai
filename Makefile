# PROJECT = urusai

.DEFAULT_GOAL := build
.PHONY: exmpp deps build clean

exmpp:
	mkdir -p deps
	sh build-exmpp deps/exmpp

deps: exmpp
	rebar get-deps

build:
	rebar compile

clean:
	rm -rf ebin
