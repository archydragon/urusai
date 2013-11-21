#!/bin/sh

if [ -z $1 ]; then
    echo "Please specify your configuration file base name (e.g. 'default' for the file default.config)"
    exit
fi

erl -pa ebin -pa deps/*/ebin -config $1 -s urusai
