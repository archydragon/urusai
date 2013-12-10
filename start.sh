#!/bin/sh

if [ -z $1 ]; then
    echo "Please specify your configuration file base name (e.g. 'default' for the file default.config)"
    exit
fi

if [ ! -f "$1.config" ]; then
    echo "'$1.config' not found"
    exit
fi

echo "Starting Urusai using '$1.config'"
erl -detached -heart -pa ebin -pa deps/*/ebin -config $1 -s urusai
