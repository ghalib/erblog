#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -sname mochi -boot start_sasl -s reloader -s blog -mnesia dir '"db"'
