#!/bin/sh
cd `dirname $0`
# exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s blog
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -sname mochi -boot start_sasl -s blog -mnesia dir '"db"'