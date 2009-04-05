#!/bin/sh
cd `dirname $0`
# exec erl -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s blog
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -name mochi -boot start_sasl -s reloader -s blog -mnesia dir '"db"' -kernel inet_dist_listen_min 45000 inet_dist_listen_max 45005

