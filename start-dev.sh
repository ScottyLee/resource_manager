#!/bin/sh
exec erl \
    -pa ebin deps/*/ebin \
    -boot start_sasl \
    -sname resource_manager_dev \
    -s resource_manager \
    -s reloader
