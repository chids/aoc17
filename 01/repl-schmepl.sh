#!/usr/bin/env bash

erlc -DTEST incha.erl
erl -noshell -eval "eunit:test(incha, [verbose])." -run init stop
