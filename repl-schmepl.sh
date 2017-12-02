#!/usr/bin/env bash

erlc -DTEST ${1}/${2}.erl
erl -noshell -eval "eunit:test(${2}, [verbose])." -run init stop
