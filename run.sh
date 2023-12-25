#!/bin/sh

exec guile -L $(pwd)/scheme -x $(pwd)/target/debug
