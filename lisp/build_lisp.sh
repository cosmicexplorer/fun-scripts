#!/bin/bash

buildapp --load $1 \
--entry main \
--output "$1_lisp_exe"

chmod +x "$1_lisp_exe"
