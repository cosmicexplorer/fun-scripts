#!/bin/zsh

# $1: operator
# $2: list of numbers

# bc is multiple-precision so this should be pretty accurate

inputWithOp=$(echo $2 | paste -sd "$1")
echo "scale=2;$inputWithOp" | bc
