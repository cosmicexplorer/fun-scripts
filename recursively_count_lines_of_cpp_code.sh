#!/usr/bin/zsh
wc -l $(pwd)/**/*.h $(pwd)/**/*.cpp $(pwd)/**/*.pro | grep -o "[0-9]*" | awk '{s+=$1} END {print s}'
