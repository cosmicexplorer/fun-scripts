#!/bin/sh

host="$1"
wd="$2"
num_pings="$3"
outf=\$$4

cd "$wd"

ping -c "$num_pings" "$host" >> "$outf"
