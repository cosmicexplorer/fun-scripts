#!/bin/bash

base_name_ttmp="$(echo "$1" | sed -e 's/\.c/\.ttmp/g')"

rm "$base_name_ttmp"
