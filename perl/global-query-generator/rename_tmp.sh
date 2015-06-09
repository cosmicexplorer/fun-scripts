#!/bin/bash

base_name="$(echo "$1" | sed -e 's/\.tt$//g')"

cp "$1" "$base_name"

echo "$base_name" >> "$2"
