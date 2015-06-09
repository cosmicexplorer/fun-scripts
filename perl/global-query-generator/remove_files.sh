#!/bin/bash

cat "$1" | while read -r line; do
  rm $line
done
