#!/bin/bash

grep "^|-RecordDecl" "$1" |\
  grep -v "extern" |\
  grep -Po "(?<=struct ).*(?= definition)" |\
  sort |\
  uniq |\
  perl -pe "s/\n$/:\n/g"
