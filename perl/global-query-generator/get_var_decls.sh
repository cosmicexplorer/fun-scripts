#!/bin/bash

grep "^.-VarDecl" "$1" |\
  grep -v "extern" |\
  sort |\
  uniq |\
  while read -r line; do
    if [ "$(echo "$line" | grep "':'")" = "" ]; then
      echo $line |\
        grep \
          -Po \
          "[a-zA-Z_][a-zA-Z0-9_]{0,31}+ \'[a-zA-Z_][a-zA-Z0-9_ *]{0,31}+\'" |\
          perl -pe "s/ \'/: /g" |\
          perl -pe "s/\'//g"
    else
      echo -n $line |\
        grep \
          -Po \
          "[a-zA-Z_][a-zA-Z0-9_]{0,31}+ \'" |\
        perl -pe "s/ \'/: /g" |\
        perl -pe "s/\'//g" |\
        perl -p0e "s/\n//g"
      echo $line |\
        grep -Po "(?<=\':\').*(?=\')"
    fi
  done
