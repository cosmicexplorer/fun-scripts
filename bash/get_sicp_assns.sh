#!/bin/bash

# pulls all ps and scm files from the programming assignments off of sicp
# there's no reason for this to exist, I just got bored downloading them all by
# hand and wanted a "quicker" solution

# relies on pup the html parser, available at https://github.com/EricChiang/pup

URL=https://mitpress.mit.edu/sicp/psets/

curl 2>/dev/null $URL | \
  pup 'a[href]' | \
  grep -Po "(?<=href=\")+ps.+/.+(?=\")" | \
  while read -r line; do
    echo "$(echo $URL | sed -re 's/\/[^\/]+$/\//g')$line"
  done | \
    while read -r problem; do
      curl 2>/dev/null $problem | \
        pup 'a[href]' | \
        grep -Po "(?<=\")[^\"]+\.(ps|scm)(?=\")" | \
        sed -e "s/^/$(echo $problem | sed -re "s/\/[^\/]+$/\//g" | \
                sed -re "s/\\//\\\\\//g")/g"
    done | \
      while read -r file; do
        curl 2>/dev/null $file > $(echo $file | sed -re "s/.*\///g")
        echo "done with $file"
      done
