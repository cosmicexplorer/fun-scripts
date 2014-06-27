#!/usr/bin/zsh
du -hsb $@ | cut -f1 | grep -o "[0-9,\.]*"
