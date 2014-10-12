#!/bin/bash

du -hsb $@ | cut -f1 | grep -o "[0-9,\.]*"
