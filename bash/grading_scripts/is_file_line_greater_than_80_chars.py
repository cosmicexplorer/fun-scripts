#!/bin/python

import sys
import os

infile_name = sys.argv[1]
infile_path = os.path.expanduser(infile_name)
infile = open(infile_path, "r")

is_longer = False
for line in infile:
    if (len(line) > 80):
        print("yes")
        is_longer = True
        break
if (not is_longer):
    print("no")
