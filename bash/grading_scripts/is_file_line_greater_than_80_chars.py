#!/bin/python3

# authored by Danny McClanahan
# <daniel.d.mcclanahan@vanderbilt.edu>

import sys
import os

infile_path = os.path.expanduser(sys.argv[1])
infile = open(infile_path, "r")

is_longer = False
for line in infile:
    if (len(line) > 80):
        print("yes")
        is_longer = True
        break
if (not is_longer):
    print("no")
