#!/bin/python3

# authored by Danny McClanahan
# <daniel.d.mcclanahan@vanderbilt.edu>

import sys
import os

infile_path = os.path.expanduser(sys.argv[1])
infile = open(infile_path, "r")

line_index = 0
for line in infile:
    line_index = line_index + 1
    # the + 1 and - 1 are because i think it counts the newline as a character
    if (len(line) > 80 + 1):
        print(infile_path + ":" + str(line_index) + " ("
              + str(len(line) - 1) + " chars):" + line)
        break
