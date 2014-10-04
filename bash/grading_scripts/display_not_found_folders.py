#!/bin/python

# authored by Danny McClanahan
# <daniel.d.mcclanahan@vanderbilt.edu>

import sys
import os

allUserFolders = os.path.expanduser(sys.argv[1])
assnUserFolders = os.path.expanduser(sys.argv[2])

allUserFoldersFile = open(allUserFolders, "r")
assnUserFoldersFile = open(assnUserFolders, "r")

for lineAll in allUserFoldersFile:
    assignmentExists = False
    assnUserFoldersFile.seek(0)
    for lineAssn in assnUserFoldersFile:
        if (lineAll == lineAssn):
            assignmentExists = True
    if (not assignmentExists):
        print(lineAll,end="")
