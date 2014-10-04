#!/bin/python

import sys

allUserFolders = sys.argv[1]
assnUserFolders = sys.argv[2]

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
