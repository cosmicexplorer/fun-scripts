#!/bin/bash

# a script tuned to the Vanderbilt CS 251 SVN server directory layout
# and cmake build system to run a variety of automatic tests on
# assignments and preformat the grading output

# this also calls a couple python scripts, which should be in the same
# directory

# some brittle assignment-specific extras such as looking for which
# files should be in the repository are added under the heading
# "assignment-specific," and should be modified upon each use

## currently running as:
# ~/test_if_compile.sh "Assignment3-1" "Assignment 3-1" "assignment3-1" \
# "/home/cosmicexplorer/CMakeLists.txt" "Danny McClanahan"

## args
# $1: name of assignment folder, e.g. "Assignment3-1"
# $2: formal name of assignment, e.g. "Assignment 3-1"
# $3: name of executable produced by CMakeLists.txt
# $4: path to correct CMakeLists.txt (ABSOLUTE PATH)
# $5: name of grader

## useful commands
# revert all files in directory, recursively
# svn revert -R .
# remove all untracked files in directory, recursively
# svn st | grep '^?' | awk '{print $2}' | xargs rm -rf


# find which student folders have no such assignment folders

ls -d */ | grep -o "[[:alnum:]]*" > all_user_folders

find . -name "$1" | \
	grep -o "/[[:alnum:]]*/" | \
	grep -o "[[:alnum:]]*" >  assn_user_folders

echo -e "\033[1;35mUsers without the assignment directory:\033[1;0m"

num_no_dir="$(~/display_not_found_folders.py all_user_folders assn_user_folders\
								| wc -l)"

~/display_not_found_folders.py all_user_folders assn_user_folders

echo -e "\033[1;34mtotal: $num_no_dir\033[1;0m"

rm all_user_folders
# rm assn_user_folders



# get as much automated checking as possible: compilation/lint/file listing/etc

echo -e "\033[1;35mUsers with errors:\033[1;0m"

num_make_failed=0
# read lines of file assn_user_folders
while read student; do
    echo "---------"

    make_error=""
    makeoutput=""
    makeresult=""
    cd "$student/$1"

    # assignment-specific
    # Assignment 3-1
    file_check_error=""
    # check if correct files are in folder
    # ArrayList.h  ArrayList.tpp  CMakeLists.txt  main.cpp  ScopedArray.h
    if [ "$(ls)" != \
      $'ArrayList.h\nArrayList.tpp\nCMakeLists.txt\nmain.cpp\nScopedArray.h' ];\
	then
        echo -e "\033[0;33mwrong files found: $student\033[1;0m"
        file_check_error="true"
    fi
    if [ -e "CMakeLists.txt" ]; then
        if [ "$(cmp -l "CMakeLists.txt" "$4" 2>&1)" != "" ]; then
	    echo -e "\033[1;30mCMakeLists.txt differs\033[1;0m"
        fi
    else
        echo -e "\033[0;34mCMakeLists.txt not found: $student\033[1;0m"
    fi

    rm -r CMakeFiles 2>/dev/null
    rm CMakeCache.txt 2>/dev/null
    rm cmake_install.cmake 2>/dev/null
    rm Makefile 2>/dev/null
    rm "$3" 2>/dev/null
    cmake . 2>/dev/null 1>/dev/null
    if [ "$?" -ne 0 ]; then
        echo -e "\033[1;36mcmake failure: $student\033[1;0m"
        make_error="CMAKE FAILED"
        ((num_make_failed++))
    else
        make clean
        makeoutput="$(make 2>&1)"
        makeresult="$?"
        if [ "$makeresult" -ne 0 ]; then
            echo -e "\033[1;31mmake failure: $student\033[1;0m"
            make_error="MAKE FAILED"
            ((num_make_failed++))
        elif [ "$(echo $makeoutput | grep "warn")" != "" ]; then
            echo -e "\033[1;33mmake warning: $student\033[1;0m"
            make_error="MAKE WARNED"
            ((num_make_failed++))
        fi
    fi

    rm -r CMakeFiles 2>/dev/null
    rm CMakeCache.txt 2>/dev/null
    rm cmake_install.cmake 2>/dev/null
    rm Makefile 2>/dev/null
    rm "$3" 2>/dev/null

    svn lock ../cs251Grades.txt > /dev/null
    echo -e "\n---------------------------------------------\n" >> \
        ../cs251Grades.txt
    echo "$2 (graded by $5)" >> ../cs251Grades.txt
    echo -e "Total: /100\n" >> ../cs251Grades.txt
    if [ "$make_error" != "" ]; then
        echo -e "\n$make_error\n" >> ../cs251Grades.txt
    fi
    echo -e "(/40) Execution correctness" >> ../cs251Grades.txt
    if [ "$make_error" = "MAKE WARNED" ]; then
        echo -e "[-5] Compiles with warnings.\n" >> ../cs251Grades.txt
    elif [ "$make_error" != "" ]; then
        echo -e "[-40] Program does not compile\n" >> ../cs251Grades.txt
    else
        echo "" >> ../cs251Grades.txt
    fi
    echo -e "(/30) Structure\n" >> ../cs251Grades.txt
    echo -e "(/15) Insightful Programming\n" >> ../cs251Grades.txt
    echo -e "(/15) Appropriate coding and commenting style" >> \
        ../cs251Grades.txt
    # assignment-specific linting
    # Assignment 3-1
    lint_output="$(~/lint_minus_3.5.sh ArrayList.tpp)"
    if [ "$(echo $lint_output | grep "ERROR" | grep "whitespace")" != "" ]; \
        then
        echo -e "[-3]  Trailing whitespace at end of lines." >> \
            ../cs251Grades.txt
    fi
    if [ "$(echo $lint_output | grep "ERROR" | grep "tab")" != "" ]; then
        echo -e "[-3]  Tabs used for indentation instead of spaces." >> \
            ../cs251Grades.txt
    fi
    if [ "$(~/is_file_line_greater_than_80_chars.py ArrayList.tpp)" = "yes" ]; \
        then
        echo -e "[-2]  line(s) greater than 80 characters long" >> \
            ../cs251Grades.txt
    fi
    echo "" >> ../cs251Grades.txt
    # assignment-specific file existence checking
    # Assignment 3-1
    if [ "$file_check_error" != "" ]; then
        echo -e "\n[-5] Incorrect submission: wrong files included\n" >> \
            ../cs251Grades.txt
    fi
    svn unlock ../cs251Grades.txt > /dev/null
    cd ../..
done <assn_user_folders

echo "---------"

echo -e "\033[1;34mtotal: $num_make_failed\033[1;0m"

rm assn_user_folders
