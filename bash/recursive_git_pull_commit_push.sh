#!/bin/bash

# run git pull in all subdirectories of the top directory
# if git status non-empty, add all and commit with message
# "automated commit," and run push
# asks for ssh password at beginning and reuses it as required

INITIAL_WORKING_DIR="$(pwd)"

# COMMIT_MESSAGE has actual quotes in it
COMMIT_MESSAGE="autocommit"

# get password so don't prompt for ssh pass when pushing/pulling
eval "$(ssh-agent -s)" > /dev/null
ssh-add ~/.ssh/id_rsa

# $1: command to run
# $2: acceptable output for command
# $3: command to run if $1 fails
run_quiet_and_cancel_if_failed(){
    COMMAND_OUTPUT="$($1 2>&1)"
    COMMAND_RESULT="$?"
    [ "$2" != "" ] && [ "$(echo $COMMAND_OUTPUT | grep "$2")" != "" ] &&\
        COMMAND_RESULT="0"
    if [ "$COMMAND_RESULT" -ne 0 ]; then
        $3                      # nothing if $3 blank
        echo -e \
            "\n\033[1;31mcommand \"$1\" failed in repository $(pwd):\033[1;0m"
        $1
        $3                      # again, clean up the result of $1
    fi
}

while read -r dir; do
    cd $dir
    cd ..                       # $dir is a .git dir, so move to actual repo dir
    echo -n "updating repository $(pwd)..."
    # stashed so merge abort doesn't explode
    run_quiet_and_cancel_if_failed "git stash" "local changes"
    run_quiet_and_cancel_if_failed "git fetch --all"
    run_quiet_and_cancel_if_failed "git merge -m $COMMIT_MESSAGE" ""\
        "git merge --abort"     # also consider git reset --hard ORIG_HEAD
    run_quiet_and_cancel_if_failed "git stash apply" "No stash found"
    run_quiet_and_cancel_if_failed "git add ."
    run_quiet_and_cancel_if_failed "git commit -m $COMMIT_MESSAGE" \
        "up-to-date"
    run_quiet_and_cancel_if_failed "git push"
    echo "done"
    cd $INITIAL_WORKING_DIR
done <<< "$(find . -type d | grep ".git" | grep "/\.git$")"
# using find like this "flattens out" the recursion, which helps performance (I
# think); consider storing output of find in variable; see if that helps speed
