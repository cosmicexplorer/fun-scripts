#!/bin/bash
# Lints files for common style violations

echo "Checking for trailing whitespace...."
# Check for trailing whitespace
egrep -l " +$" $1

if [[ $? -eq 0 ]]; then
    echo "ERROR: One or more files contained trailing whitespace."
else
    echo "No trailing whitespace."
fi

echo "Checking for tab literals..."

# Check for tab literals
grep -l $'\t' $1

if [[ $? -eq 0 ]]; then
    echo "ERROR: One or more files contained tab literals."
else
    echo "No tab literals."
fi

# Remove trailing whitespace into a temporary file
sed 's/[ \t]*$//' $1 > .tmp.$1

which clang-format &> /dev/null

if [[ $? -ne 0 ]]; then
    echo "Error: clang-format is not installed, please execute 'sudo apt-get install clang-format' or use your distribution's package manager."
    exit 1
fi

clang_format_version="$(clang-format --version | grep -o "[[:digit:]]\.[[:digit:]]")"

if [[ ! "$(echo "scale=2; $clang_format_version >= 3.5" | bc)" ]]; then
    echo "Error: clang-format is not recent enough. Please update your version of clang-format to 3.5 or above."
    exit 1
fi

if [ -L $0 ] ; then
    DIR=$(dirname $(readlink -f $0)) ;
else
    DIR=$(dirname $0) ;
fi

cp $DIR/clang-format-cs251 .clang-format
clang-format -style=file .tmp.$1 > .tmp.$1.formatted

echo "************ Differences between formatted & sanitized student code********************"
echo "************ This is meant as a **HINT** for possible style violations, not the end-all-be-all ****************"

diff .tmp.$1 .tmp.$1.formatted

rm -f .tmp.$1* .clang-format
