#! /bin/bash

to_find=$1

to_add=$2

exit_error() {

echo "Please input at least the first of these two parameters:"
echo "1. common ending in all the subfolders, e.g. '/results.txt'"
echo "2. desired ending after the last subfolder name, e.g. '_res.txt'"
echo ""
echo "This copies all XX runs in './path/to/runs/runXX/results.txt' to './runXX_res.txt'"
echo ""
echo "If arg 2 is empty the extension is carried over (e.g. 'runXX.txt')."

exit 1

}

[[ -n $to_find ]] || exit_error

[[ -n $to_add ]] || to_add="${to_find##*.}"

vars_to_change=$(find . -type f -name "*$to_find" -printf "/%P\n") 

while read -r FILE ; do 

LAST_DIR=$(basename "$(dirname "$FILE")")

cp ."$FILE" ./"$LAST_DIR""$to_add"

done  <<< "$vars_to_change"