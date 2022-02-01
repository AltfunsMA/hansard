#! /bin/bash
# 14 / 11 / 2019
# this shell script automatically downloads hansard data
# Alfonso Martinez Arranz



this_script=$(basename "$0")

lockfile=/var/lock/${this_script%.sh}.lock

trap "rm -f $lockfile" EXIT

if [[ -e $lockfile  ]]; then

	echo "Lockfile '$lockfile' found. $this_script most likely already running."

	exit

fi
  
echo $$ >> "$lockfile"

echo "$lockfile created by PID: $$ (bash script)"


main_folder=$1

test_run=$2

current_folder=$PWD

cd /data/hansard || exit

finished_full_text=$(ls -l general_data/02_full_text/ | wc -l)

echo $finished_full_text

max_months=1450

attempt=1

while (( finished_full_text < max_months )); do

echo "------------- This is attempt #${attempt} -------------------"

# With timeout every 12 hours 
# The R script seems to neither error nor do its work anymore after a couple of
# days.
# given that the R script also terminates if server takes longer than 60s to
# fulfil requests, this may not be so important.
timeout 43200 scripts/wrangling/01_dl_hansard.R ${main_folder} ${test_run} &

echo "Child PID (R script): $!" 

wait $!

finished_full_text=$(ls -l general_data/02_full_text/ | wc -l)

attempt=$(( attempt + 1 ))

sleep 45 

done



cd $current_folder || exit


