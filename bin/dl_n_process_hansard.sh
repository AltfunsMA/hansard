#! /bin/bash

# this shell script automatically downloads and pre-processes the first hansard

# 1. Download CSV files from the Hansard search website 
# 2. Create a folder '{hansard_project}/{main_folder}_data/01_records'
# 3. Move the CSV records to that folder and run this script

this_script=$(basename "$0")

lockfile=/var/lock/${this_script%.sh}.lock

trap "rm -f $lockfile" EXIT

if [[ -e $lockfile  ]]; then

	echo "Lockfile '$lockfile' found. $this_script most likely already running."

	exit

fi
  
echo $$ >> "$lockfile"

echo "$lockfile created with PID: $$"



main_folder=$1

test_run=$2

mentions_str=$3

current_folder=$PWD

cd /data/hansard || exit

scripts/wrangling/01_prep_n_dl_hans_records.R $main_folder $test_run

scripts/wrangling/03_clean_downloads.R $main_folder &&

scripts/wrangling/04_get_hansard_single_orator.R $main_folder $mentions_str &&

scripts/wrangling/05_tkn_split.R ${main_folder}_data/04_model_inputs/${main_folder}_hans_so.csv 300 600 main_text system_id $mentions_str &&

scripts/wrangling/06_find_missing_parties.R &&

scripts/wrangling/07_save_party_alliances.R &&

cd $current_folder || exit


