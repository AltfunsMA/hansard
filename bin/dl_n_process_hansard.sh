#! /bin/bash

# this shell script automatically downloads and pre-processes the first hansard

# 1. Download CSV files from the Hansard search website 
# 2. Create a folder '{hansard_project}/{main_folder}_data/01_records'
# 3. Move the CSV records to that folder and run this script

main_folder=$1

test_run=$2

current_folder=$PWD

cd /data/hansard || exit

scripts/wrangling/01_prep_n_dl_hans_records.R $main_folder $test_run

scripts/wrangling/03_clean_downloads.R $main_folder

scripts/wrangling/04_get_hansard_single_orator.R

scripts/wrangling/05_tkn_split.R

scripts/wrangling/06_find_missing_parties.R

scripts/wrangling/07_save_party_alliances.R

cd $current_folder || exit


