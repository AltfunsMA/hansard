#! /bin/bash



scripts/wrangling/03_clean_downloads.R $main_folder

scripts/wrangling/04_get_hansard_single_orator.R $main_folder $mentions_str &&

scripts/wrangling/05_tkn_split.R ${main_folder}_data/04_model_inputs/${main_folder}_hans_so.csv 300 600 main_text system_id $mentions_str &&

scripts/wrangling/06_find_missing_parties.R &&

scripts/wrangling/07_save_party_alliances.R &&