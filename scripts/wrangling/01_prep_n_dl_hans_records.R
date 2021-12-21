#! /usr/bin/Rscript
# Created on Tue Jan 28 15:20:35 2020
# Set-up download of text from the Hansard
# Alfonso Martínez Arranz
# R 3.6.1

# Load up--------

suppressPackageStartupMessages({ 
library(tidytable)
library(lubridate)
library(rvest)
library(tidyverse)
library(tictoc)
source('scripts/wrangling/02b_extract_text.R')
})

  # 1. Download CSV files from the Hansard search website 
  # 2. Create a folder '{hansard_project}/{main_folder}_data/01_records'
  # 3. Move the CSV records to that folder and run "dl_hansard.sh {main_folder}"

# https://parlinfo.aph.gov.au/parlInfo/search/search.w3p;adv=yes;resCount=Default

# ARGS AND OPTIONS ###################

checkdir <- function(dirpath) {

  if(!dir.exists(dirpath)) {
    dir.create(dirpath)
    
    message(dirpath, " created")
    
  }
  
}


cargs <- commandArgs(trailingOnly = T)

main_folder <- cargs[1]

test <- as.logical(cargs[2])

if (is.na(test)) {

  main_folder <- 'general'
  test <- TRUE
  
}

if(isTRUE(test)) {  
  
  warning("\nTEST MODE ON!! Only a handful of records will be queried.\n", 
          immediate. = T, call. = F)
  
}


source_path <- paste0(main_folder, '_data/01_records/')

batch_dir <- paste0(main_folder, '_data/00_batches/')

out_path <- paste0(main_folder, "_data/02_full_text/")

download_log <- paste0(main_folder, "_data/downloaded_ids.log")

fail_log <- paste0(main_folder, "_data/persistent_fail_ids.log")  


if(any(!dir.exists(c(batch_dir, out_path)))) {
  
  dir.create(batch_dir)
  dir.create(out_path)
  
}

if(!file.exists(c(download_log)))  file.create(download_log)
if(!file.exists(fail_log)) file.create(fail_log)


problem_counter <- 0 # For use inside query_in_batches.R



# CHECKING EXISTING FILES ##############
message("Loading records...")

rec_list <- list.files(path = source_path, pattern = ".csv")

names(rec_list) <- str_match(rec_list, "(^\\w+?)_")[,2]



rec_df <- map_dfr.(rec_list, ~fread.(paste0(source_path, .x)), 
                  .id = "term") %>% 
  mutate(year = year(dmy(Date))) %>% 
  distinct()


existing_ids <- read_lines(download_log)


if(length(existing_ids) == 0) {
  
  rec_df_remaining <- rec_df 
  
  
} else {
  
  rec_df_remaining <- rec_df %>% 
    filter(!`System Id` %in% existing_ids)
  
  }



# TEST OPTIONS #############################

batch_size <-  100

if(test) {

  batch_size <- 2 
  
  problematic <- rec_df_remaining %>%
    filter(year %in% c(1981, 2000)) %>%
    group_by(year) %>%
    slice_sample(n = batch_size*2) %>%
    ungroup() %>%
    # mutate(Permalink = "THIS SHOULD CAUSE ERROR FOR TESTING PURPOSES") %>%
    slice(1)
  
  rec_df_remaining <- rec_df_remaining %>% 
   filter(year %in% c(1981, 2000)) %>% 
    group_by(year) %>% 
    slice_sample(n = batch_size*2) %>% 
    ungroup() %>%
  bind_rows(problematic)
 
}

# MAIN LOOP ###############################
  
rev_sorted_years <- sort(unique(rec_df_remaining$year), decreasing = T)

for (yr in rev_sorted_years) {

  records <- rec_df_remaining %>% 
    filter(year == yr)
  

 if(nrow(records) == 0) { 
  
   message("No records left in year ", yr)
    
   next
   
    }

 
message("\n*** Processing records from ", yr, " ***")
message("AEST ", now(tzone = "Australia/Melbourne"), "\n")

source('scripts/wrangling/02_batch_download.R', local = TRUE)

}


message("Download script FINISHED")


