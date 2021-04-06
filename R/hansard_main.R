#! /usr/bin/Rscript
# Created on Tue Jan 28 15:20:35 2020
# Set-up download of text from the Hansard
# Alfonso Martínez Arranz
# R 3.6.1

suppressPackageStartupMessages({ 
library(tidyverse)
library(lubridate)
library(rvest)
library(tidyverse)
library(tictoc)
source('R/extract_text.R')
})

# Download one CSV file per search topic from the Hansard search website 
# https://parlinfo.aph.gov.au/parlInfo/search/search.w3p;adv=yes;resCount=Default

# ARGS AND OPTIONS ###################

checkdir <- function(dirpath) {

if(!dir.exists(dirpath)) {
  
  dir.create(dirpath)

message(dirpath, " created")

}

}

main_folder <- 'coal_data'

source_path <- paste0(main_folder, '/records/')

problem_counter <- 0 # For use inside query_in_batches.R


cargs <- commandArgs(trailingOnly = T)

test <- cargs[1]

if(is.na(test)) test <- FALSE else test <- TRUE

if(test) {  
  
  warning("\nTEST MODE ON!! Only a handful of records will be queried.\n", 
          immediate. = T, call. = F)
  
}

# CHECKING EXISTING FILES ##############

rec_list <- list.files(path = source_path, pattern = ".csv")

names(rec_list) <- str_match(rec_list, "(^\\w+?)_")[,2]



rec_df <- map_dfr(rec_list, ~read_csv(paste0(source_path, .x), 
                                      col_types = cols(.default = "c")), 
                  .id = "term") %>% 
  mutate(year = year(dmy(Date))) %>% 
  distinct()


out_path <- paste0(main_folder, "/yearly_dfs/")

existing_ids <- read_lines(paste0(main_folder, "/downloaded_ids.log"))
  
if(length(existing_ids) == 0) {
  
  rec_df_confirmed <- rec_df 
  
  
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
    # mutate(Permalink = "ERROR ON PURPOSE") %>%
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

source('R/dl_hansard2.R', local = TRUE)

}



