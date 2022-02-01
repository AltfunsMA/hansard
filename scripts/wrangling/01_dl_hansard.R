#! /usr/bin/Rscript
# Created on Tue Jan 28 15:20:35 2020
# Set-up download of text from the Hansard
# Alfonso Martínez Arranz
# R 3.6.1

# Load up--------

pacman::p_load(tidytable, lubridate, rvest, progress, tidyverse, tictoc)

source('scripts/wrangling/02b_extract_text.R')

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

combo_record_file <- paste0(source_path, "combined_", main_folder, "_records.csv")
# combo_record_file <- paste0(source_path, "second_try_records.csv")

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


# CHECKING EXISTING FILES ##############
message("Loading records...")

rec_list <- list.files(path = source_path, pattern = ".csv")

names(rec_list) <- str_match(rec_list, "(^\\w+?)_")[,2]

aph_url <- "https://parlinfo.aph.gov.au:443/parlInfo/search/display/display.w3p;query=Id"

if(!file.exists(combo_record_file)) {
  
  first_rec_df <- map_dfr.(rec_list, ~fread.(paste0(source_path, .x)))
  
  first_rec_df %>% 
    mutate.(Permalink = str_remove(Permalink, aph_url)) %>% 
    select.(-Source, -Page) %>% 
    fwrite.(combo_record_file)
  
}
  
if(!exists("rec_df")) {
  
  
  rec_df <- fread.(combo_record_file,
                   select = c("Permalink", "Date", "Title"))
  
  
  rec_df <- rec_df %>%
    mutate.(year_month = paste0(year(dmy(Date)), "_", month(dmy(Date), label = T)),
            year = year(dmy(Date))) %>%
    distinct.()
  
  
} else {message('Using records dataframe in the Global Env')}


existing_ids <- read_lines(download_log)


if(length(existing_ids) == 0) {
  
  rec_df_remaining <- rec_df 
  
  
} else {
  
  rec_df_remaining <- rec_df %>% 
    filter(!Permalink %in% existing_ids)
  
  }



# TEST OPTIONS #############################

batch_size <-  100

if(test) {

  batch_size <- 3 
  
  # Remove the comments to test problematic responses
  
  # problematic <- rec_df_remaining %>%
  #   slice_sample(n = 1) %>% 
  #   mutate(Permalink = "THIS SHOULD THROW AN ERROR") 
  # 
  # never_succeeds <- rec_df_remaining %>%
  #   slice_sample(n = 1) %>% 
  #   mutate(Permalink = "http://httpbin.org/status/500") 
  
  rec_remaining_final <- rec_df_remaining %>% 
   filter(year %in% c(2003, 2005)) %>% 
    slice_sample.(.by = year_month, n = batch_size) 
  # %>% 
  # bind_rows(problematic, never_succeeds)
 
} else {  rec_remaining_final <- rec_df_remaining }

# MAIN LOOP ###############################
  
rev_sorted_date <- sort(unique(rec_remaining_final$year_month), decreasing = T)

for (fecha in rev_sorted_date) {

  records <- rec_remaining_final %>% 
    filter(year_month == fecha)
  

 if(nrow(records) == 0) { 
  
   message("No records left in year_month ", fecha)
    
   next
   
    }

 
message("\n*** Processing records from ", fecha, " ***")
message("AEST ", now(tzone = "Australia/Melbourne"), "\n")

source('scripts/wrangling/02_batch_download.R', local = TRUE)

}


message("Download script FINISHED")


