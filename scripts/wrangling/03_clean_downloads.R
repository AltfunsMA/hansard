#! /usr/bin/Rscript
# Processing downloads from the hansard
# Alfonso Martínez Arranz
# R 3.6.1

## Set-up ----
pacman::p_load(tidyverse, tidytable, lubridate)


cargs <- commandArgs(trailingOnly = T)

main_folder <- cargs[1]


if (length(cargs) == 0) {
  
  main_folder <- 'general'


}

message("**** Cleaning downloaded Hansard files *****")

recpatt <- if_else(main_folder == 'coal', "All Hansard", '.csv')

records_path <- paste0(main_folder, "_data/01_records/", 
                       "combined_", main_folder, "_records.csv")

full_text_path <- paste0(main_folder, '_data/02_full_text/')



## MAIN DOWNLOAD LOOP ---------

message("Loading text...")

fp_full_text <- list.files(full_text_path, pattern = "\\.csv", full.names = T)

if(length(fp_full_text) == 0) stop("No CSV files in ", full_text_path)

text_df <- list()

for (year in seq(1901, 1921, by = 10)) {

  out_filename <- paste0(main_folder, '_data/04_model_inputs/', 
                         main_folder, '_full_downloaded_', year)
  
  decade_rx <- cptools::bound_rx(as.character(c(year:(year+9))), "", "")
  
  cat(decade_rx, "\n")
  
  decade_fp <- fp_full_text[str_detect(fp_full_text, decade_rx)]
    
  if(length(decade_fp) > 0) text_df[[as.character(year)]] <- map_dfr.(decade_fp, fread.)
  
  
}

# COMBINATION WITH RECORDS ------
message("Loading records...")

df_records <- fread.(records_path)



  
message("Merging...")
final_df <- text_df %>% 
  right_join.(df_records, by = "Permalink") %>% 
  distinct.() %>% 
  janitor::clean_names() 
  

# UTF8 returns a boolean per item in vector
# encoding_is_utf8 <- map(final_df[, map_lgl(final_df, is.character)], utf8::utf8_valid)
# 
# # Checking at vector level first; it was solved so no need to dig deeper
# map_lgl(encoding_is_utf8, ~any(!.x, na.rm = T))




message("Writing to: ", out_filename)

if(!dir.exists(dirname(out_filename))) dir.create(dirname(out_filename))

out <- final_df %>% 
  mutate.(date = dmy(date),
          year_month = paste0(year(dmy(date)), "_", month(dmy(date))),
         year = year(date)) %>% 
  arrange.(date)

max_rows_per_df <- 20000
  
if (nrow(out) > max_rows_per_df) {
  
  max_division <- ceiling(nrow(out)/max_rows_per_df)
  
  split_out <- out %>% 
    split(rep(1:max_division, length.out = nrow(.), each = ceiling(nrow(.)/max_division))) 
  
names(split_out) <- map_chr(split_out, ~paste0(min(.x$year), "_", max(.x$year)))

iwalk(split_out, ~fwrite(.x, paste0(out_filename, "_", .y, ".csv")))

} else {
  
  
  fwrite.(out, paste0(out_filename, ".csv"))
  
  
}

}


}


message("DONE")


