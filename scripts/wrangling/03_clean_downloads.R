#! /usr/bin/Rscript
# Processing downloads from the hansard
# Alfonso Martínez Arranz
# R 3.6.1

pacman::p_load(tidyverse, tidytable, lubridate, data.table)


cargs <- commandArgs(trailingOnly = T)

main_folder <- cargs[1]


if (length(cargs) == 0) {
  
  main_folder <- 'repr_rights'


}

message("**** Cleaning downloaded Hansard files *****")

recpatt <- if_else(main_folder == 'coal', "All Hansard", '.csv')

records_path <- paste0(main_folder, "_data/01_records")

full_text_path <- paste0(main_folder, '_data/02_full_text/')

out_filename <- paste0(main_folder, '_data/04_model_inputs/', 
                       main_folder, '_full_downloaded')


# Appending of rows to files in various batches created mismatches in the order of columns
# _within_ each file. This function:
# 1. reads as many rows without misspecification errors from the top file
# 2. selects Permalink (an URL that acts as ID column. The original records (apart from the text,
#   the remaining data is the same)) 
# 3. selects "main text" column 
# 4. searches for each of those columns individually in the bottom part of the file

read_clean <- function(filepath) {
  
  message("Reading", basename(filepath))
  
  df <- read_csv(filepath, col_types = 'c')
  
  probs <- problems(df)
  
  first_probrow <- probs %>% slice(1) %>% pull(row)
  
  if(length(first_probrow) < 1) {
    
    return(list("success" = select(df, Permalink, main_text)))
         
  } 
  
  df_top <- read_csv(filepath, n_max = first_probrow-1) %>% 
    select(Permalink, main_text)
  
  df_bottom <- read_csv(filepath, skip = first_probrow,
                        col_names = FALSE) %>% 
    select_if(is.character)
  

  url_ix <- map_lgl(df_bottom, ~all(str_detect(.x, "https://parlinfo.aph")), na.rm = T)
  
  text_ix <- map_lgl(df_bottom, ~any(str_detect(.x, "Content Window"), na.rm = T))
  
  df_url <- df_bottom[, url_ix] %>% set_names("Permalink")
  
  df_text <- df_bottom[, text_ix] %>% set_names("main_text")

  out <- bind_cols(df_url, df_text) %>% 
    bind_rows(df_top)
  
  if(nrow(out) == nrow(df)) { list("success" = out)
    } else (list("failure" = out))
  
  
}

## MAIN DOWNLOAD LOOP ---------

fp_full_text <- list.files(full_text_path, pattern = "\\.csv", full.names = T)

if(length(fp_full_text) == 0) stop("No CSV files in ", full_text_path)


main_df <- map(fp_full_text, read_clean)

out_error <- main_df %>% 
  map('failure') %>% 
  compact()

out_success <- main_df %>% 
  map("success") %>% 
  compact() %>% 
  bind_rows()


# COMBINATION WITH RECORDS ------

fp_records <- list.files(records_path, 
                         pattern = recpatt, full.names = T)

df_records <- map(fp_records, fread) %>% 
  rbindlist(fill = TRUE)


uselss_toptxt <- "\\s*Content Window|\\s*Download Fragment|\\s*Watch ParlView Video"
  

final_df <- out_success %>% 
  filter.(!is.na(main_text)) %>% 
  right_join.(df_records, by = "Permalink") %>% 
  distinct.() %>% 
  mutate.(main_text = str_remove_all(main_text, uselss_toptxt),
         # A couple of items under "Reference" were misencoded as UTF-8 but aren't
         # checked with calls below
        across.(where(is.character), iconv, "UTF-8", "UTF-8", sub = "")) %>% 
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
         year = year(date)) %>% 
  arrange.(date)
  
if (nrow(out) > 20000) {
  
  split_out <- split(rep(1:5, length.out = nrow(.), each = ceiling(nrow(.)/5))) 
  
names(split_out) <- map_chr(split_out, ~paste0(min(.x$year), "_", max(.x$year)))

iwalk(split_out, ~fwrite(.x, paste0(out_filename, "_", .y, ".csv")))

} else {
  
  
  fwrite.(out, paste0(out_filename, ".csv"))
  
  
}

message("DONE")


