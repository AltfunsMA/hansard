# -*- coding: utf-8 -*-
# Created on Tue Jan 28 15:20:35 2020
# Downloading text from the Hansard
# Alfonso Martínez Arranz
# R 3.6.1


# Download HTML -----



## CHECKING ALREADY DOWNLOADED BATCHES  ###
# If download interrupted before trasnferring to 02_full_text

tic(paste(fecha, "completed"))
batch_rx <- paste0(fecha, "_batch_")

fp_pre <- list.files(batch_dir, pattern = batch_rx, full.names = T)

# browser()

if(length(fp_pre) > 0) {
  
  pre_success<- map_dfr(fp_pre, read_rds) %>% 
    filter(!is.na(main_text))
  
} else {
  
  # Warning if already exists which may indeed happen when files inside it
  # contain no results
  suppressWarnings(dir.create(batch_dir))
  
  pre_success <- tibble(Permalink = NA)
  
}

# ESTABLISH REMAINING RECORDS #################

remaining_records <-  records %>% 
  anti_join(pre_success, by = "Permalink")


if(nrow(remaining_records) == 0) { 
  
  message("All records pre-downloaded. Going straight to combination.")
  
  
} else {
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  # MAIN DOWNLOAD CALL ##########
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  source("scripts/wrangling/02a_main_query.R")
  
  
} 


## COMBINE ALL DOWNLOADED BATCHES ############

success_fp <- list.files(batch_dir, pattern = batch_rx, full.names = T)


success <- map_dfr.(success_fp, read_rds)


out_file <- paste0(out_path, fecha, ".csv")

is_new_file <- !file.exists(out_file)

message("\nCleaning text and writing to file ", out_file)



uselss_toptxt <- "\\s*Content Window|\\s*Download Fragment|\\s*Watch ParlView Video"

success %>% 
  select(Permalink, Date, main_text) %>% 
  filter.(!is.na(main_text)) %>% 
  mutate.(main_text = str_remove_all(main_text, uselss_toptxt),
          # A couple of items under "Reference" were misencoded as UTF-8 but aren't
          # checked with calls below
          across.(where(is.character), iconv, "UTF-8", "UTF-8", sub = "")) %>% 
  write_delim(out_file, col_names = is_new_file,
            append = T, delim = ",")

# UPDATE LOG FILES ----

  
write_lines(success$Permalink, download_log, append = T)



if(!test) unlink(batch_dir, recursive = T)


toc()
