# -*- coding: utf-8 -*-
# Created on Tue Jan 28 15:20:35 2020
# Downloading text from the Hansard
# Alfonso Martínez Arranz
# R 3.6.1


# Download HTML -----

out_df <- tibble()

attempt <- 1

batch_dir <- paste0(main_folder, '/batches/')


# While loop for repeated attempt at downloading records

while (attempt < 10) {
  
  tic()

  ## CHECKING ALREADY DOWNLOADED BATCHES  ###
  
  batch_rx <- paste0(yr, "_batch_")
  
  fp_pre <- list.files(batch_dir, pattern = batch_rx, full.names = T)
  
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
    
    source("download_scripts/02a_main_query.R")
    
    
  } 
  

  ## COMBINE NEWLY DOWNLOADED BATCHES ############
  
  fp_post <- list.files(batch_dir, pattern = batch_rx, full.names = T)
  
  
  post_all <- map_dfr(fp_post, read_rds)
  

  
  ### REBUILD REMAINING RECORDS WITH UNSUCCESSFUL DOWNLOADS
  
  previous_remaining_records <- remaining_records
  
  remaining_records <- post_all %>% 
    filter(is.na(main_text))
  
  different_permalinks <- setdiff(remaining_records$Permalink, 
             previous_remaining_records$Permalink)
  
  
  ### STORE SUCCESSFUL ------
  new_content <- post_all %>% 
    filter(!is.na(main_text))
  
  out_df <- bind_rows(out_df, new_content) %>% 
    distinct()
  
  if(nrow(remaining_records) == 0) {

        break
    
    }  else if(length(different_permalinks) == 0) {
      
        if(test || attempt > 3) {
          
        if(test) message("\nFailures not re-attempted in test mode.")
        
        message("The same", 
                nrow(remaining_records), 
                " dls keep failing:")
        
        cat(remaining_records$`System Id`, sep = "\n")
        
        break
      }
      
      attempt <- attempt + 1 
      
      message('\n** Re-attempting... Attempt =', attempt, '\n')
      
      
    } else if(attempt == 10) {
    
    message(nrow(remaining_records), term, " records not yet dl'd for year ", yr, 
            " after 10 attempt.\n")
      
      break
    
  } else {
    
    attempt <- attempt + 1 
    
    message('\n** Re-attempting... Attempt =', attempt, '\n')

  }
  
}


out_file <- paste0(out_path, yr, ".csv")

is_new_file <- !file.exists(out_file)

write_delim(out_df, out_file, col_names = is_new_file,
            append = T, delim = ",")

# UPDATE LOG FILES

if(!test) {
write_lines(out_df$`System Id`, paste0(main_folder, "/downloaded_ids.log"), append = T)
write_lines(remaining_records$`System Id`, paste0(main_folder, "/persistent_fail_ids.log"), append = T)
}


unlink(batch_dir, recursive = T)


cat("Text extraction for", yr, "completed.\n")
toc()