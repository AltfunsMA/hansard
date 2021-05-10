# -*- coding: utf-8 -*-
# Created on Tue Jan 28 15:20:35 2020
# Downloading text from the Hansard
# Alfonso Martínez Arranz
# R 3.6.1



suppressPackageStartupMessages({
library(rvest)
library(tidyverse)
library(tictoc)
})

rm_tmp_folder <- function() {
  
  tempfiles <- list.files(tempdir)
  
  if(length(tempfiles) == 0) unlink(tempdir, recursive = T)
  
  
}


on.exit(rm_tmp_folder)


# NB! HTML output is stored in a C struct in memory and cannot be serialised (saved)
# without losing the pointer

# Download HTML -----

hans_html_final <- tibble()

attempt <- 1


# While loop for repeated attempts at downloading records

while (nrow(records) > 0 & attempt < 10) {
  
  tic("\nbatch")
  
  if(test) batch_size <- 2 else batch_size <-  100
  

  # Batched to guard against disconnects and also poor quality downloads

  ## CHECKING TEMP FILES  ###
  
  tempdir <- paste0(main_folder, "/temp/")
  batch_rx <- paste0(yr, "_batch_")

  if(dir.exists(tempdir)) {
    
    fp_temp <- list.files(tempdir, pattern = batch_rx, full.names = T)
    
    pre_downloaded <- map_dfr(fp_temp, read_rds)
    
  } else {
    
    dir.create(tempdir)
    
    pre_downloaded <- tibble(Permalink = NA)
    
  }

  
  remaining_records <-  records %>% 
    anti_join(pre_downloaded, by = "Permalink")
  
  
  if(nrow(remaining_records) == 0) { 
    
    message("All files in temp")

    
    } else { 
  

  ## QUERY BY BATCHES ##########
      
      
  # https://stackoverflow.com/questions/7060272/split-up-a-dataframe-by-number-of-rows
  n  <- nrow(remaining_records)
      
  splitfct  <- rep(1:ceiling(n/batch_size),each=batch_size)[1:n]
      
  records_list <- split(remaining_records, splitfct)
  
  batches_n <- length(records_list)
  
  message(n, ' records to process in ', batches_n,  ' batches \n')
    
  poss_read_html <- possibly(read_html, otherwise = NA)
  
  sleep_secs <- 1 
  

  for (i in 1:batches_n) {
    
    dld_batch <- records_list[[i]] %>% 
        mutate(html = map(Permalink, poss_read_html))

    
      # Assessing batch return quality to see if server is overloading
      df_nrows <- dld_batch %>% 
        filter(map_lgl(html, ~length(.x) < 1) | is.na(html)) %>% 
        nrow()
    
      perc_empty <- df_nrows/nrow(dld_batch)*100
    
      
      # Extending wait time if empties are present
      # reducing it after this is sorted out
      
      if(is.nan(perc_empty)) next
      
      if(perc_empty > 0) {
        

        cat(" Batch", i, "returned", perc_empty, "% empty html ||")
        
        sleep_secs <- min(60, sleep_secs + 6)
        
      } 
      
      else {
        
        cat("Batch", i, "all good. Sleep =", sleep_secs, "s || ")
        
        sleep_secs <- sleep_secs - 1
        sleep_secs <- max(3, sleep_secs)
        
        }

      
      ### SAVE INDIVIDUAL BATCH ##########
      
      write_rds(dld_batch, paste0(tempdir, batch_rx, i, "_temp_html.rds"),
                version = 3)
            
      Sys.sleep(sleep_secs)
      
       
  }

    }
  
  
  ## COMBINE BATCHES ############
  
  fp_temp <- list.files(tempdir, pattern = batch_rx, full.names = T)
  
  
  hans_html_temp <- read_rds(fp_temp)
  
  

  if(is.null(hans_html_temp)) break 
    
  ### REBUILD RECORDS DATAFRAME WITH ONLY UNSUCCESSFUL DOWNLOADS
  remaining_records <- hans_html_temp %>% 
    filter(is.na(html)) %>% 
    select(-html)
  
  
  ### STORE SUCCESSFUL 
  hans_html <- hans_html_temp %>% 
    filter(!is.na(html))
  
  hans_html_final <- bind_rows(hans_html_final, hans_html)

  if(nrow(remaining_records) == 0) {cat("records for", yr, "downloaded \n\n")}
  
  else if(attempt == 10) {
    
    attempts <- attempts + 1 
    
    message(nrow(records), term, " records not yet dl'd for year ", yr, 
            " after 10 attempts.\n")
    
    }
  
  else {
    
    attempts <- attempts + 1 
    
    cat("\n Attempt No.", attempts, "\n")
    
    }
  
  
}


# SAVE ALL SUCCESSFUL HTML DOWNLOADS

filepath <- paste0(main_folder, "/raw_html/", yr,
                   "_html_", 
                   format(Sys.time(), "%Y-%m-%d.%H%M"),
                   ".rds")

htmlpath <- dirname(filepath)


checkdir(htmlpath)


saveRDS(hans_html_final, filepath)


unlink(tempdir, recursive = T)


# Extract text ------

poss_html_node <- possibly(html_node, otherwise = NA)
poss_html_text <- possibly(html_text, otherwise = NA)


css_marker <- c(".docDiv", "#documentContent") # selecting using SG online


extract_text <- function(html, title) {
  
  # For each html article, try to extract nodes with all css_markers
  # Then extract text from those that don't return NA and save in list
  
  text_temp <- list()
  
  for (i in 1:length(css_marker)) {
    
    node <- poss_html_node(html, css_marker[[i]])
    
    if(!is.na(node)) {
      
      text_temp[[i]]  <- poss_html_text(node) 
      
    } 
    
  }
  
  if (length(text_temp) == 0) {
    
    message(paste0("No text for:", title)) 
    return(NA)
    
  }
  
  # Select those that have long enough text
  
  txt_selected <- unlist(text_temp[which(nchar(text_temp) > 100)])
  
  if(length(txt_selected) < 1) {
    return(NA)
  }
  
  # For several matches, pick one with the most characters
  else if (length(txt_selected) > 1) {
    
    txt_nchar <- nchar(txt_selected)
    
    return(txt_selected[which(txt_nchar == max(txt_nchar))])
    
  }
  
  # Only one returned
  txt_selected
  
}

tic("Text extraction")
hans_content <- hans_html_final %>% 
  mutate(main_text = map2_chr(html, Title, ~extract_text(.x, .y)))%>% 
  select(-html) %>% 
  filter(!is.na(main_text))
toc()

write_delim(hans_content, textpath, col_names = !file.exists(textpath),
            append = T, delim = ",")

cat("Text extraction for", yr, "completed.\n\n")