# -*- coding: utf-8 -*-
# Created on Tue Jan 28 15:20:35 2020
# Downloading text from the Hansard
# Alfonso Martínez Arranz
# R 3.6.1

library(rvest)
library(tidyverse)

records <- read_csv(paste0(hansdrive, file),
           col_types = cols(
             Program = "c",
             Department = "c",
             `Sub program` = "c",
             `Question No.` = "c"))


# Download HTML -----

# The connection can be rejected sometimes, query in smaller batches


dl_html <- function(records, batch_size = 100) {
  
  records_list <- suppressWarnings(split(records, 0:nrow(records) %/% batch_size))
    
  batches_n <- length(records_list)
  
  cat(nrow(records), 'records to process in', batches_n, 'batches \n')
  
  processed_list <- vector(mode = "list", length = batches_n)
    
  poss_read_html <- possibly(read_html, otherwise = NA)
    
  
  sleep_secs <- 5 
  
  
  for (i in 1:batches_n) {
    
      processed_list[[i]] <- records_list[[i]] %>% 
        mutate(html = map(Permalink, poss_read_html))
    
      # Assessing batch return quality
      df_nrows <- processed_list[[i]] %>% 
        filter(map_lgl(html, ~length(.x) < 1) | is.na(html)) %>% 
        nrow()
    
      perc_empty <- df_nrows/nrow(processed_list[[i]])*100
        
      # Extending wait time if empties are present
      # reducing it after this is sorted out
      
      if(perc_empty > 0) {
        
        beepr::beep(1)
        
        cat("\n Batch", i, "returned", perc_empty, "% empty html \n")
        
        sleep_secs <- min(60, sleep_secs + 10)
        
      } 
      
      else {
        
        cat("Batch", i, "all good. || ")
        
        sleep_secs <- sleep_secs - 5
        sleep_secs <- max(5, sleep_secs)
        
        }
      
      Sys.sleep(sleep_secs)
      
      # Temp save
       
      if(i %% 5 == 0) {
        
        temp_file <- bind_rows(processed_list)
        
        saveRDS(temp_file, paste0(hansdrive, "temp/",
                                  hans_mainterm,
                                  "_temp_html.rds"))
        
        cat("Temp file saved at", time, "\n")
        
        }
    
  }

  
  out <- bind_rows(processed_list)

  out

}
  
hans_html_final <- tibble()

attempts <- 1

while (nrow(records) > 0 & attempts < 10) {
  
  attempts <- attempts + 1 
  
  hans_html_temp <- dl_html(records)
  
  # to keep searching
  records <- hans_html_temp %>% 
    filter(is.na(html)) %>% 
    select(-html)
  
  # To store
  hans_html <- hans_html_temp %>% 
    filter(!is.na(html))
  
  hans_html_final <- bind_rows(hans_html_final, hans_html)

  if(nrow(records) == 0) {cat("All records finished \n\n")}
  
  else if(attempts == 10) {
    
    cat(nrow(records), hans_mainterm, "records remaining after 10 attempts.\n")
    
    }
  
  else (cat("Attempt No.", attempts, "\n"))
  
}

filepath <- paste0(hansdrive, "completed_html/", hans_mainterm,
                   "_html_", 
                   format(Sys.time(), "%Y-%m-%d.%H%M"),
                   ".rds")

saveRDS(hans_html_final, filepath)

# hans_html <- readRDS("data/hansard/batteries_html_2020-02-13.0802.rds")



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
    print(paste0("No text for:", title)) 
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


hans_content <- hans_html_final %>% 
  mutate(main_text = map2_chr(html, Title, ~extract_text(.x, .y)))%>% 
  select(-html)


write_csv(hans_content, paste0(hansdrive, "completed_content/", hans_mainterm, 
                               "_content_",
                               format(Sys.time(), "%Y-%m-%d.%H%M"),
                               ".csv"))

cat("Text extraction for", hans_mainterm, "completed.\n\n")