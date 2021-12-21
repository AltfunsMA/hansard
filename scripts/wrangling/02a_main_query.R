# SPLIT DATAFRAME IN BATCHES ##########

# https://stackoverflow.com/questions/7060272/split-up-a-dataframe-by-number-of-rows
n  <- nrow(remaining_records)

splitfct  <- rep(1:ceiling(n/batch_size),each=batch_size)[1:n]

records_list <- split(remaining_records, splitfct)

batches_n <- length(records_list)

message(n, ' records to process in ', batches_n,  ' batches \n')

# MAIN LOOP

poss_read_html <- possibly(read_html, otherwise = NA)

default_sleep <- max(problem_counter, 1)
current_sleep <- default_sleep 

for (i in 1:batches_n) {
  
  ## DOWNLOAD AND TEXT EXTRACTING (rvest funs w purrr wrappers)----
  
  dld_batch <- records_list[[i]] %>% 
    mutate(html = map(Permalink, poss_read_html),
           main_text = map2_chr(html, Title, ~extract_text(.x, .y))) %>% 
    select(-html)
  
  ## Assessing batch return quality -----
  df_nrows <- dld_batch %>% 
    filter(is.na(main_text)) %>% 
    nrow()
  
  perc_empty <- df_nrows/nrow(dld_batch)*100
  
  
  ## Extending wait time if empties are present-----
  # reducing it after this is sorted out
  
  if(is.nan(perc_empty)) next
  
  if(perc_empty > 25) {
    
    problem_counter <<- problem_counter + 1
    
    calm_increase <- perc_empty/10
    
    cat(" Batch", i, "returned", perc_empty, "% empty html ||")
    
    current_sleep <- min(60, current_sleep + calm_increase*default_sleep)
    
  } else {
    
    cat("Batch", i, "all good w sleep =", current_sleep, "s || ")
    
    problem_counter <-  problem_counter - 1 
    
    current_sleep <- current_sleep - default_sleep
    current_sleep <- max(default_sleep, current_sleep)
    
  }
  
  
  ### SAVE INDIVIDUAL BATCH WHETHER SUCCESSFUL OR NOT ##########
  
  write_rds(dld_batch, paste0(batch_dir, batch_rx, i, ".rds"))
  
  Sys.sleep(current_sleep)
  
  
}