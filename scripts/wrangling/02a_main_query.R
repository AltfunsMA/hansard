# SPLIT DATAFRAME IN BATCHES ##########

# https://stackoverflow.com/questions/7060272/split-up-a-dataframe-by-number-of-rows
n  <- nrow(remaining_records)

splitfct  <- rep(1:ceiling(n/batch_size),each=batch_size)[1:n]

records_list <- split(remaining_records, splitfct)

batches_n <- length(records_list)

message(n, ' records to process in ', batches_n,  ' batches \n')

# MAIN LOOP

poss_read_html <- possibly(read_html, otherwise = "no HTML")



# Use possibly if URLs likely to be wrong
http_get <- function(address) {
  
  httr::RETRY("GET", address, quiet = FALSE,  times = 5)
  
} 


poss_http_get <- possibly(http_get, NA)

poss_status_code <- possibly(httr::status_code, NA)


for (i in 1:batches_n) {
  
  start_time <- Sys.time()
  
  ## DOWNLOAD AND TEXT EXTRACTING (rvest funs w purrr wrappers)----
  
  
  if(test && nrow(records_list[[i]]) < 20) cat(records_list[[i]]$Permalink, sep = "\n")
  
  dld_batch <- records_list[[i]] %>% 
    mutate(html = map(paste0(aph_url, Permalink), 
                      ~httr::RETRY("GET", .x, quiet = FALSE,  times = 5)),
           http_code = map_int(html, httr::status_code),
           retry_after = map(html, list("headers", "retry-after")),
           xml = map(html, poss_read_html),
           main_text = map2_chr(xml, Title, ~extract_text(.x, .y)))
  
  
  retry_after <- unique(unlist(dld_batch$retry_after))
  
  if(!is.null(retry_after)) cat("Retry after: ", retry_after, "\r")
  
  codes <- count(dld_batch, http_code) %>% 
    mutate(total = sum(n),
           proportion = scales::percent(n/total)) %>% 
    select(http_code, proportion)
  
  if(test || any(unique(codes$http_code) != 200))  { print(codes)  }

  ### SAVE INDIVIDUAL BATCH ##########
  
  dld_batch %>% 
    select(-http_code, -html, -xml) %>% 
    write_rds(paste0(batch_dir, batch_rx, i, "_",
                              cptools::date_str(), ".rds"))
  
  end_time <- Sys.time()
  
  x <- difftime(end_time, start_time)
  
  if(x > period(60, "sec")) { 
    
    stop("Waiting time has now exceeded 1 minute. ",
    "Stopping script to improve download speed.")
    
  }
  
  cat(round(x, 2), units(x), "to dl'd & extract text in batch", i, " \r")

  
}