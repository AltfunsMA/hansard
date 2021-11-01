# R 4.0.3
# 28 June 2021
# Alfonso Martínez Arranz


# Main functions --------------------------

split_n_merge <- function(df, sep_rx, sep_replacement) {
  
  if(nrow(df) == 0) {
    
    warning("No text left to split")
    
    return(df)
    
    }
  
  cat("\n")
  print(unname(as.data.frame(paste0("'", sep_rx, "'"))), quote = FALSE, row.names = FALSE)
  cat("\n Before: \n")
  print(summary(df$n_tokens))
  
  sep_replacement <<- sep_replacement # Needed in GlobEnv but best associated w fun
  
  collapsed <- df %>% 
    separate_rows.(main_text, sep = sep_rx) %>% 
    add_token_counts() %>% 
    filter.(!is.na(n_tokens)) %>% 
    {if(debug) group_by(., system_id) %>% # tidyverse works better for debugging
    summarise(main_text = merge_sm_chunks(main_text, n_tokens), 
              .groups = "drop") else  summarize.(., .by=system_id,
               main_text = merge_sm_chunks(main_text, n_tokens))}
  
  
  out <- df %>% 
    select.(-any_of(c("main_text", "para_id", "n_tokens"))) %>% 
    distinct.() %>% 
    left_join.(collapsed) %>% 
    add_token_counts()
  
  cat("\n After: \n")
  print(summary(out$n_tokens))
  
  out
  
}




collapse <- function(...) paste(..., collapse = sep_replacement)


merge_sm_chunks <- function(text, n_tokens) {
  
  to_combine_ix <- cumsum(n_tokens) %/% tkns_4_std_paragraph 
  
  out <- unlist(map(unique(to_combine_ix), ~collapse(text[to_combine_ix == .x])))
  
  tokens_check <- quick_tkn_count(out)
  
  small_ix <- which(tokens_check < tkns_4_std_paragraph/2)
  
  if(length(small_ix) != 0 && length(out) > 1) {
    
    full_ix <- 1:length(out)
    
    large_ix <- full_ix[-c(small_ix)]
    
    target_ix <- map_int(small_ix, ~get_nearest(large_ix, .x))

    # When several small chunks are to be merged on the same target chunk:
    target_dupes_ix <- target_ix[duplicated(target_ix)]
    
    if(length(target_dupes_ix) > 0) {
      
      target_non_dupes_ix <-  target_ix[!target_ix %in% target_dupes_ix]
      
      small_on_same_ix <- small_ix[target_ix %in% target_dupes_ix]
      
      small_on_separate_ix <- small_ix[!target_ix %in% target_dupes_ix]
    
    } else { 
      
      target_non_dupes_ix <-  target_ix
       
      small_on_same_ix <- NULL
      
      small_on_separate_ix <- small_ix
      
      }
    
    # Maintaining chunk order by modifying target chunks in-place
    
    new_out <- out
    
    # Merge all small chunks belonging to the same large chunk 
    # before merging with their respective large chunk
    for (d in target_dupes_ix) {
      
      small_on_same_ix <- small_ix[target_ix == d]
      
      new_out[[d]] <- collapse(out[d], collapse(out[small_on_same_ix]))
    
    }

    for (i in target_non_dupes_ix) {
      
      j <- small_on_separate_ix[which(i %in% target_non_dupes_ix)]
      
      
      if(i < j) new_out[[i]] <- collapse(out[i], out[j])
      
      else new_out[[i]] <- collapse(out[j], out[i])
      
      
    }
     
  # check for deviations from original 'out'
    new_out <- new_out[-c(small_ix)]

    
    tokens_out <- quick_tkn_count(new_out)

    total_check_in <- sum(tokens_check)

    total_new_out <- sum(tokens_out)

    if(check_exit_length(total_check_in, total_new_out)) {
      
      if(debug && interactive()) browser() else {
        
        stop("Large discrepancy after remerging small chunks")
        
      }
      
    }

    
    out <- new_out
    
  }
  
    length_text_in <- nchar(collapse(text))
    length_text_out <- nchar(collapse(out))

    if(check_exit_length(length_text_in, length_text_out)) {
    
    if(debug && interactive()) browser() else {
      
      stop("Large discrepancy after general splitting")
      
    }
      
    }
    
  
  out
  
}


# Helper functions ------------------------



add_token_counts <- function(df) {
  
  out <- df %>% 
    select.(-any_of(c("n_tokens", "para_id"))) %>% 
    mutate.(para_id = row_number.())
  
  tokenised <- out %>%
    select(para_id, main_text) %>% 
    as_tibble() %>% # unnest_tokens has problems with tidytable
    unnest_tokens(tokens, main_text)
  
  
  para_count <- tokenised %>% 
    count.(para_id) %>% 
    rename.(n_tokens = N)
  
  out %>% 
    left_join.(para_count, by = "para_id")
  
  
}


check_exit_length <- function(value_in, value_out) {
  
  # tolerance because there may be minor reassessments of what constitutes
  # a token when merging
  value_in <= value_out*0.999 &&  value_in >= value_out*1.001
  
}


quick_tkn_count <- function(v) map_int(v, ~lengths(str_split(.x, fixed(" "))))

get_nearest <- function(vec, target) {
  
  if (target == 1)  return(2L)
  
  if (target == vec[length(vec)]) return(vec[length(vec)-1])
  
  for (i in 1:length(vec)) {
  
  if(target - i %in% vec) return(as.integer(target-i))
  
  if(target + i %in% vec) return(as.integer(target+i))
  
}
    
    
  }
  


# Debugging/checking functions --------------


print_sample_text <- function(df, path_str, n = 10) {
  
  gen_out_path <- paste0("coal_data/text_samples/", path_str, "/")
  
  if(!dir.exists(gen_out_path)) dir.create(gen_out_path)
  
  skimr::skim(df)
  
  out <- df %>% 
    slice_sample(n = n)
  
  for (i in 1:nrow(out)) {
    
    mini_title <- str_trunc(out$title[i], 10)
    
    sink(normalizePath(paste0(gen_out_path, mini_title, "_", i, ".md"), mustWork = FALSE))
    
    
    cat("# ", out$title[i], "\n\n")
    
    cat("<br><A HREF='", out$permalink[i], "'> Permalink </A><br>")
    
    cat(out$main_text[i], "\n\n") 
    
    sink()
    
  } 
  
  invisible(df)
  
  
}


sample_entire_chunked_docs <- function(df, n_docs, chunks_min, chunks_max) {
  
  df %>%
    select(system_id, main_text, n_tokens, title, permalink) %>%
    group_by(system_id) %>%
    mutate(n_chunks = n()) %>%
    filter(between(n_chunks, chunks_min, chunks_max)) %>%
    nest() %>%
    ungroup() %>%
    slice_sample(n = n_docs) %>%
    unnest(data)
  
}



