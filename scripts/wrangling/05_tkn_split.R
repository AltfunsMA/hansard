#! /usr/bin/Rscript
# R 4.0.3
# 28 June 2021
# Alfonso Martínez Arranz


# Set-up -------
suppressPackageStartupMessages({ 
library(tidyverse)
library(tidytable)
library(tidytext)
source("scripts/wrangling/05a_split_funs.R")
})

debug <- FALSE

cargs <- commandArgs(trailingOnly = T)


if (length(cargs) == 0) {
  
  filename <- "repr_rights_data/04_model_inputs/repr_rights_hans_so.csv"
  
  tkns_4_std_paragraph <- 300 # Aimed for length (inside merge_sm_chunks function)
  
  threshold <- 600 # Maximum tolerance
  
  text_var <- "main_text" # should be main_text for our hansard downloads; 
  
  unique_id_var <- "system_id" # should be system_id for Hansard; Speech DB ID for Muller-Hansen
  
  mentions_str <- "abort|contraceptive|miscarriage|rape|reproductive"
  
  all_defaults <- paste0(paste0("- source file:\n\t '",filename, "'\n"), 
                         paste0("- target token length:\n\t '",tkns_4_std_paragraph, "'\n"), 
                         paste0("- maximum token tolerance:\n\t '",threshold, "'\n"), 
                         paste0("- text variable name:\n\t '",text_var, "'\n"), 
                         paste0("- unique row ID variable name:\n\t '",unique_id_var, "'\n"),
                         paste0("- filtering regex: \n\t '", mentions_str, "'\n"))
  
  if(interactive()) {
    
    message('using the following defaults: \n', all_defaults)
  
   } else {
      
      
  cat("Are you happy to use the following defaults (y/n)? \n")
    
  cat(all_defaults)
  
  response <- readLines(file("stdin"), n = 1, ok = FALSE)
  
  
  stopifnot(response %in% c("Y",'y','yes','YES'))
  
  }
  
  
} else if(!length(cargs) %in% c(5)) {
  
  stop("Please supply exactly 5 arguments or none for defaults in script:", 
       paste0("\n - ", c("input df filepath (CSV)",
                        "text variable name",
                        "unique id variable name",
                        "target paragraph length in words",
                        "maximum tolerated yyparagraph length in words (leeway)",
                        "regex for paragraph selection")))
      
  
}  else {

filename <- cargs[1]

text_var <- cargs[2]

unique_id_var <- cargs[3]

tkns_4_std_paragraph <- as.numeric(cargs[4])

threshold <- as.numeric(cargs[5])

mentions_str <- as.numeric(cargs[6])

}


if(debug) sample_prop <- 0.25  else  sample_prop <- 1

if(!is.na(cargs[1]) && debug) warning("Debugger activated and only ", 
                                      scales::percent(sample_prop), 
                                      " of rows to be processed")


out_path <- str_replace(filename,
                        "\\.csv",
                        paste0(tkns_4_std_paragraph, "tkn_w_id.csv"))


# Load file ------

input_df <- fread.(filename)

cat("\nSplitting", nrow(input_df), "single orator documents into chunks...\n")


# Main split and recombination -------------------------------------------------


prepped <- input_df %>% 
  rename_with.(~str_replace(.x, text_var, "main_text")) %>% 
  rename_with.(~str_replace(.x, unique_id_var, "system_id")) %>% 
  add_token_counts() %>% 
  filter.(n_tokens > 100) 

save_below_threshold <- function(in_process_df) {
  
  
  if(counter > 7) stop("Need to reset prelim_df and counter", call. = F)
  
  prelim_df <<- in_process_df %>% 
    filter.(.by = system_id, all(n_tokens < threshold)) %>% 
    bind_rows.(prelim_df)
  
  counter <- counter + 1
  
  in_process_df %>% 
    anti_join.(prelim_df, by = "system_id")
  
  
}

# Execute merge -------
prelim_df <- tibble()
counter <- 0

cat("Distribution of tokens per row (paragraph) when splitting on: ")
remainder <- prepped %>% 
  split_n_merge("\\s{3,}", "\n") %>% # interventions/paragraphs
  save_below_threshold() %>% 
  split_n_merge("\n", ". ") %>%  # paragraphs/items
  save_below_threshold() %>% 
  split_n_merge("[.?!]\\s", ". ") %>%  # sentences
  save_below_threshold() %>% 
  split_n_merge("[;)]\\s", ". ") %>% # lists
  save_below_threshold() %>% 
  split_n_merge("—", ". ") %>%  # lists
  save_below_threshold() %>% 
  split_n_merge("[.][A-Z]", ". ") %>% # sentences
  save_below_threshold() 

# Add results-------

final_df <- bind_rows.(prelim_df, remainder) %>% 
  select.(-any_of("para_id")) 


# Final checks ------


original_tokens <- prepped$n_tokens
final_tokens <- final_df$n_tokens

cat("Original tokens:\n")
summary(original_tokens)
cat("Final tokens:\n")
summary(final_tokens)

missing_ids_in_final <- setdiff(unique(prepped$system_id),
                     unique(final_df$system_id))

stopifnot(length(missing_ids_in_final) == 0)




# Post-split filtering

#	All documents whose title contains basic terms or All paragraphs: count(basic_terms) != 0 

out_df <- final_df %>% 
  filter.(str_detect(title, mentions_str) | str_detect(main_text, mentions_str)) %>% 
  arrange.(desc(date)) %>% 
  mutate.(doc_id = row_number.())
  
out_df %>% 
  count.(database) %>% 
  print()

data.table::fwrite(final_df, out_path)

cat("\n\n Saved file under", out_path, "\n\n")
cat("DONE\n")

# Plot comparison ------


# original_token_calc <- tkn_single_orator %>% 
#   distinct.(system_id, n_tokens, top_categories)
# 
# 
# processed_token_calc <- final_df %>% 
#   distinct.(para_id, n_tokens, top_categories)
# 
# bind_rows.(list('original' = original_token_calc, "processed" = processed_token_calc), .id = "stage") %>% 
#   mutate.(n_tokens = cut(n_tokens, c(0, 100, 200, 300, 500, 750, 1000, 5000, Inf), 
#                  dig.lab = 5,
#                  ordered_result = TRUE),
#           top_categories = fct_lump_n(top_categories, 3)) %>% 
#   ggplot() +
#   geom_bar(aes(stage, fill = n_tokens), position = "dodge") +
#   facet_grid(~top_categories) +
#   labs(x = "", y = "documents", title = "Single-speaker Hansard documents size distribution")
#   
# 

# View distribution of speakers (not run) ------
# coal_categorised.csv <- data.table::fread(paste0(main_path, "coal_categorised.csv"))
# 
# speaker_variables <- coal_categorised.csv %>% 
#   mutate(across(c(questioner, responder, speaker, interjector), Negate(is.na)), 
#          .keep = "used")
# 
# 
# table(speaker_variables) #piping directly into 'table' yields odd results
