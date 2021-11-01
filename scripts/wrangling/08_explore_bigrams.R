# Identify ngrams

suppressPackageStartupMessages({
library(tidytable)
library(data.table)
library(stringr)
  library(tidytext)})



df <- fread("coal_data/04_model_inputs/coal_single_orator_300tkn_para.csv") %>% 
  as_tidytable()


tokens_df <- df %>% 
  select.(permalink, date, main_text) %>% 
  unnest_tokens(output = "tkns", input = main_text) %>% 
  filter.(!tkns %in% stop_words$word[stop_words$lexicon == "SMART"])


ngrams_df <- tokens_df %>% 
  summarize.(.by = c(permalink, date), 
             clean_text = paste0(tkns, collapse = " ")) %>% 
  unnest_ngrams(output = "grams", input = clean_text, n = 2, n_min = 2)
  


ngram_freq <- ngrams_df %>% 
  count.(grams, sort= T)


hist(ngram_freq$N)

ngram_freq %>% 
  filter.(N > 400) %>% 
  View()
  

