#! /usr/bin/Rscript

# NB: the scan.sh calls a somewhat faster and less complex sed bash script instead of this

# ----
library(tidyverse, quietly = T)
library(tictoc)
library(tidytable)



main_topic <- 'coal'


hansard_stopwords <- read_lines(paste0(main_topic, "_data/04_model_inputs/STOPWORDS.txt"))

stopwords_rx <- cptools::bound_rx(hansard_stopwords)

bigrams <- read_lines(paste0(main_topic, "_data/04_model_inputs/BIGRAMS.txt"))

bigrams_rx <- cptools::bound_rx(bigrams, by_element = T)

bigrams__ <- str_replace(bigrams, " ", "_")

corpus <- read_tsv(paste0(main_topic, '_data/04_model_inputs/corpus.txt'), 
                   col_names = c("year", "lemmas"), col_types = "ic") 
# -----

tic() # 30 seconds for bigram regex  inside a tidytable, very similar for the full things
out_corpus <- corpus %>% 
  mutate.(row = row_number.(),
    lemmas = stringi::stri_replace_all_regex(lemmas, 
                                                   bigrams_rx,
                                                   bigrams__,
                                                   vectorize_all = F)) %>% 
  
  separate_rows.(lemmas, sep = " ") %>% 
  filter.(!lemmas %in% hansard_stopwords) %>% 
  summarise.(lemmas = paste0(lemmas, collapse = " "), .by = c(row, year)) %>% 
  select.(-row)
toc()


write_tsv(out_corpus, paste0(main_topic, '_data/04_model_inputs/corpus_bigrammed.txt'), col_names = FALSE)

