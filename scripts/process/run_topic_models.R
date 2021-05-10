# run DTM


full_command <- paste("hansard/dtm/dtm/main", 
      "--ntopics=20",  
      "--mode=fit", 
      "--rng_seed=0", 
      "- --ntopics=20", 
      "--corpus_prefix=hansard/civility_data/civility",
      "--outname=hansard/civility_output/dtm",
      "--top_chain_var=0.005",
      "--alpha=0.1",
      "--lda_sequence_min_iter=6",
      "--lda_sequence_max_iter=20",
      "--lda_max_em_iter=10")


system(full_command)


# Create SCAN parameter files

setwd("/data/hansard")

k <- seq(5, 15, 20)
sy <- c(1900, 1980)
w <- c(5, 10)

par_list <- list(k, sy, w)

names(par_list) <- c("k_val", "start_year", "window")

params <- cross(par_list)


for (p in params) {
  
  start_year <- p['start_year']
  k_val <- p['k_val']
  window <- p['window']
  
  pars <- paste0("k", k_val, "_w", window, "_", start_year)

  new_dir <- paste0("./civility_output/dynamic_senses/", pars)

  if(!dir.exists(new_dir)) dir.create(new_dir)

  
  filename <- paste0("./parameters_", pars, ".txt")    

  write_lines(paste0("text_corpus", "\t", "./civility_data/corpus.txt", "\n",
                   "target_words", "\t", "./civility_data/targets.txt", "\n",
                   "bin_corpus_store", "\t", "./civility_data/corpus.bin", "\n",
                   "window_size", "\t", "5", "\n",
                   "full_corpus_path", "\t", "./civility_data/corpus.bin", "\n",
                   "word_corpus_path", "\t", new_dir, "\n",
                   "output_path", "\t", new_dir, "\n",
                   "kappaF", "\t", "50.0","\n",
                   "kappaK", "\t", "4.0","\n",
                   "a0", "\t", "7.0","\n",
                   "b0", "\t", "3.0","\n",
                   "num_top", "\t", k_val, "\n",
                   "iterations", "\t", "100","\n",
                   "start_time", "\t", start_year, "\n",
                   "end_time", "\t", "2020","\n",
                   "time_interval", "\t", if_else(start_year == 1900, 10, 5), "\n",
                   "min_doc_per_word", "\t", "0", "\n",
                   "max_docs_per_slice", "\t", "2000", "\n"), 
              filename)

}

# Run scan




