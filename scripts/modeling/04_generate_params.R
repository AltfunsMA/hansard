#! /usr/bin/Rscript

suppressPackageStartupMessages({
  
  library(tidyverse)
})

cargs <- commandArgs(trailingOnly = T)

main_topic <- cargs[1]

if(is.na(main_topic)) {
  
  main_topic <- Sys.getenv("HANSARD_MAIN")
  
  if(main_topic == "") main_topic <- "coal"
  
  timestamp <- Sys.getenv("TIMESTAMP")

  if(timestamp == "") timestamp <- cptools::date_str()

}

params_dir <- paste0(main_topic, "_data/05_scan_parameters/")

output_dir <- paste0(main_topic, "_output/scan_", timestamp, "/")


# Create SCAN parameter combinations -----

message("Generating parameter combinations")


k <- c(10, 30)
sy <- c(1900, 1980)
w <- c(5, 10)
k_F <- 100
k_K <- 4
a0 <- 7
b0 <- 3

# k <- 5
# sy <- 1980
# w <- 5
# k_F <- 100
# k_K <- 4
# a0 <- 7
# b0 <- 3


par_list <- list(k, sy, w, k_F, k_K)

names(par_list) <- c("k_val", "start_year", "window", "kappaF", "kappaK")

params <- cross(par_list)

unlink(output_dir, recursive = T)

dir.create(output_dir)

message("Cleaned output folder: ", output_dir)

unlink(params_dir, recursive = T)

dir.create(params_dir)

message("Cleaned out folder: ", params_dir)


for (p in params) {

    
  start_year <- p['start_year']
  k_val <- p['k_val']
  window <- p['window']
  k_F <- p['kappaF']
  k_K <- p["kappaK"]
  
  
  pars <- paste0("k", k_val, "_w", window, "_", start_year, "_F", k_F, "_kK", k_K)

  new_dir <- paste0(output_dir, pars)

  if(!dir.exists(new_dir)) dir.create(new_dir)

  
  filename <- paste0(params_dir, "parameters_", pars, ".txt")  

  write_lines(paste0("text_corpus", "\t", "./", main_topic, "_data/04_model_inputs/corpus_bigrammed.txt", "\n",
                   "target_words", "\t", "./", main_topic, "_data/04_model_inputs/targets.txt", "\n",
                   "bin_corpus_store", "\t", "./", main_topic, "_data/04_model_inputs/corpus.bin", "\n",
                   "window_size", "\t", window, "\n",
                   "full_corpus_path", "\t", "./", main_topic, "_data/04_model_inputs/corpus.bin", "\n",
                   "word_corpus_path", "\t", "./", new_dir, "\n",
                   "output_path", "\t", "./", new_dir, "\n",
                   "kappaF", "\t", k_F,"\n",
                   "kappaK", "\t", k_K, "\n",
                   "a0", "\t", a0, "\n",
                   "b0", "\t", b0,"\n",
                   "num_top", "\t", k_val, "\n",
                   "iterations", "\t", "500","\n",
                   "start_time", "\t", start_year, "\n",
                   "end_time", "\t", "2020","\n",
                   "time_interval", "\t", if_else(start_year == 1900, 10, 5), "\n",
                   "min_doc_per_word", "\t", "0", "\n",
                   "max_docs_per_slice", "\t", "1000", "\n"), 
              filename)

}


message("Generated ", length(params), " files...")


message('DONE')



# TODO: Create DTM params
# 
# full_command <- paste("hansard/dtm/dtm/main", 
#                       "--ntopics=20",  
#                       "--mode=fit", 
#                       "--rng_seed=0", 
#                       "- --ntopics=20", 
#                       "--corpus_prefix=hansard/civility_data/civility",
#                       "--outname=hansard/civility_output/dtm",
#                       "--top_chain_var=0.005",
#                       "--alpha=0.1",
#                       "--lda_sequence_min_iter=6",
#                       "--lda_sequence_max_iter=20",
#                       "--lda_max_em_iter=10")
# 
# 
# system(full_command)
# 
# 
