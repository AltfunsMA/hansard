#! /usr/bin/Rscript
# Alfonso Martinez Arranz
# 13 March 2022
# Run ATM keywords


pacman::p_load(quanteda, quanteda.textstats, tidytable, lubridate, tidyverse, keyATM, tictoc)

# Read in and create corpus ----
message("Key ATM script started at ", now("Australia/Melbourne"), " Melbourne time")

tic("Load basic data")

comarg <- commandArgs(trailingOnly = T)

if(length(comarg) == 0) in_propor <- 0.08 else in_propor <- as.numeric(comarg)

lemmatise_kw <- function(s) {
  
  s %>% 
    str_split("_") %>% 
    tokens() %>% 
    tokens_replace(pattern = lexicon::hash_lemmas$token, 
                   replacement = lexicon::hash_lemmas$lemma) %>% 
    map(~paste(.x, collapse = "_")) %>% 
    unlist(use.names = FALSE)

  
}


parl_num <- c(40)


# if(length(comarg) == 0) {
#   
#   min_freq <- 15 
#   
#   
# } else { 
  
  min_freq <- 150
  
  max_freq_opts <- c(10000, 50000)
  
  bigram_opts <- c(TRUE, FALSE)
  
  
# }


keywords_raw <- fread.("general_data/cleaned_rev3_terms.csv") %>% 
  as.list() %>% 
  map(~.x[!is.na(.x)])%>% 
  map(~.x[.x != ""])

  
  
keywords <- keywords_raw %>% 
  map(lemmatise_kw) 

df <- fread.(paste0("general_data/04_clean_text_by_parl/parl_", parl_num, ".csv"))

# if(length(comarg) == 0) stop("To run full loop run with argument from a terminal")

df_filtered <- df %>% 
  mutate.(party = fct_collapse(party_simplified_name, 
                               Coalition = c("Liberals", "Nationals", "Country Liberal Party (Northern Territory)"),
                               Others = c("Australian Greens", "Australian Democrats", "Independent", ""))) %>% 
  filter.(party %in% c("Coalition", "Labor", "Others")) %>% 
  mutate.(year = year(dmy(date))) %>% 
  slice_sample.(prop = in_propor)



bigrammise_main_text <- function(txt) {
  
  bigram_kw <- unlist(keywords_raw, use.names = F) %>% 
    `[`(., str_detect(., "_"))
  
  
  bigram_kw_changed <- bigram_kw %>% 
    str_replace("_", " ")
  
  names(bigram_kw_changed) <- bigram_kw
  
  
  str_replace_all(txt, bigram_kw_changed)
  
  
  
}



toc()
  
for(max_freq in max_freq_opts) {
  
  for(bigram_after in bigram_opts) {
    
    
    tic("Create quanteda corpus")

    
    message("Options:\n", 
            "* max_freq = ", max_freq, "\n",
            "* bigram after = ", bigram_after)
    
    
    
    if(!bigram_after) {
      
      
    df_filtered$main_text <- bigrammise_main_text(df_filtered$main_text)
      
    
      
  }
    

    corp_parl <- corpus(df_filtered, text_field = "main_text")
    


toc()
  


# Tokenise (~ 1.3 minutes) -----

exclusion_terms <- read_lines("/data/hansard/terms/simple_exclusion_terms.txt")

tic("Tokenise and ngramise")
data_tokens <- tokens(corp_parl,
                      remove_punct = T,
                      remove_numbers = T,
                      remove_symbols = T,
                      remove_separators = T,
                      remove_url = T) %>% 
  tokens_tolower() %>% 
  tokens_replace(pattern = lexicon::hash_lemmas$token, 
                 replacement = lexicon::hash_lemmas$lemma) %>% 
  tokens_remove(c(stopwords::data_stopwords_stopwordsiso$en, exclusion_terms)) %>% 
  tokens_select(min_nchar = 3)


if(bigram_after) {
  
  bad_bigrams <- str_trim(read_lines("/data/hansard/terms/bad_bigrams_ausparl.txt"))
  write_lines(sort(bad_bigrams), "/data/hansard/terms/bad_bigrams_ausparl.txt")
  
  government_names <- paste0(read_lines("/data/hansard/terms/aus_pm_names.txt"), "_government")

data_tokens <- data_tokens %>% 
  tokens(remove_numbers = T) %>%
  tokens_ngrams(n = 2L) %>%
  tokens_remove(c(bad_bigrams, government_names))
  
} 
  
  
toc()




# Document matrix (~ 7 secs) ------
tic("Create document matrix")
data_dfm <- dfm(data_tokens) %>%
  dfm_trim(min_termfreq = min_freq, min_docfreq = 2, max_termfreq = max_freq) %>% 
  dfm_subset(., ntoken(.) > 0)
toc()


if(interactive()) { freq_terms <- textstat_frequency(data_dfm, n = 200)

freq_terms

arrange(freq_terms, rank)


}

# Reading into keyATM_docs (~ 4 minutes) ----

tic("Read elements")
keyATM_docs <- keyATM_read(texts = data_dfm)

summary(keyATM_docs)



vars <- docvars(data_dfm) %>% 
  as_tibble()


vars_selected <- vars %>% 
  select(year, party) %>% 
  droplevels()

toc()

# Modelling (~10 minutes for ~6479 observations) -------------
# (~22.5 minutes for 13883 observations)


future::plan("multicore")

tic("Modelling")
out <- keyATM(docs = keyATM_docs,
       no_keyword_topics = 0,
       keywords = keywords,
       model = "covariates",
       model_settings = list(covariates_data = vars_selected,
                             covariates_formula = ~ party + year),
       options = list(parallel_init = TRUE))

toc()

out_path <- paste0("/data/hansard/general_data/keyATM_res/parl_", parl_num, 
                   "_test_min_freq_", min_freq, "_bigram_", bigram_after, ".rds")


write_rds(out, out_path)

message("Saved ", out_path)


  print(top_words(out))



  
}

  
  
    
}

