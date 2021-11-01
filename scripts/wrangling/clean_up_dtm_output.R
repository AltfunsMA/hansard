
library(tidyverse)

path_in <- "coal_output/dtm/run_21Sep/all_2a_min_freq_150_21_09_21/analysis/"

x <- readLines(paste0(path_in, "k40_var0.05_top_terms_ot.txt"))


change_years <- function(original_number, year) {
  
  x <<- gsub(paste0("^", original_number, "\t"), paste0(year, " "), x)
  
  
}

walk2(0:23, seq(1900, 2015, 5), change_years)

topic_num_equiv <- fread.(paste0(path_in, "k40_var0.05_manual.csv")) %>% 
  select(topic_no, label) %>% 
  mutate(label = paste0(topic_no, "_", label)) %>% 
  deframe()

names(topic_num_equiv) <- paste0("topic ", names(topic_num_equiv), "\\b")



y <- str_replace_all(x, topic_num_equiv)



writeLines(y, paste0(path_in, "cleaned_k40_var0.05.txt"))
