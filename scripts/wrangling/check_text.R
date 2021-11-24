# Query actual text

paragraphs_df <- fread.("coal_data/04_model_inputs/preprocessed_datasets/id_alliance_all_2a.csv")


texts_w_topics <- paragraphs_df %>% 
  left_join.(select.(topic_distributions, -c(V1, year)))


  
texts_w_topics %>% 
  filter.(`3` > 0.3) %>% 
  slice_sample.(n=10) %>% 
  pull(main_text)
  
texts_w_topics %>% 
  filter.(str_detect(main_text, "energy policy"), between(year, 1965, 1975),
          final_alliance == "Liberal") %>% 
  pull(main_text)



texts_w_topics %>% 
  filter.(str_detect(main_text, "Great Barrier Reef"), between(year, 2000, 2020), final_alliance == "Greens") %>% 
  pull(main_text)

  
texts_w_topics %>% 
  filter.(`13` > 0.4, between(year, 1965, 1980), final_alliance == "Liberal") %>% 
  pull(main_text)



count_terms <- function(rx, year_start, year_end) {
  
  
counts <- texts_w_topics %>% 
  filter.(between(year, year_start, year_end)) %>% 
  mutate.(counts = str_count(main_text, rx))
  
cat(counts %>% 
  filter.(counts > 0) %>% 
  distinct.(doc_id) %>% 
  nrow(), " documents \n")

cat("total term count", sum(counts$counts))


}
  

count_terms("\\blungs?\\b", 1939, 1949)
count_terms("\\bwages?\\b", 1939, 1949)
count_terms("\\baccidents?\\b", 1939, 1949)
count_terms("\\blong service leave\\b", 1939, 1949)

