# Making just one big civility dataset file

pacman::p_load(tidytable, lubridate, stringr, purrr, readr, lubridate, ggplot2)

civ_fps <- list.files("civility_data/04_model_inputs/", pattern = "\\d\\.csv", full.names = T)

targets <- read_lines("civility_data/04_model_inputs/targets.txt")


load_minimal <- function(x){
  
  fread.(x, select = c("speaker", "party", "date", "main_text", "database"), 
         colClasses = c("speaker" = "character", 
                        "system_id" = "character",
                        "party" = "character", 
                        "date" = "character", 
                        "main_text" = "character",
                        "database" = "character")) %>% 
  filter.(str_detect(database, regex("hansard", ignore_case = T))) %>% 
    filter.(str_detect(tolower(main_text), cptools::bound_rx(targets, "", "")))

}

df <- map_dfr.(civ_fps, load_minimal) %>% 
  mutate.(year = lubridate::year(ymd(date)))



fwrite.(df, "civility_data/04_model_inputs/civility_full_downloaded.csv")

check_terms <- function(pattern) {
  
  df %>% 
    # slice_sample.(prop = 0.25) %>% 
    mutate.(term_counts := str_count(main_text, fixed(pattern))) %>% 
    summarize.(.by = year, 
               {{pattern}} := sum(term_counts))
  
  
}



all_terms <- unlist(str_split(read_lines('civility_data/04_model_inputs/all_civility_terms.txt'), ","))


res <- lapply(all_terms, check_terms) %>% 
  reduce(left_join.)


plot_df <- res %>% 
  pivot_longer.(-year, names_to = "terms",
                values_to = "counts")


plot_df %>% 
  filter.(!terms %in% c("mary")) %>% 
  mutate.(.by = terms, totals = sum(counts)) %>% 
  mutate.(terms = ifelse.(totals < 20000, "Other", terms)) %>%
  # summarize.(.by = c(terms, year), 
  #            counts = sum(counts)) %>% 
  filter.(terms != "Other") %>% 
  ggplot() +
  geom_line(aes(year, counts, colour = terms)) +
  scale_color_brewer(palette = "Set1")



