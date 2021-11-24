#! /usr/bin/Rscript
# R 4.0.3
# 28 June 2021
# Alfonso Martínez Arranz

# Get a single orator

pacman::p_load(tidyverse, tidytable, tidytext, lubridate)


# Load files (see also 04_categorisation.R) -------------

cat("Loading raw file...\n")


cargs <- commandArgs(trailingOnly = T)

main_folder <- cargs[1]

mentions_str <- cargs[2]

if (length(cargs) == 0) {
  
  main_folder <- 'repr_rights'
  
  mentions_str <- "abort|child|contraceptive|miscarriage|rape|reproductive|health|women|woman"
  
  # coal topics: coal|lignite|coalmining
  
  
}

main_path <- paste0(main_folder, "_data/04_model_inputs/")

floor_thirty <-  function(value) value - value %% 30
  

full_raw <- read_csv(paste0(main_path, main_folder, "_full_downloaded.csv")) %>% 
  mutate.(year = year(ymd(date)),
          char_length = nchar(str_remove(main_text, "\\s{2,}")),
          mentions = str_count(tolower(main_text), mentions_str),
          tridecade = floor_thirty(year),
          type = str_extract(database, c("Hansard|Reports|Committees")),
          across.(c(responder, electorate, speaker, questioner), str_to_title)) %>%
  filter.(!is.na(title), !is.na(main_text), mentions != 0) %>%  # Only a handful
  filter.(case_when.(char_length < 250 ~ FALSE,
                   type %in% c("Committees", "Reports") & char_length < 5000 ~ FALSE,
                   TRUE ~ TRUE
))

full_raw %>% 
  count.(type) %>% 
  print()

hansard_only <- full_raw %>% 
  cptools::rm_list_cols() %>% 
  janitor::remove_empty("cols") %>% 
  select(-page, -question_no) %>% 
  filter(!str_detect(database, c("Reports|Committees"))) 



find_speakers <- function(v) { 
  
  str_detect(v, regex(cptools::bound_rx(c("DEPUTY SPEAKER, The", "SPEAKER, The"),
                                        leftbound = "^",
                                        rightbound = "$"),
                      ignore_case = T))
}


# -----
single_orator <- hansard_only %>% 
  mutate(interjector = ifelse.(find_speakers(interjector),
                               NA_character_,
                               interjector)) %>% 
  filter.((if_all.(c(questioner, responder), is.na) & !is.na(speaker)) |
            (if_all.(c(interjector, speaker, responder), is.na) & !is.na(questioner)) |
            (if_all.(c(interjector, questioner, speaker), is.na) & !is.na(responder))) %>% 
  mutate.(orator = coalesce.(speaker, responder, questioner)) %>% 
  select.(-c(speaker, responder, questioner, interjector, 
             tridecade, char_length, type, source, mentions)) 




fwrite.(single_orator, paste0(main_folder, "_data/04_model_inputs/", 
                              main_folder, "_hans_so.csv"))



