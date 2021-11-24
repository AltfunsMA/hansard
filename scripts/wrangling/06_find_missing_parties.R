#! /usr/bin/Rscript
# Alfonso Martínez Arranz

library(tidyverse)
library(lubridate)
library(WikidataR)
source("scripts/wrangling/0607_party_functions.R")

# Set-up ------
download_new <- FALSE

cargs <- commandArgs(trailingOnly = T)


main_folder <- cargs[1]


if (length(cargs) == 0) {
  
  main_folder <- 'repr_rights'
  
}


na_strings <- c("unknown", "N/A", "NA", "")

df <- read_csv(paste0(main_folder, '_data/04_model_inputs/', main_folder, 
                      '_single_orator_300tkn_para.csv'),
               na = na_strings)

full_df <- read_csv(paste0(main_folder, '_data/04_model_inputs/', main_folder, 
                           '_categorised.csv'),
                    col_types = cols_only(responder = "c",
                                          electorate = "c", 
                                          party = "c", 
                                          speaker = "c", 
                                          questioner = "c", 
                                          date = "c"),
                    na = na_strings)

# From Hansard data itself --------


pols_by_party_path <- "terms/politicians_by_party.csv"

pollies_by_party <- fread.(pols_by_party_path)


message('Querying Hansard data...')

missing_party <- df %>% 
  select(-c(permalink, n_tokens, main_text)) %>% 
  filter(is.na(party), !is.na(orator)) %>% 
  distinct(electorate, year, orator) 


known_party <- full_df %>% 
  mutate.(party = recode(party, "LNP" = "Nats")) %>% 
  distinct(speaker, party, date, electorate) %>% 
  filter(!is.na(party), !is.na(speaker)) %>% 
  mutate(orator = speaker,
         year = year(ymd(date))) %>% 
  distinct(electorate, party, year, orator) 



# by_elect_year <- missing_party %>% 
#   select(-orator) %>% 
#   arrange(year) %>% 
#   match_missing_parties_by(electorate, year) 

varying_party_officials <- c("Speaker, Mr",
               "The President",
               "Mr Speaker", "Mr Deputy Speaker",
               "The Honourable Narendra Modi",
               "President, The",
               "Chairman, The")


by_orator <- missing_party %>% 
  select(-electorate, -year) %>% 
  filter(!orator %in% varying_party_officials) %>% 
  match_missing_parties_by(orator)



lnp_removed <- df %>% 
  mutate.(party = recode(party, "LNP" = "Nats"))

first_check <- lnp_removed %>% 
  distinct(orator, party, electorate, year) %>% 
  mutate(across(c(electorate, orator), str_to_title)) %>% 
  left_join(by_orator, by = "orator") %>% 
  # left_join(by_elect_year, by = c("electorate", "year")) %>% 
  mutate(found_party = coalesce(party, 
                                # found_party.x, 
                                found_party)
         ) %>% 
  distinct(orator, found_party)

second_check <- first_check %>%
  filter(is.na(found_party)) %>% 
  distinct(orator) %>% 
  left_join(filter(first_check, !is.na(found_party)), by = "orator")


# From Wikidata ------
message('Querying Wikidata...')

recalcitrant <- second_check %>% 
  filter(is.na(found_party), !orator %in% varying_party_officials) %>% 
  filter.(str_detect(orator, fixed(","))) %>% 
  mutate(orator_wikified = wikify_person_name(orator))


message("Getting politicians' data...")

if(download_new) {
  
found_wiki <- map(recalcitrant$orator_wikified, find_item) 

orig_orator_names <- unlist(map2(recalcitrant$orator, lengths(found_wiki),
                                 ~rep.int(.x, .y)))

new_data <- flatten(found_wiki) %>% 
  set_names(orig_orator_names) %>% 
  keep(~str_detect(.x['description'], "Australian politician")) %>% 
  map_chr("id") %>% 
  map(get_item)


existing_data <- readRDS("terms/wikidata/aus_pollies_dl_data.rds")

new_data %>% 
  c(existing_data) %>% 
  write_rds("terms/wikidata/aus_pollies_dl_data.rds")

}


stored_wiki_data <- readRDS("terms/wikidata/aus_pollies_dl_data.rds")


message("Getting party data...")

parties_id_by_polly <- flatten(stored_wiki_data) %>% 
  map(list("claims", "P102", "mainsnak", "datavalue", "value", "id"))

if(download_new) {
  
parties_data <- parties_id_by_polly %>% 
  unlist() %>% 
  unique() %>% 
  map(find_item) %>% 
  flatten()

write_rds(parties_data, "terms/wikidata/aus_party_dl_data.rds")

}

parties_data <- read_rds("terms/wikidata/aus_party_dl_data.rds")


# Converting party IDs into names and into common abbreviations ---------

tibble(id = map_chr(parties_data, "id"),
  wiki_name = map_chr(parties_data, "label")) %>% 
  write_csv("terms/wikidata/aus_parties_raw.csv")

party_equivs <- read_csv("terms/party_equivs_cleaned.csv",
                           na = na_strings)

id2nm_2 <- read_csv("terms/wikidata/aus_parties_raw.csv") %>% 
  left_join(select(party_equivs, -orig_abbre), c("wiki_name" = "orig_full"))

id2abbre_df <- read_csv("terms/wikidata/aus_parties.csv")

rep_id2abbre <- id2abbre_df$main_abbre

names(rep_id2abbre) <- id2abbre_df$id

new_parties_by_polly <- parties_id_by_polly %>% 
  compact() %>% 
  map(~recode(.x, !!!rep_id2abbre)) %>% 
  map(~unique(.x[!is.na(.x)])) %>% 
  map_chr(paste0, collapse = "; ") %>% 
  enframe("orator", "found_party")


# Combining findings and saving ------

message("Saving...")

final_df <- new_parties_by_polly %>% 
  bind_rows(first_check, second_check, pollies_by_party) %>%
  filter(!is.na(found_party), found_party != "") %>% 
  distinct() %>% 
  mutate(party_history = nchar(found_party)) %>% 
  slice_max.(.by = orator, party_history) %>% 
  select(-party_history)
  

if(nrow(final_df) > nrow(pollies_by_party)) write_csv(final_df, pols_by_party_path)

message("Saved under ", pols_by_party_path)

# Check
original_missing <- missing_party %>% 
  distinct(orator) %>% 
  nrow()

now_missing <- missing_party %>% 
  distinct(orator) %>% 
  left_join(final_df, by = "orator") %>% 
  filter(is.na(found_party)) %>% 
  nrow() 

message(scales::percent(now_missing/original_missing), " of missing parties still missing")


