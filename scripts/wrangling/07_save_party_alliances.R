# Save party alliance

# Load stuff ------

pacman::p_load(stringr, tidytable, purrr, readr, forcats)

source("scripts/wrangling/0607_party_functions.R")


# NB: fread. does not handle NA values in strings very well

input_fp <- 'repr_rights_data/04_model_inputs/repr_rights_hans_so300tkn_w_id.csv'

df <- read_csv(input_fp) %>% 
  mutate.(party = recode(party, "LNP" = "Nats"))

pollies_by_party <- read_csv("terms/politicians_by_party.csv")


party_equivs <- read_csv("terms/party_equivs_cleaned.csv")

libs_abbre <- party_equivs %>% 
  filter(main_abbre == "Liberal") %>% 
  pull(orig_abbre) %>% 
  cptools::bound_rx()

replace_party_vec <- get_equivs(party_equivs, orig_abbre, main_abbre)

old_party_rx <- cptools::bound_rx(party_equivs$orig_abbre, 
                                  by_element = T)

replace_party_vec

# main filtering function ----

select_one_party <- function(p) {
  
  if(is.na(p) || !str_detect(p, ";")) {return(p)}
  

  detected <- unlist(str_extract_all(p, old_party_rx))
  
  converted <- unique(str_replace_all(detected[detected != ""], replace_party_vec))
  
  if(length(converted) == 1) { 
    
    converted 
    
  } else if (length(converted) > 1 && all(str_detect(converted, 
                                                     "Country|Liberal"))) {
    
    # browser()
    
    # If they're the same, default to Liberal
    
    
    
    if(sum(str_detect(detected, libs_abbre)) <= 
       sum(!str_detect(detected, libs_abbre))) {
      
      # browser()
      
      return("Country") 
      
      
      } else return("Liberal")
    

  } else {
    
    "Multiple party alliances" 
    
  }
  
}


full2main_abbre <- fread.("terms/party_equivs_cleaned.csv")

rep_full2main_abbre <- full2main_abbre$main_abbre

names(rep_full2main_abbre) <- full2main_abbre$orig_full


rep_orig_abbre2main_abbre <- full2main_abbre$main_abbre

names(rep_orig_abbre2main_abbre) <- full2main_abbre$orig_abbre


sum(is.na(df$party))


one_party_df <- df %>% 
  mutate.(orator = str_to_title(orator)) %>% 
  left_join.(pollies_by_party, by = "orator") %>% 
  mutate.(combo_party = coalesce.(party, found_party),
    one_party = map_chr(combo_party, select_one_party),
    final_party = dplyr::recode(one_party, !!!rep_full2main_abbre),
    alliance = dplyr::recode(final_party, !!!rep_orig_abbre2main_abbre))

find_holt(one_party_df)

para_counts <- one_party_df %>% 
  mutate.(period = cut(as.numeric(year), c(1900, 1990, 2021), dig.lab = 5)) %>% 
  distinct.(alliance, main_text, period) %>% 
  count.(alliance, sort = T) %>% 
  print()


one_party_df %>% 
  filter.(alliance == "") %>%
  distinct(alliance, party, one_party, found_party, combo_party)


final_df <- one_party_df %>% 
  filter.(!is.na(final_party)) %>% 
  rename.(original_party = party) %>% 
  select.(-combo_party, -one_party, -found_party) %>% 
  mutate.(final_alliance = fct_collapse(alliance, Other = 
                                         c("Multiple party alliances",
                                           "Independent", 
                                           "Other right", "Other centre")))

final_df %>% 
  mutate.(period = cut(as.numeric(year), c(1900, 1990, 2021), dig.lab = 5)) %>% 
  distinct.(final_alliance, main_text, period) %>% 
  count.(final_alliance, sort = T) %>% 
  print()

odd_parties <- final_df %>% 
  filter.(!final_alliance %in% c("Labor", "Liberal", "Other", "Country", "Greens"))

stopifnot(nrow(odd_parties) < nrow(final_df)*0.001)


out_df <- final_df %>% 
  filter.(final_alliance %in% c("Labor", "Liberal", "Other", "Country", "Greens")) %>% 
  droplevels()
  


fwrite.(out_df, str_replace(input_fp, ".csv", "_parties.csv"))


iwalk(split(out_df, out_df$final_alliance),
      ~fwrite.(.x, paste0(str_replace(input_fp, "tkn", paste0("tkn_", .y)))))

