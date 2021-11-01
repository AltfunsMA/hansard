# Party functions

library(tidytable)
library(stringr)

query_party <- function(party_name) {
  
  df %>% 
    filter.(str_detect(party, paste0("\\b", {{ party_name }}, "\\b"))) %>% 
    distinct.(party, orator, electorate, year)
  
  
}

get_equivs <- function(DF, from, to) {
  
  
  out <- pull.(DF, {{ to }})
  
  from_rx <- cptools::bound_rx(pull.(DF, {{ from }}), 
                               by_element = T)
  
  names(out) <- from_rx
  
  out
  
  
}


wikify_person_name <- function(surnms_first) {
  
  no_comma <- str_detect(surnms_first, ",", negate = T)
  
  if(sum(no_comma) > 0) {
    
    stop("No comma in strings ", paste(surnms_first[no_comma], "|"), ".\n",
         "Is it really in format 'Surname, Name?'")
    
    
  }
  
  str_split(surnms_first, ",") %>% 
    map_chr.(~paste(str_remove_all(.x[[2]], "(Sen |Senator |Mr |Ms )"), .x[[1]])) %>% 
    str_squish() %>% 
    str_replace("&Rsquo;", "'")
  
  
}


match_missing_parties_by <- function(DF, ...) {
  
  library(tidyverse) # cant' use summarize.(.by = !!!mp_or_electorate_id_vars, ...)
  
  mp_or_electorate_id_vars <- enquos(...)
  
  DF %>% 
    left_join(known_party) %>% 
    filter(!is.na(party)) %>% 
    rename(found_party = party) %>% 
    distinct(!!!mp_or_electorate_id_vars, found_party) %>% 
    group_by(!!!mp_or_electorate_id_vars) %>% 
    summarize(found_party = paste0(found_party, collapse = "; "), 
              .groups = "drop")%>% 
    filter(complete.cases(.))
  
}


find_holt <- function(df) {
  
  df %>% 
    filter.(str_detect(orator, regex("holt, harold", ignore_case = T))) %>% 
    distinct.(orator, contains("party"), contains("year"))
  
  
}
