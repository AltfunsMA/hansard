# Party functions

pacman::p_load(tidytable, stringr)

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
 
    
  no_comma <- all(str_detect(surname_first_vector, ",", negate = T))
  
  
  if(no_comma) {
    
    warning("No comma in strings ", head(paste(surname_first_vector[no_comma], "\n")), ".\n",
         "Is it really in format 'Surname, Name?'")
    
    return(surname_first_vector)
    
    
  }
  
str_split(surname_first_vector, ",") %>% 
    map_if(~length(.x) == 2, ~paste(str_remove_all(.x[[2]], "(Sen |Senator |Mr |Ms )"), .x[[1]])) %>%  
    unlist() %>% 
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
