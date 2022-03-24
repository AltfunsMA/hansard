

# basic regexes ----

TITLE <- c('Mr', 'Sir', 'Senator', 'Mrs', 'Ms', 'Dr', 
           'Dame', 'Madam', "Colonel", "Major")

TITLE_SP <- cptools::bound_rx(paste0(TITLE, " "), leftbound = "^")

TITLE_SP


boilerplate <- "monday|tuesday|wednesday|thursday|friday|prayers|resumed|order"


det <- function(s, p) str_detect(str_remove_all(s, "'"), p)

det_chair <- function(s) str_detect(s, "speaker|president|chairman|clerk")



procedure_rx <- "\\bayes\\b|a list of ministers and the offices they hold"



# Choosing orators ------

test_comprehensiveness <- function(df) {
  
  df %>%
    filter.(.by = permalink, !any(row_matches_orator)) %>%
    filter.(!(!!! useless_entries |
                (len > 100 & tolower(speaker_surname) %in% pollies_df$surname))) %>%
    nest.(not_id = -permalink) %>%
    mutate.(temp_id = row_number()) %>%
    unnest.(not_id)
  
}




orator_basic_cases <- function(df) {
  

  df %>% 
    mutate.(across.(c(speaker, questioner, responder, interjector),
                    ~na_if(.x, "")), 
            
            orator = case_when.(
              !is.na(speaker) & is.na(speaker_surname) ~ speaker,
              
              # Cases where detection and metadata agree
              det(speaker, speaker_surname) ~ speaker,
              det(questioner, speaker_surname) ~ questioner,
              det(responder, speaker_surname) ~ responder,
              row_matches_interjector & !is.na(recon_speaker) ~ recon_speaker,
              !is.na(interjector) & is.na(speaker) & is.na(questioner) & is.na(responder) 
              & !is.na(recon_speaker)
              ~ recon_speaker,
              det(main_text, "ask") & !is.na(questioner) ~ questioner,
              det(main_text, "answer") & !is.na(responder) ~ responder,
              
              # Cases involving the chair
              
              speaker %in% chairs_df$list_name ~ recon_speaker, # Likely mislabelled
              
              
              det_chair(speaker) ~ recon_speaker,
              
              det_chair(interjector) ~ "__citizen initiatives__",
              det_chair(responder) ~ "__procedural inquiries__",
              
              
              det_chair(speaker_surname) & 
                grepl("standing order|order!", main_text) & len < 500 
              ~ "__chair order call__",
              
              
              det_chair(speaker_surname) ~ "__other_intervention_from_chair__",
              
              # Other cases to handle later
              str_count(main_text, "\\bminister\\b|\\bbill\\b") > 15 
              ~ "__enumeration__",
              one_main_orator & !is.na(recon_speaker) ~ NA_character_,
              one_main_orator ~ coalesce(questioner, responder, speaker),
              empty_orator & !is.na(recon_speaker) ~ recon_speaker,
              
              TRUE ~ NA_character_)
    )
  
}



fix_missing_orators <- function(temp_id, main_text, speaker, 
                                questioner, responder, interjector,
                                recon_speaker, len, orator) {
  
  
  qanda <- all(is.na(speaker) & !is.na(questioner) & !is.na(responder))
  
  non_na_orator <- orator[!is.na(orator)]
  
  
  if(qanda && length(non_na_orator) > 0) {
    
    # Are both already covered
    
    participants <- unique(c(responder, questioner))
    
    both_covered <-  all(det(participants, cptools::bound_rx(non_na_orator)))
    
    
    if(both_covered && length(main_text) == 2) {
      
      
      not_covered <- participants[!participants %in% non_na_orator]
      
      
      replace_na(orator, not_covered)
      
      
      
    } else {
      
    out <- coalesce(orator, recon_speaker)
      
    # if(any(is.na(out))) browser()
    
    out
      
    }
    
    
    
  } else {
    
    
    # Check whether the persons identified as speaker was in parliament
    if(all(is.na(speaker))) return(recon_speaker)
    if(all(is.na(recon_speaker))) return(speaker)
    
    
    out <- coalesce(speaker, recon_speaker)
    
    
    # if(any(is.na(out))) browser()
       
       
       out
    
    # recon_speaker[recon_speaker %in% pollies_df$orator]
    # recon_speaker[recon_speaker %in% pollies_df$recon_name]
    # recon_speaker[recon_speaker %in% pollies_df$orator_surname]
    
    
    
    
    # Likely captured some reported speech as if they were different speakers
    
    
  }
  
}



# Formatting-----

format_name <- function(s) {
  
  normal_names <- s[!str_detect(s, fixed(",")) & str_detect(s, fixed(" "))]
  
  if(length(normal_names) == 0) {
    
    warning("No need to format any names")
    
    
    return(s)
    
  }
  
  split_names <- stringi::stri_split_fixed(normal_names, " ", n = 2)
  
  browser()
  
  corrected_names <- map_chr.(split_names, ~paste0(.x[2], ", ", .x[1]))
  
  names(corrected_names) <- normal_names
  
  recode(s, !!!corrected_names)
  
  
}



clean_orator <- function(s) {
  
  
  lower_title <- tolower(cptools::bound_rx(TITLE))
  
  
  s %>% 
    tolower() %>% 
    str_remove_all(lower_title) %>% # Mr Mrs, etc.
    str_remove_all(",?\\bsen |, ?mp\\b") %>% # Other titles
    # brackets, full stops, and apostrophe's
    str_remove_all(" \\(.*?\\)|'|\\.|\\,$") %>% 
    str_squish()
  
  
}



get_initials <- function(s) {
  
  str_match_all(s, "([A-Z])[a-z]+\\b") %>% 
    map(~paste0(.x[,2], collapse = "")) %>% 
    unlist()
  
  
}



alternative_surname <- function(s) {
  
  # Not maintaining Sir ___ because not surnames 
  s %>% 
    str_replace_all(c("nee Bushby; formerly Summers" = "Bushby",
                      "Also Sophie Panopolous" = "Panopolous", 
                      "Also Walter Massy Green; later Sir Walter." = "Green",
                      "Also Kathryn Jean Martin before marriage" = "Martin")) %>% 
    str_remove_all("^nee |Later Sir.*$")
  
  
  
}


unify_electorates <- function(s) {
  
  ausstates <- c("TAS" = "Tasmania",
                 "VIC" = "Victoria",
                 "QLD" = "Queensland",
                 "WA" = "Western Australia",
                 "SA" = "South Australia",
                 "NT" = "Northern Territory",
                 "ACT" = "Australian Capital Territory",
                 "NSW" = "New South Wales")
  
  
  
  s %>% 
    recode(!!!ausstates) %>% 
    tolower() %>% 
    str_remove_all(", ?[a-z ]+$") %>% 
    str_squish()
  
}


# Final assignment ----



assign_pollies <- function(text_df, politicians_df) {
  

  output <- text_df %>% 
    left_join(select(politicians_df, uniqueID, orator),
              by = "orator") %>% 
    left_join(select(politicians_df, uniqueID, recon_name), 
              by = c("orator" = "recon_name")) %>% 
    cptools::replace_xy() %>% 
    left_join(select(politicians_df, uniqueID, common_recon_name), 
              by = c("orator" = "common_recon_name")) %>% 
    cptools::replace_xy() %>% 
    left_join(select(politicians_df, uniqueID, alt_recon_name), 
              by = c("orator" = "alt_recon_name")) %>% 
    cptools::replace_xy() %>% 
    left_join(select(politicians_df, uniqueID, alt_common_recon_name), 
              by = c("orator" = "alt_common_recon_name")) %>% 
    cptools::replace_xy() %>% 
    left_join(select(politicians_df, uniqueID, orator_surname), 
              by = "orator_surname") %>% 
    cptools::replace_xy() %>% 
    left_join(select(politicians_df, uniqueID, recon_initials), 
              by = c("orator" = "recon_initials")) %>% 
    cptools::replace_xy() %>% 
    left_join(select(politicians_df, uniqueID, recon_full_initials),
              by = c("orator" =  "recon_full_initials")) %>% 
    cptools::replace_xy() %>% 
    left_join(select(politicians_df, uniqueID, orator_surname), 
              by = "orator_surname") %>% 
    cptools::replace_xy() %>% 
    distinct.()
  
  
  empty <- output %>% 
    filter(is.na(uniqueID))
  
  wrong <- nrow(empty)/nrow(output)
  
  stopifnot(wrong < 0.03)
  
  
  cat("\nFailed: ", nrow(empty), ", i.e.,", 
      scales::percent(wrong, accuracy = 0.1), "\n\n")
  
  
  cat("Sample failed: \n\n")
  to_sample <- empty %>% 
    pull(orator) %>% 
    unique()
  
  
    sample(to_sample, size = min(5, length(to_sample))) %>% 
    cat(sep = " | ")
  
  cat("\n\n")
  
  output
  
}




