# FUNS ------

test_comprehensiveness <- function(df) {
  
  df %>%
    filter.(.by = permalink, !any(row_matches_orator)) %>%
    filter.(!(!!! useless_entries |
                (len > 100 & tolower(speaker_surname) %in% polsurnames))) %>%
    nest.(not_id = -permalink) %>%
    mutate.(temp_id = row_number()) %>%
    unnest.(not_id)
  
}



clean_orator <- function(s) {
  
  
  lower_title <- tolower(cptools::bound_rx(TITLE))
  
  
 s %>% 
   tolower() %>% 
   str_remove_all(lower_title) %>% # Mr Mrs, etc.
    str_remove_all(",?\\bsen |, ?mp\\b") %>% # Other titles
   str_remove_all(" \\(.*?\\)|'|\\.") %>% # brackets, full stops, and apostrophe's
   str_squish()
   
  
}



orator_basic_cases <- function(df) {
  

  df %>% 
    mutate.(across.(c(speaker, questioner, responder, interjector),
                    ~na_if(.x, "")), 
            
            orator = case_when.(
              !is.na(speaker) & is.na(speaker_surname) ~ speaker,
              
              # Normal cases
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
              !det(interjector, speaker_surname) & !is.na(speaker) 
              ~ "__interjection_w/o_interjector__",
              one_main_orator ~ coalesce(questioner, responder, speaker),
              empty_orator & !is.na(recon_speaker) ~ recon_speaker,
              
              TRUE ~ NA_character_)
    )
  
}



format_name <- function(s) {
  
  normal_names <- s[!str_detect(s, fixed(",")) & str_detect(s, fixed(" "))]
  
  
  split_names <- stringi::stri_split_fixed(normal_names, " ", n = 2)
  
  # browser()
  
  corrected_names <- map_chr.(split_names, ~paste0(.x[2], ", ", .x[1]))
  
  names(corrected_names) <- normal_names
  
  str_replace_all(s, fixed(corrected_names))
  
  
}

get_initials <- function(s) {
  
  str_match_all(s, "([A-Z])[a-z]+\\b") %>% 
    map(~paste0(.x[,2], collapse = "")) %>% 
    unlist()
  
  
}


assign_pollies <- function(text_df, politicians_df) {
  
  parliament <- unique(text_df$parl_no)
  
  start_date <- parl_start_dates[names(parl_start_dates) == parliament]
  
  cat("Parliament start date:", format(start_date, "%d %b %Y"))
  
  if(parliament == 46) { # Last parliament when study carried out ca. 2022
    
    end_date <- ymd("2022-03-01")
    
  } else {
    
    end_date <- parl_start_dates[names(parl_start_dates) == parliament + 1]
    
  }
  
  pols_filt_df <- politicians_df %>% 
    filter(start_date <= date_to & end_date >= date_from)
  
  
  
  output <- text_df %>% 
    left_join(select(pols_filt_df, uniqueID, orator),
              by = "orator") %>% 
    left_join(select(pols_filt_df, uniqueID, recon_name), 
              by = c("orator" = "recon_name")) %>% 
    cptools::replace_xy() %>% 
    left_join(select(pols_filt_df, uniqueID, recon_initials), 
              by = c("orator" = "recon_initials")) %>% 
    cptools::replace_xy() %>% 
    left_join(select(pols_filt_df, uniqueID, recon_full_initials),
              by = c("orator" =  "recon_full_initials")) %>% 
    cptools::replace_xy() %>% 
    left_join(select(pols_filt_df, uniqueID, orator_surname), 
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
  empty %>% 
    pull(orator) %>% 
    unique() %>% 
    sample(5) %>% 
    cat(sep = " | ")
  
  cat("\n\n")
  
  output
  
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
    
    coalesce(orator, recon_speaker)
    
    
  }
  
  
  
  } else {
    
    # Likely captured some reported speech as if they were different speakers
    
    coalesce(speaker, recon_speaker)
    
  }
  
}

