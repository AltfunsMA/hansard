#! /usr/bin/Rscript

## Assign party to each section ----


pacman::p_load(tidyverse, tictoc, pbmcapply, lubridate, tidytable)

source("scripts/wrangling/04_choose_orator_FUNS.R")

test <- FALSE


split_text_path <- "general_data/03_split/"

fp <- list.files(split_text_path, pattern = "csv$", full.names = T)

out_path <- "general_data/04_clean_text_by_parl/"


per_wrong_stats_fp <- paste0(out_path, "perc_wrong_stats.log")

problematic_orator_fp <- paste0(out_path, "problematic_orator.log")

duped_parties_fp <- paste0(out_path, "duped_parties4orator.log")

if(test) message("Operating under test settings") 


invisible({
  
  file.remove(per_wrong_stats_fp)
  file.remove(duped_parties_fp)
  file.remove(problematic_orator_fp)
  
})




# Load permanent politicians data -------------
# Pre-saved from the various datasets in AustralianPoliticians::get_auspol()

auspol_all <- fread.("terms/auspol_all.csv")


auspol_party <-  fread.("terms/auspol_party.csv")


auspol_divisions <- fread.("terms/auspol_div_mps.csv") %>% 
  select(uniqueID, electorate = division, 
         date_from = mpFrom, date_to = mpTo)

auspol_senate <- fread.("terms/auspol_senate.csv") %>% 
  select(uniqueID, electorate = senatorsState, 
         date_from = senatorFrom , date_to = senatorTo)

auspol_electorates <- bind_rows.(auspol_divisions, auspol_senate)


chairs <- fread.("terms/speaker.csv")

deputy_chairs <- fread.("terms/deputy_speaker.csv")


chairs_df <- bind_rows(chairs, deputy_chairs) %>% 
  mutate(full_name = str_remove_all(name, "( Jr\\.| Sr\\.)"),
         surname = tolower(str_extract(full_name, "[-A-Za-z']+$")),
         first_name = tolower(str_extract(full_name, "^[-A-Za-z']+")),
         list_name = paste0(surname, ", ", first_name)) %>% 
  select(list_name, full_name, party) %>% 
  distinct()



all_pollies_df <- auspol_all %>% 
  mutate(orator = clean_orator(displayName),
         recon_name = clean_orator(paste0(surname, ", ", firstName)),
         common_recon_name = clean_orator(paste0(surname, ", ", commonName)),
         orator_surname = clean_orator(surname),
         alt_orator_surname = alternative_surname(earlierOrLaterNames),
         alt_recon_name = clean_orator(paste0(alt_orator_surname, ", ", firstName)),
         alt_common_recon_name = clean_orator(paste0(surname, ", ", commonName)),
         initials = get_initials(firstName),
         full_initials = get_initials(allOtherNames),
         recon_initials = clean_orator(paste0(surname, ", ", initials)),
         recon_full_initials = clean_orator(paste0(surname, ", ", full_initials))) %>% 
  select(-gender, -matches("wiki|adb|comments|title")) %>% 
  left_join(auspol_electorates, by = "uniqueID") %>% 
  mutate(electorate = unify_electorates(electorate),
  # Below exclusion when filtering by term in office
         date_to = replace_na(date_to, ymd("2022-03-01"))) 
  


# Load permanent metadata ----


# NB: Presenter variable is all NA
identifying_info <- fread.("general_data/01_records/combined_general_records.csv",
                        select= c("Permalink", "Date", "Electorate", "Interjector",
                                  "Parl No.",
                                  "Questioner", "Responder", "Speaker", "Database")) %>% 
  janitor::clean_names()



# 

parl_start_dates <- identifying_info %>% 
  distinct.(date, parl_no) %>% 
  mutate.(date = dmy(date)) %>% 
  slice_min.(.by = parl_no, date) %>% 
  relocate(parl_no, date) %>% 
  deframe()



## Main loop extracting split textual data -----

if(test) {
  
  test_parl_no <- 31
  
  parl_start_dates <- parl_start_dates[names(parl_start_dates) %in% c(test_parl_no, test_parl_no + 1)]
  
} 

for (parl_no in names(parl_start_dates)) {

  cat("### Parliament number:", parl_no, " ###\n")
  
  parl_no <- as.integer(parl_no)
  
  fp_as_dates <-  ym(str_remove(basename(fp), "\\.csv$"))
  
  start_date <- parl_start_dates[names(parl_start_dates) == parl_no]
  
  cat("Parliament start date:", format(start_date, "%d %b %Y"), "\n")
  
  if(parl_no == 46L) { # Last parliament when study carried out ca. 2022
    
    end_date <- ymd("2022-03-01")
    
  } else {
    
    end_date <- parl_start_dates[names(parl_start_dates) == (parl_no + 1L)] - months(1)
    
  }
  
  
  cat("Next parliament start date:", format(end_date, "%d %b %Y"), '\n')
  
  files_within_interval <- start_date <= fp_as_dates & end_date > fp_as_dates
  

  
  one_parliament <- fp[files_within_interval]
    
  cat("Range of dates selected:", 
      format(range(ym(str_remove(basename(one_parliament), "\\.csv$"))),
             "%d-%b-%Y"),
      "\n")  
  
  # Load data for one parliament period ----
  
  
  main_split_df <- map_dfr.(one_parliament, fread.)
  
  merged_df <- identifying_info %>% 
    left_join.(janitor::clean_names(main_split_df), 
               by = c("permalink", "date")) %>% 
    filter.(!is.na(main_text)) %>% 
    mutate.(len = as.double(len))
  
  stopifnot(length(unique(merged_df$parl_no)) == 1)
  
  
  pollies_df <- all_pollies_df %>% 
    filter(start_date <= date_to & end_date >= date_from)
    
  
  pollies_parties <- auspol_party %>% 
    filter(case_when( 
      is.na(partyFrom) & is.na(partyTo) ~ TRUE,
      is.na(partyFrom) & partyTo >= start_date & partyTo <= end_date ~ TRUE,
      is.na(partyTo) & partyFrom >= start_date & partyFrom <= end_date ~ TRUE,
      partyFrom >= start_date & partyTo <= end_date ~ TRUE,
      TRUE ~ FALSE))
  
  
  
  
  
  # Prep filtering -----
  

  tic("* First cleaning")
  
  # Enables checking for correspondences further below
  first_clean_df <- merged_df %>% 
    mutate.(speaker_surname = str_extract(speakers_split, "[-A-Za-z']+$"),
            speaker_first_name = 
              str_extract(str_remove_all(speakers_split, TITLE_SP), 
                          "^[A-Za-z']+"),
            across.(c(main_text, speaker, responder, questioner, interjector, 
                      speaker_surname, speaker_first_name), 
                    tolower),
            recon_speaker = case_when.(
              is.na(speaker_surname) ~  NA_character_,
              speaker_surname == speaker_first_name ~ speaker_surname,
              TRUE ~ paste0(speaker_surname, ", ", speaker_first_name)),
            across.(c(speaker, responder, questioner, recon_speaker),
                    clean_orator))
            
  toc()
  
  tic("* Prep filtering")
    
  prep_filtering <-  first_clean_df %>% 
    mutate.(across.(c(questioner, responder, speaker), 
            ~det(.x, speaker_surname),
            .names = "{.col}_match"),
  across.(c(speaker, responder, questioner, interjector), 
          ~det(.x, "^$"), 
          .names = "{.col}_empty"),
  across.(c(questioner, responder, speaker), 
          ~!det(.x, "^$"),
          .names = "{.col}_main_orator")) %>% 
      mutate.(empty_orator = rowSums(select.(., contains("_empty"))) == 4,
              one_main_orator = rowSums(select.(., contains("_main_orator"))) == 1,
              row_matches_orator = rowSums(select(., contains("_match"))) != 0) %>% 
      select.(-contains("_empty$|_match$_main_orator$")) %>% 
      mutate.(row_matches_interjector = det(interjector, speaker_surname),
             text_is_boilerplate = str_detect(main_text, boilerplate),
             procedural = grepl(procedure_rx, main_text),
             chair_in_metadata = det_chair(speaker),
             chair_detected = det_chair(speaker_surname),
             len = as.numeric(len)) %>% 
      filter.(!is.na(permalink))
    
  
  useless_entries <- quos((len < 100 | # formulaic text, interruptions, and other fillers
          (len < 200 & text_is_boilerplate) | 
          (len < 300 & row_matches_interjector) | # short meaningless heckles
          (len < 300 & chair_detected) | 
          (len < 1000 & chair_in_metadata & chair_detected) |
            empty_orator | # when not a formulaic text as above: list of legislation to be put in motion
          (procedural & (chair_in_metadata | chair_detected))
  ))
  
  toc()
  
  
  

  perc_wrong <- nrow(test_comprehensiveness(prep_filtering))/nrow(prep_filtering)
  
  if(!file.exists(per_wrong_stats_fp)) file.create(per_wrong_stats_fp)
  
  perc_wrong_df <- tibble(parliament = parl_no, wrong = perc_wrong)
  
    
  write_delim(perc_wrong_df, per_wrong_stats_fp, append = TRUE)
  
  
  # stopifnot(perc_wrong < 0.002)
  
  
  # choosing orator -----
  

  
  tic("* Filtering and choosing orators in parallel")
  chosen_df <- prep_filtering %>% 
    filter(!(!!!useless_entries)) %>% 
    split(sample(rep(1:8, length.out = nrow(.)))) %>%
    pbmclapply(orator_basic_cases, mc.cores = 8) %>%
    bind_rows.()
  toc()
  
  
  cat("\nDistribution of problematic orator assignments:\n")
  chosen_df %>% 
    mutate(trouble = is.na(orator) | det(orator, "__")) %>% 
    filter(trouble) %>% 
    count(orator) %>% 
    janitor::adorn_totals() %>% 
    print()
  
  cat("\n\n")
  
  
  
  # Fix missing orators -----
  
  
  basic_vars <- c("permalink", "main_text", "speaker", "questioner",
                   "responder", "interjector", "electorate", "parl_no",
                   "recon_speaker", "section_id", "len", "orator","date", "database")
  
  tic("* Separate missing orators")
  missing_orators <- chosen_df %>% 
    group_by(permalink) %>% 
    filter(any(is.na(orator))) %>%
    ungroup() %>% 
    nest.(data = -permalink) %>% 
    mutate.(temp_id = row_number()) %>% 
    unnest.(data) %>% 
    select(temp_id, all_of(basic_vars)) 
    
    
  fixed_missing_orators <- missing_orators %>% 
    group_by(temp_id) %>% 
    mutate(orator = fix_missing_orators(temp_id, main_text, speaker, 
                                        questioner, responder, interjector,
                                        recon_speaker, len, orator)) %>% 
    ungroup() %>% 
    select(-temp_id) %>% 
    filter.(!is.na(orator), !str_detect(orator, fixed("__")), !det_chair(orator))
    
  toc()
  
  
  
  tic("* Format orators and identification fields")
  
  formatted_df <- chosen_df %>% 
    select.(all_of(basic_vars)) %>% 
    filter.(!is.na(orator), !str_detect(orator, fixed("__")), !det_chair(orator)) %>% 
    bind_rows.(fixed_missing_orators) %>% 
    mutate.(electorate_in_text = unify_electorates(electorate),
            orator = format_name(clean_orator(orator)),
            orator_surname = str_extract(orator, "^[a-z]+"),
            orator_surname = ifelse.(str_detect(orator, fixed(", ")), 
                                     orator_surname,
                                     orator)) %>% 
    select.(-electorate)
    

  toc()
  
  
  
  stopifnot(sum(str_detect(formatted_df$orator, ",", negate = T) & str_detect(formatted_df$orator, " ")) == 0)
  
  
  tic("* Assign pollies")
  
  # NB: sometimes Senators and MPs are recorded as speaking in the chamber, so that
  # distinction is not useful

  assigned <- assign_pollies(formatted_df, pollies_df)
  
  
  toc()

  
  
  # Fix duplicates -------
  
  
  tic("* Fix duplicates")
  dupes <- assigned %>%
    distinct.() %>% 
    janitor::get_dupes(permalink, section_id)
    
  sum(dupes$electorate == "")
  
  fixed_dupes <- dupes %>% 
    left_join(select(pollies_df, uniqueID, electorate), "uniqueID") %>% 
    filter(electorate == electorate_in_text) %>% 
    distinct.()
  
  
  unfixed_dupes <- dupes %>% 
    anti_join(fixed_dupes, by = "orator") %>% 
    pull(orator) %>% 
    unique()
  
  
  # stopifnot(nrow(unfixed_dupes) < 50)
  
  toc()
  
  unduped_df <- assigned %>% 
    anti_join(dupes, by = "permalink") %>% 
    bind_rows(fixed_dupes)
  
  
  missing_orator <- unduped_df %>% 
    filter(is.na(uniqueID)) 
    
  # missing_orator_repeated <- missing_orator %>% 
    # count(orator, sort = T) %>% 
  #   filter(n > 20) %>% 
  #   pull(orator)
  

  # 
  # unduped_df %>% 
  #   group_by(permalink) %>% 
  #   filter(any(is.na(uniqueID))) %>% 
  #   ungroup() %>% 
  #   nest(data = -permalink) %>% 
  #   slice_sample(n = 10) %>% 
  #   unnest(data) %>% 
  #   arrange(permalink, section_id) %>% 
  #   View()
  
  


  
  if(!file.exists(problematic_orator_fp)) file.create(problematic_orator_fp)
  
  problematic_orator <- c(unique(pull(missing_orator, orator)), unfixed_dupes)
    
  write_lines(problematic_orator, problematic_orator_fp, append = TRUE)
  
# 
#   missing_orator_counts %>% 
#     mutate(orator = if_else(n == 1, "one_off", orator)) %>% 
#     group_by(orator) %>% 
#     summarize(n = sum(n)) %>% 
#     arrange(desc(n)) %>% 
#     janitor::adorn_totals("row") %>% 
#     print()
#   
  
  
  filled_df <- unduped_df %>% 
    arrange(permalink, section_id) %>% 
    fill.(uniqueID, .direction = "down")

  
  # Add parties -----
  
  tic("* Add parties")
  partied_df <- filled_df %>% 
    left_join(pollies_parties, by = "uniqueID")
  
  
  partied_df %>% 
    distinct.(uniqueID, partyName) %>% 
    janitor::get_dupes(uniqueID) %>% 
    write_csv(duped_parties_fp, append = TRUE)
  
  toc()
  # Save -----
  
  get_perma_id <- function(s) {
    
    str_extract(s, "%[A-Z0-9]+%[0-9]+$") %>% 
      str_remove(fixed("%"))
    
    
  }
  
  tic("* Save")
  
  final_df <- partied_df %>% 
    select(orator_id = uniqueID, date, permalink, section_id, main_text, 
           parl_no, database, electorate_in_text, electorate,
           partyAbbrev, partyName, partySimplifiedName) %>% 
    janitor::clean_names() %>% 
    distinct.(orator_id, permalink, section_id, .keep_all = T) %>% 
    mutate(analysis_id = paste(orator_id, str_remove(database, " Hansard"), 
                               date, get_perma_id(permalink), 
                               section_id, sep = "_"))
    
  
  fwrite.(final_df, paste0(out_path, "parl_", parl_no, ".csv"))
  
  if(test) stop(paste0(parl_no, " testing done"), call. = FALSE) else message(parl_no, " DONE")
  
  
  
}




