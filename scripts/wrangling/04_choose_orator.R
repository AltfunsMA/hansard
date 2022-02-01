## Assign party to each section


pacman::p_load(tidyverse, tictoc, lubridate, tidytable)

source("scripts/wrangling/04_choose_orator_FUNS.R")



# Some helper variables -------------
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
         surname = tolower(str_extract(full_name, "[A-Za-z']+$")),
         first_name = tolower(str_extract(full_name, "^[A-Za-z']+")),
         list_name = paste0(surname, ", ", first_name)) %>% 
  select(list_name, full_name, party) %>% 
  distinct()



pollies_df <- auspol_all %>% 
  mutate(orator = clean_orator(displayName),
         recon_name = clean_orator(paste0(surname, ", ", firstName)),
         orator_surname = clean_orator(surname),
         initials = get_initials(firstName),
         full_initials = get_initials(allOtherNames),
         recon_initials = clean_orator(paste0(surname, ", ", initials)),
         recon_full_initials = clean_orator(paste0(surname, ", ", full_initials))) %>% 
  select(-gender, -matches("wiki|adb|comments|title")) %>% 
  left_join(auspol_electorates, by = "uniqueID") %>% 
  # Avoids exclusion when filtering by term
  mutate(electorate = unify_electorates(electorate),
         date_to = replace_na(date_to, ymd("2022-03-01"))) 





# Load main data ----


# NB: Presenter variable is all NA
identifying_info <- fread.("general_data/01_records/combined_general_records.csv",
                        select= c("Permalink", "Date", "Electorate", "Interjector",
                                  "Parl No.",
                                  "Questioner", "Responder", "Speaker", "Database")) %>% 
  janitor::clean_names()


parl_start_dates <- identifying_info %>% 
  distinct.(date, parl_no) %>% 
  mutate.(date = dmy(date)) %>% 
  slice_min.(.by = parl_no, date) %>% 
  relocate(parl_no, date) %>% 
  deframe()


split_text_path <- "general_data/03_split/"


fp <- list.files(split_text_path, pattern = "csv$", full.names = T)

somefp <- fp[grepl("1978|1990|2003", fp)]


main_split_df <- map_dfr.(somefp, fread.)

merged_df <- identifying_info %>% 
  left_join.(janitor::clean_names(main_split_df), by = c("permalink", "date")) %>% 
  filter.(!is.na(main_text)) %>% 
  mutate(len = as.double(len))


# Prep filtering -----


boilerplate <- "monday|tuesday|wednesday|thursday|friday|prayers|resumed|order"


det <- function(s, p) str_detect(str_remove_all(s, "'"), p)

det_chair <- function(s) str_detect(s, "speaker|president|chairman|clerk")


TITLE <- c('Mr', 'Sir', 'Senator', 'Mrs', 'Ms', 'Dr', 
           'Dame', 'Madam', "Colonel", "Major")

TITLE_SP <- cptools::bound_rx(paste0(TITLE, " "), leftbound = "^")

TITLE_SP

procedure_rx <- "\\bayes\\b|a list of ministers and the offices they hold"

tic("Preparing filtering")

prep_filtering <- merged_df %>% 
  mutate.(speaker_surname = str_extract(speakers_split, "[A-Za-z']+$"),
          speaker_first_name = str_extract(str_remove_all(speakers_split, TITLE_SP), "^[A-Za-z']+"),
          across.(c(main_text, speaker, responder, questioner, interjector, 
                    speaker_surname, speaker_first_name), 
                  tolower),
          recon_speaker = case_when.(
            is.na(speaker_surname) ~  NA_character_,
            speaker_surname == speaker_first_name ~ speaker_surname,
            TRUE ~ paste0(speaker_surname, ", ", speaker_first_name)),
         across.(c(questioner, responder, speaker), 
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


stopifnot(perc_wrong < 0.001)


# choosing orator -----



tic("filtering and choosing orators in parallel")
chosen_df <- prep_filtering %>% 
  filter(!(!!!useless_entries)) %>% 
  split(sample(rep(1:8, length.out = nrow(.)))) %>%
  pbmclapply(orator_basic_cases, mc.cores = 8) %>%
  bind_rows.()
toc()


cat("Distribution of problematic orator assignments:\n")
chosen_df %>% 
  mutate(trouble = is.na(orator) | det(orator, "__")) %>% 
  filter(trouble) %>% 
  count(orator) %>% 
  janitor::adorn_totals()




# Fixing missing orators -----


basic_vars <- c("permalink", "main_text", "speaker", "questioner",
                 "responder", "interjector", "electorate", "parl_no",
                 "recon_speaker", "len", "orator","date", "database")

tic("separate missing orators")
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



tic("format orators and identification fields")

formatted_df <- chosen_df %>% 
  select.(all_of(basic_vars)) %>% 
  filter.(!is.na(orator), !str_detect(orator, fixed("__")), !det_chair(orator)) %>% 
  bind_rows.(fixed_missing_orators) %>% 
  mutate.(.by = permalink, section_id = row_number.()) %>% 
  mutate.(electorate_in_text = unify_electorates(electorate),
    orator = format_name(clean_orator(orator)),
          orator_surname = str_extract(orator, "^[a-z]+"),
         orator_surname = ifelse.(str_detect(orator, fixed(", ")), 
                                  orator_surname,
                                  orator)) %>% 
  select.(-electorate)
toc()


stopifnot(sum(str_detect(formatted_df$orator, ",", negate = T) & str_detect(formatted_df$orator, " ")) == 0)


tic("assign pollies")


senate_df <- formatted_df %>% 
  filter(database == "Senate Hansard") %>% 
  split(.$parl_no) %>% 
  map(~assign_pollies(.x, filter(pollies_df, senator == 1)))

house_df <- formatted_df %>% 
  filter(database == "House Hansard") %>% 
  split(.$parl_no) %>% 
  map( ~assign_pollies(.x, filter(pollies_df, member == 1)))


assigned <- bind_rows.(senate_df, house_df)

toc()

assigned

# Fix duplicates -------

dupes <- assigned %>%
  janitor::get_dupes(permalink, section_id)
  

sum(!is.na(dupes$electorate))

fixed_dupes <- dupes %>% 
  left_join(select(pollies_df, uniqueID, electorate)) %>% 
  filter(electorate == electorate_in_text) %>% 
  distinct.()



final_df <- assigned %>% 
  anti_join(dupes) %>% 
  bind_rows(fixed_dupes)




rmd_permalink <- setdiff(merged_df$permalink, final_df$permalink)


merged_df %>% 
  filter(permalink %in% rmd_permalink, len > 500) %>% 
  View("excluded observations")




