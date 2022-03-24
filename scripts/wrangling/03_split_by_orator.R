#! /usr/bin/Rscript

## Inspired by https://github.com/RohanAlexander/hansard
## See: /data/rohan_hansard/scripts/run_2_parse_pdfs/run_3_split_out_speakers.R


#### Set-up -----

pacman::p_load(lubridate, tidyverse, tidytable, tictoc, pbmcapply, parallel)


fix_wrong_names <-
  read_csv2("/data/hansard/terms/rohan_name_corrections_fixed.csv",
            show_col_types = F) %>%
  mutate(numberOfCharacters = nchar(original)) %>%
  arrange(desc(numberOfCharacters)) %>%
  select(-numberOfCharacters)

full_text_path  <- "general_data/02_full_text/"
split_text_path <- "general_data/03_split/"


file_names <-
  list.files(
    path = full_text_path,
    pattern = "*.csv",
    recursive = FALSE,
    full.names = TRUE
  )

test <- FALSE


already_done <- list.files(split_text_path)


left_to_do <- setdiff(basename(file_names), already_done)


if(test) {
  
  left_to_do <- basename(file_names[grepl("2009_Aug.csv", file_names)])
  
  cat("Doing: ", left_to_do, sep = "\n")
  
}




# Regexes---- 

# START <- "(^|\\s|-|\\.)"

TITLE <- c('Mr', 'Sir', 'Senator', 'Mrs', 'Ms', 'Dr', 
           'Dame', 'Madam', "Colonel", "Major") # General and others are far less common and more confusing

# TITLE <- paste0(START, TITLE)


NAME <- "(([A-Z]([^\\s]+)? ?){1,3})" # WHITLAM, Gough Whitlam, E G Whitlam

ENDINGS <- c(' -', '\\(', "\n", '\n\n', "\n—", ":")

ALL_CASES <- paste0(TITLE, " ", NAME, " ?")

RX_COMBOS <- c(map_chr(cross2(c(ALL_CASES), 
                               ENDINGS), paste, collapse = ""))



main_rx <- paste(RX_COMBOS,  collapse = "|")



#### Run -----

sep_speaker_string <- "%_#_%"


process_speakers <- function(group_id, str_vec, n) {
  
  if(is.na(unique(str_vec))) return(str_vec)
  

  # speakers are repeated for all rows in group so first one will do  
  s <- str_vec[1] %>% 
    str_split(sep_speaker_string) %>% 
    unlist() %>% 
    # everything except letters and hyphens unless the hyphens are at the end
    str_remove_all("[^[:alpha:]-\\s]|-(\\s+)?$") %>% 
    str_squish()
  
  
  # if(unique(group_id) %in% c("%3A%22chamber%2Fhansardr%2F2009-08-12%2F0091%22")) {
  # 
  # 
  #   browser()
  # 
  # }

  
  # The first chunk generally doesn't have a speaker
  if(length(s) != 1 && length(s) < n) {
    
     c(NA, s)
    
  } else if (length(s) > n) {
    
 paste0(s[1], "## mismatch ##")
    
  } else {
    
    s
  }
  
  
}


split_by_speaker <- function(path) {
    
  # Some of the permalinks were downloaded twice or more; `distinct` alone does
  # not work because the text has slight differences in
  # a handful of cases. By the looks of it, HTML parsing.
  # When duplicated permalinks are passed through process_speaker, this throws an error
    
    input_file <-
      read_csv(path,
               trim_ws = FALSE,
               col_types = "ccc") %>% 
      distinct.(Permalink, .keep_all = T)
  

    # Retaining \n for use below
    input_file$main_text <-
      str_replace_all(input_file$main_textain_text, "\\h+", " ")

    # Ad-hoc fixes retained from Rohan Alexander's
    input_file$main_text <- str_replace_all(input_file$main_text,
                                            c("M[[:space:]]?r[[:punct:]]+[[:space:]]" = "Mr ",
                                              "M[[:space:]]?r[[:space:]]?s[[:punct:]]?+[[:space:]]" = "Mrs",
                                              "(?<=\\S)Sir" = " Sir",
                                              "D[[:space:]]?r[[:space:]]*[[:punct:]]" = "Dr ",
                                              "Mr SYDNEY; SMITH" = "Mr SYDNEY SMITH",
                                              "S i r" = "Sir",
                                              " mc " = " me ",
                                              "Mi-\\."= "Mr"))
    


    input_file$main_text <-
      stringi::stri_replace_all_regex(
        input_file$main_text,
        fix_wrong_names$original,
        fix_wrong_names$corrected,
        vectorize_all = FALSE
      )
    

     separated <-  input_file %>%
       # Need to save the speakers first because separate_rows removes them
       mutate(speakers = map_chr(stringi::stri_extract_all_regex(main_text, main_rx),
                                  ~paste(.x[!is.na(.x)], collapse = sep_speaker_string))) %>%
       separate_rows(main_text, sep = main_rx)

     # browser()

     out_file <- separated %>%
       group_by(Permalink) %>%
       mutate(n = n(),
              section_id = row_number(),
              speakers_split = process_speakers(Permalink, speakers[1], n[1])) %>%
       ungroup() %>%
       select(-n) %>%
       mutate(len = nchar(main_text))


    outpath <- str_replace(path, full_text_path, split_text_path)

    write_csv(out_file, outpath)
    
    'DONE'
    
}

if(test) {
  
res <- lapply(paste0(full_text_path, left_to_do), split_by_speaker)


} else {

res <- pbmclapply(paste0(full_text_path, left_to_do), split_by_speaker, mc.cores = 11,
                  ignore.interactive = T)

# write_rds(res, 
#           paste0("general_data/03_split/performance/split_run_res_", 
#                  cptools::date_str()), ".rds")
# 
# 
# successes <- map_lgl(res, ~class(.x) == "character")
# 
# success_percent <- scales::percent(sum(successes)/length(res))


cat("Success:", success_percent , '\n')


if(success_percent != "100%") {
  
  
  cat("Sample error:")
  
  print(sample(res[!successes], 1))
  
}

}




