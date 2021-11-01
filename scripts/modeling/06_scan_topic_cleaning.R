#! /usr/bin/Rscript
# SCAN Topic cleaning
# Parses the output.dat files created by SCAN

suppressPackageStartupMessages({
library(tidyverse)
})

cargs <- commandArgs(trailingOnly = T)

main_topic <- cargs[1]

if(is.na(main_topic)) {
  
  main_topic <- Sys.getenv("HANSARD_MAIN")
  
  if(main_topic == "") main_topic <- "coal"

  timestamp <- Sys.getenv("TIMESTAMP")

  if(timestamp == "") timestamp <- cptools::date_str()
  
}

message("Processing: ", main_topic)

input_folder <- paste0(main_topic, "_output/scan_", timestamp, "/")

output_folder <- paste0(main_topic, "_output/cleaned/scan_", timestamp, "/")

unlink(output_folder, recursive = T)

dir.create(output_folder)

message("Cleaned output folder: ", output_folder)


fp <- list.files(input_folder, 
                 recursive = T, pattern = "output.dat", full.names = T)

stopifnot(length(fp) > 0)

terms <- str_match(fp, "/([a-z_]+)/output.dat")[,2]


message("for terms: ", paste0(unique(terms), collapse = ", "))

# filepath looks like: scan/p_a_r_a_m_s/target_term/output.dat

names(fp) <- str_match(fp, ".*?scan/(.*?)/(.*?)/output.dat")[,3] %>% 
  str_replace_all(c("^/+" = "", "/" = "_"))


extract_values <- function(file) {
    
  
  raw_output <- read_lines(file, skip_empty_rows = T)
  
  basic_info <- raw_output[1]
  
  sep_type_time_ix <- str_which(raw_output, "per time")
  
  
  heading_length <- 6
  
  per_type <- raw_output[heading_length:sep_type_time_ix]
  
  per_topic <- raw_output[sep_type_time_ix:length(raw_output)]
  
  list(per_type, per_topic)
  
  
  map(list("type" = per_type, "topic" = per_topic), function(section) {
    
    
    suppressMessages(suppressWarnings(
      tibble(
        significand = as.numeric(str_extract(section, "^[0-9.]+")),
        exponent = replace_na(as.numeric(str_match(
          section, "e-([0-9]+)"
        )[, 2]), 1),
        prob = round(significand * 10 ** -exponent, 7),
        time = str_match(section, "T=([0-9]+)")[, 2],
        topic = str_match(section, "K=([0-9]+)")[, 2],
        words = str_match_all(section, "(?<!\\d)[a-z_]{2,}")
      ) %>%
        select(-significand, -exponent) %>%
        mutate(
          words = map(words, str_trim),
          words = map(words, ~ .x[!.x %in% c("sum_t", "ime")])
        ) %>%
        unnest_wider(words)
    ))
    
  })
  
}

# if not enough data the file will be empty, catch the error and return NULL
poss_extract_values <- possibly(extract_values, NULL)

out <- map(fp, poss_extract_values)

clean_out <- compact(out)




# output for topic coherence analysis

if(!dir.exists(output_folder)) dir.create(output_folder, recursive = T)

message("Saving at ", output_folder)

walk2(1:length(clean_out), names(clean_out), 
      ~write_csv(filter(pluck(clean_out, .x, "type"), !is.na(time)),
                 paste0(output_folder, .y, ".csv")
                
                  )

      
      )

message('DONE')
