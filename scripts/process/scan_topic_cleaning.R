# SCAN Topic cleaning

library(tidyverse)

setwd("/data/hansard")

main_topic <- Sys.getenv("HANSARD_TOPIC_TO_PROCESS", "coal")

message("Processing: ", main_topic)

fp <- list.files(paste0(main_topic, "_output/scan/"), 
                 recursive = T, pattern = "output", full.names = T)

terms <- str_match(fp, "([a-z_]+)/output.dat")[,2]

message("with terms: ", paste0(unique(terms), collapse = ", "))

start_years <- str_match(fp, "([0-9]{4})(-|/)")[,2]

k = str_extract(fp, "k\\d+")

names(fp) <- paste0(terms, "_", start_years, "_", k)


extract_values <- function(file) {
  
  
  raw_output <- read_lines(file, skip_empty_rows = T)
  
  basic_info <- raw_output[1]
  
  cat(basic_info, "\n")
  
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

# if not enough data the file will be empty
poss_extract_values <- possibly(extract_values, NULL)


out <- map(fp, poss_extract_values)


# output for topic coherence analysis

walk2(1:length(out), names(out), 
      ~write_csv(filter(pluck(out, .x, "type"), !is.na(time)),
                 paste0("analyse_output/scan/", .y, ".csv")
                 )
)


