# Processing downloads from the hansard
# Alfonso Martínez Arranz
# R 3.6.1


main_folder <- 'coal_data'

fp <- list.files("coal_data/yearly_dfs/", full.names = T)

# Adding of data in various batches created mismatches in the order of columns for each file.
# This function reads as many rows as are without problems then finds:
# 1. the main text column
# 2. another column that will link to the original records (apart from the text, the remaining data is the same)

read_clean <- function(filepath) {
  
  df <- read_csv(filepath, col_types = cols(.default = "c"))
  
  probs <- problems(df)
  
  first_probrow <- probs %>% slice(1) %>% pull(row)
  
  if(length(first_probrow) < 1) {
    
    return(list("success" = select(df, Permalink, main_text)))
         
  } 
  
  df_top <- read_csv(filepath, n_max = first_probrow-1) %>% 
    select(Permalink, main_text)
  
  df_bottom <- read_csv(filepath, skip = first_probrow,
                        col_names = FALSE) %>% 
    select_if(is.character)
  

  url_ix <- map_lgl(df_bottom, ~all(str_detect(.x, "https://parlinfo.aph")), na.rm = T)
  
  text_ix <- map_lgl(df_bottom, ~any(str_detect(.x, "Content Window"), na.rm = T))
  
  df_url <- df_bottom[, url_ix] %>% set_names("Permalink")
  
  df_text <- df_bottom[, text_ix] %>% set_names("main_text")

  out <- bind_cols(df_url, df_text) %>% 
    bind_rows(df_top)
  
  if(nrow(out) == nrow(df)) { list("success" = out)
    } else (list("failure" = out))
  
  
}


main_df <- map(fp, read_clean)


out_success <- main_df %>% 
  map("success") %>% 
  compact() %>% 
  bind_rows()


fp_records <- list.files("/data/hansard/coal_data/records", 
                         pattern = "all_Hansard", full.names = T)

df_records <- map_dfr(fp_records, read_csv)

rm_multiple <- function(str) {
  
  str %>% 
  str_remove("^Content Window") %>% 
    str_remove("^\n\n") %>% 
    str_remove("Download Fragment") %>% 
    str_remove("^\\s") %>% 
    str_remove("  Watch ParlView Video")
  
}


final_df <- out_success %>% 
  filter(!is.na(main_text)) %>% 
  right_join(df_records, by = "Permalink") %>% 
  distinct() %>% 
  mutate(main_text = rm_multiple(main_text)) %>% 
  janitor::clean_names()


write_csv(final_df, paste0(main_folder, '/processed/coal_final_df.csv.gz'))
