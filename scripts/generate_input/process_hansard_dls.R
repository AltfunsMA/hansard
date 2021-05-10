# Processing downloads from the hansard
# Alfonso Martínez Arranz
# R 3.6.1

library(tidyverse)

main_path <- "/data/hansard/"

main_folder <- 'civility_data'

fp <- list.files(paste0(main_path, main_folder, '/yearly_dfs/'), pattern = "\\.csv", full.names = T)

# Appending of rows to files in various batches created mismatches in the order of columns
# _within_ each file. This function:
# 1. reads as many rows without misspecification errors from the top file
# 2. selects Permalink (an URL that acts as ID column. The original records (apart from the text,
#   the remaining data is the same)) 
# 3. selects "main text" column 
# 4. searches for each of those columns individually in the bottom part of the file

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
  
  # the string "Content Window" appears in pretty much all scrapped text
  text_ix <- map_lgl(df_bottom, ~any(str_detect(.x, "Content Window"), na.rm = T))
  
  df_url <- df_bottom[, url_ix] %>% set_names("Permalink")
  
  df_text <- df_bottom[, text_ix] %>% set_names("main_text")

  out <- bind_cols(df_url, df_text) %>% 
    bind_rows(df_top)
  
  if(nrow(out) == nrow(df)) { list("success" = out)
    } else (list("failure" = out))
  
  
}

main_df <- map(fp, read_clean)

out_error <- main_df %>% 
  map('failure') %>% 
  compact()

out_success <- main_df %>% 
  map("success") %>% 
  compact() %>% 
  bind_rows()




fp_records <- list.files(paste0(main_path, main_folder, "/records"), 
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
  mutate(main_text = rm_multiple(main_text),
         # A couple of items under "Reference" were misencoded as UTF-8 but aren't
         # checked with calls below
        across(where(is.character), iconv, "UTF-8", "UTF-8", sub = "")) %>% 
  janitor::clean_names()

# UTF8 returns a boolean per item in vector
encoding_is_utf8 <- map(final_df[, map_lgl(final_df, is.character)], utf8::utf8_valid)

# Checking at vector level first; it was solved so no need to dig deeper
map_lgl(encoding_is_utf8, ~any(!.x, na.rm = T))


out_filename <- paste0(main_path, main_folder, '/',
                       str_remove(main_folder, "_data"), '_full_downloaded.csv')


write_csv(final_df, out_filename)




