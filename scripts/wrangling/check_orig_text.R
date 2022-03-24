

orig_fp <- list.files("general_data/02_full_text/", pattern = "csv$", full.names = T)

some_orig_fp <- orig_fp[grepl("1978|1990|2003", orig_fp)]

orig_text <- map_dfr.(some_orig_fp, fread.)


check_id <- function(id) {
  
  orig_text %>% 
    filter(Permalink == id) %>% 
    pull(main_text) %>% 
    str_replace_all("\\h+", " ") %>% 
    cat()

assigned %>%
  filter(permalink == id) %>%
  View("assigned")


# unfilt %>%
#   filter(Permalink == id) %>%
#   View("unfilt")

}





id <- "%3A%22hansard80%2Fhansardr80%2F1978-03-09%2F0052%22"

check_text <- check_id(id)

check_text


  # Check how separation works
orig_text %>% 
  filter(Permalink == id) %>% 
  mutate(main_text = str_replace_all(main_text, "\\h+", " ")) %>% 
  mutate(speakers = map_chr(stringi::stri_extract_all_regex(main_text, main_rx), 
                          ~paste(.x[!is.na(.x)], collapse = sep_speaker_string))) %>% 
  separate_rows(main_text, sep = main_rx) 




split_by_speaker("general_data/02_full_text/1990_Nov.csv")



str_split(check_text, main_rx)


pre_treatment <- function(text) {
  
  str_replace_all(text, "\\h+", " ") %>% 
  str_replace_all(c("M[[:space:]]?r[[:punct:]]+[[:space:]]" = "Mr ",
                    "M[[:space:]]?r[[:space:]]?s[[:punct:]]?+[[:space:]]" = "Mrs",
                    "(?<=\\S)Sir" = " Sir",
                    "D[[:space:]]?r[[:space:]]*[[:punct:]]" = "Dr ",
                    "Mr SYDNEY; SMITH" = "Mr SYDNEY SMITH",
                    "S i r" = "Sir",
                    " mc " = " me ",
                    "Mi-\\."= "Mr")) %>% 
  stringi::stri_replace_all_regex(
    fix_wrong_names$original,
    fix_wrong_names$corrected,
    vectorize_all = FALSE
  )
  
}



orig_text %>% 
  filter(Permalink == id) %>% 
  mutate(main_text = pre_treatment(main_text)) %>% 
  separate_rows(main_text, sep = main_rx)


testname <- function(s) polsurnames[grepl(s, polsurnames, ignore.case = T)]

testname("PAGE")


