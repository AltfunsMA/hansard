#! /usr/bin/Rscript

# Provide some distinctions between potentially very different types of
# documents stored in the Hansard system



library(tidyverse)
library(tidytable)
library(lubridate)
library(tidytext)



# Tom's parsing that suggested the need to understand this better -----------
# 
# parsed <- read_csv("/data/hansard/data/06_dtm/dtm/parsed_yearly_samples.csv")
# 
# parsed %>% 
#   mutate(length_txt = nchar(parsed_phrase)) %>% 
#   pull(length_txt) %>% 
#   summary()
# 
# full %>% 
#   semi_join(parsed) %>% 
#   View()

cargs <- commandArgs(trailingOnly = T)

main_folder <- cargs[1]

mentions_str <- cargs[2]

if(length(cargs) == 0) {
  
  main_folder <- "repr_rights"
  
  mentions_str <- "abortion|contraceptive"
  
}


main_path <- paste0(main_folder, "_data/04_model_inputs/")
sample_outpath <- paste0(main_folder, "_data/text_samples/")

# Load downloaded dataset and eliminate obviously bad docs -------
floor_thirty <-  function(value) value - value %% 30


full_raw <- data.table::fread(paste0(main_path, main_folder, "_full_downloaded.csv")) %>% 
  mutate.(year = year(ymd(date)),
         char_length = nchar(str_remove(main_text, "\\s{2,}")),
         mentions = str_count(tolower(main_text), mentions_str),
         tridecade = floor_thirty(year),
         type = str_extract(database, c("Hansard|Reports|Committees")),
         across.(c(responder, electorate, speaker, questioner), str_to_title)) %>%
  filter.(!is.na(title), !is.na(main_text), mentions != 0) # Only a handful


# full_raw %>% 
#   filter(char_length < 250) %>% 
#   sample_n(5) %>% 
#   pull(main_text) %>% 
#   cat()
#  
# 
# full_raw %>% 
#   filter(type %in% c("Committees", "Reports") & char_length > 5000 & char_length < 10000) %>% 
#   slice_sample(n = 1) %>% 
#   pull(main_text) %>%
#   cat()

full <- full_raw %>% 
  filter.(case_when.(char_length < 250 ~ FALSE,
    type %in% c("Committees", "Reports") & char_length < 5000 ~ FALSE,
    TRUE ~ TRUE
  ))


fwrite.(full, paste0(main_path, main_folder, "_categorised.csv"))




full %>% 
  group_by(type, year) %>% 
  summarize(chars = sum(char_length)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = chars, colour = type))




# Deal with reports and committees (small percentage) -------

full %>%
  # mutate(before_2000 = ifelse.(year < 2010, "yes", 'no')) %>% 
  group_by(type) %>% 
  summarise(total_docs = n(),
            million_chars = sum(char_length)/10^6,
            char = list(summary(char_length))
            ) %>% 
  unnest_wider(char, names_sep = "_") %>% 
  arrange(desc(million_chars)) %>% 
  mutate(across(where(is.numeric), scales::comma, accuracy = 1)) %>% 
  write_csv(paste0(main_path, "stats_by_doc_type.csv"))




sample_reports <- full %>% 
  filter.(type %in% c("Committees", "Reports")) %>% 
  select.(title, tridecade, date, type, char_length, main_text) %>% 
  group_by(type, tridecade) %>% 
  mutate(quant = list(quantile(char_length))) %>% 
  ungroup() %>% 
  unnest_longer(quant) %>% 
  filter.(quant == char_length)

sample_reports %>% 
  select(type, tridecade, char_length, quant, quant_id)

if(!dir.exists(sample_outpath)) {
  
  dir.create(sample_outpath)
  
  dir.create(paste0(sample_outpath, "/committees_reports/"))
  
}


for (i in 1:nrow(sample_reports)) {
  
  out_df <- sample_reports[i,]
  
  fp <- paste0(sample_outpath, "/committees_reports/", 
         out_df$type, "_", out_df$tridecade, "_",
         out_df$quant_id, ".md")
  
  
  sink(fp)
  
  cat("#", out_df$title, "\n\n")
  cat("###", out_df$date, "\n\n")
  
  cat(str_trunc(out_df$main_text, 10^5, 
                ellipsis = paste0("[... truncated: ", 
                (out_df$char_length - 10^5), " remaining ] \n\n")))
  
  sink()
  
}



# Further categorise Hansard documents ------


cats <- c("question", "adjournment",  "paper", "statement", "bill", "speech", 
          "report", "motion", "matters? of", "documents") %>%
  cptools::bound_rx("", "")

subcats <- c("second reading", "first_reading", "budget", "petition",
             "private member", "notice", "answer", "committee", "estimate",
             "condolences")  %>% 
  cptools::bound_rx("","")

print(paste("cats:", cats))

print(paste("subcats:", subcats))


find_categories <- function(string, patterns) {
  

  str_to_lower(string) %>% 
    str_extract_all(regex(patterns)) %>% 
    map.(~paste(sort(unique(str_remove(.x, "e?s$"))), collapse = " - ")) %>% 
    unlist() %>% 
    str_to_title()
    
  
}


categorised <- full %>% 
  filter(!str_detect(database, c("Reports|Committees"))) %>% 
  distinct.(title, main_text, year, char_length, permalink, database) %>% 
  mutate.(category = find_categories(title, cats),
         subcategory = find_categories(title, subcats),
         category = ifelse.(category == "", subcategory, category),
         category = str_replace(category, "Matters", "Matter"),
         ) %>% 
  select(-main_text)


uncategorised <- categorised %>% 
  filter.(category == "")

catcount <- categorised %>% 
  group_by(category) %>% 
  summarize( N = n(),
         char_dist = list(summary(char_length))) %>%
  unnest_wider(char_dist) %>% 
  arrange.(desc(N)) %>% 
  mutate(across(c(where(is.numeric), -N), scales::comma, accuracy = 1))
  

top_cats <- slice_max.(catcount, N, n = 6)

cat("top categories:", top_cats$category, sep = "\n")


cat("top_cats proportion of total: \n", sum(top_cats$N)/nrow(full))



full_text_with_cats <- full %>% 
  left_join(categorised) %>% 
  mutate(category = str_replace(category, "^$", "[empty]")) %>%
  mutate.(.by = category,
          type_per_cat = n()) %>% 
  group_by(category, tridecade, database) %>% 
  mutate(type_per_subsection = n(),
         quant = list(quantile(char_length, 
                               probs = seq(0, 1, length.out = 6)))) %>% 
  ungroup()


hansard_only_cats <- full_text_with_cats %>% 
  filter(!str_detect(database, c("Reports|Committees"))) %>%
  mutate(category = fct_infreq(fct_lump_n(category, 10)))


hansard_only_cats %>% 
  cptools::rm_list_cols() %>% 
  janitor::remove_empty("cols") %>% 
  select(-page, -question_no, -subcategory, 
         -type_per_cat, -type_per_subsection) %>% 
  data.table::fwrite(paste0(main_path, main_folder, "_hans_only.csv"))


# Plots, text samples  and various statistics on Hansard categories ------

hansard_only_cats %>% 
  group_by(type, year, category) %>% 
  summarise(chars = sum(char_length)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = chars, colour = category)) +
  labs(title = "categories as per title contents in Hansard documents")

  

hansard_only_cats %>% 
  summarize.(.by = category,
             N = n(),
             char_dist = list(summary(char_length))) %>%
  unnest_wider(char_dist) %>% 
  arrange.(desc(N))
  mutate.(across.(c(where(is.numeric), -N), scales::comma, accuracy = 1))




full_text_with_cats %>% 
  select(category, type, mentions, char_length) %>% 
  mutate(density = mentions/(char_length/10000)) %>% 
  group_by(category) %>% 
  summarize(N= n(),
    dens = list(summary(density)),
    cite = list(summary(mentions))) %>% 
  unnest_wider(cite, names_sep = "_") %>% 
  unnest_wider(dens, names_sep = "_") %>% 
  select(-contains("Mean")) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  arrange(desc(N)) %>% 
  write_csv(paste0(sample_outpath, main_folder, "_stats_by_category.csv")) %>% 
  {if(interactive()) View(.) else .}
  
  



category_sample <- full_text_with_cats %>% 
  filter(category %in% top_cats$category & !str_detect(database, c("Reports|Committees"))) %>%
  unnest_longer(quant, values_to = "quant_num", indices_to = "name_quant") %>% 
  filter.(.by = c(category, tridecade, database, name_quant),
          char_length == quant_num) %>% 
  arrange.(tridecade, desc(type_per_cat), quant_num) %>% 
  distinct.(permalink, title, main_text, tridecade, year, database, category, name_quant, char_length,
            type_per_subsection, type_per_cat)




for (categ in unique(category_sample$category)) {

    cat_df <- category_sample %>% 
    filter(category == categ)
  
  sink(paste0(sample_outpath, "categ_", unique(cat_df$type_per_cat),
       "_", categ, ".md"))

  for(dec in unique(category_sample$tridecade)) {
    
        out_df <- cat_df %>% 
          filter(tridecade == dec)
    
    
  cat("\n# ----------- TRIDECADE ", dec, "-------------\n")
    
    for (i in 1:nrow(out_df)) {

      
  cat("<br><br><br> \n <H2> Full title:", str_replace_all(out_df$title[i], "\n", " "), "  </H2> \n",
      "\n <center> <B>|||| ", out_df$database[i], "||||</B> </center>", "\n",
      "\n <center> <B>|||| Present sample document length = ", out_df$char_length[i], "(quantile:", out_df$name_quant[i], ") ||||</B> </center>\n",
      "\n <center> <B>|||| N in its subset = ", out_df$type_per_subsection[i], "||||</B> </center>\n\n")
       
      out_df$main_text[i] %>% 
        str_replace_all(c("\\s{3,}" = "<p> **\\\\s{3,}** ")) %>% 
        str_trunc(2000, ellipsis = paste0("<br> **[... ", scales::comma(nchar(out_df$main_text[i]) - 2000), 
                  " chars REMOVED...]** \n")) %>% 
  
      cat("\n #### <A href= '", out_df$permalink[i], "'>Permalink</A> \n\n")
  cat("\n <center> <B>|| ENTRY END ||</B> </center> \n\n")

    }

  }
  
  sink()

}



