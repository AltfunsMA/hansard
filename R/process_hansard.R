# Processing downloads from the hansard
# Alfonso Martínez Arranz
# R 3.6.1


main_folder <- 'coal_data'

fp <- list.files("coal_data/yearly_dfs/", full.names = T)


main_df <- map_dfr(fp, read_csv, col_types = cols(`Parl No.` = "i",
  `Question No.` = "i",
  year = "i",
  .default = "c"
)) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(main_text)) %>% 
  distinct()


main_df %>% 
  split(.$year) %>% 
  iwalk(~write_csv( .x, paste0(main_folder, "/processed/", .y, ".csv")))

write_lines(unique(main_df$system_id), paste0(main_folder, "/downloaded_ids.log"))



rec_df %>% 
  # anti_join(main_df, by = "Permalink") %>% 
  pull(year) %>% 
  sort() %>% 
  unique()


main_df %>% 
  filter(!is.na(main_text)) %>% 
  distinct(Permalink, main_text) %>% 
  nrow()
