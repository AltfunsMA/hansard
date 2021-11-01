pacman::p_load(tidytable)


source_path <- "coal_data/04_model_inputs/preprocessed_datasets/"

add_alliance_col <- function(id_path, party_path) {

  # Tom added the ID to the DF without parties, so it has to be added here.
  
id_df <- fread.(paste0(source_path, id_path))


party_df <- fread.(paste0(source_path, party_path))


out_df <- id_df %>% 
  left_join.(select.(party_df, all_of(c("final_party", 
                                                   "final_alliance",
                                                   'permalink', 'main_text'))),
             by = c('permalink', 'main_text'))


out_df %>% 
  filter.(!is.na(final_alliance))



}


all <- add_alliance_col("all_2a_id.csv", "all_2a_w_parties.csv")

last20 <- add_alliance_col("all_2a_last_20_years_id.csv", "all_2a_last_20_years_w_parties.csv")


fwrite.(all, paste0(source_path, "id_alliance_all_2a.csv"))
fwrite.(last20, paste0(source_path, "id_alliance_last20_2a.csv"))

