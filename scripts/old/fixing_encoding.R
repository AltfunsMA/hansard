

library(tidyverse)


initial <- read_csv("data/speeches_20yrs_unfiltered.csv")
  
fixed <- initial %>% 
  mutate(Txt = str_replace_all(Txt, "â€”", "--"))


write_csv(fixed, "data/speeches_20yrs_unfiltered_fixed.csv")

fixed %>% 
  filter(str_detect(Txt, regex("in fact if the ethanol plant were to go", 
                               ignore_case = TRUE))) %>% 
  pull(Txt)

first_problem <- initial %>%   
  filter(str_detect(Txt, regex("in fact if the ethanol plant were to go", 
                               ignore_case = TRUE)))

speeches <- read_csv("data/speeches_filtered_13_4_2020.csv")


problem <- speeches %>% 
  filter(str_detect(Txt, regex("in fact if the ethanol plant were to go", 
                               ignore_case = TRUE)))



problem %>% select(Date, Bill, Type)

print(first_problem$Txt)

