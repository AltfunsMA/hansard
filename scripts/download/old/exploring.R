# R. 3.6.2
# Analysing hansard speeches


library(tidyverse)
library(lubridate)

initial <- read_csv("data/speeches_20yrs_unfiltered.csv")

unique(initial$Party)

initial %>% 
  mutate(Party = fct_collapse(Party, 
                              Labour = "ALP", 
                              Liberal = "LP", 
                              Greens = "AG", 
                              Nationals = c("Nats", "NATS", "NatsWA"), 
                              NA = "UNKNOWN",
                              other_level = "Other",
                              )) %>% 
  mutate(Party = fct_explicit_na(Party)) %>% 
  group_by(Party, Month = cut(Date, "month")) %>% 
  summarise(speech_count = n()) %>% 
  ungroup() %>% 
  ggplot() +
    geom_line(aes(x = as_date(Month), y = speech_count, colour = Party))
