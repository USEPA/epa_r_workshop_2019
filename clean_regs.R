library(tidyverse)
library(stringr)
regs <- read_csv("2017-09-06_registrants.csv", na = c("", "None")) %>%
  select(name = Name, intro = `Introduction to R`, 
         spatial = `Spatial Analysis/GIS with R`, 
         viz = `Data Visualization with ggplot2`,
         email = Email) %>%
  mutate(name = str_trim(name)) %>%
  mutate(first_name = str_extract(name, "[[:alpha:]]+"),
         last_name = str_extract(name, "[[:alpha:]]+$")) %>%
  select(first_name, last_name, intro, spatial, viz, email) %>%
  arrange(last_name)

# Changes
regs <- regs %>% 
  mutate(intro = ifelse(first_name == "Amr", NA, intro),
         viz = ifelse(first_name == "Amr", "PM", viz),
         intro = ifelse(last_name == "Pluntke", "All Day Session", intro),
         viz = ifelse(last_name == "Pluntke", NA, viz),
         spatial = ifelse(last_name == "Pluntke", NA, spatial))

write_csv(regs, "2017-09-06_registrants_clean.csv")

regs %>%
  filter(!is.na(intro)) %>%
  select(first_name, last_name, intro, email) %>%
  write_csv("2017-09-06_intro_registrant.csv")

regs %>%
  filter(!is.na(spatial)) %>%
  select(first_name, last_name, spatial, email) %>%
  write_csv("2017-09-06_spatial_registrant.csv")

regs %>%
  filter(!is.na(viz)) %>%
  select(first_name, last_name, viz, email) %>%
  write_csv("2017-09-06_viz_registrant.csv")


