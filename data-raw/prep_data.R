library(tidyverse)
library(cvpiaFlow)
library(cvpiaHabitat)
library(lubridate)
library(grandTab)

# prep grandtab data------
grandTab::grandtab %>% 
  mutate(watershed = ifelse(watershed == 'Upper-mid Sacramento River', 'Upper Sacramento River', watershed)) %>% 
  group_by(year, watershed, run, type) %>% 
  summarise(count = sum(count, na.rm = TRUE)) %>% 
  write_rds('data/grandtab.rds')

# get median flows for spawning and rearing---------
square_meters_to_acres <- function(sq_mt) {return(sq_mt * 0.000247105)}

spawning_locations <- cvpiaHabitat::watershed_lengths %>% 
  filter(lifestage == 'spawning') %>%
  pull(watershed) %>% unique()

median_flows <- cvpiaFlow::flows_cfs %>% 
  gather(watershed, flow, -date) %>% 
  filter(year(date) >= 1975) %>% 
  mutate(spawn_period = month(date) %in% 9:12) %>% 
  group_by(watershed, spawn_period) %>% 
  summarise(median = median(flow, na.rm = TRUE)) %>% 
  spread(spawn_period, median) %>% 
  rename(rearing_flow = `FALSE`, spawning_flow = `TRUE`) %>% 
  filter(watershed %in% spawning_locations)

median_fry_hab <- pmap_dbl(list(watershed = median_flows$watershed, species = 'fr', life_stage = 'fry', flow = median_flows$rearing_flow, month = 1), 
                           cvpiaHabitat::set_instream_habitat) %>% square_meters_to_acres

median_spawn_hab <- pmap_dbl(list(watershed = median_flows$watershed, species = 'fr', flow = median_flows$spawning_flow, month = 1), 
                             cvpiaHabitat::set_spawning_habitat) %>% square_meters_to_acres

median_flows$fry_hab <- median_fry_hab
median_flows$spawn_hab <- median_spawn_hab

write_rds(median_flows, 'data/median_flow_hab.rds')