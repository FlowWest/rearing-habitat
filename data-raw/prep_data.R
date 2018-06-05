library(tidyverse)
library(cvpiaFlow)
library(cvpiaHabitat)
library(lubridate)
# library(grandTab)

# prep grandtab data------
# grandTab::grandtab %>% 
#   mutate(watershed = ifelse(watershed == 'Upper-mid Sacramento River', 'Upper Sacramento River', watershed)) %>% 
#   group_by(year, watershed, run, type) %>% 
#   summarise(count = sum(count, na.rm = TRUE)) %>% 
#   write_rds('data/grandtab.rds')

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

# flow to rearing area-------------
cvpiaHabitat::modeling_exist %>% View

watersheds <- cvpiaHabitat::modeling_exist %>% 
  filter(FR_juv) %>% 
  pull(Watershed) %>% .[-1]

ws_is <- paste0(gsub(' |-', '_', tolower(watersheds)), '_instream')
x <- map_df(ws_is, function(w) {
  df <- do.call(`::`, list(pkg = 'cvpiaHabitat', name = w))
  select(df, flow_cfs) %>% 
    summarise(min_flow = min(flow_cfs), max_flow = max(flow_cfs)) %>% 
    mutate(watersed = w)
})
x$watersed <- watersheds
x

# upper mid sac region streams flow 50-900
upmidsac_ws <- cvpiaHabitat::modeling_exist %>% 
  filter(Region == 'Upper-mid Sacramento River', UseRearRegionApprox) %>% 
  pull(Watershed)

upmidsac_hab <- map_df(upmidsac_ws, function(w) {
  flows = seq(50, 900, length.out = 50)
  spawn = square_meters_to_acres(set_spawning_habitat(watershed = w, species = 'fr', flow = flows))
  fry = square_meters_to_acres(set_instream_habitat(watershed = w, species = 'fr', life_stage = 'fry', flow = flows))
  juv = square_meters_to_acres(set_instream_habitat(watershed = w, species = 'fr', life_stage = 'juv', flow = flows))
  data.frame(flow = flows,
             spawn = spawn,
             fry = fry,
             juv = juv,
             watershed = rep(w, 50),
             stringsAsFactors = FALSE)
})

upmidsac_hab %>% 
  gather(type, hab, - flow, - watershed) %>% 
  ggplot(aes(x = flow, y = hab, color = type)) +
  geom_line() +
  facet_wrap(~watershed)

# others
# minus sac and bypasses
x1 <- x[c(-6, -11, -14, -7, -12, -20), ]
ll <- list(min_f = x1$min_flow, max_f = x1$max_flow, watersed = x1$watersed)
dd <- map2(x1$min_flow, x1$max_flow, function(x, y) {seq(x, y, length.out = 50)})

trib_hab <- map2_df(x1$watersed, dd, function(watershed, flow) {
  flows = flow
  spawn = square_meters_to_acres(set_spawning_habitat(watershed = watershed, species = 'fr', flow = flows))
  fry = square_meters_to_acres(set_instream_habitat(watershed = watershed, species = 'fr', life_stage = 'fry', flow = flows))
  juv = square_meters_to_acres(set_instream_habitat(watershed = watershed, species = 'fr', life_stage = 'juv', flow = flows))
  data.frame(flow = flows,
             spawn = spawn,
             fry = fry,
             juv = juv,
             watershed = rep(watershed, 50),
             stringsAsFactors = FALSE)
})

x3 <- trib_hab %>% 
  bind_rows(upmidsac_hab) %>% pull(watershed) %>% unique() 

cvpiaHabitat::modeling_exist$Watershed[!(cvpiaHabitat::modeling_exist$Watershed %in% x3)]

x
flows = seq(50, 5000, length.out = 50)
spawn = square_meters_to_acres(set_spawning_habitat(watershed = 'San Joaquin River', species = 'fr', flow = flows))
fry = square_meters_to_acres(set_instream_habitat(watershed = 'San Joaquin River', species = 'fr', life_stage = 'fry', flow = flows))
juv = square_meters_to_acres(set_instream_habitat(watershed = 'San Joaquin River', species = 'fr', life_stage = 'juv', flow = flows))
san_j <- data.frame(flow = flows,
           spawn = spawn,
           fry = fry,
           juv = juv,
           watershed = rep('San Joaquin River', 50),
           stringsAsFactors = FALSE)

flows = seq(50, 1000, length.out = 50)
spawn = square_meters_to_acres(set_spawning_habitat(watershed = 'Cosumnes River', species = 'fr', flow = flows))
fry = square_meters_to_acres(set_instream_habitat(watershed = 'Cosumnes River', species = 'fr', life_stage = 'fry', flow = flows))
juv = square_meters_to_acres(set_instream_habitat(watershed = 'Cosumnes River', species = 'fr', life_stage = 'juv', flow = flows))
cos_r <- data.frame(flow = flows,
                    spawn = spawn,
                    fry = fry,
                    juv = juv,
                    watershed = rep('Cosumnes River', 50),
                    stringsAsFactors = FALSE)

flows = seq(3250, 31000, length.out = 100)
spawn = square_meters_to_acres(set_spawning_habitat(watershed = 'Upper Sacramento River', species = 'fr', flow = flows, month = 1))
fry = square_meters_to_acres(set_instream_habitat(watershed = 'Upper Sacramento River', species = 'fr', life_stage = 'fry', flow = flows, month = 1))
juv = square_meters_to_acres(set_instream_habitat(watershed = 'Upper Sacramento River', species = 'fr', life_stage = 'juv', flow = flows, month = 1))
up_sac <- data.frame(flow = flows,
                    spawn = spawn,
                    fry = fry,
                    juv = juv,
                    watershed = rep('Upper Sacramento River', 100),
                    stringsAsFactors = FALSE)

trib_hab %>% 
  bind_rows(upmidsac_hab) %>% 
  bind_rows(san_j) %>% 
  bind_rows(cos_r) %>% 
  bind_rows(up_sac) %>% write_rds('data/flow_to_acres.rds')

read_rds('data/flow_to_acres.rds') %>%   
  gather(type, hab, - flow, - watershed) %>% 
  ggplot(aes(x = flow, y = hab, color = type)) +
  geom_line() +
  facet_wrap(~watershed, scales = 'free')
  