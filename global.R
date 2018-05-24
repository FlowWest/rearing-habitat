library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(plotly)
library(leaflet)
library(rgdal)
library(tidyverse)
library(lubridate)
library(cvpiaHabitat)
library(cvpiaFlow)
library(readxl)

source('modules/instream.R')

# functions -------------
pretty_num <- function(num, places = 0) {
  format(round(num, places), big.mark = ',', trim = FALSE)
}

territory_needs <- function() {
  territory <- function(L) {
    return(10 ^ (2.61 * log(L, base = 10) - 2.83))
  }
  
  territory_size <- rep(0,3)
  territory_size[1] <- territory(mean(c(3.75,4.2))) 
  territory_size[2] <- territory(mean(c(4.2,7.4)))
  territory_size[3] <- territory(mean(c(7.4,11))) 
  territory_size[4] <- 0
  
  return(territory_size)
}

# inputs ----------------
fry_territory <- territory_needs()[1]
  
spawning_locations <- cvpiaHabitat::watershed_lengths %>% 
  filter(lifestage == 'spawning') %>%
  pull(watershed) %>% unique()

grandtab <- read_rds('data/grandtab.rds')
doubling <- read_rds('data/doubling_goal.rds') 

median_flow_hab <- read_rds('data/median_flow_hab.rds')

flow_to_acres <- read_rds('data/flow_to_acres.rds')

flows <- cvpiaFlow::flows_cfs %>% 
  filter(between(date, as.Date('1980-01-01'), as.Date('1999-12-31'))) %>% 
  gather(watershed, flow_cfs, -date) 

# fp
source('modules/fp.R', local = TRUE)
source('R/print_model_details.R', local = TRUE)

watersheds <- read_rds('data/watersheds.rds')

# temp remove bypasses and sacramento
watersheds <- filter(watersheds, !(watershed %in% c('Yolo Bypass', 'Sutter Bypass')))

salmonid_extents <- readOGR("data/salmonid_habitat_extents/salmonid_habitat_extents.shp", 
                            stringsAsFactors = FALSE) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

sps <- subset(salmonid_extents, Species == 'Spring Run Chinook' & Habitat == 'spawning')
spr <- subset(salmonid_extents, Species == 'Spring Run Chinook' & Habitat == 'rearing')
fs <- subset(salmonid_extents, Species == 'Fall Run Chinook' & Habitat == 'spawning')
fr <- subset(salmonid_extents, Species == 'Fall Run Chinook' & Habitat == 'rearing')
sts <- subset(salmonid_extents, Species == 'Steelhead' & Habitat == 'spawning')
str <- subset(salmonid_extents, Species == 'Steelhead' & Habitat == 'rearing')