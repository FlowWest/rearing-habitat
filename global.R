library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(plotly)
library(tidyverse)
library(cvpiaHabitat)
library(cvpiaFlow)

source('modules/rearing.R')

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

