library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(plotly)
library(tidyverse)
library(cvpiaHabitat)
library(cvpiaFlow)

source('modules/rearing.R')

spawning_locations <- cvpiaHabitat::watershed_lengths %>% 
  filter(lifestage == 'spawning') %>%
  pull(watershed) %>% unique()

grandtab <- read_rds('data/grandtab.rds')
median_flow_hab <- read_rds('data/median_flow_hab.rds')


