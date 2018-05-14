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
