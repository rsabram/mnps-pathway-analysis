library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(plotly)

mnps_tcap <- readRDS('./data/mnps_tcap.RDS')

subgroups <- unique(mnps_tcap$Subgroup)
grades <- unique(mnps_tcap$'Grade Level')
subjects <- unique(mnps_tcap$Subject)

theme_set(theme_grey(base_size = 18)) 

