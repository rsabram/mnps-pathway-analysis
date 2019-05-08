library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(plotly)

mnps_tcap <- readRDS('./data/mnps_tcap.RDS')
compare_info <- readRDS('./data/school_data_compare.RDS')
pathway_scores <- readRDS('./data/cluster_graphs.RDS')

subgroups <- unique(mnps_tcap$Subgroup)
grades <- c('All Grades','3','4','5','6','7','8','9','10','11','12')
subjects <- unique(mnps_tcap$Subject)
schools <- unique(compare_info$`School Name.x`)
clusters <- unique(pathway_scores$cluster)

theme_set(theme_grey(base_size = 18)) 

