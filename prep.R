library(tidyverse)
library(maps)
library(sf)
library(countrycode)
library(ggspatial)
library(patchwork)

################################################################################
transit_cost_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')
