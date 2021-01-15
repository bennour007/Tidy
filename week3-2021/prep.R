# Importing libraries and downloading data

################################################################################
library(tidyverse)
library(gganimate)
library(ggridges)
library(magick)
library(cowplot)
library(patchwork)
################################################################################
artwork <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

