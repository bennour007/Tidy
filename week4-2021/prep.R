library(tidyverse)
library(janitor)
library(cowplot)
library(stringdist)
library(sf)
library(rgeos)


# Importing and cleaning names 
gender <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv') %>% 
  clean_names() %>%
  mutate(county = as_factor(county))
crops <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv') %>% 
  rename(county = "SubCounty") %>%
  clean_names() %>%
  mutate(county = as_factor(county))
households <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv') %>% 
  clean_names() %>%
  mutate(county = as_factor(county))

###############################################################################
# unify the county names 
# gender counties as references
county_levels <- gender$county %>% levels

levels(crops$county) <- county_levels
levels(households$county) <- county_levels

################################################################################
# create one unique dataset 

full <- inner_join(gender, crops, by = "county") %>%
  inner_join(., households, by = "county")

################################################################################
# let the fun begin, Not sure what to start visualizing here!

# let's tidy the data first 

almost_tidy <- full %>%
  pivot_longer(cols = male:intersex, 
               names_to = "gender",
               values_to = "gender_population") %>%
  pivot_longer(cols = tea:khat_miraa,
               names_to = "crop",
               values_to = "crop_farmers") %>%
  mutate(farming_ratio = farming/population)

# What about the population in each county
almost_tidy %>%
  filter(county != "Total") %>%
  mutate(county = fct_reorder(county, crop_farmers)) %>%
  ggplot() +
  geom_point(aes(x = crop_farmers,
                 y = county,
                 color = farming_ratio), 
             alpha = 0.3) +
  scale_color_viridis_c() +
  facet_wrap(~ crop)

# What are areas with the most farming ratio

full %>%
  mutate(farming_ratio = farming/population) %>%
  mutate(county = fct_reorder(county,farming_ratio)) %>%
  ggplot() +
  geom_col(aes(x = farming_ratio, 
                 y = county, 
               fill = )) +
  scale_x_continuous(position = "top", labels = scales::percent)
