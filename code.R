library(tidyverse)
library(arules)
library(arulesViz)
library(lubridate)


unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')



pl_issues <- left_join(unvotes, issues, 'rcid') %>%
  left_join(., roll_calls, 'rcid') %>%
  filter(issue == 'Palestinian conflict')

# okay, we got all the voting data around the palestinian issue>
# My idea is to find out how the UN voted over these issues from 2 perspectives, the US and the Eastern countries.
# In other words, I will have to estimate a distance function for each vote that happened between all the countries to actually 
# understand where each country stands on this issue from an objective pov

pl_issues %>% 
  mutate(date = year(date)) %>% 
  filter(date == 1970) %>% 
  ggplot() +
  geom_point(aes(x = country, y = vote)) +
  coord_flip() 





