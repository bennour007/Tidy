library(tidyverse)
library(janitor)
library(cowplot)
library(rKenyaCensus)
library(sf)

# setting up theme
ptions(repr.plot.width = 1, repr.plot.height = 0.75)

ma_theme <- theme(
  line = element_blank(),
  text = element_text(family = "DejaVu Sans Mono", color = "#4a4a4a"),
  panel.background = element_rect(fill = "#efe8d1"),
  panel.grid.major = element_line(colour = "#9ae5de", size = 0.1),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.text.x = element_text(hjust = 0.7),
  plot.background = element_rect(fill = "#efe8d1", color = NA),
  legend.background = element_rect(fill = "#efe8d1"),
  strip.background = element_blank()

)

# Focusing on education 


edu <- V4_T2.5 %>% 
  as_tibble() %>%
  clean_names() 


edu_sub_county <- edu %>%
  filter(sub_county != "KENYA") %>%
  select(sub_county, gender, total, pre_primary,
         primary, secondary, university)


counties <- V4_T1.9	%>% pull(County) %>% unique()

edu_county <- edu_sub_county %>% 
  filter(sub_county %in% counties) %>%
  pivot_longer(cols = pre_primary:university,
               names_to = "level",
               values_to = "reached") 
  

sf_data <- KenyaCounties_SHP %>% 
  st_as_sf() %>% 
  left_join(edu_county, by = c("County" = "sub_county")) 

map_reached <- sf_data %>%
  ggplot() +
  geom_sf(aes(fill = reached), size = 0.1) +
  scale_fill_viridis_c(direction = -1) +
  facet_grid(level ~ gender) +
  ma_theme +
  labs(title = "Education level reached by the Kenyan population",
       subtitle = "Divided by ducation level and gender")+
       #caption = "Bennour's|www.bennour.xyz")+
  theme(panel.border = element_blank(),
        strip.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "#efe8d1", color = NA))

cols_reached <-  edu_county %>%
  mutate(sub_county = fct_reorder(sub_county, reached, sum)) %>%
  ggplot() +
  geom_col(aes(x = reached,
               y = sub_county,
               fill = level)) +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_viridis_d() +
  facet_wrap(~ gender) +
  ma_theme 

library(patchwork)

map_reached + cols_reached 
ggsave("test.png", device = "png", scale = 2)
