################################################################################
################################################################################
# imporing libraries

library(tidyverse)
library(janitor)
library(patchwork)
library(rKenyaCensus)
library(sf)

################################################################################
################################################################################
# importing data

edu <- V4_T2.5 %>% 
  as_tibble() %>%
  clean_names() 


counties <- V4_T1.9	%>% pull(County) %>% unique()


################################################################################
################################################################################
# tweaking data

edu_sub_county <- edu %>%
  filter(sub_county != "KENYA") %>%
  select(sub_county, gender, total, pre_primary,
         primary, secondary, university)

edu_county <- edu_sub_county %>% 
  filter(sub_county %in% counties) %>%
  pivot_longer(cols = pre_primary:university,
               names_to = "level",
               values_to = "reached") 

sf_data <- KenyaCounties_SHP %>% 
  st_as_sf() %>% 
  left_join(edu_county, by = c("County" = "sub_county")) 

################################################################################
################################################################################
# Data Viz


################################################################################
## the theme 

ma_theme <- theme(
  text = element_text(
    family = "DejaVu Sans Mono",
    color = "#986D8E"
  ),
  plot.background = element_rect(fill = "#C9CCD5"),
  legend.background = element_rect(fill = "#C9CCD5"),
  legend.box.background = element_rect(fill = "#C9CCD5", colour = NA),
  panel.background = element_rect(fill= "#C9CCD5"),
  legend.key = element_rect(fill = "#C9CCD5"),
  strip.background = element_rect(fill = "#C9CCD5"),
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank()
)


################################################################################
## the plot 

map_reached <- sf_data %>%
  ggplot() +
  geom_sf(
    aes(
      fill = reached
    ), 
    size = 0.1
  ) +
  facet_grid(
    gender ~ level
  ) +
  labs(
    title = "Education level reached by the Kenyan population",
    subtitle = "Divided by ducation level and gender",
    caption = "@Bennour007sin|bennour.tn"
  ) +
  scale_fill_gradientn(
    colors = MetBrewer::met.brewer("Isfahan1",type = "continuous")
  ) +
  ma_theme


ggsave("week4-2021/reached.png", map_reached, dpi = "retina")
