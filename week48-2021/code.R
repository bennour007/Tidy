################################################################################
################################################################################
# imporing libraries

library(tidyverse)
library(ggridges)

################################################################################
################################################################################
# importing data

episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')


################################################################################
################################################################################
# tweaking data

eps <- episodes %>% 
  select(
    season_number,
    episode_number,
    type,
    uk_viewers,
    rating,
    duration
  ) %>% 
  filter(
    type == "episode",
    season_number != 13
  ) %>% 
  group_by(season_number) %>% 
  mutate(
    season_number = factor(season_number),
    avg_viewers = uk_viewers %>% 
      mean() %>% 
      round(.,0) %>% 
      factor()
  ) 

################################################################################
################################################################################
# Data Viz


################################################################################
## the theme 

letheme <- theme(
  text = element_text(
    family = "DejaVu Sans Mono",
    color = "#3D2C8D"
  ),
  plot.title =  element_text(size = 11,
                             hjust = 0),
  plot.subtitle = element_text(size = 8,
                               hjust = 0),
  plot.caption = element_text(size = 5,
                              hjust = 0.5),
  panel.grid.major.x  = element_line(size = 0.1, color = "grey"),
  panel.grid.major.y  = element_line(size = 0.1, color = "grey"),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  axis.text = element_text(color = "#3D2C8D"),
  axis.ticks = element_blank(),
  strip.text.x = element_text(color = "#3D2C8D"),
  strip.text.y = element_text(color = "#3D2C8D"),
  plot.background = element_rect(fill = "#FEF5ED"),
  legend.background = element_rect(fill = "#FEF5ED"),
  legend.box.background = element_rect(fill = "#FEF5ED", colour = NA),
  panel.background = element_rect(fill= "#FEF5ED"),
  legend.key = element_rect(fill = "#FEF5ED")
)


################################################################################
## the plot 

drwho <- eps %>% 
  ggplot() +
  geom_density_ridges2(
    aes(
      rating,
      season_number,
      fill = avg_viewers,
      color = avg_viewers
    ),
    alpha = 0.8
  ) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1, accuracy = 1)
  ) +
  scale_fill_manual(
    values = MetBrewer::met.brewer("Nattier", 4)
  ) +
  scale_color_manual(
    values = MetBrewer::met.brewer("Nattier", 4)
  ) +
  labs(
    title = 'Distribution of the ratings given the average viewers per season',
    x = "Rating", 
    y = "season Number",
    caption = '@Bennour007sin|bennour.tn'
  ) + 
  guides(
    fill = guide_legend("Average viewers \nin the UK"),
    color = "none"
  ) +
  letheme

################################################################################
################################################################################
# Plot save


ggsave("~/Projects/tidy/week48-2021/drwho.png", drwho, dpi = "retina")
