library(tidyverse)
library(patchwork)



nuclear_explosions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")


le_theme <- theme(
  panel.background = element_rect(
    fill = '#E5EFC1',
    color = NA
  ),
  plot.background = element_rect(
    fill = '#E5EFC1',
    color = NA
  ),
  strip.background = element_rect(
    fill = '#E5EFC1',
    color = NA
  ),
  legend.background = element_rect(
    fill = '#E5EFC1',
    color = NA
  ),
  legend.box.background = element_rect(
    fill = '#E5EFC1',
    color = NA
  ),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  legend.text = element_text(
    family = 'URWGothic'
  ),
  legend.title = element_text(
    family = 'URWGothic'
  ),
  plot.title = element_text(
    family = 'URWGothic', 
    size = 25,
    hjust = 0.5, 
    colour = '#463333'
  ),
  plot.subtitle = element_text(
    family = 'URWGothic', 
    hjust = 0.5, 
    colour = '#835858'
  ),
  plot.caption = element_text(
    family = 'URWGothic', 
    hjust = 1, 
    colour = '#A97555'
  ),
  axis.text = element_blank(),
  axis.title = element_blank(),
  strip.text = element_text(
    family = 'URWGothic'
  ),
  legend.key = element_rect(colour = NA, fill = NA),
  legend.position = 'bottom',
  plot.margin = margin(15, 25, 15, 25)
)


nuclear_explosions <- nuclear_explosions %>% 
  mutate(
    country = if_else(
      country == 'USSR', 
      'RUSSIA',
      country
    ),
    country = if_else(
      country == 'PAKIST',
      'PAKISTAN',
      country
    ),
    country = if_else(
      country == 'UK',
      'UNITED KINGDOM',
      country
    ),
    country = tolower(country),
    yield_upper = na_if(yield_upper, 0)
  ) 




map_data <- sf::st_as_sf(
  rworldmap::getMap(resolution = 'high')
)


map <- map_data %>%  
  ggplot() + 
  geom_sf(
    fill = '#85C88A',
    color = '#F3E9DD',
    size = 0.05
  ) + 
  geom_point(
    data = nuclear_explosions,
    aes(
      longitude,
      latitude, 
      size = yield_upper,
      color = country
    ),
    alpha = 0.7
  ) +
  scale_color_manual(values=MetBrewer::met.brewer("Juarez", 7))+
  guides(
    colour = guide_legend(
      title = "Country",
      override.aes = list(size=6),
      title.position = 'top',
      title.hjust = 0.5
    ),
    size = guide_legend(
      title = 'Upper yield Magniture in Kiltons',
      title.position = 'top',
      title.hjust = 0.5
    )
  )+
  labs(
    title = 'Nuclear Explosions around the world from 1945 to 1998',
    subtitle = 'Color depict the country owner of the Device and size reflects the Magnitude in kilotons of TNT',
    caption = "@Bennour007sin | www.bennour.tn \n data: SIPRI"
  ) +
  le_theme 


numbers <- nuclear_explosions %>%
  count(country, name = 'count') %>% 
  mutate(
    country = str_to_upper(country),
    country = fct_reorder(country, count)
  ) %>% 
  ggplot(
    aes(
      count,
      country,
      label = count
    )
  ) + 
  geom_col(
    fill = '#85C88A'
  ) +
  ggrepel::geom_text_repel(family = 'Courier') +
  scale_x_continuous(breaks = scales::breaks_extended()) + 
  labs(
    title = 'Number of nuclear explosions per county from 1945 to 1998'
  ) +
  theme(
    panel.background = element_rect(
      fill = '#E5EFC1',
      color = NA
    ),
    plot.background = element_rect(
      fill = '#E5EFC1',
      color = NA
    ),
    strip.background = element_rect(
      fill = '#E5EFC1',
      color = NA
    ),
    legend.background = element_rect(
      fill = '#E5EFC1',
      color = NA
    ),
    legend.box.background = element_rect(
      fill = '#E5EFC1',
      color = NA
    ),
    axis.text = element_text(
      family = 'URWGothic'
    ),
    axis.title = element_text(
      family = 'URWGothic'
    ),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(
      family = 'URWGothic'
    ),
    legend.title = element_text(
      family = 'URWGothic'
    ),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      family = 'URWGothic', 
      size = 18,
      hjust = 0.5, 
      colour = '#463333'
    ),
    plot.subtitle = element_text(
      family = 'URWGothic', 
      hjust = 0.5, 
      colour = '#835858'
    ),
    plot.caption = element_text(
      family = 'URWGothic', 
      hjust = 0.5, 
      colour = '#A97555'
    ),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.position = 'bottom'
  )



timed <- nuclear_explosions %>% 
  group_by(year) %>% 
  summarise(
    count = n(),
    total = sum(count, na.rm =T)
  )  %>% 
  ggplot(
    aes(
      year, 
      count
    )
  ) + 
  geom_point(
    color = '#85C88A'
  )+
  geom_line(
    color = '#85C88A'
  ) +
  scale_x_continuous( 
    breaks = scales::breaks_width(5)
  ) + 
  labs(
    title = 'Evolution of the number of Nuclear explosions between 1945 and 1998'
  ) + 
  theme(
    panel.background = element_rect(
      fill = '#E5EFC1',
      color = NA
    ),
    plot.background = element_rect(
      fill = '#E5EFC1',
      color = NA
    ),
    strip.background = element_rect(
      fill = '#E5EFC1',
      color = NA
    ),
    legend.background = element_rect(
      fill = '#E5EFC1',
      color = NA
    ),
    legend.box.background = element_rect(
      fill = '#E5EFC1',
      color = NA
    ),
    axis.text = element_text(
      family = 'URWGothic'
    ),
    axis.title = element_text(
      family = 'URWGothic'
    ),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(
      family = 'URWGothic'
    ),
    legend.title = element_text(
      family = 'URWGothic'
    ),
    panel.grid.major = element_line(
      size = 0.1
    ),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      family = 'URWGothic', 
      size = 18,
      hjust = 0.5, 
      colour = '#463333'
    ),
    plot.subtitle = element_text(
      family = 'URWGothic', 
      hjust = 0.5, 
      colour = '#835858'
    ),
    plot.caption = element_text(
      family = 'URWGothic', 
      hjust = 0.5, 
      colour = '#A97555'
    ),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.position = 'bottom',
    plot.margin = margin(10, 20, 10, 20)
  )

le_plot <- map / (timed | numbers)

ggsave(
  'week34-2019/nuclear.jpg',
  plot = map, device = 'jpg',
  width = 15, 
  height = 10
)  

