library(tidyverse)
library(geojsonio)



colony <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

bees_theme <- theme(
  panel.background = element_rect(
    fill = '#EBD4D4',
    color = NA
  ),
  plot.background = element_rect(
    fill = '#EBD4D4',
    color = NA
  ),
  strip.background = element_rect(
    fill = '#EBD4D4',
    color = NA
  ),
  legend.background = element_rect(
    fill = '#EBD4D4',
    color = NA
  ),
  legend.box.background = element_rect(
    fill = '#EBD4D4',
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
    hjust = 0.5, 
    colour = '#A97555'
  ),
  axis.text = element_blank(),
  axis.title = element_blank(),
  strip.text = element_text(
    family = 'URWGothic'
  ),
  legend.key = element_rect(colour = NA, fill = NA),
  legend.position = 'bottom'
)

colony_state <- colony %>% 
  group_by(
    state
  ) %>% 
  summarise(
    colony_n = mean(colony_n, na.rm = T),
    loss_pct = mean(colony_lost_pct, na.rm = T),
    reno_pct = mean(colony_reno_pct, na.rm = T), # we can have renoval pct bigger than the loss we just renovated more than we lost :P
    reno_loss_pct = reno_pct / loss_pct
  )

stressor_state <- stressor %>% 
  group_by(state, stressor) %>% 
  summarise(
    mean_stress = mean(stress_pct, na.rm = T) # mean stress  over the years
  ) %>% 
  mutate(
    max = max(mean_stress)
  ) %>% 
 filter(
   mean_stress == max
 )

bees_states <- colony_state %>% 
  left_join(
    stressor_state, by = 'state'
  )

################################################################################
################################################################################
# prep hex map 
################################################################################

map_hex <- 
  geojson_read(
    'week02-2022/us_states_hexgrid.geojson',  
    what = "sp"
  )

map_hex@data <-
  map_hex@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

map_hex_fortified <- broom::tidy(map_hex, region = "google_name")

## smaller polygons for frame
# map_hex_buffer <-
#   rgeos::gBuffer(map_hex, width = -.15, byid = T)
# 
# map_hex_buffer_fortified <- broom::tidy(map_hex_buffer, region = "google_name")


## calculate centroids
centr <- cbind.data.frame(
  data.frame(
    gCentroid(map_hex, byid = T),
    id = map_hex@data$iso3166_2,
    id_long = str_wrap(map_hex@data$google_name, 12)
  )
) %>% 
  left_join(
    bees_states,
    by = c('id_long' = 'state')
  ) %>% 
  mutate(
    reno_loss_pct = round((reno_loss_pct * 100), 2), 
    reno_loss = paste(reno_loss_pct, '%'),
    reno_loss = if_else(
      reno_loss == 'NA %',
      'No Data',
      reno_loss
    )
  )

## Combine data

combined <- 
  map_hex_fortified %>% 
  left_join(
    bees_states, 
    by = c('id' = 'state')
  )
  
bees <- combined  %>% 
  ggplot() +
  geom_polygon(
    aes(
      long, 
      lat,
      group = group,
      fill = log(colony_n)
    ),
    color = '#A0937D'
  ) + 
  geom_text(
    data = centr,
    aes(
      x, 
      y = y -0.5, 
      label = reno_loss
    ),
    color = 'white',
    family = 'Courier'
  ) +
  geom_text(
    data = centr,
    aes(
      x, 
      y = y + 0.6, 
      label = id
    ),
    color = 'white',
    family = 'Courier'
  )+
  scale_fill_gradientn(
    colors = MetBrewer::met.brewer("Morgenstern")
  ) + 
  labs(
    title = 'Percentage of Renewed to lost Bees colonies across the US',
    subtitle = 'Colors depict the volume(logarithmic) of the colonies of each state, data averaged from 2015 to 2020',
    caption = "@Bennour007sin | www.bennour.tn \n data: USDA"
  ) +
  guides(
    fill = guide_colorbar(
      barheight = unit(3, units = "mm"),  
      barwidth = unit(80, units = "mm"),
      direction = "horizontal",
      ticks.colour = "#e8d8c3",
      title.position = "top",
      title.hjust = 0.5,
      title = 'Colonies'
    ) , 
  ) +
  bees_theme


ggsave(
  here::here('week02-2022', '2022-02-bees.jpg'),
  plot = bees, 
  width = 15, 
  height = 10, 
  device = 'jpg'
)  

