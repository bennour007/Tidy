library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rvest)
library(latex2exp)

atheme <- theme(
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  panel.grid = element_blank(),
  panel.background = element_rect(fill = '#9fc2cc'),
  legend.background = element_rect(fill = '#9fc2cc'),
  plot.background = element_rect(
    fill = '#9fc2cc', 
    color = NA
  ),
  plot.title = element_text(
    family = "Atlantis Headline", 
    face = "bold", 
    size = 17.45,
    colour = "#474056",
    vjust = 0.3,
    hjust = 0.5
  ),
  plot.subtitle = element_text(
    family = "Helvetica-Narrow",
    size = 10, 
    vjust = 0.4,
    hjust = 0.5,
    colour = "#474056"
  ),
  plot.caption = element_text(
    family = "Atlantis Headline", 
    face = "italic", 
    size = 7,
    colour = "#474056"
    # vjust = 0.3,
    # hjust = 0.5
  ),
  legend.text = element_text(
    family = "Palatino", 
    size = 12.45,
    colour = "#694d75"
  ),
  legend.title = element_text(
    family = "Palatino", 
    size = 12.45,
    colour = "#694d75"
  )
)


################################################################################
################################################################################


water_data <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv')

tun_nn <- ne_countries(
  scale = "large", 
  returnclass = "sf"
  ) %>% 
  filter(
    sovereignt == "Tunisia"
    # sovereignt %in% c("Tunisia", "Algeria", "Libya")
  )

neighborhood <- tun_nn %>% 
  pull(name) %>% 
  unique()

water_countries_data <- water_data %>% 
  filter(
    # COUNTRY %in% neighborhood
    COUNTRY == "Tunisia"
  )


# LAKES <- ne_download(
#   scale = "large", 
#   type = 'lakes', 
#   category = 'physical', 
#   returnclass = "sf"
# )
# 
# RIVERS <- ne_download(
#   scale =  "large", type
#   type = 'rivers_lake_centerlines',
#   category = 'physical', 
#   returnclass = "sf"
# )

# african_rivers <- read_sf("another_week/rivers_africa_37333/rivers_africa_37333.shp")
african_rivers_1 <- read_sf("another_week/africa_rivers_1/africa_rivers_1.shp")

# african_lakes <- read_sf("another_week/hydrobasins_africa/hydrobasins_africa.shp")




################################################################################
################################################################################


normalised_data <- water_countries_data %>% 
  group_by(COUNTRY) %>% 
  mutate(
    # POP_SERVED = POP_SERVED/1000,
    nor_pop_serv = (POP_SERVED - mean(POP_SERVED))/sd(POP_SERVED)
  )
  

ggplot() +
  geom_sf(
    data = tun_nn,
    colour = '#f1ecce',
    fill = '#f1ecce'
  ) +
  geom_sf(
    data = african_rivers_1,
    color = '#9fc2cc',
    size = 0.69
  ) +
  geom_point(
    data = normalised_data,
    aes(
      x = LON_WWTP,
      y = LAT_WWTP,
      size = POP_SERVED,
      color = WASTE_DIS 
    ),
    alpha = 0.5
  ) + 
  scale_fill_gradientn(
    aesthetics = c("colour", "fill"),
    colors = MetBrewer::met.brewer("Troy")
  )+
  xlim(5, 14)+
  ylim(28, 38)+
  atheme +
  theme(
    legend.key = element_rect(fill = '#9fc2cc'),
    legend.position = c(0.5, 0.1),
    legend.box = "horizontal", legend.direction = "horizontal"
    # legend.justification = c(0.5, 1)
  ) +
  guides(
    size = guide_legend(
      title = "Population served per unit",
      title.position = "top",
      label.position = "bottom",
      title.hjust = 0.5,
      override.aes = list(
        color = "#331832",
        fill = "#331832",
        alpha = 1
      )
    ),
    color = guide_colorbar(
      barwidth = unit(15, "lines"),
      barheight = unit(1, "lines"),
      title.position = "top",
      title = "Waste discharged in m3 d-1",
      title.hjust = 0.5, 
      label.vjust = 0.5
      )
    ) +
  scale_size_continuous(labels = scales::label_number()) + 
  labs(
    title = 'WATER WASTE TREATMENT PLANTS IN TUNISIA',
    subtitle = "This map shows the SIZE of the WWTP and their location in the vicinity to natural water bodies.\nThe SIZE of these plants is decomposed in this map into 2 dimensions, number of people- \nserved by the plant and the waste discharged in m3d-1.",
    caption = "data from :Macedo et al, 2022 by way of Data is Plural \n Missing points present in data \nBENNOUR", 
  )
  
ggsave(
  "week38_2022/wwtp_tunisian.png",
  device = ragg::agg_png()
)
