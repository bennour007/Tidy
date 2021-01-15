source("prep.R")
################################################################################
# cleaning 

transit_cost <- transit_cost_raw %>% 
  drop_na() %>%
  mutate(country = if_else(country == "UK", "GB", country)) %>%
  mutate(tunnel_per = str_remove(tunnel_per, "%")) %>%
  mutate(country = countrycode(country, "iso2c", "country.name")) %>%
  mutate_at(vars(start_year, end_year, tunnel_per, real_cost), str_trim) %>%
  mutate_at(vars(start_year, end_year, tunnel_per, real_cost), as.numeric)

################################################################################
# Adding the number of lines for each country
# Adding the number of lines for each city
# Adding the total length of all lines for each country, didn't
# Adding the total length of all lines for each city, didn't


transit_updated_1 <- transit_cost %>%
  nest_by(country) %>%
  mutate(country_total_lines = nrow(data)) %>%
  unnest("data") %>%
  ungroup() %>%
  nest_by(city) %>%
  mutate(city_total_lines = nrow(data)) %>%
  unnest("data") %>%
  ungroup()

################################################################################

world <- ne_countries(scale = "medium" , returnclass = "sf")
country_names <- transit_updated_1$country %>% unique()
city_names <- transit_updated_1$city %>% unique()

################################################################################
# adding lat and long cooronates of each city we have to the data

lon_lat_cities <- world.cities %>%
  as_tibble() %>%
  mutate(name = str_remove(name, "'")) %>%
  filter(country.etc %in% country_names) %>%
  filter(name %in% city_names) %>%
  right_join(transit_updated_1, by = c("name" = "city")) %>%
  select(- country.etc)

# filtering only the countries in our data 

my_world <- world %>%
  filter(name %in% country_names)

my_world_updated <- my_world %>%
  left_join(lon_lat_cities, by = c("name" = "country"))
################################################################################
# plotting function 

# length of each line, in each city, and the ttoal number of lines
# in each city, a map, can be improved 

my_world_updated %>%
  ggplot() +
  geom_sf() +
  geom_point(data = my_world_updated, 
             aes(x = long, y = lat, color = length, size = city_total_lines), alpha = 0.05) +
  scale_color_viridis_c() +
  facet_wrap(~ subregion) +
  theme_void()

################################################################################
subregions <- my_world_updated %>%
  as_tibble() %>%
  select(name, subregion, -geometry) 

################################################################################
################################################################################
theme_plot <- theme(text = element_text(family = "DejaVu Sans Mono",
                                        color = "lightpink",
                                        margin = margin(t = 10)),
                    plot.title =  element_text(size = 11,
                                               hjust = 0),
                    plot.subtitle = element_text(size = 8,
                                                 hjust = 0),
                    plot.caption = element_text(size = 5,
                                                hjust = 0.5),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    strip.background = element_blank(),
                    axis.text = element_text(color = "#9ae5de"),
                    strip.text.x = element_text(color = "#efe8d1"),
                    strip.text.y = element_text(color = "#efe8d1"),
                    plot.background = element_rect(fill = "#4a4a4a"),
                    legend.background = element_rect(fill = "#4a4a4a"),
                    legend.box.background = element_rect(fill = "#4a4a4a", colour = NA),
                    panel.background = element_rect(fill= "#4a4a4a"),
                    plot.margin = margin(10, 100, 10, 100))
################################################################################
################################################################################

# 
p0 <- transit_updated_1 %>% 
  mutate(lines_status = if_else(end_year <= 2020, "completed", "not_completed")) %>%
  group_by(country, lines_status) %>%
  summarise(length = mean(length),
            tunnel_per = mean(tunnel_per),
            cost_km = mean(cost_km_millions)) %>%
  drop_na() %>%
  mutate(country = fct_reorder(country, cost_km)) %>%
  ggplot(aes(x = cost_km, y = country)) +
  geom_col(aes(fill = tunnel_per)) +
  facet_wrap(~ lines_status, 
             scale = "free_y", ) +
  labs(title = "Countries with the highest cost/Km in $\nfor completed and incomplete projects",
       subtitle = "Given the percentage of the underground infrastructure per project",
       x = "Average cost/Km (millions USD$)") +
  scale_fill_viridis_c() +
  theme_plot +
  theme(axis.text.x = element_text(angle = 45))



p1 <- transit_updated_1 %>%
  mutate(period_years = end_year - start_year) %>%
  group_by(country) %>%
  summarise(mean_period = mean(period_years),
            mean_length = mean(length),
            mean_tunnel_per = mean(tunnel_per)) %>%
  mutate(mean_tunnel_per = if_else(
    mean_tunnel_per < 100 & mean_tunnel_per >=75, 75, mean_tunnel_per
    )) %>%
  mutate(mean_tunnel_per = if_else(
    mean_tunnel_per < 75 & mean_tunnel_per >= 50, 50, mean_tunnel_per
  )) %>%
  mutate(mean_tunnel_per = if_else(
    mean_tunnel_per < 50 & mean_tunnel_per >= 25, 25, mean_tunnel_per
  )) %>%
  mutate(mean_tunnel_per = if_else(
    mean_tunnel_per < 25 & mean_tunnel_per >= 0, 0, mean_tunnel_per
  )) %>%
  mutate(mean_tunnel_per = as_factor(mean_tunnel_per)) %>%
  mutate(country = fct_reorder(country, mean_period)) %>%
  drop_na() %>%
  ggplot() +
  geom_col(aes(y = country, x = mean_period),fill= "#acc8d4", width = 0.7, size = 0.5) +
  facet_wrap(~ mean_tunnel_per, scale = "free_y") +
  labs(title = "A quartile decomposition for the effect of planned\npercentage of tunnels on the average construction period.",
       x = "Average construction period") +
  theme_plot

ggsave(p0, filename = "p0.png", height = 18, width = 14)
ggsave(p1, filename = "p1.png", height = 18, width = 14)
