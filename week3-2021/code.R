
source("prep.R")

################################################################################

full <- left_join(
  artwork %>% select(- 17:20), 
  artists %>% select(- url),
  c("artist" = "name")
) %>%
  mutate(artist_status = if_else(is.na(yearOfDeath), "alive", "dead"),
         art_type = if_else(is.na(depth), "2D", "3D"),
         surface = if_else(!is.na(depth), width*height, NULL),
         decade = year- year %% 10,
         dims = width*height,
         ratio = height/width,
         decade_year = year %% 10)

################################################################################

ma_theme <- theme(
  line = element_blank(),
  text = element_text(family = "courier", color = "#8abbd0"),
  panel.background = element_rect(fill = "#4a4a4a"),
  panel.border = element_blank(),
  panel.grid.major = element_line(colour = "#2b2d42", size = 0.1),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "#4a4a4a")
)

################################################################################

full %>%
  count(artist, sort = T) %>%
  mutate(freq = n / sum(n))
# drop Turner

full %>% 
  filter(artist != "Turner, Joseph Mallord William",
         decade >= 1700) %>%
  count(acquisitionYear,art_type, decade, sort = T) %>%
  filter(!is.na(decade)) %>%
  mutate(acquisition_decade = acquisitionYear-acquisitionYear%%10) %>%
  group_by(acquisition_decade) %>%
  mutate(decade_acquisitions = sum(n)) %>%
  ungroup() %>%
  #mutate(acquisition_decade = factor(acquisition_decade)) %>%
  filter(!is.na(acquisition_decade)) %>%
  ggplot(aes(x = acquisition_decade, 
             y = decade_acquisitions)) +
  geom_col(fill = "#acc8d4", position = "dodge")+
  geom_text(aes(label = decade_acquisitions), 
            position =  position_dodge(0.9),
            vjust = -1, 
            color = "#91b8bd", 
            family = "courier", size = 3) +
  labs(title = "Development of Tate acquisitions over time",
       y = "Number of acquisitions",
       x = "Time(decades)")+
  ma_theme +
  theme(axis.text = element_text(color = "#8abbd0", size = 10))



full %>%
  filter(art_type == "2D",
         artist != "Turner, Joseph Mallord William",
         decade >= 1700,
         dims %in% c(17201:122816)) %>% 
  mutate(decade = factor(decade)) %>%
  ggplot(aes(x = dims, 
             y = decade)) +
  geom_density_ridges(fill = "#acc8d4", 
                      color = "#d4dddd") +
  geom_vline(aes(xintercept = median(dims)), 
             color = "#efe8d1", 
             linetype = "dotted") +
  labs(title = "Distribution of surfaces",
       subtitle = "elements between the first and the third quartiles",
       x = 'Surface(in mm)', 
       y = "Decade") +
  ma_theme +
  theme(axis.text = element_text(color = "#8abbd0", size = 10))


full %>%
  filter(art_type == "2D",
         artist != "Turner, Joseph Mallord William",
         decade >= 1700,
         dims %in% c(17201:122816)) %>%
  mutate(decade = factor(decade)) %>%
  ggplot() +
  geom_density_ridges(aes(x = ratio, 
                          y = decade),
                      fill = "#acc8d4", 
                      color = "#d4dddd") +
  geom_vline(aes(xintercept = 1), 
             color = "#efe8d1", 
             linetype = "dotted") +
  labs(title ="Distribution of the ratios",
       subtitle = "1 indicates a square : equal height/width ratio",
       x = "Height/width ratio",
       y = "Decade") +
  ma_theme +
  theme(axis.text = element_text(color = "#8abbd0", size = 10))

################################################################################

full %>%
  filter(art_type == "2D",
         artist != "Turner, Joseph Mallord William",
         decade >= 1700,
         dims %in% c(17201:122816)) %>%
  count(medium, decade, art_type, sort = T) %>%
  mutate(medium = fct_lump(medium, 15)) %>%
  mutate(decade = factor(decade)) %>%
  filter(medium != "Other",
         !is.na(medium)) %>%
  ggplot() +
  geom_point(aes(y = medium, 
                 x = decade, 
                 size = n), 
             alpha = 0.6, 
             shape = 15, 
             color = "#acc8d4") +
  ma_theme +
  theme(axis.text = element_text(color = "#8abbd0", size = 10),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank())

full %>%
  filter(art_type == "3D",
         artist != "Turner, Joseph Mallord William",
         decade >= 1700,
         dims %in% c(17201:122816)) %>%
  count(medium, decade, art_type, sort = T) %>%
  mutate(medium = fct_lump(medium, 15)) %>%
  mutate(decade = factor(decade)) %>%
  filter(medium != "Other",
         !is.na(medium)) %>%
  ggplot() +
  geom_point(aes(y = medium, 
                 x = decade, 
                 size = n), 
             alpha = 0.6, 
             shape = 15, 
             color = "#acc8d4") +
  ma_theme +
  theme(axis.text = element_text(color = "#8abbd0", size = 10),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank())
