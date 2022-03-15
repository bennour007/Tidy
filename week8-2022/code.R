library(tidyverse)
library(sf)
library(biscale)


freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv') %>% 
  janitor::clean_names()


overall <- freedom %>% 
  group_by(region_name) %>% 
  summarise(
    avg_CL = mean(cl),
    avg_PR = mean(pr),
    avg_L = (avg_CL + avg_PR)/2
  ) %>% 
  pivot_longer(
    cols = 2:3,
    names_to = 'index',
    values_to = 'value'
  ) %>% 
  mutate(
    region_name = fct_reorder(region_name, avg_L, .desc = T)
  ) %>% 
  ggplot()+
  geom_col(
    aes(
      value,
      region_name,
      fill = index
    ),
    position = 'dodge'
  ) + 
  geom_point(
    aes(
      avg_L,
      region_name
    ),
    shape = 18,
    size = 10,
    color = 'lightblue'
  )+
  labs(
    title = 'Overall year/region mean of Civil and Political liberties', 
    subtitle = 'less is better'
  )


################################################################################
################################################################################
################################################################################


freedom %>% 
  filter(
    year %in% 2009:2012
  ) %>% 
  mutate(
    year = as_factor(year),
    region_name = as_factor(region_name),
    l = (cl + pr)/2
  ) %>% 
  # group_by(region_name, year) %>% 
  #  summarise(
  #    avg_l = mean(l),
  #    avg_cl = mean(cl),
  #    avg_pr = mean(pr)
  #  ) %>% 
  ggplot(
    aes(
      year,
      l
    )
  )+
  stat_summary(
    fun.data="mean_sdl",  fun.args = list(mult=1), 
    geom = "pointrange", color = "#FC4E07", size = 0.4
    
  )+
  geom_jitter(
    aes(
      year,
      l
    ),
    width = 0.25
  ) + 
  facet_grid(~region_name) + 
  labs(
    title = 'Average liberty per year from 2009 to 2012 for each continent',
    subtitle = 'no major changes or disruptions on the aggregate level'
  )
    
  
################################################################################
################################################################################
################################################################################


freedom %>%
  group_by(status, region_name, year) %>% 
  summarise(
    country,
    number_of = n()
  ) %>% 
  ggplot(aes(
    year,
    number_of, 
    color = status
    )
  ) +
  geom_point() + 
  facet_grid(~region_name) + 
  labs( title = 'Number of counrtries per continent for each status from 1995 to 2020')

################################################################################
################################################################################
################################################################################


freedom %>% 
  filter(
    year == 2020
  ) %>% 
  count(region_name, status) %>% 
  pivot_wider(
    names_from = status,
    values_from = n
  ) %>% 
  janitor::clean_names() %>% 
  mutate(
    nf = replace_na(nf,0),
    t = f + nf + pf, 
    f = f / t,
    pf = pf / t,
    nf = nf / t
  ) %>% 
  gt::gt()
  
################################################################################
################################################################################
################################################################################

le_theme <- theme(
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(color = NA, 
                                  fill = "#D3E4CD"),
  plot.background = element_rect(color = NA, 
                                 fill = "#D3E4CD",
                                 size = 5),
  plot.title = element_text(family = "Changa One", 
                            color = "black",
                            size = 25, 
                            face = "bold",
                            hjust = 0.5,
                            margin = margin(t = 36, b = 6)),
  plot.subtitle = element_text(family = "Changa One", 
                               color = "#5F939A",
                               size = 20, 
                               hjust = 0.5,
                               margin = margin(t = 6, b = 20)),
  plot.caption = element_text(family = "Changa One", 
                              color = "#806A8A", 
                              size = 14, 
                              face = "plain",
                              hjust = 0.5,
                              margin = margin(t = 0, b = 36))
)

africa_map_data <- st_as_sf(
  rworldmap::getMap(resolution = 'high')
  ) %>% 
  filter(
    continent == 'Africa'
  ) 
  

africa_map <- africa_map_data %>%
  left_join(
    freedom %>% 
      filter(year == 2020), 
    by = c("NAME"="country")
  ) %>% 
  bi_class(
    x = pr, 
    y = cl, 
    dim= 3, 
    style = 'quantile'
  ) %>% 
  ggplot() +
  geom_sf(
    aes(
      fill = bi_class
    ),
    color = "grey20",
    lwd = 0.2,
    show.legend = F
  ) + 
  bi_scale_fill(
    pal = "DkCyan", 
    dim = 3
  )+ 
  labs(
    title = 'Political and Civil Liberties in Africa in 2020',
    subtitle = 'Lighter colors denote more freedom',
    caption = "@Bennour007sin|www.bennour.tn \n data: UN & Freedom House"
  ) + le_theme
  # bi_theme(
  #   bg_color = "#D3E4CD",
  #   base_family = "Changa One",
  #   base_size = 12
  # ) 
  

africa_map_legend <- bi_legend(
  pal = "DkCyan",
  dim = 3,
  xlab = "Political Rights",
  ylab = "Civil Liberties"
) + 
  bi_theme(
    bg_color = "#D3E4CD",
    base_family = "Changa One",
    base_size = 15
  )
  
africa_map_final <- cowplot::ggdraw() +
  cowplot::draw_plot(africa_map, 0, 0, 1, 1) +
  cowplot::draw_plot(africa_map_legend, 0.15, 0.25, 0.2, 0.2)

ggsave(
  here::here('week8-2022', '2022-18-freedom.pdf'),
  plot = africa_map_final, 
  width = 15, 
  height = 16.5, 
  device = cairo_pdf
)
