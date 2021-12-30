################################################################################
################################################################################
# imporing libraries

library(tidyverse)
library(ggridges)

################################################################################
################################################################################
# importing data

starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')


################################################################################
################################################################################
# tweaking data

starbux <- starbucks %>% 
  # filter(size == "venti") %>% 
  # arrange(caffeine_mg) %>%
  # top_n(50) %>%
  mutate(
    total_fat_g = if_else(total_fat_g <= 1, 1, total_fat_g,),
    total_fat_g = if_else(total_fat_g <= 5 & total_fat_g > 1, 5, total_fat_g,),
    total_fat_g = if_else(total_fat_g <= 10 & total_fat_g > 5, 10, total_fat_g,),
    total_fat_g = if_else(total_fat_g <= 15 & total_fat_g >10, 15, total_fat_g,),
    total_fat_g = if_else(total_fat_g <= 20 & total_fat_g >15, 20, total_fat_g,),
    total_fat_g = if_else(total_fat_g <= 25 & total_fat_g >20, 25, total_fat_g,),
    total_fat_g = if_else(total_fat_g > 25, 30, total_fat_g,),
    total_fat_g = factor(total_fat_g),
    product_name = fct_reorder(product_name, calories),
    product_name = fct_lump(product_name, 20)
  ) %>% 
  pivot_longer(
    cols = c(milk, whip),
    names_to = "addition",
    values_to = "addition_serving"
  )

  
  

################################################################################
################################################################################
# Data Viz


################################################################################
## the theme 

letheme <- theme(
  text = element_text(
    family = "DejaVu Sans Mono",
    color = "#986D8E"
  ),
  plot.title =  element_text(size = 11,
                             hjust = 0),
  plot.subtitle = element_text(size = 8,
                               hjust = 0),
  plot.caption = element_text(size = 5,
                              hjust = 0.5),
  panel.grid.major.x  = element_line(size = 0.1, color = "#B1D0E0"),
  panel.grid.major.y  = element_line(size = 0.1, color = "#B1D0E0"),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  axis.text = element_text(color = "#116530"),
  axis.text.x = element_text(),
  axis.ticks = element_blank(),
  strip.text.x = element_text(color = "#116530"),
  strip.text.y = element_text(color = "#116530"),
  plot.background = element_rect(fill = "#C2FFF9"),
  legend.background = element_rect(fill = "#C2FFF9"),
  legend.box.background = element_rect(fill = "#C2FFF9", colour = NA),
  panel.background = element_rect(fill= "#C2FFF9"),
  legend.key = element_rect(fill = "#C2FFF9")
)


################################################################################
## the plot 

starbux %>% 
  ggplot() +
  geom_count(
    aes(
      y = product_name,
      x = calories,
      color = total_fat_g,
      size = sugar_g
    ),
    alpha = 0.5
  ) +
  # geom_point(
  #   aes(
  #     calories,
  #     product_name
  #   )
  # ) +
  scale_color_brewer(
    palette = "PuOr"
  ) +
  scale_x_binned(
    n.breaks = 20
  )+
  labs(
    title = "Top 20 Starbucks drinks with the highest calories.",
    subtitle = "Macros of sugar and fat are noted in the legend.",
    y = "Drink name",
    x = "Calories",
    caption = "@Bennour007sin|www.bennour.tn"
  ) +
  annotate(
    "text", 
    label = "How to read:\rThe circle size depict the sugar content, and the colours depict the fat content,\rmore purple-ish colours means more fat, and bigger circles means more sugar!",
    size = 3,
    color = "#986D8E", 
    family = "DejaVu Sans Mono",
    x = 300,
    y = "Cappuccino"
  ) +
  annotate(
    "text",
    x = 500,
    y = "English Breakfast Black Tea Latte",
    label = "Notice the relationship between\nthe calories content and the sugar\nand fat content(well, Obviously)",
    color = "#986D8E", 
    family = "DejaVu Sans Mono"
  ) +
  guides(
    color = guide_legend("Fat in g"),
    size = guide_legend("sugar in g")
  ) +
  # facet_grid(size~., scales = "free") +
  letheme
  