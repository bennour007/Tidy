library(tidyverse)
#library(gghighlight)

# Theme set 

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
  

# data import 

plastic <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

# data cleaning 

tidy_plastic <- plastic %>%
  pivot_longer(cols = empty:pvc, 
               names_to = "plastic_type",
               values_to = "plastic_count") %>%
  rename(company_total = "grand_total") %>%
  mutate(country = tolower(country))

# Generate plastic counts for each country per type (currently per company)

tidy_plastic_mutated <- tidy_plastic %>%
  filter(year == 2019,
         plastic_type != "empty",
         country != "empty") %>%
  group_by(country, plastic_type) %>%
  mutate(total_type_count = sum(plastic_count)) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(total_count = plastic_count %>% sum,
         total_volunteers = sum(volunteers)) %>%
  ungroup() 

# volunteers vs plastic type count per country
  
tidy_plastic_mutated %>%
  filter(parent_company != "Grand Total") %>%
  mutate(country = fct_reorder(country, total_type_count)) %>%
  ggplot() +
  geom_point(aes(x = total_type_count,
                 y = total_volunteers,
                 size = num_events),
             alpha = 0.4, 
             color = "skyblue") +
  scale_x_continuous(labels = scales::comma, 
                     trans = "log10") +
  scale_y_continuous(labels = scales::comma, 
                     limits = c(0,35000000)) +
  geom_text(aes(x = total_type_count,
                y = total_volunteers,
                label = country),
            alpha = 0.3,
            size = 3,
            hjust = -0.1,
            vjust = 0.1,
            nudge_x = 0.05,
            check_overlap = T) +
  facet_grid(plastic_type ~. , 
             space = "free") +
  labs(title = "Volunteers vs plastic type count per country",
       x = "Plastic type count for each country(log scaled)",
       y = "Volunteers for each country",
       size = "# of events") +
  ma_theme +
  theme(legend.key = element_blank())

tidy_plastic_mutated %>%
  filter(year == 2019,
         parent_company != "Grand Total") %>%
  mutate(parent_company = fct_lump(parent_company, 20),
         parent_company = fct_reorder(parent_company, company_total)) %>%
  filter(parent_company != "Other", 
         parent_company != "Unbranded") %>%
  unite(col = "company", c("country","parent_company"), sep = "|") %>% 
  ggplot() +
    geom_col(aes(x = company_total,
                 y = company))
  geom_boxplot(aes(x = company_total,
               y = parent_company))
  
# Stuff to be done in the next updates: 
  # 1- Enlarging the space between the title, and the axis title.
  # 2- xlim change.
  # 3- the alpha thingy must be updated as well there is not consistency in the 
  # alphas.
  

