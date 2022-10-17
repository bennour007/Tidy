
rivers_africa  <- read_html("https://en.wikipedia.org/wiki/List_of_river_systems_by_length") %>% 
  html_nodes("table") %>% 
  nth(6) %>% 
  html_table(header = T)


names(rivers_africa) <- c(
  "rank",                                            
  "river",                                           
  "length_km",                                     
  "length_miles",                                  
  "drainage_area_km2",             
  "average_discharge_m3ps",        
  "outflow",                                         
  "countries"
)

clean_african_rivers <- rivers_africa %>% 
  separate_rows(countries, sep = ",") %>% 
  mutate(
    countries = str_remove_all(countries, "[:digit:]"),
    countries = str_remove_all(countries, "[:punct:]"),
    countries = str_remove_all(countries, "[()]"),
    countries = str_trim(countries, side = "both")
  ) %>% 
  filter(countries %in% african_countries)


clean_african_rivers %>% 
  pull(river) %>% 
  unique()


RIVERS %>% 
  pull(name) %>% 
  unique()



