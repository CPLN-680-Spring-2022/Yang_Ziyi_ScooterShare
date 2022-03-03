census_ct = CH_Census_ct
origin_ct = CH_open_ct
census_geoinfo = CH_Census_ct 
boundary = CH_Census_ct
proj = CH_proj
city_name = "Chicago"

ggplot()+
  geom_sf(data = census_ct, fill = "white")+
  #geom_sf(data = LV_college, shape = 23, fill = "cornflowerblue", size = 2)+
  geom_sf(data = college, color = "red", size = 1.5)+
  geom_sf(data = boundary, fill='transparent')+
  labs(title = paste("Location of offices, retails, and colleges in",city_name),
       subtitle = "Red dots as office, orange dots as retails, and blue dots as colleges") +
  mapTheme()

grid.arrange(
  ggplot()+
    geom_sf(data = census_ct, fill = "white")+
    geom_sf(data = cycleway, color = "chartreuse3", size = 1.5, alpha = 0.6)+
    geom_sf(data = leisure, color = "lightsalmon",alpha = 0.6)+
    geom_sf(data = boundary,fill='transparent')+
    labs(title = paste("Location of cycleway and leisure places in", city_name),
         subtitle = "Green lines as cycleway and light pink dots as leisure places") +
    mapTheme(),
  
  ggplot()+
    geom_sf(data = census_ct, fill = "white")+
    geom_sf(data = restaurant, color = "turquoise",alpha = 0.6)+
    geom_sf(data = tourism, color = "hotpink", alpha = 0.6)+
    geom_sf(data = boundary,fill='transparent')+
    labs(title = paste("Location of restaurant and tourism spots in", city_name),
         subtitle = "Turqoise dots as restaurants and pink dots as tourism spots") +
    mapTheme(),
  
  ggplot()+
    geom_sf(data = census_ct, fill = "white")+
    geom_sf(data = office, color = "red", alpha = 0.6, size = 2)+
    geom_sf(data = retail, color = "orange", alpha = 0.6, size = 2)+
    geom_sf(data = college, shape = 23, fill = "cornflowerblue", size = 2)+
    geom_sf(data = boundary,fill='transparent')+
    labs(title = paste("Location of offices, retails, and colleges in", city_name),
         subtitle = "Red dots as office, orange dots as retails, and blue dots as colleges\n") +
    mapTheme(),
  ncol = 3)
