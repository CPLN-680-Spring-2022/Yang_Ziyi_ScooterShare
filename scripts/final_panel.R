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

census_panel <- census_geoinfo %>% dplyr::select(-tractce10)


# knn for each spatial effects ####
census_panel$KNN_college <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                        coordinates(college %>% st_coordinates()),
                                        1)
census_panel$KNN_restaurant <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                           coordinates(restaurant %>% st_coordinates()),
                                           5)

census_panel$KNN_public_transport <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                                 coordinates(public_transport %>% st_coordinates()),
                                                 5)

census_panel$KNN_office <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                       coordinates(office %>% st_coordinates()),
                                       5)

census_panel$KNN_retail <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                       coordinates(retail %>% st_coordinates()),
                                       5)


census_panel$KNN_tourism <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                        coordinates(tourism %>% st_coordinates()),
                                        5)

census_panel$KNN_leisure <- nn_function(coordinates(as.data.frame(census_panel)[,2:3]),
                                        coordinates(leisure %>% st_coordinates()),
                                        5)


## count and density #### square foot to square kilometer
census_geoinfo$area <- as.numeric(st_area(census_geoinfo))*9.29e-8
# retail 
retail_ct <- st_join(census_geoinfo %>% st_intersection(boundary), retail) %>%
  group_by(geoid10,area) %>%
  summarise(count_retail= n())
retail_ct$density_retail <- retail_ct$count_retail/retail_ct$area
retail_ct$ratio_retail <- retail_ct$count_retail/12722

# office 
office_ct <- st_join(census_geoinfo %>% st_intersection(boundary), office) %>%
  group_by(geoid10,area) %>%
  summarise(count_office= n())
office_ct$density_office <- office_ct$count_office/office_ct$area
office_ct$ratio_office <- office_ct$count_office/1845

# restaurant 
restaurant_ct <- st_join(census_geoinfo %>% st_intersection(boundary),restaurant) %>%
  group_by(geoid10,area) %>%
  summarise(count_restaurant= n())
restaurant_ct$density_restaurant <- restaurant_ct$count_restaurant/restaurant_ct$area
restaurant_ct$ratio_restaurant <- restaurant_ct$count_restaurant/8335

# public transport
public_transport_ct <- st_join(census_geoinfo %>% st_intersection(boundary),public_transport) %>%
  group_by(geoid10,area) %>%
  summarise(count_pubtran= n())
public_transport_ct$density_pubtran <- public_transport_ct$count_pubtran/public_transport_ct$area
public_transport_ct$ratio_pubtran <- public_transport_ct$count_pubtran/6068

# cycleway
cycleway_ct_len <- st_intersection(cycleway, boundary) %>%
  mutate(length = as.numeric(st_length(.))*0.000189394) %>%
  group_by(geoid10) %>%
  summarise(total_length = sum(length)) %>%
  st_set_geometry(NULL) %>%
  merge(census_geoinfo, on='geoid10', all.y=T) %>%
  st_as_sf()
cycleway_ct_len$total_length <- replace_na(cycleway_ct_len$total_length,0)
cycleway_ct_len$ratio_cycleway <- cycleway_ct_len$total_length/3434

# leisure
leisure_ct <- st_join(census_geoinfo %>% st_intersection(boundary), leisure) %>%
  group_by(geoid10,area) %>%
  summarise(count_leisure= n())
leisure_ct$density_leisure <- leisure_ct$count_leisure/leisure_ct$area
leisure_ct$ratio_leisure <- leisure_ct$count_leisure/31014

# tourism
tourism_ct <- st_join(census_geoinfo %>% st_intersection(boundary), tourism) %>%
  group_by(geoid10,area) %>%
  summarise(count_tourism= n())
tourism_ct$density_tourism <- tourism_ct$count_tourism/tourism_ct$area
tourism_ct$ratio_tourism <- tourism_ct$count_tourism/2204


# college
college_ct <- st_join(census_geoinfo %>% st_intersection(boundary), college) %>%
  group_by(geoid10,area) %>%
  summarise(count_college= n())
college_ct$density_college <- college_ct$count_college/college_ct$area
college_ct$ratio_college <- college_ct$count_college/85

spatial_panel <- left_join(census_panel, retail_ct%>%st_set_geometry(NULL)%>%dplyr::select(geoid10, count_retail, density_retail,ratio_retail), by = 'geoid10') %>%
  left_join(office_ct %>% st_set_geometry(NULL) %>% dplyr::select(geoid10, count_office, density_office,ratio_office), by = 'geoid10') %>%
  left_join(leisure_ct %>% st_set_geometry(NULL) %>% dplyr::select(geoid10, count_leisure, density_leisure,ratio_leisure), by = 'geoid10') %>%
  left_join(tourism_ct %>% st_set_geometry(NULL) %>% dplyr::select(geoid10, count_tourism, density_tourism,ratio_tourism), by = 'geoid10') %>%
  left_join(public_transport_ct %>% st_set_geometry(NULL) %>% dplyr::select(geoid10, count_pubtran,density_pubtran,ratio_pubtran), by = 'geoid10') %>%
  left_join(restaurant_ct %>% st_set_geometry(NULL) %>% dplyr::select(geoid10, count_restaurant, density_restaurant,ratio_restaurant), by = 'geoid10') %>%
  left_join(college_ct %>% st_set_geometry(NULL) %>% dplyr::select(geoid10, count_college, density_college,ratio_college), by = 'geoid10') %>%
  left_join(cycleway_ct_len %>% st_set_geometry(NULL) %>% dplyr::select(geoid10, total_length,ratio_cycleway), by = 'geoid10')

# ratio
CH_spatial_census$ratio_retail <- CH_spatial_census$count_retail/length(CH_spatial_census$count_retail)[1]
CH_spatial_census$ratio_office <- CH_spatial_census$count_office/length(CH_spatial_census$count_office)[1]
CH_spatial_census$ratio_restaurant <- CH_spatial_census$count_restaurant/length(CH_spatial_census$count_office)[1]
CH_spatial_census$ratio_public_transport <- CH_spatial_census$count_pubtran/length(CH_spatial_census$count_pubtran)[1]
CH_spatial_census$ratio_leisure <- CH_spatial_census$count_leisure/length(CH_spatial_census$count_leisure)[1]
CH_spatial_census$ratio_tourism <- CH_spatial_census$count_tourism/length(CH_spatial_census$count_tourism)[1]
CH_spatial_census$ratio_college <- CH_spatial_census$count_college/length(CH_spatial_census$count_college)[1]
CH_spatial_census$ratio_cycleway <- CH_spatial_census$total_length/sum(CH_spatial_census$total_length)
#CH_spatial_census$ratio_street <- CH_spatial_census$street_length/sum(CH_spatial_census$street_length)


spatial_panel[is.na(spatial_panel)] <- 0

CH_spatial_census <- left_join(spatial_panel, CH_open_ct%>%st_set_geometry(NULL)%>%dplyr::select(origins_cnt, geoid10), by = 'geoid10')

CH_spatial_census_nozero <- filter(CH_spatial_census, origins_cnt > 0)%>%
  dplyr::select(-c(Mean_Commute_Time,geoid10, centroid_X, centroid_Y))%>%
  dplyr::select(-starts_with('density'), -starts_with('ratio'), -starts_with('count'), -ends_with('length'))%>% relocate(origins_cnt, .before = TotPop)
