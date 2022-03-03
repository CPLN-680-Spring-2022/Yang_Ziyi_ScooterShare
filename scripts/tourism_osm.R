tourism <- opq (city_name) %>%
  add_osm_feature(key = 'tourism', value = c("aquarium", "artwork", "attraction", "gallery", "museumm", "theme_park", 'viewpoint', 'zoo')) %>%
  osmdata_sf(.)


tourism.sf <- st_geometry(tourism$osm_polygons) %>%
  st_transform(st_crs(chicagoTracts)) %>%
  st_sf() %>%
  cbind(., tourism$osm_polygons$name) %>%
  rename(NAME = tourism.osm_polygons.name) %>%
  na.omit()

tourism <- st_geometry(tourism$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Tourism',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)
