q0 <- opq(bbox = c(-88.30,42.20,-87.60,41.50)) 




restaurant <- add_osm_feature(opq = q0, key = 'amenity', value = c("restaurant", "fast_food")) %>%
  osmdata_sf(.)

restaurant.sf <- st_geometry(restaurant$osm_polygons) %>%
  st_transform(st_crs(chicagoTracts)) %>%
  st_sf() %>%
  cbind(., restaurant$osm_polygons$name) %>%
  rename(NAME = restaurant.osm_polygons.name) %>%
  na.omit()

restaurant <- st_geometry(restaurant$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Restaurant',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)
