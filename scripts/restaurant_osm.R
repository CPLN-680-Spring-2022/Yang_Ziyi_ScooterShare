# restaurant ####
restaurant <- opq (city_name) %>%
  add_osm_feature(key = 'amenity', value = c("restaurant", "fast_food")) %>%
  osmdata_sf(.)

restaurant <- st_geometry(restaurant$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Restaurant',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

restaurant.sf <- st_geometry(restaurant$osm_polygons) %>%
  st_transform(st_crs(chicagoTracts)) %>%
  st_sf() %>%
  cbind(., restaurant$osm_polygons$name) %>%
  rename(NAME = restaurant.osm_polygons.name) %>%
  na.omit()
