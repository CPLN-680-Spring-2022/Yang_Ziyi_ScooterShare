retail <- opq (city_name) %>%
  add_osm_feature(key = 'shop') %>%
  osmdata_sf(.)

retail.sf <- st_geometry(retail$osm_polygons) %>%
  st_transform(st_crs(chicagoTracts)) %>%
  st_sf() %>%
  cbind(., retail$osm_polygons$name) %>%
  rename(NAME = retail.osm_polygons.name) %>%
  na.omit()

retail <- st_geometry(retail$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Retails',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)
