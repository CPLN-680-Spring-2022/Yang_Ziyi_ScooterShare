# office ####
office <- opq (city_name) %>%
  add_osm_feature(key = 'office') %>%
  osmdata_sf(.)

office.sf <- st_geometry(office$osm_polygons) %>%
  st_transform(st_crs(chicagoTracts)) %>%
  st_sf() %>%
  cbind(., office$osm_polygons$name) %>%
  rename(NAME = office.osm_polygons.name) %>%
  na.omit()

office <- st_geometry(office$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Office',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)