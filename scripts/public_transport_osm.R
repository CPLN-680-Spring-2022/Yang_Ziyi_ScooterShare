public_transport <- opq (city_name) %>%
  add_osm_feature(key = 'public_transport', value = c("stop_position", "station")) %>%
  osmdata_sf(.)

public_transport.sf <- st_geometry(public_transport$osm_polygons) %>%
  st_transform(st_crs(chicagoTracts)) %>%
  st_sf() %>%
  cbind(., public_transport$osm_polygons$name) %>%
  rename(NAME = public_transport.osm_polygons.name) %>%
  na.omit()


public_transport <- st_geometry(public_transport$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Public.Transport',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)
