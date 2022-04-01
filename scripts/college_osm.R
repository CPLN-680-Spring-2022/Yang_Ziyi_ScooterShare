q0 <- opq(bbox = c(-88.30,42.20,-87.60,41.50)) 

college1 <- add_osm_feature(opq = q0, key = 'amenity', value = c("university", "college")) %>%
  osmdata_sf(.)

college.sf <- st_geometry(college1$osm_polygons) %>%
  st_transform(st_crs(chicagoTracts)) %>%
  st_sf() %>%
  cbind(., college1$osm_polygons$name) %>%
  rename(NAME = college1.osm_polygons.name) %>%
  na.omit()


college <- opq (city_name) %>%
  add_osm_feature(key = 'amenity', value = c("university", "college")) %>%
  osmdata_sf(.)
# 
college <- st_geometry(college$osm_polygons) %>%
  st_transform(proj) %>%
  st_sf()%>%
  st_intersection(boundary) %>%
  st_centroid() %>%
  mutate(Legend = 'College',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

# cycleway ####
cycleway <- opq (city_name) %>%
  add_osm_feature(key = 'cycleway') %>%
  osmdata_sf(.)

cycleway <- st_geometry(cycleway$osm_lines) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Cycleway',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)

cycleway %>% st_join(census_geoinfo %>% st_intersection(boundary))


# leisure  ####
leisure <- opq (city_name) %>%
  add_osm_feature(key = 'leisure', value = c('adult_gaming_center','amusement_arcade','common','fitness_center','hackerspace','park',
                                             'pitch','stadium')) %>%
  osmdata_sf(.)

leisure <- st_geometry(leisure$osm_points) %>%
  st_transform(proj) %>%
  st_sf() %>%
  st_intersection(boundary) %>%
  mutate(Legend = 'Leisure',
         City = city_name) %>%
  dplyr::select(Legend, City, geometry)
