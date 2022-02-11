library(osmdata)

q0 <- opq(bbox = c(-88.30,42.20,-87.60,41.50)) 

college <- add_osm_feature(opq = q0, key = 'amenity', value = c("university", "college")) %>%
  osmdata_sf(.)

college.sf <- st_geometry(college$osm_polygons) %>%
  st_transform(st_crs(chicagoTracts)) %>%
  st_sf() %>%
  cbind(., college$osm_polygons$name) %>%
  rename(NAME = college.osm_polygons.name) %>%
  na.omit()

  

