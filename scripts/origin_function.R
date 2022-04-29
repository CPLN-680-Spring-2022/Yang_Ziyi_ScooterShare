# Count dests for each census tract
make_CH_open_sf <- function(x,
                            trip_start, # define whether you want the origins or the destinations
                            proj) { 
  
  if(!grepl("ori|des", trip_start)) {
    
    stop("trip_start must be either 'origins' or 'dests'")
    
  } else if (grepl("ori", trip_start)) {
    
    output <- x %>%
      dplyr::select(Trip.ID,
                    Start.Community.Area.Number,
                    Start.Centroid.Latitude,   
                    Start.Centroid.Longitude,
                    Start.Time) %>% 
      st_as_sf(coords = c("Start.Centroid.Longitude", "Start.Centroid.Latitude"), 
               crs = 4326) %>% 
      st_transform(proj)
    
  } else {
    
    output <- x %>%
      dplyr::select(Trip.ID,
                    End.Community.Area.Number,
                    End.Centroid.Latitude,   
                    End.Centroid.Longitude, 
                    End.Time) %>% 
      st_as_sf(coords = c("End.Centroid.Longitude", "End.Centroid.Latitude"), 
               crs = 4326) %>% 
      st_transform(proj)
    
  }
  output
}
CH_proj=3529

CH_open_all_sf <- CH_open_origins

CH_open_origins <- make_CH_open_sf(CH_scooter_clean,
                                   trip_start = "origins",
                                   proj = CH_proj) %>%
  .[CH_ct,]

CH_open_dests <- make_CH_open_sf(CH_scooter_clean,
                                 trip_start = "dests",
                                 proj = CH_proj) %>%
  .[CH_ct,]



CH_open_origins_ct <- CH_Census_ct %>% 
  mutate(origins_cnt = (lengths(st_intersects(., CH_open_all_sf))))

CH_open_dests_ct <- CH_Census_ct %>%
  mutate(dests_cnt = lengths(st_intersects(., CH_open_all_sf)))

# Combine
CH_open_ct <- CH_open_origins_ct %>%
  left_join(CH_open_dests_ct %>%
              st_drop_geometry() %>%
              dplyr::select(geoid10, dests_cnt),
            by = "geoid10")

CH_open_ct <- CH_open_origins_ct
CH_open_ct <-na.omit(CH_open_ct) 
#
CH_ORIGINS <- CH_scooter_ct %>%
  group_by(Start.Community.Area.Number) %>% 
  summarise(Outflow = n()) %>%
  na.omit()

CH_DESTS <- CH_scooter_ct %>%
  group_by(End.Community.Area.Number) %>% 
  summarise(Inflow = n()) %>%
  na.omit()


spatial_correlation.long <-
  st_set_geometry(CH_spatial_census, NULL) %>%
  dplyr::select(origins_cnt, KNN_college, KNN_restaurant, KNN_public_transport, 
                KNN_retail, KNN_office, KNN_tourism, KNN_leisure, count_retail, 
                density_retail, count_office, density_office, count_leisure,
                density_leisure, count_tourism, total_length, density_tourism,count_pubtran,
                density_pubtran, count_restaurant, density_restaurant,
                count_college, density_college,ratio_college, ratio_restaurant, ratio_pubtran, 
                ratio_retail, ratio_office, ratio_tourism, ratio_leisure, ) %>%
  gather(Variable, Value, -origins_cnt )

spatial_correlation.cor <-
  spatial_correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, origins_cnt, use = "complete.obs"))

ggplot(spatial_correlation.long, aes(Value, origins_cnt)) +
  geom_point(size = 0.1) +
  geom_text(data = spatial_correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "gold") +
  facet_wrap(~Variable, ncol = 5, scales = "free") +
  labs(title = "Spatial factors")






demo_correlation.long <-
  st_set_geometry(CH_spatial_census, NULL) %>%
  dplyr::select(origins_cnt,TotPop,MdAge,pFemale,pWhite) %>%
  gather(Variable, Value, -origins_cnt )

demo_correlation.cor <-
  demo_correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, origins_cnt, use = "complete.obs"))

ggplot(demo_correlation.long, aes(Value, origins_cnt)) +
  geom_point(size = 0.1) +
  geom_text(data = demo_correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "gold") +
  facet_wrap(~Variable, ncol = 5, scales = "free") +
  labs(title = "Demographic factors")


#ggplot(data=spatial_correlation.long) + 
#  geom_boxplot(aes(x=Variable, y=Value)) 
