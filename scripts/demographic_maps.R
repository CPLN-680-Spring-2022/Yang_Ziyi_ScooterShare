view(load_variables(2019,'acs5',cache = TRUE))
variable19 <- load_variables(2019, "acs5", cache = TRUE)

chicagoTracts <- 
  tigris::tracts(state = "Illinois", county = "Cook") %>%
  dplyr::select(GEOID) %>% filter(GEOID != 17031990000)

neighborhoodList <- 
  c("Grant Park","Printers Row","Loop","Millenium Park","West Loop","United Center",
    "West Town","East Village","Ukranian Village","Wicker Park","River North",
    "Rush & Division","Streeterville","Gold Coast","Old Town","Bucktown","Lincoln Park",
    "Sheffield & DePaul","Lake View","Boystown","Wrigleyville","North Center","Uptown", 
    "Lincoln Square","Little Italy, UIC")

nhoods <- 
  st_read("https://data.cityofchicago.org/api/geospatial/bbvz-uum9?method=export&format=GeoJSON") %>%
  st_transform(st_crs(chicagoTracts)) %>%
  filter(pri_neigh %in% neighborhoodList)

studyArea.tracts <-
  st_intersection(chicagoTracts, st_union(nhoods))


tracts19<- 
  get_acs(geography = "tract",variables = c("B01001_001E","B02001_002E","B19013_001E","B01002_001E"),  
          year=2019, state="Illinois", county="Cook", geometry=T, output="wide") %>%
  st_transform(st_crs(chicagoTracts)) %>%
  rename(TotalPop = B01001_001E, 
         Whites = B02001_002E,
         MedHHInc = B19013_001E,
         MedAge = B01002_001E) %>%
  mutate(percentWhite = Whites / TotalPop,
         raceContext = ifelse(percentWhite > .5, "Majority_White", "Majority_Non_White"),
         Income = ifelse(MedHHInc > 60000, "High_Income", "Low_Income"),
         Age = ifelse(MedAge <35, "Young Community","Old Community")) %>%
  .[studyArea.tracts,]

ggplot() + 
  geom_sf(data = na.omit(tracts19), aes(fill = Income)) +
  scale_fill_manual(values = c("#fbb4b9", "#bdd7e7"), name="Income") +
  labs(title = "Income Context",
       subtitle = "Chicago, IL\n") +
  mapTheme() + 
  theme(legend.position="bottom") +
  plotTheme()

ggplot() + 
  geom_sf(data = na.omit(tracts19), aes(fill = raceContext)) +
  scale_fill_manual(values = c("#fbb4b9", "#bdd7e7"), name="Race") +
  labs(title = "Race Context, Chicago")+
  mapTheme() + 
  theme(legend.position="bottom") +
  plotTheme()

ggplot() + 
  geom_sf(data = na.omit(tracts19), aes(fill = Age)) +
  scale_fill_manual(values = c("#fbb4b9", "#bdd7e7"), name="Median Age") +
  labs(title = "Age Context, Chicago") +
  mapTheme() + 
  theme(legend.position="bottom") +
  plotTheme()
