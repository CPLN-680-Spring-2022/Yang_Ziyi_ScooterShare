ggplot() +
  #geom_sf(data = boundary, fill = "grey40") +
  geom_sf(data = CH_DESTS, aes(colour = q5(Inflow))) +
  #scale_colour_manual(values = palette5,
  #                    label= levels(data_cuts),)
  #                  name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Philadelphia; 2000") +
  mapTheme()

data_cuts <- cut(merge1$Inflow, 5)
options(scipen=999)

TEST <-
  CH_Census_raw %>%
  dplyr::select(GEOID, geometry)


dest_sf <-
  TEST %>%
  st_intersection(CH_DESTS) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(sumInflow = sum(Inflow)) 


ggplot() +
  geom_sf(data = merge2, aes(fill = Outflow))+
  scale_fill_gradient(low = "#aed6f5",
                      high = "#132B43")+
  labs(title = "Outflow")+
  mapTheme()

Community <- st_read("C:/Users/y4ngz/Desktop/A/yang_ziyi_scootershare/raw_data/Boundaries - Community Areas (current).geojson")

Community <- Community %>%
  dplyr::select(area_numbe,geometry)

ggplot() +
  geom_sf(data = Community_merge, aes(fill = Inflow))


Community_merge <-
  Community %>%
  merge(CH_DESTS, by.y='End.Community.Area.Number', by.x='area_numbe')
  
CH_ORIGINS <- CH_ORIGINS%>%
  st_drop_geometry()

merge1 <- merge(y=CH_DESTS, x=Community, by.y='End.Community.Area.Number', by.x='area_numbe')
merge2 <- merge(y=CH_ORIGINS, x=Community, by.y='Start.Community.Area.Number', by.x='area_numbe')


ggplot(data=CH_spatial_census) +
  geom_area(stat='bin', bins = 30, aes(x=origins_cnt)) +
  xlim(c(0,10000))
