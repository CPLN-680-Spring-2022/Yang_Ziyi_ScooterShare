library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(tidycensus)
library(viridis)
library(riem)
library(gridExtra)
library(knitr)
library(kableExtra)
library(RSocrata)
library(FNN)
library(gganimate)
library(caret)

options(tigris_class = "sf")
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette7 <- c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c","#527D82","#123F5A")
palette5 <- c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c")
palette4 <- c("#D2FBD4","#92BCAB","#527D82","#123F5A")
palette2 <- c("#6baed6","#08519c")

Chiride <- read.csv("C:/Users/y4ngz/Desktop/E-Scooter_Trips_-_2020.csv")

ChiNei <- read.csv("C:/Users/y4ngz/Desktop/A/yang_ziyi_scootershare/raw_data/E-Scooter_Trips_-_Census_Tract_Summary_-_2020.csv")

Chiride2 <-
  Chiride %>% 
  mutate(interval60 = floor_date(mdy_hms(Start.Time), unit = "hour"),
         interval15 = floor_date(mdy_hms(End.Time), unit = "15 mins"),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE))

census_api_key("5e18665e0e44badf7aeb0d87f1a9453abcc8f5bb", overwrite = TRUE)

ChiCensus <- 
  get_acs(geography = "tract", 
          variables = c("B01003_001", "B19013_001", 
                        "B02001_002", "B08013_001",
                        "B08012_001", "B08301_001", 
                        "B08301_010", "B01002_001"), 
          year = 2019, 
          state = "Illinois", 
          geometry = TRUE,
          county= "Cook",
          output = "wide") %>%
  rename(Total_Pop =  B01003_001E,
         Med_Inc = B19013_001E,
         Med_Age = B01002_001E,
         White_Pop = B02001_002E,
         Travel_Time = B08013_001E,
         Num_Commuters = B08012_001E,
         Means_of_Transport = B08301_001E,
         Total_Public_Trans = B08301_010E) %>%
  select(Total_Pop, Med_Inc, White_Pop, Travel_Time,
         Means_of_Transport, Total_Public_Trans,
         Med_Age,
         GEOID, geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Mean_Commute_Time = Travel_Time / Total_Public_Trans,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport)

ChiTracts <- 
  ChiCensus %>%
  as.data.frame() %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  select(GEOID, geometry) %>% 
  st_sf

dat_census <- st_join(Chiride2 %>% 
                        filter(is.na(Start.Centroid.Longitude) == FALSE &
                                 is.na(Start.Centroid.Latitude) == FALSE &
                                 is.na(End.Centroid.Latitude) == FALSE &
                                 is.na(End.Centroid.Longitude) == FALSE) %>%
                        st_as_sf(., coords = c("Start.Centroid.Longitude", "Start.Centroid.Latitude"), crs = 4326),
                      ChiTracts %>%
                        st_transform(crs=4326),
                      join=st_intersects,
                      left = TRUE) %>%
  rename(Origin.Tract = GEOID) %>%
  mutate(Start.Centroid.Longitude = unlist(map(geometry, 1)),
         Start.Centroid.Latitude = unlist(map(geometry, 2)))%>%
  as.data.frame() %>%
  select(-geometry)%>%
  st_as_sf(., coords = c("End.Centroid.Longitude", "End.Centroid.Latitude"), crs = 4326) %>%
  st_join(., ChiTracts %>%
            st_transform(crs=4326),
          join=st_intersects,
          left = TRUE) %>%
  rename(Destination.Tract = GEOID)  %>%
  mutate(to_longitude = unlist(map(geometry, 1)),
         to_latitude = unlist(map(geometry, 2)))%>%
  as.data.frame() %>%
  select(-geometry)

weather.Data <- 
  riem_measures(station = "ORD", date_start = "2020-09-01", date_end = "2020-10-01")

weather.Panel <-  
  weather.Data %>%
  mutate_if(is.character, list(~replace(as.character(.), is.na(.), "0"))) %>% 
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid, 1, 13))) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Precipitation = sum(p01i),
            Wind_Speed = max(sknt)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))

grid.arrange(top = "Weather Data - Chicago",
                 ggplot(weather.Panel, aes(interval60,Precipitation)) + geom_line() + 
                   labs(title="Precipitation", x="Hour", y="Precipitation") + plotTheme(),
                 ggplot(weather.Panel, aes(interval60,Wind_Speed)) + geom_line() + 
                   labs(title="Wind Speed", x="Hour", y="Wind Speed") + plotTheme(),
                 ggplot(weather.Panel, aes(interval60,Temperature)) + geom_line() + 
                   labs(title="Temperature", x="Hour", y="Temperature") + plotTheme())




counts <- table(ChiNei$Trip.Count)
barplot(counts, main="Trip Distribution",
        xlab="Number of Trip")

company <- table(Chiride$Vendor)
barplot(company, main ="Differences in the number of people travelling",
        xlab="Company Name", col="#6baed6")
