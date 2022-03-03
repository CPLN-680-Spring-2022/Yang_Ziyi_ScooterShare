library(sf)
library(measurements)
library(tidycensus)
library(tidyverse)
library(tmap)
library(lubridate)
library(knitr)
library(kableExtra)
library(rgeos)
library(raster)
library(spatstat)
library(data.table)
library(janitor)
library(vroom)
library(here)
library(dplyr)
library(sp)
library(viridis)
library(maptools)
library(stringr)
library(grid)
library(gridExtra)
library(corrplot)
library(osmdata)
library(FNN)
library(janitor)
library(caret)
library(furrr)
library(ggplot2)
library(rsample)
library(recipes)
library(parsnip)
library(workflows)
library(tune)
library(yardstick)
library(ranger)
library(xgboost)
library(ggsn)

plotTheme <- theme(
  plot.title =element_text(size=15),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 10, hjust = 1),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.2),
  axis.ticks=element_blank())

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

#### KNN ####
# nn function ####
nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}


CH_proj <- 3529

CH_scooter_raw <- read.csv("C:/Users/y4ngz/Desktop/E-Scooter_Trips_-_2020.csv")

CH_scooter_clean <- CH_scooter_raw[!is.na(CH_scooter_raw$`Start.Centroid.Location`),]
CH_scooter_clean <- CH_scooter_raw[!is.na(CH_scooter_raw$`Start.Centroid.Longitude`),]
CH_scooter_clean <- CH_scooter_raw[!is.na(CH_scooter_raw$`Start.Centroid.Latitude`),]
CH_scooter_clean <- CH_scooter_clean[!is.na(CH_scooter_clean$`End.Centroid.Location`),]
CH_scooter_clean <- CH_scooter_clean[!is.na(CH_scooter_clean$`End.Centroid.Longitude`),]
CH_scooter_clean <- CH_scooter_clean[!is.na(CH_scooter_clean$`End.Centroid.Latitude`),]
CH_scooter_clean$`Start.Time` <- as.POSIXct(CH_scooter_clean$`Start.Time`, format='%m/%d/%Y %I:%M:%S %p')
CH_scooter_clean$`End.Time` <- as.POSIXct(CH_scooter_clean$`End.Time`, format='%m/%d/%Y %I:%M:%S %p')
#names(CH_scooter_clean)


CH_09 <- CH_scooter_clean %>%
  filter(month(`Start.Time`) == 9)
#write.csv(CH_09, "trip09.csv")

CH_scooter_clean_ori <- CH_09 %>%
  st_as_sf(coords = c("Start.Centroid.Longitude", "Start.Centroid.Latitude"), 
                                 crs = 4326, agr = "constant") %>% 
  st_transform(CH_proj)


CH_ct <- st_read("C:/Users/y4ngz/Desktop/A/yang_ziyi_scootershare/raw_data/Boundaries - Census Tracts - 2010/geo_export_d0bc8109-a6a5-4ccd-8d39-49d610644b47.shp") %>%
  st_transform(CH_proj)


census_df <- data.frame(vars =     c("B01003_001E", 
                                     "B01001_026E",
                                     "B00002_001E",
                                     "B19013_001E", 
                                     "B01002_001E", 
                                     "B02001_002E",
                                     "B08014_001E",
                                     "B08014_002E",
                                     "B08013_001E",
                                     "B08012_001E",
                                     "B08012_008E",
                                     "B08012_009E",
                                     "B08012_010E",
                                     "B08012_011E",
                                     "B08012_012E",
                                     "B08012_013E",
                                     "B08301_001E",
                                     "B08301_002E",
                                     "B08301_010E",
                                     "B25002_001E",
                                     "B25002_002E",
                                     "B25077_001E",
                                     "B25064_001E"),
                        
                        colNames = c("TotPop",
                                     "TotFemale",
                                     "TotHseUni",
                                     "MdHHInc",
                                     "MdAge",
                                     "White_Pop",
                                     "Vehicle_own_pop",
                                     "No_vehicle",
                                     "Total_Travel_Time",
                                     "Travel_Time_3034",
                                     "Travel_Time_3539",
                                     "Travel_Time_4044",
                                     "Travel_Time_4559",
                                     "Travel_Time_6089",
                                     "Travel_Time_90plus",
                                     "Num_Commuters",
                                     "Means_of_Transport_pop",
                                     "Total_cartruckvan",
                                     "Total_Public_Trans",
                                     "Total_occupancy",
                                     "Occupied",
                                     "MedValue",
                                     "MedRent"),
                        stringsAsFactors = FALSE)

census_vars <- census_df$vars
census_colNames <- census_df$colNames

# Function for renaming columns after collecting census data
rename_census_cols <- function(x){
  
  output <- x %>% 
    rename_at(vars(census_vars), 
              ~ census_colNames)
  
  output
}

CH_Census_raw <- get_acs(geography = "tract", 
                         variables = census_vars, 
                         year = 2018, 
                         state = "IL", 
                         geometry = TRUE, 
                         county = c("Cook"),
                         output = "wide") %>%
  rename_census_cols %>%
  dplyr::select(GEOID, 
                geometry,
                census_colNames) %>% 
  st_transform(CH_proj)

CH_tract_list <- CH_ct$geoid10

CH_Census_geoinfo <- CH_Census_raw %>%
  dplyr::select(GEOID, geometry)

# extract centroid of each census tract
CH_ct <- CH_ct %>% 
  mutate(centroid_X = st_coordinates(st_centroid(CH_ct))[, 1],
         centroid_Y = st_coordinates(st_centroid(CH_ct))[, 2])


CH_Census <- CH_Census_raw %>% 
  mutate(pWhite = White_Pop / TotPop,
         Mean_Commute_Time = Total_Travel_Time / Num_Commuters,
         pTrans = Total_Public_Trans / Means_of_Transport_pop,
         pDrive = Total_cartruckvan/Means_of_Transport_pop,
         pFemale = TotFemale/TotPop,
         pCom30plus = (Travel_Time_3034 + Travel_Time_3539 + Travel_Time_4044 + Travel_Time_4559 +
                         Travel_Time_6089 + Travel_Time_90plus) / Total_Travel_Time,
         pOccupied = Occupied/Total_occupancy,
         pVehAvai = 1 - No_vehicle / Vehicle_own_pop)

CH_Census <- CH_Census %>%
  dplyr::select(GEOID, TotPop, TotHseUni, MdHHInc, MdAge, MedValue, MedRent, pWhite, Mean_Commute_Time,
                pTrans, pDrive, pFemale, pCom30plus, pOccupied, pVehAvai)

CH_Census_ct <- CH_Census %>%
  filter(CH_Census$GEOID %in% CH_tract_list) %>%
  st_set_geometry(NULL)

# rejoin geometry infor from ct_LV
CH_Census_ct <- merge(CH_ct, CH_Census_ct, by.x = 'geoid10', by.y = 'GEOID')

CH_Census_ct <- CH_Census_ct %>%
  dplyr::select(-c(commarea, commarea_n, countyfp10, name10, namelsad10, notes, statefp10))

CH_scooter_sf <- CH_scooter_clean_ori %>% st_as_sf(coords = c('Start.Centroid.Longitude','Start.Centroid.Latitude'),crs=4326) %>%
  st_transform(CH_proj) %>%
  mutate(Start.Centroid.Longitude = unlist(map(geometry, 1)),
         Start.Centroid.Latitude = unlist(map(geometry, 2)))
CH_scooter_ct <- st_join(CH_scooter_sf %>% st_transform(3529), CH_Census_geoinfo %>% dplyr::select(GEOID), st_within, left=T)
  # %>% rename(Start.Census.Tract=GEOID)

CH_scooter_ct <- 
  left_join(CH_scooter_ct,spatial_panel %>%
              as.data.frame() %>%
              dplyr::select(-geometry), by = c("GEOID" = "geoid10"))
