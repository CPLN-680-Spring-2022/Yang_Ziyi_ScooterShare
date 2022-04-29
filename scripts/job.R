CH_WAC <- read_csv("C:/Users/y4ngz/Desktop/A/yang_ziyi_scootershare/raw_data/il_wac_S000_JT00_2019.csv.gz") %>% 
  dplyr::select(geocode = w_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(jobs_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% CH_tract_list) 

# Read in RAC Data
CH_RAC <- read_csv("C:/Users/y4ngz/Desktop/A/yang_ziyi_scootershare/raw_data/il_rac_S000_JT00_2019.csv.gz") %>% 
  dplyr::select(geocode = h_geocode, C000) %>% 
  mutate(geocode = as.character(substr(geocode, 1, 11))) %>% 
  group_by(geocode) %>% 
  summarize(workers_in_tract = sum(C000, na.rm = TRUE)) %>% 
  filter(geocode %in% CH_tract_list) 

# Join them
CH_LODES <- left_join(CH_WAC, CH_RAC, by = c("geocode"))

CH_spatial_census <- merge(CH_spatial_census, CH_LODES, by.x = 'geoid10', by.y = 'geocode')

soc_correlation.long <-
  st_set_geometry(CH_spatial_census, NULL) %>%
  dplyr::select(origins_cnt,jobs_in_tract,workers_in_tract,TotHseUni,MdHHInc,MedValue,MedRent,Mean_Commute_Time,pTrans,pDrive,pCom30plus,pOccupied,pVehAvai) %>%
  gather(Variable, Value, -origins_cnt )

soc_correlation.cor <-
  soc_correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, origins_cnt, use = "complete.obs"))

ggplot(soc_correlation.long, aes(Value, origins_cnt)) +
  geom_point(size = 0.1) +
  geom_text(data = soc_correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "gold") +
  facet_wrap(~Variable, ncol = 4, scales = "free") +
  labs(title = "Socio-economic factors")

