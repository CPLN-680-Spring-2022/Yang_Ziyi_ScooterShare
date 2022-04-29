studyArea.tracts <-CH_ct

Demo_19 <- CH_Census %>%
  dplyr::select(GEOID, TotPop,MdAge, pWhite, pFemale)%>%
  mutate(raceContext = ifelse(pWhite > .5, "Majority_White", "Majority_Non_White"),
         genderContext = ifelse(pFemale > .5, "Majority_Female", "Majority_Male"),
         Age = ifelse(MdAge <35, "Young Community","Old Community"))%>%
  .[studyArea.tracts,]

ggplot() + 
  geom_sf(data = na.omit(Demo_19), aes(fill = genderContext)) +
  scale_fill_manual(values = c("#fbb4b9", "#bdd7e7"), name="Gender") +
  labs(title = "Gender Context, Chicago")+
  mapTheme() + 
  theme(legend.position="bottom") 

ggplot() + 
  geom_sf(data = na.omit(Demo_19), aes(fill = raceContext)) +
  scale_fill_manual(values = c("#fbb4b9", "#bdd7e7"), name="Race") +
  labs(title = "Race Context, Chicago")+
  mapTheme() + 
  theme(legend.position="bottom")

ggplot() + 
  geom_sf(data = na.omit(Demo_19), aes(fill = Age)) +
  scale_fill_manual(values = c("#fbb4b9", "#bdd7e7"), name="Median Age") +
  labs(title = "Age Context, Chicago") +
  mapTheme() + 
  theme(legend.position="bottom") 


Soc_19 <-CH_Census%>%
  dplyr::select(GEOID, TotHseUni, MdHHInc, MedValue, MedRent, Mean_Commute_Time,
                pTrans, pDrive, pCom30plus, pOccupied, pVehAvai)%>%
  mutate(incomeContext = ifelse(MdHHInc > 58000, "High_Income", "Low_Income"),
         TransitContext = ifelse(pDrive > .5, "Driving", "Non-driving"))%>%
  .[studyArea.tracts,]

ggplot() + 
  geom_sf(data = na.omit(Soc_19), aes(fill = TransitContext)) +
  scale_fill_manual(values = c("#fbb4b9", "#bdd7e7"), name="Median Income") +
  labs(title = "Driving, Chicago") +
  mapTheme() + 
  theme(legend.position="bottom") 
