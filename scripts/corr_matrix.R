numericVars <- 
  select_if(st_drop_geometry(CH_scooter_ct), is.numeric) %>% 
  dplyr::select(-End.Centroid.Latitude,-End.Centroid.Longitude,-Start.Centroid.Longitude,-Start.Centroid.Latitude,
                -centroid_Y,-centroid_X,-KNN_college,-KNN_restaurant,-KNN_public_transport,-KNN_office,
                -KNN_retail,-KNN_tourism,-KNN_leisure,-count_retail,-count_office,-count_leisure,-count_tourism,-count_pubtran,-count_restaurant,-count_college)%>% na.omit()

ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c(low = "steelblue", mid = "white", high = "darkred"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation Matrix of variables") 
