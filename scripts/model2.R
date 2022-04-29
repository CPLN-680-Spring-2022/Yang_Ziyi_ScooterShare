Model_clean <- CH_spatial_census %>%
  dplyr::select(-Mean_Commute_Time)%>%
  filter(!is.na(origins_cnt))%>%
  filter(!is.na(geoid10))
Model.geo <- Model_clean %>% dplyr::select(geoid10,centroid_X, centroid_Y,geometry)
Model_clean <- Model_clean %>%
  dplyr::select(-starts_with('density'), -starts_with('ratio'), -starts_with('count'), -ends_with('length'))
Model_clean <- Model_clean %>% relocate(origins_cnt, .before = TotPop)
Model_clean <- Model_clean %>%dplyr::select(-c(centroid_X, centroid_Y,geometry))
options(scipen=999)

Model_clean <- 
  Model_clean %>%
  as.data.frame() %>%
  dplyr::select(-geometry)


reg1 <- 
  lm(origins_cnt ~ ., data= Model_clean)

summary(reg1)


osm_features <- c("KNN_retail","KNN_office", "KNN_restaurant", "KNN_public_transport",
                  "KNN_leisure","KNN_tourism", "KNN_college",
                  "jobs_in_tract", "workers_in_tract")

census_features <- c("TotPop", "TotHseUni","MdHHInc",              
                     "MdAge", "MedValue",          
                     "MedRent", "pWhite",                
                     "pTrans", "pDrive",                
                     "pFemale", "pCom30plus",            
                     "pOccupied", "pVehAvai")

#poisson-regression
data_split <- initial_split(Model_clean, strata = "origins_cnt", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)



model1 <- glm(origins_cnt ~ ., family="poisson", data = as.data.frame(train.set) %>% dplyr::select(osm_features, origins_cnt))

model2 <- glm(origins_cnt ~ ., family="poisson", data = as.data.frame(train.set) %>% dplyr::select(census_features, origins_cnt))

# Predicting on train set
train.set$pred_OSM <- predict(model1, train.set, type = "response")
train.set$pred_census <- predict(model2, train.set, type = "response")
test.set$pred_OSM <- predict(model1, test.set, type = "response")
test.set$pred_census <- predict(model2, test.set, type = "response")

model3 <- glm(origins_cnt ~ ., family="poisson", data = as.data.frame(train.set) %>% dplyr::select(osm_features, census_features, origins_cnt))
train.set$pred_final <- predict(model3, train.set, type = "response")
train.set$AE_ps <- abs(train.set$pred_final - train.set$origins_cnt)
train.set$Error_ps <- train.set$pred_final - train.set$origins_cnt
mean(train.set$AE_ps)
mean(train.set$origins_cnt)

test.set$pred_final <- predict(model3, test.set, type = "response")
test.set$AE_ps <- abs(test.set$pred_final - test.set$origins_cnt)
test.set$Error_ps <- test.set$pred_final - test.set$origins_cnt
mean(test.set$AE_ps)
mean(test.set$origins_cnt)

# Without 0 ####
data_split <- initial_split(Model_clean, strata = "origins_cnt", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)

train.set <- train.set %>% subset(train.set$origins_cnt!=0)
test.set <- test.set %>% subset(test.set$origins_cnt!=0)

model1 <- glm(origins_cnt ~ ., family="poisson", data = as.data.frame(train.set) %>% dplyr::select(osm_features, origins_cnt))

model2 <- glm(origins_cnt ~ ., family="poisson", data = as.data.frame(train.set) %>% dplyr::select(census_features, origins_cnt))

# Predicting on train set
train.set$pred_OSM <- predict(model1, train.set, type = "response")
train.set$pred_census <- predict(model2, train.set, type = "response")
test.set$pred_OSM <- predict(model1, test.set, type = "response")
test.set$pred_census <- predict(model2, test.set, type = "response")

model3 <- glm(origins_cnt ~ ., family="poisson", data = as.data.frame(train.set) %>% dplyr::select(pred_OSM, pred_census, origins_cnt))
train.set$pred_final <- predict(model3, train.set, type = "response")
train.set$AE_ps <- abs(train.set$pred_final - train.set$origins_cnt)
train.set$Error_ps <- train.set$pred_final - train.set$origins_cnt
mean(train.set$AE_ps)
mean(train.set$origins_cnt)

test.set$pred_final <- predict(model3, test.set, type = "response")
test.set$AE_ps <- abs(test.set$pred_final - test.set$origins_cnt)
test.set$Error_ps <- test.set$pred_final - test.set$origins_cnt
mean(test.set$AE_ps)
mean(test.set$origins_cnt)
