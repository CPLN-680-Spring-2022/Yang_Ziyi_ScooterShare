Model_clean <- CH_spatial_census %>%
  dplyr::select(-c(Mean_Commute_Time,geoid10, centroid_X, centroid_Y))
Model_clean <- Model_clean %>%
  dplyr::select(-starts_with('density'), -starts_with('ratio'), -starts_with('count'), -ends_with('length'))
Model_clean <- Model_clean %>% relocate(origins_cnt, .before = TotPop)


Model_clean <- 
  Model_clean %>%
              as.data.frame() %>%
              dplyr::select(-geometry)


reg1 <- 
  lm(origins_cnt ~ ., data= Model_clean)

summary(reg1)

set.seed(717)
theme_set(theme_bw())
"%!in%" <- Negate("%in%")

#create 20 cvID for later 20-fold cross-validation
Model_clean_2 <- Model_clean %>%
  mutate(cvID = sample(round(nrow(Model_clean) / 78), size=nrow(Model_clean), replace = TRUE))

### Initial Split for Training and Test ####
data_split <- initial_split(Model_clean_2, strata = "origins_cnt", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)

### Cross Validation
## LOGOCV on Neighborhood with group_vfold_cv()
cv_splits_geo <- group_vfold_cv(train.set,  strata = "origins_cnt", group = "cvID")
print(cv_splits_geo)

### Create Recipes ####
# Feature Creation
model_rec <- recipe(origins_cnt ~ ., data = train.set) %>%
  update_role(cvID, new_role = "cvID") %>%
  step_other(cvID, threshold = 0.005) %>% #pool infrequently occurrin values into an "other" category.
  step_dummy(all_nominal()) %>%
  #  step_log(ORIGINS_CNT) %>%  #has zero, cannot log 
  step_zv(all_predictors()) %>% #remove variables that contain only a single value.
  step_center(all_predictors(), -origins_cnt) %>% #normalize numeric data to have a mean of zero.
  step_scale(all_predictors(), -origins_cnt)  #normalize numeric data to have a standard deviation of one.
#  %>% step_ns(Latitude, Longitude, options = list(df = 4)) #create new columns that are basis expan- sions of variables using natural splines.


# Model specifications
lm_plan <- 
  linear_reg() %>% 
  set_engine("lm") # kerast

glmnet_plan <- 
  linear_reg() %>% 
  set_args(penalty  = tune()) %>%
  set_args(mixture  = tune()) %>%
  set_engine("glmnet")
