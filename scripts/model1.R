set.seed(421)
theme_set(theme_bw())
"%!in%" <- Negate("%in%")

#create 20 cvID for later 20-fold cross-validation
Model_clean_2 <- Model_clean %>%
  mutate(cvID = sample(round(nrow(Model_clean) / 38.95), size=nrow(Model_clean), replace = TRUE))
Model_clean_2 <- Model_clean_2 %>%
  filter(!is.na(origins_cnt))
### Initial Split for Training and Test ####
data_split <- initial_split(Model_clean_2, strata = "origins_cnt", prop = 0.75)
train.set <- training(data_split)
test.set  <- testing(data_split)

### Cross Validation
## LOGOCV on Neighborhood with group_vfold_cv()
cv_splits_geo <- group_vfold_cv(train.set,  strata = "origins_cnt", group = "cvID")
#print(cv_splits_geo)

### Create Recipes ####
# Feature Creation
model_rec <- recipe(origins_cnt ~ ., data = train.set) %>%
  update_role(cvID, new_role = "cvID") %>%
  step_other(cvID, threshold = 0.005) %>% 
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -origins_cnt) %>%
  step_scale(all_predictors(), -origins_cnt)  


# Model specifications
lm_plan <- 
  linear_reg() %>% 
  set_engine("lm") # kerast

ps_plan <- 
  poisson_reg() %>% 
  set_engine("glm")

rf_plan <- 
  rand_forest() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 1000) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")


#XGB: Extreme Gradient Boosting
XGB_plan <- 
  boost_tree() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 100) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")


rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))


# create workflow
lm_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(lm_plan)
ps_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(ps_plan)
rf_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(rf_plan)
xgb_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(XGB_plan)


# fit model to workflow and calculate metrics
control <- control_resamples(save_pred = TRUE, verbose = TRUE)

lm_tuned <- lm_wf %>%
  tune::fit_resamples(.,
                      resamples = cv_splits_geo,
                      control   = control,
                      metrics   = metric_set(rmse, rsq))

ps_tuned <- ps_wf %>%
  fit_resamples(.,
                resamples = cv_splits_geo,#cv_splits_geo
                control   = control,
                metrics   = metric_set(rmse, rsq))


rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned <- xgb_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))


## metrics across grid
# autoplot(xgb_tuned)
# collect_metrics(xgb_tuned)
show_best(lm_tuned, metric = "rmse", n = 15)
show_best(ps_tuned, metric = "rmse", n = 15)
show_best(rf_tuned, metric = "rmse", n = 15)
show_best(xgb_tuned, metric = "rmse", n = 15)

lm_best_params     <- select_best(lm_tuned, metric = "rmse")
ps_best_params     <- select_best(ps_tuned, metric = "rmse")
rf_best_params     <- select_best(rf_tuned, metric = "rmse")
xgb_best_params    <- select_best(xgb_tuned, metric = "rmse")

## Final workflow
lm_best_wf     <- finalize_workflow(lm_wf, lm_best_params)
ps_best_wf     <- finalize_workflow(ps_wf, ps_best_params)
rf_best_wf     <- finalize_workflow(rf_wf, rf_best_params)
xgb_best_wf    <- finalize_workflow(xgb_wf, xgb_best_params)


# last_fit() emulates the process where, after determining the best model, the final fit on the entire training set is needed and is then evaluated on the test set.
lm_val_fit_geo <- lm_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

ps_val_fit_geo <- ps_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))


rf_val_fit_geo <- rf_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo <- xgb_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))



# Pull best preds from out-of-fold predictions
lm_best_OOF_preds <- collect_predictions(lm_tuned) 

ps_best_OOF_preds <- collect_predictions(ps_tuned) 

rf_best_OOF_preds <- collect_predictions(rf_tuned) %>% 
  filter(mtry  == rf_best_params$mtry[1] & min_n == rf_best_params$min_n[1])

xgb_best_OOF_preds <- collect_predictions(xgb_tuned) %>% 
  filter(mtry  == xgb_best_params$mtry[1] & min_n == xgb_best_params$min_n[1])


# collect validation set predictions from last_fit model
lm_val_pred_geo     <- collect_predictions(lm_val_fit_geo)
ps_val_pred_geo     <- collect_predictions(ps_val_fit_geo)
rf_val_pred_geo     <- collect_predictions(rf_val_fit_geo)
xgb_val_pred_geo    <- collect_predictions(xgb_val_fit_geo)

# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds <- rbind(data.frame(dplyr::select(lm_best_OOF_preds, .pred, origins_cnt), model = "lm"),
                   data.frame(dplyr::select(ps_best_OOF_preds, .pred, origins_cnt), model = "Poisson"),
                   data.frame(dplyr::select(rf_best_OOF_preds, .pred, origins_cnt), model = "RF"),
                   data.frame(dplyr::select(xgb_best_OOF_preds, .pred, origins_cnt), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(#.pred = exp(.pred),
    # origins_cnt = exp(origins_cnt),
    RMSE = yardstick::rmse_vec(origins_cnt, .pred),
    MAE  = yardstick::mae_vec(origins_cnt, .pred),
    MAPE = yardstick::smape_vec(origins_cnt, .pred)) %>% 
  ungroup()



# average error for each model
ggplot(data = OOF_preds %>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "red") +
  # geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  theme_bw()


# average MAPE for each model
ggplot(data = OOF_preds %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = paste0(round(MAPE,2),"%"))) +
  theme_bw()

# OOF predicted versus actual
ggplot(OOF_preds, aes(x =.pred, y = origins_cnt, group = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 1) +
  xlim(0,60000)+
  ylim(0,60000)+
  theme_bw()

# Aggregate predictions from Validation set
val_preds <- rbind(data.frame(lm_val_pred_geo, model = "lm"),
                   data.frame(ps_val_pred_geo, model = "Poisson"),
                   data.frame(rf_val_pred_geo, model = "rf"),
                   data.frame(xgb_val_pred_geo, model = "xgb")) %>% 
  left_join(., Model_clean_2 %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(cvID, .row), 
            by = ".row") %>% 
  group_by(model) %>%
  mutate(# .pred = exp(.pred),
    # origins_cnt = exp(origins_cnt),
    RMSE = yardstick::rmse_vec(origins_cnt, .pred),
    MAE  = yardstick::mae_vec(origins_cnt, .pred),
    MAPE = yardstick::smape_vec(origins_cnt, .pred)) %>% 
  ungroup()

# plot MAE by model type
# plot MAPE by model type
ggplot(data = val_preds %>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "red") +
  #geom_line(aes(y=mean(val_preds$origins_cnt)),linetype="dotted")+
  geom_label(aes(label = paste0(round(MAE,1),"%"))) +
  theme_bw()


# plot MAPE by model type
ggplot(data = val_preds %>% 
         dplyr::select(model, RMSE) %>% 
         distinct() , 
       aes(x = model, y = RMSE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = paste0(round(RMSE,1),"%"))) +
  theme_bw()

# Validation Predicted vs. actual
ggplot(val_preds, aes(x =.pred, y = origins_cnt, group = model)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  xlim(0,60000)+
  ylim(0,60000)+
  facet_wrap(~model, nrow = 1) +
  theme_bw()


c1<-train.set%>% left_join(Model.geo,by="geoid10")
c2<-test.set%>% left_join(Model.geo,by="geoid10")
c<-rbind(c1,c2)
ggplot()+geom_sf(data = c, aes(geometry = geometry, fill=AE_ps))+
  labs(title = "Error Map")+
  mapTheme()
