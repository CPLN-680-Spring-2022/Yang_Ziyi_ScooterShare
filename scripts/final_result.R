data <- Model_clean_2 %>%
  drop_na()


##===============Find where the model performs well=============####

# First extract work flow 
ps_best_wf     <- finalize_workflow(ps_wf, ps_best_params)
# fit model on the whole dataset
ps_wflow_final_fit <- fit(ps_best_wf, data = data)
# extract recipe
dat_recipe     <- extract_recipe(ps_wflow_final_fit)
# extract fit
ps_final_fit <- extract_fit_parsnip(ps_wflow_final_fit)

# predict bike counts for the whole dataset
data$.pred <- predict(ps_final_fit, 
                      new_data = bake(dat_recipe,data))$.pred

data <- data %>% 
  mutate(absE = abs(.pred - origins_cnt)) 

data$originalCount <- NULL 

yardstick::mae_vec(data$origins_cnt, data$.pred)

ggplot()+geom_histogram(data = data,aes(absE),binwidth = 1,fill="#219ebc")

data <- data%>%
  mutate(incomeContext = ifelse(MdHHInc > 58000, "High_Income", "Low_Income"))
# observed vs preicted by different characteristics
ggplot(data, aes(y=.pred , x = origins_cnt,group = incomeContext))+ 
  geom_point(alpha = 0.3) +
  coord_equal() +
  geom_abline(linetype = "dashed",color = "red") +
  geom_smooth(method="Possion", color = "blue") +
  facet_wrap(~incomeContext,ncol = 2)+
  theme_bw()+
  ylim(0,500)+
  xlim(0,500)+
  labs(x = "Observed",
       y = "Predicted",
       title = "Observed vs. Predicted on the testing set")


# join the geometry
data.geo <- data %>% left_join(CH_spatial_census,by = "") %>% st_as_sf()





#Predict scenario1
ps_new_fit <- fit(ps_best_wf, data = Model_clean_2)

ps_final_newfit <- extract_fit_parsnip(ps_new_fit)
dia_rec3_M    <- extract_recipe(ps_new_fit)
final<-Model_clean_2%>%
  filter(!is.na(origins_cnt))
final$.pred <- predict(ps_final_newfit, 
                                 new_data = bake(dia_rec3_M , Model_clean_2))$.pred

Model.geo <-
  Model.geo %>%
  merge(final, on="geoid10")%>% st_as_sf()

final<-Model.geo%>%
  dplyr::select(-cvID)%>%
  mutate(absE = abs(.pred - origins_cnt))

ggplot() +
  geom_sf(data = final, aes(fill = .pred)) +
  scale_fill_viridis(direction = -1, discrete = FALSE, option="viridis")+
  labs(title="E-Scooter Prediction this year") +
  mapTheme()

ggplot() +
  geom_sf(data = final, aes(fill = absE)) +
  scale_fill_viridis(direction = -1, discrete = FALSE, option="viridis")+
  labs(title="Absolute predicting Error on each road segment") +
  mapTheme()
