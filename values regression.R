cases <- read.csv("10_Property_stolen_and_recovered.csv")
colnames(cases)

model <- lm(Value_of_Property_Recovered~Value_of_Property_Stolen, data = cases)

model %>% tidy()
model %>% glance()

data_with_preds <- cases %>% 
  add_predictions(model) %>% 
  mutate(residual = Value_of_Property_Recovered- pred)

mse <- data_with_preds %>% 
  summarise(mse = mean(residual^2))

new_data <- tibble(Value_of_Property_Stolen = c(4352566,33452578,569055,735745,3263756,109117))

predictions <- predict(model, newdata = new_data)

pr_df <- bind_cols(new_data, predicted_recovery = predictions)

#plot
lp<- cases %>% 
  ggplot(aes(Value_of_Property_Stolen/10000, Value_of_Property_Recovered/1000000, color = Area_Name))+
  geom_point( size = 4)+
  geom_smooth(method = "lm", se = F, color = "brown")+
  geom_point(data = pr_df, aes(Value_of_Property_Stolen/10000, predicted_recovery/1000000), size= 5, color = "blue")+
  labs(
    title = "Recovery prediction",
    x= "Stolen Property",
    y= "Recovered property"
    
  )

ggplotly(lp)

