library(broom)
library(modelr)
library(tidyverse)
library(plotly)

cases <- read.csv("10_Property_stolen_and_recovered.csv") %>% 
  view()
colnames(cases)

model <- lm(Cases_Property_Recovered~Cases_Property_Stolen, data = cases)

model %>% tidy()
model %>% glance()

data_with_preds <- cases %>% 
  add_predictions(model) %>% 
  mutate(residual = Cases_Property_Recovered - pred)

mse <- data_with_preds %>% 
  summarise(mse = mean(residual^2))

new_data <- tibble(Cases_Property_Stolen = c(66,78,55,45,56,117))

predictions <- predict(model, newdata = new_data)

pr_df <- bind_cols(new_data, predicted_recovery = predictions)

#plot
lp<- cases %>% 
  ggplot(aes(Cases_Property_Stolen, Cases_Property_Recovered, color = Area_Name))+
  geom_point( size = 4)+
  geom_smooth(method = "lm", se = F, color = "brown")+
  geom_point(data = pr_df, aes(Cases_Property_Stolen, predicted_recovery), size= 5, color = "blue")+
  labs(
    title = "Recovery prediction",
    x= "Stolen Property",
    y= "Recovered property"
    
  )

colnames(cases)

ggplotly(lp)






