library(broom)
library(modelr)
library(tidyverse)
library(plotly)
library(janitor)

sales_data <- read.csv("sales_marketing_data.csv") %>% 
  clean_names()
colnames(sales_data)

model <- lm(sales~ tv+direct+ social_media, data = sales_data)

model %>% tidy()
model %>% glance()

sales_with_preds <- sales_data %>% 
  add_predictions(model) %>% 
  mutate(residual = sales - pred)

mse <- sales_with_preds %>% 
  summarise(mse = mean(residual^2))

new_data <- tibble(tv = c(66,78,55,45,56,117),
                   direct = c(34,45,66,33,32,12),
                   social_media = c(99,98,56,45,35,23))

predictions <- predict(model, newdata = new_data)

pr_df <- bind_cols(new_data, predicted_sales= predictions)

#plot
lp<- sales_data %>% 
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






