library(broom)
library(modelr)
library(tidyverse)

#create data
data <- tibble (
  ads = c(10,15,20,25),
  sales = c(100,130,190,233)
)

#Fit Linear model

model <- lm(sales~ads, data = data)
#Tidy model summary
model %>% tidy()

model %>% glance()

# Predict and calculate the residuals
data_with_preds <- data %>% 
  add_predictions(model) %>% 
  mutate(residual = sales - pred)

#calculate MSE
mse <- data_with_preds %>% 
  summarise(mse = mean(residual^2))

print(mse)

#predict new values
new_data <- tibble(ads = c(12,18))

predictions <- predict(model, newdata = new_data)

pr_df <- bind_cols(new_data, predicted_sales = predictions)

#plot
data %>% 
  ggplot(aes(ads, sales))+
  geom_point(color = "green", size = 4)+
  geom_smooth(method = "lm", se = F, color = "brown")+
  geom_point(data = pr_df, aes(ads, predicted_sales), size= 5, color = "blue")+
  labs(
    title = "Linear Regreesion Example(Tidyverse)",
    x= "Ad Budget",
    y= "Sales"
    
  )


#Use "10_prfnsijdbfnoskm.csv"
#create a model for Recover and property stolen for both value and cases
#Assignment ; Do the same for different case groups
