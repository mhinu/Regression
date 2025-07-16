#logistic regression - categorical variables when we have boolean values
#logarithum uses sigmoid function alpha = 1/1+e^-Z  the curve is s shaped
#ranges from 0 to 1  (0,1)

library(tidyverse)
library(broom) #for model summary
library(modelr) # for model metrics
library(plotly)
# Example Data
data <- tibble(
  age = c(22, 25, 30, 35, 40, 28, 32, 21),
  bought = c(0, 0, 1, 1, 1, 0, 1, 0)
  )
# Fit Logistic Regression
model <- glm(bought~ age, data=data, family=binomial)

# Summary
tidy(model)

# Predict Probabilities

data_with_preds <- data %>%
  mutate(prob=predict(model, type = "response"))

new_data <- tibble(age= c(24, 29, 38))
new_data %>%
  mutate(probability = predict(model, newdata = ., type = "response"), 
         predicted_class = if_else(probability > 0.5, 1, 0)) -> new_data
print (new_data)
# Plot: Age vs Probability of Buying
map<- ggplot(data_with_preds, aes(x = age, y = prob))+
  geom_point(aes(y = bought), color= "blue", size = 4)+
  geom_line (color = "red")+
  geom_point(data = new_data,
             aes (x = age, y = predicted_class),
             color = "green", size = 4) +
labs(title = "Logistic Regression: Probability of Buying",
     x = "Age", y = "Predicted Probability") +
  theme_minimal()


ggplotly(map)

