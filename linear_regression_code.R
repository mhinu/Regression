library(tidyverse)
library(broom)    # for model summary
library(modelr)   # for model metrics

# Create data
data <- tibble(
  ads = c(10, 15, 20, 25),
  sales = c(100, 130, 190, 233)
)

# Fit linear model
model <- lm(sales ~ ads, data = data)

# Tidy model summary
model %>% tidy()
model %>% glance()

# Predict and calculate residuals
data_with_preds <- data %>%
  add_predictions(model) %>%
  mutate(residual = sales - pred)

# Calculate MSE
mse <- data_with_preds %>%
  summarise(mse = mean(residual^2))

print(mse)

# Predict new values
new_data <- tibble(ads = c(12, 18))
predictions <- predict(model, newdata = new_data)
pr_df <- bind_cols(new_data, predicted_sales = predictions)

# Plot
data %>%
  ggplot(aes(x = ads, y = sales)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_point(data = pr_df, aes(x = ads, y = predicted_sales), size = 5, color = "green") +
  labs(
    title = "Linear Regression Example (Tidyverse)",
    x = "Ad Budget",
    y = "Sales"
  )


model <- lm(Sales ~ TV + Direct + Social_Media + Influencer, data = sales_data)



  
