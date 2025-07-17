library(tidyverse)
library(broom)    # for model summary
library(modelr)   # for model metrics

# Example Data
data <- tibble(
  age = c(22, 25, 30, 35, 40, 28, 32, 21),
  bought = c(0, 0, 1, 1, 1, 0, 1, 0)
)

# Fit Logistic Regression
model <- glm(bought ~ age, data = data, family = binomial)

# Summary
tidy(model)

# Predict Probabilities
data_with_preds <- data %>%
  mutate(prob = predict(model, type = "response"))


new_data <- tibble(age = c(24, 29, 38))

new_data %>%
  mutate(probability = predict(model, newdata = ., type = "response"),
         predicted_class = if_else(probability > 0.5, 1, 0)) -> new_data_pre


print(new_data)

# Plot: Age vs Probability of Buying
ggplot(data_with_preds, aes(x = age, y = prob)) +
  geom_point(aes(y = bought), color = "blue") +
  geom_line(color = "red") +
  labs(title = "Logistic Regression: Probability of Buying",
       x = "Age", y = "Predicted Probability") +
  theme_minimal()




library(tidyverse)

sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

tibble(z = seq(-10, 10, by = 0.1)) %>%
  mutate(sigmoid_value = sigmoid(z)) %>%
  ggplot(aes(x = z, y = sigmoid_value)) +
  geom_line(color = "red", size = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(title = "Sigmoid (Logistic) Function",
       x = "Input (z)",
       y = "Sigmoid Output (Probability)") +
  theme_minimal()







