library(randomForest)


# A Random Forest is an ensemble machine learning model that combines 
# multiple decision trees. Each tree in the forest is trained on a 
# random sample of the data (bootstrap sampling) and considers only a 
# random subset of features when making splits (feature randomization).

# Bootstrap Sampling: Each tree gets its own unique training set, 
# created by randomly sampling from the original data with replacement.
# This means some data points may appear multiple times while others aren’t used.

# Random Feature Selection: When making a split, each tree only considers a random
# subset of features (typically square root of total features).

# Growing Trees: Each tree grows using only its bootstrap sample and selected features,
# making splits until it reaches a stopping point (like pure groups or minimum sample size).

# Final Prediction: All trees vote together for the final prediction. For classification,
# take the majority vote of class predictions; for regression, average the predicted values from all trees.

data <- tibble(
  age = sample(20:50, 30, replace = TRUE),
  income = sample(seq(20000, 100000, by = 5000), 30, replace = TRUE),
  city = factor(sample(c("Delhi", "Mumbai", "Chennai", "Bangalore"), 30, replace = TRUE)),
  gender = factor(sample(c("Male", "Female"), 30, replace = TRUE)),
  bought = factor(sample(c(0, 1), 30, replace = TRUE))
)


rf_model <- randomForest(
  bought ~ age + income + city + gender,
  data = data,
  ntree = 100,       # number of trees
  mtry = 2,          # number of features to consider per split
  importance = TRUE  # show variable importance
)

print(rf_model)

# Variable importance
importance(rf_model)
varImpPlot(rf_model)


new_data <- data.frame(
  age = c(27, 36),
  income = c(45000, 72000),
  city = factor(c("Delhi", "Chennai"), levels = levels(data$city)),
  gender = factor(c("Female", "Male"), levels = levels(data$gender))
)

predict(rf_model, newdata = new_data)





