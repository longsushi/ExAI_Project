# load required packages
library(tidymodels)
library(rpart.plot)

# load the trained random forest model
rf_model <- readRDS("model/rf_model.rds")

# use the training data from the RF model itself
train_data <- rf_model$training

# get predicted probabilities from the black-box RF model
pred_probs <- predict(rf_model, newdata = train_data, type = "prob")[, "yes"]

# remove the true target variable y_bin and combine with predictions
surrogate_data <- train_data %>%
  select(-y_bin) %>%
  mutate(pred_prob = pred_probs)

# define a simple interpretable model (decision tree)
tree_spec <- decision_tree(
  cost_complexity = 0.01,
  tree_depth = 3
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# train the surrogate model to approximate the RF predictions
surrogate_fit <- tree_spec %>%
  fit(pred_prob ~ ., data = surrogate_data)

# visualize the surrogate tree
rpart.plot(surrogate_fit$fit,
           main = "Surrogate Tree: Key Drivers of Random Forest Predictions",
           type = 4, extra = 101, digits = 2, roundint = FALSE, under = TRUE)

# save the plot to the graphics folder
png("Grafiken/ExAI/Global/surrogate_tree_rf.png", width = 800, height = 600)
rpart.plot(surrogate_fit$fit,
           main = "Surrogate Tree: Key Drivers of Random Forest Predictions",
           type = 4, extra = 101, digits = 2, roundint = FALSE, under = TRUE)
dev.off()