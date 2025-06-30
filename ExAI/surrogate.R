# load required packages
library(tidymodels)
library(rpart.plot)  # for visualizing the decision tree

# extract the trained random forest model
rf_model_object <- extract_fit_parsnip(rf_fit)

# preprocess the training data
train_baked <- bake(prep(recipe_rf), new_data = train)

# get predicted probabilities for the "yes" class
pred_probs <- predict(rf_model_object, train_baked, type = "prob")$.pred_yes

# combine the data and predictions
surrogate_data <- train_baked %>%
  mutate(pred_prob = pred_probs)

# define a simple decision tree to mimic the random forest
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

# fit the surrogate model using the predicted probabilities
surrogate_fit <- tree_spec %>%
  fit(pred_prob ~ ., data = surrogate_data)

# visualize the surrogate decision tree
rpart.plot(surrogate_fit$fit,
           main = "Surrogate Tree: Key Drivers of RF Predictions",
           type = 2, extra = 101, under = TRUE)

# save the plot to the graphics folder
png("Grafiken/ExAI/Local/surrogate_tree_rf.png", width = 800, height = 600)
rpart.plot(surrogate_fit$fit,
           main = "Surrogate Tree: Key Drivers of RF Predictions")
dev.off()
