# load required package
library(randomForest)

# load the trained random forest model
rf_model <- readRDS("model/rf_model.rds")

# use the training data from the model itself
train_data <- rf_model$training

# select one example to explain
one_obs <- train_data %>% slice(17)

# define a prediction function for the class yes
predict_prob <- function(new_data) {
  predict(rf_model, new_data, type = "prob")[, "yes"]
}

# calculate prediction for the original observation
original_prob <- predict_prob(one_obs)

# create a counterfactual by toggling 'housing_numeric'
cf_obs <- one_obs
cf_obs$housing_numeric <- ifelse(cf_obs$housing_numeric == 1, 0, 1)  

# calculate prediction for the counterfactual observation
cf_prob <- predict_prob(cf_obs)

# print results
cat("Original prediction (housing_numeric =", one_obs$housing_numeric, 
    "):", round(original_prob, 3), "\n")
cat("Counterfactual prediction (housing_numeric =", cf_obs$housing_numeric, 
    "):", round(cf_prob, 3), "\n")

