# load required package
library(tidymodels)

# extract the trained decision tree model
model_object <- extract_fit_parsnip(fit)

# build an interpretation recipe WITHOUT normalization so features stay in original units
interp_recipe <- recipe(y_bin ~ ., data = home_training) %>%
  step_impute_mode(all_nominal_predictors()) %>%  # handle missing categories
  step_unknown(all_nominal_predictors()) %>%     # encode unknowns
  step_dummy(all_nominal_predictors()) %>%       # turn factors into dummies
  step_zv(all_predictors()) %>%                  # remove zero-variance predictors
  prep()

# bake the interpretation data once
train_interp <- bake(interp_recipe, new_data = home_training)

# select one example to explain
one_obs <- train_interp %>% slice(17)

# define a prediction function for the class yes
predict_prob <- function(new_data) {
  predict(model_object, new_data, type = "prob")$.pred_yes
}

# calculate prediction for the original observation
original_prob <- predict_prob(one_obs)

# create a counterfactual by increasing 'month_success_rate' by 1
cf_obs <- one_obs
cf_obs$month_success_rate <- cf_obs$month_success_rate + 1  # using real units from train_interp

# calculate prediction for the counterfactual observation
cf_prob <- predict_prob(cf_obs)

# print results
cat("Original prediction (P(yes)):", round(original_prob, 3), "\n")
cat("Counterfactual prediction (month_success_rate + 1):", round(cf_prob, 3), "\n")

