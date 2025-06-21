library(tidyverse)
library(janitor)
library(tidymodels)


bdata <- read_csv("data/bank/bank.csv")



bank_data_cleaned <- bdata %>%
  mutate(across(where(is.character), ~na_if(., "unknown")))

cleahome_split <- initial_split(bank_data_cleaned, 
                            prop = 0.7, 
                            strata = "y")

home_training <- home_split %>%
  training()


home_test <- home_split %>% 
  testing()

model <- decision_tree() %>% 
  # Set the model engine
  set_engine('rpart') %>% 
  # Set the model mode
  set_mode('classification')

rezept <- recipe(y ~ ., data = home_training) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

wf <- workflow() %>%
  add_model(model) %>%
  add_recipe(rezept)

fit -> fit(wf, data = home_training)

preds <- predict(fit, new_data = home_test, type = "class")

conf_mat(results, truth = truth, estimate = prediction)


