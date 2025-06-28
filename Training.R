library(tidyverse)
library(janitor)
library(tidymodels)
library(yardstick)  # Für zusätzliche Metriken

# Daten laden
bdata <- readRDS("data/processed/bdata.rds")

# Vorverarbeitung: "unknown" zu NA
bank_data_cleaned <- bdata %>%
  mutate(across(where(is.character), ~na_if(., "unknown")))

# Trainings- und Testdaten aufteilen
home_split <- initial_split(bank_data_cleaned, 
                            prop = 0.8, 
                            strata = "y_bin")

home_training <- training(home_split)
home_test <- testing(home_split)

# Modell: Entscheidungsbaum
model <- decision_tree() %>% 
  set_engine('rpart', parms = list(prior = c(yes = 0.65, no = 0.35))) %>% 
  set_mode('classification')

# Rezept: Dummy-Variablen, Skalierung, Variablen mit 0 Varianz entfernen
rezept <- recipe(y_bin ~ ., data = home_training) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())


# Workflow
wf <- workflow() %>%
  add_model(model) %>%
  add_recipe(rezept)

# Modell fitten
fit <- fit(wf, data = home_training)

# Vorhersagen auf Testdaten
preds_class <- predict(fit, new_data = home_test, type = "class")
preds_prob <- predict(fit, new_data = home_test, type = "prob")

# Ergebnisse kombinieren
results <- home_test %>%
  select(y_bin) %>%
  bind_cols(preds_class, preds_prob)

# Sicherstellen, dass die Zielvariable ein Faktor ist
results <- results %>%
  mutate(
    y = as.factor(y_bin),
    .pred_class = as.factor(.pred_class)
  )

# 📌 Confusion Matrix
print(conf_mat(results, truth = y, estimate = .pred_class))

accuracy_value <- mean(results$.pred_class == results$y)
cat("Genauigkeit (Accuracy):", round(accuracy_value, 4), "\n")





