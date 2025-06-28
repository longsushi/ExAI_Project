library(randomForest)
library(dplyr)
library(readr)


# Daten laden
bdata <- readRDS("data/processed/bdata.rds")


# Trainings-/Test-Split
set.seed(123)
sample_idx <- sample(nrow(bdata_clean), 0.8 * nrow(bdata_clean))
train <- bdata_clean[sample_idx, ]
test <- bdata_clean[-sample_idx, ]

# Modell trainieren
rf_model <- randomForest(y_bin ~ ., data = train, ntree = 100, mtry = 3, importance = TRUE, classwt = c("no" = 1, "yes" = 50))

# Vorhersage
rf_probs <- predict(rf_model, newdata = test, type = "prob")
rf_pred_custom <- ifelse(rf_probs[, "yes"] > 0.3, "yes", "no")

# Konfusionsmatrix & Genauigkeit
print(table(Predicted = rf_pred_custom, Actual = test$y_bin))
cat("Genauigkeit:", mean(preds == test$y_bin), "\n")
