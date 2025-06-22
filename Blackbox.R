library(randomForest)
library(dplyr)

# Daten laden
bdata <- read_delim("data/bank/bank-full.csv", delim = ";")

# Dummy-Feature: Zielvariable binär
bdata$y_bin <- ifelse(bdata$y == "yes", 1, 0)

# Beispiel: Nur numerische Features nehmen (vereinfachte Version!)
bdata_clean <- bdata %>%
  mutate(job_unemployed = ifelse(job == "unemployed", 1, 0)) %>%
  select(age, balance, duration, campaign, job_unemployed, y_bin)

set.seed(123)
sample_idx <- sample(nrow(bdata_clean), 0.8 * nrow(bdata_clean))
train <- bdata_clean[sample_idx, ]
test <- bdata_clean[-sample_idx, ]

rf_model <- randomForest(y_bin ~ ., data = train, ntree = 100, mtry = 2, importance = TRUE)

preds <- predict(rf_model, newdata = test, type = "response")

# Konfusionsmatrix
table(Predicted = preds, Actual = test$y_bin)

# Genauigkeit berechnen
mean(preds == test$y_bin)