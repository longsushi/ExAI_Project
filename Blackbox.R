library(randomForest)
library(dplyr)
library(readr)

# Daten laden
bdata <- readRDS("data/processed/bdata.rds")

# Zielvariable binär
bdata$y_bin <- factor(ifelse(bdata$y == "yes", "yes", "no"))

# Feature Engineering – Fokus auf informative numerische Variablen
bdata_clean <- bdata %>%
  mutate(
    job_unemployed = ifelse(job == "unemployed", 1, 0),
    has_contact_before = ifelse(pdays == -1, 0, 1),
    duration_log = log(duration + 1),
    balance_log = log(abs(balance) + 1),
    campaign_intensity = campaign * duration,
    contact_summer = ifelse(month %in% c("jun", "jul", "aug"), 1, 0)
  ) %>%
  select(
    age, balance_log, duration_log, campaign, campaign_intensity,
    job_unemployed, has_contact_before, contact_summer,
    y_bin
  )

# Trainings-/Test-Split
set.seed(123)
sample_idx <- sample(nrow(bdata_clean), 0.8 * nrow(bdata_clean))
train <- bdata_clean[sample_idx, ]
test <- bdata_clean[-sample_idx, ]

# Modell trainieren
rf_model <- randomForest(y_bin ~ ., data = train, ntree = 100, mtry = 3, importance = TRUE)

# Vorhersage
preds <- predict(rf_model, newdata = test)

# Konfusionsmatrix & Genauigkeit
print(table(Predicted = preds, Actual = test$y_bin))
cat("Genauigkeit:", mean(preds == test$y_bin), "\n")
