library(randomForest)
library(dplyr)
library(readr)




bdata_clean <- readRDS("data/processed/bdata.rds")


# Feature Engineering – Fokus auf informative numerische Variablen
 

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



# ====== Visualisierungen hinzugefügt (ab hier eingefügt) ======
library(ggplot2)
# Balkendiagramm: Vorhergesagte vs. tatsächliche Klassen
ggplot(data.frame(Actual = test$y_bin, Predicted = rf_pred_custom), aes(x = Actual, fill = Predicted)) +
  geom_bar(position = "dodge") +
  labs(title = "Vorhergesagte vs. Tatsächliche Klassen (Random Forest)",
       x = "Tatsächliche Klasse",
       fill = "Vorhergesagte Klasse") +
  theme_minimal()

# ROC-Kurve (falls Wahrscheinlichkeiten vorhanden)
if ("yes" %in% colnames(rf_probs)) {
  if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
  library(pROC)
  roc_obj <- roc(test$y_bin, rf_probs[, "yes"], levels = c("no", "yes"))
  plot(roc_obj, main = "ROC-Kurve (Random Forest)", col = "red")
  cat("AUC:", round(auc(roc_obj), 4), "\n")
}
# ====== Visualisierungen Ende ======
