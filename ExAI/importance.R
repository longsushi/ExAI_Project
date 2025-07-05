# load required packages
library(randomForest)
library(vip)
library(ggplot2)

# load the trained random forest model from blackbox.R
rf_model <- readRDS("model/rf_model.rds")

# define custom prediction wrapper for class probabilities
pred_wrapper <- function(object, newdata) {
  predict(object, newdata, type = "prob")[, "yes"]
}
# calculate permutation feature importance using the random forest model
vip_plot <- vip(
  object = rf_model,
  method = "permute",
  train = rf_model$training,
  target = "y_bin",  # Deine Zielvariable
  metric = "roc_auc",  # Alternativ z. B. "accuracy"
  pred_wrapper = pred_wrapper,
  nsim = 10
) +
  ggtitle("Permutation Feature Importance (Random Forest Model)")

# show the plot
print(vip_plot)

# save the plot
ggsave("Grafiken/ExAI/Global/permutation_importance_rf.png", vip_plot, width = 8, height = 5)
