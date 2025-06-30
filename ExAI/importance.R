# load required packages
library(tidymodels)
library(vip)

# extract the trained decision tree model
model_object <- extract_fit_parsnip(fit)

# plot the top 10 most important features. We only display the top 10 for better readability
vip_plot <- vip(model_object$fit, num_features = 10) +
  ggtitle("Feature Importance (Decision Tree Model)")

# show the plot
print(vip_plot)

# save the plot to the graphics folder
ggsave("Grafiken/ExAI/Global/feature_importance_dt.png", vip_plot, width = 8, height = 5)
