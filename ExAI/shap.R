# load required packages
library(tidyverse)
library(DALEX)

# load the trained random forest model
rf_model <- readRDS("model/rf_model.rds")

# load the training data used in blackbox.R
bdata_clean <- readRDS("data/processed/bdata.rds")

# use 2000 background observations to ensure stable SHAP values estimation
set.seed(123)
bg_idx <- sample(seq_len(nrow(bdata_clean)), 2000)
background <- bdata_clean[bg_idx, ]
background_y <- as.numeric(background$y_bin == "yes")

# make an explainer object for the model with the background data
explainer_rf <- explain(
  model = rf_model,
  data = background %>% select(-y_bin),
  y = background_y,
  label = "SHAP Explanation RF",
  verbose = FALSE
)

# choose one observation to explain
one_obs <- bdata_clean %>% select(-y_bin) %>% slice(5)

# calculate the SHAP values for that observation
shap_rf <- predict_parts(
  explainer = explainer_rf,
  new_observation = one_obs,
  type = "shap",
  N = 50,
  B = 30
)

# make the plot for the SHAP values and print it
shap_plot <- plot(shap_rf, max_vars = 10)
print(shap_plot)

# save the plot to the graphics folder
ggsave("Grafiken/ExAI/Local/shap_rf_obs5.png", plot = shap_plot, width = 8, height = 6)

