# load required packages
library(tidyverse)
library(DALEX)
library(iBreakDown)

# get the trained random forest model
rf_model_object <- extract_fit_parsnip(fit)

# prepare the training data with dummy variables
train_baked <- bake(prep(rezept), new_data = home_training)

# to keep the duration of the calculation moderate, we only pick a smaller random sample of 200 rows. 
# for more accurate SHAP values, increase this number, but this will have an impact on the computing time.
set.seed(123)  
bg_idx <- sample(seq_len(nrow(train_baked)), 200)
background <- train_baked[bg_idx, ]
background_y <- as.numeric(home_training$y_bin == "yes")[bg_idx]

# Make an explainer object for the model with the background data
explainer_rf <- explain(
  model = rf_model_object,
  data = background,
  y = background_y,
  label = "SHAP Explanation for One Observation (Random Forest, BG = 200)",
  verbose = FALSE
)

# choose one observation to explain
one_obs <- train_baked %>% slice(5)

# calculate the SHAP values for that observation. B = 5 means we do 5 permutations to estimate the values.
# you can increase B for more precise estimates, but then it will take longer.
shap_rf <- predict_parts(
  explainer = explainer_rf,
  new_observation = one_obs,
  type = "shap",
  N = 50,
  B = 5
)

# make the plot for the SHAP values and print it
shap_plot <- plot(shap_rf)
print(shap_plot)

# save the plot to the graphics folder
ggsave("Grafiken/ExAI/Local/shap_rf_obs1.png", plot = shap_plot, width = 8, height = 6)
