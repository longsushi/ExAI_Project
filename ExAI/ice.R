# load required packages
library(tidymodels)
library(pdp)
library(ggplot2)

# preprocess the training data
train_baked <- bake(prep(rezept), new_data = home_training)

# compute ICE curves for the feature 'month_success_rate'
set.seed(42)
ice_month <- partial(
  object      = extract_fit_parsnip(fit)$fit,      # trained decision tree model
  pred.var    = "month_success_rate",              # feature of interest
  ice         = TRUE,                              # compute ICE curves
  center      = FALSE,                             # no centering
  train       = train_baked,                       # processed training data
  prob        = TRUE,                              # output probabilities
  which.class = "yes",                             # focus on P(yes) class
  plot        = FALSE                              # do not plot automatically
)

# if .id is missing, create it so grouping works
if (!".id" %in% names(ice_month)) {
  grid_vals        <- unique(ice_month$month_success_rate)
  n_grid           <- length(grid_vals)
  ice_month$.id    <- rep(seq_len(nrow(train_baked)), each = n_grid)
}

# sample 100 ICE curves to keep the plot readable
set.seed(42)
sample_ids <- sample(unique(ice_month$.id), 100)
ice_sample <- subset(ice_month, .id %in% sample_ids)

# build the ggplot object
p <- ggplot(ice_sample, aes(x = month_success_rate, y = yhat, group = .id)) +
  geom_line(alpha = 0.2) +
  labs(
    title = "ICE: Effect of Month Success Rate on Predicted P('yes')",
    x     = "Success Rate in Current Month",
    y     = "Predicted Probability"
  )

# display the ICE plot
print(p)

# save the ICE plot to the graphics folder
ggsave(
  filename = "Grafiken/ExAI/Local/ice_month_success_dt.png",
  plot     = p,
  width    = 8,
  height   = 6
)

