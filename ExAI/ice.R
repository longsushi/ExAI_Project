# load required packages
library(randomForest)
library(pdp)
library(ggplot2)

# load the trained random forest model from blackbox.R
rf_model <- readRDS("model/rf_model.rds")

# extract the training data 
train_data <- rf_model$training

# compute ICE curves for the feature 'balance'
set.seed(42)
ice_balance <- partial(
  object = rf_model,           
  pred.var = "balance",          
  ice = TRUE,               
  center = FALSE,              
  train = train_data,        
  prob = TRUE,               
  which.class = "yes",              
  plot = FALSE
)

# if .id is missing, create it so grouping works
if (!".id" %in% names(ice_balance)) {
  grid_vals <- unique(ice_balance$balance)
  n_grid <- length(grid_vals)
  ice_balance$.id <- rep(seq_len(nrow(train_data)), each = n_grid)
}

# sample 100 ICE curves to keep the plot readable
set.seed(42)
sample_ids <- sample(unique(ice_balance$.id), 100)
ice_sample <- subset(ice_balance, .id %in% sample_ids)

# build the ggplot object
ice_plot <- ggplot(ice_sample, aes(x = balance, y = yhat, group = .id)) +
  geom_line(alpha = 0.2) +
  labs(
    title = "ICE: Effect of Balance on Predicted Probability for 'Yes'",
    x = "Balance",
    y = "Predicted Probability"
)

# display the ICE plot
print(ice_plot)

# save the ICE plot to the graphics folder
ggsave(
  filename = "Grafiken/ExAI/Local/ice_balance_rf.png",
  plot = ice_plot,
  width = 8,
  height = 6
)

