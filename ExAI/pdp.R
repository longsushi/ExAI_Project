# load required packages
library(randomForest)
library(pdp)
library(ggplot2)
library(scales)  

# load the trained random forest model from blackbox.R
rf_model <- readRDS("model/rf_model.rds")

# use the training data from the model itself
train_data <- rf_model$training

# create a partial dependence plot object for the variable 'balance'
pdp_balance <- partial(
  object      = rf_model,         
  pred.var    = "balance",        
  train       = train_data,       
  prob        = TRUE,             
  which.class = "yes",            
  plot        = FALSE
)

# convert to data frame for ggplot
pdp_df <- as.data.frame(pdp_balance)

# create ggplot with readable axis labels
pdp_plot <- ggplot(pdp_df, aes(x = balance, y = yhat)) +
  geom_line(linewidth = 1, color = "steelblue") +
  labs(
    title = "PDP: Effect of Account Balance on prediction 'yes'",
    x     = "Account Balance",
    y     = "Predicted Probability for 'Yes'"
  ) +
  scale_x_continuous(labels = label_comma()) +
  theme_minimal()

# show the plot
print(pdp_plot)

# save the plot to the graphics folder
ggsave(
  filename = "Grafiken/ExAI/Global/pdp_balance_rf.png",
  plot     = pdp_plot,
  width    = 8,
  height   = 5
)

