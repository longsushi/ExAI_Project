# load required packages
library(tidymodels)
library(pdp)

# create a partial dependence plot for the variable 'month_success_rate'
pdp_month <- partial(
  object      = extract_fit_parsnip(fit)$fit,               # trained decision tree model
  pred.var    = "month_success_rate",                       # feature of interest
  train       = bake(prep(rezept), new_data = home_training),  # processed training data
  prob        = TRUE,                                       # output probabilities
  which.class = "yes"                                       # focus on probability of the yes class
)

# plot the result
plot(pdp_month,
     main = "Effect of Month Success Rate on P('yes')",
     xlab = "Success Rate in Current Month",
     ylab = "Predicted Probability for 'Yes'")

# save the plot to the Graphics folder
png("Grafiken/ExAI/Global/pdp_month_success_dt.png", width = 800, height = 600)
plot(pdp_month,
     main = "Effect of Month Success Rate on P('yes')",
     xlab = "Success Rate in Current Month",
     ylab = "Predicted Probability for 'Yes'")
dev.off()
