library(tidyverse)
library(janitor)
library(tidymodels)


bdata <- read_delim("data/bank/bank-full.csv", delim = ";")

bdata <- bdata %>%
  mutate(
    marital = as.factor(marital),
    job = as.factor(job),
    education = as.factor(education),
    default = as.factor(default),
    housing = as.factor(housing),
    loan = as.factor(loan),
    contact = as.factor(contact),
    month = as.factor(month),
    poutcome = as.factor(poutcome),
    y = as.factor(y)  # Zielvariable auch als Faktor!
  )

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(bdata, file = "data/processed/bdata.rds")
