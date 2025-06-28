library(tidyverse)
library(janitor)
library(tidymodels)

# Daten laden
bdata <- read_delim("data/bank/bank-full.csv", delim = ";")

# Dummy-Feature für Zielvariable vorab (für Encoding)
bdata <- bdata %>%
  mutate(
    y_numeric = ifelse(y == "yes", 1, 0)
  )

# Target Encoding vorbereiten
poutcome_means <- bdata %>%
  group_by(poutcome) %>%
  summarise(poutcome_numeric = mean(y_numeric, na.rm = TRUE))

# Haupt-Feature-Engineering inkl. aller numerischen Variablen für RandomForest
bdata <- bdata %>%
  mutate(
    # Faktoren
    marital = as.factor(marital),
    job = as.factor(job),
    education = as.factor(education),
    default = as.factor(default),
    housing = as.factor(housing),
    loan = as.factor(loan),
    contact = as.factor(contact),
    month = as.factor(month),
    poutcome = as.factor(poutcome),
    y = as.factor(y),
    
    # Zielvariable als binärer Faktor für RandomForest
    y_bin = factor(ifelse(y == "yes", "yes", "no")),
    
    # Numerische Features
    job_numeric = ifelse(job == "unemployed", 1, 0),
    marital_numeric = case_when(
      marital == "single" ~ 0,
      marital == "married" ~ 1,
      marital == "divorced" ~ 2,
      TRUE ~ NA_real_
    ),
    education_numeric = case_when(
      education == "primary" ~ 0,
      education == "secondary" ~ 1,
      education == "tertiary" ~ 2,
      TRUE ~ NA_real_
    ),
    default_numeric = ifelse(default == "yes", 1, 0),
    housing_numeric = ifelse(housing == "yes", 1, 0),
    loan_numeric = ifelse(loan == "yes", 1, 0),
    log_balance = log(abs(balance) + 1),
    pdays_category = as.factor(ifelse(pdays == -1, "no_previous_contact", "had_contact")),
    
    # Weitere strukturierende Features
    age_group = cut(age, breaks = c(0, 25, 45, 65, 100), right = FALSE),
    contact_season = case_when(
      month %in% c("mar", "apr", "may") ~ "spring",
      month %in% c("jun", "jul", "aug") ~ "summer",
      month %in% c("sep", "oct", "nov") ~ "autumn",
      TRUE ~ "winter"
    ),
    balance_sign = sign(balance)
  )

# Target Encoding anhängen
bdata <- bdata %>%
  left_join(poutcome_means, by = "poutcome")

# Speichern
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(bdata, file = "data/processed/bdata.rds")
