library(tidyverse)
library(janitor)
library(tidymodels)

# Daten laden
bdata <- read_delim("data/bank/bank-full.csv", delim = ";")

bdata <- bdata %>%
  mutate(
    y_bin = factor(ifelse(y == "yes", "yes", "no")),
    y_numeric = ifelse(y == "yes", 1, 0)
  )

# Target-Encoding poutcome
poutcome_means <- bdata %>%
  group_by(poutcome) %>%
  summarise(poutcome_numeric = mean(y_numeric, na.rm = TRUE))

# Monatserfolg für month_score
month_success <- bdata %>%
  group_by(month) %>%
  summarise(month_success_rate = mean(y_numeric, na.rm = TRUE))

# Feature Engineering
bdataend <- bdata %>%
  # Standard-Kodierungen
  mutate(
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
    pdays_category = as.factor(ifelse(pdays == -1, "no_contact", "had_contact")),
    age_group = cut(age, breaks = c(0, 30, 45, 60, 100), right = FALSE),
    contact_season = case_when(
      month %in% c("mar", "apr", "may") ~ "spring",
      month %in% c("jun", "jul", "aug") ~ "summer",
      month %in% c("sep", "oct", "nov") ~ "autumn",
      TRUE ~ "winter"
    ),
    balance_sign = sign(balance)
  ) %>%
  # Erweiterte Metriken:
  mutate(
    interaction_rate = campaign / (1 + previous),
    had_recent_contact = if_else(pdays >= 0, 1, 0),
    financial_risk_score = log_balance * (1 + loan_numeric + housing_numeric),
    is_senior = if_else(age >= 60, 1, 0),
    young_student = if_else(age < 30 & job == "student", 1, 0),
    old_retired = if_else(age > 60 & job == "retired", 1, 0),
    balance_level = case_when(
      balance < 0 ~ "negativ",
      balance < 500 ~ "niedrig",
      balance < 1500 ~ "mittel",
      TRUE ~ "hoch"
    )
  ) %>%
  # Join: poutcome + month success
  left_join(poutcome_means, by = "poutcome") %>%
  left_join(month_success, by = "month")

bdataend1 <- bdataend %>%
  select(
    y_bin,                   # Zielvariable
    
    # Demografisch
    age, age_group,
    
    # Sozioökonomisch
    job_numeric, marital_numeric, education_numeric,
    default_numeric, housing_numeric, loan_numeric,
    
    # Finanzielle Situation
    balance, log_balance, balance_sign,
    
    # Kontaktverlauf
    campaign, pdays, previous, poutcome_numeric,
    
    # Verhalten & abgeleitete Metriken
    interaction_rate, had_recent_contact, financial_risk_score,
    
    # Alters-Profile
    is_senior, young_student, old_retired,
    
    # Monatliche Erfolgsquote
    month_success_rate
  ) %>%
  drop_na()

print(bdataend1)

# Speichern
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
saveRDS(bdataend1, file = "data/processed/bdata.rds")
