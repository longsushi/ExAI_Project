library(tidyverse)

# Success rate by job
bank.additional.full %>%
  group_by(job) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  ) %>%
  ggplot(aes(x = fct_reorder(job, success_rate), y = success_rate)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Campaign Success Rate by Job",
    x = "Job",
    y = "Success Rate (%)"
  ) +
  theme_minimal()

# Summary by job group – only successful contacts
job_summary <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(job) %>%
  summarise(
    count = n(),
    mean_duration = mean(duration, na.rm = TRUE)
  )

# Success rate by job group (total base)
success_rate <- bank.additional.full %>%
  group_by(job) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# Merge datasets
job_combined <- left_join(success_rate, job_summary, by = "job")

# Plot: success rate + avg. call duration
ggplot(job_combined, aes(x = fct_reorder(job, success_rate))) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)), 
             color = "black", size = 3) +
  coord_flip() +
  scale_y_continuous(
    name = "Success Rate (%)",
    sec.axis = sec_axis(~ . * max(job_combined$mean_duration, na.rm = TRUE),
                        name = "Avg. Call Duration (seconds)")
  ) +
  labs(
    title = "Success Rate & Contact Effort by Job Group",
    subtitle = "Bars = Success Rate | Dots = Avg. Duration of Successful Calls",
    x = "Job"
  ) +
  theme_minimal()

# Avg. duration for successful calls by default group
default_duration <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(default) %>%
  summarise(
    mean_duration = mean(duration, na.rm = TRUE),
    count_success = n()
  )

# Success rate by default group
default_success <- bank.additional.full %>%
  group_by(default) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# Combine
default_combined <- left_join(default_success, default_duration, by = "default")

# Plot
ggplot(default_combined, aes(x = fct_reorder(default, success_rate))) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)), 
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Success Rate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(default_combined$mean_duration, na.rm = TRUE),
                        name = "Avg. Call Duration (seconds)")
  ) +
  labs(
    title = "Success Rate & Contact Effort by Credit Default History",
    subtitle = "Bars = Success Rate | Dots = Avg. Duration of Successful Calls",
    x = "Default Status"
  ) +
  theme_minimal()

# Avg. duration by education
edu_duration <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(education) %>%
  summarise(
    mean_duration = mean(duration, na.rm = TRUE),
    count_success = n()
  )

# Success rate by education
edu_success <- bank.additional.full %>%
  group_by(education) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# Merge
edu_combined <- left_join(edu_success, edu_duration, by = "education")

# Plot
ggplot(edu_combined, aes(x = fct_reorder(education, success_rate))) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Success Rate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(edu_combined$mean_duration, na.rm = TRUE),
                        name = "Avg. Call Duration (seconds)")
  ) +
  labs(
    title = "Success Rate & Contact Effort by Education Level",
    subtitle = "Bars = Success Rate | Dots = Avg. Duration of Successful Calls",
    x = "Education Level"
  ) +
  theme_minimal()

# Age groups
bank.additional.full <- bank.additional.full %>%
  mutate(age_group = cut(
    age,
    breaks = c(18, 30, 40, 50, 60, 100),
    labels = c("18–30", "31–40", "41–50", "51–60", "61+"),
    right = FALSE
  ))

# Avg. duration for successful contacts by age group
age_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(age_group) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

# Success rate by age group
age_success <- bank.additional.full %>%
  group_by(age_group) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# Merge
age_combined <- left_join(age_success, age_dur, by = "age_group")

# Plot
ggplot(age_combined, aes(x = age_group)) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Success Rate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(age_combined$mean_duration, na.rm = TRUE),
                        name = "Avg. Call Duration (seconds)")
  ) +
  labs(
    title = "Success Rate & Contact Effort by Age Group",
    subtitle = "Bars = Success Rate | Dots = Avg. Duration of Successful Calls",
    x = "Age Group"
  ) +
  theme_minimal()

# Housing loan
housing_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(housing) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

housing_success <- bank.additional.full %>%
  group_by(housing) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

housing_combined <- left_join(housing_success, housing_dur, by = "housing")

ggplot(housing_combined, aes(x = housing)) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Success Rate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(housing_combined$mean_duration, na.rm = TRUE),
                        name = "Avg. Call Duration (seconds)")
  ) +
  labs(
    title = "Success Rate & Contact Effort by Housing Loan",
    subtitle = "Bars = Success Rate | Dots = Avg. Duration of Successful Calls",
    x = "Housing Loan"
  ) +
  theme_minimal()

# Loan status
loan_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(loan) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

loan_success <- bank.additional.full %>%
  group_by(loan) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

loan_combined <- left_join(loan_success, loan_dur, by = "loan")

ggplot(loan_combined, aes(x = loan)) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Success Rate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(loan_combined$mean_duration, na.rm = TRUE),
                        name = "Avg. Call Duration (seconds)")
  ) +
  labs(
    title = "Success Rate & Contact Effort by Personal Loan",
    subtitle = "Bars = Success Rate | Dots = Avg. Duration of Successful Calls",
    x = "Personal Loan"
  ) +
  theme_minimal()

# Marital status
marital_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(marital) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

marital_success <- bank.additional.full %>%
  group_by(marital) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

marital_combined <- left_join(marital_success, marital_dur, by = "marital")

ggplot(marital_combined, aes(x = fct_reorder(marital, success_rate))) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Success Rate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(marital_combined$mean_duration, na.rm = TRUE),
                        name = "Avg. Call Duration (seconds)")
  ) +
  labs(
    title = "Success Rate & Contact Effort by Marital Status",
    subtitle = "Bars = Success Rate | Dots = Avg. Duration of Successful Calls",
    x = "Marital Status"
  ) +
  theme_minimal()

# Contact type
contact_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(contact) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

contact_success <- bank.additional.full %>%
  group_by(contact) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

contact_combined <- left_join(contact_success, contact_dur, by = "contact")

ggplot(contact_combined, aes(x = fct_reorder(contact, success_rate))) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Success Rate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(contact_combined$mean_duration, na.rm = TRUE),
                        name = "Avg. Call Duration (seconds)")
  ) +
  labs(
    title = "Success Rate & Contact Effort by Contact Type",
    subtitle = "Bars = Success Rate | Dots = Avg. Duration of Successful Calls",
    x = "Contact Type"
  ) +
  theme_minimal()