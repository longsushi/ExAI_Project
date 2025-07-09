# # Age Overview
# Calculate metrics for age
age_stats <- bank.additional.full %>%
  summarise(
    mean = mean(age, na.rm = TRUE),
    median = median(age, na.rm = TRUE),
    q25 = quantile(age, 0.25, na.rm = TRUE),
    q75 = quantile(age, 0.75, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "type", values_to = "value")

# Plot with legend
ggplot(bank.additional.full, aes(x = age)) +
  geom_histogram(bins = 30, fill = "#4C78A8", color = "white", alpha = 0.85) +
  
  geom_vline(data = age_stats, aes(xintercept = value, color = type, linetype = type), size = 1.2) +
  
  scale_color_manual(
    name = "Statistic",
    values = c(
      mean = "#E45756",
      median = "#72B7B2",
      q25 = "#FF9DA6",
      q75 = "#FF9DA6"
    ),
    labels = c(
      mean = "Mean",
      median = "Median",
      q25 = "1st Quartile",
      q75 = "3rd Quartile"
    )
  ) +
  scale_linetype_manual(
    name = "Statistic",
    values = c(
      mean = "dashed",
      median = "solid",
      q25 = "dotted",
      q75 = "dotted"
    ),
    labels = c(
      mean = "Mean",
      median = "Median",
      q25 = "1st Quartile",
      q75 = "3rd Quartile"
    )
  ) +
  
  labs(
    title = "Age Distribution of Prospects with Key Statistics",
    subtitle = "Histogram with Mean, Median, and Quartiles",
    x = "Age",
    y = "Number of Prospects"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    panel.grid.major.y = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# Job distribution
bank.additional.full %>%
  count(job) %>%
  ggplot(aes(x = fct_reorder(job, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Distribution of Job Roles",
    x = "Job",
    y = "Number of Prospects"
  ) +
  theme_minimal()

# Marital status distribution
bank.additional.full %>%
  count(marital) %>%
  ggplot(aes(x = fct_reorder(marital, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Distribution of Marital Status",
    x = "Marital Status",
    y = "Number of Prospects"
  ) +
  theme_minimal()

# Education overview
bank.additional.full %>%
  count(education) %>%
  ggplot(aes(x = fct_reorder(education, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Distribution of Education Levels",
    x = "Education Level",
    y = "Number of Prospects"
  ) +
  theme_minimal()

# Credit default status
bank.additional.full %>%
  count(default) %>%
  ggplot(aes(x = fct_reorder(default, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Distribution of Credit Default Status",
    x = "Credit Default (Yes/No)",
    y = "Number of Prospects"
  ) +
  theme_minimal()

# Housing loan
bank.additional.full %>%
  count(housing) %>%
  ggplot(aes(x = fct_reorder(housing, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Distribution of Housing Loan Status",
    x = "Housing Loan",
    y = "Number of Prospects"
  ) +
  theme_minimal()

# Personal loan
bank.additional.full %>%
  count(loan) %>%
  ggplot(aes(x = fct_reorder(loan, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Distribution of Personal Loan Status",
    x = "Personal Loan",
    y = "Number of Prospects"
  ) +
  theme_minimal()