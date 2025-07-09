library(tidyverse)

# Define duration bins
breaks <- seq(0, 2000, by = 100)
labels <- paste0("≤ ", breaks[-1], "s")

# Group by duration
duration_analysis <- bank.additional.full %>%
  filter(duration <= 2000) %>%  # Only consider calls up to 2000s
  mutate(duration_group = cut(
    duration,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = TRUE
  )) %>%
  group_by(duration_group) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  ) %>%
  drop_na()

# Plot: Duration group vs. success rate
ggplot(duration_analysis, aes(x = duration_group)) +
  geom_col(aes(y = total), fill = "#4C78A8", alpha = 0.85) +
  geom_line(aes(y = success_rate * max(total), group = 1),
            color = "black", size = 1) +
  scale_y_continuous(
    name = "Number of Prospects",
    sec.axis = sec_axis(~ . / max(duration_analysis$total),
                        name = "Success Rate", labels = scales::percent)
  ) +
  labs(
    title = "Call Duration (≤ X Seconds) vs. Success Rate",
    subtitle = "Bars = Number of Prospects | Line = Success Rate per Duration Group",
    x = "Call Duration",
    y = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ⏱️ Average duration for successful prospects by previous outcome
poutcome_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(poutcome) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

# ✅ Success rate by previous outcome
poutcome_success <- bank.additional.full %>%
  group_by(poutcome) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# 🔄 Merge
poutcome_combined <- left_join(poutcome_success, poutcome_dur, by = "poutcome")

# 📊 Plot
ggplot(poutcome_combined, aes(x = fct_reorder(poutcome, success_rate))) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             size = 4, color = "black") +
  scale_y_continuous(
    name = "Success Rate",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(poutcome_combined$mean_duration, na.rm = TRUE),
                        name = "Avg. Duration (Seconds)")
  ) +
  labs(
    title = "Success Rate & Avg. Call Duration by Previous Outcome",
    subtitle = "Bars = Success Rate | Dots = Avg. Duration (Successful Prospects Only)",
    x = "Previous Outcome"
  ) +
  theme_minimal(base_size = 13)

# 🗓️ Success rate & duration by weekday
# Step 1: Average duration for successful calls per weekday
dow_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(day_of_week) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

# Step 2: Success rate per weekday
dow_success <- bank.additional.full %>%
  group_by(day_of_week) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# Step 3: Merge
dow_combined <- left_join(dow_success, dow_dur, by = "day_of_week")

# Ensure order: Mon–Fri
dow_combined$day_of_week <- factor(
  dow_combined$day_of_week,
  levels = c("mon", "tue", "wed", "thu", "fri")
)

# Step 4: Plot
ggplot(dow_combined, aes(x = day_of_week)) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             size = 4, color = "black") +
  scale_y_continuous(
    name = "Success Rate",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(dow_combined$mean_duration, na.rm = TRUE),
                        name = "Avg. Duration (Seconds)")
  ) +
  labs(
    title = "Success Rate & Avg. Duration by Day of the Week",
    subtitle = "Bars = Success Rate | Dots = Avg. Duration of Successful Calls",
    x = "Day of the Week"
  ) +
  theme_minimal(base_size = 13)

# ✅ Success rate by Consumer Price Index (CPI)
cpi_success <- bank.additional.full %>%
  group_by(cons_price_idx) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# 🧮 You must define cpi_dur beforehand if you want the secondary axis to work:
# For example:
# cpi_dur <- bank.additional.full %>%
#   filter(y == "yes") %>%
#   group_by(cons_price_idx) %>%
#   summarise(mean_duration = mean(duration, na.rm = TRUE))

# 🔄 Merge with durations
cpi_combined <- left_join(cpi_success, cpi_dur, by = "cons_price_idx")

# 📊 Plot
ggplot(cpi_combined, aes(x = cons_price_idx)) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85, width = 0.03) +
  scale_y_continuous(
    name = "Success Rate",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(cpi_combined$mean_duration, na.rm = TRUE),
                        name = "Avg. Duration (Seconds)")
  ) +
  labs(
    title = "Success Rate & Avg. Duration vs. Consumer Price Index (CPI)",
    subtitle = "Bars = Success Rate | Line = Avg. Duration of Successful Prospects",
    x = "Consumer Price Index"
  ) +
  theme_minimal(base_size = 13)