library(tidyverse)

# Dauergrenzen definieren
breaks <- seq(0, 2000, by = 100)
labels <- paste0("≤ ", breaks[-1], "s")

# Dauer gruppieren
duration_analysis <- bank.additional.full %>%
  filter(duration <= 2000) %>%  # Nur bis 2000s berücksichtigen
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

# Plot
ggplot(duration_analysis, aes(x = duration_group)) +
  geom_col(aes(y = total), fill = "#4C78A8", alpha = 0.85) +
  geom_line(aes(y = success_rate * max(total), group = 1),
            color = "black", size = 1) +
  scale_y_continuous(
    name = "Anzahl der Kontakte",
    sec.axis = sec_axis(~ . / max(duration_analysis$total),
                        name = "Erfolgsrate", labels = scales::percent)
  ) +
  labs(
    title = "Gesprächsdauer (≤ X Sekunden) vs. Erfolgsrate",
    subtitle = "Balken = Kontaktanzahl | Linie = Erfolgsrate pro Dauergruppe",
    x = "Gesprächsdauer",
    y = "Anzahl"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# ⏱️ Average duration for successful contacts by poutcome
poutcome_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(poutcome) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

# ✅ Success rate per poutcome
poutcome_success <- bank.additional.full %>%
  group_by(poutcome) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# 🔄 Merge both metrics
poutcome_combined <- left_join(poutcome_success, poutcome_dur, by = "poutcome")

# 📊 Plot
ggplot(poutcome_combined, aes(x = fct_reorder(poutcome, success_rate))) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             size = 4, color = "black") +
  scale_y_continuous(
    name = "Success Rate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(poutcome_combined$mean_duration, na.rm = TRUE),
                        name = "Ø Duration (Seconds)")
  ) +
  labs(
    title = "Success Rate & Contact Duration by Previous Outcome",
    subtitle = "Bars = Success Rate | Dots = Mean Duration (successful only)",
    x = "Previous Outcome (poutcome)"
  ) +
  theme_minimal(base_size = 13)





# Step 1: Average duration for successful contacts per weekday
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

# Step 3: Merge both
dow_combined <- left_join(dow_success, dow_dur, by = "day_of_week")

# Optional: Ensure days are in order (Mon–Fri)
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
                        name = "Ø Duration (Seconds)")
  ) +
  labs(
    title = "Success Rate & Contact Duration by Day of the Week",
    subtitle = "Bars = Success Rate | Points = Avg. Duration of Successful Calls",
    x = "Day of the Week"
  ) +
  theme_minimal(base_size = 13)





# ✅ Success rate per CPI level
cpi_success <- bank.additional.full %>%
  group_by(cons_price_idx) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# 🔄 Combine both
cpi_combined <- left_join(cpi_success, cpi_dur, by = "cons_price_idx")

# 📊 Plot
ggplot(cpi_combined, aes(x = cons_price_idx)) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85, width = 0.03) +
  scale_y_continuous(
    name = "Success Rate",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(cpi_combined$mean_duration, na.rm = TRUE),
                        name = "Ø Duration (Seconds)")
  ) +
  labs(
    title = "Success Rate & Contact Duration vs. Consumer Price Index",
    subtitle = "Bars = Success Rate | Line = Avg. Duration of Successful Contacts",
    x = "Consumer Price Index (CPI)"
  ) +
  theme_minimal(base_size = 13)
