
# # Übersicht Alter
# Metriken berechnen für Alter
age_stats <- bank.additional.full %>%
  summarise(
    mean = mean(age, na.rm = TRUE),
    median = median(age, na.rm = TRUE),
    q25 = quantile(age, 0.25, na.rm = TRUE),
    q75 = quantile(age, 0.75, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "type", values_to = "value")

# Plot mit Legende
ggplot(bank.additional.full, aes(x = age)) +
  geom_histogram(bins = 30, fill = "#4C78A8", color = "white", alpha = 0.85) +
  
  geom_vline(data = age_stats, aes(xintercept = value, color = type, linetype = type), size = 1.2) +
  
  scale_color_manual(
    name = "Statistik",
    values = c(
      mean = "#E45756",
      median = "#72B7B2",
      q25 = "#FF9DA6",
      q75 = "#FF9DA6"
    ),
    labels = c(
      mean = "Mittelwert",
      median = "Median",
      q25 = "1. Quartil",
      q75 = "3. Quartil"
    )
  ) +
  scale_linetype_manual(
    name = "Statistik",
    values = c(
      mean = "dashed",
      median = "solid",
      q25 = "dotted",
      q75 = "dotted"
    ),
    labels = c(
      mean = "Mittelwert",
      median = "Median",
      q25 = "1. Quartil",
      q75 = "3. Quartil"
    )
  ) +
  
  labs(
    title = "Altersverteilung der Kunden mit statistischen Kennwerten",
    subtitle = "Histogramm mit Mittelwert, Median und Quartilen",
    x = "Alter",
    y = "Anzahl Kunden"
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

# Verteilung der Berufe
bank.additional.full %>%
  count(job) %>%
  ggplot(aes(x = fct_reorder(job, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Verteilung der Berufe",
    x = "Beruf",
    y = "Anzahl"
  ) +
  theme_minimal()

# Verteilung des Familienstands
bank.additional.full %>%
  count(marital) %>%
  ggplot(aes(x = fct_reorder(marital, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Verteilung des Familienstands",
    x = "Familienstand",
    y = "Anzahl"
  ) +
  theme_minimal()

# Übersicht Education

bank.additional.full %>%
  count(education) %>%
  ggplot(aes(x = fct_reorder(education, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Verteilung des Bildungsstands",
    x = "Bildungsstand",
    y = "Anzahl"
  ) +
  theme_minimal()

# Verteilung des Kreditstatus
bank.additional.full %>%
  count(default) %>%
  ggplot(aes(x = fct_reorder(default, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Verteilung des Kreditstatus (Default)",
    x = "Default (Kreditausfall)",
    y = "Anzahl Kunden"
  ) +
  theme_minimal()

# Housing
bank.additional.full %>%
  count(housing) %>%
  ggplot(aes(x = fct_reorder(housing, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Verteilung Wohnkredit-Status",
    x = "Wohnkredit",
    y = "Anzahl Kunden"
  ) +
  theme_minimal()

# Loan
bank.additional.full %>%
  count(loan) %>%
  ggplot(aes(x = fct_reorder(loan, n), y = n)) +
  geom_col(fill = "#4C78A8", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Verteilung Privatkredit-Status",
    x = "Privatkredit",
    y = "Anzahl Kunden"
  ) +
  theme_minimal()

