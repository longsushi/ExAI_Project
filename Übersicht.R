library(ggplot2)
library(dplyr)
library(tidyverse)

# Metriken berechnen
V1_stats <- bank.additional.full %>%
  summarise(
    mean = mean(V1),
    median = median(V1),
    q25 = quantile(V1, 0.25),
    q75 = quantile(V1, 0.75)
  ) %>%
  pivot_longer(everything(), names_to = "type", values_to = "value")

# Plot mit Legende
ggplot(bank.full, aes(x = V1)) +
  geom_histogram(bins = 30, fill = "#4C78A8", color = "white", alpha = 0.85) +
  
  # Vertikale Linien mit Farben & Legende
  geom_vline(data = V1_stats, aes(xintercept = value, color = type, linetype = type), size = 1.2) +
  
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
