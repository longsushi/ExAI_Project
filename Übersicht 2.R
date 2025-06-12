library(ggplot2)

ggplot(bank.full, aes(x = fct_infreq(job))) +
  geom_bar(fill = "#4C78A8") +
  coord_flip() +
  labs(
    title = "Anzahl der Kunden pro Berufsgruppe",
    x = "Beruf",
    y = "Anzahl"
  ) +
  theme_minimal(base_size = 14)