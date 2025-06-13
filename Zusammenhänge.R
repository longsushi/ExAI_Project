library(tidyverse)

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
    title = "Erfolgsrate der Kampagne nach Beruf",
    x = "Beruf",
    y = "Erfolgsrate (%)"
  ) +
  theme_minimal()


# Zusammenfassung pro Berufsgruppe – nur erfolgreiche Kontakte
job_summary <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(job) %>%
  summarise(
    count = n(),
    mean_duration = mean(duration, na.rm = TRUE)
  )

# Erfolgsrate für jede Berufsgruppe (gesamte Basis)
success_rate <- bank.additional.full %>%
  group_by(job) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# Merge beide Datensätze
job_combined <- left_join(success_rate, job_summary, by = "job")

# Plot: Erfolgsrate + Ø Gesprächsdauer
ggplot(job_combined, aes(x = fct_reorder(job, success_rate))) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)), 
             color = "black", size = 3) +
  coord_flip() +
  scale_y_continuous(
    name = "Erfolgsrate (%)",
    sec.axis = sec_axis(~ . * max(job_combined$mean_duration, na.rm = TRUE),
                        name = "Ø Gesprächsdauer (Sekunden)")
  ) +
  labs(
    title = "Erfolgsrate & Kontaktaufwand nach Berufsgruppe",
    subtitle = "Balken = Erfolgsrate | Punkte = Ø Dauer erfolgreicher Gespräche",
    x = "Beruf"
  ) +
  theme_minimal()




# Ø Dauer für erfolgreiche Abschlüsse je default-Gruppe
default_duration <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(default) %>%
  summarise(
    mean_duration = mean(duration, na.rm = TRUE),
    count_success = n()
  )

# Erfolgsrate je default-Gruppe
default_success <- bank.additional.full %>%
  group_by(default) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# Zusammenführen
default_combined <- left_join(default_success, default_duration, by = "default")

# Plot
ggplot(default_combined, aes(x = fct_reorder(default, success_rate))) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)), 
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Erfolgsrate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(default_combined$mean_duration, na.rm = TRUE),
                        name = "Ø Gesprächsdauer (Sekunden)")
  ) +
  labs(
    title = "Erfolgsrate & Kontaktaufwand nach Kreditausfall-Historie",
    subtitle = "Balken = Erfolgsrate | Punkt = Ø Dauer erfolgreicher Gespräche",
    x = "Default-Status"
  ) +
  theme_minimal()



# Ø Dauer nur bei erfolgreichen Abschlüssen
edu_duration <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(education) %>%
  summarise(
    mean_duration = mean(duration, na.rm = TRUE),
    count_success = n()
  )

# Erfolgsrate gesamt
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
    name = "Erfolgsrate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(edu_combined$mean_duration, na.rm = TRUE),
                        name = "Ø Gesprächsdauer (Sekunden)")
  ) +
  labs(
    title = "Erfolgsrate & Kontaktaufwand nach Bildungsstand",
    subtitle = "Balken = Erfolgsrate | Punkt = Ø Dauer erfolgreicher Gespräche",
    x = "Bildungsniveau"
  ) +
  theme_minimal()


# Altersgruppen definieren
bank.additional.full <- bank.additional.full %>%
  mutate(age_group = cut(
    age,
    breaks = c(18, 30, 40, 50, 60, 100),
    labels = c("18–30", "31–40", "41–50", "51–60", "61+"),
    right = FALSE
  ))

# Ø Dauer bei erfolgreichem Abschluss
age_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(age_group) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

# Erfolgsrate je Gruppe
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
    name = "Erfolgsrate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(age_combined$mean_duration, na.rm = TRUE),
                        name = "Ø Gesprächsdauer (Sekunden)")
  ) +
  labs(
    title = "Erfolgsrate & Kontaktaufwand nach Altersgruppe",
    subtitle = "Balken = Erfolgsrate | Punkt = Ø Dauer erfolgreicher Gespräche",
    x = "Altersgruppe"
  ) +
  theme_minimal()




# Dauer bei y == "yes"
housing_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(housing) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

# Erfolgsrate
housing_success <- bank.additional.full %>%
  group_by(housing) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# Merge
housing_combined <- left_join(housing_success, housing_dur, by = "housing")

# Plot
ggplot(housing_combined, aes(x = housing)) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Erfolgsrate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(housing_combined$mean_duration, na.rm = TRUE),
                        name = "Ø Gesprächsdauer (Sekunden)")
  ) +
  labs(
    title = "Erfolgsrate & Kontaktaufwand nach Wohnkredit",
    subtitle = "Balken = Erfolgsrate | Punkt = Ø Dauer erfolgreicher Gespräche",
    x = "Wohnkredit (housing)"
  ) +
  theme_minimal()





# ⏱️ Ø Dauer für erfolgreiche Abschlüsse pro loan-Status
loan_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(loan) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

# ✅ Erfolgsrate pro loan-Status
loan_success <- bank.additional.full %>%
  group_by(loan) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# 🔄 Zusammenführen
loan_combined <- left_join(loan_success, loan_dur, by = "loan")

# 📊 Plot
ggplot(loan_combined, aes(x = loan)) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Erfolgsrate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(loan_combined$mean_duration, na.rm = TRUE),
                        name = "Ø Gesprächsdauer (Sekunden)")
  ) +
  labs(
    title = "Erfolgsrate & Kontaktaufwand nach Privatkredit",
    subtitle = "Balken = Erfolgsrate | Punkt = Ø Dauer erfolgreicher Gespräche",
    x = "Privatkredit (loan)"
  ) +
  theme_minimal()






# Ø Dauer bei y == "yes"
marital_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(marital) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

# Erfolgsrate je marital-Status
marital_success <- bank.additional.full %>%
  group_by(marital) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# Zusammenführen
marital_combined <- left_join(marital_success, marital_dur, by = "marital")

# Plot erstellen
ggplot(marital_combined, aes(x = fct_reorder(marital, success_rate))) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Erfolgsrate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(marital_combined$mean_duration, na.rm = TRUE),
                        name = "Ø Gesprächsdauer (Sekunden)")
  ) +
  labs(
    title = "Erfolgsrate & Kontaktaufwand nach Familienstand",
    subtitle = "Balken = Erfolgsrate | Punkt = Ø Dauer erfolgreicher Gespräche",
    x = "Familienstand"
  ) +
  theme_minimal()






# Ø Dauer für erfolgreiche Abschlüsse pro Kontaktart
contact_dur <- bank.additional.full %>%
  filter(y == "yes") %>%
  group_by(contact) %>%
  summarise(mean_duration = mean(duration, na.rm = TRUE))

# Erfolgsrate pro Kontaktart
contact_success <- bank.additional.full %>%
  group_by(contact) %>%
  summarise(
    total = n(),
    success = sum(y == "yes"),
    success_rate = success / total
  )

# Zusammenführen
contact_combined <- left_join(contact_success, contact_dur, by = "contact")

# Plot erstellen
ggplot(contact_combined, aes(x = fct_reorder(contact, success_rate))) +
  geom_col(aes(y = success_rate), fill = "#4C78A8", alpha = 0.85) +
  geom_point(aes(y = mean_duration / max(mean_duration, na.rm = TRUE)),
             color = "black", size = 4) +
  scale_y_continuous(
    name = "Erfolgsrate (%)",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . * max(contact_combined$mean_duration, na.rm = TRUE),
                        name = "Ø Gesprächsdauer (Sekunden)")
  ) +
  labs(
    title = "Erfolgsrate & Kontaktaufwand nach Kontaktart",
    subtitle = "Balken = Erfolgsrate | Punkt = Ø Dauer erfolgreicher Gespräche",
    x = "Kontaktart (contact)"
  ) +
  theme_minimal()
