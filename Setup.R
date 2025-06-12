library(tidyverse)  # enthält dplyr, ggplot2, tidyr etc.
library(janitor)


# 1. Treat the first row as the real header
bank.additional.full <- bank.additional.full %>%
  row_to_names(row_number = 1) %>%   # makes first row the header
  clean_names() %>%                  # optional: cleans column names (snake_case)
  mutate(across(where(is.character), ~ type.convert(.x, as.is = TRUE)))   # 2. Convert types (all are currently characters)