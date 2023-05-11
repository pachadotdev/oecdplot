library(tidyr)
library(dplyr)
library(forcats)

protected_terrestrial_areas <- read_excel("dev/protected_terrestrial_areas.xlsx")

pta <- pivot_longer(protected_terrestrial_areas, minimum:maximum) %>%
  mutate(
    region = case_when(
      name == "minimum" ~ reg_min,
      name == "maximum" ~ reg_max,
      TRUE ~ "Country Value"
    )
  ) %>%
  select(country, region, category = name, pct_pta = value) %>%
  mutate(
    category = case_when(
      category == "minimum" ~ "Minimum",
      category == "maximum" ~ "Maximum",
      TRUE ~ "Country Value"
    )
  ) %>%
  mutate_if(is.character, as.factor)

pta$category <- fct_relevel(pta$category, "Minimum", "Country Value", "Maximum")

levels(pta$category)

use_data(pta, overwrite = TRUE)
