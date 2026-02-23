library(tidyverse)
library(here)

# dataset from tidy tuesday:
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-09-22/readme.md
# Also: https://www.himalayandatabase.com/.

members <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv"
)

everest_2019 <- members |>
  filter(peak_name == "Everest") |>
  mutate(
    died = if_else(died, "Yes", "No"),
    sex = if_else(sex == "M", "Male", "Female"),
    success = if_else(success, "Yes", "No"),
    oxygen_used = if_else(oxygen_used, "With", "Without")
  ) |>
  filter(!is.na(age) & !is.na(highpoint_metres)) |>
  filter(year == 2019) |>
  relocate(
    year,
    expedition_id,
    member_id,
    highpoint_metres,
    sex,
    success,
    oxygen_used
  )

write_csv(everest_2019, here("slides/12", "data/everest/everest-2019.csv"))
