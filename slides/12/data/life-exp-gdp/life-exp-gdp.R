# load packages ----

library(tidyverse)

# load data ----

gdp_pcap_raw <- read_csv(here::here(
  "slides/12",
  "data/life-exp-gdp/gdp-pcap.csv"
)) |>
  select(-geo)

life_exp_raw <- read_csv(here::here(
  "slides/12",
  "data/life-exp-gdp/life-exp.csv"
)) |>
  select(-geo)

continents <- read_csv(here::here(
  "slides/12",
  "data/life-exp-gdp/continents.csv"
)) |>
  select(entity, continent)

# prep data ----

life_exp <- life_exp_raw |>
  pivot_longer(
    cols = -name,
    names_to = "year",
    values_to = "life_exp",
    values_transform = as.numeric
  ) |>
  filter(year %in% seq(1950, 2025, by = 5))

gdp_pcap <- gdp_pcap_raw |>
  pivot_longer(
    cols = -name,
    names_to = "year",
    values_to = "gdp_percap",
    values_transform = as.numeric
  ) |>
  filter(year %in% seq(1950, 2025, by = 5))

life_exp_gdp <- life_exp |>
  left_join(gdp_pcap, by = c("name", "year")) |>
  left_join(continents, by = join_by(name == entity)) |>
  mutate(
    continent = case_when(
      name == "UAE" ~ "Asia",
      name == "Bahamas" ~ "North America",
      name == "Congo, Dem. Rep." ~ "Africa",
      name == "Congo, Rep." ~ "Africa",
      name == "Cape Verde" ~ "Africa",
      name == "Czech Republic" ~ "Europe",
      name == "Micronesia, Fed. Sts." ~ "Oceania",
      name == "UK" ~ "Europe",
      name == "Gambia" ~ "Africa",
      name == "Kyrgyz Republic" ~ "Asia",
      name == "Lao" ~ "Asia",
      name == "Turkey" ~ "Asia",
      name == "USA" ~ "North America",
      .default = continent
    ),
    name = if_else(name == "Turkey", "Turkiye", name)
  ) |>
  rename(country = name) |>
  relocate(country, continent) |>
  filter(country != "Liechtenstein")

# inspect NAs

life_exp_gdp |>
  filter(is.na(gdp_percap) | is.na(life_exp) | is.na(continent)) |>
  select(country, continent, year, life_exp, gdp_percap)

# write csv ----

write_csv(
  life_exp_gdp,
  here::here("slides/12", "data/life-exp-gdp/life-exp-gdp.csv")
)
