library(tidyverse)
library(htmltab)

# URLs for scrapping the data

confirmed_url <- "https://docs.google.com/spreadsheets/u/0/d/e/2PACX-1vR5bdx6Tzz-MSIOfT2A-fm8XMNRpZ7j5PtO2L7PsQe-jNWguN1NGVWyUd_pJ9ICflXZ3Q2iZxrRc0o2/pubhtml/sheet?headers=false&gid=1608044654"

suspected_url <- "https://docs.google.com/spreadsheets/u/0/d/e/2PACX-1vR5bdx6Tzz-MSIOfT2A-fm8XMNRpZ7j5PtO2L7PsQe-jNWguN1NGVWyUd_pJ9ICflXZ3Q2iZxrRc0o2/pubhtml/sheet?headers=false&gid=286583454"


# Process confirmed re-infections -----------------------------------------

confirmed_raw <- htmltab(doc = confirmed_url) %>%
  filter(!is.na(V2))

confirmed_head_df <- confirmed_raw %>%
  slice(1:2)
colnames(confirmed_head_df) <- confirmed_head_df[1,]
confirmed_head_df <- confirmed_head_df[2, ] %>%
  janitor::clean_names() %>%
  select(cases, deaths, recovered, average_interval)

confirmed_df <- confirmed_raw %>%
  slice(3:nrow(confirmed_raw)) %>%
  select(-1)
colnames <- confirmed_df[1,]
colnames(confirmed_df) <- colnames
confirmed_df <- confirmed_df %>%
  janitor::clean_names() %>%
  select(-links, -interval_2)
nrows <- nrow(confirmed_df)
confirmed_df <- confirmed_df[2:(nrows - 1 ), ] %>%
  mutate(
    reported = glue::glue("{reported}, 2020") %>% lubridate::mdy(),
    iso3c = countrycode::countryname(location, destination = "iso3c"),
    age = as.numeric(str_extract(patient, "\\d+")),
    gender = str_extract(patient, "M|F"),
    interval_unit = str_extract(interval, "[a-z]+"),
    interval = as.numeric(str_extract(interval, "\\d+"))
  ) %>%
  relocate(
    iso3c,
    .after = location
  ) %>%
  relocate(
    age, gender,
    .after = patient
  ) %>%
  relocate(
    interval_unit,
    .after = interval
  )


# Process suspected re-infections -----------------------------------------

suspected_raw <- htmltab(doc = suspected_url) %>%
  select(-V1) %>%
  filter(!is.na(V2))

suspected_head_df <- suspected_raw[2, 1:2]
colnames(suspected_head_df) <- suspected_raw[1, 1:2]

# kludgy code to separate by country, need to think about something better

colnames <- suspected_raw[4,]

suspected_df <- bind_rows(
  suspected_raw[6:28,] %>%
    mutate(country = "United States"),
  suspected_raw[30:32,] %>%
    mutate(country = "Argentina"),
  suspected_raw[34,] %>%
    mutate(country = "Australia"),
  suspected_raw[36,] %>%
    mutate(country = "Bahamas"),
  suspected_raw[38:77,] %>%
    mutate(country = "Brazil"),
  suspected_raw[79,] %>%
    mutate(country = "Bulgaria"),
  suspected_raw[81:83,] %>%
    mutate(country = "China"),
  suspected_raw[85:86,] %>%
    mutate(country = "Colombia"),
  suspected_raw[88,] %>%
    mutate(country = "Costa Rica"),
  suspected_raw[90,] %>%
    mutate(country = "Dominican Republic"),
  suspected_raw[92,] %>%
    mutate(country = "El Salvador"),
  suspected_raw[94,] %>%
    mutate(country = "Estonia"),
  suspected_raw[96:97,] %>%
    mutate(country = "Germany"),
  suspected_raw[99,] %>%
    mutate(country = "India"),
  suspected_raw[101,] %>%
    mutate(country = "Iraq"),
  suspected_raw[103,] %>%
    mutate(country = "Israel"),
  suspected_raw[105:106,] %>%
    mutate(country = "Italy"),
  suspected_raw[108,] %>%
    mutate(country = "Latvia"),
  suspected_raw[110:113,] %>%
    mutate(country = "Mexico"),
  suspected_raw[115,] %>%
    mutate(country = "Netherlands"),
  suspected_raw[117:119,] %>%
    mutate(country = "Pakistan"),
  suspected_raw[121:123,] %>%
    mutate(country = "Paraguay"),
  suspected_raw[125:126,] %>%
    mutate(country = "Peru"),
  suspected_raw[128,] %>%
    mutate(country = "Philippines"),
  suspected_raw[130,] %>%
    mutate(country = "Portugal"),
  suspected_raw[132:133,] %>%
    mutate(country = "Qatar"),
  suspected_raw[135,] %>%
    mutate(country = "Russia"),
  suspected_raw[137,] %>%
    mutate(country = "South Africa"),
  suspected_raw[135,] %>%
    mutate(country = "Spain"),
  suspected_raw[141,] %>%
    mutate(country = "Sweden"),
  suspected_raw[143,] %>%
    mutate(country = "Switzerland"),
  suspected_raw[145,] %>%
    mutate(country = "Turkey"),
  suspected_raw[147:148,] %>%
    mutate(country = "Ukraine"),
  suspected_raw[150:153,] %>%
    mutate(country = "United Kingdom")
)

colnames(suspected_df)[1:6] <- colnames
suspected_df <- suspected_df %>%
  janitor::clean_names() %>%
  mutate(
    reported = glue::glue("{reported}, 2020") %>%
      lubridate::mdy(),
    iso3c = countrycode::countryname(country, "iso3c")
  ) %>%
  relocate(
    country, iso3c,
    .before = reported
  ) %>%
  select(-links)

# Save cleaned up data ----------------------------------------------------

write_csv(
  confirmed_raw,
  file = "data/covid19_confirmed_reinfections_raw.csv"
)

write_csv(
  confirmed_head_df,
  file = "data/covid19_confirmed_reinfections_totals.csv"
)

write_csv(
  confirmed_df,
  file = "data/covid19_confirmed_reinfections.csv"
)

write_csv(
  suspected_raw,
  file = "data/covid19_suspected_reinfections_raw.csv"
)

write_csv(
  suspected_head_df,
  file = "data/covid19_suspected_reinfections_totals.csv"
)

write_csv(
  suspected_df,
  file = "data/covid19_suspected_reinfections.csv"
)

save(
  confirmed_head_df,
  confirmed_df,
  suspected_head_df,
  suspected_df,
  file = "data/covid19_reinfections.Rdata"
)
