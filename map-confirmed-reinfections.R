library(tidyverse)
library(tmap)

confirmed <- read_csv(
  "data/covid19_confirmed_reinfections.csv",
  na = c("", "-", "NA", "N/A")
  ) %>%
  group_by(
    location, iso3c
  ) %>%
  tally(name = "Confirmed")

my_data <- World %>%
  left_join(
    confirmed,
    by = c("iso_a3" = "iso3c")
  )

tmap_mode("plot")

map1 <- tm_shape(my_data) +
  tm_polygons("Confirmed", palette = "-inferno") +
  tm_layout(
    main.title = "Confirmed COVID-19 reinfections"
  ) +
  tm_credits(
    text = "Source: BNO News (https://bit.ly/confirmedcovid19reinfections)\n2020-12-14 // @jmcastagnetto, Jesus M. Castagnetto",
    fontface = "bold",
    fontfamily = "Inconsolata",
    bg.color = "white",
    position = c("center", "bottom")
  )
map1
tmap_save(
  tm = map1,
  filename = "20201214-map-confimed-cases-covid19-reinfection.png",
  width = 10,
  height = 5
)

