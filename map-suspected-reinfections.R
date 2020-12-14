library(tidyverse)
library(tmap)

suspected <- read_csv(
  "data/covid19_suspected_reinfections.csv",
  na = c("", "-", "NA", "N/A")
  ) %>%
  group_by(
    country, iso3c
  ) %>%
  summarise(
    Suspected = sum(cases, na.rm = TRUE)
  )

my_data <- World %>%
  left_join(
    suspected,
    by = c("iso_a3" = "iso3c")
  )

tmap_mode("plot")

map1 <- tm_shape(my_data) +
  tm_polygons("Suspected", palette = "-inferno") +
  tm_layout(
    main.title = "Suspected COVID-19 reinfections"
  ) +
  tm_credits(
    text = "Source: BNO News (https://bit.ly/suspectedcovid19reinfections)\n2020-12-14 // @jmcastagnetto, Jesus M. Castagnetto",
    fontface = "bold",
    fontfamily = "Inconsolata",
    bg.color = "white",
    position = c("center", "bottom")
  )
map1
tmap_save(
  tm = map1,
  filename = "20201214-map-suspected-cases-covid19-reinfection.png",
  width = 10,
  height = 5
)

