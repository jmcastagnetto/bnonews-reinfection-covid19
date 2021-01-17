library(tidyverse)
library(tmap)
data(World)
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
    text = paste0("Source: BNO News (https://bit.ly/confirmedcovid19reinfections)\n", Sys.Date(), " // @jmcastagnetto, Jesus M. Castagnetto"),
    fontface = "bold",
    fontfamily = "Inconsolata",
    bg.color = "white",
    position = c("center", "bottom")
  )
map1
fname <- paste0(
  format(Sys.Date(), "%Y%m%d"),
  "-map-confirmed-cases-covid19-reinfection.png"
)
tmap_save(
  tm = map1,
  filename = fname,
  width = 10,
  height = 5
)

