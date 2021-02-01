library(tidyverse)
library(tmap)
data(World)
raw <- read_csv(
  "data/covid19_confirmed_reinfections.csv",
  na = c("", "-", "NA", "N/A")
)
confirmed <- raw %>%
  group_by(
    location, iso3c
  ) %>%
  tally(name = "Confirmed")

my_data <- World %>%
  left_join(
    confirmed,
    by = c("iso_a3" = "iso3c")
  )

n_confirmed <- sum(confirmed$Confirmed, na.rm = TRUE)
date_range <- range(raw$reported, na.rm = TRUE)

tmap_mode("plot")

map1 <- tm_shape(my_data) +
  tm_polygons("Confirmed", palette = "-inferno") +
  tm_layout(
    main.title = glue::glue("Confirmed COVID-19 reinfections\nWorldwide total: {scales::comma(n_confirmed)}. From {date_range[1]} to {date_range[2]}"),
  ) +
  tm_credits(
    text = paste0("Source: BNO News (https://bit.ly/confirmedcovid19reinfections)\n", Sys.Date(), " // @jmcastagnetto, Jesus M. Castagnetto"),
    fontface = "bold",
    fontfamily = "Inconsolata",
    size = 1,
    bg.color = "white",
    position = c("center", "bottom")
  )
map1
fname <- paste0(
  "plots/",
  format(Sys.Date(), "%Y%m%d"),
  "-map-confirmed-cases-covid19-reinfection.png"
)
tmap_save(
  tm = map1,
  filename = fname,
  width = 10,
  height = 6
)

