library(tidyverse)
library(tmap)
data(World)

raw <-read_csv(
  "data/covid19_suspected_reinfections.csv",
  na = c("", "-", "NA", "N/A")
)
suspected <- raw %>%
  group_by(
    country, iso3c
  ) %>%
  summarise(
    Suspected = sum(cases, na.rm = TRUE)
  )

n_suspected <- sum(suspected$Suspected, na.rm = TRUE)
date_range <- range(raw$reported, na.rm = TRUE)

my_data <- World %>%
  left_join(
    suspected,
    by = c("iso_a3" = "iso3c")
  )

tmap_mode("plot")

map1 <- tm_shape(my_data) +
  tm_polygons("Suspected", palette = "-inferno") +
  tm_layout(
    main.title = glue::glue("Suspected COVID-19 reinfections\nWorldwide total: {scales::comma(n_suspected)}. From {date_range[1]} to {date_range[2]}"),
  ) +
  tm_credits(
    text = paste0("Source: BNO News (https://bit.ly/suspectedcovid19reinfections)\n", Sys.Date()," // @jmcastagnetto, Jesus M. Castagnetto"),
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
  "-map-suspected-cases-covid19-reinfection.png"
)
tmap_save(
  tm = map1,
  filename = fname,
  width = 10,
  height = 6
)

