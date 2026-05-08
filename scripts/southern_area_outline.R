# build once
library(tigris)
library(sf)
library(dplyr)

r8_states <- c(
  "Texas",
  "Oklahoma",
  "Arkansas",
  "Louisiana",
  "Mississippi",
  "Alabama",
  "Georgia",
  "Florida",
  "South Carolina",
  "North Carolina",
  "Tennessee",
  "Kentucky",
  "Virginia"
)

states_sf <- tigris::states(cb = TRUE, year = 2024) |>
  st_transform(4326) |>
  filter(NAME %in% r8_states)

r8_outline <- states_sf |>
  summarise()

# optional offshore buffer
r8_outline <- st_buffer(
  st_transform(r8_outline, 5070),
  dist = 10000
) |>
  st_transform(4326)

st_write(
  r8_outline,
  "./r8_outline.gpkg",
  layer = "r8_outline",
  delete_dsn = TRUE
)
