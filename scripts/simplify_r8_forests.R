library(sf)

r8_forests <- st_read("r8_forests", quiet = TRUE) |>
  st_transform(4326) |>
  st_make_valid()

r8_forests_simplified <- st_simplify(
  r8_forests,
  dTolerance = 0.001,
  preserveTopology = TRUE
)

saveRDS(
  r8_forests_simplified,
  "r8_forests_simplified.rds"
)
