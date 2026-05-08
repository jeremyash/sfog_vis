library(terra)
library(sf)
library(dplyr)
library(leaflet)
library(lubridate)

base_url <- "https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.seast/VP.001-003"

files <- c(
  temp = "ds.temp.bin",
  rh   = "ds.rhm.bin",
  wind = "ds.wspd.bin",
  sky  = "ds.sky.bin"
)

dir.create("ndfd_seast", showWarnings = FALSE)

download_ndfd <- function(x) {
  url <- file.path(base_url, x)
  out <- file.path("ndfd_seast", sub("\\.bin$", ".grib2", x))
  download.file(url, out, mode = "wb", quiet = FALSE)
  out
}

paths <- lapply(files, download_ndfd)

r_temp <- rast(paths$temp)
r_rh   <- rast(paths$rh)
r_wind <- rast(paths$wind)
r_sky  <- rast(paths$sky)

# Align grids
r_rh   <- resample(r_rh, r_temp)
r_wind <- resample(r_wind, r_temp)
r_sky  <- resample(r_sky, r_temp)

# Make sure layer counts match
n <- min(nlyr(r_temp), nlyr(r_rh), nlyr(r_wind), nlyr(r_sky))

r_temp <- r_temp[[1:n]]
r_rh   <- r_rh[[1:n]]
r_wind <- r_wind[[1:n]]
r_sky  <- r_sky[[1:n]]

classify_superfog <- function(temp, rh, wind, sky) {
  temp_bad <- temp <= 55
  rh_bad   <- rh >= 90
  wind_bad <- wind <= 4
  sky_bad  <- sky <= 40
  
  n_bad <- temp_bad + rh_bad + wind_bad + sky_bad
  
  # 3 = PB Piedmont Required
  # 2 = PB Piedmont Recommended
  # 1 = Watch
  # 0 = Minimal
  classify(
    n_bad,
    rcl = matrix(
      c(
        0, 0, 0,
        1, 2, 1,
        3, 3, 2,
        4, 4, 3
      ),
      ncol = 3,
      byrow = TRUE
    )
  )
}

sfog <- classify_superfog(r_temp, r_rh, r_wind, r_sky)

names(sfog) <- paste0("hour_", seq_len(nlyr(sfog)))

pal <- colorFactor(
  palette = c(
    "0" = "#58afdd",
    "1" = "#FFDA00",
    "2" = "#FF9900",
    "3" = "#CA0020"
  ),
  domain = c(0, 1, 2, 3),
  na.color = "transparent"
)

labels <- c(
  "0" = "Minimal",
  "1" = "Watch",
  "2" = "PB Piedmont Recommended",
  "3" = "PB Piedmont Required"
)

m <- leaflet() |>
  addProviderTiles(providers$CartoDB.Voyager)

for (i in seq_len(nlyr(sfog))) {
  m <- m |>
    addRasterImage(
      sfog[[i]],
      colors = pal,
      opacity = 0.65,
      group = names(sfog)[i],
      project = TRUE
    )
}

m |>
  addLayersControl(
    overlayGroups = names(sfog),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addLegend(
    pal = pal,
    values = c(0, 1, 2, 3),
    labels = labels,
    title = "Superfog Screening",
    opacity = 0.8
  )