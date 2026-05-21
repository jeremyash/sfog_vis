library(terra)
library(leaflet)
library(htmlwidgets)
library(lubridate)
library(sf)

base_url <- "https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd"

download_ndfd <- function(file) {
  
  dir.create("ndfd_region8", showWarnings = FALSE)
  
  out <- file.path(
    "ndfd_region8",
    paste0(
      "AR.conus_",
      sub("\\.bin$", ".grib2", file)
    )
  )
  
  download.file(
    url = paste(
      base_url,
      "AR.conus",
      "VP.001-003",
      file,
      sep = "/"
    ),
    destfile = out,
    mode = "wb"
  )
  
  out
}

temp_file <- download_ndfd("ds.temp.bin")
rh_file   <- download_ndfd("ds.rhm.bin")
wind_file <- download_ndfd("ds.wspd.bin")
sky_file  <- download_ndfd("ds.sky.bin")

out_dir <- "debug_superfog_layers"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

r8 <- st_read("r8_outline.gpkg", quiet = TRUE) |>
  st_transform(4326)

r8_v <- terra::vect(r8)

ndfd_dir <- "ndfd_region8"

temp_file <- file.path(ndfd_dir, "AR.conus_ds.temp.grib2")
rh_file   <- file.path(ndfd_dir, "AR.conus_ds.rhm.grib2")
wind_file <- file.path(ndfd_dir, "AR.conus_ds.wspd.grib2")
sky_file  <- file.path(ndfd_dir, "AR.conus_ds.sky.grib2")

stopifnot(file.exists(temp_file))
stopifnot(file.exists(rh_file))
stopifnot(file.exists(wind_file))
stopifnot(file.exists(sky_file))

temp_f <- (terra::rast(temp_file) * 9 / 5) + 32
rh <- terra::rast(rh_file)
wind_mph <- terra::rast(wind_file) * 2.23694
sky <- terra::rast(sky_file)

rh <- terra::resample(rh, temp_f, method = "near")
wind_mph <- terra::resample(wind_mph, temp_f, method = "near")
sky <- terra::resample(sky, temp_f, method = "near")

n <- min(nlyr(temp_f), nlyr(rh), nlyr(wind_mph), nlyr(sky))

temp_f <- temp_f[[1:n]]
rh <- rh[[1:n]]
wind_mph <- wind_mph[[1:n]]
sky <- sky[[1:n]]

valid_times <- terra::time(temp_f)

if (is.null(valid_times) || all(is.na(valid_times))) {
  valid_times <- seq(
    from = lubridate::floor_date(Sys.time(), "hour"),
    by = "1 hour",
    length.out = n
  )
}

temp_f_ll <- terra::project(temp_f, "EPSG:4326")
rh_ll <- terra::project(rh, "EPSG:4326")
wind_ll <- terra::project(wind_mph, "EPSG:4326")
sky_ll <- terra::project(sky, "EPSG:4326")

temp_f_ll <- terra::mask(terra::crop(temp_f_ll, r8_v), r8_v)
rh_ll <- terra::mask(terra::crop(rh_ll, r8_v), r8_v)
wind_ll <- terra::mask(terra::crop(wind_ll, r8_v), r8_v)
sky_ll <- terra::mask(terra::crop(sky_ll, r8_v), r8_v)

hour <- 12

temp_class <- terra::classify(temp_f_ll, matrix(c(-Inf,55,3, 55,70,2, 70,Inf,1), ncol = 3, byrow = TRUE))
rh_class <- terra::classify(rh_ll, matrix(c(-Inf,70,1, 70,90,2, 90,Inf,3), ncol = 3, byrow = TRUE))
wind_class <- terra::classify(wind_ll, matrix(c(-Inf,4,3, 4,7,2, 7,Inf,1), ncol = 3, byrow = TRUE))
sky_class <- terra::classify(sky_ll, matrix(c(-Inf,40,3, 40,60,2, 60,Inf,1), ncol = 3, byrow = TRUE))

pal_class <- colorFactor(
  palette = c("1" = "#58AFDD", "2" = "#FFB000", "3" = "#CA0020"),
  domain = c(1, 2, 3),
  na.color = "transparent"
)

m <- leaflet() |>
  addProviderTiles(providers$OpenStreetMap.Mapnik) |>
  fitBounds(-96, 24, -74, 38) |>
  addRasterImage(temp_class[[hour]], colors = pal_class, opacity = 0.65, group = "Temp Threshold") |>
  addRasterImage(rh_class[[hour]], colors = pal_class, opacity = 0.65, group = "RH Threshold") |>
  addRasterImage(wind_class[[hour]], colors = pal_class, opacity = 0.65, group = "Wind Threshold") |>
  addRasterImage(sky_class[[hour]], colors = pal_class, opacity = 0.65, group = "Cloud Threshold") |>
  addPolygons(data = r8, color = "#5b6573", weight = 1.2, fill = FALSE) |>
  addLayersControl(
    overlayGroups = c("Temp Threshold", "RH Threshold", "Wind Threshold", "Cloud Threshold"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  hideGroup(c("RH Threshold", "Wind Threshold", "Cloud Threshold"))

htmlwidgets::saveWidget(
  m,
  file.path(out_dir, "ndfd_superfog_threshold_check.html"),
  selfcontained = TRUE
)

browseURL(file.path(out_dir, "ndfd_superfog_threshold_check.html"))