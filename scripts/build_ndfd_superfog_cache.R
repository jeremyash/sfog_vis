 ## Data for superfog visualization: https://vlab.noaa.gov/web/mdl/ndfd

library(terra)
library(sf)
library(lubridate)

r8_outline <- st_read(
  "r8_outline.gpkg",
  layer = "r8_outline",
  quiet = TRUE
)

r8_outline_sf <- st_transform(r8_outline, 4326)
r8_outline_v  <- terra::vect(r8_outline_sf)

r8_forests <- st_read("r8_forests", quiet = TRUE)
r8_forests_sf <- sf::st_transform(r8_forests, 4326)

base_url <- "https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd"

files <- c(
  temp = "ds.temp.bin",
  rh   = "ds.rhm.bin",
  wind = "ds.wspd.bin",
  sky  = "ds.sky.bin"
)

dir.create("ndfd_region8", showWarnings = FALSE)

download_ndfd <- function(file) {
  out <- file.path(
    "ndfd_region8",
    paste0("AR.conus_", sub("\\.bin$", ".grib2", file))
  )
  
  download.file(
    url = paste(base_url, "AR.conus", "VP.001-003", file, sep = "/"),
    destfile = out,
    mode = "wb",
    quiet = FALSE
  )
  
  out
}
ndfd_conus_crs <- paste(
  "+proj=lcc",
  "+lat_1=25",
  "+lat_2=25",
  "+lat_0=25",
  "+lon_0=-95",
  "+x_0=0",
  "+y_0=0",
  "+R=6371200",
  "+units=m",
  "+no_defs"
)

read_variable_conus <- function(file, convert_fun = NULL) {
  path <- download_ndfd(file)
  out <- terra::rast(path)
  
  terra::crs(out) <- ndfd_conus_crs
  
  if (!is.null(convert_fun)) {
    out <- convert_fun(out)
  }
  
  out
}

classify_superfog_score <- function(temp, rh, wind, sky) {
  temp_critical <- temp <= 55
  rh_critical   <- rh >= 90
  wind_critical <- wind <= 4
  sky_critical  <- sky <= 40
  
  temp_watch <- temp <= 70
  rh_watch   <- rh >= 70
  wind_watch <- wind <= 7
  sky_watch  <- sky <= 60
  
  n_watch <- temp_watch + rh_watch + wind_watch + sky_watch
  n_crit  <- temp_critical + rh_critical + wind_critical + sky_critical
  
  terra::ifel(
    n_watch < 4,
    n_watch,
    4 + n_crit
  )
}

r_temp <- read_variable_conus(
  files["temp"],
  convert_fun = function(x) (x * 9 / 5) + 32
)

r_wind <- read_variable_conus(
  files["wind"],
  convert_fun = function(x) x * 2.23694
)

r_rh  <- read_variable_conus(files["rh"])
r_sky <- read_variable_conus(files["sky"])

r_rh   <- terra::resample(r_rh, r_temp, method = "near")
r_wind <- terra::resample(r_wind, r_temp, method = "near")
r_sky  <- terra::resample(r_sky, r_temp, method = "near")

n <- min(
  terra::nlyr(r_temp),
  terra::nlyr(r_rh),
  terra::nlyr(r_wind),
  terra::nlyr(r_sky)
)

r_temp <- r_temp[[1:n]]
r_rh   <- r_rh[[1:n]]
r_wind <- r_wind[[1:n]]
r_sky  <- r_sky[[1:n]]


valid_times <- terra::time(r_temp)

if (is.null(valid_times) || all(is.na(valid_times))) {
  valid_times <- seq(
    from = lubridate::floor_date(Sys.time(), "hour"),
    by = "1 hour",
    length.out = n
  )
}

sfog <- classify_superfog_score(r_temp, r_rh, r_wind, r_sky)
names(sfog) <- format(valid_times, "%Y-%m-%d %H:%M")

sfog_ll <- terra::project(sfog, "EPSG:4326", method = "near")
sfog_ll <- terra::crop(sfog_ll, r8_outline_v)
sfog_ll <- terra::mask(sfog_ll, r8_outline_v, touches = TRUE)
sfog_ll <- terra::round(sfog_ll)
sfog_ll <- terra::clamp(sfog_ll, lower = 0, upper = 8, values = TRUE)

cache <- list(
  sfog_ll = terra::wrap(sfog_ll),
  r8_forests_sf = r8_forests_sf,
  valid_times = valid_times,
  last_refresh = Sys.time()
)

dir.create("cache", showWarnings = FALSE)
saveRDS(cache, "cache/ndfd_superfog_cache.rds")