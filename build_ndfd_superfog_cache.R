 ## Data for superfog visualization: https://vlab.noaa.gov/web/mdl/ndfd

library(terra)
library(sf)
library(lubridate)

region_8 <- st_read("region_8", quiet = TRUE)
region_8_sf <- sf::st_transform(region_8, 4326)
region_8_v  <- terra::vect(region_8_sf)

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

read_variable_conus <- function(file, convert_fun = NULL) {
  path <- download_ndfd(file)
  out <- terra::rast(path)
  
  if (!is.null(convert_fun)) {
    out <- convert_fun(out)
  }
  
  region_8_match <- terra::project(region_8_v, terra::crs(out))
  
  out <- terra::crop(out, region_8_match)
  out <- terra::mask(out, region_8_match)
  
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
sfog_ll <- terra::crop(sfog_ll, region_8_v)
sfog_ll <- terra::mask(sfog_ll, region_8_v)

cache <- list(
  sfog_ll = sfog_ll,
  r8_forests_sf = r8_forests_sf,
  valid_times = valid_times,
  last_refresh = Sys.time()
)

dir.create("cache", showWarnings = FALSE)
saveRDS(cache, "cache/ndfd_superfog_cache.rds")