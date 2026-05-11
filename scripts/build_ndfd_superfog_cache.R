 ## Data for superfog visualization: https://vlab.noaa.gov/web/mdl/ndfd

library(terra)
library(sf)
library(lubridate)
library(png)

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


read_variable_conus <- function(file, convert_fun = NULL) {
  path <- download_ndfd(file)
  out <- terra::rast(path)
  
  # terra::crs(out) <- ndfd_conus_crs
  
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

# if (is.null(valid_times) || all(is.na(valid_times))) {
#   valid_times <- seq(
#     from = lubridate::floor_date(Sys.time(), "hour"),
#     by = "1 hour",
#     length.out = n
#   )
# }
risk_colors <- c(
  "0" = "#F2F2F2",
  "1" = "#D9EAF7",
  "2" = "#A9D3EA",
  "3" = "#58AFDD",
  "4" = "#FFDA00",
  "5" = "#FFB000",
  "6" = "#FF7A00",
  "7" = "#E64B00",
  "8" = "#CA0020"
)

sfog <- classify_superfog_score(
  temp = r_temp,
  rh   = r_rh,
  wind = r_wind,
  sky  = r_sky
)

# Analytical raster: still used for point/click extraction.
sfog_ll <- terra::project(sfog, "EPSG:4326", method = "near")
sfog_ll <- terra::crop(sfog_ll, r8_outline_v)
sfog_ll <- terra::mask(sfog_ll, r8_outline_v, touches = TRUE)
sfog_ll <- terra::round(sfog_ll)
sfog_ll <- terra::clamp(sfog_ll, lower = 0, upper = 8, values = TRUE)

if (exists("valid_times") && length(valid_times) == terra::nlyr(sfog_ll)) {
  names(sfog_ll) <- as.character(valid_times)
} else if (is.null(names(sfog_ll)) || any(names(sfog_ll) == "")) {
  names(sfog_ll) <- paste0("forecast_hour_", seq_len(terra::nlyr(sfog_ll)))
}

# -------------------------------------------------------------------------
# Build PNG overlays for fast Leaflet display
# -------------------------------------------------------------------------

cache_dir <- "cache"
png_dir <- file.path(cache_dir, "sfog_pngs")

dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(png_dir, showWarnings = FALSE, recursive = TRUE)

# Update this if your repo/path changes.
png_base_url <- "https://raw.githubusercontent.com/jeremyash/sfog_vis/cache-data/cache/sfog_pngs"

e <- terra::ext(sfog_ll)

sfog_bounds <- list(
  lng1 = terra::xmin(e),
  lat1 = terra::ymin(e),
  lng2 = terra::xmax(e),
  lat2 = terra::ymax(e)
)

write_sfog_png <- function(r, filename, risk_colors) {
  vals <- terra::as.matrix(r, wide = TRUE)
  
  vals_chr <- as.character(round(vals))
  vals_chr[is.na(vals)] <- NA_character_
  
  hex <- risk_colors[vals_chr]
  hex[is.na(hex)] <- "#00000000"
  
  rgba <- grDevices::col2rgb(hex, alpha = TRUE) / 255
  
  arr <- array(
    data = as.numeric(rgba),
    dim = c(4, nrow(vals), ncol(vals))
  )
  
  arr <- aperm(arr, c(2, 3, 1))
  
  # If the overlay appears vertically flipped in Leaflet, change this to TRUE.
  flip_y <- FALSE
  if (flip_y) {
    arr <- arr[nrow(arr):1, , , drop = FALSE]
  }
  
  png::writePNG(arr, target = filename)
  invisible(filename)
}

sfog_png_files <- character(terra::nlyr(sfog_ll))

for (i in seq_len(terra::nlyr(sfog_ll))) {
  png_name <- sprintf("sfog_%03d.png", i)
  png_path <- file.path(png_dir, png_name)
  
  write_sfog_png(
    r = sfog_ll[[i]],
    filename = png_path,
    risk_colors = risk_colors
  )
  
  sfog_png_files[[i]] <- png_name
}

sfog_png_urls <- file.path(png_base_url, sfog_png_files)

cache <- list(
  sfog_ll = terra::wrap(sfog_ll),
  sfog_png_urls = sfog_png_urls,
  sfog_bounds = sfog_bounds,
  r8_forests_sf = r8_forests_sf,
  valid_times = if (exists("valid_times")) valid_times else names(sfog_ll),
  last_refresh = Sys.time()
)

saveRDS(cache, file.path(cache_dir, "ndfd_superfog_cache.rds"))

message("Saved cache to: ", file.path(cache_dir, "ndfd_superfog_cache.rds"))
message("Saved PNG overlays to: ", png_dir)
message("PNG count: ", length(sfog_png_urls))
