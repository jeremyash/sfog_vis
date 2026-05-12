## Data for superfog visualization: https://vlab.noaa.gov/web/mdl/ndfd

library(terra)
library(sf)
library(lubridate)
library(raster)
library(leaflet)
library(png)
library(openssl)

# ----------------------------
# 1. Region 8 spatial data
# ----------------------------

r8_outline <- st_read(
  "r8_outline.gpkg",
  layer = "r8_outline",
  quiet = TRUE
)

r8_outline_sf <- st_transform(r8_outline, 4326)
r8_outline_v  <- terra::vect(r8_outline_sf)

r8_forests <- st_read("r8_forests", quiet = TRUE)
r8_forests_sf <- sf::st_transform(r8_forests, 4326)
r8_forests_sf <- sf::st_simplify(
  r8_forests_sf,
  dTolerance = 0.001,
  preserveTopology = TRUE
)

# ----------------------------
# 2. NDFD data download/read
# ----------------------------

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
  
  out
}

# ----------------------------
# 3. Superfog classification
# ----------------------------

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

sfog <- classify_superfog_score(
  temp = r_temp,
  rh   = r_rh,
  wind = r_wind,
  sky  = r_sky
)

# ----------------------------
# 4. Analytical raster for point extraction
# ----------------------------

sfog_ll <- terra::project(sfog, "EPSG:4326", method = "near")
sfog_ll <- terra::crop(sfog_ll, r8_outline_v)
sfog_ll <- terra::mask(sfog_ll, r8_outline_v, touches = TRUE)
sfog_ll <- terra::round(sfog_ll)
sfog_ll <- terra::clamp(sfog_ll, lower = 0, upper = 8, values = TRUE)

# Reclassify original score:
# 1 = Minimal  (0-3)
# 2 = Moderate (4-6)
# 3 = High     (7-8)
sfog_ll <- terra::ifel(
  sfog_ll <= 3, 1,
  terra::ifel(
    sfog_ll <= 6, 2,
    terra::ifel(sfog_ll <= 8, 3, NA)
  )
)

sfog_ll <- terra::round(sfog_ll)
sfog_ll <- terra::clamp(sfog_ll, lower = 1, upper = 3, values = TRUE)

if (length(valid_times) == terra::nlyr(sfog_ll)) {
  names(sfog_ll) <- as.character(valid_times)
} else if (is.null(names(sfog_ll)) || any(names(sfog_ll) == "")) {
  names(sfog_ll) <- paste0("forecast_hour_", seq_len(terra::nlyr(sfog_ll)))
}

# ----------------------------
# 5. Leaflet-projected PNG display layers
# ----------------------------
# Only sfog_ll is retained as a raster object in the cache.
# PNG overlays are generated from Leaflet-projected rasters for fast display.

cache_dir <- "cache"
png_dir <- file.path(cache_dir, "sfog_pngs")

dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(png_dir, showWarnings = FALSE, recursive = TRUE)

sfog_ll_tif <- tempfile(fileext = ".tif")

terra::writeRaster(
  sfog_ll,
  sfog_ll_tif,
  overwrite = TRUE
)

sfog_raster <- raster::brick(sfog_ll_tif)

sfog_leaflet_proj <- raster::stack(lapply(seq_len(raster::nlayers(sfog_raster)), function(i) {
  leaflet::projectRasterForLeaflet(
    sfog_raster[[i]],
    method = "ngb"
  )
}))

names(sfog_leaflet_proj) <- names(sfog_ll)

# Convert Web Mercator extent to lat/lon bounds for L.imageOverlay().
e <- raster::extent(sfog_leaflet_proj[[1]])

origin_shift <- 2 * pi * 6378137 / 2

west  <- (e@xmin / origin_shift) * 180
east  <- (e@xmax / origin_shift) * 180

south <- (e@ymin / origin_shift) * 180
north <- (e@ymax / origin_shift) * 180

south <- 180 / pi * (2 * atan(exp(south * pi / 180)) - pi / 2)
north <- 180 / pi * (2 * atan(exp(north * pi / 180)) - pi / 2)

sfog_png_bounds <- list(
  west = west,
  south = south,
  east = east,
  north = north
)

risk_colors <- c(
  "1" = "#58AFDD",
  "2" = "#FFB000",
  "3" = "#CA0020"
)

write_raster_png <- function(r, filename, risk_colors) {
  vals <- raster::as.matrix(r)
  
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
  
  png::writePNG(arr, target = filename)
  invisible(filename)
}

sfog_png_files <- character(raster::nlayers(sfog_leaflet_proj))

for (i in seq_len(raster::nlayers(sfog_leaflet_proj))) {
  png_name <- sprintf("sfog_%03d.png", i)
  png_path <- file.path(png_dir, png_name)
  
  write_raster_png(
    r = sfog_leaflet_proj[[i]],
    filename = png_path,
    risk_colors = risk_colors
  )
  
  sfog_png_files[[i]] <- png_name
}

sfog_png_data <- vapply(
  file.path(png_dir, sfog_png_files),
  function(path) {
    b64 <- openssl::base64_encode(readBin(path, "raw", file.info(path)$size))
    paste0("data:image/png;base64,", b64)
  },
  character(1)
)

# ----------------------------
# 6. Save cache
# ----------------------------

cache <- list(
  sfog_ll = terra::wrap(sfog_ll),
  sfog_png_data = sfog_png_data,
  sfog_png_bounds = sfog_png_bounds,
  r8_forests_sf = r8_forests_sf,
  valid_times = valid_times,
  last_refresh = lubridate::with_tz(Sys.time(), "UTC")
)

saveRDS(cache, file.path(cache_dir, "ndfd_superfog_cache.rds"))

message("Saved cache to: ", file.path(cache_dir, "ndfd_superfog_cache.rds"))
message("Analytical raster layers: ", terra::nlyr(sfog_ll))
message("PNG overlays: ", length(sfog_png_data))
message("Last refresh: ", as.character(cache$last_refresh))
