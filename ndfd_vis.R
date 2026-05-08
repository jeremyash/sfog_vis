# ----------------------------
# 0. Packages and basic data
# ----------------------------

library(terra)
library(sf)
library(htmltools)
library(jsonlite)
library(lubridate)
library(shiny)
library(leaflet)

# gis
region_8 <- st_read("region_8")
region_8_sf <- sf::st_transform(region_8, 4326)
region_8_v  <- terra::vect(region_8_sf)

r8_forests <- st_read("r8_forests")
r8_forests_sf <- sf::st_transform(r8_forests, 4326)

# ----------------------------
# 1. Download NDFD Day 1-3 SE grids
# ----------------------------

base_url <- "https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd"

ndfd_areas <- c("AR.conus")

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
  
  if (!file.exists(out)) {
    download.file(
      url = paste(base_url, "AR.conus", "VP.001-003", file, sep = "/"),
      destfile = out,
      mode = "wb",
      quiet = FALSE
    )
  }
  
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

r_temp <- read_variable_conus(
  files["temp"],
  convert_fun = function(x) (x * 9 / 5) + 32
)

r_wind <- read_variable_conus(
  files["wind"],
  convert_fun = function(x) x * 2.23694
)

r_rh <- read_variable_conus(files["rh"])
r_sky <- read_variable_conus(files["sky"])

# ----------------------------
# 2. Align layers
# ----------------------------

r_rh   <- resample(r_rh, r_temp, method = "near")
r_wind <- resample(r_wind, r_temp, method = "near")
r_sky  <- resample(r_sky, r_temp, method = "near")

n <- min(nlyr(r_temp), nlyr(r_rh), nlyr(r_wind), nlyr(r_sky))

r_temp <- r_temp[[1:n]]
r_rh   <- r_rh[[1:n]]
r_wind <- r_wind[[1:n]]
r_sky  <- r_sky[[1:n]]

valid_times <- time(r_temp)

if (is.null(valid_times) || all(is.na(valid_times))) {
  valid_times <- seq(
    from = floor_date(Sys.time(), "hour"),
    by = "1 hour",
    length.out = n
  )
}

# ----------------------------
# 3. Classify superfog risk
# ----------------------------

classify_superfog_score <- function(temp, rh, wind, sky) {
  
  # Critical thresholds
  temp_critical <- temp <= 55
  rh_critical   <- rh >= 90
  wind_critical <- wind <= 4
  sky_critical  <- sky <= 40
  
  # Watchout-or-critical thresholds
  temp_watch <- temp <= 70
  rh_watch   <- rh >= 70
  wind_watch <- wind <= 7
  sky_watch  <- sky <= 60
  
  n_watch <- temp_watch + rh_watch + wind_watch + sky_watch
  n_crit  <- temp_critical + rh_critical + wind_critical + sky_critical
  
  # 0-3 = number of watchout variables met
  # 4-8 = all 4 watchout met, plus critical severity
  ifel(
    n_watch < 4,
    n_watch,
    4 + n_crit
  )
}

sfog <- classify_superfog_score(r_temp, r_rh, r_wind, r_sky)
names(sfog) <- format(valid_times, "%Y-%m-%d %H:%M")

# Optional: reduce file size for faster local viewing
# sfog <- aggregate(sfog, fact = 2, method = "modal", na.rm = TRUE)

# Project to lat/lon for Leaflet and crop to R8
sfog_ll <- project(sfog, "EPSG:4326", method = "near")
sfog_ll <- terra::crop(sfog_ll, region_8_v)
sfog_ll <- terra::mask(sfog_ll, region_8_v)


# ----------------------------
# 4. PLotting
# ----------------------------

pal <- leaflet::colorFactor(
  palette = c(
    "0" = "#F2F2F2",  # none
    "1" = "#D9EAF7",  # low
    "2" = "#A9D3EA",  # moderate-low
    "3" = "#58AFDD",  # near threshold
    "4" = "#FFDA00",  # all watchout met
    "5" = "#FFB000",  # all watchout + 1 critical
    "6" = "#FF7A00",  # all watchout + 2 critical
    "7" = "#E64B00",  # all watchout + 3 critical
    "8" = "#CA0020"   # all critical
  ),
  domain = 0:8,
  na.color = "transparent"
)

legend_html <- HTML('
<div style="background:white; padding:10px; border-radius:6px;">
  <div style="font-weight:bold; margin-bottom:6px;">Superfog Risk</div>
  <div><span style="background:#F2F2F2; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> 0</div>
  <div><span style="background:#D9EAF7; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> 1</div>
  <div><span style="background:#A9D3EA; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> 2</div>
  <div><span style="background:#58AFDD; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> 3</div>
  <div><span style="background:#FFDA00; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> Watchout</div>
  <div><span style="background:#FFB000; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> 5</div>
  <div><span style="background:#FF7A00; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> 6</div>
  <div><span style="background:#E64B00; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> 7</div>
  <div><span style="background:#CA0020; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> Critical</div>
</div>
')
layer_labels <- names(sfog_ll)

if (is.null(layer_labels) || any(layer_labels == "")) {
  layer_labels <- paste("Forecast hour", seq_len(terra::nlyr(sfog_ll)))
}

ui <- fluidPage(
  titlePanel("NDFD Superfog Screening"),
  
  sidebarLayout(
    sidebarPanel(
      
      # -------------------------
      # Top row: arrows + time
      # -------------------------
      
      fluidRow(
        
        column(
          width = 2,
          actionButton(
            "prev_hour",
            label = NULL,
            icon = icon("chevron-left"),
            width = "100%"
          )
        ),
        
        column(
          width = 8,
          div(
            style = "
          text-align:center;
          font-weight:bold;
          font-size:22px;
          padding-top:6px;
        ",
            textOutput("valid_time")
          )
        ),
        
        column(
          width = 2,
          actionButton(
            "next_hour",
            label = NULL,
            icon = icon("chevron-right"),
            width = "100%"
          )
        )
      ),
      
      br(),
      
      # -------------------------
      # Second row: slider
      # -------------------------
      
      sliderInput(
        inputId = "hour",
        label = NULL,
        min = 1,
        max = terra::nlyr(sfog_ll),
        value = 1,
        step = 1,
        animate = animationOptions(
          interval = 900,
          loop = TRUE
        )
      )
    ),
    
    mainPanel(
      leafletOutput("sfog_map", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  output$valid_time <- renderText({
    
    t <- as.POSIXct(layer_labels[input$hour], tz = "UTC")
    
    format(
      lubridate::with_tz(t, "America/New_York"),
      "%b %d, %Y %I:%M %p ET"
    )
  })
  
  output$sfog_map <- renderLeaflet({
    
    leaflet() |>
      addProviderTiles(providers$CartoDB.Voyager) |>
      
      fitBounds(
        lng1 = -95,
        lat1 = 24,
        lng2 = -75,
        lat2 = 38
      ) |>
      
      addPolygons(
        data = r8_forests_sf,
        color = "darkgreen",
        weight = 1.2,
        opacity = 0.8,
        fillColor = "darkgreen",
        fillOpacity = 0.15,
        group = "Region 8 Forests"
      ) |>
      
      addRasterImage(
        sfog_ll[[1]],
        colors = pal,
        opacity = 0.7,
        project = FALSE,
        method = "ngb",
        group = "superfog"
      ) |>
      
      addControl(
        html = legend_html,
        position = "bottomright"
      )
  })
  
  observeEvent(input$prev_hour, {
    
    new_val <- max(1, input$hour - 1)
    
    updateSliderInput(
      session,
      "hour",
      value = new_val
    )
  })
  
  observeEvent(input$next_hour, {
    
    new_val <- min(terra::nlyr(sfog_ll), input$hour + 1)
    
    updateSliderInput(
      session,
      "hour",
      value = new_val
    )
  })
  
  
  observeEvent(input$hour, {
    leafletProxy("sfog_map") |>
      clearImages() |>
      addRasterImage(
        sfog_ll[[input$hour]],
        colors = pal,
        opacity = 0.7,
        project = FALSE,
        method = "ngb",
        group = "superfog"
      )
  })
}

shinyApp(ui, server)












