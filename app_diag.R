# ----------------------------
# 1. Packages and basic data
# ----------------------------

library(shiny)
library(leaflet)
library(terra)
library(sf)
library(htmltools)
library(lubridate)

cache_url <- "https://raw.githubusercontent.com/jeremyash/sfog_vis/main/cache/ndfd_superfog_cache.rds"

cache_file <- tempfile(fileext = ".rds")
download.file(cache_url, cache_file, mode = "wb")

cache <- readRDS(cache_file)

sfog_ll <- terra::unwrap(cache$sfog_ll)
r8_forests_sf <- cache$r8_forests_sf
layer_labels <- names(sfog_ll)
last_refresh <- cache$last_refresh

library(tigris)
library(dplyr)

options(tigris_use_cache = TRUE)

states_ref <- tigris::states(cb = TRUE, year = 2024) |>
  sf::st_transform(4326) |>
  dplyr::filter(STUSPS %in% c(
    "TX", "OK", "AR", "LA", "MS", "AL", "GA", "FL",
    "SC", "NC", "TN", "KY", "VA"
  ))

sfog_footprint <- sfog_ll[[1]]
sfog_footprint[!is.na(sfog_footprint)] <- 1

sfog_poly <- terra::as.polygons(
  sfog_footprint,
  dissolve = TRUE,
  na.rm = TRUE
) |>
  sf::st_as_sf() |>
  sf::st_transform(4326)

r8_outline <- sf::st_read(
  "r8_outline.gpkg",
  layer = "r8_outline",
  quiet = TRUE
) |>
  sf::st_transform(4326)


# ----------------------------
# 2. PLotting
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
  titlePanel("USFS Southern Area Superfog Risk"),
  
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
      # Slider
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
      ),
      
      br(),
      
      # -------------------------
      # Last refresh
      # -------------------------
      
      div(
        style = "
      font-size:12px;
      color:#555;
      text-align:center;
    ",
        textOutput("last_refresh")
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
  
  output$last_refresh <- renderText({
    paste(
      "Last updated:",
      format(
        lubridate::with_tz(last_refresh, "America/New_York"),
        "%b %d, %Y %I:%M %p ET"
      )
    )
  })
  
  output$sfog_map <- renderLeaflet({
    
    leaflet() |>
      addProviderTiles(providers$CartoDB.Voyager) |>
      
      fitBounds(
        lng1 = -97,
        lat1 = 17,
        lng2 = -64,
        lat2 = 40
      ) |>
      
      addPolygons(
        data = states_ref,
        fill = FALSE,
        color = "black",
        weight = 1,
        opacity = 0.8,
        group = "State boundaries"
      ) |>
      
      addPolygons(
        data = r8_outline,
        fill = FALSE,
        color = "red",
        weight = 2,
        opacity = 0.9,
        group = "Clipping outline"
      ) |>
      
      addPolygons(
        data = r8_forests_sf,
        color = "darkgreen",
        weight = 1.2,
        opacity = 0.8,
        fillColor = "darkgreen",
        fillOpacity = 0.15,
        group = "R8 Forests"
      ) |>
      
      addPolygons(
        data = sfog_poly,
        fill = TRUE,
        fillColor = "blue",
        fillOpacity = 0.15,
        color = "blue",
        weight = 1,
        group = "Final raster footprint"
      ) |>
      
      addRasterImage(
        sfog_ll[[1]],
        colors = pal,
        opacity = 0.7,
        project = FALSE,
        method = "ngb",
        group = "Superfog Risk"
      ) |>
      
      addLayersControl(
        overlayGroups = c(
          "Superfog Risk",
          "R8 Forests",
          "State boundaries",
          "Clipping outline",
          "Final raster footprint"
        ),
        options = layersControlOptions(collapsed = FALSE)
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
  
  observeEvent(input$hour, {
    leafletProxy("sfog_map") |>
      clearImages() |>
      addRasterImage(
        sfog_ll[[input$hour]],
        colors = pal,
        opacity = 0.7,
        project = FALSE,
        method = "ngb",
        group = "Superfog Risk"
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












