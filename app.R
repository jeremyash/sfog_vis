# ----------------------------
# 1. Packages and basic data
# ----------------------------

library(shiny)
library(leaflet)
library(terra)
library(sf)
library(htmltools)
library(lubridate)
library(htmlwidgets)

cache_url <- "https://raw.githubusercontent.com/jeremyash/sfog_vis/cache-data/cache/ndfd_superfog_cache.rds"

cache_file <- tempfile(fileext = ".rds")
download.file(cache_url, cache_file, mode = "wb")

cache <- readRDS(cache_file)
# cache <- readRDS("cache/ndfd_superfog_cache.rds")

sfog_ll <- terra::unwrap(cache$sfog_ll)
r8_forests_sf <- cache$r8_forests_sf
layer_labels <- names(sfog_ll)
last_refresh <- cache$last_refresh


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
                    display:flex;
                    align-items:center;
                    justify-content:center;
                    height:38px;
                    font-weight:bold;
                    font-size:22px;
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
        lng1 = -96,
        lat1 = 24,
        lng2 = -74,
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
        project = TRUE,
        method = "ngb",
        group = "Superfog Risk",
        maxBytes = 50 * 1024 * 1024
      ) |>
      
      addControl(
        html = HTML('
        <button id="reset_map_view" 
          style="
            background:white;
            border:2px solid rgba(0,0,0,0.2);
            border-radius:4px;
            padding:6px 10px;
            font-size:13px;
            font-weight:600;
            cursor:pointer;
            box-shadow:0 1px 4px rgba(0,0,0,0.3);
          ">
          Reset Map View
        </button>
      '),
        position = "topright"
      ) |>
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          setTimeout(function() {
            var btn = document.getElementById('reset_map_view');
            if (btn) {
              btn.onclick = function() {
                map.fitBounds([
                  [24, -96],
                  [38, -74]
                ]);
              };
            }
          }, 100);
        }
      ") |>
      
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
        project = TRUE,
        method = "ngb",
        group = "Superfog Risk",
        maxBytes = 50 * 1024 * 1024
      )
  }, ignoreInit = TRUE)
}


shinyApp(ui, server)












