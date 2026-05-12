# ----------------------------
# 1. Packages and basic data
# ----------------------------

library(shiny)
library(leaflet)
library(terra)
library(sf)
library(htmltools)
library(lubridate)

cache_url <- "https://raw.githubusercontent.com/jeremyash/sfog_vis/cache-data/cache/ndfd_superfog_cache.rds"

cache_file <- tempfile(fileext = ".rds")
download.file(cache_url, cache_file, mode = "wb")

cache <- readRDS(cache_file)

# Analytical raster used for point/click extraction
sfog_ll <- terra::unwrap(cache$sfog_ll)

# PNG overlay URLs used for fast map display
sfog_png_urls <- unname(as.list(cache$sfog_png_urls))
sfog_png_bounds <- cache$sfog_png_bounds

r8_forests_sf <- readRDS("r8_forests_simplified.rds")
last_refresh <- cache$last_refresh

raw_times <- cache$valid_times

if (inherits(raw_times, "POSIXct") || inherits(raw_times, "POSIXt")) {
  layer_times <- raw_times
} else {
  layer_times <- suppressWarnings(lubridate::ymd_hms(
    gsub(" UTC$", "", as.character(raw_times)),
    tz = "UTC"
  ))
}

if (is.null(layer_times) || length(layer_times) != terra::nlyr(sfog_ll) || any(is.na(layer_times))) {
  layer_times <- suppressWarnings(lubridate::ymd_hms(
    gsub(" UTC$", "", as.character(names(sfog_ll))),
    tz = "UTC"
  ))
}

if (is.null(layer_times) || length(layer_times) != terra::nlyr(sfog_ll) || any(is.na(layer_times))) {
  layer_times <- seq(
    from = lubridate::floor_date(Sys.time(), "hour"),
    by = "1 hour",
    length.out = terra::nlyr(sfog_ll)
  )
}

n_layers <- terra::nlyr(sfog_ll)

# ----------------------------
# 2. Time helpers
# ----------------------------

parse_time_safe <- function(x, tz = "UTC") {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(x)
  
  x_chr <- as.character(x)
  
  parsed <- suppressWarnings(as.POSIXct(x_chr, tz = tz, format = "%Y-%m-%d %H:%M:%S"))
  
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(as.POSIXct(x_chr, tz = tz, format = "%Y-%m-%d %H:%M"))
  }
  
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(lubridate::ymd_hms(
      gsub(" UTC$| EST$| EDT$", "", x_chr),
      tz = tz
    ))
  }
  
  parsed
}

format_time_et <- function(x, fallback = NULL) {
  t <- parse_time_safe(x, tz = "UTC")
  
  if (length(t) == 0 || is.na(t[1])) {
    if (!is.null(fallback)) return(as.character(fallback))
    return(as.character(x))
  }
  
  format(
    lubridate::with_tz(t[1], "America/New_York"),
    "%b %d, %Y %I:%M %p ET"
  )
}

# ----------------------------
# 3. Plotting objects
# ----------------------------

risk_colors <- c(
  "1" = "#58AFDD",  # Minimal
  "2" = "#FFB000",  # Moderate
  "3" = "#CA0020"   # High
)

legend_html <- HTML('
<div style="background:white; padding:10px; border-radius:6px;">
  <div style="font-weight:bold; margin-bottom:6px;">Superfog Risk</div>
  <div><span style="background:#58AFDD; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> Minimal</div>
  <div><span style="background:#FFB000; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> Moderate</div>
  <div><span style="background:#CA0020; width:14px; height:14px; display:inline-block; border:1px solid #777;"></span> High</div>
</div>
')

# ----------------------------
# 4. UI
# ----------------------------

addResourcePath("favicon", "www")

sfog_overlay_js <- "
Shiny.addCustomMessageHandler('sfog_set_overlay', function(data) {
  var widgets = HTMLWidgets.findAll('.leaflet');
  if (!widgets.length) return;

  var map = null;
  for (var i = 0; i < widgets.length; i++) {
    if (widgets[i].getMap) {
      map = widgets[i].getMap();
      break;
    }
  }
  if (!map) return;

  if (window.sfogRiskOverlay && map.hasLayer(window.sfogRiskOverlay)) {
    map.removeLayer(window.sfogRiskOverlay);
  }

  var bounds = [
    [data.south, data.west],
    [data.north, data.east]
  ];

  window.sfogRiskOverlay = L.imageOverlay(data.url, bounds, {
    opacity: 0.7,
    className: 'sfog-png-overlay'
  }).addTo(map);
});
"

ui <- fluidPage(
  titlePanel("USFS Southern Area Superfog Risk"),
  
  tags$head(
    tags$script(HTML(sfog_overlay_js)),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon/favicon.ico?v=8"),
    tags$link(rel = "shortcut icon", type = "image/x-icon", href = "favicon/favicon.ico?v=8"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon/favicon-32x32.png?v=8"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon/favicon-16x16.png?v=8"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "favicon/apple-touch-icon.png?v=8")
  ),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width = 2, actionButton("prev_hour", label = NULL, icon = icon("chevron-left"), width = "100%")),
        column(
          width = 8,
          div(
            style = "display:flex; align-items:center; justify-content:center; height:38px; font-weight:bold; font-size:22px;",
            textOutput("valid_time")
          )
        ),
        column(width = 2, actionButton("next_hour", label = NULL, icon = icon("chevron-right"), width = "100%"))
      ),
      
      br(),
      
      sliderInput(
        inputId = "hour",
        label = NULL,
        min = 1,
        max = n_layers,
        value = 1,
        step = 1,
        animate = animationOptions(interval = 900, loop = TRUE)
      ),
      
      br(),
      
      div(style = "font-size:12px; color:#555; text-align:center;", textOutput("last_refresh")),
      
      hr(),
      
      h4("Point Risk Time Series"),
      p("Enter a latitude/longitude or click the map."),
      
      textInput("query_lat", "Latitude", value = "", placeholder = "e.g. 35.5951"),
      textInput("query_lon", "Longitude", value = "", placeholder = "e.g. -82.5515"),
      
      actionButton("extract_point", "Plot Point Risk", width = "100%")
    ),
    
    mainPanel(
      div(
        style = "position:relative;",
        
        actionButton(
          "reset_map_view",
          "Reset Map View",
          style = "position:absolute; top:10px; right:10px; z-index:1000; background:white; border:2px solid rgba(0,0,0,0.2); border-radius:4px; padding:6px 10px; font-size:13px; font-weight:600; cursor:pointer; box-shadow:0 1px 4px rgba(0,0,0,0.3);"
        ),
        
        leafletOutput("sfog_map", height = "650px")
      ),
      br(),
      plotOutput("point_risk_plot", height = "280px")
    )
  )
)

# ----------------------------
# 5. Server
# ----------------------------

server <- function(input, output, session) {
  
  selected_point <- reactiveVal(NULL)
  map_layers_added <- reactiveVal(FALSE)
  
  output$valid_time <- renderText({
    req(input$hour)
    
    format(
      lubridate::with_tz(layer_times[input$hour], "America/New_York"),
      "%b %d, %Y %I:%M %p %Z"
    )
  })
  
  output$last_refresh <- renderText({
    paste("Last updated:", format_time_et(last_refresh, fallback = last_refresh))
  })
  
  output$sfog_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      fitBounds(lng1 = -96, lat1 = 24, lng2 = -74, lat2 = 38)
  })
  
  set_sfog_overlay <- function(hour_index) {
    session$sendCustomMessage(
      type = "sfog_set_overlay",
      message = list(
        url = sfog_png_urls[[hour_index]],
        west = sfog_png_bounds$west,
        south = sfog_png_bounds$south,
        east = sfog_png_bounds$east,
        north = sfog_png_bounds$north
      )
    )
  }
  
  observeEvent(input$sfog_map_bounds, {
    req(!map_layers_added())
    
    leafletProxy("sfog_map") |>
      addPolygons(
        data = r8_forests_sf,
        color = "darkgreen",
        weight = 1.2,
        opacity = 0.8,
        fillColor = "darkgreen",
        fillOpacity = 0.15,
        group = "Region 8 Forests"
      ) |>
      addControl(
        html = legend_html,
        position = "bottomright",
        layerId = "sfog_legend"
      )
    
    set_sfog_overlay(input$hour)
    map_layers_added(TRUE)
  }, ignoreInit = FALSE)
  
  observeEvent(input$hour, {
    req(input$hour)
    req(map_layers_added())
    set_sfog_overlay(input$hour)
  }, ignoreInit = TRUE)
  
  observeEvent(input$prev_hour, {
    updateSliderInput(session, "hour", value = max(1, input$hour - 1))
  })
  
  observeEvent(input$next_hour, {
    updateSliderInput(session, "hour", value = min(n_layers, input$hour + 1))
  })
  
  observeEvent(input$reset_map_view, {
    selected_point(NULL)
    
    updateTextInput(session, "query_lat", value = "")
    updateTextInput(session, "query_lon", value = "")
    
    leafletProxy("sfog_map") |>
      clearGroup("Point Query") |>
      fitBounds(lng1 = -96, lat1 = 24, lng2 = -74, lat2 = 38)
  })
  
  observeEvent(input$extract_point, {
    lat <- as.numeric(input$query_lat)
    lon <- as.numeric(input$query_lon)
    
    selected_point(list(lat = lat, lon = lon, source = "manual"))
  })
  
  observeEvent(input$sfog_map_click, {
    lat <- input$sfog_map_click$lat
    lon <- input$sfog_map_click$lng
    
    updateTextInput(session, "query_lat", value = round(lat, 5))
    updateTextInput(session, "query_lon", value = round(lon, 5))
    
    selected_point(list(lat = lat, lon = lon, source = "map"))
  })
  
  point_risk <- reactive({
    pt_info <- selected_point()
    req(pt_info)
    
    lat <- pt_info$lat
    lon <- pt_info$lon
    
    validate(
      need(!is.na(lat), "Please enter a valid latitude."),
      need(!is.na(lon), "Please enter a valid longitude.")
    )
    
    pt <- data.frame(lon = lon, lat = lat)
    pt_v <- terra::vect(pt, geom = c("lon", "lat"), crs = "EPSG:4326")
    
    inside_domain <- !is.na(terra::extract(sfog_ll[[1]], pt_v)[1, 2])
    validate(need(inside_domain, "Location is outside of the Southern Area."))
    
    vals <- terra::extract(sfog_ll, pt_v)
    risk_vals <- as.numeric(vals[1, -1])
    
    leafletProxy("sfog_map") |>
      clearGroup("Point Query") |>
      addMarkers(
        lng = lon,
        lat = lat,
        group = "Point Query",
        label = paste0("Point Query: ", round(lat, 4), ", ", round(lon, 4))
      ) |>
      fitBounds(lng1 = lon - 0.5, lat1 = lat - 0.5, lng2 = lon + 0.5, lat2 = lat + 0.5)
    
    times_utc <- layet_times
    
    data.frame(
      time_utc = times_utc,
      time_et = lubridate::with_tz(times_utc, "America/New_York"),
      risk = risk_vals,
      lat = lat,
      lon = lon
    )
  })
  
  output$point_risk_plot <- renderPlot({
    df <- point_risk()
    point_cols <- risk_colors[as.character(df$risk)]
    
    par(mar = c(6, 6.5, 4, 6) + 0.1, xpd = TRUE)
    
    plot(
      df$time_et,
      df$risk,
      type = "l",
      lwd = 2,
      col = "#666666",
      ylim = c(0.8, 3.2),
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      main = paste0(
        "Superfog Risk at ",
        round(df$lat[1], 4),
        ", ",
        round(df$lon[1], 4)
      )
    )
    
    axis(
      side = 2,
      at = c(1, 2, 3),
      labels = c("Minimal", "Moderate", "High"),
      las = 1,
      tick = TRUE,
      cex.axis = 0.95
    )
    
    points(df$time_et, df$risk, pch = 21, bg = point_cols, col = "#333333", cex = 2.1, lwd = 1.2)
    
    axis.POSIXct(side = 1, x = df$time_et, format = "%m/%d\n%H:%M", las = 2)
  })
}

shinyApp(ui, server)
