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

if (is.null(layer_labels) || any(layer_labels == "")) {
  layer_labels <- paste("Forecast hour", seq_len(terra::nlyr(sfog_ll)))
}

# ----------------------------
# 2. Plotting objects
# ----------------------------

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

pal <- leaflet::colorFactor(
  palette = risk_colors,
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

# ----------------------------
# 3. UI
# ----------------------------

ui <- fluidPage(
  titlePanel("USFS Southern Area Superfog Risk"),
  
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "superfog-favicon.svg?v=1"),
    tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "superfog-favicon-512x512.png?v=1"),
    tags$link(rel = "icon", type = "image/png", sizes = "192x192", href = "android-chrome-192x192.png?v=1"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png?v=1"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png?v=1"),
    tags$link(rel = "shortcut icon", href = "favicon.ico?v=1"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.png?v=1")
  ),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          width = 2,
          actionButton("prev_hour", label = NULL, icon = icon("chevron-left"), width = "100%")
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
          actionButton("next_hour", label = NULL, icon = icon("chevron-right"), width = "100%")
        )
      ),
      
      br(),
      
      sliderInput(
        inputId = "hour",
        label = NULL,
        min = 1,
        max = terra::nlyr(sfog_ll),
        value = 1,
        step = 1,
        animate = animationOptions(interval = 900, loop = TRUE)
      ),
      
      br(),
      
      div(
        style = "font-size:12px; color:#555; text-align:center;",
        textOutput("last_refresh")
      ),
      
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
          style = "
            position:absolute;
            top:10px;
            right:10px;
            z-index:1000;
            background:white;
            border:2px solid rgba(0,0,0,0.2);
            border-radius:4px;
            padding:6px 10px;
            font-size:13px;
            font-weight:600;
            cursor:pointer;
            box-shadow:0 1px 4px rgba(0,0,0,0.3);
          "
        ),
        
        leafletOutput("sfog_map", height = "650px")
      ),
      br(),
      plotOutput("point_risk_plot", height = "280px")
    )
  )
)

# ----------------------------
# 4. Server
# ----------------------------

server <- function(input, output, session) {
  
  selected_point <- reactiveVal(NULL)
  
  output$valid_time <- renderText({
    t <- as.POSIXct(layer_labels[input$hour], tz = "UTC")
    format(lubridate::with_tz(t, "America/New_York"), "%b %d, %Y %I:%M %p ET")
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
      fitBounds(lng1 = -96, lat1 = 24, lng2 = -74, lat2 = 38) |>
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
      addControl(html = legend_html, position = "bottomright")
  })
  
  observeEvent(input$prev_hour, {
    updateSliderInput(session, "hour", value = max(1, input$hour - 1))
  })
  
  observeEvent(input$next_hour, {
    updateSliderInput(session, "hour", value = min(terra::nlyr(sfog_ll), input$hour + 1))
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
  
  observeEvent(input$reset_map_view, {
    selected_point(NULL)
    
    updateTextInput(session, "query_lat", value = "")
    updateTextInput(session, "query_lon", value = "")
    
    leafletProxy("sfog_map") |>
      clearGroup("Point Query") |>
      fitBounds(
        lng1 = -96,
        lat1 = 24,
        lng2 = -74,
        lat2 = 38
      )
  })
  
  observeEvent(input$extract_point, {
    lat <- as.numeric(input$query_lat)
    lon <- as.numeric(input$query_lon)
    
    selected_point(list(
      lat = lat,
      lon = lon,
      source = "manual"
    ))
  })
  
  observeEvent(input$sfog_map_click, {
    lat <- input$sfog_map_click$lat
    lon <- input$sfog_map_click$lng
    
    updateTextInput(session, "query_lat", value = round(lat, 5))
    updateTextInput(session, "query_lon", value = round(lon, 5))
    
    selected_point(list(
      lat = lat,
      lon = lon,
      source = "map"
    ))
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
    
    pt_v <- terra::vect(
      pt,
      geom = c("lon", "lat"),
      crs = "EPSG:4326"
    )
    
    inside_domain <- !is.na(
      terra::extract(sfog_ll[[1]], pt_v)[1, 2]
    )
    
    validate(
      need(inside_domain, "Location is outside of the Southern Area.")
    )
    
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
      fitBounds(
        lng1 = lon - 0.5,
        lat1 = lat - 0.5,
        lng2 = lon + 0.5,
        lat2 = lat + 0.5
      )
    
    times_utc <- as.POSIXct(
      names(sfog_ll),
      tz = "UTC",
      format = "%Y-%m-%d %H:%M"
    )
    
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
    
    par(mar = c(6, 4, 4, 6) + 0.1, xpd = TRUE)
    
    plot(
      df$time_et,
      df$risk,
      type = "l",
      lwd = 2,
      col = "#666666",
      ylim = c(0, 8.4),
      xaxt = "n",
      xlab = "Valid time",
      ylab = "Superfog Risk",
      main = paste0(
        "Superfog Risk at ",
        round(df$lat[1], 4),
        ", ",
        round(df$lon[1], 4)
      )
    )
    
    points(
      df$time_et,
      df$risk,
      pch = 21,
      bg = point_cols,
      col = "#333333",
      cex = 2.1,
      lwd = 1.2
    )
    
    axis.POSIXct(
      side = 1,
      x = df$time_et,
      format = "%m/%d\n%H:%M",
      las = 2
    )
    
    par(xpd = FALSE)
    
    abline(h = 4, lty = 2, col = "#FFDA00", lwd = 2)
    abline(h = 8, lty = 2, col = "#CA0020", lwd = 2)
    
    par(xpd = NA)
    
    usr <- par("usr")
    
    text(
      x = usr[2] + 0.008 * diff(usr[1:2]),
      y = 4,
      labels = "Watchout",
      pos = 4,
      col = "#B38F00",
      cex = 1.15,
      font = 2
    )
    
    text(
      x = usr[2] + 0.008 * diff(usr[1:2]),
      y = 8,
      labels = "Critical",
      pos = 4,
      col = "#CA0020",
      cex = 1.15,
      font = 2
    )
    
    par(xpd = FALSE)
  })
}

shinyApp(ui, server)