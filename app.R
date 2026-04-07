suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(leaflet)
  library(DT)
  library(sf)
  library(scales)
})

# source("R/simulate_data.R")
source("/Users/marvinguth/Documents/neue_paper_2025/Colloquium Ambulatory Assessment /R/simulate_data.R")

build_palette <- function(values) {
  pal <- colorNumeric(
    palette = c("#0b3c5d", "#328cc1", "#d9b310", "#b01c1c"),
    domain = values,
    na.color = "#999999"
  )
  pal
}

var_choices <- c(
  "Speed\n(km/h)" = "speed_kmh",
  "NDVI\n(100m)" = "ndvi_mean_100m",
  "Imperviousness\n(100m)" = "imperviousness_mean_100m",
  "Population\n(100m)" = "population",
  "Distance to\nhome" = "distance_to_home",
  "Crowded\narea" = "crowded_area"
)

day_metric_choices <- c(
  "Cumulative\ndistance (day)" = "cumulative_distance_day",
  "Minutes slow\n(km/h day)" = "Minutes_slow_kmh_day",
  "Minutes fast\n(km/h day)" = "Minutes_fast_kmh_day",
  "Minutes stationary\n(day)" = "Minutes_Stationary_day",
  "Crowded area\nminutes (day)" = "crowded_area_minutes_day",
  "Steps\n(day)" = "steps_day",
  "Mean NDVI\n(100m day)" = "mean_ndvi_100m_day",
  "Mean imperviousness\n(100m day)" = "mean_imperviousness_100m_day"
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      body { background: linear-gradient(160deg, #f2efe8 0%, #fbf9f4 55%, #e6ece8 100%); font-family: 'Avenir Next', 'Helvetica Neue', sans-serif; }\n      .app-title { font-size: 30px; font-weight: 700; color: #1f2a33; margin-bottom: 4px; }\n      .app-sub { color: #415361; margin-bottom: 16px; }\n      .well { background: #ffffffcc; border: 1px solid #d9dfde; border-radius: 12px; }\n      .nav-tabs > li > a { color: #1f2a33; font-weight: 600; }\n      .selectize-control .selectize-input, .selectize-control .selectize-input input { white-space: pre-line !important; height: auto !important; }\n      .selectize-control .selectize-dropdown .option, .selectize-control .selectize-dropdown .optgroup-header { white-space: pre-line !important; }\n      .selectize-control .item { white-space: pre-line !important; }\n      .split-shell { display: flex; gap: 14px; align-items: stretch; }\n      .split-sidebar { flex: 0 0 320px; max-width: 38%; }\n      .split-main { flex: 1 1 auto; min-width: 0; }\n      .split-shell.is-collapsed .split-sidebar { display: none; }\n      .split-shell.is-collapsed .split-main { flex-basis: 100%; }\n      .split-card { background: #ffffffcc; border: 1px solid #d9dfde; border-radius: 12px; padding: 10px; }\n      .split-head { display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px; }\n      .split-title { font-weight: 700; color: #1f2a33; margin: 0; }\n      .expand-toggle { display: none; margin-bottom: 8px; }\n      .split-shell.is-collapsed .expand-toggle { display: inline-block; }\n      @media (max-width: 991px) {\n        .split-shell { display: block; }\n        .split-sidebar { max-width: 100%; margin-bottom: 10px; }\n      }\n    ")),
    tags$script(HTML("\n      function toggleSplit(id) {\n        var container = document.getElementById(id);\n        if (!container) return;\n        container.classList.toggle('is-collapsed');\n        setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 260);\n      }\n    "))
  ),
  div(class = "app-title", "Geospatial and intensive longitudinal methods for characterizing everyday contextual exposures"),
  div(class = "app-sub", "Shiny demo with simulated GPS data (Karlsruhe city center) and a simulated example raster"),
  tabsetPanel(
    tabPanel(
      "Minute-Level Map",
      div(
        id = "minute_shell",
        class = "split-shell",
        div(
          class = "split-sidebar split-card",
          div(
            class = "split-head",
            h4(class = "split-title", "Minute-Level Controls"),
            tags$button(
              type = "button",
              class = "btn btn-default btn-xs",
              onclick = "toggleSplit('minute_shell')",
              "Collapse"
            )
          ),
          actionButton("regen", "Resimulate 5000 points"),
          br(), br(),
          selectInput("user_filter", "User ID", choices = NULL),
          selectInput("var_filter", "Color variable", choices = var_choices),
          sliderInput("point_size", "Point size", min = 2, max = 10, value = 5),
          checkboxInput("show_buffers", "Show 100m buffer for clicked point", value = TRUE)
        ),
        div(
          class = "split-main",
          tags$button(
            type = "button",
            class = "btn btn-default btn-xs expand-toggle",
            onclick = "toggleSplit('minute_shell')",
            "Open controls"
          ),
          leafletOutput("minute_map", height = 560),
          br(),
          DTOutput("minute_table")
        )
      )
    ),
    tabPanel(
      "Day-Level Aggregation",
      div(
        id = "day_shell",
        class = "split-shell",
        div(
          class = "split-sidebar split-card",
          div(
            class = "split-head",
            h4(class = "split-title", "Day-Level Controls"),
            tags$button(
              type = "button",
              class = "btn btn-default btn-xs",
              onclick = "toggleSplit('day_shell')",
              "Collapse"
            )
          ),
          selectInput("day_user", "User ID", choices = NULL),
          selectInput("day_metric", "Day-level metric", choices = day_metric_choices)
        ),
        div(
          class = "split-main",
          tags$button(
            type = "button",
            class = "btn btn-default btn-xs expand-toggle",
            onclick = "toggleSplit('day_shell')",
            "Open controls"
          ),
          plotOutput("day_plot", height = 320),
          br(),
          plotOutput("calendar_plot", height = 250),
          br(),
          leafletOutput("day_map", height = 300),
          br(),
          DTOutput("day_table")
        )
      )
    ),
    tabPanel(
      "Methodology & Input Data",
      div(
        id = "method_shell",
        class = "split-shell",
        div(
          class = "split-sidebar split-card",
          div(
            class = "split-head",
            h4(class = "split-title", "Methodology Story"),
            tags$button(
              type = "button",
              class = "btn btn-default btn-xs",
              onclick = "toggleSplit('method_shell')",
              "Collapse"
            )
          ),
            tags$ol(
              tags$li("Simulated GPS points in Karlsruhe city center (5000 points, multiple IDs)."),
              tags$li("OSM-analog layers (shop, pedestrian, railway, department) are processed with geometry-specific buffers (50m/10m/50m/0m)."),
              tags$li("A 100m buffer is created around each GPS position and intersected with a simulated example raster surface."),
              tags$li("Minute-level features are aggregated into day-level indicators.")
            ),
            hr(),
            selectInput("method_user", "User ID", choices = NULL),
            hr(),
            h4("Example Raster"),
            textOutput("ndvi_source_info"),
            hr(),
            h4("Selected Point"),
            verbatimTextOutput("selected_point_info")
        ),
        div(
          class = "split-main",
          tags$button(
            type = "button",
            class = "btn btn-default btn-xs expand-toggle",
            onclick = "toggleSplit('method_shell')",
            "Open controls"
          ),
          leafletOutput("method_map", height = 650)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  sim_data <- reactiveVal(simulate_apac_like_data(n_points = 5000, n_users = 25, seed = 42))
  selected_idx <- reactiveVal(1)

  observeEvent(input$regen, {
    sim_data(simulate_apac_like_data(n_points = 5000, n_users = 25, seed = sample.int(10000, 1)))
  })

  observe({
    users <- sort(unique(sim_data()$minute_data$userId))
    updateSelectInput(session, "user_filter", choices = users, selected = users[1])
    updateSelectInput(session, "method_user", choices = users, selected = users[1])
    updateSelectInput(session, "day_user", choices = users, selected = users[1])
  })

  filtered_minute <- reactive({
    req(input$user_filter)
    sim_data()$minute_data |>
      filter(userId == input$user_filter)
  })

  output$minute_map <- renderLeaflet({
    df <- filtered_minute()
    df <- df |>
      mutate(
        metric_value = .data[[input$var_filter]],
        row_id = seq_len(n())
      )
    pal <- build_palette(df$metric_value)

    leaflet(df) |>
      addProviderTiles("CartoDB.Positron") |>
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = input$point_size,
        stroke = FALSE,
        fillOpacity = 0.75,
        color = ~pal(metric_value),
        layerId = ~row_id,
        popup = ~paste0(
          "<b>", userId, "</b><br>",
          "Time: ", timestamp_minute, "<br>",
          "Speed: ", round(speed_kmh, 2), " km/h<br>",
          "NDVI: ", round(ndvi_mean_100m, 3), "<br>",
          "Crowded: ", crowded_area
        )
      ) |>
        addLegend("bottomright", pal = pal, values = df$metric_value, title = input$var_filter, opacity = 0.9)
  })

  observeEvent(input$minute_map_marker_click, {
    click <- input$minute_map_marker_click
    if (!is.null(click$id)) {
      selected_idx(as.integer(click$id))
    }
  })

  observe({
    req(input$show_buffers)
    df <- filtered_minute()
    idx <- selected_idx()
    if (idx < 1 || idx > nrow(df)) {
      return(NULL)
    }

    pt <- df[idx, ]
    pt_sf <- st_as_sf(pt, coords = c("longitude", "latitude"), crs = 4326)
    pt_buf <- st_transform(pt_sf, 25832) |> st_buffer(100) |> st_transform(4326)

    leafletProxy("minute_map") |>
      clearGroup("buffer") |>
      addPolygons(data = pt_buf, color = "#1f2a33", weight = 2, fillOpacity = 0.08, group = "buffer")
  })

  output$minute_table <- renderDT({
    filtered_minute() |>
      select(userId, timestamp_minute, speed_kmh, crowded_area, ndvi_mean_100m, imperviousness_mean_100m, population, distance_to_home) |>
      arrange(desc(timestamp_minute)) |>
      head(250)
  }, options = list(pageLength = 10, scrollX = TRUE))

  output$day_plot <- renderPlot({
    req(input$day_user, input$day_metric)

    df <- sim_data()$day_data |>
      filter(userId == input$day_user) |>
      mutate(metric_value = .data[[input$day_metric]])

    ggplot(df, aes(x = date, y = metric_value)) +
      geom_line(color = "#1f2a33", linewidth = 1) +
      geom_point(color = "#d95f02", size = 2) +
      theme_minimal(base_family = "Avenir Next") +
      labs(x = "Date", y = input$day_metric, title = paste("Daily trajectory:", input$day_metric))
  })

  output$calendar_plot <- renderPlot({
    req(input$day_user, input$day_metric)

    cal_df <- sim_data()$day_data |>
      filter(userId == input$day_user) |>
      mutate(
        metric_value = .data[[input$day_metric]],
        week_start = lubridate::floor_date(date, unit = "week", week_start = 1),
        weekday = factor(
          lubridate::wday(date, week_start = 1),
          levels = 1:7,
          labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
        )
      )

    ggplot(cal_df, aes(x = week_start, y = weekday, fill = metric_value)) +
      geom_tile(color = "white", linewidth = 0.7) +
      scale_fill_gradientn(colours = c("#f7fcf5", "#74c476", "#00441b"), name = input$day_metric) +
      theme_minimal(base_family = "Avenir Next") +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position = "right"
      ) +
      labs(title = "Calendar heatmap")
  })

  output$day_map <- renderLeaflet({
    day_df <- sim_data()$day_data |>
      filter(userId == input$day_user)

    day_pts <- sim_data()$minute_data |>
      filter(userId == input$day_user) |>
      group_by(date) |>
      summarise(longitude = mean(longitude), latitude = mean(latitude), .groups = "drop") |>
      left_join(day_df, by = "date") |>
      mutate(metric_value = .data[[input$day_metric]])

    pal <- colorNumeric(c("#f7fcf5", "#00441b"), day_pts$metric_value)

    leaflet(day_pts) |>
      addProviderTiles("CartoDB.Positron") |>
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        color = ~pal(metric_value),
        stroke = FALSE,
        fillOpacity = 0.85,
        popup = ~paste0("Date: ", date, "<br>", input$day_metric, ": ", round(metric_value, 2))
      ) |>
      addLegend("bottomright", pal = pal, values = day_pts$metric_value, title = input$day_metric)
  })

  output$day_table <- renderDT({
    sim_data()$day_data |>
      filter(userId == input$day_user) |>
      arrange(desc(date))
  }, options = list(pageLength = 12, scrollX = TRUE))

  output$ndvi_source_info <- renderText({
    "Example Raster (e.g. population density or NDVI)"
  })

  output$method_map <- renderLeaflet({
    req(input$method_user)
    layers <- sim_data()$layers
    pts <- sim_data()$minute_data |>
      filter(userId == input$method_user)
    ndvi_grid <- layers$ndvi_grid
    shops_buf <- st_transform(layers$shops, 25832) |> st_buffer(50) |> st_transform(4326)
    pedestrian_buf <- st_transform(layers$pedestrian, 25832) |> st_buffer(10) |> st_transform(4326)
    railway_buf <- st_transform(layers$railway, 25832) |> st_buffer(50) |> st_transform(4326)

    gps_points_sf <- st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    gps_buffers <- st_transform(gps_points_sf, 25832) |> st_buffer(100) |> st_transform(4326)
    gps_union <- st_union(gps_buffers)
    raster_intersections <- suppressWarnings(st_intersection(ndvi_grid, gps_union))
    shop_intersections <- suppressWarnings(st_intersection(layers$shops, gps_union))
    pedestrian_intersections <- suppressWarnings(st_intersection(layers$pedestrian, gps_union))
    railway_intersections <- suppressWarnings(st_intersection(layers$railway, gps_union))
    department_intersections <- suppressWarnings(st_intersection(layers$department, gps_union))

    map_obj <- leaflet() |> addProviderTiles("CartoDB.Positron")
    map_obj <- addPolygons(
      map_obj,
      data = ndvi_grid,
      fillColor = ~colorNumeric(c("#f7fcf5", "#006d2c"), ndvi)(ndvi),
      fillOpacity = 0.35,
      color = "#b4c3b7",
      weight = 0.4,
      group = "Example Raster (e.g. population density or NDVI)"
    )

    map_obj |>
      addCircleMarkers(data = pts, lng = ~longitude, lat = ~latitude, radius = 2, color = "#1f2a33", opacity = 0.4, group = "GPS Points") |>
      addPolygons(data = gps_buffers, color = "#1f2a33", weight = 1, fillOpacity = 0.05, group = "GPS Buffer Sample (100m)") |>
      addPolygons(data = raster_intersections, color = "#c1121f", weight = 1.5, fillColor = "#f77f00", fillOpacity = 0.45, group = "GPS x Example Raster Intersects") |>
      addCircleMarkers(data = shop_intersections, radius = 5, color = "#9b2226", stroke = FALSE, fillOpacity = 0.9, group = "GPS x Shop Intersects") |>
      addPolylines(data = pedestrian_intersections, color = "#1d3557", weight = 3, opacity = 0.95, group = "GPS x Pedestrian Intersects") |>
      addCircleMarkers(data = railway_intersections, radius = 5, color = "#005f73", stroke = FALSE, fillOpacity = 0.9, group = "GPS x Railway Intersects") |>
      addPolygons(data = department_intersections, color = "#9d0208", weight = 2, fillColor = "#dc2f02", fillOpacity = 0.5, group = "GPS x Department Intersects") |>
      addPolygons(data = shops_buf, color = "#6f1d1b", weight = 1, fillOpacity = 0.12, group = "Shop Buffer (50m)") |>
      addPolygons(data = pedestrian_buf, color = "#4a4e69", weight = 1, fillOpacity = 0.12, group = "Pedestrian Buffer (10m)") |>
      addPolygons(data = railway_buf, color = "#005f73", weight = 1, fillOpacity = 0.12, group = "Railway Buffer (50m)") |>
      addCircleMarkers(data = layers$shops, radius = 3, color = "#6f1d1b", stroke = FALSE, fillOpacity = 0.7, group = "OSM Shop Points") |>
      addPolylines(data = layers$pedestrian, color = "#4a4e69", weight = 2, group = "OSM Pedestrian Lines") |>
      addCircleMarkers(data = layers$railway, radius = 4, color = "#005f73", stroke = FALSE, fillOpacity = 0.8, group = "OSM Railway Points") |>
      addPolygons(data = layers$department, color = "#9b2226", weight = 1.5, fillOpacity = 0.2, group = "OSM Department Polygons") |>
      addLayersControl(
        overlayGroups = c(
          "GPS Points",
          "GPS Buffer Sample (100m)",
          "Example Raster (e.g. population density or NDVI)",
          "GPS x Example Raster Intersects",
          "OSM Shop Points",
          "GPS x Shop Intersects",
          "OSM Pedestrian Lines",
          "GPS x Pedestrian Intersects",
          "OSM Railway Points",
          "GPS x Railway Intersects",
          "OSM Department Polygons",
          "GPS x Department Intersects",
          "Shop Buffer (50m)",
          "Pedestrian Buffer (10m)",
          "Railway Buffer (50m)"
        ),
        options = layersControlOptions(collapsed = TRUE)
      ) |>
      hideGroup("GPS Buffer Sample (100m)") |>
      hideGroup("Example Raster (e.g. population density or NDVI)") |>
      hideGroup("GPS x Example Raster Intersects") |>
      hideGroup("GPS x Shop Intersects") |>
      hideGroup("GPS x Pedestrian Intersects") |>
      hideGroup("GPS x Railway Intersects") |>
      hideGroup("GPS x Department Intersects") |>
      hideGroup("Shop Buffer (50m)") |>
      hideGroup("Pedestrian Buffer (10m)") |>
      hideGroup("Railway Buffer (50m)") |>
      hideGroup("OSM Shop Points") |>
      hideGroup("OSM Pedestrian Lines") |>
      hideGroup("OSM Railway Points") |>
      hideGroup("OSM Department Polygons")
  })

  observe({
    df <- filtered_minute()
    idx <- selected_idx()
    if (idx < 1 || idx > nrow(df)) {
      output$selected_point_info <- renderText("No point selected")
      return(NULL)
    }

    point <- df[idx, ]
    output$selected_point_info <- renderText({
      paste0(
        "userId: ", point$userId, "\n",
        "timestamp_minute: ", point$timestamp_minute, "\n",
        "speed_kmh: ", round(point$speed_kmh, 2), "\n",
        "distance_to_home: ", round(point$distance_to_home, 1), " m\n",
        "ndvi_mean_100m: ", round(point$ndvi_mean_100m, 3), "\n",
        "imperviousness_mean_100m: ", round(point$imperviousness_mean_100m, 1), "\n",
        "population: ", round(point$population, 0), "\n",
        "crowded_area: ", point$crowded_area, " (shop=", point$shop,
        ", pedestrian=", point$pedestrian,
        ", railway=", point$railway,
        ", department=", point$department, ")"
      )
    })
  })
}

shinyApp(ui, server)
