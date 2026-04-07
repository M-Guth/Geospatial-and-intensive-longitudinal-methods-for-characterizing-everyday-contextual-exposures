suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(sf)
  library(tidyr)
  library(purrr)
})

clip_range <- function(x, min_val, max_val) {
  pmax(min_val, pmin(max_val, x))
}

haversine_dist <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  r <- 6371000
  dlon <- (lon2 - lon1) * rad
  dlat <- (lat2 - lat1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

nearest_hotspot <- function(point_lon, point_lat, hotspot_lon, hotspot_lat) {
  n <- length(point_lon)
  m <- length(hotspot_lon)
  dist_matrix <- matrix(NA_real_, nrow = n, ncol = m)
  for (j in seq_len(m)) {
    dist_matrix[, j] <- haversine_dist(point_lon, point_lat, hotspot_lon[j], hotspot_lat[j])
  }
  nearest_idx <- max.col(-dist_matrix, ties.method = "first")
  nearest_dist <- dist_matrix[cbind(seq_len(n), nearest_idx)]
  list(idx = nearest_idx, dist = nearest_dist)
}

make_method_layers <- function(seed = 42) {
  set.seed(seed)

  bbox <- list(
    xmin = 8.38,
    xmax = 8.428,
    ymin = 48.995,
    ymax = 49.032
  )

  ndvi_grid <- st_as_sfc(
    st_bbox(c(
      xmin = bbox$xmin,
      xmax = bbox$xmax,
      ymin = bbox$ymin,
      ymax = bbox$ymax
    ), crs = 4326)
  ) |>
    st_make_grid(n = c(25, 20), what = "polygons") |>
    st_as_sf() |>
    mutate(cell_id = row_number())

  centroids <- st_coordinates(st_centroid(ndvi_grid))
  ndvi_grid <- ndvi_grid |>
    mutate(
      ndvi = clip_range(
        0.45 - (abs(centroids[, 1] - 8.404) * 8) + rnorm(n(), 0, 0.08),
        -0.1,
        0.9
      ),
      imperviousness = clip_range(100 - (ndvi * 95) + rnorm(n(), 0, 7), 5, 100),
      population = clip_range(1500 + (imperviousness * 65) + rnorm(n(), 0, 400), 300, 12000)
    )

  shops <- tibble(
    lon = runif(120, bbox$xmin, bbox$xmax),
    lat = runif(120, bbox$ymin, bbox$ymax)
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    mutate(layer = "shop")

  ped_lines <- map_dfr(1:20, function(i) {
    x0 <- runif(1, bbox$xmin, bbox$xmax - 0.01)
    y0 <- runif(1, bbox$ymin, bbox$ymax - 0.01)
    x1 <- x0 + runif(1, 0.004, 0.012)
    y1 <- y0 + runif(1, -0.004, 0.01)
    tibble(id = i, x0 = x0, y0 = y0, x1 = x1, y1 = y1)
  })

  ped_geom <- lapply(seq_len(nrow(ped_lines)), function(i) {
    st_linestring(matrix(c(
      ped_lines$x0[i], ped_lines$y0[i],
      ped_lines$x1[i], ped_lines$y1[i]
    ), ncol = 2, byrow = TRUE))
  })

  pedestrian <- st_sf(
    layer = "pedestrian",
    geometry = st_sfc(ped_geom, crs = 4326)
  )

  railway <- tibble(
    lon = runif(15, 8.394, 8.418),
    lat = runif(15, 49.002, 49.025)
  ) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    mutate(layer = "railway")

  dept_boxes <- tibble(
    lon = runif(8, 8.39, 8.42),
    lat = runif(8, 49.0, 49.028),
    dx = runif(8, 0.0008, 0.0018),
    dy = runif(8, 0.0006, 0.0014)
  )

  dept_geom <- lapply(seq_len(nrow(dept_boxes)), function(i) {
    x <- dept_boxes$lon[i]
    y <- dept_boxes$lat[i]
    dx <- dept_boxes$dx[i]
    dy <- dept_boxes$dy[i]
    st_polygon(list(matrix(c(
      x - dx, y - dy,
      x + dx, y - dy,
      x + dx, y + dy,
      x - dx, y + dy,
      x - dx, y - dy
    ), ncol = 2, byrow = TRUE)))
  })

  department <- st_sf(
    layer = "department",
    geometry = st_sfc(dept_geom, crs = 4326)
  )

  list(
    bbox = bbox,
    ndvi_grid = ndvi_grid,
    shops = shops,
    pedestrian = pedestrian,
    railway = railway,
    department = department
  )
}

simulate_apac_like_data <- function(
    n_points = 5000,
    n_users = 25,
    seed = 42
) {
  set.seed(seed)

  layers <- make_method_layers(seed = seed)
  bbox <- layers$bbox

  users <- tibble(
    userId = sprintf("P%03d", seq_len(n_users)),
    home_lon = runif(n_users, 8.388, 8.424),
    home_lat = runif(n_users, 48.998, 49.029)
  )

  base <- tibble(
    idx = seq_len(n_points),
    userId = sample(users$userId, n_points, replace = TRUE),
    date = sample(seq.Date(as.Date("2026-01-01"), as.Date("2026-01-31"), by = "day"), n_points, replace = TRUE),
    minute_of_day = sample(0:1439, n_points, replace = TRUE),
    state = sample(c("stationary", "slow", "fast"), n_points, replace = TRUE, prob = c(0.65, 0.28, 0.07))
  ) |>
    left_join(users, by = "userId") |>
    mutate(
      timestamp_minute = as.POSIXct(date) + minute_of_day * 60,
      offset_m = case_when(
        state == "stationary" ~ rnorm(n(), 0, 25),
        state == "slow" ~ rnorm(n(), 0, 120),
        TRUE ~ rnorm(n(), 0, 350)
      ),
      angle = runif(n(), 0, 2 * pi),
      longitude = home_lon + ((offset_m * cos(angle)) / (111320 * cos(home_lat * pi / 180))),
      latitude = home_lat + ((offset_m * sin(angle)) / 111320),
      longitude = clip_range(longitude, bbox$xmin, bbox$xmax),
      latitude = clip_range(latitude, bbox$ymin, bbox$ymax),
      accuracy = clip_range(rnorm(n(), 14, 6), 3, 40),
      altitude = clip_range(rnorm(n(), 118, 25), 70, 220),
      heading = runif(n(), 0, 360),
      filled = as.integer(runif(n()) < 0.12)
    ) |>
    arrange(userId, timestamp_minute)

  points_sf <- st_as_sf(base, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  points_m <- st_transform(points_sf, 25832)

  shops_m <- st_transform(layers$shops, 25832)
  ped_m <- st_transform(layers$pedestrian, 25832)
  rail_m <- st_transform(layers$railway, 25832)
  dept_m <- st_transform(layers$department, 25832)

  shop_buf <- st_buffer(shops_m, 50)
  ped_buf <- st_buffer(ped_m, 10)
  rail_buf <- st_buffer(rail_m, 50)

  shop_hit <- lengths(st_intersects(points_m, shop_buf)) > 0
  ped_hit <- lengths(st_intersects(points_m, ped_buf)) > 0
  rail_hit <- lengths(st_intersects(points_m, rail_buf)) > 0
  dept_hit <- lengths(st_intersects(points_m, dept_m)) > 0

  enriched <- base |>
    mutate(
      ndvi_mean_100m = clip_range(0.45 - (abs(longitude - 8.404) * 8) + rnorm(n(), 0, 0.08), -0.1, 0.9),
      ndvi_green_area_100m_m2 = clip_range(1000 + (ndvi_mean_100m * 1200) + rnorm(n(), 0, 150), 0, 31416),
      ndvi_green_share_100m = 100 * ndvi_green_area_100m_m2 / (pi * 100^2),
      imperviousness_mean_100m = clip_range(100 - (ndvi_mean_100m * 95) + rnorm(n(), 0, 7), 5, 100),
      population = clip_range(1500 + (imperviousness_mean_100m * 65) + rnorm(n(), 0, 400), 300, 12000),
      shop = as.integer(shop_hit),
      pedestrian = as.integer(ped_hit),
      railway = as.integer(rail_hit),
      department = as.integer(dept_hit),
      crowded_area = as.integer(shop + pedestrian + railway + department > 0)
    ) |>
    group_by(userId) |>
    arrange(timestamp_minute, .by_group = TRUE) |>
    mutate(
      distance_to_prev = haversine_dist(dplyr::lag(longitude), dplyr::lag(latitude), longitude, latitude),
      distances = replace_na(distance_to_prev, 0),
      delta_min = as.numeric(difftime(timestamp_minute, dplyr::lag(timestamp_minute), units = "mins")),
      delta_min = if_else(is.na(delta_min) | delta_min <= 0, 1, delta_min),
      speed_kmh = (distances / delta_min) * 60 / 1000,
      Minutes_Stationary = as.integer(speed_kmh <= 0.001),
      `Minutes_<20_kmh` = as.integer(speed_kmh > 0.001 & speed_kmh <= 20),
      `Minutes_>20_kmh` = as.integer(speed_kmh > 20),
      steps = case_when(
        Minutes_Stationary == 1 ~ rpois(n(), 4),
        `Minutes_<20_kmh` == 1 ~ rpois(n(), 60),
        TRUE ~ rpois(n(), 20)
      )
    ) |>
    ungroup() |>
    mutate(
      distance_to_home = haversine_dist(longitude, latitude, home_lon, home_lat),
      at_home = as.integer(distance_to_home <= 200),
      date = as.Date(timestamp_minute)
    )

  hotspots <- tibble(
    lon = c(8.404, 8.401, 8.410, 8.396),
    lat = c(49.014, 49.009, 49.018, 49.021),
    hotspot = c("center", "market", "station", "campus")
  )

  nearest <- nearest_hotspot(
    point_lon = enriched$longitude,
    point_lat = enriched$latitude,
    hotspot_lon = hotspots$lon,
    hotspot_lat = hotspots$lat
  )
  nearest_idx <- nearest$idx
  nearest_dist <- nearest$dist

  enriched <- enriched |>
    mutate(
      cluster_number = if_else(nearest_dist <= 120, nearest_idx, 0L),
      cluster_id = if_else(cluster_number == 0, "noise", paste0("C", cluster_number))
    )

  cluster_daily <- enriched |>
    arrange(userId, date, timestamp_minute) |>
    group_by(userId, date) |>
    summarise(
      day_unique_cluster_count = n_distinct(cluster_number[cluster_number > 0]),
      day_total_cluster_changes = sum(cluster_id != dplyr::lag(cluster_id), na.rm = TRUE),
      day_mean_time_at_cluster = {
        freq <- table(cluster_id[cluster_id != "noise"])
        if (length(freq) == 0) 0 else as.numeric(mean(freq))
      },
      day_total_time_in_noise = sum(cluster_id == "noise"),
      .groups = "drop"
    )

  minute_data <- enriched |>
    left_join(cluster_daily, by = c("userId", "date")) |>
    group_by(userId, date) |>
    arrange(timestamp_minute, .by_group = TRUE) |>
    mutate(
      cumulative_distance = cumsum(distances),
      cumulative_distance_slow = cumsum(if_else(`Minutes_<20_kmh` == 1, distances, 0)),
      cumulative_distance_fast = cumsum(if_else(`Minutes_>20_kmh` == 1, distances, 0))
    ) |>
    ungroup() |>
    transmute(
      userId,
      timestamp_minute,
      date,
      latitude,
      longitude,
      altitude,
      accuracy,
      heading,
      filled,
      distances,
      speed_kmh,
      cumulative_distance,
      cumulative_distance_slow,
      cumulative_distance_fast,
      `Minutes_<20_kmh`,
      `Minutes_>20_kmh`,
      Minutes_Stationary,
      cluster_number,
      cluster_id,
      day_unique_cluster_count,
      day_total_cluster_changes,
      day_mean_time_at_cluster,
      day_total_time_in_noise,
      home_latitude = home_lat,
      home_longitude = home_lon,
      distance_to_home,
      at_home,
      crowded_area,
      shop,
      pedestrian,
      railway,
      department,
      imperviousness_mean_100m,
      ndvi_mean_100m,
      ndvi_green_area_100m_m2,
      ndvi_green_share_100m,
      population,
      steps
    )

  day_data <- minute_data |>
    group_by(userId, date) |>
    summarise(
      Minutes_slow_kmh_day = sum(`Minutes_<20_kmh`, na.rm = TRUE),
      Minutes_fast_kmh_day = sum(`Minutes_>20_kmh`, na.rm = TRUE),
      Minutes_Stationary_day = sum(Minutes_Stationary, na.rm = TRUE),
      steps_day = sum(steps, na.rm = TRUE),
      cumulative_distance_day = max(cumulative_distance, na.rm = TRUE),
      cumulative_distance_slow_day = max(cumulative_distance_slow, na.rm = TRUE),
      cumulative_distance_fast_day = max(cumulative_distance_fast, na.rm = TRUE),
      day_unique_cluster_count = dplyr::first(day_unique_cluster_count),
      day_total_cluster_changes = dplyr::first(day_total_cluster_changes),
      day_mean_time_at_cluster = dplyr::first(day_mean_time_at_cluster),
      day_total_time_in_noise = dplyr::first(day_total_time_in_noise),
      crowded_area_minutes_day = sum(crowded_area, na.rm = TRUE),
      pedestrian_minutes_day = sum(pedestrian, na.rm = TRUE),
      shop_minutes_day = sum(shop, na.rm = TRUE),
      minutes_at_home_day = sum(at_home, na.rm = TRUE),
      minutes_not_home_day = sum(1 - at_home, na.rm = TRUE),
      mean_imperviousness_100m_day = mean(imperviousness_mean_100m, na.rm = TRUE),
      mean_ndvi_100m_day = mean(ndvi_mean_100m, na.rm = TRUE),
      mean_popululation_day = mean(population, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    minute_data = minute_data,
    day_data = day_data,
    layers = layers
  )
}
